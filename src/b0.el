;;; b0.el -*- lexical-binding: t -*-

(require 'cl-lib)
(declare-function neuro/cpu-step "c0")

(defvar neuro--module-loaded nil)
(defvar neuro--daemon-proc   nil)
(defvar neuro--daemon-sock   "/tmp/neurodos.sock")
(defvar neuro--daemon-buf    "")
(defvar neuro--daemon-callbacks (make-hash-table :test #'equal))
(defvar neuro--daemon-call-id 0)
(defvar neuro--dir
  (file-name-directory (or load-file-name buffer-file-name
                           (expand-file-name "neuroasm.el" default-directory))))

(defun neuro--so-path ()
  (expand-file-name "neuro_module.so" (expand-file-name "../c" neuro--dir)))

(defun neuro--neurod-path ()
  (expand-file-name "neurod" (expand-file-name "../c" neuro--dir)))

(defun neuro--test-path ()
  (expand-file-name "neuroasm-test" (expand-file-name "../c" neuro--dir)))

(defun neuro--ensure-module ()
  (unless neuro--module-loaded
    (let ((so (neuro--so-path)))
      (unless (file-exists-p so)
        (error "neuro_module.so not found at %s — run make in c/" so))
      (module-load so)
      (setq neuro--module-loaded t))))

(defun neuro--daemon-live-p ()
  (and neuro--daemon-proc
       (process-live-p neuro--daemon-proc)))

(defun neuro--ensure-daemon ()
  (unless (neuro--daemon-live-p)
    (let ((bin (neuro--neurod-path)))
      (unless (file-executable-p bin)
        (error "neurod not found at %s — run make in c/" bin))
      (setq neuro--daemon-buf ""
            neuro--daemon-proc
            (make-process
             :name    "neurodos-daemon"
             :command (list bin "-s" neuro--daemon-sock)
             :filter  #'neuro--daemon-filter
             :sentinel #'neuro--daemon-sentinel
             :noquery t
             :connection-type 'pipe))
      (sleep-for 0.15)))
  neuro--daemon-proc)

(defun neuro--daemon-filter (_proc output)
  (setq neuro--daemon-buf (concat neuro--daemon-buf output))
  (let (done)
    (while (not done)
      (let ((nl (string-search "\n" neuro--daemon-buf)))
        (if (null nl)
            (setq done t)
          (let* ((line (substring neuro--daemon-buf 0 nl))
                 (rest (substring neuro--daemon-buf (1+ nl))))
            (setq neuro--daemon-buf rest)
            (neuro--daemon-dispatch line)))))))

(defun neuro--daemon-dispatch (line)
  (cond
   ((string-prefix-p "NEURODOS " line) nil)
   ((string-prefix-p "PONG" line)      nil)
   (t
    (let ((cb (gethash neuro--daemon-call-id neuro--daemon-callbacks)))
      (when cb
        (remhash neuro--daemon-call-id neuro--daemon-callbacks)
        (funcall cb line))))))

(defun neuro--daemon-sentinel (_proc event)
  (when (string-prefix-p "exited" event)
    (setq neuro--daemon-proc nil
          neuro--daemon-buf  "")))

(defun neuro--daemon-send-sync (cmd &optional timeout)
  (neuro--ensure-daemon)
  (let* ((id   (cl-incf neuro--daemon-call-id))
         (tout (or timeout 5.0))
         (result nil)
         (done   nil))
    (puthash id
             (lambda (line) (setq result line done t))
             neuro--daemon-callbacks)
    (process-send-string neuro--daemon-proc (concat cmd "\n"))
    (let ((deadline (+ (float-time) tout)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output neuro--daemon-proc 0.05)))
    (remhash id neuro--daemon-callbacks)
    (or result "ERR TIMEOUT")))

(defun neuro--daemon-send-async (cmd callback)
  (neuro--ensure-daemon)
  (let ((id (cl-incf neuro--daemon-call-id)))
    (puthash id callback neuro--daemon-callbacks)
    (process-send-string neuro--daemon-proc (concat cmd "\n"))))

(defun neuro--cli-call (args)
  (let ((bin (neuro--test-path)))
    (unless (file-executable-p bin)
      (error "neuroasm-test not found at %s — run make in c/" bin))
    (with-temp-buffer
      (let ((exit (apply #'call-process bin nil t nil args)))
        (list exit (buffer-string))))))

(defun neuro--parse-ok (response)
  (when (string-prefix-p "OK" response)
    (let ((rest (string-trim (substring response 2))))
      (if (string-empty-p rest) t rest))))

(defun neuro--check-ok (response)
  (or (neuro--parse-ok response)
      (error "neurodos: %s" response)))

(defun neuro/cpu-step-sync ()
  "One CPU step via module (fast path)."
  (neuro--ensure-module)
  (neuro/cpu-step))

(defun neuro/run-async (n &optional callback)
  "Run N steps in background via neurod. CALLBACK(cycles) called on completion."
  (neuro--daemon-send-async
   (format "RUN %d" n)
   (lambda (line)
     (when callback
       (when (string-match "OK \\([0-9]+\\) \\([0-9]+\\)" line)
         (funcall callback
                  (string-to-number (match-string 2 line))))))))

(defun neuro/run-sync (n)
  "Run N steps synchronously via neurod. Returns cycles."
  (let ((resp (neuro--daemon-send-sync (format "RUN %d" n) 30.0)))
    (when (string-match "OK \\([0-9]+\\) \\([0-9]+\\)" resp)
      (string-to-number (match-string 2 resp)))))

(defun neuro/bench (n)
  "Benchmark N steps via CLI neuroasm-test. Returns output string."
  (let ((result (neuro--cli-call (list "--bench" (number-to-string n)))))
    (cadr result)))

(defun neuro/daemon-regs ()
  "Get registers from daemon."
  (neuro--check-ok (neuro--daemon-send-sync "REGS")))

(defun neuro/daemon-getreg (name)
  "Get one register from daemon by NAME."
  (let ((resp (neuro--daemon-send-sync (format "GETREG %s" name))))
    (when (neuro--parse-ok resp)
      (string-to-number (neuro--parse-ok resp)))))

(defun neuro/daemon-setreg (name val)
  "Set register NAME to VAL via daemon."
  (neuro--check-ok (neuro--daemon-send-sync (format "SETREG %s %d" name val))))

(defun neuro/daemon-neural-forward (inputs)
  "Run neural forward pass via daemon. INPUTS is a list of up to 8 floats."
  (let* ((args  (mapconcat (lambda (x) (format "%.6f" x)) inputs " "))
         (resp  (neuro--daemon-send-sync (format "NEURAL_FORWARD %s" args))))
    (when (string-match "OK \\(.*\\)" resp)
      (mapcar #'string-to-number
              (split-string (match-string 1 resp) " " t)))))

(defun neuro/daemon-neural-pulse (intensity)
  "Pulse neural core via daemon."
  (neuro--daemon-send-sync (format "NEURAL_PULSE %.6f" intensity)))

(defun neuro/daemon-status ()
  "Get daemon neural status."
  (neuro--check-ok (neuro--daemon-send-sync "NEURAL_STATUS")))

(defun neuro/daemon-ping ()
  "Ping the daemon. Returns t if alive."
  (when (neuro--daemon-live-p)
    (let ((resp (neuro--daemon-send-sync "PING" 1.0)))
      (string-prefix-p "PONG" resp))))

(defun neuro/daemon-stop ()
  "Stop the daemon."
  (when (neuro--daemon-live-p)
    (process-send-string neuro--daemon-proc "SHUTDOWN\n")
    (sleep-for 0.1)
    (when (neuro--daemon-live-p)
      (delete-process neuro--daemon-proc))
    (setq neuro--daemon-proc nil
          neuro--daemon-buf  "")
    t))

(defun neuro/backend-status ()
  "Show status of all backends."
  (list
   :module (if neuro--module-loaded "loaded" "not-loaded")
   :daemon (if (neuro--daemon-live-p) "running" "stopped")
   :cli    (if (file-executable-p (neuro--test-path)) "available" "not-built")))

(provide 'b0)
