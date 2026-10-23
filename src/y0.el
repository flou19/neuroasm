;;; y0.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defvar neuro-threat            0)
(defvar neuro-game-beaten       nil)
(defvar neuro-fragments-collected (make-hash-table :test 'equal))
(defvar neuro-cores-found       0)
(defvar neuro-total-commands    0)
(defvar neuro-research-unlocked nil)
(defvar neuro-observe-streak 0)

(defvar neuro-sec-locked-paths    (make-hash-table :test 'equal))
(defvar neuro-sec-allowed-syscalls
  (let ((h (make-hash-table :test #'eql)))
    (dotimes (i #x30) (puthash i t h)) h))
(defvar neuro-sec-audit-log   nil)
(defvar neuro-sec-max-audit   512)
(defvar neuro-sec-session-id  1)

(defun neuro-sec-init ()
  (setq neuro-sec-locked-paths (make-hash-table :test 'equal)
        neuro-sec-audit-log    nil
        neuro-sec-session-id   (neuro-now-ms)))

(defun neuro-sec-lock-path   (path) (puthash (upcase path) t neuro-sec-locked-paths))
(defun neuro-sec-unlock-path (path) (remhash (upcase path)  neuro-sec-locked-paths))
(defun neuro-sec-is-locked   (path) (gethash (upcase path)  neuro-sec-locked-paths))

(defun neuro-sec-audit (entry)
  (push (cons (neuro-now-ms) entry) neuro-sec-audit-log)
  (when (> (length neuro-sec-audit-log) neuro-sec-max-audit)
    (setq neuro-sec-audit-log (seq-take neuro-sec-audit-log neuro-sec-max-audit))))

(defun neuro-sec-allowed-syscall (n) (gethash n neuro-sec-allowed-syscalls))

(defvar neuro-sandbox-mem-used 0)
(defvar neuro-sandbox-active      nil)
(defvar neuro-sandbox-mem-cap     #x10000)
(defvar neuro-sandbox-insn-limit  1000000)
(defvar neuro-sandbox-insn-count  0)

(defun neuro-sandbox-init ()
  (setq neuro-sandbox-active     nil
        neuro-sandbox-insn-count 0))

(defun neuro-sandbox-activate ()

  (setq neuro-sandbox-active     t
        neuro-sandbox-insn-count 0))

(defun neuro-sandbox-deactivate ()
  (setq neuro-sandbox-active     nil
        neuro-sandbox-insn-count 0
        neuro-sandbox-mem-used   0))

(defun neuro-sandbox-tick ()
  (when neuro-sandbox-active
    (cl-incf neuro-sandbox-insn-count)
    (when (>= neuro-sandbox-insn-count neuro-sandbox-insn-limit)
      (error "Sandbox: instruction limit exceeded (%d)" neuro-sandbox-insn-limit))))

(defun neuro-sandbox-check-mem (used)

  (when neuro-sandbox-active
    (setq neuro-sandbox-mem-used (+ neuro-sandbox-mem-used used))
    (when (> neuro-sandbox-mem-used neuro-sandbox-mem-cap)
      (error "Sandbox: memory cap exceeded (%d > %d)"
             neuro-sandbox-mem-used neuro-sandbox-mem-cap))))

(defun neuro-sandbox-reset-mem ()
  (setq neuro-sandbox-mem-used 0))

(defconst neuro-session-file "neuroasm_session.dat")

(defun neuro-session-save ()

  (let* ((frags (let (lst)
                  (maphash (lambda (k _) (push k lst)) neuro-fragments-collected)
                  (sort lst #'string<)))
         (data (concat
                (format "threat=%d\n"    neuro-threat)
                (format "beaten=%s\n"    (if neuro-game-beaten "true" "false"))
                (format "fragments=%s\n" (mapconcat #'identity frags ","))
                (format "cores=%d\n"     neuro-cores-found)
                (format "commands=%d\n"  neuro-total-commands)
                (format "research=%s\n"  (if neuro-research-unlocked "true" "false"))
                (format "observe=%d\n" neuro-observe-streak))))
    (with-temp-file neuro-session-file (insert data))))

(defun neuro-session-load ()
  (when (file-exists-p neuro-session-file)
    (with-temp-buffer
      (insert-file-contents neuro-session-file)
      (dolist (line (split-string (buffer-string) "\n" t))
        (when (string-match "^\\([^=]+\\)=\\(.*\\)$" line)
          (let ((k (match-string 1 line))
                (v (match-string 2 line)))
            (cond
             ((equal k "threat")    (setq neuro-threat (string-to-number v)))
             ((equal k "beaten")    (setq neuro-game-beaten (equal v "true")))
             ((equal k "research")  (setq neuro-research-unlocked (equal v "true")))
             ((equal k "cores")     (setq neuro-cores-found (string-to-number v)))

             ((equal k "commands")  (setq neuro-total-commands (string-to-number v)))
             ((equal k "observe")   (setq neuro-observe-streak (string-to-number v)))
             ((equal k "fragments")
              (unless (string-empty-p v)
                (dolist (f (split-string v "," t))
                  (puthash f t neuro-fragments-collected))))))))
      t)))

(provide 'y0)
