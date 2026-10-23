;;; v0.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)

(defvar neuro-event-queue    nil)
(defvar neuro-event-history  nil)
(defvar neuro-event-next-id  1)
(defvar neuro-event-max-history 256)
(defvar neuro-event-handlers (make-hash-table :test 'equal))

(defun neuro-event-init ()
  (setq neuro-event-queue    nil
        neuro-event-history  nil
        neuro-event-next-id  1
        neuro-event-handlers (make-hash-table :test 'equal)))

(defun neuro-event-emit (kind &optional data)
  (let ((ev (list :id   neuro-event-next-id
                  :kind kind
                  :data data
                  :time (neuro-now-ms))))

    (setq neuro-event-next-id
          (if (> neuro-event-next-id most-positive-fixnum)
              1
            (1+ neuro-event-next-id)))
    (push ev neuro-event-queue)

    (setq neuro-event-history (append neuro-event-history (list ev)))
    (when (> (length neuro-event-history) neuro-event-max-history)

      (setq neuro-event-history (last neuro-event-history neuro-event-max-history)))
    ev))

(defun neuro-event-on (kind handler)
  (unless (functionp handler)
    (error "neuro-event-on: handler must be a function, got %S" handler))
  (let ((lst (gethash kind neuro-event-handlers)))
    (puthash kind (cons handler lst) neuro-event-handlers)))

(defun neuro-event-dispatch ()

  (let ((queue (nreverse neuro-event-queue)))
    (setq neuro-event-queue nil)
    (dolist (ev queue)
      (let* ((kind     (plist-get ev :kind))
             (handlers (gethash kind neuro-event-handlers)))
        (dolist (h handlers)
          (condition-case err
              (funcall h ev)
            (error
             (message "[event-error] %s handler: %s" kind (error-message-string err)))))))))

(defun neuro-event-history-by-kind (kind &optional n)

  (let* ((filtered (seq-filter (lambda (ev) (equal (plist-get ev :kind) kind))
                               neuro-event-history))
         (cnt (or n 10)))
    (last filtered cnt)))

(defun neuro-event-latest (kind)
  "Return the single most recent event of KIND, or nil."
  (car (last (neuro-event-history-by-kind kind 1))))

(defun neuro-event-emit-boot       ()      (neuro-event-emit 'boot))
(defun neuro-event-emit-command    (cmd)   (neuro-event-emit 'command cmd))
(defun neuro-event-emit-threat     (level) (neuro-event-emit 'threat-raise level))
(defun neuro-event-emit-fragment   (name)  (neuro-event-emit 'fragment name))
(defun neuro-event-emit-chase-start ()     (neuro-event-emit 'chase-start))
(defun neuro-event-emit-chase-end  (won)   (neuro-event-emit 'chase-end won))
(defun neuro-event-emit-spawn      (pid)   (neuro-event-emit 'process-spawn pid))
(defun neuro-event-emit-kill       (pid)   (neuro-event-emit 'process-kill pid))
(defun neuro-event-emit-u2-speak   (msg)   (neuro-event-emit 'second-user-speak msg))
(defun neuro-event-emit-ending     (name)  (neuro-event-emit 'ending name))
(defun neuro-event-emit-idle       (ticks) (neuro-event-emit 'idle ticks))

(defun neuro-event-summary ()
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (ev neuro-event-history)
      (let ((k (plist-get ev :kind)))
        (puthash k (1+ (or (gethash k counts) 0)) counts)))
    (let (pairs)
      (maphash (lambda (k v) (push (cons k v) pairs)) counts)

      (sort pairs (lambda (a b)
                    (if (= (cdr a) (cdr b))
                        (string< (format "%s" (car a)) (format "%s" (car b)))
                      (> (cdr a) (cdr b))))))))

(provide 'v0)
