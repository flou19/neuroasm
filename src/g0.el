;;; g0.el -*- lexical-binding: t -*-

(require 'u0)
(require 'subr-x)

(defvar neuro-config-values
  (let ((h (make-hash-table :test 'equal)))
    (puthash "VERSION"  "4.0.0" h)
    (puthash "PROMPT"   "> "    h)
    (puthash "MAX_PROC" "64"    h)
    (puthash "QUANTUM"  "16"    h)
    (puthash "MEM_CAP"  "1048576" h)
    (puthash "SHELL"    "n0" h)
    (puthash "HOSTNAME" "DOS622" h)
    h))

(defvar neuro-config-flags
  (let ((h (make-hash-table :test 'equal)))
    (puthash "DEBUG"  nil h)
    (puthash "SECURE" t   h)
    (puthash "NEURAL" t   h)
    (puthash "CHASE"  t   h)
    (puthash "BEEP"   t   h)
    (puthash "COLOR"  t   h)
    h))

(defvar neuro-config-counters
  (let ((h (make-hash-table :test 'equal)))
    (puthash "BOOT"  0 h)
    (puthash "CMDS"  0 h)
    (puthash "KILLS" 0 h)
    h))

(defun neuro-config-get (key)
  (gethash (upcase key) neuro-config-values))

(defun neuro-config-set (key val)

  (puthash (upcase key) (format "%s" val) neuro-config-values))

(defun neuro-config-flag (key)
  (gethash (upcase key) neuro-config-flags))

(defun neuro-config-set-flag (key on)
  (puthash (upcase key) on neuro-config-flags))

(defun neuro-config-counter (key)
  (or (gethash (upcase key) neuro-config-counters) 0))

(defun neuro-config-inc (key)
  (puthash (upcase key)
           (1+ (neuro-config-counter key))
           neuro-config-counters))

(defun neuro-config-dump ()

  (let (lines)
    (maphash (lambda (k v) (push (format "  %-14s = %s" k v) lines))
             neuro-config-values)
    (maphash (lambda (k v) (push (format "  %-14s = %s [flag]" k v) lines))
             neuro-config-flags)
    (maphash (lambda (k v) (push (format "  %-14s = %d [counter]" k v) lines))
             neuro-config-counters)
    (sort lines #'string<)))

(defun neuro-config-load-file (path)

  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (dolist (line (split-string (buffer-string) "\n" t))
        (let ((trimmed (string-trim line)))
          (unless (or (string-empty-p trimmed)
                      (string-prefix-p "#" trimmed)
                      (string-prefix-p ";" trimmed))
            (when (string-match "^\\([^=]+\\)=\\(.*\\)$" trimmed)
              (let ((k (upcase (string-trim (match-string 1 trimmed))))
                    (v (string-trim (match-string 2 trimmed))))
                (cond
                 ((gethash k neuro-config-flags)
                  (neuro-config-set-flag k (member v '("true" "yes" "1"))))
                 ((member v '("true" "false" "yes" "no" "1" "0"))
                  (neuro-config-set-flag k (member v '("true" "yes" "1"))))
                 (t
                  (neuro-config-set k v)))))))))
    t))

(provide 'g0)
