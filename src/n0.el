;;; n0.el -*- lexical-binding: t -*-

(defconst neuroasm-version "4.0.0")

(let ((this-file (or load-file-name
                     (and (boundp 'buffer-file-name) buffer-file-name)
                     (expand-file-name "n0.el" default-directory))))
  (add-to-list 'load-path (file-name-directory this-file)))

(require 'cl-lib)
(require 'u0)
(require 'f0)
(require 'c0)
(require 'e0)
(require 'a0)
(require 'k0)
(require 'y0)
(require 'w0)
(require 'v0)
(require 's0)
(require 'd0)
(require 'g0)
(require 'b0)
(require 'r0)

(declare-function module-load "emacs" (file))

(defun neuroasm ()
  "Launch neurodos: DOS-like neural simulator."
  (interactive)
  (neuro-repl-run))

(defun neuroasm-load-module ()
  "Load the C module for hardware-accurate CPU, RNG, and neural core."
  (when (fboundp 'module-load)
    (condition-case err
        (progn (neuro--ensure-module) t)
      (error (message "neurodos: C module not loaded: %s" (cadr err)) nil))))

(defun neuroasm-start-daemon ()
  "Start the neurodos background daemon."
  (interactive)
  (neuro--ensure-daemon)
  (message "neurodos daemon started"))

(defun neuroasm-stop-daemon ()
  "Stop the neurodos background daemon."
  (interactive)
  (neuro/daemon-stop)
  (message "neurodos daemon stopped"))

(defun neuroasm-backend-status ()
  "Show status of all backends in minibuffer."
  (interactive)
  (let ((st (neuro/backend-status)))
    (message "neurodos backends — module: %s  daemon: %s  cli: %s"
             (plist-get st :module)
             (plist-get st :daemon)
             (plist-get st :cli))))

(neuroasm-load-module)

(provide 'n0)
