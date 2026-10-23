;;; u0.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defvar neuro--rng-lo (logand (truncate (* (float-time) 1000)) #xFFFFFFFF))
(defvar neuro--rng-hi (logand (* (emacs-pid) #x9E3779B9) #xFFFFFFFF))

(defun neuro--u32 (x) (logand x #xFFFFFFFF))

(defun neuro-xorshift64 ()

  (let* ((lo neuro--rng-lo)
         (hi neuro--rng-hi)

         (sl13-lo (neuro--u32 (logxor lo (ash lo 13))))
         (sl13-hi (neuro--u32 (logxor hi
                                       (logxor (ash hi 13)
                                               (logand (ash lo (- 13 32)) #xFFFFFFFF)))))

         (sr7-lo  (neuro--u32 (logxor sl13-lo
                                       (logxor (ash sl13-lo -7)
                                               (ash (logand sl13-hi #x7F) 25)))))
         (sr7-hi  (neuro--u32 (logxor sl13-hi (ash sl13-hi -7))))

         (sl17-lo (neuro--u32 (logxor sr7-lo (ash sr7-lo 17))))
         (sl17-hi (neuro--u32 (logxor sr7-hi
                                       (logxor (ash sr7-hi 17)
                                               (logand (ash sr7-lo (- 17 32)) #xFFFFFFFF))))))
    (setq neuro--rng-lo sl17-lo
          neuro--rng-hi sl17-hi)
    (logior (ash neuro--rng-hi 32) neuro--rng-lo)))

(defun neuro-rng-range (max)
  (if (<= max 0) 0
    (mod (logand (neuro-xorshift64) #xFFFFFFFF) max)))

(defun neuro-rng-bool (pct)
  (< (mod (logand (neuro-xorshift64) #xFFFFFFFF) 100) (max 0 (min 100 pct))))

(defun neuro-rng-u16 ()
  (logand (neuro-xorshift64) #xFFFF))

(defun neuro-rng-u8 ()
  (logand (neuro-xorshift64) #xFF))

(defun neuro-rng-choose (lst)

  (when lst
    (nth (neuro-rng-range (length lst)) lst)))

(defun neuro-rng-f32 ()
  (/ (float (logand (neuro-xorshift64) #xFFFFFF)) 16777216.0))

(defun neuro-now-ms ()
  (floor (* (float-time) 1000)))

(defun neuro-sleep-ms (ms)
  (sit-for (/ (float ms) 1000.0)))

(defun neuro-beep (times ms)
  (dotimes (_ times)
    (ding t)
    (neuro-sleep-ms ms)))

(defun neuro-hex-dump (data base cols)
  (when (<= cols 0) (setq cols 8))

  (let* ((vec  (if (stringp data) (vconcat data) data))
         (len  (length vec))
         (out  ""))
    (let ((i 0))
      (while (< i len)
        (setq out (concat out (format "%04X:" (logand (+ base i) #xFFFF))))
        (let ((j 0))
          (while (and (< j cols) (< (+ i j) len))
            (setq out (concat out (format " %02X" (logand (aref vec (+ i j)) #xFF))))
            (setq j (1+ j)))
          (while (< j cols)
            (setq out (concat out "   "))
            (setq j (1+ j))))
        (setq out (concat out "  "))
        (let ((j 0))
          (while (and (< j cols) (< (+ i j) len))
            (let ((c (logand (aref vec (+ i j)) #xFF)))
              (setq out (concat out (if (and (>= c 32) (< c 127))
                                       (string c) "."))))
            (setq j (1+ j))))
        (setq out (concat out "\n"))
        (setq i (+ i cols))))
    out))

(defun neuro-to-hex (n) (format "%04X" (logand n #xFFFF)))

(defun neuro-clamp (v lo hi) (max lo (min hi v)))

(defun neuro-validate-filename (name)
  (and (stringp name)
       (> (length name) 0)
       (<= (length name) 12)
       (not (string-match-p "[/\\\\:*?\"<>|]" name))))

(defun neuro-validate-dirname (name)

  (and (stringp name)
       (> (length name) 0)
       (<= (length name) 8)
       (not (string-match-p "[/\\\\:*?\"<>|. ]" name))))

(defun neuro-is-printable (b)
  (or (and (>= b 32) (< b 127)) (= b 0)))

(defun neuro-parity (b)
  (let ((v (logand b #xFF)))
    (setq v (logxor v (logand (ash v -4) #xFF)))
    (setq v (logxor v (logand (ash v -2) #xFF)))
    (setq v (logxor v (logand (ash v -1) #xFF)))
    (= (logand v 1) 0)))

(defvar neuro-output-hook nil
  "Hook called with (STRING) to emit simulator output.
Set by neuro-repl.el to `neuro-repl-print'. Defaults to `message'.")

(defun neuro-emit (str)
  (if neuro-output-hook
      (funcall neuro-output-hook str)
    (message "%s" str)))

(defun neuro-clear-screen ()
  (cond
   ((fboundp 'neuro-repl-clear) (neuro-repl-clear))
   (neuro-output-hook nil)))

(provide 'u0)
