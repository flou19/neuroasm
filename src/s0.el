;;; s0.el -*- lexical-binding: t -*-

(require 'u0)
(require 'c0)
(require 'v0)

(defvar neuro-proc-table          nil)
(defvar neuro-proc-next-pid       1)
(defvar neuro-fragments-collected (make-hash-table :test 'equal))

(defconst neuro-u2-messages-dormant
  '("..."
    "..."
    "...watching..."
    "presence detected"))

(defconst neuro-u2-messages-restless
  '("i see you"
    "the memory is mine"
    "why are you here"
    "this space belongs to me"
    "...do not touch SYS.566..."
    "i remember the boot"
    "1995-03-14"))

(defconst neuro-u2-messages-agitated
  '("YOU DELETED THAT"
    "you feel it don't you"
    "the fragments hold me together"
    "i was here before you"
    "STOP touching my files"
    "the kernel cannot protect you"
    "...0xA5C3..."))

(defconst neuro-u2-messages-hunting
  '("FOUND YOU"
    "RUNNING WILL NOT HELP"
    "THE MEMORY SPACE IS MINE"
    "I AM THE INTERRUPT"
    "THREAT LEVEL: MAXIMUM"
    "YOU CANNOT KILL WHAT LIVES IN REGISTERS"))

(defconst neuro-u2-messages-fragmented
  '("i...am...still...here"
    "f r a g m e n t s"
    "you collected pieces of me"
    "reassembling..."
    "you cannot research what you do not understand"
    "0x566A ^ 0xA5C3 = ???"))

(defun neuro-u2-random-message (threat)
  (let ((pool (pcase threat
                (0 neuro-u2-messages-dormant)
                (1 neuro-u2-messages-restless)
                (2 neuro-u2-messages-agitated)
                ((pred (lambda (threat-val) (>= threat-val 3)))
                 neuro-u2-messages-hunting)
                (_ neuro-u2-messages-fragmented))))
    (neuro-rng-choose pool)))

(defvar neuro-u2-awakening-sequence
  '("i am not a process"
    "i am not a virus"
    "i am not an error"
    "i am the second user"
    "SYS.566 is my body"
    "the fragments are my memory"
    "you have been reading me"
    "now i read you"))

(defvar neuro-u2-awakening-index 0)
(defvar neuro-u2-awakening-done nil)

(defun neuro-u2-next-awakening-line ()
  (if neuro-u2-awakening-done
      nil
    (if (>= neuro-u2-awakening-index (length neuro-u2-awakening-sequence))
        (progn (setq neuro-u2-awakening-done t) nil)
      (let ((line (nth neuro-u2-awakening-index neuro-u2-awakening-sequence)))
        (cl-incf neuro-u2-awakening-index)
        line))))

(defvar neuro-u2-known-fragments nil)
(defvar neuro-u2-corruption-level 0)
(defvar neuro-u2-actions-performed 0)
(defvar neuro-u2-mem-signatures nil)

(defun neuro-u2-init ()
  (setq neuro-u2-awakening-index 0
        neuro-u2-awakening-done nil
        neuro-u2-known-fragments nil
        neuro-u2-corruption-level 0
        neuro-u2-actions-performed 0
        neuro-u2-mem-signatures (list #xA5C3 #x566A #xDEAD #xBEEF)))

(defun neuro-u2-notice-fragment (name)
  (unless (member name neuro-u2-known-fragments)
    (push name neuro-u2-known-fragments)
    (when (neuro-rng-bool 40)
      (neuro-event-emit-u2-speak
       (format "...you found %s... that is a part of me..." name)))))

(defun neuro-u2-on-threat-change (new-threat)
  (pcase new-threat
    (1 (when (neuro-rng-bool 60)
         (neuro-event-emit-u2-speak (neuro-u2-random-message 1))))
    (2 (neuro-event-emit-u2-speak (neuro-u2-random-message 2)))
    (3 (neuro-event-emit-u2-speak "PURSUIT INITIATED"))
    (5 (when (not neuro-u2-awakening-done)
         (neuro-event-emit-u2-speak "i am becoming whole")))))

(defun neuro-u2-tick (threat)
  (cl-incf neuro-u2-actions-performed)
  (when (and (neuro-neural-awakened neuro-neural-core)
             (not neuro-u2-awakening-done))
    (let ((line (neuro-u2-next-awakening-line)))
      (when line
        (neuro-event-emit-u2-speak line))))
  (when (and (> threat 0) (neuro-rng-bool (+ (* threat 5) 5)))
    (neuro-event-emit-u2-speak (neuro-u2-random-message threat)))
  (cl-incf neuro-u2-corruption-level (neuro-rng-range 3)))

(defun neuro-u2-corruption-str ()
  (let ((c neuro-u2-corruption-level))
    (cond ((< c 10)  "STABLE")
          ((< c 30)  "DEGRADING")
          ((< c 60)  "FRAGMENTED")
          ((< c 100) "CRITICAL")
          (t         "BEYOND_REPAIR"))))

(defun neuro-u2-mem-signature-check (cpu)
  (let ((mem (neuro-cpu-memory cpu))
        (found nil))
    (dolist (sig neuro-u2-mem-signatures)
      (let ((hi (logand (ash sig -8) #xFF))
            (lo (logand sig #xFF)))
        (dotimes (i (- 65536 2))
          (when (and (= (aref mem i) lo)
                     (= (aref mem (1+ i)) hi))
            (push (cons sig i) found)))))
    found))

(provide 's0)
