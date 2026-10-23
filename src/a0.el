;;; a0.el -*- lexical-binding: t -*-

(require 'u0)

(defvar neuro-asm--labels nil)
(defvar neuro-asm--origin #x100)

(defun neuro-asm-set-origin (org) (setq neuro-asm--origin org))

(defun neuro-asm--parse-int (s)
  (cond
   ((string-prefix-p "0x" s) (string-to-number (substring s 2) 16))
   ((string-prefix-p "0X" s) (string-to-number (substring s 2) 16))
   ((string-prefix-p "$"  s) (string-to-number (substring s 1) 16))
   ((string-suffix-p "h"  s) (string-to-number (substring s 0 -1) 16))
   ((string-suffix-p "H"  s) (string-to-number (substring s 0 -1) 16))
   (t (string-to-number s))))

(defun neuro-asm--reg-idx (r)
  (pcase (downcase r)
    ("ax" 0) ("cx" 1) ("dx" 2) ("bx" 3)
    ("sp" 4) ("bp" 5) ("si" 6) ("di" 7)
    (_ -1)))

(defun neuro-asm--reg8-idx (r)
  (pcase (downcase r)
    ("al" 0) ("cl" 1) ("dl" 2) ("bl" 3)
    ("ah" 4) ("ch" 5) ("dh" 6) ("bh" 7)
    (_ -1)))

(defun neuro-asm--is-reg16 (s) (>= (neuro-asm--reg-idx s) 0))
(defun neuro-asm--is-reg8  (s) (>= (neuro-asm--reg8-idx s) 0))
(defun neuro-asm--is-reg   (s) (or (neuro-asm--is-reg16 s) (neuro-asm--is-reg8 s)))

(defun neuro-asm--encode-modrm-rr (dst-ri src-ri)

  (logior #xC0 (ash (logand src-ri 7) 3) (logand dst-ri 7)))

(defun neuro-asm--u16-bytes (v)
  (list (logand v #xFF) (logand (ash v -8) #xFF)))

(defun neuro-asm--resolve (s)
  (let ((label-val (cdr (assoc (upcase s) neuro-asm--labels))))
    (if label-val
        label-val
      (if (string-match-p "^[0-9$]\\|^0[xX]\\|[hH]$" s)
          (neuro-asm--parse-int s)
        (error "Undefined symbol: %s" s)))))

(defun neuro-asm--sbyte (v) (if (> v 127) (- v 256) v))
(defun neuro-asm--u8 (v) (logand v #xFF))

(defun neuro-asm--estimate-size (line)
  (let* ((clean (car (split-string line ";")))
         (parts (split-string (string-trim clean) "[ ,\t]+" t))
         (mnem  (upcase (or (car parts) "")))
         (ops   (cdr parts)))
    (pcase mnem
      ("NOP"   1) ("HLT"  1) ("RET"  1) ("IRET" 1)
      ("CLC"   1) ("STC"  1) ("CLI"  1) ("STI"  1)
      ("CLD"   1) ("STD"  1) ("CMC"  1)
      ("PUSHA" 1) ("POPA" 1) ("CBW"  1) ("CWD"  1)
      ("SAHF"  1) ("LAHF" 1) ("PUSHF" 1) ("POPF" 1)

      ((pred (lambda (m) (member m '("INC" "DEC" "PUSH" "POP"))))
       (if (and (car ops) (neuro-asm--is-reg16 (car ops))) 1 2))

      ("MOV"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond
          ((and dst src (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src)) 2)
          ((and dst src (neuro-asm--is-reg8  dst) (neuro-asm--is-reg8  src)) 2)
          ((and dst (neuro-asm--is-reg8  dst)) 2)
          (t 3))))

      ((pred (lambda (m) (member m '("ADD" "SUB" "CMP" "AND" "OR" "XOR"))))
       (let ((dst (car ops)) (src (cadr ops)))
         (if (and dst src (neuro-asm--is-reg dst) (neuro-asm--is-reg src)) 2 3)))

      ("JMP"  3)
      ("CALL" 3)

      ((pred (lambda (m) (string-prefix-p "J" m))) 2)
      ("LOOP" 2)
      ("INT"  2)
      ("XCHG" 2)
      ("IN"   2) ("OUT" 2)
      ("RCL"  3) ("RCR" 3) ("ROL" 3) ("ROR" 3)
      ("SHL"  3) ("SHR" 3) ("SAR" 3)
      ("LEA"  3)
      (_ 1))))

(defun neuro-asm--jcc-opcode (mnem)
  (pcase (upcase mnem)
    ("JO"  #x70) ("JNO" #x71) ("JC"  #x72) ("JNC" #x73)
    ("JZ"  #x74) ("JE"  #x74) ("JNZ" #x75) ("JNE" #x75)
    ("JBE" #x76) ("JA"  #x77) ("JS"  #x78) ("JNS" #x79)
    ("JP"  #x7A) ("JNP" #x7B) ("JL"  #x7C) ("JGE" #x7D)
    ("JLE" #x7E) ("JG"  #x7F)
    (_ nil)))

(defun neuro-asm--encode (line addr)
  (let* ((clean (car (split-string line ";")))
         (parts (split-string (string-trim clean) "[ ,\t]+" t))
         (mnem  (upcase (or (car parts) "")))
         (ops   (cdr parts)))
    (pcase mnem
      ("NOP"   (list #x90))

      ("HLT"   (list #xF4))
      ("RET"   (list #xC3))
      ("IRET"  (list #xCF))
      ("CLC"   (list #xF8)) ("STC" (list #xF9))
      ("CLI"   (list #xFA)) ("STI" (list #xFB))
      ("CLD"   (list #xFC)) ("STD" (list #xFD))
      ("CMC"   (list #xF5))
      ("PUSHA" (list #x60)) ("POPA" (list #x61))
      ("CBW"   (list #x98)) ("CWD"  (list #x99))
      ("PUSHF" (list #x9C)) ("POPF" (list #x9D))
      ("SAHF"  (list #x9E)) ("LAHF" (list #x9F))

      ("MOV"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond

          ((and (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src))
           (list #x89 (neuro-asm--encode-modrm-rr
                       (neuro-asm--reg-idx dst) (neuro-asm--reg-idx src))))

          ((neuro-asm--is-reg16 dst)
           (let ((ri (neuro-asm--reg-idx dst)))
             (append (list (+ #xB8 ri))
                     (neuro-asm--u16-bytes (neuro-asm--resolve src)))))

          ((neuro-asm--is-reg8 dst)
           (let ((ri (neuro-asm--reg8-idx dst)))
             (list (+ #xB0 ri) (logand (neuro-asm--resolve src) #xFF))))
          (t nil))))

      ("ADD"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond
          ((and (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src))
           (list #x01 (neuro-asm--encode-modrm-rr
                       (neuro-asm--reg-idx dst) (neuro-asm--reg-idx src))))
          ((equal (upcase dst) "AX")
           (append (list #x05) (neuro-asm--u16-bytes (neuro-asm--resolve src))))
          (t nil))))

      ("SUB"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond
          ((and (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src))
           (list #x29 (neuro-asm--encode-modrm-rr
                       (neuro-asm--reg-idx dst) (neuro-asm--reg-idx src))))
          ((equal (upcase dst) "AX")
           (append (list #x2D) (neuro-asm--u16-bytes (neuro-asm--resolve src))))
          (t nil))))

      ("CMP"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond
          ((and (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src))
           (list #x39 (neuro-asm--encode-modrm-rr
                       (neuro-asm--reg-idx dst) (neuro-asm--reg-idx src))))
          ((equal (upcase dst) "AX")
           (append (list #x3D) (neuro-asm--u16-bytes (neuro-asm--resolve src))))
          (t nil))))

      ("AND"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond
          ((and (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src))
           (list #x21 (neuro-asm--encode-modrm-rr
                       (neuro-asm--reg-idx dst) (neuro-asm--reg-idx src))))
          ((equal (upcase dst) "AX")
           (append (list #x25) (neuro-asm--u16-bytes (neuro-asm--resolve src))))
          (t nil))))

      ("OR"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond
          ((and (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src))
           (list #x09 (neuro-asm--encode-modrm-rr
                       (neuro-asm--reg-idx dst) (neuro-asm--reg-idx src))))
          ((equal (upcase dst) "AX")
           (append (list #x0D) (neuro-asm--u16-bytes (neuro-asm--resolve src))))
          (t nil))))

      ("XOR"
       (let ((dst (car ops)) (src (cadr ops)))
         (cond
          ((and (neuro-asm--is-reg16 dst) (neuro-asm--is-reg16 src))
           (list #x31 (neuro-asm--encode-modrm-rr
                       (neuro-asm--reg-idx dst) (neuro-asm--reg-idx src))))
          ((equal (upcase dst) "AX")
           (append (list #x35) (neuro-asm--u16-bytes (neuro-asm--resolve src))))
          (t nil))))

      ("INC"
       (let ((r16 (neuro-asm--reg-idx  (car ops)))
             (r8  (neuro-asm--reg8-idx (car ops))))
         (cond ((>= r16 0) (list (+ #x40 r16)))
               ((>= r8  0) (list #xFE (logior #xC0 r8)))
               (t nil))))

      ("DEC"
       (let ((r16 (neuro-asm--reg-idx  (car ops)))
             (r8  (neuro-asm--reg8-idx (car ops))))
         (cond ((>= r16 0) (list (+ #x48 r16)))
               ((>= r8  0) (list #xFE (logior #xC8 r8)))
               (t nil))))

      ("PUSH"
       (let ((r (neuro-asm--reg-idx (car ops))))
         (if (>= r 0) (list (+ #x50 r)) nil)))

      ("POP"
       (let ((r (neuro-asm--reg-idx (car ops))))
         (if (>= r 0) (list (+ #x58 r)) nil)))

      ("JMP"
       (let* ((target (neuro-asm--resolve (car ops)))
              (rel8   (- target (+ addr 2)))
              (rel16  (- target (+ addr 3))))

         (if (and (>= rel8 -128) (<= rel8 127))
             (list #xEB (logand rel8 #xFF))
           (append (list #xE9) (neuro-asm--u16-bytes (logand rel16 #xFFFF))))))

      ("CALL"
       (let* ((target (neuro-asm--resolve (car ops)))
              (rel16  (- target (+ addr 3))))
         (append (list #xE8) (neuro-asm--u16-bytes (logand rel16 #xFFFF)))))

      ((pred (lambda (m) (neuro-asm--jcc-opcode m)))
       (let* ((opc    (neuro-asm--jcc-opcode mnem))
              (target (neuro-asm--resolve (car ops)))

              (rel    (- target (+ addr 2))))
         (unless (and (>= rel -128) (<= rel 127))
           (error "Conditional jump out of range: %d" rel))
         (list opc (logand rel #xFF))))

      ("LOOP"
       (let* ((target (neuro-asm--resolve (car ops)))
              (rel    (- target (+ addr 2))))
         (list #xE2 (logand rel #xFF))))

      ("INT"
       (list #xCD (logand (neuro-asm--resolve (car ops)) #xFF)))

      ("XCHG"

       (let* ((a (neuro-asm--reg-idx (car ops)))
              (b (neuro-asm--reg-idx (cadr ops))))
         (cond
          ((and (= a 0) (= b 0)) (list #x90))
          ((= a 0) (list (+ #x91 (1- b))))
          ((= b 0) (list (+ #x91 (1- a))))
          (t (list #x87 (neuro-asm--encode-modrm-rr a b))))))

      ("IN"
       (let ((port (neuro-asm--resolve (cadr ops))))
         (list #xE4 (logand port #xFF))))
      ("OUT"
       (let ((port (neuro-asm--resolve (car ops))))
         (list #xE6 (logand port #xFF))))

      ((pred (lambda (m) (member m '("SHL" "SHR" "SAR" "ROL" "ROR" "RCL" "RCR"))))
       (let* ((rm-ri (neuro-asm--reg-idx (car ops)))
              (ext (pcase (upcase mnem)
                     ("ROL" 0) ("ROR" 1) ("RCL" 2) ("RCR" 3)
                     ("SHL" 4) ("SHR" 5) ("SAR" 7)
                     (_ 4)))
              (modrm (logior #xC0 (ash ext 3) (logand rm-ri 7))))
         (if (equal (upcase (cadr ops)) "CL")
             (list #xD3 modrm)
           (let ((cnt (logand (neuro-asm--resolve (cadr ops)) #xFF)))
             (list #xC1 modrm cnt)))))

      (_ nil))))

(defun neuro-asm--split-label-instr (line)
  "Return (label . rest) or (nil . line)."

  (if (string-match "^[[:space:]]*\\([A-Za-z_][A-Za-z0-9_]*\\)[[:space:]]*:\\(.*\\)$" line)
      (cons (string-trim (match-string 1 line))
            (string-trim (match-string 2 line)))
    (cons nil line)))

(defun neuro-asm-assemble (source)
  (setq neuro-asm--labels nil)
  (let* ((raw-lines (split-string source "\n"))

         (pairs
          (let (result)
            (dolist (l raw-lines)
              (let* ((trimmed (string-trim l))
                     (no-comment (car (split-string trimmed ";"))))
                (unless (string-empty-p (string-trim no-comment))
                  (push (neuro-asm--split-label-instr
                         (string-trim no-comment))
                        result))))
            (nreverse result))))

    (let ((addr neuro-asm--origin))
      (dolist (pair pairs)
        (let ((label (car pair))
              (instr (cdr pair)))
          (when label
            (push (cons (upcase label) addr) neuro-asm--labels))
          (unless (string-empty-p instr)
            (setq addr (+ addr (neuro-asm--estimate-size instr))))))

    (let ((code nil)
          (addr neuro-asm--origin))
      (dolist (pair pairs)
        (let ((instr (cdr pair)))
          (unless (string-empty-p instr)
            (let ((bytes (condition-case err
                             (neuro-asm--encode instr addr)
                           (error
                            (message "asm error at %04X [%s]: %s"
                                     addr instr (cadr err))
                            nil))))
              (if bytes
                  (progn
                    (setq code (append code bytes))
                    (setq addr (+ addr (length bytes))))

                (message "asm: unsupported instruction [%s] at %04X (skipped)"
                         instr addr))))))
      (vconcat code)))))

(provide 'a0)
