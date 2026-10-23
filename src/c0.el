;;; c0.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'u0)

(defvar --cl-block-neuro-cpu-step-- nil)
(defvar --cl-block-neuro-cpu--rep-- nil)

(defconst neuro-FLAG-CF #x0001)
(defconst neuro-FLAG-PF #x0004)
(defconst neuro-FLAG-AF #x0010)
(defconst neuro-FLAG-ZF #x0040)
(defconst neuro-FLAG-SF #x0080)
(defconst neuro-FLAG-TF #x0100)
(defconst neuro-FLAG-IF #x0200)
(defconst neuro-FLAG-DF #x0400)
(defconst neuro-FLAG-OF #x0800)
(defconst neuro-FLAGS-ALWAYS1 #x0002)
(defconst neuro-FLAGS-VALID   #x0FD7)

(cl-defstruct neuro-regs ax bx cx dx si di sp bp ip flags cs ds es ss)

(defun neuro-regs-new ()
  (make-neuro-regs
   :ax 0 :bx 0 :cx 0 :dx 0 :si 0 :di 0
   :sp #xFFFE :bp 0 :ip 0
   :flags (logior neuro-FLAGS-ALWAYS1 neuro-FLAG-IF)
   :cs #xF000 :ds 0 :es 0 :ss 0))

(defun neuro-regs-dump (r)
  (format "AX=%04X BX=%04X CX=%04X DX=%04X SI=%04X DI=%04X SP=%04X BP=%04X IP=%04X FL=%04X"
          (neuro-regs-ax r) (neuro-regs-bx r) (neuro-regs-cx r) (neuro-regs-dx r)
          (neuro-regs-si r) (neuro-regs-di r) (neuro-regs-sp r) (neuro-regs-bp r)
          (neuro-regs-ip r) (neuro-regs-flags r)))

(defun neuro-regs-flag-string (r)
  (let ((f (neuro-regs-flags r)) (s ""))
    (when (/= 0 (logand f neuro-FLAG-CF)) (setq s (concat s "CF ")))
    (when (/= 0 (logand f neuro-FLAG-PF)) (setq s (concat s "PF ")))
    (when (/= 0 (logand f neuro-FLAG-AF)) (setq s (concat s "AF ")))
    (when (/= 0 (logand f neuro-FLAG-ZF)) (setq s (concat s "ZF ")))
    (when (/= 0 (logand f neuro-FLAG-SF)) (setq s (concat s "SF ")))
    (when (/= 0 (logand f neuro-FLAG-TF)) (setq s (concat s "TF ")))
    (when (/= 0 (logand f neuro-FLAG-IF)) (setq s (concat s "IF ")))
    (when (/= 0 (logand f neuro-FLAG-DF)) (setq s (concat s "DF ")))
    (when (/= 0 (logand f neuro-FLAG-OF)) (setq s (concat s "OF ")))
    s))

(defun neuro-regs-get-flag (r flag)
  (/= 0 (logand (neuro-regs-flags r) flag)))

(defun neuro-regs-set-flag (r flag on)
  (if on
      (setf (neuro-regs-flags r)
            (logior (neuro-regs-flags r) (logand flag #xFFFF) neuro-FLAGS-ALWAYS1))
    (setf (neuro-regs-flags r)
          (logior (logand (neuro-regs-flags r)
                          (logand (lognot (logand flag #xFFFF)) #xFFFF))
                  neuro-FLAGS-ALWAYS1))))

(defun neuro-u16  (v) (logand v #xFFFF))
(defun neuro-u8   (v) (logand v #xFF))
(defun neuro-hi8  (v) (logand (ash v -8) #xFF))
(defun neuro-lo8  (v) (logand v #xFF))
(defun neuro-make16 (hi lo) (logior (ash (logand hi #xFF) 8) (logand lo #xFF)))
(defun neuro-sbyte (b) (if (> (logand b #xFF) 127) (- (logand b #xFF) 256) (logand b #xFF)))
(defun neuro-sword (w) (let ((v (neuro-u16 w))) (if (> v #x7FFF) (- v #x10000) v)))

(defun neuro-regs-get (r name)
  (pcase name
    ("ax" (neuro-regs-ax r)) ("bx" (neuro-regs-bx r))
    ("cx" (neuro-regs-cx r)) ("dx" (neuro-regs-dx r))
    ("si" (neuro-regs-si r)) ("di" (neuro-regs-di r))
    ("sp" (neuro-regs-sp r)) ("bp" (neuro-regs-bp r))
    ("ip" (neuro-regs-ip r)) ("flags" (neuro-regs-flags r))
    ("cs" (neuro-regs-cs r)) ("ds" (neuro-regs-ds r))
    ("es" (neuro-regs-es r)) ("ss" (neuro-regs-ss r))
    (_ 0)))

(defun neuro-regs-set (r name val)
  (pcase name
    ("ax" (setf (neuro-regs-ax r) (neuro-u16 val)))
    ("bx" (setf (neuro-regs-bx r) (neuro-u16 val)))
    ("cx" (setf (neuro-regs-cx r) (neuro-u16 val)))
    ("dx" (setf (neuro-regs-dx r) (neuro-u16 val)))
    ("si" (setf (neuro-regs-si r) (neuro-u16 val)))
    ("di" (setf (neuro-regs-di r) (neuro-u16 val)))
    ("sp" (setf (neuro-regs-sp r) (neuro-u16 val)))
    ("bp" (setf (neuro-regs-bp r) (neuro-u16 val)))
    ("ip" (setf (neuro-regs-ip r) (neuro-u16 val)))))

(cl-defstruct neuro-cpu regs memory halted cycles irq-pending seg-prefix)

(defun neuro-cpu-new ()
  (make-neuro-cpu
   :regs (neuro-regs-new)
   :memory (make-vector 65536 0)
   :halted nil :cycles 0 :irq-pending nil :seg-prefix nil))

(defun neuro-cpu-new-segs (cs ds es ss sp ip)
  (let ((cpu (neuro-cpu-new)))
    (setf (neuro-regs-cs (neuro-cpu-regs cpu)) cs
          (neuro-regs-ds (neuro-cpu-regs cpu)) ds
          (neuro-regs-es (neuro-cpu-regs cpu)) es
          (neuro-regs-ss (neuro-cpu-regs cpu)) ss
          (neuro-regs-sp (neuro-cpu-regs cpu)) sp
          (neuro-regs-ip (neuro-cpu-regs cpu)) ip)
    cpu))

(defun neuro-cpu--seg-off (seg off)
  (logand (+ (ash (logand seg #xFFFF) 4) (logand off #xFFFF)) #xFFFFF))

(defun neuro-cpu--mem-read8 (cpu seg off)
  (let ((addr (logand (neuro-cpu--seg-off seg off) #xFFFF)))
    (aref (neuro-cpu-memory cpu) addr)))

(defun neuro-cpu--mem-write8 (cpu seg off val)

  (when (and (boundp 'neuro-sandbox-active) neuro-sandbox-active
             (fboundp 'neuro-sandbox-check-mem))
    (neuro-sandbox-check-mem 1))
  (let ((addr (logand (neuro-cpu--seg-off seg off) #xFFFF)))
    (aset (neuro-cpu-memory cpu) addr (neuro-u8 val))))

(defun neuro-cpu--mem-read16 (cpu seg off)
  (let ((lo (neuro-cpu--mem-read8 cpu seg off))
        (hi (neuro-cpu--mem-read8 cpu seg (neuro-u16 (1+ off)))))
    (neuro-make16 hi lo)))

(defun neuro-cpu--mem-write16 (cpu seg off val)
  (neuro-cpu--mem-write8 cpu seg off       (neuro-lo8 val))
  (neuro-cpu--mem-write8 cpu seg (neuro-u16 (1+ off)) (neuro-hi8 val)))

(defun neuro-cpu-fetch (cpu)
  (let* ((r   (neuro-cpu-regs cpu))
         (cs  (neuro-regs-cs r))
         (ip  (neuro-regs-ip r))
         (b   (neuro-cpu--mem-read8 cpu cs ip)))
    (setf (neuro-regs-ip r) (neuro-u16 (1+ ip)))
    b))

(defun neuro-cpu-fetch16 (cpu)

  (let* ((lo (neuro-cpu-fetch cpu))
         (hi (neuro-cpu-fetch cpu)))
    (neuro-make16 hi lo)))

(defun neuro-cpu-push (cpu val)
  (let* ((r  (neuro-cpu-regs cpu))
         (sp (neuro-regs-sp r))
         (ss (neuro-regs-ss r)))
    (when (< sp 2) (error "Stack overflow at SS=%04X SP=%04X" ss sp))
    (setq sp (neuro-u16 (- sp 2)))
    (setf (neuro-regs-sp r) sp)
    (neuro-cpu--mem-write8 cpu ss sp       (neuro-lo8 val))
    (neuro-cpu--mem-write8 cpu ss (neuro-u16 (1+ sp)) (neuro-hi8 val))))

(defun neuro-cpu-pop (cpu)
  (let* ((r  (neuro-cpu-regs cpu))
         (sp (neuro-regs-sp r))
         (ss (neuro-regs-ss r)))
    (when (> sp #xFFFE) (error "Stack underflow at SS=%04X SP=%04X" ss sp))
    (let ((val (neuro-make16
                (neuro-cpu--mem-read8 cpu ss (neuro-u16 (1+ sp)))
                (neuro-cpu--mem-read8 cpu ss sp))))
      (setf (neuro-regs-sp r) (neuro-u16 (+ sp 2)))
      val)))

(defun neuro-cpu-reg16 (cpu idx)
  (let ((r (neuro-cpu-regs cpu)))
    (pcase (logand idx 7)
      (0 (neuro-regs-ax r)) (1 (neuro-regs-cx r))
      (2 (neuro-regs-dx r)) (3 (neuro-regs-bx r))
      (4 (neuro-regs-sp r)) (5 (neuro-regs-bp r))
      (6 (neuro-regs-si r)) (_ (neuro-regs-di r)))))

(defun neuro-cpu-set-reg16 (cpu idx val)
  (let ((r (neuro-cpu-regs cpu)) (v (neuro-u16 val)))
    (pcase (logand idx 7)
      (0 (setf (neuro-regs-ax r) v)) (1 (setf (neuro-regs-cx r) v))
      (2 (setf (neuro-regs-dx r) v)) (3 (setf (neuro-regs-bx r) v))
      (4 (setf (neuro-regs-sp r) v)) (5 (setf (neuro-regs-bp r) v))
      (6 (setf (neuro-regs-si r) v)) (_ (setf (neuro-regs-di r) v)))))

(defun neuro-cpu-reg8-get (cpu idx)
  (let ((r (neuro-cpu-regs cpu)))
    (pcase (logand idx 7)
      (0 (neuro-lo8 (neuro-regs-ax r)))
      (1 (neuro-lo8 (neuro-regs-cx r)))
      (2 (neuro-lo8 (neuro-regs-dx r)))
      (3 (neuro-lo8 (neuro-regs-bx r)))
      (4 (neuro-hi8 (neuro-regs-ax r)))
      (5 (neuro-hi8 (neuro-regs-cx r)))
      (6 (neuro-hi8 (neuro-regs-dx r)))
      (_ (neuro-hi8 (neuro-regs-bx r))))))

(defun neuro-cpu-reg8-set (cpu idx val)
  (let ((r (neuro-cpu-regs cpu)) (v (neuro-u8 val)))
    (pcase (logand idx 7)
      (0 (setf (neuro-regs-ax r) (logior (logand (neuro-regs-ax r) #xFF00) v)))
      (1 (setf (neuro-regs-cx r) (logior (logand (neuro-regs-cx r) #xFF00) v)))
      (2 (setf (neuro-regs-dx r) (logior (logand (neuro-regs-dx r) #xFF00) v)))
      (3 (setf (neuro-regs-bx r) (logior (logand (neuro-regs-bx r) #xFF00) v)))
      (4 (setf (neuro-regs-ax r) (logior (logand (neuro-regs-ax r) #x00FF) (ash v 8))))
      (5 (setf (neuro-regs-cx r) (logior (logand (neuro-regs-cx r) #x00FF) (ash v 8))))
      (6 (setf (neuro-regs-dx r) (logior (logand (neuro-regs-dx r) #x00FF) (ash v 8))))
      (_ (setf (neuro-regs-bx r) (logior (logand (neuro-regs-bx r) #x00FF) (ash v 8)))))))

(defun neuro-cpu--ea (cpu mod rm disp)
  (let* ((r   (neuro-cpu-regs cpu))
         (seg (or (neuro-cpu-seg-prefix cpu)
                  (neuro-regs-ds r)))
         (base
          (pcase rm
            (0 (neuro-u16 (+ (neuro-regs-bx r) (neuro-regs-si r))))
            (1 (neuro-u16 (+ (neuro-regs-bx r) (neuro-regs-di r))))
            (2 (progn (setq seg (neuro-regs-ss r))
                      (neuro-u16 (+ (neuro-regs-bp r) (neuro-regs-si r)))))
            (3 (progn (setq seg (neuro-regs-ss r))
                      (neuro-u16 (+ (neuro-regs-bp r) (neuro-regs-di r)))))
            (4 (neuro-regs-si r))
            (5 (neuro-regs-di r))
            (6 (if (= mod 0)
                   (progn (setq seg (neuro-regs-ds r)) 0)
                 (progn (setq seg (neuro-regs-ss r))
                        (neuro-regs-bp r))))
            (_ (neuro-regs-bx r)))))
    (cons seg (neuro-u16 (+ base disp)))))

(defun neuro-cpu--read-modrm (cpu is-word)
  (let* ((b    (neuro-cpu-fetch cpu))
         (mod  (logand (ash b -6) 3))
         (reg  (logand (ash b -3) 7))
         (rm   (logand b 7)))
    (pcase mod
      (3

       (let ((get-fn (if is-word
                         (lambda () (neuro-cpu-reg16 cpu rm))
                       (lambda () (neuro-cpu-reg8-get cpu rm))))
             (set-fn (if is-word
                         (lambda (v) (neuro-cpu-set-reg16 cpu rm v))
                       (lambda (v) (neuro-cpu-reg8-set cpu rm v)))))
         (list :mod mod :reg reg :rm rm :get-fn get-fn :set-fn set-fn :ea nil)))
      (_

       (let* ((disp (pcase mod
                      (0 (if (= rm 6) (neuro-cpu-fetch16 cpu) 0))
                      (1 (neuro-sbyte (neuro-cpu-fetch cpu)))
                      (2 (neuro-sword (neuro-cpu-fetch16 cpu)))
                      (_ 0)))
              (ea   (neuro-cpu--ea cpu mod rm disp))
              (eseg (car ea))
              (eoff (cdr ea))
              (get-fn (if is-word
                          (lambda () (neuro-cpu--mem-read16 cpu eseg eoff))
                        (lambda () (neuro-cpu--mem-read8 cpu eseg eoff))))
              (set-fn (if is-word
                          (lambda (v) (neuro-cpu--mem-write16 cpu eseg eoff v))
                        (lambda (v) (neuro-cpu--mem-write8 cpu eseg eoff v)))))
         (list :mod mod :reg reg :rm rm :get-fn get-fn :set-fn set-fn
               :ea (cons eseg eoff)))))))

(defun neuro-cpu--reg-fn (cpu reg is-word)
  (if is-word
      (cons (lambda () (neuro-cpu-reg16 cpu reg))
            (lambda (v) (neuro-cpu-set-reg16 cpu reg v)))
    (cons (lambda () (neuro-cpu-reg8-get cpu reg))
          (lambda (v) (neuro-cpu-reg8-set cpu reg v)))))

(defun neuro-flags-update-add (r a b res)
  (let ((a16 (neuro-u16 a)) (b16 (neuro-u16 b)) (r16 (neuro-u16 res)))
    (neuro-regs-set-flag r neuro-FLAG-CF (> (+ a16 b16) #xFFFF))
    (neuro-regs-set-flag r neuro-FLAG-ZF (= r16 0))
    (neuro-regs-set-flag r neuro-FLAG-SF (/= 0 (logand r16 #x8000)))
    (neuro-regs-set-flag r neuro-FLAG-PF (neuro-parity (neuro-lo8 r16)))
    (neuro-regs-set-flag r neuro-FLAG-OF
                          (and (= 0 (logand (logxor a16 b16) #x8000))
                               (/= 0 (logand (logxor a16 r16) #x8000))))
    (neuro-regs-set-flag r neuro-FLAG-AF
                          (> (+ (logand a16 #xF) (logand b16 #xF)) #xF))))

(defun neuro-flags-update-sub (r a b res)
  (let ((a16 (neuro-u16 a)) (b16 (neuro-u16 b)) (r16 (neuro-u16 res)))
    (neuro-regs-set-flag r neuro-FLAG-CF (< a16 b16))
    (neuro-regs-set-flag r neuro-FLAG-ZF (= r16 0))
    (neuro-regs-set-flag r neuro-FLAG-SF (/= 0 (logand r16 #x8000)))
    (neuro-regs-set-flag r neuro-FLAG-PF (neuro-parity (neuro-lo8 r16)))
    (neuro-regs-set-flag r neuro-FLAG-OF
                          (and (/= 0 (logand (logxor a16 b16) #x8000))
                               (/= 0 (logand (logxor a16 r16) #x8000))))
    (neuro-regs-set-flag r neuro-FLAG-AF
                          (< (logand a16 #xF) (logand b16 #xF)))))

(defun neuro-flags-update-logic (r res)
  (let ((r16 (neuro-u16 res)))
    (neuro-regs-set-flag r neuro-FLAG-CF nil)
    (neuro-regs-set-flag r neuro-FLAG-OF nil)
    (neuro-regs-set-flag r neuro-FLAG-ZF (= r16 0))
    (neuro-regs-set-flag r neuro-FLAG-SF (/= 0 (logand r16 #x8000)))
    (neuro-regs-set-flag r neuro-FLAG-PF (neuro-parity (neuro-lo8 r16)))
    (neuro-regs-set-flag r neuro-FLAG-AF nil)))

(defun neuro-flags-update-inc (r a res)
  (let ((a16 (neuro-u16 a)) (r16 (neuro-u16 res)))
    (neuro-regs-set-flag r neuro-FLAG-ZF (= r16 0))
    (neuro-regs-set-flag r neuro-FLAG-SF (/= 0 (logand r16 #x8000)))
    (neuro-regs-set-flag r neuro-FLAG-PF (neuro-parity (neuro-lo8 r16)))
    (neuro-regs-set-flag r neuro-FLAG-OF (and (= a16 #x7FFF) (= r16 #x8000)))
    (neuro-regs-set-flag r neuro-FLAG-AF (= (logand r16 #xF) 0))))

(defun neuro-flags-update-dec (r a res)
  (let ((a16 (neuro-u16 a)) (r16 (neuro-u16 res)))
    (neuro-regs-set-flag r neuro-FLAG-ZF (= r16 0))
    (neuro-regs-set-flag r neuro-FLAG-SF (/= 0 (logand r16 #x8000)))
    (neuro-regs-set-flag r neuro-FLAG-PF (neuro-parity (neuro-lo8 r16)))
    (neuro-regs-set-flag r neuro-FLAG-OF (and (= a16 #x8000) (= r16 #x7FFF)))
    (neuro-regs-set-flag r neuro-FLAG-AF (= (logand a16 #xF) 0))))

(defun neuro-cpu--seg-override (cpu prefix-byte)
  (let ((r (neuro-cpu-regs cpu)))
    (pcase prefix-byte
      (#x26 (setf (neuro-cpu-seg-prefix cpu) (neuro-regs-es r)))
      (#x2E (setf (neuro-cpu-seg-prefix cpu) (neuro-regs-cs r)))
      (#x36 (setf (neuro-cpu-seg-prefix cpu) (neuro-regs-ss r)))
      (#x3E (setf (neuro-cpu-seg-prefix cpu) (neuro-regs-ds r))))))

(defvar neuro-cpu--idt nil

(defun neuro-cpu--idt-init ()
  (setq neuro-cpu--idt (make-vector 256 nil))
  (dotimes (i 256)
    (aset neuro-cpu--idt i (cons #xF000 (* i 4)))))

(defun neuro-cpu--idt-set (vec seg off)
  (when neuro-cpu--idt
    (aset neuro-cpu--idt vec (cons seg off))))

(defun neuro-cpu--do-int (cpu vec)
  (let* ((r    (neuro-cpu-regs cpu))
         (entry (when neuro-cpu--idt (aref neuro-cpu--idt vec)))
         (new-cs  (if entry (car entry) #xF000))
         (new-ip  (if entry (cdr entry) (* vec 4))))

    (neuro-cpu-push cpu (neuro-regs-flags r))
    (neuro-cpu-push cpu (neuro-regs-cs r))
    (neuro-cpu-push cpu (neuro-regs-ip r))

    (neuro-regs-set-flag r neuro-FLAG-IF nil)
    (neuro-regs-set-flag r neuro-FLAG-TF nil)

    (setf (neuro-regs-cs r) new-cs)
    (setf (neuro-regs-ip r) (neuro-u16 new-ip))
    (setf (neuro-cpu-irq-pending cpu) vec)))

(defun neuro-cpu--check-tf (cpu)
  (when (neuro-regs-get-flag (neuro-cpu-regs cpu) neuro-FLAG-TF)
    (neuro-cpu--do-int cpu 1)))

(defun neuro-cpu-step (cpu)
  (when (neuro-cpu-halted cpu) (error "CPU halted"))
  (setf (neuro-cpu-seg-prefix cpu) nil)
  (let* ((op (neuro-cpu-fetch cpu))
         (r  (neuro-cpu-regs cpu)))
    (cl-incf (neuro-cpu-cycles cpu))
    (pcase op

      ((pred (lambda (x) (member x '(#x26 #x2E #x36 #x3E))))
       (neuro-cpu--seg-override cpu op)
       (neuro-cpu-step cpu)
       (setf (neuro-cpu-seg-prefix cpu) nil)
       (cl-return-from neuro-cpu-step nil))

      (#x90 nil)
      ((pred (lambda (x) (and (>= x #x91) (<= x #x97))))
       (let ((ri (- op #x90)) (tmp (neuro-regs-ax r)))
         (setf (neuro-regs-ax r) (neuro-cpu-reg16 cpu ri))
         (neuro-cpu-set-reg16 cpu ri tmp)))

      (#x00
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (a  (funcall (plist-get m :get-fn)))
              (b  (funcall (car rv)))
              (res (neuro-u8 (+ a b))))
         (neuro-flags-update-add r a b res)
         (funcall (plist-get m :set-fn) res)))
      (#x01
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (a  (funcall (plist-get m :get-fn)))
              (b  (funcall (car rv)))
              (res (neuro-u16 (+ a b))))
         (neuro-flags-update-add r a b res)
         (funcall (plist-get m :set-fn) res)))
      (#x02
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (a  (funcall (car rv)))
              (b  (funcall (plist-get m :get-fn)))
              (res (neuro-u8 (+ a b))))
         (neuro-flags-update-add r a b res)
         (funcall (cdr rv) res)))
      (#x03
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (a  (funcall (car rv)))
              (b  (funcall (plist-get m :get-fn)))
              (res (neuro-u16 (+ a b))))
         (neuro-flags-update-add r a b res)
         (funcall (cdr rv) res)))
      (#x04
       (let* ((imm (neuro-cpu-fetch cpu))
              (a   (neuro-lo8 (neuro-regs-ax r)))
              (res (neuro-u8 (+ a imm))))
         (neuro-flags-update-add r a imm res)
         (setf (neuro-regs-ax r) (logior (logand (neuro-regs-ax r) #xFF00) res))))
      (#x05
       (let* ((imm (neuro-cpu-fetch16 cpu))
              (a   (neuro-regs-ax r))
              (res (neuro-u16 (+ a imm))))
         (neuro-flags-update-add r a imm res)
         (setf (neuro-regs-ax r) res)))

      (#x08
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (res (neuro-u8 (logior (funcall (plist-get m :get-fn)) (funcall (car rv))))))
         (neuro-flags-update-logic r res)
         (funcall (plist-get m :set-fn) res)))
      (#x09
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (res (neuro-u16 (logior (funcall (plist-get m :get-fn)) (funcall (car rv))))))
         (neuro-flags-update-logic r res)
         (funcall (plist-get m :set-fn) res)))
      (#x0A
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (res (neuro-u8 (logior (funcall (car rv)) (funcall (plist-get m :get-fn))))))
         (neuro-flags-update-logic r res)
         (funcall (cdr rv) res)))
      (#x0B
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (res (neuro-u16 (logior (funcall (car rv)) (funcall (plist-get m :get-fn))))))
         (neuro-flags-update-logic r res)
         (funcall (cdr rv) res)))
      (#x0C
       (let* ((imm (neuro-cpu-fetch cpu))
              (res (neuro-u8 (logior (neuro-lo8 (neuro-regs-ax r)) imm))))
         (neuro-flags-update-logic r res)
         (setf (neuro-regs-ax r) (logior (logand (neuro-regs-ax r) #xFF00) res))))
      (#x0D
       (let* ((imm (neuro-cpu-fetch16 cpu))
              (res (neuro-u16 (logior (neuro-regs-ax r) imm))))
         (neuro-flags-update-logic r res)
         (setf (neuro-regs-ax r) res)))

      (#x20
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (res (neuro-u8 (logand (funcall (plist-get m :get-fn)) (funcall (car rv))))))
         (neuro-flags-update-logic r res)
         (funcall (plist-get m :set-fn) res)))
      (#x21
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (res (neuro-u16 (logand (funcall (plist-get m :get-fn)) (funcall (car rv))))))
         (neuro-flags-update-logic r res)
         (funcall (plist-get m :set-fn) res)))
      (#x22
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (res (neuro-u8 (logand (funcall (car rv)) (funcall (plist-get m :get-fn))))))
         (neuro-flags-update-logic r res)
         (funcall (cdr rv) res)))
      (#x23
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (res (neuro-u16 (logand (funcall (car rv)) (funcall (plist-get m :get-fn))))))
         (neuro-flags-update-logic r res)
         (funcall (cdr rv) res)))
      (#x24
       (let* ((imm (neuro-cpu-fetch cpu))
              (res (neuro-u8 (logand (neuro-lo8 (neuro-regs-ax r)) imm))))
         (neuro-flags-update-logic r res)
         (setf (neuro-regs-ax r) (logior (logand (neuro-regs-ax r) #xFF00) res))))
      (#x25
       (let* ((imm (neuro-cpu-fetch16 cpu))
              (res (neuro-u16 (logand (neuro-regs-ax r) imm))))
         (neuro-flags-update-logic r res)
         (setf (neuro-regs-ax r) res)))

      (#x28
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (a (funcall (plist-get m :get-fn))) (b (funcall (car rv)))
              (res (neuro-u8 (- a b))))
         (neuro-flags-update-sub r a b res)
         (funcall (plist-get m :set-fn) res)))
      (#x29
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (a (funcall (plist-get m :get-fn))) (b (funcall (car rv)))
              (res (neuro-u16 (- a b))))
         (neuro-flags-update-sub r a b res)
         (funcall (plist-get m :set-fn) res)))
      (#x2A
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (a (funcall (car rv))) (b (funcall (plist-get m :get-fn)))
              (res (neuro-u8 (- a b))))
         (neuro-flags-update-sub r a b res)
         (funcall (cdr rv) res)))
      (#x2B
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (a (funcall (car rv))) (b (funcall (plist-get m :get-fn)))
              (res (neuro-u16 (- a b))))
         (neuro-flags-update-sub r a b res)
         (funcall (cdr rv) res)))
      (#x2C
       (let* ((imm (neuro-cpu-fetch cpu))
              (a (neuro-lo8 (neuro-regs-ax r)))
              (res (neuro-u8 (- a imm))))
         (neuro-flags-update-sub r a imm res)
         (setf (neuro-regs-ax r) (logior (logand (neuro-regs-ax r) #xFF00) res))))
      (#x2D
       (let* ((imm (neuro-cpu-fetch16 cpu)) (a (neuro-regs-ax r))
              (res (neuro-u16 (- a imm))))
         (neuro-flags-update-sub r a imm res)
         (setf (neuro-regs-ax r) res)))

      (#x30
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (res (neuro-u8 (logxor (funcall (plist-get m :get-fn)) (funcall (car rv))))))
         (neuro-flags-update-logic r res)
         (funcall (plist-get m :set-fn) res)))
      (#x31
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (res (neuro-u16 (logxor (funcall (plist-get m :get-fn)) (funcall (car rv))))))
         (neuro-flags-update-logic r res)
         (funcall (plist-get m :set-fn) res)))
      (#x32
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (res (neuro-u8 (logxor (funcall (car rv)) (funcall (plist-get m :get-fn))))))
         (neuro-flags-update-logic r res)
         (funcall (cdr rv) res)))
      (#x33
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (res (neuro-u16 (logxor (funcall (car rv)) (funcall (plist-get m :get-fn))))))
         (neuro-flags-update-logic r res)
         (funcall (cdr rv) res)))
      (#x34
       (let* ((imm (neuro-cpu-fetch cpu))
              (res (neuro-u8 (logxor (neuro-lo8 (neuro-regs-ax r)) imm))))
         (neuro-flags-update-logic r res)
         (setf (neuro-regs-ax r) (logior (logand (neuro-regs-ax r) #xFF00) res))))
      (#x35
       (let* ((imm (neuro-cpu-fetch16 cpu))
              (res (neuro-u16 (logxor (neuro-regs-ax r) imm))))
         (neuro-flags-update-logic r res)
         (setf (neuro-regs-ax r) res)))

      (#x38
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (a (funcall (plist-get m :get-fn))) (b (funcall (car rv))))
         (neuro-flags-update-sub r a b (neuro-u8 (- a b)))))
      (#x39
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (a (funcall (plist-get m :get-fn))) (b (funcall (car rv))))
         (neuro-flags-update-sub r a b (neuro-u16 (- a b)))))
      (#x3A
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (a (funcall (car rv))) (b (funcall (plist-get m :get-fn))))
         (neuro-flags-update-sub r a b (neuro-u8 (- a b)))))
      (#x3B
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (a (funcall (car rv))) (b (funcall (plist-get m :get-fn))))
         (neuro-flags-update-sub r a b (neuro-u16 (- a b)))))
      (#x3C
       (let* ((imm (neuro-cpu-fetch cpu)) (a (neuro-lo8 (neuro-regs-ax r))))
         (neuro-flags-update-sub r a imm (neuro-u8 (- a imm)))))
      (#x3D
       (let* ((imm (neuro-cpu-fetch16 cpu)) (a (neuro-regs-ax r)))
         (neuro-flags-update-sub r a imm (neuro-u16 (- a imm)))))

      ((pred (lambda (x) (and (>= x #x40) (<= x #x47))))
       (let* ((ri (- op #x40)) (a (neuro-cpu-reg16 cpu ri)) (v (neuro-u16 (1+ a))))
         (neuro-flags-update-inc r a v)
         (neuro-cpu-set-reg16 cpu ri v)))

      ((pred (lambda (x) (and (>= x #x48) (<= x #x4F))))
       (let* ((ri (- op #x48)) (a (neuro-cpu-reg16 cpu ri)) (v (neuro-u16 (1- a))))
         (neuro-flags-update-dec r a v)
         (neuro-cpu-set-reg16 cpu ri v)))

      ((pred (lambda (x) (and (>= x #x50) (<= x #x57))))
       (neuro-cpu-push cpu (neuro-cpu-reg16 cpu (- op #x50))))
      ((pred (lambda (x) (and (>= x #x58) (<= x #x5F))))
       (neuro-cpu-set-reg16 cpu (- op #x58) (neuro-cpu-pop cpu)))

      (#x60
       (let ((orig-sp (neuro-regs-sp r)))
         (neuro-cpu-push cpu (neuro-regs-ax r))
         (neuro-cpu-push cpu (neuro-regs-cx r))
         (neuro-cpu-push cpu (neuro-regs-dx r))
         (neuro-cpu-push cpu (neuro-regs-bx r))
         (neuro-cpu-push cpu orig-sp)
         (neuro-cpu-push cpu (neuro-regs-bp r))
         (neuro-cpu-push cpu (neuro-regs-si r))
         (neuro-cpu-push cpu (neuro-regs-di r))))
      (#x61
       (setf (neuro-regs-di r) (neuro-cpu-pop cpu))
       (setf (neuro-regs-si r) (neuro-cpu-pop cpu))
       (setf (neuro-regs-bp r) (neuro-cpu-pop cpu))
       (neuro-cpu-pop cpu)
       (setf (neuro-regs-bx r) (neuro-cpu-pop cpu))
       (setf (neuro-regs-dx r) (neuro-cpu-pop cpu))
       (setf (neuro-regs-cx r) (neuro-cpu-pop cpu))
       (setf (neuro-regs-ax r) (neuro-cpu-pop cpu)))

      (#x70 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when   (neuro-regs-get-flag r neuro-FLAG-OF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x71 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (unless (neuro-regs-get-flag r neuro-FLAG-OF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x72 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when   (neuro-regs-get-flag r neuro-FLAG-CF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x73 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (unless (neuro-regs-get-flag r neuro-FLAG-CF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x74 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when   (neuro-regs-get-flag r neuro-FLAG-ZF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x75 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (unless (neuro-regs-get-flag r neuro-FLAG-ZF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x76 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when (or (neuro-regs-get-flag r neuro-FLAG-CF)
                         (neuro-regs-get-flag r neuro-FLAG-ZF))
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x77 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when (and (not (neuro-regs-get-flag r neuro-FLAG-CF))
                          (not (neuro-regs-get-flag r neuro-FLAG-ZF)))
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x78 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when   (neuro-regs-get-flag r neuro-FLAG-SF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x79 (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (unless (neuro-regs-get-flag r neuro-FLAG-SF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x7A (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when   (neuro-regs-get-flag r neuro-FLAG-PF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x7B (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (unless (neuro-regs-get-flag r neuro-FLAG-PF)
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x7C (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when (not (eq (neuro-regs-get-flag r neuro-FLAG-SF)
                              (neuro-regs-get-flag r neuro-FLAG-OF)))
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x7D (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when (eq (neuro-regs-get-flag r neuro-FLAG-SF)
                         (neuro-regs-get-flag r neuro-FLAG-OF))
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x7E (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when (or (neuro-regs-get-flag r neuro-FLAG-ZF)
                         (not (eq (neuro-regs-get-flag r neuro-FLAG-SF)
                                  (neuro-regs-get-flag r neuro-FLAG-OF))))
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#x7F (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
               (when (and (not (neuro-regs-get-flag r neuro-FLAG-ZF))
                          (eq (neuro-regs-get-flag r neuro-FLAG-SF)
                              (neuro-regs-get-flag r neuro-FLAG-OF)))
                 (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))

      (#x80
       (let* ((m   (neuro-cpu--read-modrm cpu nil))
              (imm (neuro-cpu-fetch cpu))
              (a   (funcall (plist-get m :get-fn))))
         (pcase (plist-get m :reg)
           (0 (let ((res (neuro-u8 (+ a imm))))
                (neuro-flags-update-add r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (1 (let ((res (neuro-u8 (logior a imm))))
                (neuro-flags-update-logic r res)
                (funcall (plist-get m :set-fn) res)))
           (2 (let* ((cf (if (neuro-regs-get-flag r neuro-FLAG-CF) 1 0))
                     (res (neuro-u8 (+ a imm cf))))
                (neuro-flags-update-add r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (3 (let* ((cf (if (neuro-regs-get-flag r neuro-FLAG-CF) 1 0))
                     (res (neuro-u8 (- a imm cf))))
                (neuro-flags-update-sub r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (4 (let ((res (neuro-u8 (logand a imm))))
                (neuro-flags-update-logic r res)
                (funcall (plist-get m :set-fn) res)))
           (5 (let ((res (neuro-u8 (- a imm))))
                (neuro-flags-update-sub r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (6 (let ((res (neuro-u8 (logxor a imm))))
                (neuro-flags-update-logic r res)
                (funcall (plist-get m :set-fn) res)))
           (7 (neuro-flags-update-sub r a imm (neuro-u8 (- a imm)))))))
      (#x81
       (let* ((m   (neuro-cpu--read-modrm cpu t))
              (imm (neuro-cpu-fetch16 cpu))
              (a   (funcall (plist-get m :get-fn))))
         (pcase (plist-get m :reg)
           (0 (let ((res (neuro-u16 (+ a imm))))
                (neuro-flags-update-add r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (1 (let ((res (neuro-u16 (logior a imm))))
                (neuro-flags-update-logic r res)
                (funcall (plist-get m :set-fn) res)))
           (4 (let ((res (neuro-u16 (logand a imm))))
                (neuro-flags-update-logic r res)
                (funcall (plist-get m :set-fn) res)))
           (5 (let ((res (neuro-u16 (- a imm))))
                (neuro-flags-update-sub r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (6 (let ((res (neuro-u16 (logxor a imm))))
                (neuro-flags-update-logic r res)
                (funcall (plist-get m :set-fn) res)))
           (7 (neuro-flags-update-sub r a imm (neuro-u16 (- a imm)))))))
      (#x83
       (let* ((m   (neuro-cpu--read-modrm cpu t))
              (imm (neuro-sbyte (neuro-cpu-fetch cpu)))
              (a   (funcall (plist-get m :get-fn))))
         (pcase (plist-get m :reg)
           (0 (let ((res (neuro-u16 (+ a imm))))
                (neuro-flags-update-add r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (5 (let ((res (neuro-u16 (- a imm))))
                (neuro-flags-update-sub r a imm res)
                (funcall (plist-get m :set-fn) res)))
           (7 (neuro-flags-update-sub r a imm (neuro-u16 (- a imm)))))))

      (#x86
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil))
              (tmp (funcall (plist-get m :get-fn))))
         (funcall (plist-get m :set-fn) (funcall (car rv)))
         (funcall (cdr rv) tmp)))
      (#x87
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t))
              (tmp (funcall (plist-get m :get-fn))))
         (funcall (plist-get m :set-fn) (funcall (car rv)))
         (funcall (cdr rv) tmp)))

      (#x88
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil)))
         (funcall (plist-get m :set-fn) (funcall (car rv)))))
      (#x89
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t)))
         (funcall (plist-get m :set-fn) (funcall (car rv)))))
      (#x8A
       (let* ((m (neuro-cpu--read-modrm cpu nil))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) nil)))
         (funcall (cdr rv) (funcall (plist-get m :get-fn)))))
      (#x8B
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (rv (neuro-cpu--reg-fn cpu (plist-get m :reg) t)))
         (funcall (cdr rv) (funcall (plist-get m :get-fn)))))

      (#x8D
       (let* ((m  (neuro-cpu--read-modrm cpu t))
              (ea (plist-get m :ea)))
         (when ea
           (neuro-cpu-set-reg16 cpu (plist-get m :reg) (cdr ea)))))

      (#x8C
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (seg (pcase (plist-get m :reg)
                     (0 (neuro-regs-es r)) (1 (neuro-regs-cs r))
                     (2 (neuro-regs-ss r)) (_ (neuro-regs-ds r)))))
         (funcall (plist-get m :set-fn) seg)))
      (#x8E
       (let* ((m (neuro-cpu--read-modrm cpu t))
              (v (funcall (plist-get m :get-fn))))
         (pcase (plist-get m :reg)
           (0 (setf (neuro-regs-es r) v)) (1 (setf (neuro-regs-cs r) v))
           (2 (setf (neuro-regs-ss r) v)) (_ (setf (neuro-regs-ds r) v)))))

      (#x98
       (let ((al (neuro-lo8 (neuro-regs-ax r))))
         (setf (neuro-regs-ax r) (if (> al 127) (logior #xFF00 al) al))))
      (#x99
       (if (/= 0 (logand (neuro-regs-ax r) #x8000))
           (setf (neuro-regs-dx r) #xFFFF)
         (setf (neuro-regs-dx r) 0)))

      (#x9C (neuro-cpu-push cpu (neuro-regs-flags r)))
      (#x9D (setf (neuro-regs-flags r)
                  (logior (logand (neuro-cpu-pop cpu) neuro-FLAGS-VALID)
                          neuro-FLAGS-ALWAYS1)))
      (#x9E
       (setf (neuro-regs-flags r)
             (logior (logand (neuro-regs-flags r) #xFF00)
                     (logand (neuro-hi8 (neuro-regs-ax r)) #xD5)
                     neuro-FLAGS-ALWAYS1)))
      (#x9F
       (setf (neuro-regs-ax r)
             (logior (logand (neuro-regs-ax r) #x00FF)
                     (ash (logand (neuro-regs-flags r) #xFF) 8))))

      ((pred (lambda (x) (and (>= x #xB0) (<= x #xB7))))
       (neuro-cpu-reg8-set cpu (- op #xB0) (neuro-cpu-fetch cpu)))
      ((pred (lambda (x) (and (>= x #xB8) (<= x #xBF))))
       (neuro-cpu-set-reg16 cpu (- op #xB8) (neuro-cpu-fetch16 cpu)))

      (#xC2
       (let ((n (neuro-cpu-fetch16 cpu)))
         (setf (neuro-regs-ip r) (neuro-cpu-pop cpu))
         (setf (neuro-regs-sp r) (neuro-u16 (+ (neuro-regs-sp r) n)))))
      (#xC3 (setf (neuro-regs-ip r) (neuro-cpu-pop cpu)))
      (#xCA
       (let ((n (neuro-cpu-fetch16 cpu)))
         (setf (neuro-regs-ip r) (neuro-cpu-pop cpu))
         (setf (neuro-regs-cs r) (neuro-cpu-pop cpu))
         (setf (neuro-regs-sp r) (neuro-u16 (+ (neuro-regs-sp r) n)))))
      (#xCB
       (setf (neuro-regs-ip r) (neuro-cpu-pop cpu))
       (setf (neuro-regs-cs r) (neuro-cpu-pop cpu)))

      (#xC6
       (let* ((m   (neuro-cpu--read-modrm cpu nil))
              (imm (neuro-cpu-fetch cpu)))
         (funcall (plist-get m :set-fn) imm)))
      (#xC7
       (let* ((m   (neuro-cpu--read-modrm cpu t))
              (imm (neuro-cpu-fetch16 cpu)))
         (funcall (plist-get m :set-fn) imm)))

      (#xCF
       (setf (neuro-regs-ip r)    (neuro-cpu-pop cpu))
       (setf (neuro-regs-cs r)    (neuro-cpu-pop cpu))
       (setf (neuro-regs-flags r) (logior (logand (neuro-cpu-pop cpu) neuro-FLAGS-VALID)
                                          neuro-FLAGS-ALWAYS1)))

      (#xD0 (neuro-cpu--shift8  cpu 1))
      (#xD1 (neuro-cpu--shift16 cpu 1))
      (#xD2 (neuro-cpu--shift8  cpu (logand (neuro-regs-cx r) #xFF)))
      (#xD3 (neuro-cpu--shift16 cpu (logand (neuro-regs-cx r) #xFF)))

      (#xA4 (neuro-cpu--movsb cpu))
      (#xA5 (neuro-cpu--movsw cpu))
      (#xAA (neuro-cpu--stosb cpu))
      (#xAB (neuro-cpu--stosw cpu))
      (#xAC (neuro-cpu--lodsb cpu))
      (#xAD (neuro-cpu--lodsw cpu))
      (#xAE (neuro-cpu--scasb cpu))
      (#xAF (neuro-cpu--scasw cpu))
      (#xA6 (neuro-cpu--cmpsb cpu))
      (#xA7 (neuro-cpu--cmpsw cpu))

      (#xF2 (neuro-cpu--rep cpu nil))
      (#xF3 (neuro-cpu--rep cpu t))

      (#xE4
       (let ((port (neuro-cpu-fetch cpu)))
         (setf (neuro-regs-ax r)
               (logior (logand (neuro-regs-ax r) #xFF00)
                       (logand (neuro-port-inb port) #xFF)))))
      (#xE5
       (let ((port (neuro-cpu-fetch cpu)))
         (setf (neuro-regs-ax r) (neuro-u16 (neuro-port-inb port)))))
      (#xE6
       (let ((port (neuro-cpu-fetch cpu)))
         (neuro-port-outb port (neuro-lo8 (neuro-regs-ax r)))))
      (#xE7
       (let ((port (neuro-cpu-fetch cpu)))
         (neuro-port-outb port (neuro-lo8 (neuro-regs-ax r)))
         (neuro-port-outb (1+ port) (neuro-hi8 (neuro-regs-ax r)))))
      (#xEC
       (setf (neuro-regs-ax r)
             (logior (logand (neuro-regs-ax r) #xFF00)
                     (logand (neuro-port-inb (neuro-lo8 (neuro-regs-dx r))) #xFF))))
      (#xED
       (setf (neuro-regs-ax r)
             (neuro-u16 (neuro-port-inb (neuro-regs-dx r)))))
      (#xEE
       (neuro-port-outb (neuro-lo8 (neuro-regs-dx r)) (neuro-lo8 (neuro-regs-ax r))))
      (#xEF
       (neuro-port-outb (neuro-regs-dx r) (neuro-lo8 (neuro-regs-ax r)))
       (neuro-port-outb (1+ (neuro-regs-dx r)) (neuro-hi8 (neuro-regs-ax r))))

      (#xE0
       (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
         (setf (neuro-regs-cx r) (neuro-u16 (1- (neuro-regs-cx r))))
         (when (and (/= (neuro-regs-cx r) 0)
                    (not (neuro-regs-get-flag r neuro-FLAG-ZF)))
           (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#xE1
       (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
         (setf (neuro-regs-cx r) (neuro-u16 (1- (neuro-regs-cx r))))
         (when (and (/= (neuro-regs-cx r) 0)
                    (neuro-regs-get-flag r neuro-FLAG-ZF))
           (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))
      (#xE2
       (let ((d (neuro-sbyte (neuro-cpu-fetch cpu))))
         (setf (neuro-regs-cx r) (neuro-u16 (1- (neuro-regs-cx r))))
         (when (/= (neuro-regs-cx r) 0)
           (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) d))))))

      (#xE8
       (let* ((rel (neuro-cpu-fetch16 cpu)) (srel (neuro-sword rel)))
         (neuro-cpu-push cpu (neuro-regs-ip r))
         (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) srel)))))
      (#xE9
       (let* ((rel (neuro-cpu-fetch16 cpu)) (srel (neuro-sword rel)))
         (setf (neuro-regs-ip r) (neuro-u16 (+ (neuro-regs-ip r) srel)))))
      (#xEB
       (setf (neuro-regs-ip r)
             (neuro-u16 (+ (neuro-regs-ip r) (neuro-sbyte (neuro-cpu-fetch cpu))))))

      (#x9A
       (let ((off (neuro-cpu-fetch16 cpu)) (seg (neuro-cpu-fetch16 cpu)))
         (neuro-cpu-push cpu (neuro-regs-cs r))
         (neuro-cpu-push cpu (neuro-regs-ip r))
         (setf (neuro-regs-cs r) seg)
         (setf (neuro-regs-ip r) off)))

      (#xEA
       (let ((off (neuro-cpu-fetch16 cpu)) (seg (neuro-cpu-fetch16 cpu)))
         (setf (neuro-regs-cs r) seg)
         (setf (neuro-regs-ip r) off)))

      (#xF4 (setf (neuro-cpu-halted cpu) t))

      (#xF5 (neuro-regs-set-flag r neuro-FLAG-CF
                                   (not (neuro-regs-get-flag r neuro-FLAG-CF))))
      (#xF8 (neuro-regs-set-flag r neuro-FLAG-CF nil))
      (#xF9 (neuro-regs-set-flag r neuro-FLAG-CF t))
      (#xFA (neuro-regs-set-flag r neuro-FLAG-IF nil))
      (#xFB (neuro-regs-set-flag r neuro-FLAG-IF t))
      (#xFC (neuro-regs-set-flag r neuro-FLAG-DF nil))
      (#xFD (neuro-regs-set-flag r neuro-FLAG-DF t))

      (#xF6 (neuro-cpu--f6 cpu))
      (#xF7 (neuro-cpu--f7 cpu))

      (#xFF (neuro-cpu--ff cpu))

      (#xCC (neuro-cpu--do-int cpu 3))
      (#xCD (neuro-cpu--do-int cpu (neuro-cpu-fetch cpu)))
      (#xCE (when (neuro-regs-get-flag r neuro-FLAG-OF)
               (neuro-cpu--do-int cpu 4)))

      (_ (neuro-cpu--do-int cpu 6))))

  (neuro-cpu--check-tf cpu))

(defun neuro-cpu--shift8 (cpu count)
  (let* ((m   (neuro-cpu--read-modrm cpu nil))
         (ext (plist-get m :reg))
         (val (funcall (plist-get m :get-fn)))
         (cnt (logand count 31))
         (r   (neuro-cpu-regs cpu)))
    (when (> cnt 0)
      (let ((res
             (pcase ext
               (0
                (let ((c (mod cnt 8)))
                  (neuro-u8 (logior (ash val c) (ash val (- c 8))))))
               (1
                (let ((c (mod cnt 8)))
                  (neuro-u8 (logior (ash val (- c)) (ash val (- 8 c))))))
               (4
                (neuro-u8 (ash val cnt)))
               (5
                (neuro-u8 (ash val (- cnt))))
               (7
                (neuro-u8 (ash (if (> val 127) (- val 256) val) (- cnt))))
               (_ val))))
        (funcall (plist-get m :set-fn) res)
        (neuro-flags-update-logic r res)))))

(defun neuro-cpu--shift16 (cpu count)
  (let* ((m   (neuro-cpu--read-modrm cpu t))
         (ext (plist-get m :reg))
         (val (funcall (plist-get m :get-fn)))
         (cnt (logand count 31))
         (r   (neuro-cpu-regs cpu)))
    (when (> cnt 0)
      (let ((res
             (pcase ext
               (0 (let ((c (mod cnt 16)))
                    (neuro-u16 (logior (ash val c) (ash val (- c 16))))))
               (1 (let ((c (mod cnt 16)))
                    (neuro-u16 (logior (ash val (- c)) (ash val (- 16 c))))))
               (4 (neuro-u16 (ash val cnt)))
               (5 (neuro-u16 (ash val (- cnt))))
               (7 (neuro-u16 (ash (neuro-sword val) (- cnt))))
               (_ val))))
        (funcall (plist-get m :set-fn) res)
        (neuro-flags-update-logic r res)
        (neuro-regs-set-flag r neuro-FLAG-CF
                              (if (member ext '(4 0))
                                  (/= 0 (logand val (ash 1 (- 16 cnt))))
                                (/= 0 (logand val (ash 1 (1- cnt))))))))))

(defun neuro-cpu--movsb (cpu)
  (let* ((r (neuro-cpu-regs cpu))
         (b (neuro-cpu--mem-read8 cpu (neuro-regs-ds r) (neuro-regs-si r))))
    (neuro-cpu--mem-write8 cpu (neuro-regs-es r) (neuro-regs-di r) b)
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (progn (setf (neuro-regs-si r) (neuro-u16 (1- (neuro-regs-si r))))
               (setf (neuro-regs-di r) (neuro-u16 (1- (neuro-regs-di r)))))
      (progn (setf (neuro-regs-si r) (neuro-u16 (1+ (neuro-regs-si r))))
             (setf (neuro-regs-di r) (neuro-u16 (1+ (neuro-regs-di r))))))))

(defun neuro-cpu--movsw (cpu)
  (let* ((r (neuro-cpu-regs cpu))
         (w (neuro-cpu--mem-read16 cpu (neuro-regs-ds r) (neuro-regs-si r))))
    (neuro-cpu--mem-write16 cpu (neuro-regs-es r) (neuro-regs-di r) w)
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (progn (setf (neuro-regs-si r) (neuro-u16 (- (neuro-regs-si r) 2)))
               (setf (neuro-regs-di r) (neuro-u16 (- (neuro-regs-di r) 2))))
      (progn (setf (neuro-regs-si r) (neuro-u16 (+ (neuro-regs-si r) 2)))
             (setf (neuro-regs-di r) (neuro-u16 (+ (neuro-regs-di r) 2)))))))

(defun neuro-cpu--stosb (cpu)
  (let ((r (neuro-cpu-regs cpu)))
    (neuro-cpu--mem-write8 cpu (neuro-regs-es r) (neuro-regs-di r)
                            (neuro-lo8 (neuro-regs-ax r)))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (setf (neuro-regs-di r) (neuro-u16 (1- (neuro-regs-di r))))
      (setf (neuro-regs-di r) (neuro-u16 (1+ (neuro-regs-di r)))))))

(defun neuro-cpu--stosw (cpu)
  (let ((r (neuro-cpu-regs cpu)))
    (neuro-cpu--mem-write16 cpu (neuro-regs-es r) (neuro-regs-di r) (neuro-regs-ax r))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (setf (neuro-regs-di r) (neuro-u16 (- (neuro-regs-di r) 2)))
      (setf (neuro-regs-di r) (neuro-u16 (+ (neuro-regs-di r) 2))))))

(defun neuro-cpu--lodsb (cpu)
  (let ((r (neuro-cpu-regs cpu)))
    (setf (neuro-regs-ax r)
          (logior (logand (neuro-regs-ax r) #xFF00)
                  (neuro-cpu--mem-read8 cpu (neuro-regs-ds r) (neuro-regs-si r))))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (setf (neuro-regs-si r) (neuro-u16 (1- (neuro-regs-si r))))
      (setf (neuro-regs-si r) (neuro-u16 (1+ (neuro-regs-si r)))))))

(defun neuro-cpu--lodsw (cpu)
  (let ((r (neuro-cpu-regs cpu)))
    (setf (neuro-regs-ax r)
          (neuro-cpu--mem-read16 cpu (neuro-regs-ds r) (neuro-regs-si r)))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (setf (neuro-regs-si r) (neuro-u16 (- (neuro-regs-si r) 2)))
      (setf (neuro-regs-si r) (neuro-u16 (+ (neuro-regs-si r) 2))))))

(defun neuro-cpu--scasb (cpu)
  (let* ((r (neuro-cpu-regs cpu))
         (al (neuro-lo8 (neuro-regs-ax r)))
         (b  (neuro-cpu--mem-read8 cpu (neuro-regs-es r) (neuro-regs-di r))))
    (neuro-flags-update-sub r al b (neuro-u8 (- al b)))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (setf (neuro-regs-di r) (neuro-u16 (1- (neuro-regs-di r))))
      (setf (neuro-regs-di r) (neuro-u16 (1+ (neuro-regs-di r)))))))

(defun neuro-cpu--scasw (cpu)
  (let* ((r (neuro-cpu-regs cpu))
         (ax (neuro-regs-ax r))
         (w  (neuro-cpu--mem-read16 cpu (neuro-regs-es r) (neuro-regs-di r))))
    (neuro-flags-update-sub r ax w (neuro-u16 (- ax w)))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (setf (neuro-regs-di r) (neuro-u16 (- (neuro-regs-di r) 2)))
      (setf (neuro-regs-di r) (neuro-u16 (+ (neuro-regs-di r) 2))))))

(defun neuro-cpu--cmpsb (cpu)
  (let* ((r  (neuro-cpu-regs cpu))
         (a  (neuro-cpu--mem-read8 cpu (neuro-regs-ds r) (neuro-regs-si r)))
         (b  (neuro-cpu--mem-read8 cpu (neuro-regs-es r) (neuro-regs-di r))))
    (neuro-flags-update-sub r a b (neuro-u8 (- a b)))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (progn (setf (neuro-regs-si r) (neuro-u16 (1- (neuro-regs-si r))))
               (setf (neuro-regs-di r) (neuro-u16 (1- (neuro-regs-di r)))))
      (progn (setf (neuro-regs-si r) (neuro-u16 (1+ (neuro-regs-si r))))
             (setf (neuro-regs-di r) (neuro-u16 (1+ (neuro-regs-di r))))))))

(defun neuro-cpu--cmpsw (cpu)
  (let* ((r  (neuro-cpu-regs cpu))
         (a  (neuro-cpu--mem-read16 cpu (neuro-regs-ds r) (neuro-regs-si r)))
         (b  (neuro-cpu--mem-read16 cpu (neuro-regs-es r) (neuro-regs-di r))))
    (neuro-flags-update-sub r a b (neuro-u16 (- a b)))
    (if (neuro-regs-get-flag r neuro-FLAG-DF)
        (progn (setf (neuro-regs-si r) (neuro-u16 (- (neuro-regs-si r) 2)))
               (setf (neuro-regs-di r) (neuro-u16 (- (neuro-regs-di r) 2))))
      (progn (setf (neuro-regs-si r) (neuro-u16 (+ (neuro-regs-si r) 2)))
             (setf (neuro-regs-di r) (neuro-u16 (+ (neuro-regs-di r) 2)))))))

(defun neuro-cpu--rep (cpu repe-p)
  (let* ((r   (neuro-cpu-regs cpu))
         (op  (neuro-cpu-fetch cpu))
         (zf-sensitive (memq op '(#xA6 #xA7 #xAE #xAF))))
    (cl-block neuro-cpu--rep
      (while (> (neuro-regs-cx r) 0)
        (pcase op
          (#xA4 (neuro-cpu--movsb cpu)) (#xA5 (neuro-cpu--movsw cpu))
          (#xAA (neuro-cpu--stosb cpu)) (#xAB (neuro-cpu--stosw cpu))
          (#xAC (neuro-cpu--lodsb cpu)) (#xAD (neuro-cpu--lodsw cpu))
          (#xAE (neuro-cpu--scasb cpu)) (#xAF (neuro-cpu--scasw cpu))
          (#xA6 (neuro-cpu--cmpsb cpu)) (#xA7 (neuro-cpu--cmpsw cpu))
          (_ (error "REP with non-string opcode #x%02X" op)))
        (setf (neuro-regs-cx r) (neuro-u16 (1- (neuro-regs-cx r))))
        (when zf-sensitive
          (let ((zf (neuro-regs-get-flag r neuro-FLAG-ZF)))
            (if repe-p
                (when (not zf) (cl-return-from neuro-cpu--rep nil))
              (when zf (cl-return-from neuro-cpu--rep nil)))))))))

(defun neuro-cpu--f6 (cpu)
  (let* ((m   (neuro-cpu--read-modrm cpu nil))
         (ext (plist-get m :reg))
         (r   (neuro-cpu-regs cpu))
         (a   (funcall (plist-get m :get-fn))))
    (pcase ext
      (0
       (let* ((imm (neuro-cpu-fetch cpu))
              (res (neuro-u8 (logand a imm))))
         (neuro-flags-update-logic r res)))
      (2
       (funcall (plist-get m :set-fn) (neuro-u8 (lognot a))))
      (3
       (let ((res (neuro-u8 (- a))))
         (neuro-flags-update-sub r 0 a res)
         (funcall (plist-get m :set-fn) res)))
      (4
       (let* ((res (* (neuro-lo8 (neuro-regs-ax r)) a))
              (overflow (/= 0 (logand res #xFF00))))
         (setf (neuro-regs-ax r) (logand res #xFFFF))
         (neuro-regs-set-flag r neuro-FLAG-CF overflow)
         (neuro-regs-set-flag r neuro-FLAG-OF overflow)))
      (5
       (let* ((sa (if (> (neuro-lo8 (neuro-regs-ax r)) 127)
                      (- (neuro-lo8 (neuro-regs-ax r)) 256)
                    (neuro-lo8 (neuro-regs-ax r))))
              (sb (if (> a 127) (- a 256) a))
              (res (* sa sb)))
         (setf (neuro-regs-ax r) (neuro-u16 res))
         (let ((overflow (not (= res (neuro-sbyte (neuro-lo8 res))))))
           (neuro-regs-set-flag r neuro-FLAG-CF overflow)
           (neuro-regs-set-flag r neuro-FLAG-OF overflow))))
      (6
       (when (= a 0) (neuro-cpu--do-int cpu 0))
       (let* ((ax  (neuro-regs-ax r))
              (q   (/ ax a))
              (rem (mod ax a)))
         (when (> q #xFF) (neuro-cpu--do-int cpu 0))
         (setf (neuro-regs-ax r) (logior (ash (logand rem #xFF) 8) (logand q #xFF)))))
      (7
       (when (= a 0) (neuro-cpu--do-int cpu 0))
       (let* ((ax  (neuro-sword (neuro-regs-ax r)))
              (sb  (if (> a 127) (- a 256) a))
              (q   (truncate ax sb))
              (rem (- ax (* q sb))))
         (when (or (> q 127) (< q -128)) (neuro-cpu--do-int cpu 0))
         (setf (neuro-regs-ax r) (logior (ash (neuro-u8 rem) 8) (neuro-u8 q))))))))

(defun neuro-cpu--f7 (cpu)
  (let* ((m   (neuro-cpu--read-modrm cpu t))
         (ext (plist-get m :reg))
         (r   (neuro-cpu-regs cpu))
         (a   (funcall (plist-get m :get-fn))))
    (pcase ext
      (0
       (let* ((imm (neuro-cpu-fetch16 cpu))
              (res (neuro-u16 (logand a imm))))
         (neuro-flags-update-logic r res)))
      (2
       (funcall (plist-get m :set-fn) (neuro-u16 (lognot a))))
      (3
       (let ((res (neuro-u16 (- a))))
         (neuro-flags-update-sub r 0 a res)
         (funcall (plist-get m :set-fn) res)))
      (4
       (let* ((res (* (neuro-regs-ax r) a))
              (overflow (/= 0 (logand res #xFFFF0000))))
         (setf (neuro-regs-ax r) (logand res #xFFFF))
         (setf (neuro-regs-dx r) (logand (ash res -16) #xFFFF))
         (neuro-regs-set-flag r neuro-FLAG-CF overflow)
         (neuro-regs-set-flag r neuro-FLAG-OF overflow)))
      (5
       (let* ((sa (neuro-sword (neuro-regs-ax r)))
              (sb (neuro-sword a))
              (res (* sa sb)))
         (setf (neuro-regs-ax r) (neuro-u16 res))
         (setf (neuro-regs-dx r) (neuro-u16 (ash res -16)))
         (let ((overflow (not (= res (neuro-sword (neuro-u16 res))))))
           (neuro-regs-set-flag r neuro-FLAG-CF overflow)
           (neuro-regs-set-flag r neuro-FLAG-OF overflow))))
      (6
       (when (= a 0) (neuro-cpu--do-int cpu 0))
       (let* ((dvd (logior (ash (neuro-regs-dx r) 16) (neuro-regs-ax r)))
              (q   (/ dvd a))
              (rem (mod dvd a)))
         (when (> q #xFFFF) (neuro-cpu--do-int cpu 0))
         (setf (neuro-regs-ax r) (neuro-u16 q))
         (setf (neuro-regs-dx r) (neuro-u16 rem))))
      (7
       (when (= a 0) (neuro-cpu--do-int cpu 0))
       (let* ((dvd (let ((v (logior (ash (neuro-regs-dx r) 16) (neuro-regs-ax r))))
                     (if (> v #x7FFFFFFF) (- v #x100000000) v)))
              (sb  (neuro-sword a))
              (q   (truncate dvd sb))
              (rem (- dvd (* q sb))))
         (when (or (> q #x7FFF) (< q -32768)) (neuro-cpu--do-int cpu 0))
         (setf (neuro-regs-ax r) (neuro-u16 q))
         (setf (neuro-regs-dx r) (neuro-u16 rem)))))))

(defun neuro-cpu--ff (cpu)
  (let* ((m   (neuro-cpu--read-modrm cpu t))
         (ext (plist-get m :reg))
         (r   (neuro-cpu-regs cpu)))
    (pcase ext
      (0
       (let* ((a (funcall (plist-get m :get-fn))) (v (neuro-u16 (1+ a))))
         (neuro-flags-update-inc r a v)
         (funcall (plist-get m :set-fn) v)))
      (1
       (let* ((a (funcall (plist-get m :get-fn))) (v (neuro-u16 (1- a))))
         (neuro-flags-update-dec r a v)
         (funcall (plist-get m :set-fn) v)))
      (2
       (let ((target (funcall (plist-get m :get-fn))))
         (neuro-cpu-push cpu (neuro-regs-ip r))
         (setf (neuro-regs-ip r) target)))
      (3
       (when-let* ((ea (plist-get m :ea)))
         (let* ((off (neuro-cpu--mem-read16 cpu (car ea) (cdr ea)))
                (seg (neuro-cpu--mem-read16 cpu (car ea) (neuro-u16 (+ (cdr ea) 2)))))
           (neuro-cpu-push cpu (neuro-regs-cs r))
           (neuro-cpu-push cpu (neuro-regs-ip r))
           (setf (neuro-regs-cs r) seg)
           (setf (neuro-regs-ip r) off))))
      (4
       (setf (neuro-regs-ip r) (funcall (plist-get m :get-fn))))
      (5
       (when-let* ((ea (plist-get m :ea)))
         (let* ((off (neuro-cpu--mem-read16 cpu (car ea) (cdr ea)))
                (seg (neuro-cpu--mem-read16 cpu (car ea) (neuro-u16 (+ (cdr ea) 2)))))
           (setf (neuro-regs-cs r) seg)
           (setf (neuro-regs-ip r) off))))
      (6
       (neuro-cpu-push cpu (funcall (plist-get m :get-fn)))))))

(defun neuro-cpu-dump-mem (cpu addr len)
  (let* ((mem      (neuro-cpu-memory cpu))
         (a0       (logand addr #xFFFF))
         (safe-len (min len (- 65536 a0)))
         (data     (let ((v (make-vector safe-len 0)))
                     (dotimes (i safe-len) (aset v i (aref mem (+ a0 i))))
                     v)))
    (neuro-hex-dump data addr 8)))

(defun neuro-cpu-write-mem (cpu addr data)
  (let ((mem (neuro-cpu-memory cpu))
        (n   (length data)))
    (dotimes (i n)
      (let ((a (logand (+ addr i) #xFFFF)))
        (aset mem a (logand (aref data i) #xFF))))))

(defun neuro-port-inb (port)
  (if (fboundp 'neuro-hw-port-inb)
      (neuro-hw-port-inb port)
    #xFF))

(defun neuro-port-outb (port val)
  (when (fboundp 'neuro-hw-port-outb)
    (neuro-hw-port-outb port val)))

(cl-defstruct neuro-process pid name state priority cpu memory-used isolated ticks parent)

(defvar neuro-proc-table      (make-hash-table :test #'eql))
(defvar neuro-proc-next-pid   1)
(defvar neuro-proc-current-pid 1)
(defvar neuro-proc-ready-queue nil)

(defun neuro-proc-init ()
  (setq neuro-proc-table       (make-hash-table :test #'eql)
        neuro-proc-next-pid    1
        neuro-proc-current-pid 1
        neuro-proc-ready-queue nil)
  (neuro-cpu--idt-init))

(defun neuro-proc-spawn (name priority &optional cs ds es ss sp ip)
  (let* ((pid neuro-proc-next-pid)
         (cpu (if cs
                  (neuro-cpu-new-segs cs (or ds 0) (or es 0) (or ss 0)
                                      (or sp #xFFFE) (or ip 0))
                (neuro-cpu-new))))
    (cl-incf neuro-proc-next-pid)
    (let ((p (make-neuro-process
              :pid pid :name name :state "ready" :priority priority
              :cpu cpu :memory-used 64 :isolated nil :ticks 0 :parent nil)))
      (puthash pid p neuro-proc-table)
      (push pid neuro-proc-ready-queue)
      pid)))

(defun neuro-proc-get  (pid) (gethash pid neuro-proc-table))

(defun neuro-proc-kill (pid)
  (remhash pid neuro-proc-table)
  (setq neuro-proc-ready-queue (delq pid neuro-proc-ready-queue))
  (when (= pid neuro-proc-current-pid)
    (setq neuro-proc-current-pid
          (or (car neuro-proc-ready-queue) 0))))

(defun neuro-proc-list ()
  (let (lst)
    (maphash (lambda (_k v) (push v lst)) neuro-proc-table)
    (sort lst (lambda (a b) (< (neuro-process-pid a) (neuro-process-pid b))))))

(defun neuro-scheduler-tick ()
  (when neuro-proc-ready-queue
    (let* ((nxt  (car neuro-proc-ready-queue))
           (proc (neuro-proc-get nxt)))
      (when proc
        (when-let* ((cur (neuro-proc-get neuro-proc-current-pid)))
          (when (equal (neuro-process-state cur) "running")
            (setf (neuro-process-state cur) "ready")
            (unless (memq neuro-proc-current-pid neuro-proc-ready-queue)
              (setq neuro-proc-ready-queue
                    (append neuro-proc-ready-queue
                            (list neuro-proc-current-pid))))))
        (setq neuro-proc-ready-queue (cdr neuro-proc-ready-queue))
        (setf (neuro-process-state proc) "running")
        (setq neuro-proc-current-pid nxt)
        nxt))))

(provide 'c0)
