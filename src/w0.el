;;; w0.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'u0)
(require 'c0)
(declare-function neuro-sandbox-tick "y0")

(defun neuro-idt-set (vec seg off)
  "Install IDT vector VEC → SEG:OFF. CPU reads this on INT."
  (neuro-cpu--idt-set vec seg off))

(defun neuro-idt-get (vec)
  (when neuro-cpu--idt
    (aref neuro-cpu--idt vec)))

(defun neuro-idt-defaults ()
  "Install default BIOS-like handlers."

  (neuro-idt-set #x00 #xF000 #x0010)
  (neuro-idt-set #x01 #xF000 #x0020)
  (neuro-idt-set #x03 #xF000 #x0030)
  (neuro-idt-set #x04 #xF000 #x0040)
  (neuro-idt-set #x05 #xF000 #x0050)
  (neuro-idt-set #x06 #xF000 #x0060)
  (neuro-idt-set #x08 #xF000 #x0100)
  (neuro-idt-set #x09 #xF000 #x0200)
  (neuro-idt-set #x10 #xF000 #x1000)
  (neuro-idt-set #x13 #xF000 #x1300)
  (neuro-idt-set #x20 #xF000 #x2000)
  (neuro-idt-set #x21 #xF000 #x2100))

(defvar neuro-irq-pending   (make-vector 256 nil))

(defvar neuro-irq-mask      (make-vector 256 nil)
  "IRQ mask table. t = masked (disabled), nil = unmasked (enabled).")
(defvar neuro-irq-in-service nil)

(defun neuro-irq-init ()
  (setq neuro-irq-pending    (make-vector 256 nil)
        neuro-irq-mask       (make-vector 256 nil)
        neuro-irq-in-service nil)

  (dotimes (i 8) (aset neuro-irq-mask (+ 8 i) t)))

(defun neuro-irq-request (vec)
  (when (and (boundp 'neuro-sandbox-active) neuro-sandbox-active)
    (neuro-sandbox-tick))
  (unless (aref neuro-irq-mask (logand vec #xFF))
    (aset neuro-irq-pending (logand vec #xFF) t)))

(defun neuro-irq-mask-vec (vec masked)
  (aset neuro-irq-mask (logand vec #xFF) (if masked t nil)))

(defun neuro-irq-acknowledge ()
  (setq neuro-irq-in-service nil))

(defun neuro-irq-next-pending ()
  "Return next pending unmasked IRQ vector, or nil."
  (when (null neuro-irq-in-service)
    (catch 'found
      (dotimes (i 256)
        (when (and (aref neuro-irq-pending i)
                   (not (aref neuro-irq-mask i)))
          (aset neuro-irq-pending i nil)
          (setq neuro-irq-in-service i)
          (throw 'found i))))))

(defvar neuro-ports       nil)
(defvar neuro-port-traps  nil)

(defconst neuro-port-unimplemented #xFF)

(defun neuro-ports-init ()
  (setq neuro-ports      (make-vector 65536 neuro-port-unimplemented)
        neuro-port-traps (make-vector 65536 nil))

  (aset neuro-ports #x40 #x00)
  (aset neuro-ports #x41 #x00)
  (aset neuro-ports #x42 #x00)
  (aset neuro-ports #x43 #x36)

  (aset neuro-ports #x20 #x00)
  (aset neuro-ports #x21 #xFF)

  (aset neuro-ports #xA0 #x00)
  (aset neuro-ports #xA1 #xFF)

  (aset neuro-ports #x60 #x00)
  (aset neuro-ports #x61 #x00)

  (aset neuro-ports #x61 #x00)

  (aset neuro-ports #x3F8 #x00)
  (aset neuro-ports #x3F9 #x00)
  (aset neuro-ports #x3FA #x00)
  (aset neuro-ports #x3FB #x00)
  (aset neuro-ports #x3FC #x00)
  (aset neuro-ports #x3FD #x60))

(defun neuro-hw-port-inb (port)
  (let ((p (logand port #xFFFF)))
    (when (and (boundp 'neuro-sandbox-active) neuro-sandbox-active)
      (neuro-sandbox-tick))
    (if (aref neuro-port-traps p)
        (error "Port trap on INB %04X" p)
      (aref neuro-ports p))))

(defun neuro-hw-port-outb (port val)
  (let ((p (logand port #xFFFF))
        (v (logand val #xFF)))
    (when (and (boundp 'neuro-sandbox-active) neuro-sandbox-active)
      (neuro-sandbox-tick))
    (if (aref neuro-port-traps p)
        (error "Port trap on OUTB %04X <- %02X" p v)
      (aset neuro-ports p v)

      (pcase p
        (#x20 (when (= v #x20) (neuro-irq-acknowledge)))
        (#x21
         (dotimes (i 8)
           (aset neuro-irq-mask i (not (= 0 (logand v (ash 1 i)))))))
        (#x43 nil)
        (#x61

         (when (= (logand v 3) 3)
           (when (and (boundp 'neuro-output-hook) neuro-output-hook)
             (neuro-emit "\a"))))))))

(cl-defstruct neuro-driver name init-fn read-fn write-fn irq tick-fn ticks last-scancode)

(defvar neuro-drivers (make-hash-table :test 'equal))

(defun neuro-driver-register (name init-fn read-fn write-fn irq &optional tick-fn)
  (puthash name
           (make-neuro-driver
            :name name :init-fn init-fn :read-fn read-fn :write-fn write-fn
            :irq irq :tick-fn tick-fn :ticks 0 :last-scancode 0)
           neuro-drivers))

(defun neuro-drivers-init ()
  (clrhash neuro-drivers)

  (neuro-driver-register
   "TIMER"
   (lambda () nil)
   (lambda (reg) (neuro-hw-port-inb (+ #x40 reg)))
   (lambda (reg val) (neuro-hw-port-outb (+ #x40 reg) val))
   8
   (lambda (drv)
     (cl-incf (neuro-driver-ticks drv))
     (when (= (mod (neuro-driver-ticks drv) 18) 0)
       (neuro-irq-request 8))))

  (neuro-driver-register
   "KBD"
   (lambda () nil)
   (lambda (reg) (if (= reg 0) (neuro-hw-port-inb #x60) (neuro-hw-port-inb #x61)))
   (lambda (reg val) (neuro-hw-port-outb (if (= reg 0) #x60 #x61) val))
   9
   (lambda (drv) (cl-incf (neuro-driver-ticks drv)))))

(defun neuro-drivers-tick ()
  (maphash (lambda (name drv)
             (when (neuro-driver-tick-fn drv)
               (condition-case err
                   (funcall (neuro-driver-tick-fn drv) drv)
                 (error
                  (message "[driver:%s] tick error: %s"
                           name (error-message-string err))))))
           neuro-drivers))

(defun neuro-hw-dispatch-irq (cpu)
  "Check for pending IRQs and dispatch into CPU if IF=1."
  (when (neuro-regs-get-flag (neuro-cpu-regs cpu) neuro-FLAG-IF)
    (when-let* ((vec (neuro-irq-next-pending)))
      (neuro-cpu--do-int cpu vec))))

(defun neuro-hw-init ()
  (neuro-irq-init)
  (neuro-ports-init)
  (neuro-drivers-init)
  (neuro-idt-defaults))

(provide 'w0)
