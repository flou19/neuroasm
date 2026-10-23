;;; e0.el -*- lexical-binding: t -*-

(require 'u0)
(require 'c0)
(require 'f0)

(defconst neuro-MOOD-DORMANT    0)
(defconst neuro-MOOD-RESTLESS   1)
(defconst neuro-MOOD-AGITATED   2)
(defconst neuro-MOOD-HUNTING    3)
(defconst neuro-MOOD-FRAGMENTED 5)

(defun neuro-mood-from-threat (threat-level)

  (cond ((<= threat-level 0) neuro-MOOD-DORMANT)
        ((= threat-level 1)  neuro-MOOD-RESTLESS)
        ((= threat-level 2)  neuro-MOOD-AGITATED)
        ((<= threat-level 4) neuro-MOOD-HUNTING)
        (t                   neuro-MOOD-FRAGMENTED)))

(defun neuro-mood-str (threat)
  (let ((m (neuro-mood-from-threat threat)))
    (cond ((= m neuro-MOOD-DORMANT)    "DORMANT")
          ((= m neuro-MOOD-RESTLESS)   "RESTLESS")
          ((= m neuro-MOOD-AGITATED)   "AGITATED")
          ((= m neuro-MOOD-HUNTING)    "HUNTING")
          ((= m neuro-MOOD-FRAGMENTED) "FRAGMENTED")
          (t                           "UNKNOWN"))))

(cl-defstruct neuro-neural
  layers layer-sizes research-progress awakened cores-online)

(defun neuro-neural-new ()
  (let* ((topo [8 16 12 4])
         (n-layers (length topo))
         (layers (make-vector n-layers nil)))
    (dotimes (l n-layers)
      (let* ((sz (aref topo l))
             (layer (make-vector sz nil)))
        (dotimes (n sz)
          (let* ((n-in (if (= l 0) 0 (aref topo (1- l))))
                 (ws   (make-vector n-in 0.0)))
            (dotimes (w n-in)
              (aset ws w (- (* (neuro-rng-f32) 2.0) 1.0)))
            (aset layer n (list :weights ws
                                :bias (- (* (neuro-rng-f32) 2.0) 1.0)
                                :activation 0.0))))
        (aset layers l layer)))
    (make-neuro-neural :layers layers
                       :layer-sizes topo
                       :research-progress 0.0
                       :awakened nil
                       :cores-online 0)))

(defun neuro-neural-tanh (x)
  "Padé approximation of tanh(x). Discontinuous derivative at ±4.
Used only as activation function — not for calculus."
  (cond
   ((> x  4.0)  1.0)
   ((< x -4.0) -1.0)
   (t (let ((x2 (* x x)))
        (/ (* x (+ 27.0 x2)) (+ 27.0 (* 9.0 x2)))))))

(defun neuro-neural-forward (nc inputs)

  (let* ((layers (neuro-neural-layers nc))
         (sizes  (neuro-neural-layer-sizes nc))
         (buf    (vconcat (seq-take (append inputs nil)
                                    (aref sizes 0)))))
    (let ((l 1))
      (while (< l (length layers))
        (let* ((n-out (aref sizes l))
               (n-in  (min (aref sizes (1- l)) (length buf)))
               (layer (aref layers l))
               (next  (make-vector n-out 0.0)))
          (dotimes (n n-out)
            (let* ((neuron (aref layer n))
                   (ws (plist-get neuron :weights))
                   (b  (plist-get neuron :bias))
                   (s  b))
              (dotimes (w n-in)
                (when (< w (length ws))
                  (setq s (+ s (* (aref ws w) (aref buf w))))))
              (let ((act (neuro-neural-tanh s)))
                (aset next n act)
                (plist-put neuron :activation act))))

          (setq buf next))
        (setq l (1+ l))))
    buf))

(defun neuro-neural-fragment-pulse (nc intensity)
  (let ((new-prog (+ (neuro-neural-research-progress nc) (* intensity 0.1))))
    (setf (neuro-neural-research-progress nc) new-prog)
    (when (and (>= new-prog 1.0) (not (neuro-neural-awakened nc)))
      (setf (neuro-neural-awakened nc) t)
      (cl-incf (neuro-neural-cores-online nc)))))

(defvar neuro-threat 0)
(defvar neuro-chase-active nil)

(defvar --cl-block-neuro-escalate-- nil)
(defvar --cl-block-neuro-second-user-act-- nil)
(defvar --cl-block-neuro-background-tick-- nil)
(defvar neuro-game-beaten nil)
(defvar neuro-second-user-pid nil)
(defvar neuro-system-ticks 0)
(defvar neuro-last-user-action-ms 0)
(defvar neuro-last-u2-msg-ms 0)
(defvar neuro-neural-core nil)
(defvar neuro-fragments-collected (make-hash-table :test 'equal))
(defvar neuro-cores-found 0)
(defvar neuro-research-unlocked nil)
(defvar neuro-endings-seen (make-hash-table :test 'equal))
(defvar neuro-observe-streak 0)
(defconst neuro-observe-required 60)
(defvar neuro-session-start-ms 0)
(defvar neuro-total-commands 0)
(defvar neuro-idle-ticks 0)
(defvar neuro-show-hidden nil)

(defun neuro-entity-init ()
  (setq neuro-threat 0
        neuro-chase-active nil
        neuro-game-beaten nil
        neuro-second-user-pid nil
        neuro-system-ticks 0
        neuro-last-user-action-ms (- (neuro-now-ms) 30000)
        neuro-last-u2-msg-ms (- (neuro-now-ms) 20000)
        neuro-neural-core (neuro-neural-new)
        neuro-fragments-collected (make-hash-table :test 'equal)
        neuro-cores-found 0
        neuro-research-unlocked nil
        neuro-endings-seen (make-hash-table :test 'equal)
        neuro-observe-streak 0
        neuro-session-start-ms (neuro-now-ms)
        neuro-total-commands 0
        neuro-idle-ticks 0
        neuro-show-hidden nil))

(defun neuro-escalate (delta)
  (when neuro-game-beaten (cl-return-from neuro-escalate nil))
  (let* ((old neuro-threat)
         (new (neuro-clamp (+ old delta) 0 5)))
    (setq neuro-threat new)
    (when (and (>= new 3) (< old 3))
      (neuro-emit "\n╔════════════════════════════════════════════════╗")
      (neuro-emit "║   SECOND USER ACTIVE                           ║")
      (neuro-emit "╚════════════════════════════════════════════════╝")
      (setq neuro-chase-active t)
      (ignore-errors (neuro-beep 5 80))
      (neuro-fs-append-log "NEURAL.LOG" "pursuit mode"))
    (when (> new old)
      (neuro-fs-append-log "NEURAL.LOG"
                            (format "threat %d -> %d" old new)))))

(defun neuro-second-user-act ()
  (when neuro-game-beaten (cl-return-from neuro-second-user-act nil))
  (unless neuro-second-user-pid (cl-return-from neuro-second-user-act nil))
  (let ((proc (neuro-proc-get neuro-second-user-pid)))
    (unless proc (cl-return-from neuro-second-user-act nil))
    (when (neuro-process-isolated proc) (cl-return-from neuro-second-user-act nil))
    (let ((cpu (neuro-process-cpu proc))
          (action (neuro-rng-range 12)))
      (pcase action
        (0 (setf (neuro-regs-ax (neuro-cpu-regs cpu)) (neuro-rng-u16)))
        (1 (setf (neuro-regs-bx (neuro-cpu-regs cpu)) (neuro-rng-u16)))
        (2 (setf (neuro-regs-cx (neuro-cpu-regs cpu)) (neuro-rng-u16)))
        (3 (setf (neuro-regs-dx (neuro-cpu-regs cpu)) (neuro-rng-u16)))
        (4 (setf (neuro-regs-si (neuro-cpu-regs cpu)) (neuro-rng-u16)))
        (5 (setf (neuro-regs-di (neuro-cpu-regs cpu)) (neuro-rng-u16)))
        (6 (setf (neuro-regs-ip (neuro-cpu-regs cpu)) (neuro-rng-u16)))
        (7 (setf (neuro-regs-flags (neuro-cpu-regs cpu))
                 (logior (logand (neuro-rng-u16) #x0FD5) #x0002)))
        (8 (let ((addr (neuro-rng-range #x7000)))
             (aset (neuro-cpu-memory cpu) addr (neuro-rng-u8))))
        (9 (when (and (< (hash-table-count neuro-proc-table) 18)
                      (neuro-rng-bool 8))
             (let ((np (neuro-proc-spawn
                        (format "USER2_%d" neuro-proc-next-pid) 3
                        #x3000 #x3000 #x3000 #x3000 #x8000 #x3000)))
               (neuro-fs-append-log "NEURAL.LOG"
                                    (format "spawned USER2_%d" np)))))
        (10 (let ((sys566 (neuro-fs-sys566-dir)))
              (when sys566
                (let (names)
                  (maphash (lambda (k _) (push k names)) (neuro-dir-files sys566))
                  (when names
                    (let ((chosen (neuro-rng-choose names)))
                      (when-let* ((f (gethash chosen (neuro-dir-files sys566))))
                        (neuro-file-append f (vector (neuro-rng-u8))))))))))
        (_ (setf (neuro-process-state proc) "active")))
      (when (and (= neuro-threat 0) (neuro-rng-bool 20))
        (setq neuro-threat 1)
        (neuro-fs-append-log "NEURAL.LOG" "threat elevated to 1 (restless)")
        (neuro-emit "\n*** The system feels... different. ***")))))

(defun neuro-background-tick ()
  (cl-incf neuro-system-ticks)
  (unless (or neuro-game-beaten neuro-chase-active)
    (let ((proc (when neuro-second-user-pid (neuro-proc-get neuro-second-user-pid))))
      (unless (and proc (neuro-process-isolated proc))
        (when (and (< neuro-threat 3) neuro-second-user-pid)
          (let ((idle-ms (- (neuro-now-ms) neuro-last-user-action-ms)))
            (when (> idle-ms 15000)
              (let ((chance (neuro-clamp (+ (* neuro-threat 8) 3
                                            (/ neuro-idle-ticks 10)) 0 40)))
                (when (neuro-rng-bool chance)
                  (neuro-second-user-act)
                  (setq neuro-last-user-action-ms (neuro-now-ms))
                  (setq neuro-idle-ticks 0))))))))
    (when (= (mod neuro-system-ticks 200) 0)
      (neuro-fs-append-log "KERNEL.LOG"
                            (format "tick=%d threat=%d"
                                    neuro-system-ticks neuro-threat)))))

(defun neuro-idle-tick ()
  (cl-incf neuro-idle-ticks)
  (neuro-background-tick))

(defun neuro-uptime-s ()
  (/ (- (neuro-now-ms) neuro-session-start-ms) 1000))

(provide 'e0)
