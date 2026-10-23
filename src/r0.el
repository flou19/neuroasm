;;; r0.el -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
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

(defvar neuro-repl-buffer-name "*neuroasm*")

(defvar --cl-block-neuro-cmd-kill-- nil)
(defvar --cl-block-neuro-cmd-collect-- nil)
(defvar --cl-block-neuro-cmd-research-- nil)
(defvar neuro-repl-output-buffer nil)
(defvar neuro-repl--input-overlay nil)
(defvar neuro-repl--history nil)
(defvar neuro-repl--hist-pos 0)

(defun neuro-repl-print (text)

  (with-current-buffer (get-buffer-create neuro-repl-buffer-name)
    (goto-char (point-max))
    (insert text)
    (unless (string-suffix-p "\n" text)
      (insert "\n"))))

(defun neuro-repl-clear ()
  (with-current-buffer (get-buffer-create neuro-repl-buffer-name)
    (erase-buffer)))

(defun neuro-repl-banner ()
  nil)

(defun neuro-repl-help ()
  (neuro-repl-print "
Core:
  HELP RESEARCH CLS EXIT SAVE LOAD
  LS [-A] CD PWD CAT RM MKDIR TOUCH TREE FIND
  PS KILL KILLALL MONITOR SYS MEM
  D <addr>   U <addr>   E <addr> <bytes...>
  HEXDUMP <file>   WRITE <file> <text>
  TRACE <pid>  ISOLATE <pid>  RELEASE <pid>
  FRAGMENTS  COLLECT <file>  STATUS  HISTORY
  REVEAL   RESEARCH   DATE   WHOAMI   UNAME   ECHO

Hardware:
  PORTS   INB <port>   OUTB <port> <val>   IRQ <vec>

Entity & Neural:
  NEURAL           show neural core state
  CORRUPTION       Second User corruption level
  SIGNATURES       scan memory for entity signatures
  ASM              interactive assembly mode

Config & Events:
  CONFIG           dump runtime config
  EVENTS           event bus summary

Threat & Entity:
  Deleting/editing SYS.566 files raises threat.
  Threat 3+ triggers memory-space pursuit (WASD, reach E).
  Collect FRAGMENTs for research. REVEAL unlocks RESEARCH/.
  ISOLATE <pid> freezes second user temporarily.

Idle time feeds the entity.
"))

(defun neuro-cmd-ls (flag)

  (let* ((show  (or neuro-show-hidden (equal flag "-a") (equal flag "-A")))
         (dir   (neuro-fs-current))
         (names (neuro-dir-list-names dir show)))
    (neuro-repl-print (format "\n Directory of %s\n" (neuro-fs-cwd-string)))
    (dolist (name names)
      (if (string-suffix-p "/" name)
          (let* ((dname (substring name 0 -1))
                 (sub   (gethash dname (neuro-dir-dirs dir)))
                 (hid   (if (and sub (neuro-dir-hidden sub)) " (hidden)" "")))
            (neuro-repl-print (format "%-14s <DIR>%s" dname hid)))
        (when-let* ((f (gethash name (neuro-dir-files dir))))
          (let ((tag (if (member "FRAGMENT" (or (neuro-file-tags f) nil)) " [FRAG]" ""))
                (hid (if (neuro-file-hidden f) " (hidden)" "")))
            (neuro-repl-print (format "%-14s %7d bytes%s%s"
                                      name (neuro-file-size f) tag hid))))))
    (neuro-repl-print "")))

(defun neuro-cmd-cat (name)

  (let ((result (neuro-fs-cat name)))
    (pcase (car result)
      ('ok
       (neuro-repl-print (format "\n--- %s ---" name))
       (neuro-repl-print (cdr result))
       (when (neuro-fs-in-sys566-p)
         (neuro-repl-print "*** SYS.566 content accessed ***"))
       (neuro-fs-append-log "ACCESS.LOG" (format "CAT %s" name)))
      ('err
       (neuro-repl-print (format "Error: %s" (cdr result))))
      (_
       (neuro-repl-print (format "Unexpected result from fs-cat: %S" result))))))

(defun neuro-cmd-rm (name)
  (let ((in-sys (neuro-fs-in-sys566-p)))
    (let ((err (neuro-fs-remove name)))
      (if err
          (neuro-repl-print err)
        (neuro-repl-print (format "File %s deleted." name))
        (when in-sys
          (neuro-escalate 2)
          (when (< neuro-threat 3)
            (neuro-repl-print "*** The system trembles... ***")))
        (neuro-fs-append-log "ACCESS.LOG" (format "RM %s" name))))))

(defun neuro-cmd-ps ()
  (neuro-repl-print "\n PID  Name            State       Pri  Isol")
  (dolist (p (neuro-proc-list))
    (let ((state (if (and (eql neuro-second-user-pid (neuro-process-pid p))
                          (>= neuro-threat 2))
                     "pursuit"
                   (neuro-process-state p))))
      (neuro-repl-print (format "%4d  %-15s %-10s %3d  %s"
                                (neuro-process-pid p)
                                (neuro-process-name p)
                                state
                                (neuro-process-priority p)
                                (neuro-process-isolated p))))))

(defun neuro-cmd-kill (pid)
  (when (= pid 1)
    (neuro-repl-print "KILL: cannot kill kernel")
    (cl-return-from neuro-cmd-kill nil))
  (when (eql neuro-second-user-pid pid)
    (setq neuro-second-user-pid nil
          neuro-threat 0
          neuro-chase-active nil)
    (neuro-repl-print "Second user terminated. System stabilized (temporary?).")
    (neuro-fs-append-log "NEURAL.LOG" "USER2 killed by operator"))
  (if (neuro-proc-get pid)
      (progn (neuro-proc-kill pid)
             (neuro-repl-print (format "Process %d killed." pid)))
    (neuro-repl-print "Process not found.")))

(defun neuro-cmd-killall-user2 ()
  (let ((pids nil))
    (maphash (lambda (k p)
               (when (string-prefix-p "USER2" (neuro-process-name p))
                 (push k pids)))
             neuro-proc-table)
    (dolist (pid pids) (neuro-proc-kill pid))
    (setq neuro-second-user-pid nil
          neuro-threat 0
          neuro-chase-active nil)
    (if pids
        (neuro-repl-print (format "Terminated %d USER2 processes. Stabilized." (length pids)))
      (neuro-repl-print "No USER2 processes."))))

(defun neuro-cmd-monitor ()
  (neuro-repl-print "\n╔════════════════ CPU / Entity ════════════════╗")
  (when-let* ((p (neuro-proc-get neuro-proc-current-pid)))
    (neuro-repl-print (neuro-regs-dump (neuro-cpu-regs (neuro-process-cpu p))))
    (neuro-repl-print (format "Flags: %s"
                              (neuro-regs-flag-string
                               (neuro-cpu-regs (neuro-process-cpu p))))))
  (neuro-repl-print (format "Threat: %d (%s)" neuro-threat (neuro-mood-str neuro-threat)))
  (when neuro-second-user-pid
    (when-let* ((p (neuro-proc-get neuro-second-user-pid)))
      (neuro-repl-print (format "Entity: %s isol=%s"
                                (neuro-regs-dump (neuro-cpu-regs (neuro-process-cpu p)))
                                (neuro-process-isolated p)))))
  (neuro-repl-print (format "Fragments: %d  Cores: %d  Research: %s"
                            (hash-table-count neuro-fragments-collected)
                            neuro-cores-found
                            neuro-research-unlocked))
  (neuro-repl-print "╚══════════════════════════════════════════════╝\n"))

(defun neuro-cmd-sysinfo ()
  (neuro-repl-print "System: neuroasm [neuroasm v4.0 Emacs/C99]")
  (neuro-repl-print (format "Ticks: %d  Uptime: %ds" neuro-system-ticks (neuro-uptime-s)))
  (neuro-repl-print (format "Processes: %d  Files (approx): %d"
                            (hash-table-count neuro-proc-table)
                            (neuro-fs-count-files-recursive)))
  (neuro-repl-print (format "Second User: %s"
                            (if neuro-second-user-pid "PRESENT" "GONE")))
  (neuro-repl-print (format "Game beaten: %s" neuro-game-beaten))
  (neuro-repl-print (format "Commands this session: %d" neuro-total-commands)))

(defun neuro-cmd-mem ()
  (let ((used (apply #'+ (mapcar #'neuro-process-memory-used (neuro-proc-list)))))
    (neuro-repl-print (format "Memory: %d KB used / 640 KB" used))
    (neuro-repl-print "00000-9FFFF System RAM")
    (neuro-repl-print "A0000-BFFFF Video")
    (neuro-repl-print "C0000-FFFFF ROM/BIOS")))

(defun neuro-cmd-ports ()
  (neuro-repl-print "Selected I/O ports:")
  (neuro-repl-print "  0020: PIC master command")
  (neuro-repl-print "  0021: PIC master data")
  (neuro-repl-print "  0060: Keyboard data")
  (neuro-repl-print "  0061: Speaker/NMI")
  (neuro-repl-print "  03F8: COM1 data")
  (neuro-repl-print "  03F9: COM1 IER"))

(defun neuro-cmd-dump (addr-str)
  (condition-case nil
      (let* ((addr (string-to-number (string-remove-prefix "0x" addr-str) 16)))
        (when-let* ((p (neuro-proc-get neuro-proc-current-pid)))
          (let* ((mem (neuro-cpu-memory (neuro-process-cpu p)))
                 (len 128)
                 (data (let ((v (make-vector len 0)))
                         (dotimes (i len)
                           (aset v i (aref mem (logand (+ addr i) #xFFFF))))
                         v)))
            (neuro-repl-print (neuro-hex-dump data addr 8)))))
    (error (neuro-repl-print "invalid address"))))

(defun neuro-cmd-disasm (addr-str &optional n)
  (condition-case err
      (let* ((addr (string-to-number (string-remove-prefix "0x" addr-str) 16))
             (count (or n 16)))
        (when-let* ((p (neuro-proc-get neuro-proc-current-pid)))
          (neuro-repl-print (format "\nDisassembly at %04X:\n" addr))
          (neuro-repl-print (neuro-disasm-range (neuro-cpu-memory (neuro-process-cpu p))
                                                addr count))
          (neuro-repl-print "")))
    (error (neuro-repl-print (format "disasm error: %s" (cadr err))))))

(defun neuro-cmd-edit-mem (addr-str byte-strs)
  (condition-case nil
      (let* ((addr (string-to-number (string-remove-prefix "0x" addr-str) 16)))
        (when-let* ((p (neuro-proc-get neuro-proc-current-pid)))
          (let ((mem (neuro-cpu-memory (neuro-process-cpu p))))
            (let ((i 0))
              (dolist (bs byte-strs)
                (let ((b (string-to-number (string-remove-prefix "0x" bs) 16)))
                  (let ((off (logand (+ addr i) #xFFFF)))
                    (aset mem off (logand b #xFF)))
                  (setq i (1+ i)))))
            (neuro-repl-print "Memory updated."))))
    (error (neuro-repl-print "invalid address or bytes"))))

(defun neuro-cmd-trace (pid)
  (if-let* ((p (neuro-proc-get pid)))
      (progn
        (neuro-repl-print (format "Tracing PID %d for 8 steps..." pid))
        (let ((cpu (neuro-process-cpu p)))
          (dotimes (step 8)
            (condition-case err
                (progn
                  (neuro-cpu-step cpu)
                  (neuro-repl-print (format "  step %d: IP=%04X AX=%04X"
                                            step
                                            (neuro-regs-ip (neuro-cpu-regs cpu))
                                            (neuro-regs-ax (neuro-cpu-regs cpu)))))
              (error
               (neuro-repl-print (format "  step %d: %s" step (cadr err)))))))
        (neuro-fs-append-log "NEURAL.LOG" (format "TRACE on %d" pid)))
    (neuro-repl-print "Process not found.")))

(defun neuro-trigger-ending (name lines)
  (setq neuro-game-beaten t
        neuro-chase-active nil)
  (puthash name t neuro-endings-seen)
  (dolist (l lines) (neuro-repl-print l))
  (neuro-fs-append-log "NEURAL.LOG" (format "ENDING %s" name))
  (neuro-session-save)
  (neuro-repl-display))

(defun neuro-observe-tick ()
  (if (<= neuro-threat 1)
      (cl-incf neuro-observe-streak)
    (setq neuro-observe-streak 0))
  (when (and (not neuro-game-beaten)
             (>= neuro-observe-streak neuro-observe-required))
    (neuro-trigger-ending
     "observe"
     '("\n╔════════════════════════════════════════════════╗"
       "║   OBSERVATION COMPLETE                         ║"
       "║   threat held at 1 or below.                   ║"
       "╚════════════════════════════════════════════════╝"))))

(defun neuro-cmd-isolate (pid)
  (if-let* ((p (neuro-proc-get pid)))
      (progn
        (setf (neuro-process-isolated p) t)
        (setf (neuro-process-state p) "isolated")
        (neuro-repl-print (format "PID %d isolated. Entity activity frozen." pid))
        (neuro-fs-append-log "NEURAL.LOG" (format "ISOLATE %d" pid))
        (when (and (eql pid neuro-second-user-pid)
                   (>= neuro-threat 2)
                   (not neuro-game-beaten))
          (neuro-trigger-ending
           "contain"
           '("\n╔════════════════════════════════════════════════╗"
             "║   ENTITY CONTAINED                             ║"
             "║   second user frozen in isolation.             ║"
             "╚════════════════════════════════════════════════╝"))))
    (neuro-repl-print "Not found.")))

(defun neuro-cmd-release (pid)
  (if-let* ((p (neuro-proc-get pid)))
      (progn
        (setf (neuro-process-isolated p) nil)
        (setf (neuro-process-state p) "ready")
        (neuro-repl-print (format "PID %d released." pid)))
    (neuro-repl-print "Not found.")))

(defun neuro-cmd-fragments ()
  (let (frags)
    (maphash (lambda (k _) (push k frags)) neuro-fragments-collected)
    (neuro-repl-print (format "Collected fragments: %s" (string-join (sort frags #'string<) ", ")))
    (neuro-repl-print (format "Cores found: %d" neuro-cores-found))
    (when (>= (hash-table-count neuro-fragments-collected) 5)
      (neuro-repl-print "Enough fragments for advanced RESEARCH path."))))

(defun neuro-cmd-collect (name)
  (unless (neuro-fs-in-sys566-p)
    (neuro-repl-print "Must be in SYS.566 to collect.")
    (cl-return-from neuro-cmd-collect nil))
  (let ((dir (neuro-fs-current)))
    (if-let* ((f (gethash name (neuro-dir-files dir))))
        (if (or (member "FRAGMENT" (or (neuro-file-tags f) nil))
                (string-match-p "FRAGMENT" name))
            (progn
              (puthash name t neuro-fragments-collected)
              (neuro-repl-print (format "Fragment %s collected. Total: %d"
                                       name (hash-table-count neuro-fragments-collected)))
              (when (member "CORE" (neuro-file-tags f))
                (cl-incf neuro-cores-found)
                (neuro-repl-print "Core signature detected.")
                (neuro-neural-fragment-pulse neuro-neural-core 0.5))
              (neuro-neural-fragment-pulse neuro-neural-core 0.2)
              (when (and (>= (hash-table-count neuro-fragments-collected) 3)
                         (not neuro-research-unlocked))
                (neuro-repl-print "Hint: type REVEAL to unlock research protocols.")))
          (neuro-repl-print "File is not a marked FRAGMENT."))
      (neuro-repl-print "File not found."))))

(defun neuro-cmd-status ()
  (neuro-repl-print (format "Session: %d commands, %ds uptime"
                            neuro-total-commands (neuro-uptime-s)))
  (neuro-repl-print (format "Threat: %d  Beaten: %s  Fragments: %d  Research: %s"
                            neuro-threat
                            neuro-game-beaten
                            (hash-table-count neuro-fragments-collected)
                            neuro-research-unlocked)))

(defun neuro-cmd-history ()
  (neuro-repl-print "Recent neural log:")
  (let ((result (neuro-fs-cat "NEURAL.LOG")))
    (when (eq (car result) 'ok)
      (let ((lines (split-string (cdr result) "\n" t)))
        (dolist (l (last lines 8))
          (neuro-repl-print (format "  %s" l)))))))

(defun neuro-cmd-tree ()
  (cl-labels ((walk (dir prefix show)
                (maphash (lambda (n sub)
                           (when (or show (not (neuro-dir-hidden sub)))
                             (neuro-repl-print (format "%s%s/" prefix n))
                             (walk sub (concat prefix "  ") show)))
                         (neuro-dir-dirs dir))))
    (neuro-repl-print "/")
    (walk neuro-fs--root "  " neuro-show-hidden)))

(defun neuro-cmd-find (pat)
  (dolist (r (neuro-fs-find pat))
    (neuro-repl-print r)))

(defun neuro-cmd-hexdump (name)
  (let ((dir (neuro-fs-current)))
    (if-let* ((f (gethash name (neuro-dir-files dir))))
        (neuro-repl-print (neuro-hex-dump (neuro-file-content f) 0 16))
      (neuro-repl-print "File not found."))))

(defun neuro-cmd-research ()
  (when (and (not neuro-research-unlocked)
             (< (hash-table-count neuro-fragments-collected) 3))
    (neuro-repl-print "Insufficient data. Collect FRAGMENT files from SYS.566 and REVEAL.")
    (cl-return-from neuro-cmd-research nil))
  (when (and (not neuro-game-beaten)
             neuro-research-unlocked
             (>= (hash-table-count neuro-fragments-collected) 8))
    (neuro-trigger-ending
     "analysis"
     '("\n╔════════════════════════════════════════════════╗"
       "║   DEEP ANALYSIS COMPLETE                       ║"
       "║   signature 0xA5C3 / 0x566A resolved.          ║"
       "╚════════════════════════════════════════════════╝"))
    (cl-return-from neuro-cmd-research nil))
  (neuro-repl-print "
=== RESEARCH PROTOCOL ===
Origin: 1995-03-14, malformed ISR.
Signature target: 0xA5C3 / 0x566A
Paths:
  1. Neutralize via chase (classic)
  2. Contain via ISOLATE + freeze (research)
  3. Collect 8+ fragments then attempt deeper analysis
  4. Observe only (never raise threat above 1)
Hidden RESEARCH/ directory contains more.
Type REVEAL if not yet visible.
"))

(defun neuro-cmd-neural ()
  (neuro-repl-print "\n=== NEURAL CORE ===")
  (neuro-repl-print (format "Research progress: %.1f%%"
                            (* (neuro-neural-research-progress neuro-neural-core) 100)))
  (neuro-repl-print (format "Awakened: %s  Cores online: %d"
                            (neuro-neural-awakened neuro-neural-core)
                            (neuro-neural-cores-online neuro-neural-core)))
  (let* ((inputs (vector (float neuro-threat)
                         (float (hash-table-count neuro-fragments-collected))
                         (float neuro-cores-found)
                         (if neuro-research-unlocked 1.0 0.0)
                         (float (/ neuro-idle-ticks 100.0))
                         0.0 0.0 0.0))
         (out (neuro-neural-forward neuro-neural-core inputs)))
    (neuro-repl-print (format "Neural output: [%.3f %.3f %.3f %.3f]"
                              (aref out 0) (aref out 1) (aref out 2) (aref out 3))))
  (neuro-repl-print "==================\n"))

(defun neuro-cmd-asm ()
  (neuro-repl-print "Assembly mode. Enter instructions, blank line to finish, 'run' to execute.")
  (let ((lines nil)
        (done nil))
    (while (not done)
      (let ((line (read-string "asm> ")))
        (cond
         ((string-empty-p line)
          (setq done t))
         ((equal (downcase line) "run")
          (setq done t)
          (let* ((src (string-join (nreverse lines) "\n"))
                 (code (neuro-asm-assemble src)))
            (when-let* ((p (neuro-proc-get neuro-proc-current-pid)))
              (let ((cpu (neuro-process-cpu p)))
                (neuro-cpu-write-mem cpu #x100 code)
                (setf (neuro-regs-ip (neuro-cpu-regs cpu)) #x100)
                (neuro-repl-print (format "Loaded %d bytes at 0x100, executing 16 steps..."
                                          (length code)))
                (dotimes (_ 16)
                  (condition-case err
                      (neuro-cpu-step cpu)
                    (error (neuro-repl-print (format "  halt: %s" (cadr err))))))))))
         (t (push line lines)))))
    (neuro-repl-print "Assembly mode exit.")))

(defun neuro-run-chase ()
  (neuro-repl-clear)
  (neuro-repl-print "╔══════════════════════════════════════════════════════════╗")
  (neuro-repl-print "║   MEMORY SPACE PURSUIT                                   ║")
  (neuro-repl-print "║   Reach EXIT 'E'. WASD move. Entity hunts.              ║")
  (neuro-repl-print "╚══════════════════════════════════════════════════════════╝")
  (neuro-chase-init)
  (let ((done nil))
    (while (and neuro-chase-active (not done))
      (neuro-repl-clear)
      (neuro-repl-print (neuro-chase-render))
      (when neuro-second-user-pid
        (when-let* ((p (neuro-proc-get neuro-second-user-pid)))
          (neuro-repl-print (format "Entity: %s"
                                    (neuro-regs-dump
                                     (neuro-cpu-regs (neuro-process-cpu p)))))))
      (neuro-repl-display)
      (let ((key (read-string "move> " nil nil "")))
        (cond
         ((member (downcase key) '("w" "a" "s" "d"))
          (neuro-chase-move-player (downcase key)))
         ((equal (downcase key) "q")
          (setq done t)))
        (neuro-chase-move-enemy)
        (cond
         ((neuro-chase-caught-p)
          (neuro-repl-print "\nTHE SECOND USER CAUGHT YOU. GAME OVER.")
          (neuro-repl-display)
          (sit-for 2)
          (setq neuro-chase-active nil done t))
         ((neuro-chase-escaped-p)
          (setq neuro-chase-active nil
                neuro-game-beaten t
                neuro-threat 0)
          (let (u2-pids)
            (maphash (lambda (k p)
                       (when (string-prefix-p "USER2" (neuro-process-name p))
                         (push k u2-pids)))
                     neuro-proc-table)
            (dolist (pid u2-pids) (neuro-proc-kill pid)))
          (setq neuro-second-user-pid nil)
          (puthash "neutralize" t neuro-endings-seen)
          (neuro-repl-print "\n╔════════════════════════════════════════════════╗")
          (neuro-repl-print "║   RESET OK                                     ║")
          (neuro-repl-print "║   second user cleared.                         ║")
          (neuro-repl-print "╚════════════════════════════════════════════════╝")
          (neuro-fs-append-log "NEURAL.LOG" "Entity neutralized via EXIT")
          (neuro-session-save)
          (neuro-repl-display)
          (sit-for 2)
          (setq done t))))))
  (unless neuro-chase-active
    (neuro-repl-print "Chase ended.")))

(defun neuro-repl-display ()
  (when-let* ((buf (get-buffer neuro-repl-buffer-name)))
    (with-current-buffer buf
      (display-buffer buf)
      (when-let* ((win (get-buffer-window buf)))
        (set-window-point win (point-max))))))

(defun neuro-cwd-matches-locked-p (path)

  (neuro-sec-is-locked (upcase path)))

(defun neuro-bootstrap ()
  (neuro-proc-init)
  (neuro-proc-spawn "KERNEL"  0 #xF000 0 0 0 #xFFFE 0)
  (neuro-proc-spawn "MONITOR" 5 #x1000 #x1000 #x1000 #x1000 #x8000 #x100)

  (let ((u2-pid (neuro-proc-spawn "USER2" 3 #x2000 #x2000 #x2000 #x2000 #xFF00 #x0100)))
    (setq neuro-second-user-pid u2-pid)
    (when-let* ((p (neuro-proc-get u2-pid)))
      (let ((mem (neuro-cpu-memory (neuro-process-cpu p))))
        (dotimes (i 4096)
          (aset mem (+ #x2000 i) (logand (+ (* i 13) #x55) #xFF))))))
  (setq neuro-proc-current-pid 1)
  (neuro-fs-append-log "BOOT.LOG"   "shell online")
  (neuro-fs-append-log "NEURAL.LOG" "Entity registered, threat=0"))

(defun neuro-repl-dispatch (line)
  (let* ((parts (split-string line))
         (cmd   (upcase (car parts)))
         (args  (cdr parts)))
    (pcase cmd
      ("HELP"     (neuro-repl-help))
      ("RESEARCH" (neuro-cmd-research))
      ("CLS"      (neuro-repl-clear))
      ("CLEAR"    (neuro-repl-clear))
      ("EXIT"     (neuro-session-save) (neuro-repl-print "System halted. Session saved.") 'exit)
      ("QUIT"     (neuro-session-save) (neuro-repl-print "System halted. Session saved.") 'exit)
      ("HALT"     (neuro-session-save) (neuro-repl-print "System halted. Session saved.") 'exit)
      ("LS"       (neuro-cmd-ls (car args)))
      ("DIR"      (neuro-cmd-ls (car args)))
      ("CD"       (if (car args)
                      (let ((err (neuro-fs-change-dir (car args))))
                        (when err (neuro-repl-print err)))
                    (neuro-repl-print "CD: missing directory")))
      ("PWD"      (neuro-repl-print (neuro-fs-cwd-string)))
      ("CAT"      (if (car args) (neuro-cmd-cat (car args))
                    (neuro-repl-print "CAT: missing file")))
      ("TYPE"     (if (car args) (neuro-cmd-cat (car args))
                    (neuro-repl-print "TYPE: missing file")))
      ("RM"       (if (car args) (neuro-cmd-rm (car args))
                    (neuro-repl-print "RM: missing file")))
      ("DEL"      (if (car args) (neuro-cmd-rm (car args))
                    (neuro-repl-print "DEL: missing file")))
      ("MKDIR"    (if (car args)
                      (let ((err (neuro-fs-mkdir (car args))))
                        (if err (neuro-repl-print err)
                          (neuro-repl-print "Directory created.")))
                    (neuro-repl-print "MKDIR: missing name")))
      ("MD"       (if (car args)
                      (let ((err (neuro-fs-mkdir (car args))))
                        (if err (neuro-repl-print err)
                          (neuro-repl-print "Directory created.")))
                    (neuro-repl-print "MD: missing name")))
      ("PS"       (neuro-cmd-ps))
      ("KILL"     (if (car args)
                      (neuro-cmd-kill (string-to-number (car args)))
                    (neuro-repl-print "KILL: missing PID")))
      ("KILLALL"  (neuro-cmd-killall-user2))
      ("MONITOR"  (neuro-cmd-monitor))
      ("REG"      (neuro-cmd-monitor))
      ("SYS"      (neuro-cmd-sysinfo))
      ("INFO"     (neuro-cmd-sysinfo))
      ("MEM"      (neuro-cmd-mem))
      ("PORTS"    (neuro-cmd-ports))
      ("D"        (if (car args) (neuro-cmd-dump (car args))
                    (neuro-repl-print "D: missing address")))
      ("U"        (if (car args) (neuro-cmd-disasm (car args))
                    (neuro-repl-print "U: missing address")))
      ("E"        (if (and (car args) (cadr args))
                      (neuro-cmd-edit-mem (car args) (cdr args))
                    (neuro-repl-print "E: address byte [byte...]")))
      ("TRACE"    (if (car args) (neuro-cmd-trace (string-to-number (car args)))
                    (neuro-repl-print "TRACE: missing PID")))
      ("ISOLATE"  (if (car args) (neuro-cmd-isolate (string-to-number (car args)))
                    (neuro-repl-print "ISOLATE: missing PID")))
      ("RELEASE"  (if (car args) (neuro-cmd-release (string-to-number (car args)))
                    (neuro-repl-print "RELEASE: missing PID")))
      ("FRAGMENTS" (neuro-cmd-fragments))
      ("COLLECT"  (if (car args) (neuro-cmd-collect (car args))
                    (neuro-repl-print "COLLECT: missing file")))
      ("STATUS"   (neuro-cmd-status))
      ("HISTORY"  (neuro-cmd-history))
      ("SAVE"     (neuro-session-save) (neuro-repl-print "Session state written."))
      ("LOAD"     (if (neuro-session-load)
                      (neuro-repl-print "Session restored.")
                    (neuro-repl-print "No valid session or load failed.")))
      ("TREE"     (neuro-cmd-tree))
      ("FIND"     (if (car args) (neuro-cmd-find (car args))
                    (neuro-repl-print "FIND: missing pattern")))
      ("DATE"     (neuro-repl-print (format "System time: %d ms" (neuro-now-ms))))
      ("WHOAMI"   (neuro-repl-print "operator (threat-aware shell)"))
      ("UNAME"    (neuro-repl-print "neuroasm [neuroasm v4.0 Emacs/C]"))
      ("ECHO"     (when args (neuro-repl-print (string-join args " "))))
      ("TOUCH"    (if (car args)
                      (progn (neuro-fs-write-file (car args) "")
                             (neuro-repl-print "Touched."))
                    (neuro-repl-print "TOUCH: missing file")))
      ("HEXDUMP"  (if (car args) (neuro-cmd-hexdump (car args))
                    (neuro-repl-print "HEXDUMP: missing file")))
      ("REVEAL"   (neuro-fs-reveal-hidden "RESEARCH")
                  (setq neuro-show-hidden t neuro-research-unlocked t)
                  (neuro-repl-print "Hidden research directory revealed.")
                  (neuro-fs-append-log "NEURAL.LOG" "RESEARCH directory made visible"))
      ("NEURAL"   (neuro-cmd-neural))
      ("ASM"      (neuro-cmd-asm))
      ("DAEMON"
       (pcase (upcase (or (car args) "STATUS"))
         ("START"  (neuro--ensure-daemon)
                   (neuro-repl-print "Daemon started."))
         ("STOP"   (neuro/daemon-stop)
                   (neuro-repl-print "Daemon stopped."))
         ("PING"   (neuro-repl-print
                    (if (neuro/daemon-ping) "PONG" "Daemon not responding.")))
         ("STATUS" (neuro-repl-print
                    (format "Daemon: %s"
                            (if (neuro--daemon-live-p) "running" "stopped"))))
         (_        (neuro-repl-print "DAEMON START|STOP|PING|STATUS"))))
      ("RUN"
       (if (car args)
           (let* ((n (string-to-number (car args)))
                  (cb (lambda (cycles)
                        (neuro-repl-print
                         (format "Done. Cycles: %d" cycles)))))
             (if (> n 10000)
                 (progn
                   (neuro-repl-print (format "Running %d steps async via daemon..." n))
                   (neuro/run-async n cb))
               (let ((cycles (neuro/run-sync n)))
                 (neuro-repl-print (format "Done. Cycles: %d" (or cycles 0))))))
         (neuro-repl-print "RUN <n>")))
      ("BENCH"
       (if (car args)
           (let ((out (neuro/bench (car args))))
             (neuro-repl-print out))
         (neuro-repl-print "BENCH <n>")))
      ("BACKENDS" (let ((st (neuro/backend-status)))
                    (neuro-repl-print
                     (format "module=%s  daemon=%s  cli=%s"
                             (plist-get st :module)
                             (plist-get st :daemon)
                             (plist-get st :cli)))))
      ("WRITE"    (if (and (car args) (cadr args))
                      (progn
                        (neuro-fs-write-file (car args) (string-join (cdr args) " "))
                        (neuro-repl-print (format "Written: %s" (car args))))
                    (neuro-repl-print "WRITE: filename content...")))
      ("CONFIG"   (neuro-repl-print "\n=== CONFIG ===")
                  (dolist (l (neuro-config-dump)) (neuro-repl-print l))
                  (neuro-repl-print ""))
      ("EVENTS"   (let ((summary (neuro-event-summary)))
                    (neuro-repl-print "\n=== EVENT BUS ===")
                    (dolist (pair (seq-take summary 10))
                      (neuro-repl-print (format "  %-20s %d" (car pair) (cdr pair))))
                    (neuro-repl-print "")))
      ("CORRUPTION" (neuro-repl-print (format "Second User corruption: %s (level %d)"
                                              (neuro-u2-corruption-str)
                                              neuro-u2-corruption-level)))
      ("SIGNATURES" (when neuro-second-user-pid
                      (when-let* ((p (neuro-proc-get neuro-second-user-pid)))
                        (let ((sigs (neuro-u2-mem-signature-check
                                     (neuro-process-cpu p))))
                          (if sigs
                              (dolist (s sigs)
                                (neuro-repl-print (format "  Sig %04X at %04X"
                                                          (car s) (cdr s))))
                            (neuro-repl-print "No signatures found."))))))
      ("IOPORTS"    (neuro-cmd-ports))
      ("INB"      (if (car args)
                      (let ((port (string-to-number (string-remove-prefix "0x" (car args)) 16)))
                        (neuro-repl-print (format "Port %04X: %02X" port (neuro-port-inb port))))
                    (neuro-repl-print "INB: missing port")))
      ("OUTB"     (if (and (car args) (cadr args))
                      (let ((port (string-to-number (string-remove-prefix "0x" (car args)) 16))
                            (val  (string-to-number (string-remove-prefix "0x" (cadr args)) 16)))
                        (neuro-port-outb port val)
                        (neuro-repl-print (format "Port %04X <- %02X" port val)))
                    (neuro-repl-print "OUTB: port value")))
      ("IRQ"      (if (car args)
                      (progn
                        (neuro-irq-request (string-to-number (car args)))
                        (neuro-repl-print (format "IRQ %s raised." (car args))))
                    (neuro-repl-print "IRQ: missing vector")))
      (_          (cond
                   ((or (string-suffix-p ".EXE" cmd) (string-suffix-p ".COM" cmd))
                    (neuro-repl-print (format "Launch of %s simulated." cmd)))
                   (t (neuro-repl-print (format "Bad command or file name: %s" cmd))))))))

(defun neuro-repl-run ()
  (interactive)
  (neuro-fs-init)
  (neuro-entity-init)
  (neuro-sec-init)
  (neuro-sandbox-init)
  (neuro-hw-init)
  (neuro-event-init)
  (neuro-u2-init)

  (setq neuro-output-hook #'neuro-repl-print)
  (neuro-config-inc "BOOT")
  (neuro-bootstrap)
  (neuro-event-emit-boot)
  (neuro-session-load)
  (let ((buf (get-buffer-create neuro-repl-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'neuro-repl-mode)
        (neuro-repl-mode)))
    (switch-to-buffer buf))
  (neuro-repl-clear)
  (neuro-repl-banner)
  (let ((running t))
    (while running
      (when neuro-chase-active
        (neuro-run-chase))
      (let ((threat neuro-threat))
        (when (and (> threat 0) neuro-second-user-pid
                   (> (- (neuro-now-ms) neuro-last-u2-msg-ms) 8000))
          (when-let* ((p (neuro-proc-get neuro-second-user-pid)))
            (unless (neuro-process-isolated p)
              (neuro-repl-print (format "\r[Second User] %s"
                                        (neuro-regs-dump
                                         (neuro-cpu-regs (neuro-process-cpu p)))))
              (setq neuro-last-u2-msg-ms (neuro-now-ms))))))
      (neuro-repl-display)
      (let* ((prompt (format "%s> " (neuro-fs-cwd-string)))
             (line   (condition-case nil
                         (read-string prompt)
                       (quit nil))))
        (if (null line)
            (setq running nil)
          (let ((trimmed (string-trim line)))
            (if (string-empty-p trimmed)
                (neuro-idle-tick)
              (cl-incf neuro-total-commands)
              (setq neuro-last-user-action-ms (neuro-now-ms)
                    neuro-idle-ticks 0)
              (push trimmed neuro-repl--history)
              (neuro-event-emit-command trimmed)
              (neuro-config-inc "CMDS")
              (let ((result (neuro-repl-dispatch trimmed)))
                (when (eq result 'exit)
                  (setq running nil)))
              (neuro-observe-tick)
              (neuro-background-tick)
              (neuro-drivers-tick)

              (when-let* ((p (neuro-proc-get neuro-proc-current-pid)))
                (neuro-hw-dispatch-irq (neuro-process-cpu p)))
              (neuro-event-dispatch)
              (let ((u2-events (neuro-event-history-by-kind 'second-user-speak 1)))
                (when u2-events
                  (let ((msg (plist-get (car u2-events) :data)))
                    (when (and msg (stringp msg))
                      (neuro-repl-print (format "\n[Second User] %s\n" msg))))))
              (when neuro-second-user-pid
                (neuro-u2-tick neuro-threat)))))))))

(define-derived-mode neuro-repl-mode fundamental-mode "NeuroASM"
  "Major mode for neuroasm DOS simulator.")

(provide 'r0)
