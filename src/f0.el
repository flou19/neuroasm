;;; f0.el -*- lexical-binding: t -*-

(require 'u0)

(cl-defstruct neuro-file
  name content mode created modified accessed hidden system readonly tags)

(cl-defstruct neuro-dir
  name files dirs hidden)

(defun neuro-file-new (name content)

  (let ((t0 (neuro-now-ms)))
    (make-neuro-file :name name :content (cond
                                           ((stringp content) (string-to-vector content))
                                           ((vectorp content)  (vconcat content))
                                           (t (vector)))
                     :mode #o644 :created t0 :modified t0 :accessed t0
                     :hidden nil :system nil :readonly nil :tags nil)))

(defun neuro-file-size (f) (length (neuro-file-content f)))

(defun neuro-file-is-text (f)

  (let ((c (neuro-file-content f)))
    (and (> (length c) 0)
         (cl-every (lambda (b) (neuro-is-printable b)) c))))

(defun neuro-file-as-str (f)

  (let ((v (neuro-file-content f)))
    (cond ((stringp v) v)
          ((vectorp v) (concat v))
          (t ""))))

(defun neuro-file-touch (f)
  (setf (neuro-file-modified f) (neuro-now-ms))
  (setf (neuro-file-accessed f) (neuro-now-ms)))

(defun neuro-file-append (f bytes)
  (let* ((old (neuro-file-content f))
         (extra (if (stringp bytes) (string-to-vector bytes) bytes))
         (new-v (vconcat old extra)))
    (setf (neuro-file-content f) new-v)
    (neuro-file-touch f)))

(defun neuro-dir-new (name)
  (make-neuro-dir :name name :files (make-hash-table :test 'equal)
                  :dirs  (make-hash-table :test 'equal) :hidden nil))

(defun neuro-dir-list-names (dir show-hidden)

  (let (dirs files)
    (maphash (lambda (k v)
               (when (or show-hidden (not (neuro-dir-hidden v)))
                 (push (concat k "/") dirs)))
             (neuro-dir-dirs dir))
    (maphash (lambda (k v)
               (when (or show-hidden (not (neuro-file-hidden v)))
                 (push k files)))
             (neuro-dir-files dir))
    (append (sort dirs #'string<) (sort files #'string<))))

(defvar neuro-fs--root nil)
(defvar neuro-fs--cwd nil)

(defun neuro-fs-init ()

  (setq neuro-fs--root (neuro-dir-new "/")
        neuro-fs--cwd  (list "/"))
  (neuro-fs--make-structure))

(defun neuro-fs--put-file (dir name content &optional tags hidden sys)
  (let ((f (neuro-file-new name content)))
    (when tags   (setf (neuro-file-tags f) tags))
    (when hidden (setf (neuro-file-hidden f) t))
    (when sys    (setf (neuro-file-system f) t))
    (puthash name f (neuro-dir-files dir))))

(defun neuro-fs--make-structure ()
  (let ((root neuro-fs--root))
    (let ((sys566 (neuro-dir-new "SYS.566")))
      (neuro-fs--put-file sys566 "KERNEL.DAT"
        "CORE PROCESS TABLE\r\nPID 1 KERNEL F000:0000\r\nPID 2 MONITOR 1000:1000\r\nPID 3 USER2 2000:2000\r\n" '("SYSTEM") nil t)
      (neuro-fs--put-file sys566 "ENTITY.BIN"
        (let ((v (make-vector 64 0)))
          (dotimes (i 64) (aset v i (logand (+ (* i 13) #x55) #xFF))) v)
        '("CORE" "FRAGMENT") nil t)
      (neuro-fs--put-file sys566 "FRAGMENT.001"
        "NEURAL FRAGMENT 001\r\nORIGIN: 1995-03-14 ISR MISFIRE\r\nSIG: 0xA5C3\r\n" '("FRAGMENT"))
      (neuro-fs--put-file sys566 "FRAGMENT.002"
        "NEURAL FRAGMENT 002\r\nCALCULATION: 0x566A ^ 0xA5C3\r\nSTATUS: DORMANT\r\n" '("FRAGMENT"))
      (neuro-fs--put-file sys566 "FRAGMENT.003"
        "NEURAL FRAGMENT 003\r\nSECOND USER TRACE\r\nLAST SEEN: BOOT+00:01:12\r\n" '("FRAGMENT"))
      (neuro-fs--put-file sys566 "THREAT.LOG" "" nil nil t)
      (setf (neuro-dir-hidden sys566) nil)
      (puthash "SYS.566" sys566 (neuro-dir-dirs root)))

    (let ((etc (neuro-dir-new "ETC")))
      (neuro-fs--put-file etc "CONFIG.SYS"
        "DEVICE=HIMEM.SYS\r\nFILES=30\r\nBUFFERS=20\r\nSTACKS=9,256\r\n")
      (neuro-fs--put-file etc "AUTOEXEC.BAT"
        "@ECHO OFF\r\nPROMPT $P$G\r\nPATH C:\\DOS;C:\\WINDOWS\r\nSET TEMP=C:\\TEMP\r\n")
      (neuro-fs--put-file etc "MOTD"
        "Welcome to neuroasm. Type HELP for commands.\r\nThe system is alive. SYS.566 watches.\r\n")
      (puthash "ETC" etc (neuro-dir-dirs root)))

    (let ((dos (neuro-dir-new "DOS")))
      (neuro-fs--put-file dos "COMMAND.COM" "MS-DOS Command Interpreter v6.22" '("SYSTEM") nil t)
      (neuro-fs--put-file dos "FORMAT.COM"  "DOS Format Utility" nil nil t)
      (neuro-fs--put-file dos "FDISK.EXE"   "Fixed Disk Setup" nil nil t)
      (puthash "DOS" dos (neuro-dir-dirs root)))

    (let ((logs (neuro-dir-new "LOGS")))
      (neuro-fs--put-file logs "BOOT.LOG"   "")
      (neuro-fs--put-file logs "NEURAL.LOG" "")
      (neuro-fs--put-file logs "KERNEL.LOG" "")
      (neuro-fs--put-file logs "ACCESS.LOG" "")
      (puthash "LOGS" logs (neuro-dir-dirs root)))

    (let ((research (neuro-dir-new "RESEARCH")))
      (setf (neuro-dir-hidden research) t)
      (neuro-fs--put-file research "PROTOCOL.TXT"
        "=== RESEARCH PROTOCOL ===\r\nOrigin: 1995-03-14, malformed ISR.\r\nSig target: 0xA5C3 / 0x566A\r\n\r\nPaths:\r\n  1. Neutralize via chase\r\n  2. Contain via ISOLATE\r\n  3. Collect 8+ fragments then deeper analysis\r\n  4. Observe only (keep threat <= 1)\r\n")
      (neuro-fs--put-file research "ENTITY_ANALYSIS.DAT"
        "SECOND USER ENTITY PROFILE\r\nDesignation: USER2 / SYS.566\r\nFirst detected: 1995-03-14\r\nBehavior: autonomous, memory-invasive\r\nWeakness: ISOLATE command, EXIT node\r\n")
      (puthash "RESEARCH" research (neuro-dir-dirs root)))

    (neuro-fs--put-file root "AUTOEXEC.BAT"
      "@ECHO OFF\r\nPROMPT $P$G\r\n" nil nil t)
    (neuro-fs--put-file root "CONFIG.SYS"
      "DEVICE=HIMEM.SYS\r\nFILES=30\r\n" nil nil t)))

(defun neuro-fs-cwd-string ()

  (cond
   ((or (null neuro-fs--cwd) (equal neuro-fs--cwd (list "/"))) "/")
   (t (concat "/" (string-join (cdr neuro-fs--cwd) "/")))))

(defun neuro-fs--navigate (dir path-list)
  "Navigate from DIR through PATH-LIST. Returns nil if any segment missing."
  (if (null path-list)
      dir
    (let ((seg (car path-list)))
      (when-let* ((sub (gethash seg (neuro-dir-dirs dir))))
        (neuro-fs--navigate sub (cdr path-list))))))

(defun neuro-fs-current ()
  (if (equal neuro-fs--cwd (list "/"))
      neuro-fs--root
    (neuro-fs--navigate neuro-fs--root (cdr neuro-fs--cwd))))

(defun neuro-fs-change-dir (path)
  (cond
   ((equal path "..")
    (when (> (length neuro-fs--cwd) 1)
      (setq neuro-fs--cwd (butlast neuro-fs--cwd)))
    nil)
   ((equal path "/")
    (setq neuro-fs--cwd (list "/"))
    nil)
   (t
    (let ((dir (neuro-fs-current)))
      (if-let* ((sub (gethash path (neuro-dir-dirs dir))))
          (progn
            (setq neuro-fs--cwd (append neuro-fs--cwd (list path)))
            nil)
        (format "Directory not found: %s" path))))))

(defun neuro-fs-cat (name)
  (let ((dir (neuro-fs-current)))
    (if-let* ((f (gethash name (neuro-dir-files dir))))
        (cons 'ok (neuro-file-as-str f))
      (cons 'err (format "File not found: %s" name)))))

(defun neuro-fs-write-file (name content)
  (unless (neuro-validate-filename name)
    (error "Invalid filename: %s" name))
  (let ((dir (neuro-fs-current)))
    (let ((f (or (gethash name (neuro-dir-files dir))
                 (neuro-file-new name ""))))
      (setf (neuro-file-content f)
            (if (stringp content) (string-to-vector content)
              (or content (vector))))
      (neuro-file-touch f)
      (puthash name f (neuro-dir-files dir)))))

(defun neuro-fs-remove (name)
  (let ((dir (neuro-fs-current)))
    (if (gethash name (neuro-dir-files dir))
        (progn (remhash name (neuro-dir-files dir)) nil)
      (format "File not found: %s" name))))

(defun neuro-fs-mkdir (name)
  (unless (neuro-validate-dirname name)
    (error "Invalid dirname: %s" name))
  (let ((dir (neuro-fs-current)))
    (if (gethash name (neuro-dir-dirs dir))
        (format "Directory exists: %s" name)
      (puthash name (neuro-dir-new name) (neuro-dir-dirs dir))
      nil)))

(defun neuro-fs-append-log (name text)

  (let* ((logs-dir (gethash "LOGS" (neuro-dir-dirs neuro-fs--root))))
    (when logs-dir
      (let ((f (or (gethash name (neuro-dir-files logs-dir))
                   (let ((nf (neuro-file-new name "")))
                     (puthash name nf (neuro-dir-files logs-dir))
                     nf))))
        (neuro-file-append f (concat text "
"))))))

(defun neuro-fs-reveal-hidden (dirname)
  (when-let* ((d (gethash dirname (neuro-dir-dirs neuro-fs--root))))
    (setf (neuro-dir-hidden d) nil)))

(defun neuro-fs-count-files-recursive (&optional dir)
  (let* ((d (or dir neuro-fs--root))
         (n (hash-table-count (neuro-dir-files d))))
    (maphash (lambda (_k sub) (setq n (+ n (neuro-fs-count-files-recursive sub))))
             (neuro-dir-dirs d))
    n))

(defun neuro-fs-find (pattern &optional dir path)
  (let* ((d (or dir neuro-fs--root))
         (p (or path ""))
         results)
    (maphash (lambda (k _f)
               (when (string-match-p (regexp-quote (upcase pattern)) (upcase k))
                 (push (concat p "/" k) results)))
             (neuro-dir-files d))
    (maphash (lambda (k sub)
               (setq results
                     (append results
                             (neuro-fs-find pattern sub (concat p "/" k)))))
             (neuro-dir-dirs d))
    results))

(defun neuro-fs-sys566-dir ()
  (gethash "SYS.566" (neuro-dir-dirs neuro-fs--root)))

(defun neuro-fs-in-sys566-p ()

  (cl-some (lambda (seg) (string= (upcase seg) "SYS.566")) neuro-fs--cwd))

(provide 'f0)
