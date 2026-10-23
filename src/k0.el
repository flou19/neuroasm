;;; k0.el -*- lexical-binding: t -*-

(require 'u0)

(defconst neuro-chase-W 24)
(defconst neuro-chase-H 18)

(defvar neuro-chase--map      nil)
(defvar neuro-chase--px       2)
(defvar neuro-chase--py       2)
(defvar neuro-chase--ex       20)
(defvar neuro-chase--ey       15)
(defvar neuro-chase--start-ms 0)
(defvar neuro-chase--exits    nil)

(defun neuro-chase--idx (x y) (+ (* y neuro-chase-W) x))

(defun neuro-chase--set (x y c)
  (aset neuro-chase--map (neuro-chase--idx x y) c))

(defun neuro-chase--get (x y)
  (if (and (>= x 0) (< x neuro-chase-W) (>= y 0) (< y neuro-chase-H))
      (aref neuro-chase--map (neuro-chase--idx x y))
    ?#))

(defun neuro-chase--passable (x y) (/= (neuro-chase--get x y) ?#))

(defun neuro-chase--reachable-p (sx sy tx ty)
  (let ((visited (make-hash-table :test #'equal))
        (queue   (list (cons sx sy)))
        (found   nil))
    (puthash (cons sx sy) t visited)
    (while (and queue (not found))
      (let* ((cur (pop queue))
             (cx (car cur)) (cy (cdr cur)))
        (when (and (= cx tx) (= cy ty)) (setq found t))
        (unless found
          (dolist (d (list (cons -1 0) (cons 1 0) (cons 0 -1) (cons 0 1)))
            (let ((nx (+ cx (car d))) (ny (+ cy (cdr d))))
              (when (and (neuro-chase--passable nx ny)
                         (not (gethash (cons nx ny) visited)))
                (puthash (cons nx ny) t visited)
                (push (cons nx ny) queue)))))))
    found))

(defun neuro-chase-init ()
  (setq neuro-chase--map    (make-vector (* neuro-chase-W neuro-chase-H) ?.)
        neuro-chase--exits  nil
        neuro-chase--start-ms (neuro-now-ms))

  (dotimes (y neuro-chase-H)
    (dotimes (x neuro-chase-W)
      (when (or (= x 0) (= x (1- neuro-chase-W))
                (= y 0) (= y (1- neuro-chase-H)))
        (neuro-chase--set x y ?#))))

  (setq neuro-chase--px 2 neuro-chase--py 2
        neuro-chase--ex 20 neuro-chase--ey 15)

  (let ((attempts 0))
    (catch 'ok
      (while (< attempts 30)
        (cl-incf attempts)

        (dotimes (y (- neuro-chase-H 2))
          (dotimes (x (- neuro-chase-W 2))
            (neuro-chase--set (+ x 1) (+ y 1) ?.)))

        (dotimes (y (- neuro-chase-H 2))
          (dotimes (x (- neuro-chase-W 2))
            (let ((ax (+ x 1)) (ay (+ y 1)))
              (unless (or (and (= ax neuro-chase--px) (= ay neuro-chase--py))
                          (and (= ax neuro-chase--ex) (= ay neuro-chase--ey)))
                (when (neuro-rng-bool 18)
                  (neuro-chase--set ax ay ?#))))))

        (when (neuro-chase--reachable-p
               neuro-chase--px neuro-chase--py
               neuro-chase--ex neuro-chase--ey)
          (throw 'ok t)))))

  (let ((placed 0) (tries 0))
    (while (and (< placed 3) (< tries 100))
      (cl-incf tries)
      (let ((ex (+ 2 (neuro-rng-range 20)))
            (ey (+ 2 (neuro-rng-range 14))))
        (when (and (neuro-chase--passable ex ey)
                   (neuro-chase--reachable-p neuro-chase--px neuro-chase--py ex ey))
          (neuro-chase--set ex ey ?E)
          (push (cons ex ey) neuro-chase--exits)
          (cl-incf placed))))))

(defun neuro-chase-render ()

  (let ((rows nil))
    (dotimes (y neuro-chase-H)
      (let ((row ""))
        (dotimes (x neuro-chase-W)
          (setq row (concat row
                            (cond
                             ((and (= x neuro-chase--px) (= y neuro-chase--py)) "@")
                             ((and (= x neuro-chase--ex) (= y neuro-chase--ey)) "█")
                             (t (string (neuro-chase--get x y)))))))
        (push row rows)))
    (let ((elapsed (/ (float (- (neuro-now-ms) neuro-chase--start-ms)) 1000.0)))
      (concat
       "╔══════════════ Memory Map ══════════════════╗\n"
       (string-join (nreverse rows) "\n")
       "\n"
       (format "Pos (%d,%d) Enemy (%d,%d) t=%.1fs\n"
               neuro-chase--px neuro-chase--py
               neuro-chase--ex neuro-chase--ey elapsed)
       "WASD to move, reach E to escape\n"))))

(defun neuro-chase-move-player (dir)
  (let ((nx neuro-chase--px) (ny neuro-chase--py))
    (pcase dir
      ("w" (setq ny (1- ny))) ("s" (setq ny (1+ ny)))
      ("a" (setq nx (1- nx))) ("d" (setq nx (1+ nx))))
    (when (neuro-chase--passable nx ny)
      (setq neuro-chase--px nx neuro-chase--py ny))))

(defun neuro-chase--astar (sx sy tx ty)
  (let* ((open-set  (list (list 0 sx sy nil)))
         (g-cost    (make-hash-table :test #'equal))
         (came-from (make-hash-table :test #'equal))
         (dirs      '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
         (found     nil))
    (puthash (cons sx sy) 0 g-cost)
    (while (and open-set (not found))
      (setq open-set
            (sort open-set (lambda (a b) (< (car a) (car b)))))
      (let* ((node  (pop open-set))
             (cx    (nth 1 node))
             (cy    (nth 2 node)))
        (if (and (= cx tx) (= cy ty))
            (setq found (cons cx cy))
          (dolist (d dirs)
            (let* ((nx  (+ cx (car d)))
                   (ny  (+ cy (cdr d)))
                   (key (cons nx ny))
                   (ng  (1+ (gethash (cons cx cy) g-cost 0))))
              (when (and (neuro-chase--passable nx ny)
                         (< ng (gethash key g-cost most-positive-fixnum)))
                (puthash key ng g-cost)
                (puthash key (cons cx cy) came-from)
                (let ((f (+ ng (abs (- nx tx)) (abs (- ny ty)))))
                  (push (list f nx ny) open-set))))))))
    (when found
      (if (and (= sx tx) (= sy ty))
          nil
        (let ((path nil) (cur (cons tx ty)))
          (while (not (and (= (car cur) sx) (= (cdr cur) sy)))
            (push cur path)
            (setq cur (gethash cur came-from)))
          path)))))

(defun neuro-chase-move-enemy ()
  (let ((path (neuro-chase--astar
               neuro-chase--ex neuro-chase--ey
               neuro-chase--px neuro-chase--py)))
    (when path
      (setq neuro-chase--ex (caar path)
            neuro-chase--ey (cdar path)))))

(defun neuro-chase-caught-p ()
  (and (= neuro-chase--px neuro-chase--ex)
       (= neuro-chase--py neuro-chase--ey)))

(defun neuro-chase-escaped-p ()
  (= (neuro-chase--get neuro-chase--px neuro-chase--py) ?E))

(defun neuro-chase-elapsed-s ()
  (/ (float (- (neuro-now-ms) neuro-chase--start-ms)) 1000.0))

(defun neuro-chase-speed-ms ()
  (let ((elapsed (neuro-chase-elapsed-s)))
    (cond ((> elapsed 40) 30)
          ((> elapsed 30) 50)
          ((> elapsed 20) 100)
          ((> elapsed 10) 150)
          (t 200))))

(provide 'k0)
