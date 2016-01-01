(in-package :cl-user)

(defpackage :nbody
  (:use :common-lisp
        :6e
        :prompt)
  (:export :main
           :system
           :create-state))

(in-package :nbody)

(defparameter *prompts* '())

(defstruct body
  position
  velocity
  mass)

(defconstant +solar-mass+ (* 4 pi pi))

(defun create-state (elements mass)
  (destructuring-bind (a e i o l m0)
      elements
    (destructuring-bind (x y z dx dy dz)
        (6e:xyz :u (+ mass +solar-mass+)
             :alpha a :e e :i i :node o :periapsis l :m0 m0)
      (make-body :position (list x y z) :velocity (list dx dy dz) :mass mass))))

(defun combinations (l r)
  (if (< (length l) 2)
         r
         (combinations (cdr l) (append r (loop for i in (cdr l)
                                         collect (list (car l) i))))))

(defun advance (dt system names pairs)
  (loop for p in pairs
     for p1 = (gethash (first p) system)
     for p2 = (gethash (second p) system)
     for pos1 = (body-position p1)
     for pos2 = (body-position p2)
     for x1 = (nth 0 pos1)
     for y1 = (nth 1 pos1)
     for z1 = (nth 2 pos1)
     for x2 = (nth 0 pos2)
     for y2 = (nth 1 pos2)
     for z2 = (nth 2 pos2)
     for m1 = (body-mass p1)
     for m2 = (body-mass p2)
     for dx = (- x1 x2)
     for dy = (- y1 y2)
     for dz = (- z1 z2)
     for mag = (* dt (expt (+ (* dx dx) (* dy dy) (* dz dz)) -1.5))
     for b1m = (* m1 mag)
     for b2m = (* m2 mag)
     for vel1 = (body-velocity p1)
     for vel2 = (body-velocity p2)
     for v1x = (nth 0 vel1)
     for v1y = (nth 1 vel1)
     for v1z = (nth 2 vel1)
     for v2x = (nth 0 vel2)
     for v2y = (nth 1 vel2)
     for v2z = (nth 2 vel2)
     do (progn
          (setf (nth 0 vel1) (- v1x (* dx b2m)))
          (setf (nth 1 vel1) (- v1y (* dy b2m)))
          (setf (nth 2 vel1) (- v1z (* dz b2m)))
          (setf (nth 0 vel2) (+ v2x (* dx b1m)))
          (setf (nth 1 vel2) (+ v2y (* dy b1m)))
          (setf (nth 2 vel2) (+ v2z (* dz b1m)))))
  (loop for body in names
       for b = (gethash body system)
       for pos = (body-position b)
       for vel = (body-velocity b)
       for rx = (nth 0 pos)
       for ry = (nth 1 pos)
       for rz = (nth 2 pos)
       do (progn
            (setf (nth 0 pos) (+ rx (* dt (nth 0 vel))))
            (setf (nth 1 pos) (+ ry (* dt (nth 1 vel))))
            (setf (nth 2 pos) (+ rz (* dt (nth 2 vel)))))))
            
(defun report-energy (system names pairs)
  (let ((e 0.0))
    (loop for p in pairs
       for p1 = (gethash (first p) system)
       for p2 = (gethash (second p) system)
       for pos1 = (body-position p1)
       for pos2 = (body-position p2)
       for x1 = (nth 0 pos1)
       for y1 = (nth 1 pos1)
       for z1 = (nth 2 pos1)
       for x2 = (nth 0 pos2)
       for y2 = (nth 1 pos2)
       for z2 = (nth 2 pos2)
       for m1 = (body-mass p1)
       for m2 = (body-mass p2)
       for dx = (- x1 x2)
       for dy = (- y1 y2)
       for dz = (- z1 z2)
       do (setf e (- e (/ (* m1 m2) (expt (+ (* dx dx) (* dy dy) (* dz dz)) .5)))))
    (loop for body in names
       for b = (gethash body system)
       for vel = (body-velocity b)
       for vx = (nth 0 vel)
       for vy = (nth 1 vel)
       for vz = (nth 2 vel)
       for m = (body-mass b)
         do (setf e (+ e (/ (* m (+ (* vx vx) (* vy vy) (* vz vz))) 2))))
    (format t "~a~%" e)))

(defun offset-momentum (ref system names)
  (let* ((px 0)
         (py 0)
         (pz 0)
         (b (gethash ref system))
         (bm (body-mass b))
         (bv (body-velocity b)))
    (loop for body in names
       for b = (gethash body system)
       for vel = (body-velocity b)
       for vx = (nth 0 vel)
       for vy = (nth 1 vel)
       for vz = (nth 2 vel)
       for m = (body-mass b)
       do (progn
            (setf px (- px (* vx m)))
            (setf py (- py (* vy m)))
            (setf pz (- pz (* vz m)))))
    (setf (nth 0 bv) (/ px bm))
    (setf (nth 1 bv) (/ py bm))
    (setf (nth 2 bv) (/ pz bm))))

(defvar collection-period 1)

(let ((counter 0))
  (defun snapshot (stream time system names)
    ;; iterate over all bodies, writing the pos to a file w/ time
    (if (>= counter collection-period)
        (progn
          (setf counter 0)
          (loop 
             for name in names
             for body = (gethash name system)
             for pos  = (body-position body)
             for vel  = (body-velocity body)
             do (format stream "~a,~f,~f,~f,~f,~f,~f,~f~%" name (nth 0 pos) (nth 1 pos) (nth 2 pos) (nth 0 vel) (nth 1 vel) (nth 2 vel) time)))
        (progn
          (setf counter (+ 1 counter))))))

(defvar system '())

(defun simulate (stepsize years infile outfile)
  (setq system (make-hash-table))
  (load infile)
  (let* ((solar-system (loop for x being the hash-keys of system collect x))
         (pairs (combinations solar-system '())))
    (offset-momentum 'sun system solar-system)
    (with-open-file (stream outfile :direction :output :if-exists :supersede)
      (format stream "name,x,y,z,vx,vy,vz,time~%") 
      (let ((time 0))
        (loop repeat 1
           for nop = (report-energy system solar-system pairs)
           do (loop repeat (/ years stepsize)
                 do (progn
                      (snapshot stream time system solar-system) 
                      (setf time (+ time stepsize))
                      (advance stepsize system solar-system pairs))))))
    (report-energy system solar-system pairs)))

(defprompt ("q" "quit" *prompts*)
  (end-prompt))

(defvar infile  "input.txt")
(defvar outfile "output.txt")

(defprompt ("input" "read input from file" *prompts*)
  (setf infile (poparg)))

(defprompt ("output" "file to store output data in" *prompts*)
  (setf outfile (poparg)))

(defvar stepsize 0.01d0)
(defvar years 1000)
(defun dump ()
  (format t "years:             ~a~%" years)
  (format t "step:              ~a~%" stepsize)
  (format t "collection period: ~a~%" collection-period)
  (format t "input file:        ~a~%" infile)
  (format t "output file:       ~a~%" outfile))

(defprompt ("dump" "show variables" *prompts*)
  (dump))

(defprompt ("run" "start simulation" *prompts*)
    (dump)
  (simulate stepsize years infile outfile))

(defprompt ("years" "number of years to simulate" *prompts*)
  (setf years (parse-integer (poparg) :junk-allowed t)))

(defprompt ("step" "stepsize (in years) to use in simulation" *prompts*)
  (setf stepsize (read-from-string (poparg))))

(defprompt ("cp" "collection period" *prompts*)
    (setf collection-period (parse-integer (poparg) :junk-allowed t)))

(defun shell-command-rc (cmd)
  (multiple-value-bind (a b c) 
      (uiop:run-program (list "/bin/sh" "-c" cmd) :output nil :ignore-error-status t)
    (declare (ignore a b)) c))

(defprompt ("plot" "display a plot of the output data" *prompts*)
  (shell-command-rc (concatenate 'string "Rscript plot.r " outfile)))
  
(defun main ()
  (prompt "> " *prompts* :helpname "?"))

