;;locations
(defparameter *nodes* 
'((living-room (you are in the living-room. a wizard is snoring loudly on the couch.))
  (garden (you are in a beautiful garden. there is a well in front of you))
  (attic (you are in the attic. (you are in the attic. there is a giant welding torch in the corner.)))))

;;paths
(defparameter *edges*
  '((living-room (garden west door)
                 (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

;object location
(defparameter *object-locations* '((whiskey living-room)
				  (bucket living-room)
				  (chain garden)
				  (frog garden)))
;;objects
(defparameter *objects* '(whiskey bucket chain frog))

;;store current location
(defparameter *location* 'living-room)

;;describle the location
(defun describle-location (location nodes)
  (cadr (assoc location nodes)))

;;describle the path
(defun describle-path (edge)
  `(there is a ,(cadr edge) going ,(cadr edge) from here.))

(defun describle-paths (location edges)
  (apply #'append (mapcar #'describle-path (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describle-objects (loc objs obj-loc)
  (labels ((describle-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describle-obj (objects-at loc objs obj-loc)))))

(defun look ()
  (append (describle-location *location* *nodes*)
	  (describle-paths *location* *edges*)
	  (describle-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next 
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

(defun pickup (object)
  (cond 
         ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	 (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

(defmacro walkto (direction)
  `(walk ',direction))
