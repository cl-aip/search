;;;; The Debugging Output Facility:

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG-ID ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug-id (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug-id (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

(defun undebug-all ()
  (undebug-id))

;;;
;;;
;;;

(defparameter *cities*
  '((Atlanta      ATL 33.636667  -84.428056) (Los-Angeles   LAX 33.9425   -118.408056)
     (Boston       BOS 42.363056  -71.006389) (Memphis       MEM 35.0425    -89.976667)  
     (Chicago      ORD 41.978611  -87.904722) (New-York      LGA 40.77725   -73.872611) 
     (Denver       DEN 39.861656 -104.673178) (Oklahoma-City OKC 35.393056  -97.600833)
     (Eugene       EUG 44.123056 -123.218611) (Pittsburgh    PIT 40.491389  -80.232778) 
     (Flagstaff    FLG 35.140278 -111.669167) (Quebec        YQB 46.791111  -71.393333)
     (Grand-Jct    GJT 39.1225   -108.526667) (Reno          RNO 39.499167 -119.768056)
     (Houston      IAH 29.984444  -95.341389) (San-Francisco SFO 37.618889 -122.375)
     (Indianapolis IND 39.717222  -86.294444) (Tampa         TPA 27.975556  -82.533333)
     (Jacksonville JAX 30.494167  -81.687778) (Victoria      YYJ 48.647222 -123.425833)
     (Kansas-City  MCI 39.2975    -94.713889) (Wilmington    ILM 34.270556  -77.9025)))

(defstruct (city (:type list)) name airport lat long)

(defun city (name) 
  "Find the city with this name."
  (assoc name *cities*))

(defconstant earth-diameter 12742.0
  "Diameter of planet earth in kilometers.")

(defun air-distance (city1 city2)
  "The great circle distance between two cities."
  (let ((d (distance (xyz-coords city1) (xyz-coords city2))))
    ;; d is the straight-line chord between the two cities,
    ;; The length of the subtending arc is given by:
    (* earth-diameter (asin (/ d 2)))))

(defun xyz-coords (city)
  "Returns the x,y,z coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)."
  (let ((psi (deg->radians (city-lat city)))
        (phi (deg->radians (city-long city))))
    (list (* (cos psi) (cos phi))
          (* (cos psi) (sin phi))
          (sin psi))))

(defun distance (point1 point2)
  "The Euclidean distance between two points.
  The points are coordinates in n-dimensional space."
  (sqrt (reduce #'+ (mapcar #'(lambda (a b) (expt (- a b) 2))
                            point1 point2))))

(defun deg->radians (deg)
  "Convert degrees to radians."
  (* deg pi 1/180))

(defun reachable-p (city1 city2)
  (and (not (eq city1 city2))
       (< (air-distance city1 city2) 1000.0)))

(defun neighbors (city)
  "Find all cities within 1000 kilometers."
  (remove-if-not #'(lambda (c) (reachable-p c city))
                 *cities*))

(defconstant fail nil)
(defun isa (value &key (key #'identity) (test #'eql))
  "Return a predicate that tests for a given value."
  #'(lambda (x) (funcall test value (funcall key x))))

;;;
;;; Depth First Search
;;;

(defun trip1 (start dest)
  (depth-first-search start (isa dest) #'neighbors))

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (graph-search (list start) goal-p successors #'append))

(defun graph-search (states goal-p successors combiner &optional (state= #'eql) trail)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.  
  Don't try the same state twice."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((find-if goal-p states)
         (reverse (cons (find-if goal-p states) trail)))
        (t (let ((new-states
                  (funcall combiner
                           (new-states (first states) states successors state= trail)
                           (rest states))))
             (graph-search (remove-if-not #'(lambda (c) (reachable-p c (first states)))
                                          new-states)
                           goal-p successors combiner state=
                           (cons (first states) trail))))))

(defun new-states (first states successors state= trail)
  "Generate successor states that have not been seen before."
  (remove-if
   #'(lambda (state)
       (or (member state states :test state=)  ; remove self
           (member state trail :test state=))) ; avoid revisit
   (funcall successors first)))

;;;
;;; Breadth First Search
;;;

(defun prepend (x y)
  "Prepend y to start of x"
  (append y x))

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (graph-search (list start) goal-p successors #'prepend))

(defun trip2 (start dest)
  (breadth-first-search start (isa dest) #'neighbors))

;;;
;;; Best First Search
;;;

(defun distance-to-goal (goal)
  #'(lambda (c) (air-distance c goal)))

(defun sorter (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (graph-search (list start) goal-p successors (sorter cost-fn)))

(defun trip3 (start dest)
  (best-first-search start (isa dest) #'neighbors (distance-to-goal dest)))

;;;
;;;
;;;

(defstruct (journey (:print-function print-journey))
  state (previous nil) (cost-so-far 0) (total-cost 0))

(defun print-journey (journey &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Journey to ~A cost ~,1f>"
    (journey-state journey) (journey-total-cost journey)))

;;;(defun print-journey (journey &optional (stream t) depth)
;;;  (declare (ignore depth))
;;;  (format stream "#<Journey to ~A with cost ~,1f from ~W>"
;;;    (journey-state journey) (journey-total-cost journey) (journey-path journey)))

(defun journey-path (journey)
  (if (null journey) '()
    (cons (city-name (journey-state journey))
          (journey-path (journey-previous journey)))))

(defun journey-saver (successors cost-fn cost-left-fn)
  #'(lambda (old-journey)
      (let ((old-state (journey-state old-journey)))
        (mapcar
          #'(lambda (new-state)
              (let ((old-cost
                      (+ (journey-cost-so-far old-journey)
                         (funcall cost-fn old-state new-state))))
                (make-journey
                  :state new-state
                  :previous old-journey
                  :cost-so-far old-cost
                  :total-cost (+ old-cost (funcall cost-left-fn new-state)))))
          (funcall successors old-state)))))
#|
(defun best-first-search* (start goal-p successors cost-fn)
  (graph-search* (list (make-journey :state start))
               goal-p
               successors
               (sorter cost-fn)
               ))

(defun graph-search* (states goal-p successors combiner &optional (state= #'eql) old-states)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  (dbg :search "~&;; Search: ~a" states)
  (cond ((null states) fail)
        ((find-if goal-p states)
         (reverse (journey-path (find-if goal-p states))))
        (t (let ((new-states
                  (funcall combiner
                           (new-states (first states) states successors state= old-states)
                           (rest states))))
             (graph-search* (remove-if-not
                            #'(lambda (ns)
                                (reachable-p (journey-state ns)
                                             (journey-state (first states))))
                            new-states)
                          goal-p successors combiner state=
                          (cons (first states) old-states))))))

(defun trip4 (start dest)
  (best-first-search* start
                      (isa dest :key #'journey-state)
                      (journey-saver #'neighbors #'air-distance
                                     (distance-to-goal dest))
                      #'journey-total-cost))
|#
(defun a*-search (journeys goal-p successors cost-fn cost-left-fn
                           &optional (state= #'eql) old-journeys)
  "Find a journey whose state satisfies goal-p.  Start with journeys,
  and expand successors, exploring least cost first.
  When there are duplicate states, keep the one with the
  lower cost and discard the other."
  (dbg :search ";; Search: ~a" journeys)
  (cond
   ((null journeys) fail)
   ((find-if goal-p journeys)
    (reverse (journey-states (find-if goal-p journeys)))
    )
   (t (let* ((journey (pop journeys))
             (state (journey-state journey)))
        ;; Update states and old-journeys to reflect
        ;; the new successors of STATE:
        (setf old-journeys (insert-journey journey old-journeys))
        (dolist (state2 (funcall successors state))
          (let* ((cost (+ (journey-cost-so-far journey)
                          (funcall cost-fn state2 state)))
                 (cost2 (funcall cost-left-fn state2))
                 (journey2 (make-journey
                            :state state2
                            :previous journey
                            :cost-so-far cost
                            :total-cost (+ cost cost2)))
                 (old nil))
            ;; Place the new journey, journey2, in the right list:
            (cond
             ((setf old (find-journey journey2 journeys state=))
              (when (better-journey journey2 old)
                (setf journeys (insert-journey
                                journey2 (delete old journeys)))))
             ((setf old (find-journey journey2 old-journeys state=))
              (when (better-journey journey2 old)
                (setf journeys (insert-journey journey2 journeys))
                (setf old-journeys (delete old old-journeys))))
             (t (setf journeys (insert-journey journey2 journeys))))))
        ;; Finally, call A* again with the updated journey lists:
        (a*-search journeys goal-p successors cost-fn cost-left-fn
                   state= old-journeys)))))

(defun find-journey (journey journeys state=)
  "Find the journey with this journey's state among a list of journeys."
  (find (journey-state journey) journeys :key #'journey-state :test state=))

(defun better-journey (journey1 journey2)
  "Is journey1 cheaper than journey2?"
  (< (journey-total-cost journey1) (journey-total-cost journey2)))

(defun insert-journey (journey journeys)
  "Put journey into the right position, sorted by total cost."
  ;; MERGE is a built-in function
  (merge 'list (list journey) journeys #'< :key #'journey-total-cost))

(defun journey-states (journey)
  "Collect the states along this journey."
  (if (null journey)
      nil
      (cons (journey-state journey)
            (journey-states (journey-previous journey)))))

(defun trip5 (start dest)
  (a*-search (list (make-journey :state start))
             (isa dest :key #'journey-state)
             #'neighbors
             #'air-distance
             (distance-to-goal dest)
             ))