(defparameter standard-goal '(0 1 2 3 4 5 6 7 8)) 
(defvar *nodes-expanded* 0)

(defun find-blank (board-state)
  (find-tile-at board-state 0 0))

(defun find-tile (board-state tile)
  (find-tile-at board-state tile 0))

(defun find-tile-at (board-state tile position)
  (cond ((> position 8) 
         nil)
        ((equal (elt board-state position) tile)
         position)
        (t
         (find-tile-at board-state tile (1+ position)))))

(defun move-direction (board-state direction &optional ignore-invalid-move)
  ; Takes current state and returns that state modified by the move specified by direction
  ; Board states are numbered 0 through 8 with 0 in the upper left, 8 in the lower right
  ; Direction corresponds to where to move the "blank" tile
  ; "left" moves the tile left of the blank to the right so the blank is now left of where it was
  ; If ignore-invalid-move is not NIL, if a move is invalid (e.g. moving blank left from position 4), 
  ; it will just return the same initial state. Otherwise it will return NIL.
  (let ((blank-position (find-blank board-state)))
    (cond ((equalp direction "left")
           (if (or (equal blank-position 0) (equal blank-position 3) (equal blank-position 6))
               ; invalid move
               (if ignore-invalid-move
                   board-state
                   nil)
             (swap-tile board-state blank-position (1- blank-position))))
          ((equalp direction "right")
           (if (or (equal blank-position 2) (equal blank-position 5) (equal blank-position 8))
               ; invalid move
               (if ignore-invalid-move
                   board-state
                   nil)
             (swap-tile board-state blank-position (1+ blank-position))))
          ((equalp direction "down")
           (if (or (equal blank-position 6) (equal blank-position 7) (equal blank-position 8))
               ; invalid move
               (if ignore-invalid-move
                   board-state
                   nil)
             (swap-tile board-state blank-position (+ blank-position 3))))
          ((equalp direction "up")
           (if (or (equal blank-position 0) (equal blank-position 1) (equal blank-position 2))
               ; invalid move
               (if ignore-invalid-move
                   board-state
                   nil)
             (swap-tile board-state blank-position (- blank-position 3)))))))

(defun swap-tile (board-state tile1 tile2)
  (let ((new-state (copy-seq board-state)))
    (rotatef (elt new-state tile1) (elt new-state tile2))
    new-state))

(defun random-move (board-state moves &optional (random-st *random-state*))
  (if (< moves 1)
      board-state
    (let ((next-state board-state) (randomMove (random 4 random-st)))
      (cond ((= randomMove 0)
             (setf next-state (move-direction board-state '"left" t)))
            ((= randomMove 1)
             (setf next-state (move-direction board-state '"right" t)))
            ((= randomMove 2)
             (setf next-state (move-direction board-state '"up" t)))
            ((= randomMove 3)
             (setf next-state (move-direction board-state '"down" t))))
      (random-move next-state (1- moves) random-st))))

(defun find-row (tile-position)
  (cond ((< tile-position 3)
         0)
        ((> tile-position 5)
         2)
        (t
         1)))

(defun find-column (tile-position)
  (mod tile-position 3))
      
(defun make-random-board ( )
  (progn
    (setf *random-state* (make-random-state t))
    (random-move '(0 1 2 3 4 5 6 7 8) 100)))

(defun make-random-simple-board ( )
  (progn
    (setf *random-state* (make-random-state t))
    (random-move '(0 1 2 3 4 5 6 7 8) 20)))

(defun random-case ( )
  (list (make-random-board) (make-random-board) 
        (make-random-board) (make-random-board) 
        (make-random-board)))

(defun random-case-simple ( )
  (list (make-random-simple-board) (make-random-simple-board)
        (make-random-simple-board) (make-random-simple-board)
        (make-random-simple-board)))

(defun tile-manhattan (board-state tile-position goal-state)
  ; Returns Manhattan distance from home for an individual tile at given position on board
  (let 
      ((tile-number (elt board-state tile-position))
       (row (find-row tile-position))
       (column (find-column tile-position)))
    (if (not (= tile-number 0))
        (let 
            ((row-goal (find-row (find-tile goal-state tile-number)))
             (column-goal (find-column (find-tile goal-state tile-number))))
          (+ (abs (- row row-goal)) (abs (- column column-goal))))
      0)))
  
(defun manhattan (board-state goal-state)
  (let ((sum 0))
    (dotimes (x 9 sum)
      (setf sum (+ sum (tile-manhattan board-state x goal-state))))))            
                    
(defun same-sign (x y)
  (cond ((and (> x 0) (> y 0))
             t)
        ((and (< x 0) (< y 0))
             t)
        ((and (= x 0) (= y 0))
             t)
        (t nil)))

(defun get-row (rownum board-state)
  (list
   (elt board-state (+ (* 3 rownum) 0))
   (elt board-state (+ (* 3 rownum) 1))
   (elt board-state (+ (* 3 rownum) 2))))

(defun get-column (colnum board-state)
  (list
   (elt board-state (+ colnum 0))
   (elt board-state (+ colnum 3))
   (elt board-state (+ colnum 6))))

(defun extracredit (board-state goal-state)
  (let ((manhattan-dist (manhattan board-state goal-state)))
    (let ((row-conflicts 0) (column-conflicts 0))
      (dotimes (x 9 row-conflicts)
        (setf row-conflicts (+ row-conflicts (tile-extracredit-rows board-state x goal-state))))
      (dotimes (x 9 column-conflicts)
        (setf column-conflicts (+ column-conflicts (tile-extracredit-columns board-state x goal-state))))
      (+ (+ row-conflicts column-conflicts) manhattan-dist))))

(defun manhattan (board-state goal-state)
  (let ((sum 0))
    (dotimes (x 9 sum)
      (setf sum (+ sum (tile-manhattan board-state x goal-state))))))

(defun tile-misplaced (board-state tile-position goal-state)
  (let
      ((tile-number (elt board-state tile-position)))
    (if (/= tile-number 0)
        (if (= tile-number tile-position) 0 1)
      0)))

(defun misplaced (board-state goal-state)
  (let ((sum 0))
    (dotimes (x 9 sum)
      (setf sum (+ sum (tile-misplaced board-state x goal-state))))))

(defun same-state (state1 state2)
  (if (and (equal (length state1) (length state2)) (equal (length state1) 1))
      (equal (car state1) (car state2))
    (if (equal (car state1) (car state2))
        (same-state (cdr state1) (cdr state2)))))

(defun is-goal (board-state)
  (same-state board-state standard-goal))

(defun find-possible-actions (from-position)
  (let ((row (find-row (find-blank from-position)))
        (column (find-column (find-blank from-position))))
    (let ((actions
           (list
            (if (or (= row 0) (= row 1)) '"down")
            (if (or (= row 2) (= row 1)) '"up")
            (if (or (= column 0) (= column 1)) '"right")
            (if (or (= column 2) (= column 1)) '"left"))))
      (remove-if #'null actions))))

(defun 8-puzzle (start-state heuristic)
  (if (same-state start-state standard-goal)
      '(() 0 0)
    (let ((start-node (make-node :state start-state
                                 :parent nil
                                 :action nil
                                 :path-cost 0
                                 :heuristic-estimation (funcall heuristic start-state standard-goal)
                                 :astar-value (funcall heuristic start-state standard-goal)
                                 :depth 0)))
      (setf *nodes-expanded* 0)
      (let ((start-successors (expand #'successor start-node heuristic)))
        (let ((start-fringe (make-q)))
          (q-insert start-fringe start-successors)
          (let ((moves-list (graph-search start-fringe nil #'successor #'is-goal #'same-state heuristic)))
            (list moves-list (length moves-list) *nodes-expanded*)))))))

(defun successor (state heuristic)
  ; Should return a list of action-state-cost-heuristic lists (for each successor)
  (let ((possible-moves (find-possible-actions state)))
    (let ((initial-state-list 
           (make-list (length possible-moves) :initial-element state))
          (goal-state-list
           (make-list (length possible-moves) :initial-element standard-goal)))
      (let ((result-states 
             (mapcar #'move-direction initial-state-list possible-moves)))
        (let ((heuristic-ests (mapcar heuristic result-states goal-state-list)))
          (mapcar 
           #'list 
           possible-moves 
           result-states 
           (make-list (length possible-moves) :initial-element '1)
           heuristic-ests))))))

(defun tile-extracredit-rows (board-state tile-position goal-state)
  ; For each tile, check if it's in its goal row, and if so if there is another tile that belongs in 
  ; that row in the same row. If so, return 1, else 0. (Thus for each pair of these tiles, 2 total 
  ; moves will be added to Manhattan distance.)
  (let 
      ((tile-number (elt board-state tile-position))
       (row (find-row tile-position))
       (column (find-column tile-position)))
    (if (not (= tile-number 0))
        (let 
            ((row-goal (find-row (find-tile goal-state tile-number)))
             (column-goal (find-column (find-tile goal-state tile-number))))
          (if (= row row-goal)
              (let 
                  ((row-members (get-row row board-state))
                   (goalrow-members (get-row row goal-state)))
                (let
                    ((other-row-members (remove tile-number row-members)))
                  (let ((num-also-in-goal 0))
                    (if (member (car other-row-members) goalrow-members)
                        (setf num-also-in-goal (1+ num-also-in-goal)))
                    (if (member (cadr other-row-members) goalrow-members)
                        (setf num-also-in-goal (1+ num-also-in-goal)))
                    (let ((tilepos-also-in-goal
                           (if (= num-also-in-goal 1)
                               (if (member (car other-row-members) goalrow-members)
                                   (find-tile board-state (car other-row-members))
                                 (find-tile board-state (cadr other-row-members)))
                             (list (find-tile board-state (car other-row-members))
                                     (find-tile board-state (cadr other-row-members))))))
                      (let ((tilenum-also-in-goal
                             (if (= num-also-in-goal 1)
                                 (elt board-state tilepos-also-in-goal)
                               (list (elt board-state (car tilepos-also-in-goal))
                                     (elt board-state (cadr tilepos-also-in-goal))))))
                        (if (= num-also-in-goal 1)
                            (let
                                ((row-other (find-row tilepos-also-in-goal))
                                 (column-other (find-column tilepos-also-in-goal))
                                 (row-goal-other (find-row (find-tile goal-state tilenum-also-in-goal)))
                                 (column-goal-other (find-column (find-tile goal-state tilenum-also-in-goal))))
                              (if (not (same-sign (- column column-other) (- column-goal column-goal-other)))
                                  1
                                0))
                          (let
                              ((row-other-1 (find-row (car tilepos-also-in-goal)))
                               (row-other-2 (find-row (cadr tilepos-also-in-goal)))
                               (column-other-1 (find-column (car tilepos-also-in-goal)))
                               (column-other-2 (find-column (cadr tilepos-also-in-goal)))
                               (row-goal-other-1 (find-row (find-tile goal-state (car tilenum-also-in-goal))))
                               (row-goal-other-2 (find-row (find-tile goal-state (cadr tilenum-also-in-goal))))
                               (column-goal-other-1 (find-column (find-tile goal-state (car tilenum-also-in-goal))))
                               (column-goal-other-2 (find-column (find-tile goal-state (cadr tilenum-also-in-goal)))))
                            (if (not (same-sign (- column column-other-1) (- column-goal column-goal-other-1)))
                                (if (not (same-sign (- column column-other-2) (- column-goal column-goal-other-2)))
                                    2
                                  1)
                              (if (not (same-sign (- column column-other-2) (- column-goal column-goal-other-2)))
                                  1
                                0)))))))))
            0))
      0)))

(defun tile-extracredit-columns (board-state tile-position goal-state)
  (let 
      ((tile-number (elt board-state tile-position))
       (row (find-row tile-position))
       (column (find-column tile-position)))
    (if (not (= tile-number 0))
        (let 
            ((row-goal (find-row (find-tile goal-state tile-number)))
             (column-goal (find-column (find-tile goal-state tile-number))))
          (if (= column column-goal)
              (let 
                  ((column-members (get-column column board-state))
                   (goalcolumn-members (get-column column goal-state)))
                (let
                    ((other-column-members (remove tile-number column-members)))
                  (let ((num-also-in-goal 0))
                    (if (member (car other-column-members) goalcolumn-members)
                        (setf num-also-in-goal (1+ num-also-in-goal)))
                    (if (member (cadr other-column-members) goalcolumn-members)
                        (setf num-also-in-goal (1+ num-also-in-goal)))
                    (let ((tilepos-also-in-goal
                           (if (= num-also-in-goal 1)
                               (if (member (car other-column-members) goalcolumn-members)
                                   (find-tile board-state (car other-column-members))
                                 (find-tile board-state (cadr other-column-members)))
                             (list (find-tile board-state (car other-column-members))
                                     (find-tile board-state (cadr other-column-members))))))
                      (let ((tilenum-also-in-goal
                             (if (= num-also-in-goal 1)
                                 (elt board-state tilepos-also-in-goal)
                               (list (elt board-state (car tilepos-also-in-goal))
                                     (elt board-state (cadr tilepos-also-in-goal))))))
                        (if (= num-also-in-goal 1)
                            (let
                                ((row-other (find-row tilepos-also-in-goal))
                                 (column-other (find-column tilepos-also-in-goal))
                                 (row-goal-other (find-row (find-tile goal-state tilenum-also-in-goal)))
                                 (column-goal-other (find-column (find-tile goal-state tilenum-also-in-goal))))
                              (if (not (same-sign (- row row-other) (- row-goal row-goal-other)))
                                  1
                                0))
                          (let
                              ((row-other-1 (find-row (car tilepos-also-in-goal)))
                               (row-other-2 (find-row (cadr tilepos-also-in-goal)))
                               (column-other-1 (find-column (car tilepos-also-in-goal)))
                               (column-other-2 (find-column (cadr tilepos-also-in-goal)))
                               (row-goal-other-1 (find-row (find-tile goal-state (car tilenum-also-in-goal))))
                               (row-goal-other-2 (find-row (find-tile goal-state (cadr tilenum-also-in-goal))))
                               (column-goal-other-1 (find-column (find-tile goal-state (car tilenum-also-in-goal))))
                               (column-goal-other-2 (find-column (find-tile goal-state (cadr tilenum-also-in-goal)))))
                            (if (not (same-sign (- row row-other-1) (- row-goal row-goal-other-1)))
                                (if (not (same-sign (- row row-other-2) (- row-goal row-goal-other-2)))
                                    2
                                  1)
                              (if (not (same-sign (- row row-other-2) (- row row-goal-other-2)))
                                  1
                                0)))))))))
            0))
      0)))

                

;;; The following code is adapted from code provided by Prof. Alexander J. Pasik 
;;; from his lecture notes 4 and 6
(defstruct node
  (state nil)
  (parent nil)
  (action nil)
  (path-cost 0)
  (heuristic-estimation 0)
  (astar-value 0)
  (depth 0))

(defstruct q
  (enqueue #'enqueue-priority)
  (key #'node-astar-value)
  (last nil)
  (elements nil))

(defun q-emptyp (q)
  (= (length (q-elements q)) 0))

(defun q-front (q)
  (elt (q-elements q) 0))

(defun q-remove (q)
  (heap-pop (q-elements q) (q-key q)))

(defun q-insert (q items)
  (funcall (q-enqueue q) q items)
  q)

(defun enqueue-priority (q items)
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  (mapc (lambda (item)
          (heap-insert (q-elements q) item (q-key q)))
        items)
  items)

(defun graph-search (fringe closed successor goalp samep heuristic)
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (cond ((funcall goalp (node-state node))
             (action-sequence node))
            ((member (node-state node) closed :test samep :key #'node-state)
             (graph-search fringe closed successor goalp samep heuristic))
            (t (let ((successors (expand successor node heuristic)))
                 (graph-search (q-insert fringe successors)
                               (cons node closed)
                               successor goalp samep heuristic)))
            ))
    ))
; Returns the list from action-sequence

(defun expand (successor-func node heuristic)
  (let ((successor-descriptions (funcall successor-func (node-state node) heuristic)))
    (setf *nodes-expanded* (1+ *nodes-expanded*))
    (mapcar (lambda (successor-descriptions)
              (let ((action (car successor-descriptions))
                    (state (cadr successor-descriptions))
                    (cost (caddr successor-descriptions))
                    (heuristic-estimate (cadddr successor-descriptions)))
                (make-node :state state
                           :parent node
                           :action action
                           :path-cost (+ (node-path-cost node) 1)
                           :heuristic-estimation heuristic-estimate
                           :astar-value (+ (node-path-cost node) 1 heuristic-estimate)
                           :depth (1+ (node-depth node)))
                ))
            successor-descriptions)
    ))
; Returns the result of mapcar, which is a list formed by the application of the lambda
; function to every state description in successor-descriptions
; The lambda function makes a node out of those descriptions
; So expand returns a list of nodes that we now know the cost, astar value, etc. of

(defun action-sequence (node &optional (actions nil))
  (if (node-parent node)
      (action-sequence (node-parent node)
                       (cons (node-action node) actions))
    actions
    ))

;; Heap stuff below
(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  (unless (heap-leafp heap i)
    (let ((left-index (heap-left i))
          (right-index (heap-right i)))
      (let ((smaller-index
             (if (and (< right-index (length heap))
                      (< (heap-val heap right-index key)
                         (heap-val heap left-index key)))
                 right-index
               left-index)))
        (when (> (heap-val heap i key)
                 (heap-val heap smaller-index key))
          (rotatef (elt heap i)
                   (elt heap smaller-index))
          (heapify heap smaller-index key))))
    ))

(defun heap-pop (heap key)
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  (vector-push-extend nil heap)
  (setf (elt heap (heap-find-pos heap (1- (length heap))
                                 (funcall key item) key)) 
        item)
)

(defun heap-find-pos (heap i val key)
  (cond ((or (zerop i)
             (< (heap-val heap (heap-parent i) key) val))
         i)
        (t (setf (elt heap i) (elt heap (heap-parent i)))
           (heap-find-pos heap (heap-parent i) val key))
        ))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))