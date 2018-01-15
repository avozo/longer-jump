;; longer-jump.el --- Go back to last relevant cursor position
;;
;; Author: Zelly Snyder
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.	 If not, see <http://www.gnu.org/licenses/>.

(require 'cl)

;; constants

(defconst +max-history-items+ 50 ;; magic number
  "Upper limit (exclusive) of history items--feel free to change this")
;; TODO defcustom-ize

(defconst +no-closer-than+ 50
  "Controls maximum proximity of any consecutive edit positions. Earliest edit position (one you're most likely to remember) is used when there are candidates to filter out.")

(defconst +only-consider-last-n+ 200
  ;; use most-positive-fixnum for all
  "Maximum number of recent (filtered) history items to use")

;; utils

(defun enumerate (seq)
  "like Python"
  (cl-loop for x across seq and idx from 0
		   vconcat (vector (cons x idx))))

(defun cyclize (n seq)
  "Negative or too large index -> correct index. Makes an 'index' valid over (-inf, +inf). This corrects N without actually calling ELT."
  (cond ((< n 0) (+ (length seq) n))
		((>= n (length seq)) (mod n (length seq)))
		(t n)))

(cl-defun argm** (f seq &key comp)
  "argmin or argmax depending on comp"
  (when (> (length seq) 1)
	(cl-reduce #'(lambda (a b)
				   (if (funcall comp (funcall f a) (funcall f b)) a b))
			   seq)))

(cl-defun argmin (f seq &key (comp #'<))
  (argm** f seq :comp comp))

(cl-defun argmax (f seq &key (comp #'>))
  (argm** f seq :comp comp))

(defun mean (seq)
  (if (<= (length seq) 0) 0
	(/ (cl-reduce #'+ seq)
	   (length seq))))

(defun n-highest (seq n)
  "Get the latter portion of SEQ consisting of up to min(N, len(SEQ)) items"
  (subseq seq (if (> (length seq) n)
				  (- (length seq) n) 0))
  ;; for doing this by fractions of the length instead:
  ;; (let ((n (truncate (* n (length seq)))))
  ;; 	(subseq seq (- (length seq) n))))
  )

;; k-means

;; TODO this is slow--revisit later
;; (defun k-clusters (data k)
;;   "Generates K points from DATA (a vector of numbers) roughly representing the centroids of the respective K clusters"
;;   (let* ((initial-means #'(lambda () "Forgy partitioning method"
;; 							(vconcat
;; 							 (cl-loop for xs = nil then (cl-remove-duplicates
;; 														 (cons (random (length data)) xs))
;; 									  until (>= (length xs) k)
;; 									  finally return xs))))
;; 		 (distance #'(lambda (a b) "A and B are values in DATA"
;; 					   (abs (- a b))))
;; 		 (assign-to-clusters #'(lambda (means)
;; 								 "Creates a vector with cluster members corresponding to the means in the same order"
;; 								 (let ((clusters (make-vector (length means) [])))
;; 								   (cl-loop for val across data
;; 											for min-idx = 0 then (cl-loop for mean across means and this-mean-idx from 0
;; 																		  for ret = 0 then (cond ((< (funcall distance (elt means this-mean-idx) val)
;; 																									 (funcall distance (elt means ret) val))
;; 																								  this-mean-idx)
;; 																								 ((> (funcall distance (elt means this-mean-idx) val)
;; 																									 (funcall distance (elt means ret) val))
;; 																								  ret)
;; 																								 (t (if (zerop (random 2)) ;; flip a coin
;; 																										this-mean-idx ret)))
;; 																		  finally return ret)
;; 											do (setf (aref clusters min-idx)
;; 													 (vconcat (aref clusters min-idx) (vector val)))
;; 											finally return clusters))))
;; 		 (update-means #'(lambda (clusters)
;; 						   "Computes new means from the clusters"
;; 						   (map 'vector #'(lambda (cluster)
;; 											(if (<= (length cluster) 0) 0
;; 											  (/ (cl-reduce #'+ cluster) (length cluster))))
;; 								clusters)))
;; 		 (data (cl-remove-duplicates data)))
;; 	(let ((initial-means-value (cl-sort (funcall initial-means) #'<)))
;; 	  (do ((means initial-means-value (cl-sort (funcall update-means clusters) #'<))
;; 		   (clusters (funcall assign-to-clusters initial-means-value) (funcall assign-to-clusters means))
;; 		   (old-clusters '() (cons clusters old-clusters)))
;; 		  ((and (equal (car old-clusters) clusters)
;; 				(equal (cadr old-clusters) clusters))
;; 		   (let ((as-data-points ;; map each mean back to an original data point
;; 				  (cl-map 'vector
;; 						  #'(lambda (centroid)
;; 							  (argmin #'(lambda (data-point) (abs (- data-point centroid)))
;; 									  data))
;; 						  means)))
;; 			 ;; put DATA-POINTS back in its original order (which for our purposes is important since it represents time)
;; 			 (cl-loop for d across data
;; 					  when (cl-find d as-data-points) ;; works because DATA elements are all unique
;; 					  vconcat (vector d))))
;; 		(when (> (length old-clusters) 2)
;; 		  (setf (nthcdr 2 old-clusters) nil))))))


;; naive (but faster) clustering

(defun fast-make-clusters (data k)
  (when (> (length data) 1)
	(let* ((window-width-total (/ (length data) k))
		   (window-width-left (/ window-width-total 2))
		   (window-width-right (- window-width-total window-width-left)))
	  (cl-loop for nth-iter from 0
			   ;; increase stride by NTH-ITER^(2)
			   for idx from (1- (length data)) downto 0 by (+ window-width-total (truncate (expt nth-iter 2)))
			   and window = (seq-subseq data
										(max 0 (- idx window-width-left))
										(min (length data) (+ idx window-width-right 1)))
			   while (< idx (length data))
			   vconcat (vector
						(let ((avg (mean window)))
						  ;; centroid of this window
						  (argmin #'(lambda (data-point)
									  (abs (- data-point avg)))
								  window)))
			   into v
			   finally return (reverse v)))))

(defun filter-undo-list ()
  "Converting the native BUFFER-UNDO-LIST to something usable"
  (cl-loop for x in buffer-undo-list
		   when (and (consp x) (numberp (cdr x)))
		   ;; this only uses two types of elements in this list…
		   ;; insertion: use the end of the range (last place you remember being)
		   ;; deletion: abs(position)
		   vconcat (vector (abs (cdr x)))
		   ;; above actually preserves the counterintuitive ordering of BUFFER-UNDO-LIST
		   ;; let's reverse it
		   into ret
		   finally return (cl-remove-duplicates ;; and remove duplicates
						   (cl-remove-if ;; and remove positions point out of this buffer
							#'(lambda (x) (> x (buffer-size)))
							(reverse ret)))))

(defun filter-undo-list-into-list ()
  "Converting the native BUFFER-UNDO-LIST to something usable"
  (cl-loop for x in buffer-undo-list
		   ;; this only uses two types of elements in this list…
		   ;; insertion: use the end of the range (last place you remember being)
		   ;; deletion: abs(position)
  		   when (and (consp x)
					 (destructuring-bind (a . b) x
					   (or
						;; case of inserted text range (beg . end)
						;; (`end` being what we want)
						(and (numberp a) (numberp b))
						;; case of (deleted text . pos)
						(and (stringp a) (numberp b)))))
		   collect (abs (cdr x)) into ret
		   finally return (cl-remove-if ;; and remove positions point out of this buffer
						   #'(lambda (x) (> x (buffer-size)))
						   ret)))

(defun get-marker-list ()
  (cl-remove-duplicates
   (mapcar #'marker-position mark-ring)))

(cl-defun condense-undo-list (L &key (no-closer-than +no-closer-than+) (max-count +max-history-items+))
  (cl-loop for x in L and idx from 0
		   with cursor-idx = nil
		   when (and (< (length ret) max-count)
					 (or (null cursor-idx)
						 (>= (abs (- (elt L cursor-idx) x))
							 no-closer-than)))
		   collect x into ret
		   do (setq cursor-idx idx)
		   finally return ret))

(cl-defun merge-with-undo-list (undo-list lst &key (step 3))
  (cl-loop for x in (rest undo-list) and i from 1
		   if (zerop (mod i step))
		   append (list x (pop lst)) into ret
		   else
		   append (list x) into ret
		   finally return ret))

(defvar last-pos-idx 0)

(defun history-move (delta)
  "delta > 0 => go forward in time"
  (interactive)
  (make-local-variable 'last-pos-idx)
  (let ((history (merge-with-undo-list
				  (condense-undo-list (filter-undo-list-into-list))
				  ;; TODO adjust MAX-COUNT to account for interleaved marker positions
				  (get-marker-list))))
		 ;;(history (subseq history 0 (min (length history) +max-history-items+)))
	(if (zerop (length history))
		(message (format "Nowhere to go %s to. Start editing!"
						 (if (< delta 0) "back" "forward")))
	  (let ((dest-history-idx (if (or (eq last-command 'history-back)
									  (eq last-command 'history-forward)
									  (eq last-command 'history-move))
								  ;; different behavior for calling this function consecutively
								  (cyclize (+ (* -1 delta) last-pos-idx) history)
								;; 0 is the earliest edit position
								0)))

		;; remember this position in case of a forthcoming consecutive call
		(setq last-pos-idx dest-history-idx)

		;; (message "idx=%s, history=%s"
		;; 		 dest-history-idx history)

		;; navigation bar
		;; shows where you are (in temporal terms of undo history) while scrolling
		(let ((message-log-max nil)
			  (minibuffer-message-timeout 0)
			  (enable-recursive-minibuffers nil))
		  (message "%s"
				   (concat
					(propertize "【"
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					(propertize (loop repeat (- (length history) dest-history-idx) concat "●")
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					(propertize (loop repeat dest-history-idx concat "○")
								'face '(:family "Monospace")
								'face '(:weight 'ultra-light))
					(propertize "】"
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					)))

		;; actually move the cursor
		(goto-char (elt history dest-history-idx))))))

(defun history-move-old (delta)
  "delta > 0 => go forward in time"
  (interactive)
  (let ((available (filter-undo-list)))
	(if (zerop (length available))
		(message "No history to go back or forward to")
	  (let* (;;(available (n-highest (filter-undo-list) +only-consider-last-n+))
			 (history (fast-make-clusters available +max-history-items+))
			 (history ;; ensure last edit position, no matter how irrelevant, sticks
			  (if (elt available (1- (length available)))
				  (vconcat history
						   (vector (elt available (1- (length available)))))
				history))
			 (history (cl-remove-duplicates history))
			 )
		(if (<= (length history) 0)
			(message "No history to go back or forward to! Start editing!")
		  (let* ((pt (point))
				 ;; stateless--position in HISTORY is determined from current position
				 ;; closest match to current position becomes the reference point for moving
				 ;; (closest-matches (cl-sort (enumerate history)
				 ;; 						   #'<
				 ;; 						   :key #'(lambda (x)
				 ;; 									;; rank by proximity to this point
				 ;; 									(abs (- (car x) pt)))))
				 ;; less memory usage this way…
				 (closest-match-idx (cl-loop for idx from 0 below (length history)
											 for closest = 0 then (if (< (abs (- (elt history idx) pt))
																		 (abs (- (elt history closest) pt)))
																	  idx closest)
											 finally return closest))

				 ;; In case no more cyclization
				 ;; (dest-history-idx (max 0
				 ;;							(min (1- (length history))
				 ;;								 (+ delta closest-match-idx))))
				 (dest-history-idx (if (or (eq last-command 'history-back)
										   (eq last-command 'history-forward)
										   (eq last-command 'history-move))
									   ;; different behavior for first invocation vs doing it in a row
									   (cyclize (+ delta closest-match-idx) history)
									 (1- (length history))))
				 )

			;; (message "idx=%s, history=%s"
			;; 		 dest-history-idx history)

			;; navigation bar
			;; shows where you are (in temporal terms of undo history) while scrolling
			(let ((message-log-max nil)
				  (minibuffer-message-timeout 0)
				  (enable-recursive-minibuffers nil))
			  (message "%s"
					   (concat
						(propertize "【"
									'face '(:family "Monospace")
									'face '(:weight 'ultra-bold))
						(propertize (loop repeat (1+ dest-history-idx) concat "●")
									'face '(:family "Monospace")
									'face '(:weight 'ultra-bold))
						(propertize (loop repeat (- (length history) dest-history-idx 1) concat "○")
									'face '(:family "Monospace")
									'face '(:weight 'ultra-light))
						(propertize "】"
									'face '(:family "Monospace")
									'face '(:weight 'ultra-bold))
						)))

			;; actually move the cursor
			(goto-char (elt history dest-history-idx))))))))

(defun history-forward ()
  (interactive)
  (history-move 1))

(defun history-back ()
  (interactive)
  (history-move -1))

(provide 'longer-jump)
