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

(defconst +max-history-items+ 25 ;; magic number
  "Upper limit (exclusive) of history items--feel free to change this")
;; TODO defcustom-ize

(defconst +only-consider-last-n+ 210
  ;; most-positive-fixnum
  ;; ^^^^^^^^^^^^^^^^^^^^ use that if you want to consider entire
  ;; history going back as far as your UNDO-LIMIT is set (I don't
  ;; recommend)
  "Maximum (inclusive) recent history items to use")

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
  "Get the latter portion of SEQ consisting of up to (exclusive) N items"
  (subseq seq
		  (max 0 (- (length seq) n))
		  (length seq)))


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
	  (cl-loop for idx from 0 below (length data) by window-width-total
			   and window = (seq-subseq data
										(max 0 (- idx window-width-left))
										(min (length data) (+ idx window-width-right 1)))
			   while (< idx (length data))
			   vconcat (vector
						(let ((avg (mean window)))
						  ;; centroid of this window
						  (argmin #'(lambda (data-point)
									  (abs (- data-point avg)))
								  window)))))))

(defun filter-undo-list ()
  "Converting the native BUFFER-UNDO-LIST to something usable"
  (cl-loop for x in buffer-undo-list
		   when (and (consp x) (numberp (car x)))
		   vconcat (vector
					(if (numberp (car x)) (car x) (cdr x)))
		   ;; above actually preserves the counterintuitive ordering of BUFFER-UNDO-LIST
		   ;; let's reverse it
		   into ret
		   finally return (reverse ret)))

(defun history-move (delta)
  "delta > 0 => go forward in time"
  (interactive)
  (let ((history (cl-remove-duplicates
				  (fast-make-clusters
				   (n-highest (filter-undo-list) +only-consider-last-n+)
				   +max-history-items+))))
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
			 (closest-match-idx (cdr
								 (argmin #'(lambda (history-item)
											 (abs (- (car history-item) pt)))
										 (enumerate history))))
			 ;; in case no more cyclization ;; shit
			 ;; (dest-history-idx (max 0
			 ;;							(min (1- (length history))
			 ;;								 (+ delta closest-match-idx))))
			 (dest-history-idx (cyclize (+ delta closest-match-idx) history))
			 )

		;; (message "history: %s"
		;; 		 history)

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
		(goto-char (elt history dest-history-idx))))))

(defun history-forward ()
  (interactive)
  (history-move 1))

(defun history-back ()
  (interactive)
  (history-move -1))

(provide 'longer-jump)
