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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)

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

;; real code

(defun filter-undo-list ()
  (cl-loop for x in buffer-undo-list
           when (and (consp x) (numberp (car x)))
           vconcat (vector
                    (if (numberp (car x))
                        ;; this is a (beg . end) inserted text item
                        ;; use 'weight' to prioritize the stability of meaningful edits
                        ;;    pos     weight
                        (cons (car x) (- (cdr x) (car x)))
                      ;; this is a (text . pos) deleted text item
                      ;;    pos     weight
                      (cons (cdr x) (length (car x)))))
           ;; above actually preserves the counterintuitive ordering of BUFFER-UNDO-LIST
           ;; let's reverse it
           into ret
           finally return (reverse ret)))

;; (let ((u (filter-undo-list)))
;;   (- (apply #'max u) (apply #'min u)))

;; (defun variance (L)
;;   (let* ((mean (/ (apply #'+ L) (length L))))
;;  (- (/ (reduce #'(lambda (x y) (+ x (expt y 2))) L)
;;        (length L))
;;     (expt mean 2))))

(defun mean-diff (V)
  "Expected difference between each adjacent value"
  (cl-loop for i from 1 below (length V)
           for (thisPos . nil) = (elt V i)
           for (prevPos . nil) = (elt V (1- i))
           sum (abs (- thisPos prevPos)) into total
           finally return (/ total (length V))))

(defun mean-weights (V)
  "Expected value of the 'weights'--which are a number of characters a given edit had affected"
  (cl-loop for (nil . weight) across V
           sum weight into total
           finally return (/ total (length V))))

(defun maximize-separation (V min-separation)
  "Drops positions that are too close to its neighbors (according to MIN-SEPARATION) and don't represent meaningful edits"
  (cl-loop for i from 1 below (length V)
           for (thisPos . thisWeight) = (elt V i)
           for (prevPos . thatWeight) = (elt V (1- i))
           ;; with mean-weights = (mean-weights V) ;; this caused too much randomness
           ;; when (or (> thisWeight (* 2 mean-weights)) ;; more important edits have priority
           when (>= (abs (- thisPos prevPos)) min-separation)
           vconcat (vector (cons thisPos thisWeight))))

(defun make-clusters (V max-length)
  "Minimize the undo positions to a set of 'landmark' positions worthy of being included among MAX-LENGTH positions"
  (cl-loop for u = V then (maximize-separation u (mean-diff u))
           until (< (length u) max-length)
           finally return (cl-map 'vector #'car u)))

(defconst +max-history-items+ 70 ;; magic number
  "Upper limit (exclusive) of history items--feel free to change this")
;; TODO defcustom-ize

(defun history-clusters ()
  "Sane default for consumption"
  (cl-remove-duplicates (make-clusters (filter-undo-list) +max-history-items+)))

(defun history-move (delta)
  "delta > 0 => go forward in time"
  (interactive)
  (let* ((history (history-clusters))
         (pt (point))
         ;; stateless--position in HISTORY is determined from current position
         ;; closest match to current position becomes the reference point for moving
         (closest-matches (cl-sort (enumerate history)
                                   #'<
                                   :key #'(lambda (x)
                                            ;; rank by proximity to this point
                                            (abs (- (car x) pt)))))
         (closest-match-idx (cdr (elt closest-matches 0)))
         ;; (dest-history-idx (max 0
         ;;                         (min (1- (length history))
         ;;                              (+ delta closest-match-idx))))
         (dest-history-idx (cyclize (+ delta closest-match-idx) history))
         )

    ;; (message "history: predicted-idx=%s, dest-idx=%s, len(h)=%d, matches=%s; %s"
    ;;       closest-match-idx dest-history-idx (length history) (seq-subseq (append closest-matches nil) 0 5))

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

    ;; ;; fill with empty air to prevent ghosting
    ;; (propertize (loop repeat (- +max-history-items+ (length history)) concat "_")
    ;;          ;; 'invisible t
    ;;          'face '(:box t))

    ;; actually move the cursor
    (goto-char (elt history dest-history-idx))))

(defun history-forward ()
  (interactive)
  (history-move 1))

(defun history-back ()
  (interactive)
  (history-move -1))

(provide 'longer-jump)

;; (defun history-back ()
;;   (interactive)
;;   (let* ((pt (point))
;;       (found (cl-position pt *history* :test #'(lambda (x y) (<= (abs (- x y)) tolerance))))
;;       (going-to-idx (cond ((equal *history-pos* pt) (1- *history-pos*))
;;                           (found (1- found))
;;                           (t 0))))
;;  (message "history-back: Going to idx: %d; found @ %d; history-pos: %d; %s" going-to-idx (or found -1) *history-pos* *history*)
;;  (goto-char (circular-nth going-to-idx *history*))
;;  (setq *history-pos* going-to-idx)
;;  (setq *history* (history-clusters))))

;; (defun history-forward ()
;;   (interactive)
;;   (let* ((pt (point))
;;       (found (cl-position pt *history* :test #'(lambda (x y) (<= (abs (- x y)) tolerance))))
;;       (going-to-idx (cond ((equal *history-pos* pt) (1+ *history-pos*))
;;                           (found (1+ found))
;;                           (t -1))))
;;  (message "history-forward: Going to idx: %d; found @ %d; history-pos: %d; %s" going-to-idx (or found -1) *history-pos* *history*)
;;  (goto-char (circular-nth going-to-idx *history*))
;;  (setq *history-pos* going-to-idx)
;;  (setq *history* (history-clusters))))


;; for solving ties

;; (loop for (pos . idx) across closest-matches
;;    with first-pos = (car (elt closest-matches 0))
;;    while (= pos first-pos)
;;    collect idx into ret
;;    ;; solve position ties
;;    finally return (cond ((= 1 (length ret))
;;                          (first ret))
;;                         ((> delta 0)
;;                          (apply #'max ret))
;;                         ((< delta 0)
;;                          (apply #'min ret))
;;                         (t ;; error state but I'll let it slide
;;                          (first ret))))
