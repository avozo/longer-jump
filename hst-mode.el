;;; package --- Summary:
;;; hst-mode.el --- Go back to last relevant cursor position
;;; 
;;; Author: Zelly Snyder <zelly@outlook.com>
;;; 
;;; Commentary:
;;; 
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with this program.	 If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; For inspiration:
;; https://github.com/microsoft/vscode/blob/master/src/vs/workbench/services/history/browser/history.ts
;; VSCode's implementation of go back/forward

(require 'cl)
(require 'ring)

(defgroup hst nil
  "Navigate through recent cursor positions in each buffer."
  :group 'hst)

;; constants
;; configurable

(defcustom hst-mode--unvisited-point-character "-"
  "Character to display as an empty space in the navigation progress bar."
  :type 'string
  :options '(" " "-")
  :group 'hst)

(defcustom hst-mode--visited-point-character "+"
  "Character to display in the navigation/progress bar to represent points preceding the current location's point."
  :type 'string
  :options '(" " "+")
  :group 'hst)

(defcustom hst-mode-max-history-entries 16
  "Maximum number of locations per buffer that you want to be able to go back or go forward to."
  :type 'number
  :group 'hst)

(defcustom hst-mode-threshold-lines-moved 2
  "Minimum line number distance before a new history entry is added."
  :type 'number
  :group 'hst)

(defcustom hst-mode-use-multiple-buffers nil
  "Set this to T if you want to be able to go back and forth to different buffers; NIL makes your history local to each buffer."
  :type 'boolean
  :group 'hst)

;; utils

(defun hst-mode--what-line-is-this (pos)
  "Gives the line number corresponding to a position POS."
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 pos))))

(defun hst-mode--list-all-equal-p (lst)
  "Return T for a list LST with identical members."
  (unless (null lst)
    (= (length lst) (cl-count (first lst) lst))))

;; logging history

(defvar-local hst-mode--navigation-idx 0)

(defvar-local hst-mode--ring (make-ring hst-mode-max-history-entries))

(defun hst-mode--push (&optional marker)
  "Push a MARKER onto the history ring."
  (ring-insert hst-mode--ring (or marker (point-marker))))


;; clustering


(defun hst-mode--k-means (sequence k)
  (let* ((clusters (let ((l (make-list k '())))
					 (dolist (observation sequence)
					   (let ((random-cluster-idx (random k)))
						 (setf (elt l random-cluster-idx)
							   (cons observation (elt l random-cluster-idx)))))
					 l))
		 (means (make-list k 0)))
	(do ((equal-count 0 equal-count))
		((equal equal-count
				(count-if #'(lambda (x) (not (null x))) clusters))
		 clusters)
	  (message "%s" clusters)
	  ;; calculate means
	  (setq means (loop for c in clusters
						collect (if (null c)
									0
								  (/ (apply #'+ c)
									 (min 1 (length c))))))
	  ;; assignment step
	  (do ((observation sequence (cdr observation))
		   (observation-idx 0 (1+ observation-idx)))
		  ((null observation) t)
	    (let ((closest-neighbor (car (cl-sort
									  (loop for x in means and idx from 0
											collect (list x idx))
									  '<
									  :key '(lambda (x)
											  (cl-multiple-value-bind (mean i) x
												(abs (- mean (car observation)))))))))
		  (cl-multiple-value-bind (closest-neighbor-mean closest-neighbor-idx) closest-neighbor
			(if (equal closest-neighbor-idx
					   observation-idx)
				(incf equal-count)
			  (progn
				(do ((cluster-idx 0 (1+ cluster-idx)))
					((> cluster-idx (length clusters)))
				  (dolist (item (elt clusters cluster-idx))
					(when (equal item (car observation))
					  (setf (elt clusters cluster-idx)
							(remove item (elt clusters cluster-idx))))))
				(setf (elt clusters (cadr closest-neighbor))
					  (cons (car observation) (elt clusters closest-neighbor-idx)))))))))))

;; try it out
(hst-mode--k-means
 (loop repeat 14
	   collect (random 1000))
 3)


(defun hst-mode--justifies-new-entry (candidate-position)
  "Determine whether some new position CANDIDATE-POSITION should be pushed onto the history ring."
  (or (ring-empty-p hst-mode--ring)
      (let* ((newest-entry (ring-ref hst-mode--ring 0))
             (newest-entry-pos (marker-position newest-entry))
             (newest-entry-line-number (hst-mode--what-line-is-this newest-entry-pos))
             (candidate-line-number (hst-mode--what-line-is-this candidate-position)))
        (>= (abs (- newest-entry-line-number candidate-line-number))
            hst-mode-threshold-lines-moved))))

(defun hst-mode--run-check ()
  "Add current position to history if it's a good idea."
  (let ((pos (point)))
    (unless
     (or
      ;; do nothing if the editor hasn't loaded yet
      (null pos)
      ;; short circuit if we jumped by traversing history
      (hst-mode--currently-mid-navigation-p)
      ;; short circuit if the cursor is jumping as a result of
      ;; repeated (possibly programmatic) navigation; check the last
      ;; few commands
      (hst-mode--list-all-equal-p
       (mapcar
        #'first
        (cl-subseq
         command-history
         0
         (min (length command-history) 3)))))
     (unless (or (ring-empty-p hst-mode--ring)
                 (hst-mode--justifies-new-entry pos))
       ;; Replace the newest entry when we shouldn't add a new one de novo.
       ;; That way we go back to the area we remember the most (the last place we were working on). So we only actually record the places we /jumped/ to.
       (ring-remove hst-mode--ring 0))
     (hst-mode--push))))

(defun hst-mode--register-listener ()
  "Start checking for new history positions by hooking into editor commands."
  (interactive)
  (add-hook 'post-command-hook #'hst-mode--run-check t t)
  (make-local-variable 'hst-mode--ring)
  (setq hst-mode--ring (make-ring hst-mode-max-history-entries))
  (make-local-variable 'hst-mode--navigation-idx)
  (setq hst-mode--navigation-idx 0))

(defun hst-mode--unregister-listener ()
  "Stop checking for new history positions; useful for debugging this package."
  (interactive)
  (remove-hook 'post-command-hook #'hst-mode--run-check t))

(defun hst-mode--goto-mark (marker)
  "Go to a MARKER, maybe even if it's in another buffer, if you set a customization variable."
  (when hst-mode-use-multiple-buffers
    (set-buffer (marker-buffer marker)))
  (goto-char (marker-position marker)))

(defun hst-mode--navigate (delta)
  "Traverse the history ring by indexing into the history ring by an offset DELTA."
  (unless (ring-empty-p hst-mode--ring)
    (setq hst-mode--navigation-idx (if (hst-mode--currently-mid-navigation-p)
                                       (+ delta hst-mode--navigation-idx)
                                     delta))
    (let* ((dest-marker (ring-ref hst-mode--ring
                                  hst-mode--navigation-idx))
          (dest-pos (marker-position dest-marker))
          (mod-idx (mod hst-mode--navigation-idx (ring-length hst-mode--ring))))
      (hst-mode--goto-mark dest-marker)
      ;; make the destination line the center line on the screen
      (recenter nil)
      ;; show a nice visual indicator; pulse animation on the line
      (pulse-momentary-highlight-one-line dest-pos)
      ;; display a bar in minibuffer; orient yourself on where you are in your history as you are navigating through it
      (message "Navigation History %s"
               (concat
                (propertize "["
                            'face '(:family "Monospace")
                            'face '(:weight 'ultra-bold))
                (propertize (loop repeat (- (ring-length hst-mode--ring)
                                            mod-idx)
                                  concat hst-mode--visited-point-character)
                            'face '(:family "Monospace")
                            'face '(:weight 'ultra-bold))
                (propertize (loop repeat mod-idx
                                  concat hst-mode--unvisited-point-character)
                            'face '(:family "Monospace")
                            'face '(:weight 'ultra-light))
                (propertize "]"
                            'face '(:family "Monospace")
                            'face '(:weight 'ultra-bold)))))))

(defun hst-mode--currently-mid-navigation-p ()
  "Are you in the middle of traversing the history?"
  (member last-command '(hst-mode--go-back
                         hst-mode--go-forward
                         hst-mode--navigate)))

(defun hst-mode--go-back ()
  "Go to previous location in history; go to the next oldest location."
  (interactive)
  (hst-mode--navigate 1))

(defun hst-mode--go-forward ()
  "Go to the next newest location in history."
  (interactive)
  (hst-mode--navigate -1))

(defvar hst-mode-map
  (let ((m (make-sparse-keymap)))
    ;; use the Command/Windows/Super key by default
    (define-key m (kbd "s-[") #'hst-mode--go-back)
    (define-key m (kbd "s-]") #'hst-mode--go-forward)
    m))

;;;###autoload
(define-minor-mode hst-mode "hst-mode" t
  :lighter "hst"
  :keymap hst-mode-map
  :group 'hst
  (if (member #'hst-mode--run-check post-command-hook)
      (hst-mode--unregister-listener)
    (hst-mode--register-listener)))

(provide 'hst-mode)
;;; hst-mode.el ends here
