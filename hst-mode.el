;;; package --- Summary:
;;; longer-jump.el --- Go back to last relevant cursor position
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

(eval-when-compile (require 'cl))

(defgroup hst nil
  "Navigate through recent cursor positions in a buffer"
  :group 'hst)

;; constants

(defcustom unvisited-point-character "-"
  "Character to display as an empty space in the navigation progress bar"
  :type 'string
  :options '(" " "-")
  :group 'hst)

(defcustom visited-point-character "+"
  "Character to display in the navigation/progress bar to represent points preceding the current location's point"
  :type 'string
  :options '(" " "+")
  :group 'hst)

(defvar hst-mode--n-locations-in-history-before-repeat
  (/ mark-ring-max 4)
  "Number of most recent history locations to ensure are far away from the current point so as to prevent clumping of history locations.")

(defcustom hst-mode--hard-minimum-distance-to-nth-previous-location 40
  "Absolute minimum distance that must separate a new candidate history location from the Nth previous location, as defined by HST-MODE--N-LOCATIONS-IN-HISTORY-BEFORE-REPEAT")

(defcustom interval-in-seconds-to-find-new-locations 1.0
  "How many idle seconds to wait before inspecting the buffer to calculate new locations")

;; utils

(defun cyclize (n seq)
  "Negative or too large index -> correct index. Makes an 'index' valid over (-inf, +inf). This corrects N without actually calling ELT."
  (mod n (length seq)))

(defun push-into-queue (v x max-len)
  "cons X to list V as a FIFO queue; keeps len(V) <= MAX-LEN"
  (let ((ret (cons x v)))
	(if (> (length ret) max-len)
		(cl-subseq ret 0 max-len)
	  ret)))

;; logging history

(make-variable-buffer-local
 (defvar last-pos-idx 0)
 )

(defun currently-stepping-over-locations-p ()
  (or (eq last-command #'history-back)
	  (eq last-command #'history-forward)
	  (eq last-command #'history-move)))

(make-variable-buffer-local
 (defvar hst-mode--timer))

(cl-defun start-recording-points (target-buffer)
  (if (and (null hst-mode) (timerp hst-mode--timer))
      ;; Discard this timer if HST-MODE is disabled
      (cancel-timer hst-mode--timer)
    (setq hst-mode--timer
          (run-with-idle-timer
           interval-in-seconds-to-find-new-locations t
           #'(lambda (tolerance target-buffer)
               (unless (or
                        ;; make sure we're not interrupting while user is making a transient mark
                        mark-active
                        ;; don't record points while stepping over history
                        (currently-stepping-over-locations-p))
                 (let ((this-point (point)))
                   (cond
                    
                    ;; immediately add THIS-POINT to the mark-ring if the mark-ring is empty
                    ((zerop (length mark-ring))
                     (push-mark this-point t nil))
                    
                    ;; only record when the oldest point that we want to check is further than TOLERANCE points away from THIS-POINT
                    ((let ((distance (abs (- this-point
                                             (marker-position (elt mark-ring (min (1- (length mark-ring)) hst-mode--n-locations-in-history-before-repeat)))))))
                       (and (>= distance tolerance)
                            
                            ;; Still ensure that the point is no closer than a predefined static minimum distance; avoiding points too close when the TOLERANCE is too small (i.e., TOLERANCE is set dynamically based on the buffer size).
                            (>= distance hst-mode--hard-minimum-distance-to-nth-previous-location)
                            )
                       )
                     ;; Get rid of all the very recent points which are further away from THIS-POINT than TOLERANCE.
                     (progn
                       (dotimes (i (min (length mark-ring) hst-mode--n-locations-in-history-before-repeat))
                         (when (< tolerance
                                  (abs (- this-point
                                          (marker-position (elt mark-ring i)))))
                           (pop-mark)))
                       ;; Record THIS-POINT.
                       (push-mark this-point t nil)))))))
           
           ;; Dynamically sets the tolerance proportional to the number of places in MARK-RING over the total number of points in the buffer.
           ;; TOLERANCE is how close two /recent/ history locations can be to each other, measured in points.
           (/ mark-ring-max ;; global Emacs-wide variable
              (point-max))
           
           target-buffer))))

(defun history-move (delta)
  "delta > 0 => go forward in time"
  (interactive)
  (let ((history (mapcar #'marker-position mark-ring)))
		 ;;(history (subseq history 0 (min (length history) +max-history-items+)))
	(if (zerop (length history))
		(message (format "Nowhere to go %s to. Start editing!"
						 (if (< delta 0) "back" "forward")))
	  (let ((dest-history-idx (if (currently-stepping-over-locations-p)
								  ;; different behavior for calling this function consecutively
								  (cyclize (- last-pos-idx delta) history)
								;; 0 is the earliest edit position
								0)))

		;; remember this position in case of a forthcoming consecutive call
		(setq last-pos-idx dest-history-idx)

	    ;; navigation bar
		;; shows where you are (in temporal terms of undo history) while scrolling
		(let ((message-log-max nil)
			  (minibuffer-message-timeout 0)
			  (enable-recursive-minibuffers nil))
		  (message "In navigation history %s"
				   (concat
					(propertize "["
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					(propertize (loop repeat (- (length history) dest-history-idx) concat visited-point-character)
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					(propertize (loop repeat dest-history-idx concat unvisited-point-character)
								'face '(:family "Monospace")
								'face '(:weight 'ultra-light))
					(propertize "]"
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					)))

		;; actually move the cursor
		(let ((destination-char (elt history dest-history-idx)))
          (goto-char destination-char)
          ;; show a visual effect--highlight the line as a momentary flash
          (pulse-momentary-highlight-one-line destination-char))
        ;; also move window to middle of screen
        (recenter nil))))
  )

(defun history-forward ()
  (interactive)
  (history-move 1)
  )

(defun history-back ()
  (interactive)
  (history-move -1)
  )

;; (defvar hst-mode-map
;;   (let ((m (make-sparse-keymap)))
;;     (define-key m [prior] #'history-back)
;;     (define-key m [next] #'history-forward)
;;     (global-set-key (kbd "s-\\") #'(lambda ()
;;                                      (interactive)
;;                                      (hst-mode -1))))
;;     m)

(defvar hst-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "s-[") #'history-back)
    (define-key m (kbd "s-]") #'history-forward)
    m))

;;;###autoload
(define-minor-mode hst-mode "hst-mode" t
  :lighter "hst "
  :keymap hst-mode-map
  :group 'hst
  (start-recording-points (current-buffer))
  )

(provide 'hst-mode)
;;; hst-mode.el ends here
