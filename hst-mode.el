;; longer-jump.el --- Go back to last relevant cursor position
;;
;; Author: Zelly Snyder <zelly@outlook.com>
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

(eval-when-compile (require 'cl))

(defgroup hst nil
  "Navigate through recent cursor positions in a buffer"
  :group 'hst)

;; constants

(defcustom no-closer-than 120
  "Controls maximum proximity of any consecutive positions. Earlier position (one you're most likely to remember) is used when there are candidates to filter out."
  :type 'number
  :options '(70 80 150 320 400)
  :group 'hst)

(defcustom unvisited-point-character " "
  "Character to display as an empty space in the navigation progress bar"
  :type 'string
  :options '(" " "-")
  :group 'hst)

(defcustom visited-point-character "+"
  "Character to display in the navigation/progress bar to represent points preceding the current location's point"
  :type 'string
  :options '(" " "+")
  :group 'hst)

(defcustom n-locations-in-history-before-repeat 3
  "Number of most recent history locations to ensure are far away from the current point so as to prevent clumping of history locations.")

(defcustom interval-in-seconds-to-find-new-locations 0.5
  "How many idle seconds to wait before inspecting the buffer to calculate new locations")

(defvar hst-mode--in-recursive-edit nil)

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
  (or hst-mode
      (eq last-command 'history-back)
	  (eq last-command 'hitory-forward)
	  (eq last-command 'history-move)))

(cl-defun start-recording-points (target-buffer)
  (run-with-idle-timer
   interval-in-seconds-to-find-new-locations t
   #'(lambda (tolerance target-buffer)
       (let ((this-point (point)))
         (cond (mark-active
                ;; make sure we're not interrupting while user is making a transient mark
                nil)
               ;; don't record points while stepping over history
               ((currently-stepping-over-locations-p) nil)
               ;; immediately add THIS-POINT to the mark-ring if the mark-ring is empty
               ((zerop (length mark-ring))
                (push-mark this-point t nil))
               ;qqqq; don't record the same point again
               ((= (marker-position (elt mark-ring 0)) this-point)
                nil)
               ;; only record when the oldest point that we want to check is further than TOLERANCE points away from THIS-POINT
               ((>= (abs (- this-point
                            (marker-position (elt mark-ring (min (1- (length mark-ring)) n-locations-in-history-before-repeat)))))
                    tolerance)
                (push-mark this-point t nil)))))
   no-closer-than target-buffer))

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

		;; (message "idx=%s, history=%s"
		;;		 dest-history-idx history) ;; shit test

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

(defun hst-mode--cancel ()
  (interactive)
  (hst-mode -1)
  )

(defvar hst-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m [prior] #'history-back)
    (define-key m [next] #'history-forward)
    m))

(define-minor-mode hst-mode "hst-mode" t
  :lighter " hst"
  :keymap hst-mode-map
  )

(global-set-key (kbd "s-\\") #'(lambda ()
                                 (interactive)
                                 (hst-mode)))

(define-minor-mode hst-minor-mode "hst-mode" t
  :lighter "hst"
  (start-recording-points (current-buffer))
  )

(provide 'hst-minor-mode)
