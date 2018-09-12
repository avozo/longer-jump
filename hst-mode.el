;; longer-jump.el --- Go back to last relevant cursor position
;;
;; Author: Zelly D. Snyder <zelly@iappp.app>
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

(defconst +no-closer-than+ 80
  "Controls maximum proximity of any consecutive positions. Earliest position (one you're most likely to remember) is used when there are candidates to filter out.")

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

(cl-defun start-recording-points (target-buffer &optional (max-len 5) (secs-delay 0.5))
  (run-with-idle-timer
   secs-delay t
   #'(lambda (tolerance target-buffer)
       (let ((this-point (point)))
	     (when (and (eq (current-buffer) target-buffer)
				    (not (or (eq last-command 'history-back)
						   (eq last-command 'history-forward)
						   (eq last-command 'history-move)))
                    ;; make sure this point is not too close to recent points
                    (cl-every #'(lambda (other-marker)
                                  (let ((other-point (marker-position other-marker)))
                                    (>= (abs (- other-point this-point))
                                        tolerance)))
                              (subseq mark-ring 0 (min (length mark-ring)
                                                       5)) ;; how far to check back
                              ))
		   (push-mark (point) t nil))))
   +no-closer-than+ target-buffer))

(defun history-move (delta)
  "delta > 0 => go forward in time"
  (interactive)
  (let ((history (mapcar #'marker-position mark-ring)))
		 ;;(history (subseq history 0 (min (length history) +max-history-items+)))
	(if (zerop (length history))
		(message (format "Nowhere to go %s to. Start editing!"
						 (if (< delta 0) "back" "forward")))
	  (let ((dest-history-idx (if (or (eq last-command 'hst-back)
									  (eq last-command 'hst-forward)
									  (eq last-command 'history-move))
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
					(propertize (loop repeat (- (length history) dest-history-idx) concat "+")
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					(propertize (loop repeat dest-history-idx concat " ")
								'face '(:family "Monospace")
								'face '(:weight 'ultra-light))
					(propertize "]"
								'face '(:family "Monospace")
								'face '(:weight 'ultra-bold))
					)))

		;; actually move the cursor
		(goto-char (elt history dest-history-idx))))))

(defun hst-forward ()
  (interactive)
  (history-move 1)
  ;; also move window to middle of screen
  (recenter-top-bottom)
  )

(defun hst-back ()
  (interactive)
  (history-move -1)
  ;; also move window to middle of screen
  (recenter-top-bottom)
  )

(defvar hst-map (make-sparse-keymap))

(define-minor-mode hst-mode
  :lighter " hst"
  :keymap (progn
  			;; (define-key hst-map (kbd "C-{") 'hst-back)
  			;; (define-key hst-map (kbd "C-}") 'hst-forward)
  			hst-map)
  (start-recording-points (current-buffer))
)

(define-globalized-minor-mode global-hst-mode
  hst-mode
  (lambda ()
	(hst-mode t)))

(provide 'hst-mode)
