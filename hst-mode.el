;; longer-jump.el --- Go back to last relevant cursor position
;;
;; Author: Zelly D. Snyder <zds@zds.ai>
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
  (cond ((< n 0) (+ (length seq) n))
		((>= n (length seq)) (mod n (length seq)))
		(t n)))

(defun push-into-queue (v x max-len)
  "cons X to list V as a FIFO queue; keeps len(V) <= MAX-LEN"
  (let ((ret (cons x v)))
	(if (> (length ret) max-len)
		(cl-subseq ret 0 max-len)
	  ret)))

;; logging history

;(make-variable-buffer-local
(defvar hst-record '())
;)

;(make-variable-buffer-local ;; TODO try this way
(defvar last-pos-idx 0)
;)

(cl-defun start-recording-points (target-buffer &optional (max-len 4))
  (run-at-time "0 sec"
			   2
			   (lambda (target-buffer max-len tolerance)
				 ;(message "current-buffer == this-buffer: %s" (eq (current-buffer) target-buffer))
				 (when (and (eq (current-buffer) target-buffer)
							(not (or (eq last-command 'history-back)
									 (eq last-command 'history-forward)
									 (eq last-command 'history-move))))
				   (setq hst-record (push-into-queue hst-record (point) max-len))
				   (when (and (> (length hst-record) 0)
							  (< (- (apply #'max hst-record)
									(apply #'min hst-record))
								 tolerance)
							  (or (zerop (length mark-ring))
								  (> (abs (- (marker-position (first mark-ring))
											 (first hst-record)))
									 tolerance)))
					 (push-mark (first hst-record) t nil))))
			   target-buffer max-len +no-closer-than+))

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
								  (cyclize (+ (* -1 delta) last-pos-idx) history)
								;; 0 is the earliest edit position
								0)))

		;; remember this position in case of a forthcoming consecutive call
		(setq last-pos-idx dest-history-idx)

		;; (message "idx=%s, history=%s"
		;;		 dest-history-idx history)

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

(defun hst-forward ()
  (interactive)
  (history-move 1))

(defun hst-back ()
  (interactive)
  (history-move -1))

(defvar hst-map (make-sparse-keymap))

(define-minor-mode hst-mode
  :lighter " hist"
  :keymap (progn
  			(define-key hst-map (kbd "<f9>") 'hst-back)
  			(define-key hst-map (kbd "<f12>") 'hst-forward)
  			;; (define-key m (kbd "<f6>") #'(lambda ()
  			;; 							   (cancel-timer hst-timer)))
  			hst-map)
  (progn (make-local-variable 'last-pos-idx)
		 (make-local-variable 'hst-record)
		 (start-recording-points (current-buffer))))

(define-globalized-minor-mode global-hst-mode
  hst-mode
  (lambda ()
	(hst-mode t)))

(provide 'hst-mode)
