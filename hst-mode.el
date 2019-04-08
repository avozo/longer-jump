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

;; utils

(defun hst-mode--what-line-is-this (pos)
  "Gives the line number corresponding to a position"
  (save-excursion
    (beginning-of-line)
    (1+ (count-lines 1 pos))))

(defun hst-mode--list-all-equal-p (lst)
  "Returns T for all lists with identical members."
  (unless (null lst)
    (= (length lst) (cl-count (first lst) lst))))

;; logging history

(defvar hst-mode--navigation-idx 0)
(make-variable-buffer-local 'hst-mode--navigation-idx)

(defvar hst-mode--ring (make-ring hst-mode-max-history-entries))
(make-variable-buffer-local 'hst-mode--ring)

(defun hst-mode--push (&optional marker)
  (interactive)
  (ring-insert hst-mode--ring (or marker (point-marker))))

(defun hst-mode--justifies-new-entry (candidate-position)
  "Determine whether some new position should be pushed onto the history ring."
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
      ;; short circuit if the cursor is jumping as a result of repeated (possibly programmatic) navigation; check the last few commands
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
  "Starts checking for new history positions by hooking into editor commands."
  (interactive)
  (add-hook 'post-command-hook #'hst-mode--run-check t t))

(defun hst-mode--unregister-listener ()
  "Stop checking for new history positions; useful for debugging this package."
  (interactive)
  (remove-hook 'post-command-hook #'hst-mode--run-check t))

(defun hst-mode--navigate (delta)
  "Traverse the history ring by indexing into the history ring by an offset DELTA."
  (unless (ring-empty-p hst-mode--ring)
    (setq hst-mode--navigation-idx (if (hst-mode--currently-mid-navigation-p)
                                       (+ delta hst-mode--navigation-idx)
                                     delta))
    (let ((dest (marker-position
                 (ring-ref hst-mode--ring (min hst-mode--navigation-idx (ring-length hst-mode--ring)))))
          (mod-idx (mod hst-mode--navigation-idx (ring-length hst-mode--ring))))
      (goto-char dest)
      ;; make the destination line the center line on the screen
      (recenter nil)
      ;; show a nice visual indicator; pulse animation on the line
      (pulse-momentary-highlight-one-line dest)
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
