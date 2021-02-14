;;; nerd-icons-dired.el

;; Copyright (C) 2021 Wizard962 <smatyfei@gmail.com>

;; Author: Wizard962 <smatyfei@gmail.com>
;; Created: 2021/02/14
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4+"))
;; URL: https://github.com/wizard962/nerd-icons-dired
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Change Log:

;;  0.1.0  2021/02/14 Initial version.

;;; Code:
(require 'dired)
(require 'nerd-icons)

(defface nerd-icons-dired-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'nerd-icons-faces)

(defcustom nerd-icons-dired-v-adjust 0.01
  "The default vertical adjustment of the icon in the dired buffer."
  :group 'nerd-icons
  :type 'number)

(defvar-local nerd-icons-dired-displayed nil
  "Flags whether icons have been added.")

(defun nerd-icons-dired--display ()
  "Display the icons of files in a dired buffer."
  (when (and (not nerd-icons-dired-displayed) dired-subdir-alist)
    (setq-local nerd-icons-dired-displayed t)
    (let ((inhibit-read-only t)
	        (remote-p (and (fboundp 'tramp-tramp-file-p)
                         (tramp-tramp-file-p default-directory))))
      (save-excursion
	      (goto-char (point-min))
	      (while (not (eobp))
	        (when (dired-move-to-filename nil)
	          (let ((file (dired-get-filename 'verbatim t)))
	            (unless (member file '("." ".."))
		            (let ((filename (dired-get-filename nil t)))
		              (if (file-directory-p filename)
		                  (let* ((matcher (nerd-icons-match-to-alist file nerd-icons-dir-icon-alist))
			                       (icon (cond
				                            (remote-p
				                             (nerd-icons-octicon "file-directory" :v-adjust nerd-icons-dired-v-adjust :face 'nerd-icons-dired-dir-face))
				                            ((file-symlink-p filename)
				                             (nerd-icons-octicon "file-symlink-directory" :v-adjust nerd-icons-dired-v-adjust :face 'nerd-icons-dired-dir-face))
				                            ((nerd-icons-dir-is-submodule filename)
				                             (nerd-icons-octicon "file-submodule" :v-adjust nerd-icons-dired-v-adjust :face 'nerd-icons-dired-dir-face))
				                            ((file-exists-p (format "%s/.git" filename))
				                             (nerd-icons-octicon "repo" :v-adjust nerd-icons-dired-v-adjust :face 'nerd-icons-dired-dir-face))
				                            (t (apply (car matcher) (list (cadr matcher) :face 'nerd-icons-dired-dir-face :v-adjust nerd-icons-dired-v-adjust))))))
			                  (insert (concat icon " ")))
		                (insert (concat (nerd-icons-icon-for-file file :v-adjust nerd-icons-dired-v-adjust) " ")))))))
	        (forward-line 1))))))

(defun nerd-icons-dired--reset (&optional _arg _noconfirm)
  "Functions used as advice when redisplaying buffer."
  (setq-local nerd-icons-dired-displayed nil))

;;;###autoload
(define-minor-mode nerd-icons-dired-mode
  "Display nerd-icons icon for each files in a dired buffer."
  :lighter " nerd-icons-dired-mode"
  (if (and (display-graphic-p) nerd-icons-dired-mode)
      (progn
        (add-hook 'dired-after-readin-hook 'nerd-icons-dired--display t t)
        (when (derived-mode-p 'dired-mode)
          (nerd-icons-dired--display)))
    (remove-hook 'dired-after-readin-hook 'nerd-icons-dired--display t)
    (dired-revert)))

(advice-add 'dired-revert :before #'nerd-icons-dired--reset)

(provide 'nerd-icons-dired)
;;; nerd-icons-dired.el ends here
