;;; dired-settings.el --- My settings for dired file manager
;;; Commentary:
;;; Code:

(require 'dired)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(setq insert-directory-program "ls")
(setq dired-listing-switches "-ABhg --group-directories-first")

;; Use zqsd to navigate through files in dired
(define-key dired-mode-map "w" 'previous-line)
(define-key dired-mode-map "a" 'dired-up-directory)
(define-key dired-mode-map "s" 'next-line)
(define-key dired-mode-map "d" 'dired-find-file)

(define-key dired-mode-map "y" 'dired-create-directory)
(define-key dired-mode-map "t" 'dired-create-empty-file)
(define-key dired-mode-map "r" 'dired-do-rename)

(define-key dired-mode-map "k" 'kill-current-buffer)

(define-key dired-mode-map "h" 'dired-hide-details-mode)
(define-key dired-mode-map "g" 'dired-hide-dotfiles-mode)

(provide 'dired-settings)
;;; dired-settings.el ends here
