;;; keybindings.el --- My personnal keybindings for emacs

;;; Commentary:
"Using multistate to define different keymap states aswell as other keybindings"

;;; Code:
;; C-z is bound to suspend-frame by default
;; and I keep hitting it accidentally
(global-unset-key (kbd "C-z"))

(multistate-define-state
 'toplayer)

(multistate-define-state
 'normal
 :lighter "N"
 :parent 'multistate-toplayer-state-map
 :default t)

(multistate-define-state
 'editing
 :lighter "E"
 )

(multistate-define-state
 'vterm
 :lighter "V"
 )

;; (multistate-global-mode 1)
(add-hook 'prog-mode-hook 'multistate-mode)
(add-hook 'dashboard-mode-hook 'multistate-mode)
(add-hook 'vterm-mode 'multistate-mode)

;; Create "toplayer" keymap for keybindings we want to be the same everywhere
(defkey multistate-toplayer-state-map
  "i" 'ibuffer
  ;; Windows
  "x d" 'split-window-right
  "x s" 'split-window-below
  "x z" 'delete-window
  "x h e" 'enlarge-window-horizontally
  "x h s" 'shrink-window-horizontally
  "x e" 'enlarge-window
  "x r" 'other-window
  "C-j" 'windmove-down
  "C-k" 'windmove-up)

;; Have dired modemap inherit from toplayer
;;(set-keymap-parent dired-mode-map multistate-toplayer-state-map)

(defkey multistate-normal-state-map
  ;; Switch state
  "<print>" 'multistate-editing-state
  "v" 'multistate-visual-state
  ;;
  "S" 'save-buffer
  ;; Text
  "w" 'previous-line
  "s" 'next-line
  "d" 'forward-char
  "a" 'backward-char
  "C-a" 'backward-word
  "k" 'previous-line
  "j" 'next-line
  "l" 'forward-char
  "h" 'backward-char
  "C-a" 'backward-word
  "C-d" 'forward-word
  "C-q" 'beginning-of-line
  "C-e" 'end-of-line
  "C-x C-x" 'exchange-point-and-mark
  ;; Dired
  "x f" 'dired
  "x g" 'dired-jump
  ;; Killing
  "z l" 'kill-whole-line
  "z r" 'kill-region
  "z b" 'kill-current-buffer
  "z d" 'kill-dired-buffers
  "z o" 'delete-other-windows
  ;; Editing
  "C" 'kill-ring-save
  "u" 'undo
  "y" 'yank
  "S-<return>" 'crux-smart-open-line
  "C-S-<return>" 'crux-smart-open-line-above
  "D D" 'crux-duplicate-current-line-or-region
  "D c" 'crux-duplicate-and-comment-current-line-or-region
  "," 'comment-or-uncomment-region
  ;; Line
  "L l" 'goto-line
  "L d" 'display-line-numbers-mode
  ;; Eval code
  "e r" 'eval-region
  "e b" 'eval-buffer
  "e R" 'crux-eval-and-replace
  ;; Magit
  "g s" 'magit-status
  ;; ? tools
  "f" 'hs-toggle-hiding
  "c j" 'meghanada-jump-declaration
  "c s" 'isearch-forward
  "M-<return>" 'vterm
  "M-S-<return>" 'xah-open-file-at-cursor
  "C-<delete>" 'sp-delete-word
  ;; Buffers
  "b s" 'switch-to-buffer
  "b k" 'kill-buffer)

(defkey multistate-editing-state-map
  ;; Switch state
  "<print>" 'multistate-normal-state)

(add-hook 'vterm-mode-hook
	  (lambda () (local-set-key (kbd "<print>") 'other-window)))

(global-set-key (kbd "C-<tab>") 'centaur-tabs-forward)
(global-set-key (kbd "M-<tab>") 'centaur-tabs-forward-group)
(global-set-key (kbd "C-c C-s") 'scrot-take-screenshot)

(require 'lsp-java)
(define-key java-mode-map (kbd "C-c C-t") 'compile-and-run-test-java)
(require 'caml)
(define-key caml-mode-map (kbd "C-c C-e") 'tuareg-run-ocaml)
(require 'utop)
(define-key utop-mode-map (kbd "C-w") 'utop-history-goto-prev)
(define-key utop-mode-map (kbd "C-s") 'utop-history-goto-next)

;; (define-prefix-command 'ring-map)
;; (global-set-key (kbd "C-x") 'ring-map)
;; (global-unset-key (kbd "C-c"))
;; (global-set-key (kbd "C-c") 'ring-map)

(provide 'keybindings)
;;; keybindings.el ends here
