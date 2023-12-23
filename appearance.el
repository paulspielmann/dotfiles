;;; appearance.el --- Appearance tweaks
;;; Commentary:
;;; Code:
(setq inhibit-startup-message nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(column-number-mode 1)      ; Show column numberq
(menu-bar-mode -1)            ; Disable the menu bar

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Fancy themes
(use-package doom-themes :defer t)
(load-theme 'doom-catppuccin t)
(doom-themes-visual-bell-config)

;; Catppuccin settings
;;(use-package catppuccin)
(setq catppuccin-flavor 'mocha) ;; This can be latte, frappe, macchiato, mocha

;; Make your system theme match emacs theme
(require 'theme-magic)
(theme-magic-export-theme-mode)

;; Fancy font
(set-frame-font "MonoLisa 12")

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; Fancy modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 2)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name t)
  (doom-modeline-modal-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon t))

(doom-modeline-def-segment "State")

;; Fancy ligatures
;;(load-file "~/.emacs.d/elpa/ligature/ligature.el")
(ligature-set-ligatures 'prog-mode '("-->" "->" "->>" "-<" "--<"
                                       "-~" "]#" ".-" "!=" "!=="
                                       "#(" "#{" "#[" "#_" "#_("
                                       "/=" "/==" "|||" "||" ;; "|"
                                       "==" "===" "==>" "=>" "=>>"
                                       "=<<" "=/" ">-" ">->" ">="
                                       ">=>" "<-" "<--" "<->" "<-<"
                                       "<!--" "<|" "<||" "<|||"
                                       "<|>" "<=" "<==" "<==>" "<=>"
                                       "<=<" "<<-" "<<=" "<~" "<~>"
                                       "<~~" "~-" "~@" "~=" "~>"
                                       "~~" "~~>"  ".=" "..=" "---"
                                       "{|" "[|" ".."  "..."  "..<"
                                       ".?"  "::" ":::" "::=" ":="
                                       ":>" ":<" "!!"  "!!."
                                       "!!!"  "?."  "?:" "??"  "?="
                                       "**" "***" "*>" "*/" "#:"
                                       "#!"  "#?"  "##" "###" "####"
                                       "#=" "/*" "/>" "//" "///"
                                       "&&" "|}" "|]" "$>" "++"
                                       "+++" "+>" "=:=" "=!=" ">:"
                                       ">>" ">>>" "<:" "<*" "<*>"
                                       "<$" "<$>" "<+" "<+>" "<>"
                                       "<<" "<<<" "</" "</>" "^="
                                       "%%" "'''" "\"\"\"" ))

(global-ligature-mode t) ;; Show ligatures everywhere
(provide 'appearance.el)
;;; appearance.el ends here
