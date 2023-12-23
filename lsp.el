;;; lsp.el ---  LSP config

;;; Commentary:
;;; LSP setup mostly hooks

;;; Code:
(use-package lsp-mode
  :commands lsp
  :hook ((caml-mode java-mode c-mode) . lsp)
  :custom (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui)

;; Check for errors on the fly
(use-package flycheck :ensure t :init (global-flycheck-mode))

;; C / C++
(use-package ccls
  :hook ((c-mode c++-mode) .
         (lambda ()
           (require 'ccls)
           (lsp)
           (flycheck-mode +1)
           (setq c-basic-offset 4)
           )))

;; Python
(use-package anaconda-mode)
(use-package company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)

;; Java
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
;;(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            ;;(meghanada-mode t)
            (flycheck-mode +1)
            (setq c-basic-offset 4)
            ))
;;(setq meghanada-java-path "java")
;;(setq meghanada-maven-path "mvn")

;;(setq lsp-java-configuration-runtimes '[(:name "openjdk11"
                                               ;; :path "/usr/lib64/openjdk-11")])
                                               ;; :path "/usr/lib/jvm/openjdk-11")])
;; (setq lsp-java-java-path "/usr/lib64/openjdk-11/bin/java")
;; (setq meghanada-java-path "/usr/lib64/openjdk-11/bin/java")

(provide 'lsp)
;;; lsp.el ends here
