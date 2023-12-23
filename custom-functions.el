;;; custom-functions.el --- Some custom elisp functions
;;; Commentary:
;;; Code:

(defun kill-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” indicating line number.
If so, jump to that line number.
If path does not have a file extention, try with “.el” for elisp files.
Similar to `find-file-at-point' but without prompting for confirmation."

  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                                ;; chars that are likely to be delimiters of file path or url, e.g. space, tabs, brakets. The colon is a problem. cuz it's in url, but not in file name. Don't want to use just space as delimiter because path or url are often in brackets or quotes as in markdown or html
                                ($pathStops "^  \t\n\"`'‘’“”|()[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
    (if (string-match-p "\\`https?://" $path)
        (if (fboundp 'xahsite-url-to-filepath)
            (progn
              (find-file (xahsite-url-to-filepath $path)))
          (progn (browse-url $path)))
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
            (progn
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char 1)
                      (forward-line (1- $line-num)))
                  (progn
                    (when (y-or-n-p (format "File doesn't exist: 「%s」.  Create?" $fpath))
                      (find-file $fpath))))))
          (progn
            (if (file-exists-p $path)
                (find-file $path)
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "File doesn't exist: 「%s」.  Create?" $path))
                  (find-file $path ))))))))))

;; TODO: Have current file/project name and pass it to our java command
(defun compile-and-run-test-java ()
  "Compile and run a file named 'Test'."
  (interactive)
  (shell-command "javac *.java && java Test"))

(defun scrot-take-screenshot ()
  "Use 'scrot -s' to take a screenshot in a resizable rectangle."
  (interactive)
  (shell-command "cd ~/Pictures && scrot -s"))

(defun switch-between-init-and-init-exwm ()
  "'init.el'  becomes init.el-backup and vice versa."
  (interactive)
  (shell-command "mv init.el tempfile && mv init.el-backup init.el && mv tempfile init.el-backup"))

;; Custom defkey from olivertaylor's config
(defun defkey (map &rest body)
  "Define a key for MAP with specs in BODY.
A custom wrapper around `define-key' that does 2 things:
1. All string defs of a key are wrapped in `kbd'.
2. You can define multiple keys in the style of `setq'."
  (while body
    (let ((key (car body))
          (def (cadr body)))
      (define-key
        map
        (if (stringp key) (kbd key) key)
        def)
      (setq body (cddr body)))))
(provide 'defkey)

;; The key binding technique below is taken from the bind-key package. It
;; places all the bindings I don't want overridden into a minor mode which is
;; inserted into the `emulation-mode-map-alists', so only very few things can
;; override them.

(defvar bosskey-mode-map (make-sparse-keymap))

(define-minor-mode bosskey-mode
  "Minor mode for my personal keybindings, which override others.
The only purpose of this minor mode is to override global keybindings."
  :init-value t
  :global t
  :keymap bosskey-mode-map)

(add-to-list 'emulation-mode-map-alists
             `((bosskey-mode . ,bosskey-mode-map)))

(provide 'custom-functions)
;;; custom-functions.el ends here
