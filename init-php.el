;; (add-auto-mode 'php-mode "\\.php[345]?\\'\\|\\.phtml\\." "\\.(inc|tpl)$" "\\.module$")
(add-hook 'php-mode-hook 'web-mode)
;(setq mweb-default-major-mode 'html-erb-mode)
(autoload 'smarty-mode "smarty-mode" "Smarty Mode" t)
(add-auto-mode 'smarty-mode "\\.tpl$")

(defun my-html-mode-hook ()
  (interactive)
  (require 'find-file)
  (add-to-list 'ff-other-file-alist
	       '("\\.html\\'" (".js")))

  ;; multi-web-mode, auto select major mode for HTML files when point is moving
  ;; around.
  ;; using local copy at ~/.emacs.d/site-lisp/multi-web-mode.el
  ;; age: April 09, 2010
  ;; check updates at https://github.com/fgallina/multi-web-mode
  )
; (add-hook 'html-mode-hook 'my-html-mode-hook)
(add-hook 'php-mode-hook 'enable-paredit-mode)
;(define-key php-mode-map  "\C-c\C-c" 'comment-or-uncomment-region)
;(define-key php-mode-map  "\C-c\C-l" 'comment-uncomment-region-or-line)


(provide 'init-php)
