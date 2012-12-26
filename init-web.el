;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
;; (require 'mmm-mode)
(require 'rhtml-mode)
(setq mweb-default-major-mode 'html-erb-mode)

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . multi-web-mode))
(add-hook 'html-mode 'multi-web-mode-major-mode)
                  ;; (php-mode "{{" "}}")

(setq mweb-tags '((php-mode "<\\?php\\|<\\?\\|<\\?=" "\\?>")
                  (ruby-mode "<%\\|<%=\\|<%#" "%>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "erb"))
(multi-web-global-mode)

;; (eval-after-load 'web-mode
;;   '(
;;     (setq web-mode-html-offset 2)
;;     (setq web-mode-css-offset 2)
;;     (setq web-mode-script-offset 2)
;;     (set-face-attribute 'web-mode-css-rule-face nil :foreground "Pink3")
;;     (unless (fboundp 'prog-mode) (defalias 'prog-mode 'fundamental-mode))
;;     ))
;; (defun web-mode-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-html-offset 2)
;;   (setq web-mode-css-offset 2)
;;   (setq web-mode-script-offset 2)
;;   (set-face-attribute 'web-mode-css-rule-face nil :foreground "Pink3")
;;   (local-set-key (kbd "RET") 'newline-and-indent) 
;; )
;; (add-hook 'web-mode-hook  'web-mode-hook)
;; (unless (fboundp 'prog-mode) (defalias 'prog-mode 'fundamental-mode))
;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;(add-hook 'php-mode-hook 'multi-web-mode)

(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)





(provide 'init-web)
