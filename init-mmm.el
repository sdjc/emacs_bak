;;----------------------------------------------------------------------------
;; Multiple major modes
;;----------------------------------------------------------------------------
(require 'mmm-mode)
(require 'rhtml-mode)

;(require 'mmm-sample)

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\.\\php\\'" . html-erb-mode))
(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))
(require 'mmm-auto)


(setq mmm-global-mode 'buffers-with-submode-classes)
(setq mmm-submode-decoration-level 2)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\(\\.erb\\)?\\'" 'html-js)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\(\\.erb\\)?\\'" 'html-css)
(mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)


(provide 'init-mmm)
