;;----------------------------------------------------------------------------
;; Misc config - yet to be placed in separate files
;;----------------------------------------------------------------------------
(add-auto-mode 'tcl-mode "Portfile$")
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'find-file-hooks 'goto-address-prog-mode)
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(setq goto-address-mail-face 'link)

(column-number-mode 1)

(setq-default regex-tool-backend 'perl)
(setq format-title-format "%b")
(setq frame-title-format 
      '((:eval (funcall (lambda () (if buffer-file-name 
                                       buffer-file-name 
                                     (buffer-name))))))) 
(global-auto-revert-mode)

(defadvice sgml-close-tag (after close-tag-then-newline activate)
  (newline-and-indent))
(provide 'init-misc)
