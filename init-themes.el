;;------------------------------------------------------------------------------
;; Old-style color theming support (via color-theme.el)
;;------------------------------------------------------------------------------
(defcustom window-system-color-theme 'color-theme-sanityinc-solarized-dark
  "Color theme to use in window-system frames.
If Emacs' native theme support is available, this setting is
ignored: use `custom-enabled-themes' instead."
  :type 'symbol)

(defcustom tty-color-theme 'color-theme-terminal
  "Color theme to use in TTY frames.
If Emacs' native theme support is available, this setting is
ignored: use `custom-enabled-themes' instead."
  :type 'symbol)

(unless (boundp 'custom-enabled-themes)
  (defun color-theme-terminal ()
    (interactive)
    (color-theme-sanityinc-solarized-dark))

  (defun apply-best-color-theme-for-frame-type (frame)
    (with-selected-frame frame
      (funcall (if window-system
                   window-system-color-theme
                 tty-color-theme))))

  (defun reapply-color-themes ()
    (interactive)
    (mapcar 'apply-best-color-theme-for-frame-type (frame-list)))

  (set-variable 'color-theme-is-global nil)
  (add-hook 'after-make-frame-functions 'apply-best-color-theme-for-frame-type)
  (add-hook 'after-init-hook 'reapply-color-themes)
  (apply-best-color-theme-for-frame-type (selected-frame)))


;;------------------------------------------------------------------------------
;; New-style theme support, in which per-frame theming is not possible
;;------------------------------------------------------------------------------

;; If you don't customize it, this is the theme you get.
                                        ;(setq-default custom-enabled-themes '(sanityinc-solarized-light))
(setq-default custom-enabled-themes '(color-theme-sanityinc-tomorrow-eighties))


;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)
      (message "Consider using 'M-x customize-themes' to save your preferred theme.")))
  (custom-set-variables `(custom-enabled-themes ,custom-enabled-themes)))

                                        ;(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables '(custom-enabled-themes '(sanityinc-solarized-light)))
    (color-theme-sanityinc-solarized-light)))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (if (boundp 'custom-enabled-themes)
      (custom-set-variables '(custom-enabled-themes '(sanityinc-solarized-dark)))
    (color-theme-sanityinc-solarized-dark)))


;(set-frame-parameter (selected-frame) 'alpha (list 85 50)) 
;(add-to-list 'default-frame-alist (cons 'alpha (list 85 50)))
(set-frame-parameter nil 'alpha '(95 80))
(add-to-list 'default-frame-alist (cons 'alpha (list 89 80)))
;; (color-theme-sanityinc-tomorrow-eighties)
(color-theme-sanityinc-solarized-dark)

(provide 'init-themes)
