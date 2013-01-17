;; Currently loading ruby-mode and inf-ruby from the version bundled with rinari
(setq interpreter-mode-alist
      (cons '("ruby" . ruby-mode) interpreter-mode-alist))

(add-auto-mode 'ruby-mode "\\.rb$" "Rakefile$" "\.rake$" "\.rxml$" "\.rjs$" ".irbrc$" "\.builder$" "\.ru$" "\.gemspec$" "Gemfile$" )


(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")

(setq ruby-use-encoding-map nil)

(eval-after-load 'ruby-mode
  '(define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent))


;;----------------------------------------------------------------------------
;; Ruby - flymake
;;----------------------------------------------------------------------------



;;----------------------------------------------------------------------------
;; Ruby - misc
;;----------------------------------------------------------------------------
(setq compile-command "rake ")

(defalias 'ri 'yari)

;(require 'rhtml-erb)
;;----------------------------------------------------------------------------
;; Ruby - erb
;;----------------------------------------------------------------------------
;(add-auto-mode 'html-mode "\.rhtml$" "\.html\.erb$")
(add-auto-mode 'html-erb-mode "\.rhtml$" "\.html\.erb$")

;; (eval-after-load 'mmm-vars
;;   '(progn
;;      (mmm-add-classes
;;       '((eruby :submode ruby-mode :front "<%[#=]?" :back "-?%>"
;;                :match-face (("<%#" . mmm-comment-submode-face)
;;                             ("<%=" . mmm-output-submode-face)
;;                             ("<%"  . mmm-code-submode-face))
;;                :insert ((?% erb-code       nil @ "<%"  @ " " _ " " @ "%>" @)
;;                         (?# erb-comment    nil @ "<%#" @ " " _ " " @ "%>" @)
;;                         (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))))
;;      (dolist (mode (list 'html-mode 'nxml-mode))
;;        (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'eruby))
;;      (mmm-add-mode-ext-class 'yaml-mode "\\.yaml$" 'eruby)
;;      (dolist (mode (list 'js-mode 'js2-mode))
;;        (mmm-add-mode-ext-class mode "\\.js\\.erb$" 'eruby))))


;;
;; (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\(\\.erb\\)?\\'" 'html-js)
;; (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\(\\.erb\\)?\\'" 'html-css)
;; (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
;; (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)

(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . html-erb-mode))
;(add-to-list 'auto-mode-alist '("\\.jst\\.ejs\\'"  . html-erb-mode))



;;----------------------------------------------------------------------------
;; Ruby - my convention for heredocs containing SQL
;;----------------------------------------------------------------------------
;; (eval-after-load 'mmm-mode
;;   '(progn
;;      (mmm-Add-classes
;;       '((ruby-heredoc-sql :submode sql-mode :front "<<-?end_sql.*\r?\n" :back "[ \t]*end_sql" :face mmm-code-submode-face)))
;;      (mmm-add-mode-ext-class 'ruby-mode "\\.rb$" 'ruby-heredoc-sql)))


;;----------------------------------------------------------------------------
;; Ruby - compilation
;;----------------------------------------------------------------------------

; run the current buffer using Shift-F7
(add-hook 'ruby-mode-hook (lambda () (local-set-key [S-f7] 'ruby-compilation-this-buffer)))
; run the current test function using F8 key
(add-hook 'ruby-mode-hook (lambda () (local-set-key [f7] 'ruby-compilation-this-test)))

(add-hook 'ruby-mode-hook (lambda () (local-set-key [f6] 'recompile)))
(add-hook 'ruby-mode-hook
          (lambda ()
             (setq autopair-dont-activate t)
             (ruby-electric-mode t))
          )
(add-hook 'ruby-mode-hook '(lambda ()
                               ;; make ruby-electric play nice with autopair
                               (substitute-key-definition 'ruby-electric-curlies nil ruby-mode-map)
                               (substitute-key-definition 'ruby-electric-matching-char nil ruby-mode-map)
                               (substitute-key-definition 'ruby-electric-close-matching-char nil ruby-mode-map)))


(add-hook 'ruby-mode-hook
           #'(lambda () 
               (setq autopair-dont-activate t) ;; for emacsen < 24
               (autopair-mode -1))             ;; for emacsen >= 24
)


;;----------------------------------------------------------------------------
;; Yaml
;;----------------------------------------------------------------------------
(autoload 'yaml-mode "yaml-mode" "Major mode for YAML source")
(add-auto-mode 'yaml-mode "\\.ya?ml$")

  (eval-after-load 'ruby-mode
    '(progn
       (defun prelude-ruby-mode-defaults ()
         (inf-ruby-setup-keybindings)
         ;; turn off the annoying input echo in irb
         (setq comint-process-echoes t)
         (ruby-block-mode t)
         (ruby-end-mode +1)
         (ruby-tools-mode +1)
         ;; CamelCase aware editing operations
         (subword-mode +1))
       (add-hook 'ruby-mode-hook 'flymake-ruby-load)
       )
    )

(provide 'init-ruby-mode)
