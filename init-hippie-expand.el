(global-set-key (kbd "M-/") 'hippie-expand)

;(autoload 'senator-try-expand-semantic "senator")

(setq hippie-expand-try-functions-list
      '(
 ;       senator-try-expand-semantic
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-visible
        try-complete-file-name-partially
        try-expand-list
        try-expand-list-all-buffers
        try-expand-line
        try-expand-line-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        )
      )


(provide 'init-hippie-expand)
