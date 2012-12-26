(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map [f12] 'org-remember)
(setq org-default-notes-file "~/.notes")


(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Various preferences
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-fast-tag-selection-single-key 'expert
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80)
(custom-set-variables 
   '(org-display-custom-times t) 
   '(org-time-stamp-custom-formats (quote ("<%Y年%m月%d日(%A)>" .  
 "<%Y年%m月%d日(%A)%H时%M分>"))) 
 )

; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

(setq org-log-done 'note) ;;完成任务备注
(setq org-log-done 'time) ;;时间戳

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "PROJECT(P@)" "|" "CANCELLED(c@/!)"))))
(setq system-time-locale "C")
;;TODO设置（相当于汉化）
(setq  org-todo-keywords
'((sequence  "计划(t)"  "等待(w@/!)" "开始(s!)" "|" "放弃(c@/!)" "完成(d!)")))
(setq org-archive-tag "资料库"
org-closed-string "任务关闭："
org-comment-string "备注"
org-deadline-string "截止时间："
org-quote-string "引用"
org-scheduled-string "计划时间："
org-display-custom-times  (quote ("<%Y年%m月%d日 %a>" . "<%Y年%m月%d日 %a %H:%M>"))
org-time-stamp-custom-formats (quote ("<%Y年%m月%d日 %a>" . "<%Y年%m月%d日 %a %H:%M>"))
org-todo-interpretation (quote sequence))


(require 'remember)
;; (org-remember-insinuate)
(setq  org-directory  "~/org/")
(setq  org-default-notes-file  (concat  org-directory  "/notes.org"))
(define-key  global-map  "\C-cr"  'org-remember)


(setq  org-remember-templates
'(("工作计划"  ?t  "* 计划 %^{标题} %^g\n       %?     %i\n" "~/org/work.org"  "新任务")
("私人安排"  ?g  "* %^{标题} %^g\n       %?     %i\n   引用自：%a" "~/org/self.org"  "新安排")
("日记"  ?d  "* %u %^{主题}\n  %?\n   %i\n\n     引用自：%a" "~/org/note.org")
("软件"  ?s  "* %^{标题}\n  %?\n   %i\n\n     引用自：%a" "~/org/soft.org"  "新资料")))

;; (setq org-remember-templates
;;       '(("TODO" ?t "* TODO %?\n %x\n %a" "~/org/home.org" "Tasks")
;;         ("IDEA" ?i "* IDEA %?\n %i\n %a" "~/org/home.org" "Idea")
;;  ))


;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persistence-insinuate t)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; ;; Show iCal calendars in the org agenda
;; (when *is-a-mac*
;;   (eval-after-load "org"
;;     '(if *is-a-mac* (require 'org-mac-iCal)))
;;   (setq org-agenda-include-diary t)

;;   (setq org-agenda-custom-commands
;;         '(("I" "Import diary from iCal" agenda ""
;;            ((org-agenda-mode-hook
;;              (lambda ()
;;                (org-mac-iCal)))))))

;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
;;             (lambda ()
;;               (goto-char (point-min))
;;               (save-excursion
;;                 (while (re-search-forward "^[a-z]" nil t)
;;                   (goto-char (match-beginning 0))
;;                   (insert "0:00-24:00 ")))
;;               (while (re-search-forward "^ [a-z]" nil t)
;;                 (goto-char (match-beginning 0))
;;                 (save-excursion
;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;;                 (insert (match-string 0)))))
;;   )


(eval-after-load 'org
  '(progn
     (require 'org-exp)
     (require 'org-clock)
     ;;(require 'org-checklist)
     (require 'org-fstree)))

(add-hook 'org-mode-hook 'inhibit-autopair)

(provide 'init-org)
