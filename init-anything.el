;(global-set-key (kbd "M-x") 'anything-M-x)
(global-set-key (kbd "C-x b") 'anything-filelist+)


(defvar anything-c-source-gtags-select
  '((name . "--- GTAGS ---")
    (init
     . (lambda ()
         (call-process-shell-command
          "global -c" nil (list
                         (anything-candidate-buffer 'global)
                         nil))))
    (candidates-in-buffer)
    (action
     ("Goto the location" . (lambda (candidate)
                              (gtags-push-context)
                              (gtags-goto-tag candidate "")))
     ("Goto the location (other-window)" . (lambda (candidate)
                                             (gtags-push-context)
                                             (gtags-goto-tag candidate "" t)))
     ("Move to the referenced point" . (lambda (candidate)
                                         (gtags-push-context)
                                         (gtags-goto-tag candidate "r"))))))

(defvar anything-c-source-gtags-files
  '((name . "--- GTAGS FILES ---")
    (init
     . (lambda ()
         (let ((default-directory
                 (or (gtags-get-rootpath) default-directory)))
           (call-process-shell-command
            "global -Po" nil (list (anything-candidate-buffer 'global) nil)))))
    (candidates-in-buffer)
    (action
     ("Goto the location" . (lambda (candidate)
                              (gtags-push-context)
                              (gtags-goto-tag candidate "Po")))
     ("Goto the location (other-window)" . (lambda (candidate)
                                             (gtags-push-context)
                                             (gtags-goto-tag candidate "Po" t))))))

(defun sl-anything-find-tags ()
  (interactive)
  (anything
   :sources '(anything-c-source-semantic
              anything-c-source-gtags-files
              anything-c-source-gtags-select)
   :input (current-word t)
   :buffer "*Anything Project Files*"))
(defvar anything-c-source-gtags-select
  '((name . "--- GTAGS ---")
    (init
     . (lambda ()
         (call-process-shell-command
          "global -c" nil (list
                           (anything-candidate-buffer 'global)
                           nil))))
    (candidates-in-buffer)
    (action
     ("Goto the location" . (lambda (candidate)
                              (gtags-push-context)
                              (gtags-goto-tag candidate "")))
     ("Goto the location (other-window)" . (lambda (candidate)
                                             (gtags-push-context)
                                             (gtags-goto-tag candidate "" t)))
     ("Move to the referenced point" . (lambda (candidate)
                                         (gtags-push-context)
                                         (gtags-goto-tag candidate "r"))))))

(defvar anything-c-source-gtags-files
  '((name . "--- GTAGS FILES ---")
    (init
     . (lambda ()
         (let ((default-directory
                 (or (gtags-get-rootpath) default-directory)))
           (call-process-shell-command
            "global -Po" nil (list (anything-candidate-buffer 'global) nil)))))
    (candidates-in-buffer)
    (action
     ("Goto the location" . (lambda (candidate)
                              (gtags-push-context)
                              (gtags-goto-tag candidate "Po")))
     ("Goto the location (other-window)" . (lambda (candidate)
                                             (gtags-push-context)
                                             (gtags-goto-tag candidate "Po" t))))))

(defun sl-anything-find-tags ()
  (interactive)
  (anything
   :sources '(anything-c-source-semantic
              anything-c-source-gtags-files
              anything-c-source-gtags-select)
   :input (current-word t)
   :buffer "*Anything Project Files*"))


(provide 'init-anything)
