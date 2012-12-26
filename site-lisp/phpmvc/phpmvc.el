;;; phpmvc.el --- Phpmvc Is Not A mvc IDE
;; Copyright (C) 2008 Phil Hagelberg, Eric Schulte

;; Author: Phil Hagelberg, Eric Schulte
;; URL: https://github.com/eschulte/phpmvc
;; Version: DEV
;; Created: 2006-11-10
;; Keywords: ruby, rails, project, convenience, web
;; EmacsWiki: Phpmvc
;; Package-Requires: ((ruby-mode "1.0") (inf-ruby "2.2.1") (ruby-compilation "0.8") (jump "2.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Phpmvc Is Not A php IDE.

;; Well, ok it kind of is.  Phpmvc is a set of Emacs Lisp modes that is
;; aimed towards making Emacs into a top-notch php and Rails
;; development environment.

;; Phpmvc can be installed through ELPA (see http://tromey.com/elpa/)

;; To install from source, copy the directory containing this file
;; into your Emacs Lisp directory, assumed here to be ~/.emacs.d.  Add
;; these lines of code to your .emacs file:

;; ;; phpmvc
;; (add-to-list 'load-path "~/.emacs.d/phpmvc")
;; (require 'phpmvc)

;; Whether installed through ELPA or from source you probably want to
;; add the following lines to your .emacs file:

;; ;; ido
;; (require 'ido)
;; (ido-mode t)

;; Note: if you cloned this from a git repo, you will have to grab the
;; submodules which can be done by running the following commands from
;; the root of the phpmvc directory

;;  git submodule init
;;  git submodule update

;;; Code:
;;;###begin-elpa-ignore
(let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
       (util-dir (file-name-as-directory (expand-file-name "util" this-dir)))
       (inf-ruby-dir (file-name-as-directory (expand-file-name "inf-ruby" util-dir)))
       (jump-dir (file-name-as-directory (expand-file-name "jump" util-dir))))
  (dolist (dir (list util-dir inf-ruby-dir jump-dir))
    (when (file-exists-p dir)
      (add-to-list 'load-path dir))))
;;;###end-elpa-ignore
(require 'php-mode)
(require 'jump)
(require 'cl)

;; fill in some missing variables for XEmacs
(when (featurep 'xemacs)
  ;;this variable does not exist in XEmacs
  (defvar safe-local-variable-values ())
  ;;find-file-hook is not defined and will otherwise not be called by XEmacs
  (define-compatible-variable-alias 'find-file-hook 'find-file-hooks))

(defgroup phpmvc nil
  "Phpmvc customizations."
  :prefix "phpmvc-"
  :group 'phpmvc)

(defcustom phpmvc-tags-file-name
  "TAGS"
  "Path to your TAGS file inside of your rails project.  See `tags-file-name'."
  :group 'phpmvc)

(defcustom phpmvc-fontify-rails-keywords t
  "When non-nil, fontify keywords such as 'before_filter', 'url_for'.")

(defcustom phpmvc-controller-keywords
  '("logger" "polymorphic_path" "polymorphic_url" "mail" "render" "attachments"
    "default" "helper" "helper_attr" "helper_method" "layout" "url_for"
    "serialize" "exempt_from_layout" "filter_parameter_logging" "hide_action"
    "cache_sweeper" "protect_from_forgery" "caches_page" "cache_page"
    "caches_action" "expire_page" "expire_action" "rescue_from" "params"
    "request" "response" "session" "flash" "head" "redirect_to"
    "render_to_string" "respond_with" "before_filter" "append_before_filter"
    "prepend_before_filter" "after_filter" "append_after_filter"
    "prepend_after_filter" "around_filter" "append_around_filter"
    "prepend_around_filter" "skip_before_filter" "skip_after_filter" "skip_filter")
  "List of keywords to highlight for controllers"
  :group 'phpmvc
  :type '(repeat string))

(defcustom phpmvc-migration-keywords
  '("create_table" "change_table" "drop_table" "rename_table" "add_column"
    "rename_column" "change_column" "change_column_default" "remove_column"
    "add_index" "remove_index" "rename_index" "execute")
  "List of keywords to highlight for migrations"
  :group 'phpmvc
  :type '(repeat string))

(defcustom phpmvc-model-keywords
  '("default_scope" "named_scope" "scope" "serialize" "belongs_to" "has_one"
    "has_many" "has_and_belongs_to_many" "composed_of" "accepts_nested_attributes_for"
    "before_create" "before_destroy" "before_save" "before_update" "before_validation"
    "before_validation_on_create" "before_validation_on_update" "after_create"
    "after_destroy" "after_save" "after_update" "after_validation"
    "after_validation_on_create" "after_validation_on_update" "around_create"
    "around_destroy" "around_save" "around_update" "after_commit" "after_find"
    "after_initialize" "after_rollback" "after_touch" "attr_accessible"
    "attr_protected" "attr_readonly" "validates" "validate" "validate_on_create"
    "validate_on_update" "validates_acceptance_of" "validates_associated"
    "validates_confirmation_of" "validates_each" "validates_exclusion_of"
    "validates_format_of" "validates_inclusion_of" "validates_length_of"
    "validates_numericality_of" "validates_presence_of" "validates_size_of"
    "validates_uniqueness_of" "validates_with")
  "List of keywords to highlight for models"
  :group 'phpmvc
  :type '(repeat string))

(defvar phpmvc-minor-mode-hook nil
  "*Hook for customising Phpmvc.")

(defcustom phpmvc-phpmvc-env nil
  "Use this to force a value for PHPMVC_ENV when running phpmvc.
Leave this set to nil to not force any value for PHPMVC_ENV, and
leave this to the environment variables outside of Emacs.")

(defvar phpmvc-minor-mode-prefixes
  (list ";" "'")
  "List of characters, each of which will be bound (with control-c) as a prefix for `phpmvc-minor-mode-map'.")

;; (defcustom phpmvc-inf-ruby-prompt-pattern
;;   "\\(^>> .*\\)\\|\\(^\\(irb([^)]+)\\|\\(\[[0-9]+\] \\)?[Pp]ry ?([^)]+)\\|\\(jruby-\\|JRUBY-\\)?[1-9]\\.[0-9]\\.[0-9]+\\(-?p?[0-9]+\\)?\\) ?\\(:[0-9]+\\)* ?[\]>*\"'/`]>? *\\)"
;;   "The value used for `inf-ruby-prompt-pattern' in `phpmvc-console' buffers."
;;   :group 'phpmvc)

(defvar phpmvc-partial-regex
  "render \\(:partial *=> \\)?*[@'\"]?\\([A-Za-z/_]+\\)['\"]?"
  "Regex that matches a partial rendering call.")

(defadvice ruby-compilation-do (around phpmvc-compilation-do activate)
  "Set default directory to the phpmvc root before running ruby processes."
  (let ((default-directory (or (phpmvc-root) default-directory)))
    ad-do-it
    (phpmvc-launch)))

(defadvice ruby-compilation-rake (around phpmvc-compilation-rake activate)
  "Set default directory to the phpmvc root before running rake processes."
  (let ((default-directory (or (phpmvc-root) default-directory)))
    ad-do-it
    (phpmvc-launch)))

(defadvice ruby-compilation-cap (around phpmvc-compilation-cap activate)
  "Set default directory to the phpmvc root before running cap processes."
  (let ((default-directory (or (phpmvc-root) default-directory)))
    ad-do-it
    (phpmvc-launch)))

(defun phpmvc-parse-yaml ()
  "Parse key/value pairs out of a simple yaml file."
  (let ((start (point))
        (end (save-excursion (re-search-forward "^[^:]*$" nil t) (point)))
        alist)
    (while (and (< (point) end)
                (re-search-forward "^ *\\(.*\\): \\(.*\\)$" nil t))
      (setf alist (cons (cons (match-string 1) (match-string 2)) alist)))
    alist))

(defun phpmvc-root (&optional dir home)
  "Return the root directory of the project within which DIR is found.
Optional argument HOME is ignored."
  (let ((default-directory (or dir default-directory)))
    (if (file-exists-p (expand-file-name "artisan" (expand-file-name "start.php")))
        default-directory
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:/?$\\|^/$\\)" default-directory)
        (phpmvc-root (expand-file-name (file-name-as-directory "..")))))))

(defun phpmvc-highlight-keywords (keywords)
  "Highlight the passed KEYWORDS in current buffer.
Use `font-lock-add-keywords' in case of `ruby-mode' or
`ruby-extra-keywords' in case of Enhanced php Mode."
  (if (boundp 'ruby-extra-keywords)
      (progn
	(setq ruby-extra-keywords (append ruby-extra-keywords keywords))
	(ruby-local-enable-extra-keywords))
    (font-lock-add-keywords
     nil
     (list (list
            (concat "\\(^\\|[^_:.@$]\\|\\.\\.\\)\\b"
		    (regexp-opt keywords t)
		    ruby-keyword-end-re)
            (list 2 'font-lock-builtin-face))))))

(defun phpmvc-apply-keywords-for-file-type ()
  "Apply extra font lock keywords specific to models, controllers etc."
  (when (and phpmvc-fontify-phpmvc-keywords (buffer-file-name))
    (loop for (re keywords) in `(("_controller\\.rb$"   ,phpmvc-controller-keywords)
                                 ("app/models/.+\\.rb$" ,phpmvc-model-keywords)
                                 ("db/migrate/.+\\.rb$" ,phpmvc-migration-keywords))
          do (when (string-match-p re (buffer-file-name))
               (phpmvc-highlight-keywords keywords)))))

(add-hook 'phpmvc-minor-mode-hook 'phpmvc-apply-keywords-for-file-type)

;;--------------------------------------------------------------------------------
;; user functions

(defun phpmvc-rake (&optional task edit-cmd-args)
  "Select and run a rake TASK using `ruby-compilation-rake'."
  (interactive "P")
  (ruby-compilation-rake task edit-cmd-args
                         (if phpmvc-phpmvc-env (list (cons "PHPMVC_ENV" phpmvc-phpmvc-env)))))

(defun phpmvc-cap (&optional task edit-cmd-args)
  "Select and run a capistrano TASK using `ruby-compilation-cap'."
  (interactive "P")
  (ruby-compilation-cap task edit-cmd-args
                        (if phpmvc-phpmvc-env (list (cons "PHPMVC_ENV" phpmvc-phpmvc-env)))))

(defun phpmvc--discover-phpmvc-commands ()
  "Return a list of commands supported by the main phpmvc script."
  (let ((phpmvc-script (phpmvc--phpmvc-path)))
    (when phpmvc-script
      (ruby-compilation-extract-output-matches phpmvc-script "^ \\([a-z]+\\)[[:space:]].*$"))))

(defvar phpmvc-phpmvc-commands-cache nil
  "Cached values for commands that can be used with 'script/phpmvc' in Phpmvc 3.")

(defun phpmvc-get-phpmvc-commands ()
  "Return a cached list of commands supported by the main phpmvc script."
  (if (null phpmvc-phpmvc-commands-cache)
      (setq phpmvc-phpmvc-commands-cache (phpmvc--discover-phpmvc-commands)))
  phpmvc-phpmvc-commands-cache)

(defun phpmvc-script (&optional script)
  "Select and run SCRIPT from the script/ directory of the phpmvc application."
  (interactive)
  (let* ((completions (append (directory-files (phpmvc-script-path) nil "^[^.]")
                              (phpmvc-get-phpmvc-commands)))
         (script (or script (jump-completing-read "Script: " completions)))
         (ruby-compilation-error-regexp-alist ;; for jumping to newly created files
          (if (equal script "generate")
              '(("^ +\\(exists\\|create\\) +\\([^[:space:]]+\\.rb\\)" 2 3))
            ruby-compilation-error-regexp-alist))
         (script (concat (phpmvc--wrap-phpmvc-command script) " ")))
    (ruby-compilation-run (concat script (read-from-minibuffer script)))))

(defun phpmvc-test (&optional edit-cmd-args)
  "Run the current ruby function as a test, or run the corresponding test.
If current function is not a test,`phpmvc-find-test' is used to
find the corresponding test.  Output is sent to a compilation buffer
allowing jumping between errors and source code.  Optional prefix
argument EDIT-CMD-ARGS lets the user edit the test command
arguments."
  (interactive "P")
  (or (phpmvc-test-function-name)
      (string-match "test" (or (ruby-add-log-current-method)
                               (file-name-nondirectory (buffer-file-name))))
      (phpmvc-find-test))
  (let* ((fn (phpmvc-test-function-name))
         (path (buffer-file-name))
         (ruby-options (list "-I" (expand-file-name "test" (phpmvc-root)) path))
         (default-command (mapconcat
                           'identity
                           (append (list path) (if fn (list "--name" (concat "/" fn "/"))))
                           " "))
         (command (if edit-cmd-args
                      (read-string "Run w/Compilation: " default-command)
                    default-command)))
    (if path (ruby-compilation-run command ruby-options)
      (message "no test available"))))

(defun phpmvc-test-function-name()
  "Return the name of the test function at point, or nil if not found."
  (save-excursion
    (if (re-search-backward (concat "^[ \t]*\\(def\\|test\\)[ \t]+"
                                    "\\([\"'].*?[\"']\\|" ruby-symbol-re "*\\)"
                                    "[ \t]*") nil t)
        (let ((name (match-string 2)))
          (if (string-match "^[\"']\\(.*\\)[\"']$" name)
              (replace-regexp-in-string
               "\\?" "\\\\\\\\?"
               (replace-regexp-in-string " +" "_" (match-string 1 name)))
            (if (string-match "^test" name)
              name))))))

(defun phpmvc--phpmvc-path ()
  "Return the path of the 'phpmvc' command, or nil if not found."
  (let* ((script (phpmvc-script-path))
         (phpmvc-script (expand-file-name "phpmvc" script)))
    (if (file-exists-p phpmvc-script)
        phpmvc-script
      (executable-find "phpmvc"))))

(defun phpmvc--wrap-phpmvc-command (command)
  "Given a COMMAND such as 'console', return a suitable command line.
Where the corresponding script is executable, it will be run
as-is.  Otherwise, as can be the case on Windows, the command will
be prepended with `ruby-compilation-executable'."
  (let* ((default-directory (phpmvc-root))
         (script (phpmvc-script-path))
         (script-command (expand-file-name command script))
         (command-line
          (if (file-exists-p script-command)
              script-command
            (concat (phpmvc--phpmvc-path) " " command))))
    (if (file-executable-p (first (split-string-and-unquote command-line)))
        command-line
      (concat ruby-compilation-executable " " command-line))))

;; (defun phpmvc-console (&optional edit-cmd-args)
;;   "Run a Phpmvc console in a compilation buffer.
;; The buffer will support command history and links between errors
;; and source code.  Optional prefix argument EDIT-CMD-ARGS lets the
;; user edit the console command arguments."
;;   (interactive "P")
;;   (let* ((default-directory (phpmvc-root))
;;          (command (phpmvc--wrap-phpmvc-command "console")))

;;     ;; Start console in correct environment.
;;     (when phpmvc-phpmvc-env
;;       (setq command (concat command " " phpmvc-phpmvc-env)))

;;     ;; For customization of the console command with prefix arg.
;;     (setq command (if edit-cmd-args
;;                       (read-string "Run php: " (concat command " "))
;;                     command))
;;     (with-current-buffer (run-ruby command "phpmvc console")
;;       (dolist (var '(inf-ruby-prompt-pattern inf-ruby-first-prompt-pattern))
;;         (set (make-local-variable var) phpmvc-inf-ruby-prompt-pattern))
;;       (phpmvc-launch))))

(defun phpmvc-sql-buffer-name (env)
  "Return the name of the sql buffer for ENV."
  (format "*%s-sql*" env))

(defun phpmvc-sql ()
  "Browse the application's database.
Looks up login information from your conf/database.sql file."
  (interactive)
  (let* ((environment (or phpmvc-phpmvc-env (getenv "PHPMVC_ENV") "development"))
         (sql-buffer (get-buffer (phpmvc-sql-buffer-name environment))))
    (if sql-buffer
        (pop-to-buffer sql-buffer)
      (let* ((database-alist (save-excursion
                               (with-temp-buffer
                                 (insert-file-contents
                                  (expand-file-name
                                   "database.yml"
                                   (file-name-as-directory
                                    (expand-file-name "config" (phpmvc-root)))))
                                 (goto-char (point-min))
                                 (re-search-forward (concat "^" environment ":"))
                                 (phpmvc-parse-yaml))))
             (adapter (or (cdr (assoc "adapter" database-alist)) "sqlite"))
             (sql-user (or (cdr (assoc "username" database-alist)) "root"))
             (sql-password (or (cdr (assoc "password" database-alist)) ""))
             (sql-password (if (> (length sql-password) 0) sql-password nil))
             (sql-database (or (cdr (assoc "database" database-alist))
                               (concat (file-name-nondirectory (phpmvc-root))
                                       "_" environment)))
             (server (or (cdr (assoc "host" database-alist)) "localhost"))
             (port (cdr (assoc "port" database-alist)))
             (sql-server (if port (concat server ":" port) server)))
        (cond ((string-match "mysql" adapter)
               (setf adapter "mysql"))
              ((string-match "sqlite" adapter)
               (setf adapter "sqlite"))
              ((string-match "postgresql" adapter)
               (setf adapter "postgres")))
        (eval (list (intern (concat "sql-" adapter))))
        (rename-buffer sql-buffer)
        (phpmvc-launch)))))

(defun phpmvc-web-server (&optional edit-cmd-args)
  "Start a Phpmvc webserver.
Dumps output to a compilation buffer allowing jumping between
errors and source code.  Optional prefix argument EDIT-CMD-ARGS
lets the user edit the server command arguments."
  (interactive "P")
  (let* ((default-directory (phpmvc-root))
         (command (phpmvc--wrap-phpmvc-command "server")))

    ;; Start web server in correct environment.
    (when phpmvc-phpmvc-env
      (setq command (concat command " -e " phpmvc-phpmvc-env)))

    ;; For customization of the web server command with prefix arg.
    (setq command (if edit-cmd-args
                      (read-string "Run php: " (concat command " "))
                    command))

    (ruby-compilation-run command nil "server"))
  (phpmvc-launch))

(defun phpmvc-web-server-restart (&optional edit-cmd-args)
  "Ensure a fresh `phpmvc-web-server' is running, first killing any old one.
Optional prefix argument EDIT-CMD-ARGS lets the user edit the
server command arguments."
  (interactive "P")
  (let ((phpmvc-web-server-buffer "*server*"))
    (when (get-buffer phpmvc-web-server-buffer)
      (set-process-query-on-exit-flag (get-buffer-process phpmvc-web-server-buffer) nil)
      (kill-buffer phpmvc-web-server-buffer))
    (phpmvc-web-server edit-cmd-args)))

(defun phpmvc-insert-erb-skeleton (no-equals)
  "Insert an erb skeleton at point.
With optional prefix argument NO-EQUALS, don't include an '='."
  (interactive "P")
  (insert "<%") (if no-equals (insert "  -") (insert "=  ")) (insert "%>")
  (if no-equals (backward-char 4) (backward-char 3)))

(defun phpmvc-extract-partial (begin end partial-name)
  "Extracts the region from BEGIN to END into a partial called PARTIAL-NAME."
  (interactive "r\nsName your partial: ")
  (let ((path (buffer-file-name))
        (ending (phpmvc-ending)))
    (if (string-match "view" path)
        (let ((partial-name
               (replace-regexp-in-string "[[:space:]]+" "_" partial-name)))
          (kill-region begin end)
          (if (string-match "\\(.+\\)/\\(.+\\)" partial-name)
              (let ((default-directory (expand-file-name (match-string 1 partial-name)
                                                         (expand-file-name ".."))))
                (find-file (concat "_" (match-string 2 partial-name) ending)))
            (find-file (concat "_" partial-name ending)))
          (yank) (pop-to-buffer nil)
          (phpmvc-insert-partial partial-name ending))
      (message "not in a view"))))

(defun phpmvc-insert-partial (partial-name ending)
  "Insert a call to PARTIAL-NAME, formatted for the file's ENDING.

Supported markup languages are: Erb, Haml"
  (let ((prefix) (suffix))
    (cond
     ((string-match "\\(html\\)?\\.erb" ending)
      (setq prefix "<%= ")
      (setq suffix " %>"))
     ((string-match "\\(html\\)?\\.haml" ending)
      (setq prefix "= ")
      (setq suffix " ")))
    (insert (concat prefix "render :partial => \"" partial-name "\"" suffix "\n"))))

(defun phpmvc-goto-partial ()
  "Visits the partial that is called on the current line."
  (interactive)
  (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (when (string-match phpmvc-partial-regex line)
      (setq line (match-string 2 line))
      (let ((file))
        (if (string-match "/" line)
            (setq file (concat (phpmvc-root) "app/views/" (replace-regexp-in-string "\\([^/]+\\)/\\([^/]+\\)$" "\\1/_\\2" line)))
          (setq file (concat default-directory "_" line)))
        (find-file (concat file (phpmvc-ending)))))))

(defvar phpmvc-rgrep-file-endings
  "*.[^l]*"
  "Ending of files to search for matches using `phpmvc-rgrep'.")

(defun phpmvc-rgrep (&optional arg)
  "Search through the phpmvc project for a string or `regexp'.
With optional prefix argument ARG, just run `rgrep'."
  (interactive "P")
  (grep-compute-defaults)
  (if arg (call-interactively 'rgrep)
    (let ((query))
      (if mark-active
          (setq query (buffer-substring-no-properties (point) (mark)))
        (setq query (thing-at-point 'word)))
      (funcall 'rgrep (read-from-minibuffer "search for: " query)
               phpmvc-rgrep-file-endings (phpmvc-root)))))

(defun phpmvc-ending ()
  "Return the file extension of the current file."
  (let* ((path (buffer-file-name))
         (ending
          (and (string-match ".+?\\(\\.[^/]*\\)$" path)
               (match-string 1 path))))
    ending))

(defun phpmvc-script-path ()
  "Return the absolute path to the script folder."
  (concat (file-name-as-directory (expand-file-name "script" (phpmvc-root)))))

;;--------------------------------------------------------------------
;; phpmvc movement using jump.el

(defun phpmvc-generate (type name)
  "Run the generate command to generate a TYPE called NAME."
  (let* ((default-directory (phpmvc-root))
         (command (phpmvc--wrap-phpmvc-command "generate")))
    (message (shell-command-to-string (concat command " " type " " (read-from-minibuffer (format "create %s: " type) name))))))

(defvar phpmvc-ruby-hash-regexp
  "\\(:[^[:space:]]*?\\)[[:space:]]*\\(=>[[:space:]]*[\"\':]?\\([^[:space:]]*?\\)[\"\']?[[:space:]]*\\)?[,){}\n]"
  "Regexp to match subsequent key => value pairs of a ruby hash.")

(defun phpmvc-ruby-values-from-render (controller action)
  "Return (CONTROLLER . ACTION) after adjusting for the hash values at point."
  (let ((end (save-excursion
               (re-search-forward "[^,{(]$" nil t)
               (+ 1 (point)))))
    (save-excursion
      (while (and (< (point) end)
                  (re-search-forward phpmvc-ruby-hash-regexp end t))
        (if (> (length (match-string 3)) 1)
            (case (intern (match-string 1))
              (:partial
               (let ((partial (match-string 3)))
                 (if (string-match "\\(.+\\)/\\(.+\\)" partial)
                     (progn
                       (setf controller (match-string 1 partial))
                       (setf action (concat "_" (match-string 2 partial))))
                   (setf action (concat "_" partial)))))
              (:action  (setf action (match-string 3)))
              (:controller (setf controller (match-string 3)))))))
    (cons controller action)))

(defun phpmvc-which-render (renders)
  "Select and parse one of the RENDERS supplied."
  (let ((path (jump-completing-read
               "Follow: "
               (mapcar (lambda (lis)
                         (concat (car lis) "/" (cdr lis)))
                       renders))))
    (string-match "\\(.*\\)/\\(.*\\)" path)
    (cons (match-string 1 path) (match-string 2 path))))

(defun phpmvc-follow-controller-and-action (controller action)
  "Follow CONTROLLER and ACTION through to the final controller or view.
The user is prompted to follow through any intermediate renders
and redirects."
  (save-excursion ;; if we can find the controller#action pair
    (if (and (jump-to-path (format "app/controllers/%s_controller.rb#%s" controller action))
             (equalp (jump-method) action))
        (let ((start (point)) ;; demarcate the borders
              (renders (list (cons controller action))) render view)
          (ruby-forward-sexp)
          ;; collect redirection options and pursue
          (while (re-search-backward "re\\(?:direct_to\\|nder\\)" start t)
            (add-to-list 'renders (phpmvc-ruby-values-from-render controller action)))
          (let ((render (if (equalp 1 (length renders))
                            (car renders)
                          (phpmvc-which-render renders))))
            (if (and (equalp (cdr render) action)
                     (equalp (car render) controller))
                (list controller action) ;; directed to here so return
              (phpmvc-follow-controller-and-action (or (car render)
                                                       controller)
                                                   (or (cdr render)
                                                       action)))))
      ;; no controller entry so return
      (list controller action))))

(defvar phpmvc-jump-schema
 '((model
    "m"
    (("app/controllers/\\1_controller.rb#\\2$" . "app/models/\\1.rb#\\2")
     ("app/views/\\1/.*"                       . "app/models/\\1.rb")
     ("app/helpers/\\1_helper.rb"              . "app/models/\\1.rb")
     ("db/migrate/.*create_\\1.rb"             . "app/models/\\1.rb")
     ("spec/models/\\1_spec.rb"                . "app/models/\\1.rb")
     ("spec/controllers/\\1_controller_spec.rb". "app/models/\\1.rb")
     ("spec/views/\\1/.*"                      . "app/models/\\1.rb")
     ("spec/fixtures/\\1.yml"                  . "app/models/\\1.rb")
     ("test/functional/\\1_controller_test.rb" . "app/models/\\1.rb")
     ("test/unit/\\1_test.rb#test_\\2$"        . "app/models/\\1.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "app/models/\\1.rb")
     ("test/fixtures/\\1.yml"                  . "app/models/\\1.rb")
     (t                                        . "app/models/"))
    (lambda (path)
      (phpmvc-generate "model"
                       (and (string-match ".*/\\(.+?\\)\.rb" path)
                            (match-string 1 path)))))
   (controller
    "c"
    (("app/models/\\1.rb"                      . "app/controllers/\\1_controller.rb")
     ("app/views/\\1/\\2\\..*"                 . "app/controllers/\\1_controller.rb#\\2")
     ("app/helpers/\\1_helper.rb"              . "app/controllers/\\1_controller.rb")
     ("db/migrate/.*create_\\1.rb"             . "app/controllers/\\1_controller.rb")
     ("spec/models/\\1_spec.rb"                . "app/controllers/\\1_controller.rb")
     ("spec/controllers/\\1_spec.rb"           . "app/controllers/\\1.rb")
     ("spec/views/\\1/\\2\\.*_spec.rb"         . "app/controllers/\\1_controller.rb#\\2")
     ("spec/fixtures/\\1.yml"                  . "app/controllers/\\1_controller.rb")
     ("test/functional/\\1_test.rb#test_\\2$"  . "app/controllers/\\1.rb#\\2")
     ("test/functional/\\1_test.rb"            . "app/controllers/\\1.rb")
     ("test/unit/\\1_test.rb#test_\\2$"        . "app/controllers/\\1_controller.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "app/controllers/\\1_controller.rb")
     ("test/fixtures/\\1.yml"                  . "app/controllers/\\1_controller.rb")
     (t                                        . "app/controllers/"))
    (lambda (path)
      (phpmvc-generate "controller"
                       (and (string-match ".*/\\(.+?\\)_controller\.rb" path)
                            (match-string 1 path)))))
   (view
    "v"
    (("app/models/\\1.rb"                      . "app/views/\\1/.*")
     ((lambda () ;; find the controller/view
        (let* ((raw-file (and (buffer-file-name)
                              (file-name-nondirectory (buffer-file-name))))
               (file (and raw-file
                          (string-match "^\\(.*\\)_controller.rb" raw-file)
                          (match-string 1 raw-file))) ;; controller
               (raw-method (ruby-add-log-current-method))
               (method (and file raw-method ;; action
                            (string-match "#\\(.*\\)" raw-method)
                            (match-string 1 raw-method))))
          (if (and file method) (phpmvc-follow-controller-and-action file method))))
      . "app/views/\\1/\\2.*")
     ("app/controllers/\\1_controller.rb"      . "app/views/\\1/.*")
     ("app/helpers/\\1_helper.rb"              . "app/views/\\1/.*")
     ("db/migrate/.*create_\\1.rb"             . "app/views/\\1/.*")
     ("spec/models/\\1_spec.rb"                . "app/views/\\1/.*")
     ("spec/controllers/\\1_spec.rb"           . "app/views/\\1/.*")
     ("spec/views/\\1/\\2_spec.rb"             . "app/views/\\1/\\2.*")
     ("spec/fixtures/\\1.yml"                  . "app/views/\\1/.*")
     ("test/functional/\\1_controller_test.rb" . "app/views/\\1/.*")
     ("test/unit/\\1_test.rb#test_\\2$"        . "app/views/\\1/_?\\2.*")
     ("test/fixtures/\\1.yml"                  . "app/views/\\1/.*")
     (t                                        . "app/views/.*"))
    t)
   (test
    "t"
    (("app/models/\\1.rb#\\2$"                 . "test/unit/\\1_test.rb#test_\\2")
     ("app/controllers/\\1.rb#\\2$"            . "test/functional/\\1_test.rb#test_\\2")
     ("app/views/\\1/_?\\2\\..*"               . "test/functional/\\1_controller_test.rb#test_\\2")
     ("app/helpers/\\1_helper.rb"              . "test/functional/\\1_controller_test.rb")
     ("db/migrate/.*create_\\1.rb"             . "test/unit/\\1_test.rb")
     ("test/functional/\\1_controller_test.rb" . "test/unit/\\1_test.rb")
     ("test/unit/\\1_test.rb"                  . "test/functional/\\1_controller_test.rb")
     (t                                        . "test/.*"))
    t)
   (rspec
    "r"
    (("app/\\1\\.rb"                           . "spec/\\1_spec.rb")
     ("app/\\1$"                               . "spec/\\1_spec.rb")
     ("spec/views/\\1_spec.rb"                 . "app/views/\\1")
     ("spec/\\1_spec.rb"                       . "app/\\1.rb")
     (t                                        . "spec/.*"))
    t)
   (fixture
    "x"
    (("app/models/\\1.rb"                      . "test/fixtures/\\1.yml")
     ("app/controllers/\\1_controller.rb"      . "test/fixtures/\\1.yml")
     ("app/views/\\1/.*"                       . "test/fixtures/\\1.yml")
     ("app/helpers/\\1_helper.rb"              . "test/fixtures/\\1.yml")
     ("db/migrate/.*create_\\1.rb"             . "test/fixtures/\\1.yml")
     ("spec/models/\\1_spec.rb"                . "test/fixtures/\\1.yml")
     ("spec/controllers/\\1_controller_spec.rb". "test/fixtures/\\1.yml")
     ("spec/views/\\1/.*"                      . "test/fixtures/\\1.yml")
     ("test/functional/\\1_controller_test.rb" . "test/fixtures/\\1.yml")
     ("test/unit/\\1_test.rb"                  . "test/fixtures/\\1.yml")
     (t                                        . "test/fixtures/"))
    t)
   (rspec-fixture
    "z"
    (("app/models/\\1.rb"                      . "spec/fixtures/\\1.yml")
     ("app/controllers/\\1_controller.rb"      . "spec/fixtures/\\1.yml")
     ("app/views/\\1/.*"                       . "spec/fixtures/\\1.yml")
     ("app/helpers/\\1_helper.rb"              . "spec/fixtures/\\1.yml")
     ("db/migrate/.*create_\\1.rb"             . "spec/fixtures/\\1.yml")
     ("spec/models/\\1_spec.rb"                . "spec/fixtures/\\1.yml")
     ("spec/controllers/\\1_controller_spec.rb". "spec/fixtures/\\1.yml")
     ("spec/views/\\1/.*"                      . "spec/fixtures/\\1.yml")
     ("test/functional/\\1_controller_test.rb" . "spec/fixtures/\\1.yml")
     ("test/unit/\\1_test.rb"                  . "spec/fixtures/\\1.yml")
     (t                                        . "spec/fixtures/"))
    t)
   (helper
    "h"
    (("app/models/\\1.rb"                      . "app/helpers/\\1_helper.rb")
     ("app/controllers/\\1_controller.rb"      . "app/helpers/\\1_helper.rb")
     ("app/views/\\1/.*"                       . "app/helpers/\\1_helper.rb")
     ("app/helpers/\\1_helper.rb"              . "app/helpers/\\1_helper.rb")
     ("db/migrate/.*create_\\1.rb"             . "app/helpers/\\1_helper.rb")
     ("spec/models/\\1_spec.rb"                . "app/helpers/\\1_helper.rb")
     ("spec/controllers/\\1_spec.rb"           . "app/helpers/\\1_helper.rb")
     ("spec/views/\\1/.*"                      . "app/helpers/\\1_helper.rb")
     ("test/functional/\\1_controller_test.rb" . "app/helpers/\\1_helper.rb")
     ("test/unit/\\1_test.rb#test_\\2$"        . "app/helpers/\\1_helper.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "app/helpers/\\1_helper.rb")
     (t                                        . "app/helpers/"))
    t)
   (migration
    "i"
    (("app/controllers/\\1_controller.rb"      . "db/migrate/.*create_\\1.rb")
     ("app/views/\\1/.*"                       . "db/migrate/.*create_\\1.rb")
     ("app/helpers/\\1_helper.rb"              . "db/migrate/.*create_\\1.rb")
     ("app/models/\\1.rb"                      . "db/migrate/.*create_\\1.rb")
     ("spec/models/\\1_spec.rb"                . "db/migrate/.*create_\\1.rb")
     ("spec/controllers/\\1_spec.rb"           . "db/migrate/.*create_\\1.rb")
     ("spec/views/\\1/.*"                      . "db/migrate/.*create_\\1.rb")
     ("test/functional/\\1_controller_test.rb" . "db/migrate/.*create_\\1.rb")
     ("test/unit/\\1_test.rb#test_\\2$"        . "db/migrate/.*create_\\1.rb#\\2")
     ("test/unit/\\1_test.rb"                  . "db/migrate/.*create_\\1.rb")
     (t                                        . "db/migrate/"))
    (lambda (path)
      (phpmvc-generate "migration"
                       (and (string-match ".*create_\\(.+?\\)\.rb" path)
                            (match-string 1 path)))))
   (cells
    "C"
    (("app/cells/\\1_cell.rb"                  . "app/cells/\\1/.*")
     ("app/cells/\\1/\\2.*"                    . "app/cells/\\1_cell.rb#\\2")
     (t                                        . "app/cells/"))
    (lambda (path)
      (phpmvc-generate "cells"
                       (and (string-match ".*/\\(.+?\\)_cell\.rb" path)
                            (match-string 1 path)))))
   (features "F" ((t . "features/.*feature")) nil)
   (steps "S" ((t . "features/step_definitions/.*")) nil)
   (environment "e" ((t . "config/environments/")) nil)
   (application "a" ((t . "config/application.rb")) nil)
   (configuration "n" ((t . "config/")) nil)
   (script "s" ((t . "script/")) nil)
   (lib "l" ((t . "lib/")) nil)
   (log "o" ((t . "log/")) nil)
   (worker "w" ((t . "lib/workers/")) nil)
   (public "p" ((t . "public/")) nil)
   (stylesheet "y" ((t . "public/stylesheets/.*")
                    (t . "app/assets/stylesheets/.*")) nil)
   (sass "Y" ((t . "public/stylesheets/sass/.*")
              (t . "app/stylesheets/.*")) nil)
   (javascript "j" ((t . "public/javascripts/.*")
                    (t . "app/assets/javascripts/.*")) nil)
   (plugin "u" ((t . "vendor/plugins/")) nil)
   (mailer "M" ((t . "app/mailers/")) nil)
   (file-in-project "f" ((t . ".*")) nil)
   (by-context
    ";"
    (((lambda () ;; Find-by-Context
        (let ((path (buffer-file-name))
              cv)
          (when (string-match ".*/\\(.+?\\)/\\(.+?\\)\\..*" path)
            (setf cv (cons (match-string 1 path) (match-string 2 path)))
            (when (re-search-forward "<%=[ \n\r]*render(? *" nil t)
              (setf cv (phpmvc-ruby-values-from-render (car cv) (cdr cv)))
              (list (car cv) (cdr cv))))))
      . "app/views/\\1/\\2.*"))))
 "Jump schema for phpmvc.")

(defun phpmvc-apply-jump-schema (schema)
  "Define the phpmvc-find-* functions by passing each element SCHEMA to `defjump'."
  (mapcar
   (lambda (type)
     (let ((name (first type))
           (specs (third type))
           (make (fourth type)))
       (eval `(defjump
                ,(intern (format "phpmvc-find-%S" name))
                ,specs
                phpmvc-root
                ,(format "Go to the most logical %S given the current location" name)
                ,(if make `(quote ,make))
                'ruby-add-log-current-method))))
   schema))
(phpmvc-apply-jump-schema phpmvc-jump-schema)

;;--------------------------------------------------------------------
;; minor mode and keymaps

(defvar phpmvc-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Phpmvc minor mode.")

(defun phpmvc-bind-key-to-func (key func)
  "Bind KEY to FUNC with each of the `phpmvc-minor-mode-prefixes'."
  (dolist (prefix phpmvc-minor-mode-prefixes)
    (eval `(define-key phpmvc-minor-mode-map
             ,(format "\C-c%s%s" prefix key) ,func))))

(defvar phpmvc-minor-mode-keybindings
  '(("s" . 'phpmvc-script)              ("q" . 'phpmvc-sql)
    ("e" . 'phpmvc-insert-erb-skeleton) ("t" . 'phpmvc-test)
    ("r" . 'phpmvc-rake)                ("c" . 'phpmvc-console)
    ("w" . 'phpmvc-web-server)          ("g" . 'phpmvc-rgrep)
    ("x" . 'phpmvc-extract-partial)     ("p" . 'phpmvc-goto-partial)
    (";" . 'phpmvc-find-by-context)     ("'" . 'phpmvc-find-by-context)
    ("d" . 'phpmvc-cap))
  "Alist mapping of keys to functions in `phpmvc-minor-mode-map'.")

(dolist (el (append (mapcar (lambda (el)
                              (cons (concat "f" (second el))
                                    (read (format "'phpmvc-find-%S" (first el)))))
                            phpmvc-jump-schema)
                    phpmvc-minor-mode-keybindings))
  (phpmvc-bind-key-to-func (car el) (cdr el)))

;;;###autoload
(defun phpmvc-launch ()
  "Call function `phpmvc-minor-mode' if inside a phpmvc project.
Otherwise, disable that minor mode if currently enabled."
  (interactive)
  (let* ((root (phpmvc-root)) (r-tags-path (concat root phpmvc-tags-file-name)))
    (if root (progn
               (set (make-local-variable 'tags-file-name)
                    (and (file-exists-p r-tags-path) r-tags-path))
               (run-hooks 'phpmvc-minor-mode-hook)
               (phpmvc-minor-mode t))
      (if (and (fboundp 'phpmvc-minor-mode) phpmvc-minor-mode) (phpmvc-minor-mode)))))

;;;###autoload
(defvar phpmvc-major-modes
  (if (boundp 'phpmvc-major-modes)
      phpmvc-major-modes
    (list 'find-file-hook 'mumamo-after-change-major-mode-hook 'dired-mode-hook))
  "Major Modes from which to launch Phpmvc.")

;;;###autoload
(dolist (hook phpmvc-major-modes) (add-hook hook 'phpmvc-launch))

(defadvice cd (after phpmvc-on-cd activate)
  "Call `phpmvc-launch' when changing directories.
This will activate/deactivate phpmvc as necessary when changing
into and out of phpmvc project directories."
  (phpmvc-launch))

;;;###autoload
(define-minor-mode phpmvc-minor-mode
  "Enable Phpmvc minor mode to support working with the php on mvc framework."
  nil
  " Phpmvc"
  phpmvc-minor-mode-map)

(provide 'phpmvc)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; eval: (checkdoc-minor-mode 1)
;; End:

;;; phpmvc.el ends here
