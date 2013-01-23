(eval-after-load 'rinari
  `(let ((rinari-lib-dir (directory-of-library "rinari")))
     (unless (require 'jump nil t)
       (error "jump.el not found; please run 'git submodule update --init' in %s"
              rinari-lib-dir))
     ;; Prevent rinari from shadowing ruby-mode and inf-ruby with its bundled copies
     (setq load-path
           (remove (file-name-as-directory (expand-file-name "util/inf-ruby" rinari-lib-dir))
                   (remove (file-name-as-directory (expand-file-name "util" rinari-lib-dir))
                           load-path)))))

(dolist (hook '(haml-mode-hook sass-mode-hook magit-mode-hook yaml-mode-hook html-erb-mode))
  (add-hook hook 'rinari-launch))
(defalias 'root 'rinari-root)

(defvar rails-directory<-->types
  '((:controller       "app/controllers/")
    (:layout           "app/views/layouts/")
    (:view             "app/views/")
    (:observer         "app/models/" (lambda (file) (rails-core:observer-p file)))
    (:mailer           "app/models/" (lambda (file) (rails-core:mailer-p file)))
    (:model            "app/models/" (lambda (file) (and (not (rails-core:mailer-p file))
                                                         (not (rails-core:observer-p file)))))
    (:helper           "app/helpers/")
    (:unit-test        "vendor/plugins/.*/test/") ; needs to appear before more-general :plugin
    (:model            "vendor/plugins/.*/lib/") ; needs to appear before more-general :plugin
    (:plugin           "vendor/plugins/")
    (:unit-test        "test/unit/")
    (:functional-test  "test/functional/")
    (:integration-test  "test/integration/")
    (:fixture          "test/fixtures/")
    (:lib              "lib")
    (:rspec-controller "spec/controllers")
    (:rspec-controller "spec/requests")
    (:rspec-fixture    "spec/fixtures")
    (:rspec-lib        "spec/lib")
    (:rspec-model      "spec/models")
    (:migration        "db/migrate"))
  "Rails file types -- rails directories map")
;; (require 'rails-lib)
(require 'rails-core)
;; (defalias 'root 'rinari-root)
;; (defalias 'root 'rinari-root)

(defcustom rails-core:class-dirs
  '("app/controllers"
    "app/views"
    "app/models"
    "app/helpers"
    "test/unit"
    "test/functional"
    "test/fixtures"
    "spec/controllers"
    "spec/requests"
    "spec/fixtures"
    "spec/lib"
    "spec/models"
    "lib")
  "Directories with Rails classes"
  :group 'rails
  :type '(repeat string))
(defun rails:class-by-file (filename)
  "Return the class associated with FILENAME.
   <rails-root>/(app/models|app/controllers|app/helpers|test/unit|test/functional|lib|spec/controllers|spec/lib|spec/models)/foo/bar_baz
                --> Foo::BarBaz"
  (let* ((case-fold-search nil)
         (path (replace-regexp-in-string
                (format
                 "\\(.*\\(%s\\)/\\)?\\([^\.]+\\)\\(.*\\)?"
                 (strings-join "\\|" rails-core:class-dirs)) "\\3" filename))
         (path (replace-regexp-in-string "/" "  " path))
         (path (replace-regexp-in-string "_" " " path)))
    (replace-regexp-in-string
     " " ""
     (replace-regexp-in-string
      "  " "::"
      (if (string-match "^ *\\([0-9]+ *\\)?[A-Z]" path)
          path
        (capitalize path))))))


(defun rails/controller? ()
t
)
(defun rails/cur-res-title()
)
(defun rails/model?()
t
)
(defun rails/migration?()
t
)
(defun rails/controller-spec? ()
 t
 )

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))


(provide 'init-rails)
