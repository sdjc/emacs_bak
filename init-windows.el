;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(winner-mode 1)


;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-func-with-other-buffer (split-function)
  (lexical-let ((s-f split-function))
    (lambda ()
      (interactive)
      (funcall s-f)
      (set-window-buffer (next-window) (other-buffer)))))

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))


;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (funcall (split-window-func-with-other-buffer 'split-window-vertically))))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)


(unless (fboundp 'window-rightmost-p)
  (defun window-rightmost-p (window)
    (>= (nth 2 (window-edges window))
       (frame-width (window-frame window)))))

(defun other-column (prefix)
  "Switch to the buffer in the vertical split to the right whose top
of window is closest to the top of the selected window.  With PREFIX
arg move to the left."
  (interactive "p")
  (let ((curr-col (car (window-pixel-edges)))
	(curr-top (cadr (window-pixel-edges)))
	(slop 25)
	(dir (if (not (eq prefix 1)) -1 1))
	next-col)
    (setq next-col
	  (cond
	   ((and (eq dir 1) (window-rightmost-p (selected-window))) 0)
	   ((and (eq dir -1) (window-leftmost-p (selected-window)))
	    (car (window-pixel-edges (frame-rightmost-window))))
	   (t
	    (while (or (and (eq dir 1) (<= (car (window-pixel-edges)) curr-col))
		       (and (eq dir -1) (>= (car (window-pixel-edges)) curr-col)))
	      (other-window dir))
	    (car (window-pixel-edges)))))
    (let ((smallest-delta (frame-pixel-height))
	  closest-window)
      (walk-windows 
       '(lambda (window)
	  (if (eq (car (window-pixel-edges window)) next-col)
	      (let ((this-delta (abs (- curr-top (cadr (window-pixel-edges window))))))
		(if (< this-delta smallest-delta)
		    (setq smallest-delta this-delta closest-window window))))))
      (select-window closest-window))))


(defun other-window-this-column (prefix)
  (interactive "p")
  (let ((window-list (get-column-window-list)))
    (select-window 
     (cond
      ((and (eq prefix 1)
	    (> (length window-list) 1))
       (cadr (get-column-window-list)))
      ((eq prefix 1) (car window-list))
      (t
       (car (last (get-column-window-list))))))))


(defun jump-to-window (buffer-name)
  (interactive "bEnter buffer to jump to: ")
  (let ((visible-buffers (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list)))
	window-of-buffer)
    (if (not (member buffer-name visible-buffers))
	(error "'%s' does not have visible window" buffer-name)
      (setq window-of-buffer
	    (delq nil (mapcar '(lambda (window) 
                                 (if (equal buffer-name (buffer-name (window-buffer window)))
                                     window nil)) (window-list))))
      (select-window (car window-of-buffer)))))

(defun get-column-window-list (&optional side)
  (let ((column (if (eq side 'right) (caddr (window-pixel-edges))
		  (car (window-pixel-edges))))
	(current-window (selected-window))
	column-window-list)
    (mapc '(lambda (window)
	     (when (eq (car (window-pixel-edges window)) column)
	       (add-to-list 'column-window-list window)))
	  (window-list))
    (setq column-window-list 
	  (sort column-window-list
		'(lambda (a b) 
		   (< (cadr (window-pixel-edges a))
		      (cadr (window-pixel-edges b))))))
    (while (not (eq (car column-window-list) current-window))
      (setq column-window-list (rotate-list column-window-list 1)))
    column-window-list))

(defun rotate-list (list count)
  (cond
   ((eq count 0) list)
   ((not list) list)
   (t
    (rotate-list (nconc  (cdr list) (list (car list)) '()) (1- count)))))


(defvar winnav-prefix-char (kbd "C-c o")
  "Prefix character for using winnav commands")

(defvar winnav-map (make-sparse-keymap)
  "*Keymap for winnav commands.")
(global-set-key winnav-prefix-char winnav-map)
(cond
 ((define-key winnav-map "v"    'other-window-this-column)
  (define-key winnav-map "h"    'other-column)
  (define-key winnav-map "j"    'jump-to-window)
  (define-key winnav-map "i" 'ido-jump-to-window)
  (define-key winnav-map "o"    'other-window)))

;Alternative jump-to-window using ido-mode:
;; This command is AMAZING. I recommend mapping it to `C-x v' or `C-x w'
;; depending on which is easier on your keyboard.
(defun ido-jump-to-window ()
  (interactive)
  (let* ((swap (lambda (l)
                 (if (cdr l)
                     (cons (cadr l) (cons (car l) (cddr l)))
                   l)))
         ;; Swaps the current buffer name with the next one along.
         (visible-buffers (swap (mapcar '(lambda (window) (buffer-name (window-buffer window))) (window-list))))
         (buffer-name (ido-completing-read "Window: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (setq window-of-buffer
            (delq nil (mapcar '(lambda (window)
                               (if (equal buffer-name (buffer-name (window-buffer window)))
                                   window
                                 nil))
                            (window-list))))
      (select-window (car window-of-buffer)))))
(provide 'init-windows)
