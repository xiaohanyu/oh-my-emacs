;;; init.el --- Where all the magic begins
;;
;; Part of the oh-my-emacs
;;
;; This is the first thing to get loaded.
;;

;; Enter debugger if an error is signaled during Emacs startup.
;;
;; This works the same as you boot emacs with "--debug-init" every time, except
;; for errors in "init.el" itself, which means, if there's an error in
;; "init.el", "emacs --debug-init" will entering the debugger, while "emacs"
;; will not; however, if there's an error in other files loaded by init.el,
;; both "emacs" and "emacs --debug-init" will entering the debugger. I don't
;; know why.
(setq debug-on-error t)

;; believe me, you don't need menubar, toolbar nor scrollbar
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; load Org-mode from source when the ORG_HOME environment variable is set
(setq org-home (getenv "ORG_HOME"))

(when org-home
  (let ((org-lisp-dir (expand-file-name "lisp" org-home))
        (org-contrib-lisp-dir (expand-file-name "contrib/lisp" org-home)))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (add-to-list 'load-path org-contrib-lisp-dir)
      (require 'org))))

;; load the ome from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
          `(lambda ()
             ;; remember this directory
             (setq ome-dir
                   ,(file-name-directory (or load-file-name (buffer-file-name))))
             ;; only load org-mode later if we didn't load it just now
             ,(unless (and org-home
                           (file-directory-p (expand-file-name "lisp" org-home)))
                '(require 'org))
             ;; load up the ome
             (org-babel-load-file (expand-file-name "ome.org" ome-dir))))

;;; init.el ends here
