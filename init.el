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

;; Now install el-get at the very first
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

;; enable git shallow clone to save time and bandwidth
(setq el-get-git-shallow-clone t)

;; Sometimes, we need to experiment with our own recipe, or override the
;; default el-get recipe to get around bugs.
(add-to-list 'el-get-recipe-path "~/.emacs.d/ome-el-get-recipes")

;; Oh-my-emacs adopt org-mode 8.x from v0.3, so org-mode should be the first
;; package to be installed via el-get
(defun ome-org-mode-setup ()
  ;; markdown export support
  (require 'ox-md))

(add-to-list 'el-get-sources
             '(:name org-mode
                     :after (progn
                              (ome-org-mode-setup))))

(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

(defvar ome-dir (file-name-directory (or load-file-name (buffer-file-name)))
  "oh-my-emacs home directory.")

;; load up the ome
(org-babel-load-file (expand-file-name "ome.org" ome-dir))

;;; init.el ends here
