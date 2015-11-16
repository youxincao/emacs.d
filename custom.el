(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GKCJ]" "" output)))))
(defun js2-evel-cur-buffer()
      (interactive)
      (shell-command-on-region (point-min) (point-max) "node"))

(add-hook 'js2-mode-hook '(lambda ()
                           (local-set-key "\C-x\C-e" 'eval-last-sexp)
                           (local-set-key "\C-cb" 'js-send-buffer)
                           (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                           (local-set-key "\C-cl" 'js-load-file-and-go)
                           (local-set-key "\C-c!" 'run-js)
                           (local-set-key "\C-c\C-r" 'js-send-region)
                           (local-set-key "\C-c\C-j" 'js-send-line)
                           (local-set-key "\C-c\C-u" 'whitespace-clean-and-compile)
                           (local-set-key "\C-c\C-c" 'js2-evel-cur-buffer)
                           ))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
;; for magit
(require-package 'magit)

;; for grizzl
;;(require-package 'grizzl)

;; for helm
(require-package 'helm)
(require-package 'helm-projectile)

;; for projectile
(require-package 'projectile)
(projectile-global-mode)
(if *win32*
    (setq projectile-indexing-method 'alien))
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-unset-key "\C-cp")
(global-set-key (kbd "C-c m") 'evilnc-comment-or-uncomment-paragraphs)

(require-package 'anzu)
(global-anzu-mode +1)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)
(global-set-key (kbd "C-c a") 'anzu-query-replace-at-cursor)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-search-threshold 1000)
 '(anzu-replace-to-string-separator " => "))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (load-theme 'monokai t)))
  (load-theme 'monokai t))

(global-set-key (kbd "C-c C-<left>") 'windmove-left)
(global-set-key (kbd "C-c C-<right>") 'windmove-right)
(global-set-key (kbd "C-c C-<up>") 'windmove-up)
(global-set-key (kbd "C-c C-<down>") 'windmove-down)

(require-package 'nodejs-repl)
(add-to-list 'load-path
             "~/.emacs.d/lib")
(require 'nodejs-repl-eval)
