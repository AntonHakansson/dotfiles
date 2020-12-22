;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Anton Hakansson"
      user-mail-address "anton.hakansson98@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(map! :leader
      :desc "Open like spacemacs" "SPC" #'counsel-M-x)
(map! "<mouse-8>" #'previous-buffer)
(map! "<mouse-9>" #'next-buffer)


;;
;; Dired
;;
(defun dired-xdg-open ()
  (interactive)
  (let* ((file (dired-get-filename nil t)))
    (call-process "xdg-open" nil 0 nil file))
  )

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's
   loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [remap dired-find-file]
    'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    'dired-single-up-directory))

(require 'dired-single)

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

(use-package! all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file
  (kbd "C-c C-o") 'dired-xdg-open
)

(map! :leader
      :desc "Dired"
      "d d" #'dired
      :desc "Dired jump to current"
      "d j" #'dired-jump)


;;
;;
;;

;; autocorrect the previous word without leaving insert mode
(map! :i "C-i" #'flyspell-auto-correct-word)

(setq ispell-personal-dictionary nil)

;;
;; Zig config
;;
(use-package! zig-mode
  :custom
  (zig-format-on-save nil)
  )
(require 'lsp)
(add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
(lsp-register-client
  (make-lsp-client
    :new-connection (lsp-stdio-connection "/storage/git/zls/zig-cache/bin/zls")
    :major-modes '(zig-mode)
    :server-id 'zls))
(add-hook 'zig-mode-hook 'lsp)

;; org-mode
(add-hook 'org-mode-hook 'org-fragtog-mode)
(after! org
  (setq org-startup-folded 'fold
        org-hide-emphasis-markers t
        org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (org . t)
     (shell . t)
     (python . t)
     (latex . t)
     (haskell . t)
     )
   )

  (add-to-list 'org-file-apps '("\\.gp5\\'" . "tuxguitar %s") t)
  (add-to-list 'org-file-apps '("\\.png\\'" . "sxiv %s") t)
  (add-to-list 'org-file-apps '("\\.svg\\'" . "inkscape %s") t)

  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (use-package! org-pdftools
    :hook (org-load . org-pdftools-setup-link))

  (use-package! org-download
    :custom
    (org-download-image-dir "./images/")
    )


  ;; (setq org-default-notes-file "~/Documents/org/main.org")
  ;; (org-babel-load-file "~/Documents/org/roam/nutrition.org")
  ;; (org-babel-load-file "~/Documents/org/main.org")

  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory "~/Documents/org/")
    :bind (:map org-roam-mode-map
            (("C-c n l" . org-roam)
             ("C-c n f" . org-roam-find-file)
             ("C-c n g" . org-roam-graph-show))
           :map org-mode-map
            (("C-c n i" . org-roam-insert))
            (("C-c n I" . org-roam-insert-immediate))
           )
    ;; :init
    ;; (progn
    ;;   (spacemacs/declare-prefix "ar" "org-roam")
    ;;   (spacemacs/set-leader-keys
    ;;     "arl" 'org-roam
    ;;     "art" 'org-roam-dailies-today
    ;;     "arf" 'org-roam-find-file
    ;;     "arg" 'org-roam-graph)

    ;;   (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
    ;;   (spacemacs/set-leader-keys-for-major-mode 'org-mode
    ;;     "rl" 'org-roam
    ;;     "rt" 'org-roam-dailies-today
    ;;     "rb" 'org-roam-switch-to-buffer
    ;;     "rf" 'org-roam-find-file
    ;;     "ri" 'org-roam-insert
    ;;     "rg" 'org-roam-graph))
    )
  )

(defun my/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: " org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file))
  )

(map! :map org-mode-map
      "C-c d p" #'my/org-download-paste-clipboard
      "C-c d d" #'org-download-delete)

(defun my/inkscape-create (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let*((basename (if (not use-default-filename) (read-string (format "Filename [%s]: " "figure.svg") nil nil "figure.svg") nil))
        (dir (org-download--dir))
        (filepath (concat dir "/" (org-download-file-format-default basename))))
      (make-directory dir t)
      (when (not (file-exists-p filepath)) (start-process-shell-command "Inkscape" nil (format "inkscape -o %s" filepath))) ; create empty svg file
      (start-process-shell-command "Inkscape" nil (format "inkscape %s" filepath)) ; open svg file
      (org-download-insert-link basename filepath)
    )
  )

(map! :map org-mode-map
        "C-c d i" #'my/inkscape-create)
