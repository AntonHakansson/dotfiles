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

(setq system-time-locale "en_US.utf8")

(setq +latex-viewers '(zathura))

;; Spaces over tabs
;; (setq c-basic-indent 4)
;; (setq c-default-style "linux")
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(setq yas-triggers-in-field t)

(setq doom-variable-pitch-font (font-spec :family "Overpass" :size 24))
(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

(use-package! helpful)


;; company
(after! company
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))
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

(after! elfeed
  (setq elfeed-feeds
        '(("http://rss.slashdot.org/Slashdot/slashdot" news slashdot)
          ("https://www.reddit.com/r/news/.rss" news reddit)
          ("https://www.technologyreview.com/feed/" news MIT)

          ("https://xkcd.com/rss.xml" comic)

          ("https://www.reddit.com/r/GregDoucette.rss" nutrition greg)
          ("https://www.reddit.com/r/vegan/.rss" nutrition vegan)
          ("https://www.reddit.com/r/AntiVegan/.rss" nutrition anti-vegan)
          ("https://www.reddit.com/r/Volumeeating/.rss" nutrition volume)

          ("https://planet.emacslife.com/atom.xml" emacs community)
          ("https://www.reddit.com/r/emacs/.rss" emacs reddit)
          ("https://www.reddit.com/r/DoomEmacs/.rss" emacs doom)
          ("https://www.reddit.com/r/orgmode/.rss" emacs orgmode)

          ("https://lukesmith.xyz/rss.xml" linux luke)
          ("https://notrelated.xyz/rss" linux luke podcast)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" linux luke yt)

          ("https://rss.app/feeds/d1bG4OcegRj2fuZh.xml" casey handmade)
          ("https://rss.app/feeds/uVMj7YQeBCa2fLsz.xml" blow handmade))


        elfeed-search-filter "@4-months-ago +unread -nutrition"))


  ;; Load elfeed-org
  ;; (require 'elfeed-org)
  ;; Initialize elfeed-org
  ;; This hooks up elfeed-org to read the configuration when elfeed
  ;; is started with =M-x elfeed=
  ;; (elfeed-org)
(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")

(setq +zen-text-scale 0.6)

;;
;; org-mode
;;
; (add-hook! 'org-mode-hook #'mixed-pitch-mode)
(add-hook! 'org-mode-hook #'+org-pretty-mode)
(after! org
  (map!
   :after evil-org
   :map evil-org-mode-map
   ;; custom
   "C-c d p" #'my/org-download-paste-clipboard
   "C-c d d" #'org-download-delete
   "C-c d i" #'my/inkscape-create
   ;; outline navigation
   "C-l" (cmd! (if (outline-invisible-p (line-end-position)) (+org/open-fold) (org-next-visible-heading 1)))
   "C-h" (cmd! (if (not (outline-invisible-p (line-end-position))) (+org/close-fold) (org-up-heading-safe)))
   "C-j" (cmd! (org-next-visible-heading))
   "C-k" (cmd! (org-previous-visible-heading))
   ;; math
   :i "C-m" (cmd! ()))


  (setq org-startup-folded 'fold
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'smart ; try not to accidently do weird stuff in invisible regions
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
        ;; org-todo-keyword-faces
        ;;                 (quote (("TODO" :foreground "red" :weight bold)
        ;;                         ("NEXT" :foreground "blue" :weight bold)
        ;;                         ("DONE" :foreground "forest green" :weight bold)
        ;;                         ("WAITING" :foreground "orange" :weight bold)
        ;;                         ("HOLD" :foreground "magenta" :weight bold)
        ;;                         ("CANCELLED" :foreground "forest green" :weight bold)
        ;;                         ("MEETING" :foreground "forest green" :weight bold)
        ;;                         ("PHONE" :foreground "forest green" :weight bold)))
        ;; org-todo-state-tags-triggers
        ;;                 (quote (("CANCELLED" ("CANCELLED" . t))
        ;;                         ("WAITING" ("WAITING" . t))
        ;;                         ("HOLD" ("WAITING") ("HOLD" . t))
        ;;                         (done ("WAITING") ("HOLD"))
        ;;                         ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
        ;;                         ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
        ;;                         ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
        org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (dot . t)
                                 (ditaa . t)
                                 (R . t)
                                 (org . t)
                                 (shell . t)
                                 (python . t)
                                 (latex . t)
                                 (haskell . t)))

  (custom-set-faces! '(org-document-title :height 1.2))
  (setq org-agenda-deadline-faces
        '((1.001 . error)
          (1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline)))

  (add-to-list 'org-file-apps '("\\.gp5\\'" . "tuxguitar %s") t)
  (add-to-list 'org-file-apps '("\\.png\\'" . "sxiv %s") t)
  (add-to-list 'org-file-apps '("\\.svg\\'" . "write_stylus %s") t)

  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  ;; (use-package! org-pdftools
  ;;   :hook (org-load . org-pdftools-setup-link))

  (use-package! org-download
    :custom
    (org-download-image-dir "images/")
    (org-download-link-format (format "[[file:%s%%s]]" org-download-image-dir))
    (org-download-method 'directory)
    (org-download-image-org-width 400)
    )

  (use-package! org-fragtog
    :hook ((org-mode LaTeX-mode) . org-fragtog-mode))

  ;; (use-package! org-menu
  ;;   :config (map! :map org-mode-map "C-c m" 'org-menu))

  ;; Import ignore-headlines to allow a headline (but not its children) to
  ;; be ignored. Any headline tagged with the 'ignore' tag will be
  ;; ignored (i.e. will not be included in the export), but any child
  ;; headlines will not be ignored (unless explicitly tagged to be
  ;; ignored), and will instead have their levels promoted by one.
  (use-package! ox-extra
    :config
    (ox-extras-activate '(latex-header-blocks ignore-headlines)))
  (add-to-list 'org-latex-classes
               '("ieee"

                 "\\documentclass[conference]{IEEEtran}
%\\IEEEoverridecommandlockouts
% The preceding line is only needed to identify funding in the first footnote. If that is unneeded, please comment it out.
\\usepackage{amsmath,amssymb,amsfonts}
\\usepackage{algorithmic}
\\usepackage{graphicx}
\\usepackage{textcomp}
\\usepackage{xcolor}
\\def\\BibTeX{{\\rm B\\kern-.05em{\sc i\\kern-.025em b}\\kern-.08em
T\\kern-.1667em\\lower.7ex\\hbox{E}\\kern-.125emX}}"


                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; (setq org-default-notes-file "~/Documents/org/main.org")
  ;; (org-babel-load-file "~/Documents/org/roam/nutrition.org")
  ;; (org-babel-load-file "~/Documents/org/main.org")

  ;; Make those fragments look good
  (setq org-highlight-latex-and-related '(native script entities))

  ;; Youtube links
  (org-link-set-parameters "yt" :export #'+org-export-yt)
  (defun +org-export-yt (path desc backend _com)
    (cond ((org-export-derived-backend-p backend 'html)
           (format "<iframe width='440' \
height='335' \
src='https://www.youtube.com/embed/%s' \
frameborder='0' \
allowfullscreen>%s</iframe>" path (or "" desc)))
          ((org-export-derived-backend-p backend 'latex)
           (format "\\href{https://youtu.be/%s}{%s}" path (or desc "youtube")))
          (t (format "https://youtu.be/%s" path))))
  ;;

  ;; Snippet helper
  (defun +yas/org-src-header-p ()
    (or
     (looking-back "^#\\+property:[ \t]+header-args:.*" (line-beginning-position))
     (looking-back "^#\\+header:.*" (line-beginning-position))))
  ;;

  ;; (use-package org-roam
  ;;   :hook
  ;;   (after-init . org-roam-mode)
  ;;   :custom
  ;;   (org-roam-directory "~/Documents/org/")
  ;;   :bind (:map org-roam-mode-map
  ;;          (("C-c n l" . org-roam)
  ;;           ("C-c n f" . org-roam-find-file)
  ;;           ("C-c n g" . org-roam-graph-show))
  ;;          :map org-mode-map
  ;;          (("C-c n i" . org-roam-insert))
  ;;          (("C-c n I" . org-roam-insert-immediate))))
  )

(after! ox-latex
  (setq org-latex-pdf-process
        '("latexmk -%latex -shell-escape -interaction=nonstopmode -f -pdf -output-directory=%o %f"))
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

(use-package! org-special-block-extras
  :hook (org-mode . org-special-block-extras-mode)
  ;; :custom
    ;; (org-special-block-extras-fancy-links
    ;; nil "Disable this feature.")
  :config
  ;; Use short names like ‘defblock’ instead of the fully qualified name
  ;; ‘org-special-block-extras--defblock’
    (org-special-block-extras-short-names))

(use-package! org-latex-impatient
  :defer t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq
   org-latex-impatient-tex2svg-bin "~/node_modules/mathjax-node-cli/bin/tex2svg"
   org-latex-impatient-delay 0
   org-latex-impatient-scale 2.0
   ))

(defun my/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: " org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil))
        (org-download-method 'directory))
    (org-download-clipboard file))
  )


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

(defun my/write-stylus-create (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let*((basename (if (not use-default-filename) (read-string (format "Filename [%s]: " "figure.svg") nil nil "figure.svg") nil))
        (dir (org-download--dir))
        (filepath (concat dir "/" (org-download-file-format-default basename)))
        (org-download-image-org-width 400))
      (make-directory dir t)
      (when (not (file-exists-p filepath)) (copy-file "~/.doom.d/template.svg" filepath)) ; create empty svg file
      (start-process-shell-command "write_stylus" nil (format "write_stylus %s" filepath)) ; open svg file
      (org-download-insert-link basename filepath)
    )
  )


(defun my/org-syntax-convert-case-to-lower ()
  "Convert all #+KEYWORDS to #+keywords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0)
          (case-fold-search nil))
      (while (re-search-forward "#\\+[A-Z_]+" nil t)
        (replace-match (downcase (match-string 0)) t)
        (setq count (1+ count)))
      (message "Replaced %d occurances" count))))

(after! cdlatex
  (setq
   cdlatex-math-symbol-prefix (string-to-char ";")
   cdlatex-simplify-sub-super-scripts nil
   cdlatex-math-symbol-alist
   '( ;; adding missing functions to 3rd level symbols
     (?_    ("\\downarrow"  "\\Downarrow"           "\\inf"))
     (?2    ("^2"           "\\sqrt{?}"     ""     ))
     (?3    ("^3"           "\\sqrt[3]{?}"  ""     ))
     (?^    ("\\uparrow"    ""           "\\sup"))
     (?k    ("\\kappa"      ""           "\\ker"))
     (?m    ("\\mu"         ""           "\\lim"))
     (?c    (""             "\\circ"     "\\cos"))
     (?d    ("\\delta"      "\\partial"  "\\dim"))
     (?D    ("\\Delta"      "\\nabla"    "\\deg"))
     ;; no idea why \Phi isnt on 'F' in first place, \phi is on 'f'.
     (?F    ("\\Phi"))
     ;; now just conveniance
     (?v    ("\\lor" "\\vdash"))
     (?V    ("" "\\vDash"))
     (?.    ("\\cdot" "\\dots"))
     (?:    ("\\vdots" "\\ddots"))
     (?*    ("\\times" "\\star" "\\ast")))
   cdlatex-math-modify-alist
   '( ;; my own stuff
     (?B    "\\mathbb"        nil          t    nil  nil)
     (?t    "\\text"          nil          t    nil  nil)
     (?a    "\\abs"           nil          t    nil  nil)))

    (org-defkey org-cdlatex-mode-map (kbd ";") #'cdlatex-math-symbol)
    (map! :map cdlatex-mode-map
          :i ";" #'cdlatex-math-symbol)
  )

(after! tex
  (map!
    :map LaTeX-mode-map
    :ei [C-return] #'LaTeX-insert-item)
  (setq TeX-electric-math '("\\(" . ""))

  (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Evince")))

(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gz\\'"))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)
