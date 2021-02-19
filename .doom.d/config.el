;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))

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
(after! org-agenda
  (setq org-agenda-files
        (apply 'append
               (mapcar
                (lambda (directory)
                  (directory-files-recursively
                   directory org-agenda-file-regexp))
                '("~/Documents/org/"))))
  )

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

(setq doom-variable-pitch-font (font-spec :family "ubuntu"))
;;(setq doom-variable-pitch-font (font-spec :family "Overpass" :size 13))
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
(use-package! company-tabnine
  :when (featurep! :completion company)
  :config
  )

(after! company
  (setq company-idle-delay 0.15
        company-minimum-prefix-length 2
        company-show-numbers t
        )
  ;; (add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
  )

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setq tab-always-indent 'complete)

(set-company-backend!
  '(org-mode)
  '(
    company-math-symbols-unicode
    company-files
    company-yasnippet
    ))

;; TO DOWNLOAD THE DICTIONARY:
;; cd /tmp
;; curl -o "aspell6-en-custom.tar.bz2" 'http://app.aspell.net/create?max_size=80&spelling=GBs&spelling=AU&max_variant=0&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=aspell'
;; tar -xjf "aspell6-en-custom.tar.bz2"
;;
;; cd aspell6-en-custom
;; ./configure && make && sudo make install
(setq ispell-dictionary "en-custom")

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
(after! flyspell
  (setq ispell-local-dictionary "english")
  )

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
        '(("http://rss.slashdot.org/Slashdot/slashdot" news tech slashdot)
          ("https://www.technologyreview.com/feed/" news tech MIT)
          ("https://reddit.0qz.fun/r/news.json" news reddit)
          ("https://www.svt.se/nyheter/rss.xml" news swe svt)
          ("https://api.sr.se/api/rss/channel/104?format=1" news swe radio)

          ("https://usesthis.com/feed.atom" tech)
          ("https://xkcd.com/rss.xml" comic)

          ("https://moreplatesmoredates.com/feed/" health derek)
          ("https://kill-the-newsletter.com/feeds/kb84eb1tf6a3kta7.xml" health derek)

          ("https://reddit.0qz.fun/r/GregDoucette.json" nutrition greg)
          ("https://reddit.0qz.fun/r/vegan.json" nutrition vegan)
          ("https://reddit.0qz.fun/r/AntiVegan.json" nutrition anti-vegan)
          ("https://reddit.0qz.fun/r/Volumeeating.json" nutrition volume)

          ("https://planet.emacslife.com/atom.xml" emacs community)
          ("https://reddit.0qz.fun/r/emacs.json" emacs reddit)
          ("https://sachachua.com/blog/category/emacs/feed/" emacs blog)
          ("https://reddit.0qz.fun/r/DoomEmacs.json" emacs doom)
          ("https://reddit.0qz.fun/r/orgmode.json" emacs orgmode)
          ("https://helpdeskheadesk.net/index.xml" emacs blog linux)
          ("https://blog.ashfaqfarooqui.me/index.xml" emacs orgmode blog chalmers-guy)

          ("https://castel.dev/rss.xml" linux blog note-taking) ; lecture note setup

          ("https://lukesmith.xyz/rss.xml" linux lukesmith)
          ("https://notrelated.xyz/rss" linux lukesmith podcast)
          ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" linux lukesmith yt)

          ("https://kill-the-newsletter.com/feeds/5ba3cqgwcewiowyu.xml" software research andy)

          ("https://handmade.network/podcast/podcast.xml" dev handmade podcast)
          ("https://rssbridge.pofilo.fr/?action=display&bridge=Twitter&context=By+username&u=cmuratori&format=Atom" dev game handmade casey)
          ("https://rssbridge.pofilo.fr/?action=display&bridge=Twitter&context=By+username&u=Jonathan_Blow&format=Atom" dev game handmade jonathan blow)
          ("https://ourmachinery.com/index.xml" dev game)
          ("https://kill-the-newsletter.com/feeds/j0mm7ibixsb7o20q.xml" dev nullprogram)

          ("https://kill-the-newsletter.com/feeds/j0mm7ibixsb7o20q.xml" dev 4coder)

          ("https://andrewkelley.me/rss.xml" dev zig andrew)
          ("https://kill-the-newsletter.com/feeds/v71vn4ghp8q4cy4j.xml" dev zig showtime)

          ("https://hackaday.com/blog/feed/" hackaday)
          ;; ("https://rssbridge.hakanssn.com/?action=display&bridge=GithubTrending&context=By+language&language=&date_range=today&format=Atom" github trending tech)
          )

        elfeed-search-filter "@4-months-ago +unread -nutrition"))

(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)

(add-hook! 'elfeed-show-mode-hook 'mixed-pitch-mode 'writeroom-mode)
(add-hook! 'elfeed-search-mode-hook 'writeroom-mode)


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

(add-hook! 'org-mode-hook #'writeroom-mode)
(add-hook! 'org-mode-hook #'+org-pretty-mode)
(map! :map org-mode-map
      :n "SPC m l a" #'counsel-org-link)
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
        org-ellipsis "   ⮷"
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
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


  (custom-set-faces! '(org-ellipsis :foreground "#5B6268"))
  (custom-set-faces! '(org-document-title :height 1.2))
  (setq org-agenda-deadline-faces
        '((1.001 . error)
          (1.0 . org-warning)
          (0.5 . org-upcoming-deadline)
          (0.0 . org-upcoming-distant-deadline)))

  (add-to-list 'org-file-apps '("\\.gp5\\'" . "tuxguitar %s") t)
  (add-to-list 'org-file-apps '("\\.png\\'" . "sxiv %s") t)
  (add-to-list 'org-file-apps '("\\.svg\\'" . "write_stylus %s") t)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s") t)

  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (add-hook 'org-mode-hook 'org-latex-preview)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  ;; (use-package! org-pdftools
  ;;   :hook (org-load . org-pdftools-setup-link))

  (use-package! org-download
    :custom
    (org-download-image-dir "images/")
    (org-download-link-format (format "[[file:%s%%s]]\n" org-download-image-dir))
    (org-download-method 'directory)
    (org-download-image-org-width 400)
    (org-download-heading-lvl nil)
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

(use-package! org-special-block-extras
  :hook (org-mode . org-special-block-extras-mode)
  ;; :custom
    ;; (org-special-block-extras-fancy-links
    ;; nil "Disable this feature.")
  :config
  ;; Use short names like ‘defblock’ instead of the fully qualified name
  ;; ‘org-special-block-extras--defblock’
    (org-special-block-extras-short-names))

(use-package! org-appear
  :hook (org-mode . org-appear-mode))

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
     (?v    ("\\lor"   "\\vdash"))
     (?V    (""        "\\vDash"))
     (?.    ("\\cdot"  "\\dots"))
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
