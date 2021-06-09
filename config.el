;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Anton Hakansson"
      user-mail-address "anton.hakansson98@gmail.com")

(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; (after! org-agenda
;;   (setq org-agenda-files
;;         (apply 'append
;;                (mapcar
;;                 (lambda (directory)
;;                   (directory-files-recursively
;;                    directory org-agenda-file-regexp))
;;                 '("~/org/"))))
;;   )

(setq display-line-numbers-type `relative)

;; Easier to match with a bspwm rule:
;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
(setq emacs-everywhere-frame-name-format "emacs-anywhere")

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

(map! :leader
      :desc "Open like spacemacs" "SPC" #'counsel-M-x)
(map! "<mouse-8>" #'previous-buffer)
(map! "<mouse-9>" #'next-buffer)

(setq system-time-locale "en_US.utf8")

(setq +latex-viewers '(zathura))

;; Spaces over tabs
(setq c-basic-indent 2)
(setq c-default-style "linux")
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

(setq yas-triggers-in-field t)

(setq +zen-text-scale 0.6)
(custom-set-faces!
  '(outline-1 :weight extra-bold :height 1.25)
  '(outline-2 :weight bold :height 1.15)
  '(outline-3 :weight bold :height 1.12)
  '(outline-4 :weight semi-bold :height 1.09)
  '(outline-5 :weight semi-bold :height 1.06)
  '(outline-6 :weight semi-bold :height 1.03)
  '(outline-8 :weight semi-bold)
  '(outline-9 :weight semi-bold))

;; company
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t
        )
  ;; (add-hook 'evil-normal-state-entry-hook #'company-abort) ;; make aborting less annoying.
  )

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setq tab-always-indent 'complete)

(set-company-backend!
  '(text-mode markdown-mode gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet
    ))

(set-company-backend!
  '(org-mode)
  '(
    company-math-symbols-unicode
    ))

;; TO DOWNLOAD THE DICTIONARY:
;; cd /tmp
;;
;; curl -o "aspell6-en-custom.tar.bz2" 'http://app.aspell.net/create?max_size=80&spelling=US&max_variant=1&diacritic=keep&special=hacker&special=roman-numerals&encoding=utf-8&format=inline&download=aspell'
;; tar -xjf "aspell6-en-custom.tar.bz2"
;;
;; cd aspell6-en-custom
;; ./configure && make && sudo make install
;; (setq ispell-dictionary "en-custom")
(setq ispell-personal-dictionary (expand-file-name ".ispell_personal" doom-private-dir))

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

;;
;; CC config
;;
(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))

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
    :new-connection (lsp-stdio-connection "zls")
    :major-modes '(zig-mode)
    :server-id 'zls))
(add-hook 'zig-mode-hook 'lsp)

;;
;; Elfeed config
;;
;; (after! elfeed
;;   (setq elfeed-feeds
;;         '(("http://rss.slashdot.org/Slashdot/slashdot" news tech slashdot)
;;           ("https://www.technologyreview.com/feed/" news tech MIT)
;;           ("https://reddit.0qz.fun/r/news.json" reddit news)
;;           ("https://www.svt.se/nyheter/rss.xml" news swe svt)
;;           ("https://api.sr.se/api/rss/channel/104?format=1" news swe radio)

;;           ("https://usesthis.com/feed.atom" tech)
;;           ("https://xkcd.com/rss.xml" comic)

;;           ("https://moreplatesmoredates.com/feed/" health derek)
;;           ("https://kill-the-newsletter.com/feeds/kb84eb1tf6a3kta7.xml" health derek)

;;           ("https://reddit.0qz.fun/r/GregDoucette.json" reddit nutrition greg)
;;           ("https://reddit.0qz.fun/r/vegan.json" reddit nutrition vegan)
;;           ("https://reddit.0qz.fun/r/AntiVegan.json" reddit nutrition anti-vegan)
;;           ("https://reddit.0qz.fun/r/Volumeeating.json" reddit nutrition volume)

;;           ("https://planet.emacslife.com/atom.xml" emacs community)
;;           ("https://reddit.0qz.fun/r/emacs.json" emacs reddit)
;;           ("https://sachachua.com/blog/category/emacs/feed/" emacs blog)
;;           ("https://reddit.0qz.fun/r/DoomEmacs.json" reddit emacs doom)
;;           ("https://reddit.0qz.fun/r/orgmode.json" reddit emacs orgmode)
;;           ("https://helpdeskheadesk.net/index.xml" emacs blog linux)
;;           ("https://blog.ashfaqfarooqui.me/index.xml" emacs orgmode blog chalmers-guy)

;;           ("https://castel.dev/rss.xml" linux blog note-taking) ; lecture note setup

;;           ("https://lukesmith.xyz/rss.xml" linux lukesmith)
;;           ("https://notrelated.xyz/rss" linux lukesmith podcast)
;;           ("https://www.youtube.com/feeds/videos.xml?channel_id=UC2eYFnH61tmytImy1mTYvhA" linux lukesmith yt)

;;           ("https://kill-the-newsletter.com/feeds/5ba3cqgwcewiowyu.xml" software research andy)

;;           ("https://handmade.network/podcast/podcast.xml" dev handmade podcast)
;;           ("https://rssbridge.pofilo.fr/?action=display&bridge=Twitter&context=By+username&u=cmuratori&format=Atom" dev game handmade casey)
;;           ("https://rssbridge.pofilo.fr/?action=display&bridge=Twitter&context=By+username&u=Jonathan_Blow&format=Atom" dev game handmade jonathan blow)
;;           ("https://ourmachinery.com/index.xml" dev game)
;;           ("https://kill-the-newsletter.com/feeds/j0mm7ibixsb7o20q.xml" dev nullprogram)

;;           ("https://kill-the-newsletter.com/feeds/j0mm7ibixsb7o20q.xml" dev 4coder)

;;           ("https://andrewkelley.me/rss.xml" dev zig andrew)
;;           ("https://kill-the-newsletter.com/feeds/v71vn4ghp8q4cy4j.xml" dev zig showtime)

;;           ("https://hackaday.com/blog/feed/" hackaday)
;;           ;; ("https://rssbridge.hakanssn.com/?action=display&bridge=GithubTrending&context=By+language&language=&date_range=today&format=Atom" github trending tech)
;;           ))
;;   (setq elfeed-search-filter "@4-months-ago +unread -nutrition")
;;   )

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


(setq TeX-save-query nil
      TeX-show-compilation t
      TeX-command-extra-options "-shell-escape")


;;
;; org-mode
;;

;; (load! "local/org-xournal")
;; (load! "local/org-symbolab-search")

;; (add-hook! 'org-mode-hook #'writeroom-mode)
;; (add-hook! 'org-mode-hook #'+org-pretty-mode)
(add-hook! 'org-mode-hook #'mixed-pitch-mode)
(map! :map org-mode-map
      :n "SPC m l /" #'counsel-org-link)
(after! counsel
  (setq counsel-outline-display-style 'title))
(map! :map org-mode-map
  :n "C-h" #'org-evil-navigation-up
  :n "C-l" #'org-evil-navigation-down
  :n "C-j" #'org-evil-navigation-next
  :n "C-k" #'org-evil-navigation-prev)
(map! :map org-mode-map
  :n "C-c d p" #'my/org-download-paste-clipboard
  :n "C-c d d" #'org-download-delete
  :n "C-c d i" #'my/inkscape-create
  :n "C-c d s" #'my/write-stylus-create
  )
(map! :map org-mode-map :leader
      (:prefix-map ("=" . "calc")
       "=" #'calc-dispatch
       ;; Some other shortcuts
       "c" #'calc
       "q" #'quick-calc
       "g" #'calc-grab-region))

(after! org
  (setq org-startup-folded 'fold
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'smart ; try not to accidently do weird stuff in invisible regions
        org-log-done 'time
        org-ellipsis " ▾ "
        org-todo-keywords (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
        org-pretty-entities-include-sub-superscripts nil
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
  ;; (add-to-list 'org-file-apps '("\\.svg\\'" . "write_stylus %s") t)
  ;; (add-to-list 'org-file-apps '("\\.svg\\'" . "inkscape %s") t)

  (add-hook 'org-mode-hook 'org-latex-preview)
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  ;; (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
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
  :config
  (org-special-block-extras-short-names)
  )

(use-package! org-fragtog
  :hook (org-mode . org-fragtog-mode)
  )

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  )

(use-package! org-download
  :hook (org-mode . org-download-enable)
  :config
  (setq org-download-method 'directory
        org-download-image-dir "images/"
        org-download-link-format (format "[[file:%s%%s]]\n" org-download-image-dir)
        org-download-image-org-width 400
        org-download-heading-lvl nil)
)

(after! org
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  )


(setq calc-angle-mode 'rad  ; radians are rad
      calc-symbolic-mode t  ; keeps expressions like \sqrt{2} irrational for as long as possible
      calc-simplify-mode 'none)
;;
;; [[file:~/.config/doom/config.org::*Embedded calc][Embedded calc:2]]
(map! :map calc-mode-map
      :after calc
      :localleader
      :desc "Embedded calc (toggle)" "e" #'calc-embedded
      :desc "Embedded calc (activate)" "a" #'calc-embedded-activate
      :desc "Embedded calc (new formula)" "n" #'calc-embedded-new-formula
      :desc "Embedded calc (select)" "s" #'calc-embedded-select
      :desc "Embedded calc (word)" "w" #'calc-embedded-word
      :desc "Embedded calc (update)" "u" #'calc-embedded-update
      :desc "Embedded calc (duplicate)" "RET" #'calc-embedded-duplicate
      :desc "Embedded calc (next)" "j" #'calc-embedded-next
      :desc "Embedded calc (previous)" "k" #'calc-embedded-previous
      )
(map! :map org-mode-map
      :after org
      :localleader
      :desc "Embedded calc (toggle)" "E" #'calc-embedded)
(map! :map latex-mode-map
      :after latex
      :desc "Embedded calc (toggle)" "e" #'calc-embedded)

(use-package! calctex
  :commands calctex-mode
  ;; :init
  ;; (add-hook 'calc-mode-hook #'calctex-mode)
  :config
  (setq calctex-additional-latex-packages "
\\usepackage[usenames]{xcolor}
\\usepackage{soul}
\\usepackage{adjustbox}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{siunitx}
\\usepackage{cancel}
\\usepackage{mathtools}
\\usepackage{mathalpha}
\\usepackage{xparse}
\\usepackage{arevmath}"
        calctex-additional-latex-macros
        (concat calctex-additional-latex-macros
                "\n\\let\\evalto\\Rightarrow"))
  (defadvice! no-messaging-a (orig-fn &rest args)
    :around #'calctex-default-dispatching-render-process
    (let ((inhibit-message t) message-log-max)
      (apply orig-fn args)))
  ;; Fix hardcoded dvichop path (whyyyyyyy)
  (let ((vendor-folder (concat (file-truename doom-local-dir)
                               "straight/"
                               (format "build-%s" emacs-version)
                               "/calctex/vendor/")))
    (setq calctex-dvichop-sty (concat vendor-folder "texd/dvichop")
          calctex-dvichop-bin (concat vendor-folder "texd/dvichop")))
  (unless (file-exists-p calctex-dvichop-bin)
    (message "CalcTeX: Building dvichop binary")
    (let ((default-directory (file-name-directory calctex-dvichop-bin)))
      (call-process "make" nil nil nil))))

(setq org-html-mathjax-options
      '((path "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-svg.js" )
        (scale "1")
        (autonumber "ams")
        (multlinewidth "85%")
        (tagindent ".8em")
        (tagside "right")))

(setq org-html-mathjax-template
      "<script>
MathJax = {
  chtml: {
    scale: %SCALE
  },
  svg: {
    scale: %SCALE,
    fontCache: \"global\"
  },
  tex: {
    tags: \"%AUTONUMBER\",
    multlineWidth: \"%MULTLINEWIDTH\",
    tagSide: \"%TAGSIDE\",
    tagIndent: \"%TAGINDENT\"
  }
};
</script>
<script id=\"MathJax-script\" async
        src=\"%PATH\"></script>")

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
        (dir org-download-image-dir)
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
        (dir org-download-image-dir)
        (filepath (concat dir "/" (org-download-file-format-default basename)))
        (org-download-image-org-width 400))
      (make-directory dir t)
      (when (not (file-exists-p filepath)) (copy-file "~/.doom.d/template.svg" filepath)) ; create empty svg file
      (start-process-shell-command "write_stylus" nil (format "write_stylus %s" filepath)) ; open svg file
      (org-download-insert-link basename filepath)
    )
  )

(defun my/drawio-create (&optional template-path)
  (interactive)
  (require 'org-download)
  (let*((basename (read-string (format "Filename [%s]: " "figure.svg") nil nil "figure.svg"))
        (template-path (or template-path (concat doom-private-dir "drawio_template.svg")))
        (dir org-download-image-dir)
        (filepath (concat dir "/" (org-download-file-format-default basename)))
        (org-download-image-org-width 400))
      (make-directory dir t)
      (when (not (file-exists-p filepath)) (copy-file template-path filepath)) ; create empty svg file
      (start-process-shell-command "drawio" nil (format "drawio %s" filepath)) ; open svg file
      (org-download-insert-link basename filepath)
    )
  )

(defun my/drawio-edit ()
  (interactive)
  (let ((context (org-element-context)))
    (if (not (eq (car-safe context) 'link))
        (user-error "Not on a link")
      (start-process-shell-command
       "drawio"
       "drawio"
       (format "drawio %s"
               (shell-quote-wildcard-pattern
                (url-unhex-string (plist-get (cadr context) :path)))))))
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


(use-package! graphviz-dot-mode
  :commands graphviz-dot-mode
  :mode ("\\.dot\\'" "\\.gz\\'")
  :init
  (after! org
    (setcdr (assoc "dot" org-src-lang-modes)
            'graphviz-dot)))

(use-package! company-graphviz-dot
  :after graphviz-dot-mode)

(after! tex
  (map!
    :map LaTeX-mode-map
    :ei [C-return] #'LaTeX-insert-item)
  (setq TeX-electric-math '("\\(" . ""))

  (add-to-list 'TeX-view-program-list '("Evince" "evince %o"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Evince")))

(defvar org-prettify-inline-results t
  "Whether to use (ab)use prettify-symbols-mode on {{{results(...)}}}.")



;;
;; Fontifying inline src blocks in org
;;
(defun org-fontify-inline-src-blocks (limit)
  "Try to apply `org-fontify-inline-src-blocks-1'."
  (condition-case nil
      (org-fontify-inline-src-blocks-1 limit)
    (error (message "Org mode fontification error in %S at %d"
                    (current-buffer)
                    (line-number-at-pos)))))

(defun org-fontify-inline-src-blocks-1 (limit)
  "Fontify inline src_LANG blocks, from  `point' up to LIMIT."
  (let ((case-fold-search t))
    (when (re-search-forward "\\_<src_\\([^ \t\n[{]+\\)[{[]?" limit t) ; stolen from `org-element-inline-src-block-parser'
      (let ((beg (match-beginning 0))
            pt
            (lang-beg (match-beginning 1))
            (lang-end (match-end 1)))
        (remove-text-properties beg lang-end '(face nil))
        (font-lock-append-text-property lang-beg lang-end 'face 'org-meta-line)
        (font-lock-append-text-property beg lang-end 'face 'org-block-begin-line)
        (setq pt (goto-char lang-end))
        (when (org-element--parse-paired-brackets ?\[)
          (remove-text-properties pt (point) '(face nil))
          (font-lock-append-text-property pt (point) 'face 'org-block-begin-line)
          (setq pt (point)))
        (when (org-element--parse-paired-brackets ?\{)
          (remove-text-properties pt (point) '(face nil))
          (font-lock-append-text-property pt (1+ pt) 'face 'org-block-begin-line)
          (unless (= (1+ pt) (1- (point)))
            (if org-src-fontify-natively
                (org-src-font-lock-fontify-block (buffer-substring-no-properties lang-beg lang-end) (1+ pt) (1- (point)))
              (font-lock-append-text-property (1+ pt) (1- (point)) 'face 'org-block)))
          (font-lock-append-text-property (1- (point)) (point) 'face 'org-block-begin-line)
          (setq pt (point)))
        (when (re-search-forward "\\= {{{results(" limit t)
          (font-lock-append-text-property pt (1+ pt) 'face 'org-block)
          (goto-char pt))))
    (when (and org-prettify-inline-results (re-search-forward "{{{results(\\(.+?\\))}}}" limit t))
      (remove-list-of-text-properties (match-beginning 0) (point)
                                      '(composition
                                        prettify-symbols-start
                                        prettify-symbols-end))
      (font-lock-append-text-property (match-beginning 0) (match-end 0) 'face 'org-block)
      (let ((start (match-beginning 0)) (end (match-beginning 1)))
        (with-silent-modifications
          (compose-region start end "⟨")
          (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end))))
      (let ((start (match-end 1)) (end (point)))
        (with-silent-modifications
          (compose-region start end "⟩")
          (add-text-properties start end `(prettify-symbols-start ,start prettify-symbols-end ,end)))))))

(defun org-fontify-inline-src-blocks-enable ()
  "Add inline src fontification to font-lock in Org.
Must be run as part of `org-font-lock-set-keywords-hook'."
  (setq org-font-lock-extra-keywords
        (append org-font-lock-extra-keywords '((org-fontify-inline-src-blocks)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-fontify-inline-src-blocks-enable)

;;
;;
;;
