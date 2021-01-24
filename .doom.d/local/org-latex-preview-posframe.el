;;; local/latex-preview-posframe.el -*- lexical-binding: t; -*-

(require 'org)
(require 'org-element)
(require 'posframe)

(defconst org-latex-preview-posframe--output-buffer-prefix "*org-latex-preview-posframe*"
  "Prefix for buffer to hold the output.")

(defconst org-latex-preview-posframe--posframe-buffer "*org-latex-preview-posframe*"
  "Buffer to hold the preview.")

(defcustom org-latex-preview-posframe-posframe-position-handler
  #'org-latex-preview-posframe-poshandler
  "The handler for posframe position."
  :group 'org-latex-preview-posframe
  :type '(function))

(defvar org-latexd-preview-posframe--process nil)

(defun org-latex-preview-posframe-poshandler (info)
  "Default position handler for posframe.

Uses the end point of the current LaTeX fragment for inline math,
and centering right below the end point otherwise. Positions are
calculated from INFO."
    (if (fboundp 'posframe-poshandler-point-window-center)
        (posframe-poshandler-point-window-center info)
      (org-latex-preview-posframe--poshandler-point-window-center info))
  )

(defun org-latex-preview-posframe--poshandler-point-window-center (info)
  "Posframe's position handler.

Get a position which let posframe stay right below current
position, centered in the current window. The structure of INFO
can be found in docstring of `posframe-show'.

This function will be removed once a similar poshandler is
available in upstream."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-width (plist-get info :parent-window-width))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (y-pixel-offset (plist-get info :y-pixel-offset))
         (ymax (plist-get info :parent-frame-height))
         (window (plist-get info :parent-window))
         (position-info (plist-get info :position-info))
         (header-line-height (plist-get info :header-line-height))
         (tab-line-height (plist-get info :tab-line-height))
         (y-top (+ (cadr (window-pixel-edges window))
                   tab-line-height
                   header-line-height
                   (- (or (cdr (posn-x-y position-info)) 0)
                      ;; Fix the conflict with flycheck
                      ;; https://lists.gnu.org/archive/html/emacs-devel/2018-01/msg00537.html
                      (or (cdr (posn-object-x-y position-info)) 0))
                   y-pixel-offset))
         (font-height (plist-get info :font-height))
         (y-bottom (+ y-top font-height)))
    (cons (+ window-left (/ (- window-width posframe-width) 2))
          (max 0 (if (> (+ y-bottom (or posframe-height 0)) ymax)
                     (- y-top (or posframe-height 0))
                   y-bottom)))))

(defun org-latex-preview--get-latex-fragment ()
  (let ((datum (org-element-context)))
    (when (memq (org-element-type datum) '(latex-environment latex-fragment))
      (let ((beg (org-element-property :begin datum))
            (end (org-element-property :end datum)))
        (buffer-substring-no-properties beg end)
        )
      )
    )
  )

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun org-latex-preview-posframe--create-image
    (string tofile options buffer FINISH-FUNC &optional processing-type)
  "Create an image from LaTeX source using external processes.

The LaTeX STRING is saved to a temporary LaTeX file, then
converted to an image file by process PROCESSING-TYPE defined in
`org-preview-latex-process-alist'.  A nil value defaults to
`org-preview-latex-default-process'.

The generated image file is eventually moved to TOFILE.

The OPTIONS argument controls the size, foreground color and
background color of the generated image.

When BUFFER non-nil, this function is used for LaTeX previewing.
Otherwise, it is used to deal with LaTeX snippets showed in
a HTML file."
  (let* ((processing-type (or processing-type
                              org-preview-latex-default-process))
         (processing-info
          (cdr (assq processing-type org-preview-latex-process-alist)))
         (programs (plist-get processing-info :programs))
         (error-message (or (plist-get processing-info :message) ""))
         (image-input-type (plist-get processing-info :image-input-type))
         (image-output-type (plist-get processing-info :image-output-type))
         (post-clean (or (plist-get processing-info :post-clean)
                         '(".dvi" ".xdv" ".pdf" ".tex" ".aux" ".log"
                           ".svg" ".png" ".jpg" ".jpeg" ".out")))
         (latex-header
          (or (plist-get processing-info :latex-header)
              (org-latex-make-preamble
               (org-export-get-environment (org-export-get-backend 'latex))
               org-format-latex-header
               'snippet)))
         (latex-compiler (plist-get processing-info :latex-compiler))
         (image-converter (plist-get processing-info :image-converter))
         (tmpdir temporary-file-directory)
         (texfilebase (make-temp-name
                       (expand-file-name "orgtex" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (image-size-adjust (or (plist-get processing-info :image-size-adjust)
                                '(1.0 . 1.0)))
         (scale (* (if buffer (car image-size-adjust) (cdr image-size-adjust))
                   (or (plist-get options (if buffer :scale :html-scale)) 1.0)))
         (dpi (* scale (if buffer (org--get-display-dpi) 140.0)))
         (fg (or (plist-get options (if buffer :foreground :html-foreground))
                 "Black"))
         (bg (or (plist-get options (if buffer :background :html-background))
                 "Transparent"))
         (log-buf (get-buffer-create "*Org Preview LaTeX Output*"))
         (resize-mini-windows nil)) ;Fix Emacs flicker when creating image.
    (dolist (program programs)
      (org-check-external-command program error-message))
    (if (eq fg 'default)
        (setq fg (org-latex-color :foreground))
      (setq fg (org-latex-color-format fg)))
    (setq bg (cond
              ((eq bg 'default) (org-latex-color :background))
              ((string= bg "Transparent") nil)
              (t (org-latex-color-format bg))))
    ;; Remove TeX \par at end of snippet to avoid trailing space.
    (if (string-suffix-p string "\n")
        (aset string (1- (length string)) ?%)
      (setq string (concat string "%")))
    (with-temp-file texfile
      (insert latex-header)
      (insert "\n\\begin{document}\n"
              "\\definecolor{fg}{rgb}{" fg "}%\n"
              (if bg
                  (concat "\\definecolor{bg}{rgb}{" bg "}%\n"
                          "\n\\pagecolor{bg}%\n")
                "")
              "\n{\\color{fg}\n"
              string
              "\n}\n"
              "\n\\end{document}\n"))
    (deferred:$
      (deferred:next
        (lambda ()
          (let* (
             (err-msg (format "Please adjust `%s' part of `org-preview-latex-process-alist'." processing-type))
             (image-input-file
              (org-compile-file
               texfile latex-compiler image-input-type err-msg log-buf))
             (image-output-file
              (org-compile-file
               image-input-file image-converter image-output-type err-msg log-buf
               `((?D . ,(shell-quote-argument (format "%s" dpi)))
                 (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
          (copy-file image-output-file tofile 'replace)
          (dolist (e post-clean)
                  (when (file-exists-p (concat texfilebase e))
                  (delete-file (concat texfilebase e))))
          image-output-file)))
      (deferred:nextc it
        (lambda () (funcall FINISH-FUNC it)))
      )

    ;; (async-start
    ;;  (lambda ()
    ;;     (require 'org)
    ;;     (let* ((err-msg (format "Please adjust `%s' part of `org-preview-latex-process-alist'." processing-type))
    ;;        (image-input-file
    ;;         (org-compile-file
    ;;          texfile latex-compiler image-input-type err-msg log-buf))
    ;;        (image-output-file
    ;;         (org-compile-file
    ;;          image-input-file image-converter image-output-type err-msg log-buf
    ;;          `((?D . ,(shell-quote-argument (format "%s" dpi)))
    ;;            (?S . ,(shell-quote-argument (format "%s" (/ dpi 140.0))))))))
    ;;     (copy-file image-output-file tofile 'replace)
    ;;     (dolist (e post-clean)
    ;;             (when (file-exists-p (concat texfilebase e))
    ;;             (delete-file (concat texfilebase e))))
    ;;     image-output-file)
    ;;     ) FINISH-FUNC)
    ))


(defun org-latex-preview-posframe-show (&rest _)
  (let ((fragment-string (org-latex-preview--get-latex-fragment)))
    (if fragment-string
          (deferred:$
            (deferred:wait 1000) ; 1000msec
            (deferred:nextc it
              (lambda (x)
                (message "Timer sample! : %s msec" x)
                (org-create-formula-image fragment-string "/tmp/test.png" org-format-latex-options nil)
                ))
            (deferred:nextc it
              (lambda (x)
                (message "here we should update the posframe : %s" x)
                ;; (with-current-buffer (get-buffer-create org-latex-preview-posframe--posframe-buffer)
                ;; ;; (setq buffer-read-only nil)
                ;; (erase-buffer)
                ;; (clear-image-cache)
                ;; (insert-image (create-image "/tmp/test.png" 'png nil))
                ;; ;; (setq buffer-read-only t)
                ;; ;; (insert-file-contents "/tmp/test.png")
                ;; )

                ;; (when (posframe-workable-p)
                ;; (posframe-show org-latex-preview-posframe--posframe-buffer
                ;;                 :position (point)
                ;;                 :poshandler org-latex-preview-posframe-posframe-position-handler
                ;;                 :posframe
                ;;                 ))
                ;; )
              )))
          ;; (measure-time (org-latex-preview-posframe--create-image fragment-string "/tmp/test.png" org-format-latex-options t
          ;;   (lambda (result)
          ;;    (with-current-buffer (get-buffer-create org-latex-preview-posframe--posframe-buffer)
          ;;      ;; (setq buffer-read-only nil)
          ;;      (erase-buffer)
          ;;      (clear-image-cache)
          ;;      (insert-image (create-image "/tmp/test.png" 'png nil))
          ;;      ;; (setq buffer-read-only t)
          ;;      ;; (insert-file-contents "/tmp/test.png")
          ;;      )

          ;;    (when (posframe-workable-p)
          ;;      (posframe-show org-latex-preview-posframe--posframe-buffer
          ;;                     :position (point)
          ;;                     :poshandler org-latex-preview-posframe-posframe-position-handler
          ;;                     :posframe
          ;;                     ))
          ;;    )))
          ;; (async-start
          ;;  (lambda () (require 'org) (org-create-formula-image fragment-string "/tmp/test.png" org-format-latex-options nil))
          ;;  (lambda (result)
          ;;    (message result)
          ;;    (with-current-buffer (get-buffer-create org-latex-preview-posframe--posframe-buffer)
          ;;      ;; (setq buffer-read-only nil)
          ;;      (erase-buffer)
          ;;      (insert-image (create-image "/tmp/test.png"))
          ;;      ;; (setq buffer-read-only t)
          ;;      ;; (insert-file-contents "/tmp/test.png")
          ;;      )

          ;;    (when (posframe-workable-p)
          ;;      (posframe-show org-latex-preview-posframe--posframe-buffer
          ;;                     :position (point)
          ;;                     :poshandler org-latex-preview-posframe-posframe-position-handler
          ;;                     :posframe
          ;;                     ))
          ;;    ))
          ;; (measure-time (org-latex-preview-posframe--create-image fragment-string "/tmp/test.png" org-format-latex-options t))
          ;; (with-current-buffer (get-buffer-create org-latex-preview-posframe--posframe-buffer)
          ;;   (image-mode-as-text)
          ;;   (erase-buffer)
          ;;   (insert-file-contents "/tmp/test.png")
          ;;   (image-mode)
          ;;   )

          ;; (when (posframe-workable-p)
          ;;   (posframe-show org-latex-preview-posframe--posframe-buffer
          ;;                  :position (point)
          ;;                  :poshandler org-latex-preview-posframe-posframe-position-handler
          ;;                  :posframe))
      (org-latex-preview-posframe-hide)
    ))
  )

(defun org-latex-preview-posframe-hide ()
  (interactive)
  (posframe-hide org-latex-preview-posframe--posframe-buffer)
  )

:autoload
(define-minor-mode org-latex-preview-posframe-mode
  "Instant preview of LaTeX in org-mode"
  nil nil nil
  (if org-latex-preview-posframe-mode
      (progn
        (setq org-latex-preview-posframe--output-buffer
              (concat org-latex-preview-posframe--output-buffer-prefix (buffer-name)))
        (add-hook 'post-command-hook #'org-latex-preview-posframe-show nil t)
        )
        (remove-hook 'post-command-hook #'org-latex-preview-posframe-show t)
    )
  )

(provide 'org-latex-preview-posframe)
