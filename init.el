;; Init file to use with the orgmode plugin.  -*- lexical-binding: t; -*-

;; Load org-mode
;; Requires org-mode v8.x

(require 'org)
(require 'ob-core)
(require 'ob-exp)
(require 'ox)
(require 'ox-html)

(require 'package)
(setq package-load-list '(all))
(package-initialize)
(eval '(use-package ox-html-stable-ids :ensure
         :vc (:url "https://codeberg.org/jkreeftmeijer/ox-html-stable-ids.el.git")
         :config (org-html-stable-ids-add)
         :custom (org-html-stable-ids t)))

;;; Custom configuration for the export.

;;; Add any custom configuration that you would like to 'conf.el'.
(defvar nikola-use-pygments t
  "Set to `t' to use pygments to highlight src blocks.")
(defvar nikola-post-link-regexp (regexp-opt '(".org" ".md" ".rst"))
  "Regular expression to match post links, usually post file suffixes.")
(defvar nikola-ignored-prefix "/files/"
  "Do not treat files under this prefix as post files.")
(defvar nikola-pygments-html-extra-options ""
  "Extra options for pygments html formatter. Run `pygmentize -H formatter
html' for available ones.")

;; Better defaults?
(setq org-export-with-toc t
      org-export-with-section-numbers t
      org-startup-folded 'showeverything
      org-html-doctype "html5"
      org-html-html5-fancy t)

;; Stop evaluating things
(setq org-export-use-babel t)
(defun set-header (sym key value)
  (if-let* ((list (symbol-value sym))
            (pair (assq key list)))
      (setcdr pair value)
    (set sym (cons (cons key value) (symbol-value sym)))))
(set-header 'org-babel-default-header-args :eval "never-export")
(set-header 'org-babel-default-header-args :exports "both")
(set-header 'org-babel-default-inline-header-args :eval "never-export")
(set-header 'org-babel-default-inline-header-args :exports "both")
(set-header 'org-babel-default-header-args:elisp :eval "never-export")
(set-header 'org-babel-default-header-args:elisp :exports "both")
(set-header 'org-babel-default-header-args:elisp :lexical "yes")
;; Just don't warn about :eval never-export
(define-advice org-babel-check-evaluate (:around (fun info))
  (let ((inhibit-message t)) (funcall fun info)))
;; Don't evaluate inline src blocks, (:eval "never-export") does not work.
(define-advice org-babel-exp-results (:override (&rest _args)))

;; Load additional configuration from conf.el
(add-to-list 'load-path (file-name-directory load-file-name))
(load "init-experimental")

;;; Macros

;; Load Nikola macros
(defvar nikola-macro-templates
  (eval-when-compile
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "macros.org"
                         (file-name-directory (or byte-compile-current-file
                                                  load-file-name))))
      (org-mode)
      (org-macro--collect-macros))))

;; Use pygments highlighting for code
(defun pygmentize (lang code label results)
  "Use Pygments to highlight the given code and return the output"
  (with-temp-buffer
    (insert code)
    (let ((lang (get-pygments-language lang)))
      (shell-command-on-region (point-min) (point-max)
                               (format "pygmentize -f html -O cssclass='highlight%s',wrapcode=1,%s %s"
                                       (if results " results" "")
                                       nikola-pygments-html-extra-options
                                       (if lang (concat "-l " lang) "-g"))
                               (buffer-name) t))
    (when label
      (goto-char (point-min))
      (re-search-forward "\\`<div")
      (replace-match (concat "<div" label)))
    (buffer-string)))
;; Add a "results" CSS class to common blocks when they are marked as #+RESULTS
(defvar ox-nikola--block-is-results nil)
(dolist (ox-block '(org-html-example-block
                    org-html-quote-block
                    org-html-src-block))
  (advice-add
   ox-block :around
   (lambda (old-block block contents info)
     (let ((ox-nikola--block-is-results
            (org-element-property :results block)))
       (funcall old-block block contents info)))))
(define-advice org-html--make-attribute-string (:around (old-maker attributes))
  (when ox-nikola--block-is-results
    (let ((class-val (plist-get attributes :class)))
      (setq attributes (plist-put attributes :class
                                  (if class-val
                                      (concat "results " class-val)
                                    "results")))))
  (funcall old-maker attributes))
;; Add a "results" CSS class for fixed-width blocks (lines prefixed with ": ")
(defconst ox-nikola-assert-fixed-width-html
  "<pre class=\"example\">")
(define-advice org-html-fixed-width (:filter-return (html))
  (if (string-prefix-p ox-nikola-assert-fixed-width-html
                       html)
      (concat "<pre class=\"results example\">" (substring html (length ox-nikola-assert-fixed-width-html)))
    (error "unexpected fixed-width html")))
;; Add a "table" div wrapper for tables (so that one can make tables scrollable on mobile)
(define-advice org-html-table (:around (f &rest args))
  (let ((is-results (org-element-property :results (car args)))
        (html (apply f args)))
    (concat (format "<div class=\"table%s\">" (if is-results " results" ""))
            html "</div>")))

(defconst org-pygments-language-alist
  '(("asymptote" . "asymptote")
    ("awk" . "awk")
    ("c" . "c")
    ("console" . "console")
    ("c++" . "cpp")
    ("cpp" . "cpp")
    ("clojure" . "clojure")
    ("css" . "css")
    ("d" . "d")
    ("emacs-lisp" . "emacs-lisp")
    ("elisp" . "elisp")
    ("F90" . "fortran")
    ("gnuplot" . "gnuplot")
    ("groovy" . "groovy")
    ("haskell" . "haskell")
    ("java" . "java")
    ("js" . "js")
    ("julia" . "julia")
    ("latex" . "latex")
    ("lisp" . "lisp")
    ("makefile" . "makefile")
    ("matlab" . "matlab")
    ("mscgen" . "mscgen")
    ("ocaml" . "ocaml")
    ("octave" . "octave")
    ("perl" . "perl")
    ("picolisp" . "scheme")
    ("python" . "python")
    ("r" . "r")
    ("ruby" . "ruby")
    ("sass" . "sass")
    ("scala" . "scala")
    ("scheme" . "scheme")
    ("sh" . "sh")
    ("shell-session" . "shell-session")
    ("sql" . "sql")
    ("sqlite" . "sqlite3")
    ("tcl" . "tcl")
    ("text" . "text"))
  "Alist between org-babel languages and Pygments lexers.
lang is downcased before assoc, so use lowercase to describe language available.
See: http://orgmode.org/worg/org-contrib/babel/languages.html and
http://pygments.org/docs/lexers/ for adding new languages to the mapping.")

(defvar org-pygments-detected-languages nil
  "List of languages supported by Pygments, detected at runtime.")

(defun get-pygments-language (lang)
  "Try to find a Pygments lexer that matches the provided language."
  (let ((pygmentize-lang (cdr (assoc lang org-pygments-language-alist))))
    (if pygmentize-lang
        pygmentize-lang
      (when (null org-pygments-detected-languages)
        (with-temp-buffer
          ;; Extracts supported languages from "pygmentize -L lexers --json"
          (setq org-pygments-detected-languages
                (if (= 0 (call-process "pygmentize" nil (current-buffer) nil
                                       "-L" "lexers" "--json"))
                    (progn
                      (goto-char (point-min))
                      (if (featurep 'json)
                          ;; Use json to parse supported languages
                          (let ((lexers (alist-get 'lexers
                                                   (json-parse-buffer
                                                    :object-type 'alist
                                                    :array-type 'list))))
                            (mapcan (lambda (lexer)
                                      (alist-get 'aliases (cdr lexer)))
                                    lexers))
                        ;; Use regexp on a best effort basis
                        (let ((case-fold-search nil) langs)
                          (while (re-search-forward "\"\\([a-z+#/-]+\\)\""
                                                    nil t)
                            (let ((s (match-string 1)))
                              (unless (member s '("lexers" "aliases"
                                                  "filenames" "mimetypes"))
                                (push s langs))))
                          langs)))
                  ;; Fallback
                  '("text")))))
      (if (member lang org-pygments-detected-languages) lang))))

;; Override the html export function to use pygments
(defun org-html--label (block)
  (let ((lbl (org-element-property :name block)))
    (if lbl (format " id=\"%s\"" lbl) "")))
(define-advice org-html-stable-ids--extract-id (:around (fun datum))
  (or (org-element-property :name datum)
      (funcall fun datum)))
(define-advice org-html-src-block (:around (old-src-block src-block contents info))
  (if (or (not nikola-use-pygments)
          (org-export-read-attribute :attr_html src-block :textarea))
      (funcall old-src-block src-block contents info)
    (let ((lang (or (org-element-property :language src-block) ""))
          (code (car (org-export-unravel-code src-block)))
          (label (org-html--label src-block))
          (results (org-element-property :results src-block)))
      (pygmentize (downcase lang) code label results))))
(define-advice org-html-inline-src-block (:around (old-fun src-block contents info))
  (if (not nikola-use-pygments)
      (funcall old-fun src-block contents info)
    (let* ((lang (downcase (org-element-property :language src-block)))
           (code (org-element-property :value src-block))
           (label (org-html--label src-block))
           (nikola-pygments-html-extra-options "nowrap=1")
           (html (pygmentize lang code nil nil)))
      ;; Default CSS selector: .code .codetable span
      (format "<span class=\"code\"><code class=\"codetable\"%s>%s</code>" label html))))

;; Export images with custom link type ([[img-url:/images/xxx.png]])
(defun org-custom-link-img-url-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"%s\" alt=\"%s\"/>" path desc))))
(org-link-set-parameters "img-url " :export 'org-custom-link-img-url-export)

;; Handle [[file:...]] links
(require 'org-macs)
(setq org-html-link-org-files-as-html nil)
(defvar nikola-root-dir
  (file-name-concat (file-name-directory (or load-file-name ".")) "../..")
  "The directory this Nikola blog (and the conf.py) is in.")
(defun org-nikola--fix-media-path (filename)
  (when (string-prefix-p "file:" filename)
    (setq filename (substring filename 5)))
  (if (or (org-url-p filename)
          (not (file-in-directory-p filename nikola-root-dir)))
      filename
    (concat "/" (file-relative-name filename nikola-root-dir))))
;; Fix media [[file:...]] links
(define-advice org-html--format-image (:around (original path attrs info))
  (funcall original (org-nikola--fix-media-path path) attrs info))
;; Fix post [[file:...]] links
(define-advice org-export-file-uri (:around (original file))
  (let ((fixed (org-nikola--fix-media-path file)))
    (cond
     ((string-prefix-p nikola-ignored-prefix fixed)
      (substring fixed (1- (length nikola-ignored-prefix))))
     ((and (not (eq file fixed))
           (string-match-p nikola-post-link-regexp file))
      (concat "link://filename" fixed))
     ((string-prefix-p "/" file)
      (concat "link://" file))
     (t (funcall original file)))))

;; Support for magic links (link:// scheme)
(org-link-set-parameters
 "link"
 :export (lambda (path desc backend)
           (cond
            ((eq 'html backend)
             (format "<a href=\"link:%s\">%s</a>"
                     path (or desc path))))))

;; Export function used by Nikola.
(defun nikola-html-export (infile outfile)
  "Export the body only of the input file and write it to
specified location."
  (with-current-buffer (find-file infile)
    (org-macro-replace-all nikola-macro-templates)
    (org-html-export-as-html nil nil t t)
    (write-file outfile nil)))

;; Load user config last to allow advicing
(add-to-list 'load-path (file-name-parent-directory (file-name-directory load-file-name)))
(load "orgconf" t)
