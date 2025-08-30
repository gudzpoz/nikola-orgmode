;; -*- lexical-binding: t; -*-
(require 'ox)
(require 'package)

;; Org configs
(setq org-export-with-toc t
      org-export-with-section-numbers t)

(setq org-export-use-babel t)
(add-to-list 'org-babel-default-header-args
             '(:eval . "never-export"))
(add-to-list 'org-babel-default-header-args
             '(:exports . "both"))
(add-to-list 'org-babel-default-header-args:elisp
             '(:eval . "never-export"))
(add-to-list 'org-babel-default-header-args:elisp
             '(:exports . "both"))
;; Just don't warn about :eval never-export
(define-advice org-babel-check-evaluate (:around (fun info))
  (let ((inhibit-message t)) (funcall fun info)))

(add-to-list 'org-babel-default-header-args
             '(:lexical . "t"))

(setq package-load-list '(all))
(package-initialize)

(setq org-html-doctype "html5"
      org-html-html5-fancy t)

;; Semantic HTML heading ID
(use-package ox-html-stable-ids :ensure
  :vc (:url "https://codeberg.org/jkreeftmeijer/ox-html-stable-ids.el.git")
  :config (org-html-stable-ids-add)
  :custom (org-html-stable-ids t))

;; Semantic ID for Chinese chars
(defvar pinyin-inverse-map
  (eval-when-compile
    (with-temp-buffer
      (quail-use-package "chinese-py" "quail/PY")
      (let ((decode-map (list 'decode-map)) (map (make-char-table nil)))
        (quail-build-decode-map (list (quail-map)) "" decode-map 0 most-positive-fixnum)
        (dolist (pair (cdr decode-map))
          (pcase-let ((`(,pinyin . ,chars) pair))
            (if (fixnump chars)
                (aset map chars pinyin)
              (cl-loop for zh-char across chars do
                       (aset map (string-to-char zh-char) pinyin)))))
        map)))
  "A char-table mapping from chars to their Chinese pinyin.")
(defun char-to-pinyin (c)
  (if-let* ((pinyin (aref pinyin-inverse-map c)))
      (concat " " pinyin " ")
    (string c)))
(define-advice org-html-stable-ids--to-kebab-case (:around (orig string))
  (funcall orig (mapconcat #'char-to-pinyin string)))

;; Line breaks
(defun clear-single-linebreak-in-cjk-string (string)
  "clear single line-break between cjk characters that is usually soft line-breaks"
  (let* ((regexp "\\([\u4E00-\u9FA5]\\)[ \n]+\\([\u4E00-\u9FA5]\\)")
         (start (string-match regexp string)))
    (while start
      (setq string (replace-match "\\1\\2" nil nil string)
            start (string-match regexp string start))))
  string)
(defun ox-html-clear-single-linebreak-for-cjk (string backend _info)
  (when (org-export-derived-backend-p backend 'html)
    (clear-single-linebreak-in-cjk-string string)))
(add-to-list 'org-export-filter-final-output-functions
             'ox-html-clear-single-linebreak-for-cjk)

;; Footnotes
(defvar footnote-definitions (make-hash-table :test 'eq))
(defun org-nikola--find-footnote-def (footnote info)
  (let ((footnote-defs (with-memoization
                           (gethash info footnote-definitions)
                         (org-export-collect-footnote-definitions info)))
        (inner (cddr footnote)))
    (cl-block loop
      (cl-dolist (def footnote-defs)
        (when (eq (nth 2 def) inner)
          (cl-return def))))))
(defsubst org-footnote--label-id (label n)
  ;; Do not assign number labels as they appear in Org mode
  ;; - the footnotes are re-numbered by
  ;; `org-export-get-footnote-number'.  If the label is not
  ;; a number, keep it.
  (if (and (stringp label)
           (equal label (number-to-string (string-to-number label))))
      n
    label))
(defun org-nikola-footnote-definition (footnote _contents info)
  (pcase-let ((`(,n ,label ,def) (org-nikola--find-footnote-def footnote info)))
    (setq label (org-footnote--label-id label n))
    (let ((anchor (org-html--anchor
                   (format "fn.%s" label)
                   n
                   (format " class=\"footnum\" href=\"#fnr.%s\" role=\"doc-backlink\"" label)
                   info))
          (contents (org-trim (org-export-data def info))))
      (format "<div class=\"footdef\">%s %s</div>\n"
              (format (plist-get info :html-footnote-format) anchor)
              (format "<div class=\"footpara\" role=\"doc-footnote\">%s</div>" contents)))))
;; Add footnote definition handler
(let* ((backend (org-export-get-backend 'html))
       (transcoders (org-export-backend-transcoders backend)))
  (setf (org-export-backend-transcoders backend)
        (cons '(footnote-definition . org-nikola-footnote-definition)
              transcoders)))
;; Handle inline footnotes
(define-advice org-html-footnote-reference (:around (orig footnote contents info))
  (let ((def (org-nikola--find-footnote-def footnote info))
        (ref (funcall orig footnote contents info)))
    (if def
        (let ((label (org-footnote--label-id (cadr def) (car def))))
          (format "<span id=\"fn.%s\" class=\"inline-footdef\"> (%s %s) </span>"
                  label ref contents))
      ref)))
;; Disable footnote sections
(define-advice org-html-footnote-section (:override (&rest _)) nil)
;; In case you have a section named "Footnote", setting a different
;; `org-footnote-section' at build time should prevent the advice from hiding
;; your "Footnote" section.
(setq org-footnote-section "this section is disabled and should never show up")
