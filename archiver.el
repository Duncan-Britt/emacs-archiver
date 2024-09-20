;;; archiver.el --- Emacs Agenda Archiver                    -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Duncan Britt

;; Author: Duncan Britt <dbru997@gmail.com>
;; Homepage: TODO
;; Keywords: Graphics,images,themes

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "29"))

;; The software is provided “as is”, without warranty of any kind, express or implied,
;; including but not limited to the warranties of merchantability, fitness for a particular
;; purpose and noninfringement. in no event shall the authors or copyright holders be liable
;; for any claim, damages or other liability, whether in an action of contract, tort or
;; otherwise, arising from, out of or in connection with the software or the use or other
;; dealings in the software.

;;; Commentary:
;; This package is used to move org agenda headings and subheadings to an archive org file
;; in such a way that the org tree structure is mirrored by the archive file, without
;; duplicating or clobbering existing branches in the archive file.

;; This package also provides an additional feature, which is that it provides an
;; interactive function for moving the open file to an archive directory, and, as above
;; merges the directory structure into the archive directory without duplicating or
;; clobbering the existing paths.

;; Extant bug:
;; This package make the simplifying assumption that the immediate child headings will be
;; distinct under the parent heading.  That is, no duplicates.  For the author's use case,
;; it's a reasonable assumption, and handling the case where it's not true would take a lot
;; of effort.
;;;

;;; Code:
(require 'cl-lib)
(require 'org)

(defvar *archiver-agenda-archive-location* nil
  "Location of archive for Org agenda.")

(defun archiver-parse-current-heading ()
  "Parse the current heading and its body in a format suitable for the tree."
  (let ((heading (substring-no-properties (org-get-heading t t t))) ;; Get heading without any stars
        (body (archiver-get-body-text)))
    (list heading body '())))

(defun archiver-get-body-text ()
  "Get the body text of the current Org heading."
  (save-excursion
    (let ((end (org-entry-end-position)))
      (forward-line)
      (buffer-substring-no-properties (point) (min (point-max) end)))))

(defun archiver-parse-subtree-with-children ()
  "Parse the current subtree recursively into a nested list structure."
  (save-excursion
    (org-back-to-heading t)
    (let* ((heading (substring-no-properties (org-get-heading nil nil t nil))) ;; Heading without stars
           (body (archiver-get-body-text))
           (children (archiver-parse-children)))
      (list heading body children))))

(defun archiver-parse-children ()
  (save-excursion
    (let ((children nil))
      (when (org-goto-first-child)
        (push (archiver-parse-subtree-with-children) children)
        (while (org-goto-sibling)
          (push (archiver-parse-subtree-with-children) children)))
      (reverse children))))

(defun archiver-get-ancestry-and-subtree ()
  "Return the current Org mode subtree along with its ancestors in a nested list format."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (let ((subtree (archiver-parse-subtree-with-children)))
      ;; Move up to collect ancestors
      (let ((ancestors '()))
        (while (org-up-heading-safe)
          (push (archiver-parse-current-heading) ancestors))
        ;; Combine ancestors and subtree into the desired format        
        (let ((tree (reduce (lambda (acc heading) (list (car heading) (cadr heading) (list acc)))
                            (reverse ancestors)
                            :initial-value subtree)))
          (message "%s" (prin1-to-string tree)) ;; Display the result
          tree)))))

(defun get-archive-location ()
  "Return path of agenda archive, if set.
Otherwise display an error message."
  (if *archiver-agenda-archive-location*
      *archiver-agenda-archive-location*
    (error "*archiver-agenda-archive-location* has not been specified")))

(get-archive-location)

(defun archiver-archive-heading ()
  "Write the current Org subtree to the archive file.
Merge it with the existing tree."
  (interactive)
  (save-excursion
    (let ((open-file (buffer-file-name (window-buffer (minibuffer-selected-window)))))
      (when (not (member open-file org-agenda-files))
        (error "Cannot archive outside of agenda file"))
      (let ((tree-to-archive (archiver-get-ancestry-and-subtree)))
        (with-current-buffer (find-file-noselect (get-archive-location))          
          (let ((archive-tree (archiver-parse-buffer)))
            (erase-buffer)
            (goto-char (point-min))
            (insert (tree--to-org-string (tree--merge-subtree archive-tree
                                                              tree-to-archive))))))))
  (archiver--delete-subheading))

(defun archiver--delete-subheading ()
  "Remove the subheading at point from the buffer and save."  
  (save-excursion    
    (org-back-to-heading t)    
    (org-cut-subtree)    
    (save-buffer)))


(defun tree--to-org-string (tree)
  "Serialize TREE as org text."
  (concat (tree--get-pre-heading-text tree)
          (mapconcat (lambda (child-tree)
                       (tree--subtree-to-org-string child-tree 1))
                     (tree--get-children tree))))

(defun tree--subtree-to-org-string (st heading-level)
  "Serialize ST as org text with appropriate HEADING-LEVEL"
  (concat (make-string heading-level ?*) " "
          (tree--get-heading st) "\n"
          (tree--get-body st)
          (mapconcat (lambda (child-tree)
                       (tree--subtree-to-org-string child-tree (1+ heading-level)))
                     (tree--get-children st))))

(defun tree--get-pre-heading-text (tree)
  "Return pre-heading text from TREE."
  (second tree))

(defun archiver-parse-buffer ()
  "Parse the current org buffer into a tree."
  (interactive)
  (save-excursion
    (goto-char (point-min))    
    (let ((children (archiver-parse-children)))
      (list "ROOT"
            (archiver-get-pre-heading-text)
            children))))

(defun archiver-get-pre-heading-text ()
  "Return the text before the first heading in the current Org buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading (re-search-forward org-heading-regexp nil t)))
      (if first-heading
          (buffer-substring-no-properties (point-min) (match-beginning 0))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun tree--replace-nth-child (tree replacement n)
  "Return a new TREE with the REPLACEMENT child in place of the existing child N."
  (list (tree--get-heading tree)
        (tree--get-body tree)
        (cl-loop for i from 0
                 for child in (tree--get-children tree)
                 collect (if (= i n)
                             replacement
                           child))))

(defun tree--merge-subtree (tree st)
  "Return a new TREE with the subtree ST merged into the TREE.
This only works if ST is a straight line tree."
  (if-let (nth (tree--matching-child-idx st tree))
      (tree--replace-nth-child tree
                               (tree--merge-subtree (tree--get-nth-child tree nth)
                                                    (tree--get-nth-child st 0))
                               nth)
    (tree--append-child tree st)))

(defun tree--get-nth-child (tree n)
  "Get child of TREE at index N."
  (nth n (tree--get-children tree)))

(defun tree--matching-child-idx (st tree)
  "Return the index of the child of TREE matching ST or NIL."
  (tree--matching-child-idx-rec (tree--get-heading st) (tree--get-children tree) 0))

(defun tree--matching-child-idx-rec (heading children idx)
  "Return the IDX of the child in CHILDREN matching HEADING or NIL."
  (cond ((null children) nil)
        ((string= heading (tree--get-heading (first children)))
         idx)
        (t (tree--matching-child-idx-rec heading (cdr children) (1+ idx)))))

(defun tree--append-child (tree st)
  "Return new TREE with additional child ST."
  (list (tree--get-heading tree)
        (tree--get-body tree)
        (cons st
              (tree--get-children tree))))

(defun tree--get-heading (tree)
  "Return heading of TREE."
  (first tree))

(defun tree--get-body (tree)
  "Return body of TREE."
  (second tree))

(defun tree--get-children (tree)
  "Return children of TREE as list of trees."
  (third tree))

(defun tree= (t1 t2)
  "Return non-NIL if T1 = T2, else NIL."  
  (cond ((and (null t1)
              (null t2))
         t)
        ((null t1) nil)
        ((null t2) nil)
        ((and (string= (tree--get-heading t1)
                       (tree--get-heading t2))
              (string= (tree--get-body t1)
                       (tree--get-body t2)))
         (and (cl-every #'tree=
                        (tree--get-children t1)
                        (tree--get-children t2))
              (= (length (tree--get-children t1))
                 (length (tree--get-children t2)))))
        (t nil)))

(defun treeify (headings)
  "HEADINGS: \\='(\"a\" \"b\" \"c\") => \\='(\"a\" \"\" ((\"b\" \"\" (\"c\" \"\" ()))))."
  (cond ((null headings) nil)
        ((= 1 (length headings))
         (list (first headings) "" nil))
        (t (list (first headings) "" (list (treeify (cdr headings)))))))

;; (defvar test-tree '("R" "body" (("a" "" (("b" "" (("h" "" ())
;;                                                   ("c" "" (("i" "" ())
;;                                                            ("j" "" ())))))
;;                                          ("z" "" ())))
;;                                 ("k" "" (("f" "" (("g" "" ()))))))))

;; (defvar test-subtree '("a" "" (("b" "" (("c" "" (("d" "" (("e" "" ()))))))))))

;; (defvar expected '("R" "body" (("a" "" (("b" "" (("h" "" ())
;;                                                  ("c" "" (("d" "" (("e" "" ())))
;;                                                           ("i" "" ())
;;                                                           ("j" "" ())))))
;;                                         ("z" "" ())))
;;                                ("k" "" (("f" "" (("g" "" ()))))))))

;; (tree= expected (tree--merge-subtree test-tree test-subtree))
;; (pair-tree (tree--merge-subtree test-tree test-subtree))

;; The above code is for archiving org headings. What follows is for moving files to an archive directory.
(defvar my-archive-dir "~/archive"
  "Directory where files will be archived.")

(defun archiver-archive-open-file ()
  "Move the current file to the archive directory."
  (interactive)
  (if (buffer-file-name)
      (let* ((file-path (buffer-file-name))
             (relative-path (file-relative-name file-path (getenv "HOME")))
             (archive-path (expand-file-name relative-path my-archive-dir)))
        (if (file-exists-p file-path)
            (progn
              (save-buffer)
              (make-directory (file-name-directory archive-path) t)
              (rename-file file-path archive-path t)
              (kill-buffer)
              (message "Archived: %s" archive-path))
          (message "File does not exist: %s" file-path)))
    (message "No file is associated with this buffer.")))

(provide 'archiver)
;;; archiver.el ends here
