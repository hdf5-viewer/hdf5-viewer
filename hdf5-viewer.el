;;; hdf5-viewer.el --- Major mode for viewing HDF5 files -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Paul Minner, Peter Mao, Caltech

;; Author: Paul Minner <minner.paul@gmail.com>, Peter Mao <peter.mao@gmail.com>
;; Keywords: HDF5, data
;; Version: 1.1
;; Description: A major-mode for viewing HDF5 files.
;; Homepage: https://github.com/hdf5-viewer/hdf5-viewer
;; Package-Requires: ((emacs "29.1"))

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a major mode for viewing HDF5 files in Emacs.
;; It requires Python and Python's h5py package to be installed.
;; The Python logic is stored in h5parse.py, which should be installed
;; in the same location as hdf5-viewer.el.

;;; Code:
(require 'json)

(defgroup hdf5-viewer nil
  "Major mode for viewing HDF5 files."
  :group 'data)

(defcustom hdf5-viewer-python-command "python3"
  "Python interpreter to execute h5parse.py.  Must have h5py."
  :type 'string
  :group 'hdf5-viewer)

(makunbound 'hdf5-viewer-parse-command)
(defcustom hdf5-viewer-parse-command
  (format "%s %sh5parse.py"
          hdf5-viewer-python-command
          (file-name-directory (or load-file-name (buffer-file-name))))
  "Shell command to launch h5parse.py script."
  :type 'string
  :group 'hdf5-viewer)

(defvar hdf5-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'hdf5-viewer-read-field-at-cursor)
    (define-key map (kbd "SPC") 'hdf5-viewer-read-field-at-cursor)
    (define-key map (kbd "/")   'hdf5-viewer-read-field)
    (define-key map (kbd "TAB") 'hdf5-viewer-preview-field-at-cursor)
    (define-key map (kbd "'")   'hdf5-viewer-preview-field)
    (define-key map (kbd "b")   'hdf5-viewer-back)
    (define-key map (kbd "DEL") 'hdf5-viewer-back)
    (define-key map (kbd "S-SPC") 'hdf5-viewer-back)
    (define-key map (kbd "n")   'next-line)
    (define-key map (kbd "p")   'previous-line)
    (define-key map (kbd "w")   'hdf5-viewer-copy-field-at-cursor)
    map)
  "Keymap for HDF5-viewer mode.")

(defvar hdf5-viewer--buffer-filename nil
  "Temporary variable to pass the filename into the viewer buffer.

This avoids having to set the variable `buffer-file-name', which
would run the risk of overwiting the HDF5 file that is being
viewed.")

(defvar-local hdf5-viewer-file nil
  "Path to the current HDF5 file being viewed.")

(defvar-local hdf5-viewer-root nil
  "Path to begin printing the current HDF5 file fields.")

(defvar-local hdf5-viewer--parent-group ""
  "Parent group to the current view.

This is used to place the cursor when navigating back up the
tree.")

(defvar-local hdf5-viewer--forward-point-list nil
  "List of buffer point positions in the root heirarchy.

Saves buffer positions when navigating backwards.")

(defun hdf5-viewer--fix-path (path)
  "Remove extraneous '/'s from PATH."
  (let ((fsplit (file-name-split path))
        (npath ""))
    (dolist (val fsplit)
      (if (and (not (string= "" val))
               (not (string-prefix-p "/" val)))
          (setq npath (concat npath "/" val))))
    (if (string-empty-p npath)
        (setq npath "/"))
    npath))

(defun hdf5-viewer--get-field-at-cursor ()
  "Return field (group or dataset) at cursor position.

Return nil if there is nothing on this line."
  (end-of-line)
  (backward-word)
  (let ((field (thing-at-point 'filename t)))
    (when field
      (hdf5-viewer--fix-path (concat hdf5-viewer-root "/" field)))))

(defun hdf5-viewer--is-group (field)
  "Return t if FIELD is a group."
  (let ((output (hdf5-viewer--run-parser "--is-group" field hdf5-viewer-file)))
    (gethash "return" output)))

(defun hdf5-viewer--is-field (field)
  "Return t if FIELD is a field in the file."
  (let ((output (hdf5-viewer--run-parser "--is-field" field hdf5-viewer-file)))
    (gethash "return" output)))

(defun hdf5-viewer--run-parser (&rest args)
  "Run parser command with custom ARGS and return json output."
  (with-temp-buffer
    (let ((exit-code
           (apply #'call-process-shell-command
                  hdf5-viewer-parse-command nil t nil args)))
      (if (= exit-code 0)
          (progn
            (goto-char (point-min))
            (condition-case nil
                (let ((json-array-type 'list)
                      (json-object-type 'hash-table)
                      (json-false nil))
                  (json-read))
              (json-readtable-error
               (error "Failed to read parser output: Invalid JSON"))))
        (error "Parser script failed: %s"
               (buffer-substring (point-min) (point-max)))))))

(defun hdf5-viewer-back ()
  "Go back one group level and display to screen."
  (interactive)
  (unless (string= hdf5-viewer-root "/")
    (setq hdf5-viewer--parent-group (file-name-nondirectory hdf5-viewer-root))
    (push (cons hdf5-viewer-root (point)) hdf5-viewer--forward-point-list)
    (setq hdf5-viewer-root (hdf5-viewer--fix-path (file-name-directory hdf5-viewer-root)))
    (hdf5-viewer--display-fields -1)))

(defun hdf5-viewer--display-fields (direction)
  "Display current root group fields and attributes to buffer.

DIRECTION indicates which way we are navigating the heirarchy:
  0: initialization
  1: forward
 -1: backwards"
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s %s\n\n"
                    (propertize "Root:" 'face 'bold)
                    hdf5-viewer-root))
    (let* ((output (hdf5-viewer--run-parser "--get-fields" hdf5-viewer-root hdf5-viewer-file))
           (attrs  (hdf5-viewer--run-parser "--get-attrs"  hdf5-viewer-root hdf5-viewer-file))
           (num-attrs (hash-table-count attrs))
           (field-template "%-8s %-15s %20s  %-30s\n")
           (attr-template  "%-45s  %-30s\n"))
      ;; display GROUPS and DATASETS
      (insert (propertize (format field-template "*type*" "*dims*" "*range*" "*name*")
                          'face '('bold 'underline)))
      (maphash (lambda (key val)
                 (let ((type  (gethash "type"  val)))
                   (cond ((string= type "group")
                          (insert (format field-template
                                           "group" "N/A" ""
                                          (format "%s/" key))))
                         ((string= type "dataset")
                          (let ((dtype (gethash "dtype" val))
                                (shape (gethash "shape" val))
                                (range (gethash "range" val "")))
                            (insert (format field-template
                                            dtype shape range key))))
                         ((string= type "other")
                          (insert (format field-template "other" "" "" key))))))
               output)
      ;; display ATTRIBUTES
      (when (> num-attrs 0)
        (insert "\n\n")
        (insert (propertize (format attr-template "*value*" "*attribute*")
                            'face '('bold 'underline)))
        (maphash (lambda (attrkey attrval)
                   (let ((attrval-substrings (split-string attrval "\n")))
                     ;; print `attrkey' on this line
                     (insert (format attr-template (pop attrval-substrings) attrkey))
                     ;; if `attrval' breaks over multiple lines, print remainder w/o key
                     (dotimes (_junk (length attrval-substrings))
                       (insert (pop attrval-substrings) "\n"))))
                 attrs)))
    ;; set the point
    (superword-mode)
    (cond ((= direction -1)
           (goto-char (point-max))
           (search-forward (concat " " hdf5-viewer--parent-group "/") nil nil -1))
          ((and (= direction  1)
                (> (length hdf5-viewer--forward-point-list) 0))
           ;; forward navigation is more complicated because we can come up one
           ;; branch and then down a different branch, hence the check against
           ;; hdf5-viewer-root.
           (let ((fwd (pop hdf5-viewer--forward-point-list)))
             (if (string= hdf5-viewer-root (car fwd))
                 (goto-char (cdr fwd))
               (setq hdf5-viewer--forward-point-list nil) ; clear fwd history on branch change
               (goto-char (point-min))
               (forward-line 3))))
          (t
           (goto-char (point-min))
           (forward-line 3)))
    (end-of-line)
    (backward-word)
    (set-goal-column nil)
    (set-buffer-modified-p nil)))

(defun hdf5-viewer-preview-field-at-cursor ()
  "Display field contents at cursor in minibuffer."
  (interactive)
  (let ((field (hdf5-viewer--get-field-at-cursor)))
    (when field
      (hdf5-viewer-preview-field field))))

(defun hdf5-viewer-preview-field (field)
  "Display selected FIELD contents in minibuffer."
  (interactive "sEnter path: ")
  (when (hdf5-viewer--is-field field)
    (let ((field  (hdf5-viewer--fix-path field))
          (output (hdf5-viewer--run-parser "--preview-field" field hdf5-viewer-file)))
      (message (format "%s %s %s:\n%s"
                       (propertize field 'face 'bold)
                       (gethash "shape" output "")
                       (gethash "dtype" output "")
                       (gethash "data" output))))))

(defun hdf5-viewer-read-field-at-cursor ()
  "Display field contents at cursor in new buffer."
  (interactive)
  (let ((field (hdf5-viewer--get-field-at-cursor)))
    (when field
      (hdf5-viewer-read-field field))))

(defun hdf5-viewer-read-field (field)
  "Display specified FIELD contents in new buffer."
  (interactive "sEnter path: ")
  (let ((field (hdf5-viewer--fix-path field)))
    (when (hdf5-viewer--is-field field)
      (if (hdf5-viewer--is-group field)
          (let ((field-root (hdf5-viewer--fix-path (file-name-directory field))))
            (if (string= hdf5-viewer-root field-root)
                (progn ; normal forward navigation
                  (setq hdf5-viewer-root field)
                  (hdf5-viewer--display-fields 1))
              ;; user-input jump navigation
              (setq hdf5-viewer-root field
                    hdf5-viewer--forward-point-list nil)
              (hdf5-viewer--display-fields 0)))
        (let* ((output (hdf5-viewer--run-parser "--read-dataset" field hdf5-viewer-file))
               (parent-buf (string-split (buffer-name (current-buffer)) "*" t))
               (dataset-buf (concat "*" (pop parent-buf) field "*" (apply 'concat parent-buf))))
          (with-current-buffer (get-buffer-create dataset-buf)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (setq-local truncate-lines t)
              (insert (propertize (format "%s %s %s:"
                                          (propertize field 'face 'bold)
                                          (gethash "shape" output)
                                          (gethash "dtype" output)) 'face 'underline))
              (insert "\n\n" (gethash "data" output))
              (goto-char (point-min))
              (special-mode)
              (display-buffer (current-buffer) '((display-buffer-same-window))))))))))

(defun hdf5-viewer-copy-field-at-cursor ()
  "Interactively put field-at-cursor into the kill ring."
  (interactive)
  (let ((field-name (hdf5-viewer--get-field-at-cursor)))
    (if field-name
        (let ((field-type (if (hdf5-viewer--is-field field-name) "field" "attribute")))
          (kill-new field-name)
          (message (format "Copied HD5 %s name: %s" field-type field-name)))
      (message "No field or attribute found on this line."))))

;;;###autoload
(define-derived-mode hdf5-viewer-mode special-mode "HDF5"
  "Major mode for viewing HDF5 files."
  (setq-local buffer-read-only t)
  (setq-local hdf5-viewer-file hdf5-viewer--buffer-filename)
  (setq-local hdf5-viewer-root "/")
  (hdf5-viewer--display-fields 0))

;;;###autoload
(defun hdf5-viewer-maybe-startup (&optional filename _wildcards)
  "Advice to avoid loading HDF5 files into the buffer.

HDF5 files can be very large and `hdf5-viewer' does not need the file
contents to be loaded before operating on the file.  This advice
looks for the HDF5 signature in the first 8 bytes of a file.  If
it is not HDF5, then proceed with `find-file'.  If it is HDF5, then open a
buffer named \"*hdf5: FILENAME*\" and start hdf5-viewer.
`find-file' is then bypassed.

The WILDCARDS flag is not used by this advice and is passed on to
`find-file'.  HDF5 files referenced by wildcards will be opened
as normal files, without `hdf5-viewer'.

For files with the same nondirectory names, the buffer names are
disambituated with `generate-new-buffer-name', which appends an
incrementing \"<#>\" to the buffer name.  The `buffer-file-name'
is set uniquely, via `set-visited-file-name', to the HDF5
filename with \"-hdf5-viewer\" appended to the end."

  (if (not (file-regular-p filename)) nil
    (let ((hdf5-signature (unibyte-string #x89 #x48 #x44 #x46 #x0d #x0a #x1a #x0a))
          (filehead (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally filename nil 0 8 t)
                     (buffer-substring-no-properties 1 9))))
      (when (string= filehead hdf5-signature)
        (let* ((this-buffer-filename (concat filename "-hdf5-viewer"))
               (this-buffer-name (format "*hdf5: %s*" (file-name-nondirectory filename)))
               (this-buffer (find-buffer-visiting this-buffer-filename)))
          (if this-buffer
              (switch-to-buffer this-buffer)
            (let ((new-buffer-name (generate-new-buffer-name this-buffer-name)))
              (switch-to-buffer (get-buffer-create new-buffer-name))
              (setq default-directory (file-name-directory filename))
              (setq hdf5-viewer--buffer-filename filename)
              (set-visited-file-name this-buffer-filename)
              (rename-buffer new-buffer-name)
              (hdf5-viewer-mode))))
        t)))) ;; bypass find-file

;;;###autoload
(advice-add 'find-file :before-until #'hdf5-viewer-maybe-startup)

(provide 'hdf5-viewer)

;;; hdf5-viewer.el ends here
