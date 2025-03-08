;;; hdf5-mode.el --- Major mode for viewing HDF5 files -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Paul Minner, Peter Mao, Caltech

;; Author: Paul Minner <minner.paul@gmail.com>, Peter Mao <peter.mao@gmail.com>
;; Keywords: HDF5, data
;; Version: 1.1
;; Description: A major-mode for viewing HDF5 files.
;; Homepage: https://github.com/paublo96/emacs-hdf5
;; Package-Requires: ((emacs "27.1") (json "??"))

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
;; in the same location as hdf5-mode.el.

;;; Code:
(require 'json)

(defgroup hdf5-mode nil
  "Major mode for viewing HDF5 files."
  :group 'data)

(defcustom hdf5-mode-python-command "python3"
  "Python interpreter to execute h5parse.py.  Must have h5py."
  :type 'string
  :group 'hdf5-mode)

(defcustom hdf5-mode-parse-command
  (format "%s %sh5parse.py"
          hdf5-mode-python-command
          (file-name-directory (or load-file-name (buffer-file-name))))
  "Shell command to launch h5parse.py script."
  :type 'string
  :group 'hdf5-mode)

(defvar hdf5-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'hdf5-read-field-at-cursor)
    (define-key map (kbd "SPC") 'hdf5-read-field-at-cursor)
    (define-key map (kbd "/")   'hdf5-read-field)
    (define-key map (kbd "TAB") 'hdf5-preview-field-at-cursor)
    (define-key map (kbd "'")   'hdf5-preview-field)
    (define-key map (kbd "b")   'hdf5-back)
    (define-key map (kbd "DEL") 'hdf5-back)
    (define-key map (kbd "S-SPC") 'hdf5-back)
    (define-key map (kbd "n")   'next-line)
    (define-key map (kbd "p")   'previous-line)
    (define-key map (kbd "w")   'hdf5-copy-field-at-cursor)
    map)
  "Keymap for HDF5 mode.")

(defvar hdf5--buffer-filename nil
  "Temporary variable to pass the hdf5 filename into hdf-mode.

This avoids having to set the variable `buffer-file-name', which
would run the risk of overwiting the HDF5 file that is being
viewed.")

(defvar-local hdf5-mode-file nil
  "Path to the current HDF5 file being viewed.")

(defvar-local hdf5-mode-root nil
  "Path to begin printing the current HDF5 file fields.")

(defun hdf5-fix-path (path)
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

(defun hdf5-get-field-at-cursor ()
  "Return field (group or dataset) at cursor position."
  (end-of-line)
  (backward-word)
  (let ((field (thing-at-point 'filename t)))
    (hdf5-fix-path (concat hdf5-mode-root "/" field))))

(defun hdf5-is-group (field)
  "Return t if FIELD is a group."
  (let ((output (hdf5-parser-cmd "--is-group" field hdf5-mode-file)))
    (gethash "return" output)))

(defun hdf5-is-field (field)
  "Return t if FIELD is a field in the file."
  (let ((output (hdf5-parser-cmd "--is-field" field hdf5-mode-file)))
    (gethash "return" output)))

(defun hdf5-parser-cmd (&rest args)
  "Run parser command with custom ARGS and return json output."
  (with-temp-buffer
    (let ((exit-code
           (apply #'call-process-shell-command
                  hdf5-mode-parse-command nil t nil args)))
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

(defun hdf5-back ()
  "Go back one group level and display to screen."
  (interactive)
  (setq-local hdf5-mode-root
        (hdf5-fix-path (file-name-directory hdf5-mode-root)))
  (hdf5-display-fields))

(defun hdf5-display-fields ()
  "Display current root group fields to buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s %s\n\n"
                    (propertize "Root:" 'face 'bold)
                    hdf5-mode-root))
    (let* ((output (hdf5-parser-cmd "--get-fields" hdf5-mode-root hdf5-mode-file))
           (attrs  (hdf5-parser-cmd "--get-attrs"  hdf5-mode-root hdf5-mode-file))
           (num-attrs (hash-table-count attrs))
           (template "%-8s %-15s %20s  %-30s\n"))
      (insert (propertize (format template "*type*" "*dims*" "*range*" "*name*")
                          'face '('bold 'underline)))
      (maphash (lambda (key val)
                 (let ((type  (gethash "type"  val)))
                   (cond ((string= type "group")
                          (insert (format template
                                           "group" "N/A" ""
                                          (format "%s/" key))))
                         ((string= type "dataset")
                          (let ((dtype (gethash "dtype" val))
                                (shape (gethash "shape" val))
                                (range (gethash "range" val "")))
                            (insert (format template
                                            dtype shape range key)))))))
               output)
      (when (> num-attrs 0)
        (insert "\n\n")
        (insert (propertize (format template "" "*value*" "" "*attribute*")
                            'face '('bold 'underline)))
        (maphash (lambda (attrkey attrval)
                   (insert (format template "" attrval "" attrkey)))
                 attrs)))
    (superword-mode)
    (goto-char (point-min))
    (end-of-line 4)
    (backward-word)
    (set-goal-column nil)
    (set-buffer-modified-p nil)))

(defun hdf5-preview-field-at-cursor ()
  "Display field contents at cursor in minibuffer."
  (interactive)
  (let ((field (hdf5-get-field-at-cursor)))
    (hdf5-preview-field field)))

(defun hdf5-preview-field (field)
  "Display selected FIELD contents in minibuffer."
  (interactive "sEnter path: ")
  (when (hdf5-is-field field)
    (let ((field  (hdf5-fix-path field))
          (output (hdf5-parser-cmd "--preview-field" field hdf5-mode-file)))
      (message (format "%s %s %s:\n%s"
                       (propertize field 'face 'bold)
                       (gethash "shape" output "")
                       (gethash "dtype" output "")
                       (gethash "data" output))))))

(defun hdf5-read-field-at-cursor ()
  "Display field contents at cursor in new buffer."
  (interactive)
  (let ((field (hdf5-get-field-at-cursor)))
    (hdf5-read-field field)))

(defun hdf5-read-field (field)
  "Display specified FIELD contents in new buffer."
  (interactive "sEnter path: ")
  (let ((field (hdf5-fix-path field)))
    (when (hdf5-is-field field)
      (if (hdf5-is-group field)
          (progn
            (setq-local hdf5-mode-root field)
            (hdf5-display-fields))
        (let* ((output (hdf5-parser-cmd "--read-dataset" field hdf5-mode-file))
               (parent-buf (format "%s" (current-buffer)))
               (parent-nostars (substring parent-buf 1 (1- (length parent-buf)))))
          (with-current-buffer (get-buffer-create (format "*%s%s*" parent-nostars field))
            (let ((inhibit-read-only t))
              (erase-buffer)
              (setq-local truncate-lines t)
              (insert (format "%s %s %s:\n%s\n"
                              (propertize field 'face 'bold)
                              (gethash "shape" output)
                              (gethash "dtype" output)
                              (gethash "data" output)))
              (goto-char (point-min))
              (special-mode)
              (display-buffer (current-buffer) '((display-buffer-same-window))))))))))

(defun hdf5-copy-field-at-cursor ()
  "Interactively put field-at-cursor into the kill ring."
  (interactive)
  (let* ((field-name (hdf5-get-field-at-cursor))
         (field-type (if (hdf5-is-field field-name) "field" "attribute")))
    (kill-new field-name)
    (message (format "Copied HD5 %s name: %s" field-type field-name))))

;;;###autoload
(define-derived-mode hdf5-mode special-mode "HDF5"
  "Major mode for viewing HDF5 files."
  (setq-local buffer-read-only t)
  (setq-local hdf5-mode-file hdf5--buffer-file-name)
  (setq-local hdf5-mode-root "/")
  (hdf5-display-fields))

;;;###autoload
(defun hdf5-mode--maybe-startup (&optional filename wildcards)
  "Advice to avoid loading HDF5 files into the buffer.

HDF5 files can be very large and hdf5-mode does not need the file
contents to be loaded before operating on the file.  This advice
looks for the HDF5 signature in the first 8 bytes of a file.  If
it is not HDF5, then proceed with `find-file'.  If it is HDF5, then open a
buffer named \"*hdf5: FILENAME*\" and start hdf5-mode.
`find-file' is then bypassed.

WILDCARDS is not used by this advice and is passed on to
`find-file'.  This advice is also bypassed if FILENAME is not
given to `find-file' a-priori, ie, this only works from `dired'."

  (if (or wildcards (not (file-regular-p filename))) nil
    (let ((hdf5-sign (unibyte-string #x89 #x48 #x44 #x46 #x0d #x0a #x1a #x0a))
          (filehead (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally filename nil 0 8 t)
                     (buffer-substring-no-properties 1 9))))
      (when (string= filehead hdf5-sign)
        (let ((hdf5-buffer-name (concat "*hdf5: "
                                        (file-name-nondirectory filename)
                                        "*")))
          ;; for later:
          ;; if hdf5-buffer-name corresponds to an existing buffer
          ;;    if (string= filename hdf5--buffer-file-name)
          ;;       switch to buffer
          ;;    else
          ;;       create/switch to buffer with unique name
          ;;       setq hdf5--buffer-file-name filename
          ;;       (hdf5-mode)
          ;; else run next 3 lines
          (switch-to-buffer (get-buffer-create hdf5-buffer-name));; need to uniquify this name (later)
          (setq default-directory (file-name-directory filename))
          (setq hdf5--buffer-file-name filename) ;;hdf5-mode operates on hdf5--buffer-file-name
          (hdf5-mode)
          t))))) ;; bypass find-file

;;;###autoload
(advice-add 'find-file :before-until #'hdf5-mode--maybe-startup)

;; (add-to-list 'auto-mode-alist '("\\.h5\\'" . hdf5-mode))
;; (add-to-list 'auto-mode-alist '("\\.hdf5\\'" . hdf5-mode))

(provide 'hdf5-mode)

;;; hdf5-mode.el ends here
