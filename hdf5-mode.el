;;; hdf5-mode.el --- Major mode for viewing HDF5 files

;;; Commentary:
;; This package provides a major mode for viewing HDF5 files in Emacs.
;; It requires Python and Python's h5py package to be installed.
;; The Python logic is stored in h5parse.py, which should be installed
;; in the same location as hdf5-mode.el.

;;; Code:

(require 'python)
(require 'json)

(defgroup hdf5-mode nil
  "Major mode for viewing HDF5 files"
  :group 'data)

(defcustom hdf5-mode-python-command "python3"
  "Python interpreter to execute h5parse.py. Must have h5py"
  :type 'string
  :group 'hdf5-mode)

(defcustom hdf5-mode-parse-command
  (format "%s %sh5parse.py"
          hdf5-mode-python-command
          (file-name-directory (or load-file-name (buffer-file-name))))
  "Shell command to launch h5parse.py script"
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
    (define-key map (kbd "n")   'forward-line)
    (define-key map (kbd "p")   'previous-line)
    (define-key map (kbd "w")   'hdf5-copy-field-at-cursor)
    map)
  "Keymap for HDF5 mode")

(defvar-local hdf5-mode-file nil
  "Path to the current HDF5 file being viewed")

(defvar-local hdf5-mode-root nil
  "Path to begin printing the current HDF5 file fields")

(defun hdf5-fix-path (path)
  "Remove extraneous '/'s from path"
  (setq fsplit (file-name-split path))
  (setq npath "")
  (dolist (val fsplit)
    (if (and (not (string= "" val))
             (not (string-prefix-p "/" val)))
        (setq npath (concat npath "/" val))))
  (if (string-empty-p npath)
      (setq npath "/"))
  npath)

(defun hdf5-get-field-at-cursor ()
  "Return field at current cursor position"
  (goto-char (line-beginning-position))
  (setq field (thing-at-point 'word t))
  (hdf5-fix-path (concat hdf5-mode-root "/" field)))

(defun hdf5-is-group (field)
  "Return t if field is a group"
  (setq output (hdf5-parser-cmd
                "--is-group" field
                hdf5-mode-file))
  (setq return (gethash "return" output))
  (gethash "return" output))

(defun hdf5-parser-cmd (&rest args)
  "Run parser command with custom args and return json output"
  (with-temp-buffer
    (let ((exit-code
           (apply #'call-process-shell-command
                  hdf5-mode-parse-command nil t nil args)))
      (if (= exit-code 0)
          (progn
            (goto-char (point-min))
            (condition-case err
                (let ((json-array-type 'list)
                      (json-object-type 'hash-table)
                      (json-false nil))
                  (json-read))
              (json-readtable-error
               (error "Failed to read parser output: Invalid JSON"))))
        (error "Parser script failed: %s"
               (buffer-substring (point-min) (point-max)))))))

(defun hdf5-back ()
  "Go back one group level and display to screen"
  (interactive)
  (setq hdf5-mode-root
        (hdf5-fix-path (file-name-directory hdf5-mode-root)))
  (hdf5-display-fields))

(defun hdf5-display-fields ()
  "Display current root group fields to buffer"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq output (hdf5-parser-cmd
                  "--get-fields" hdf5-mode-root hdf5-mode-file))
    (insert (format "%s %s\n\n"
                    (propertize "Root:" 'face 'bold)
                    hdf5-mode-root))
    (setq template "%-15s %-15s %-15s\n")
    (insert (propertize (format template "*name*" "*dims*" "*type*")
                        'face '('bold 'underline)))
    (maphash (lambda (key val)
               (let ((type (gethash "type" val)))
                 (cond ((string= type "group")
                        (insert (format template
                                        (format "%s/" key)
                                        "N/A" "group")))
                       ((string= type "dataset")
                        (setq shape (gethash "shape" val))
                        (setq dtype (gethash "dtype" val))
                        (insert (format template
                                        key shape dtype))))))
             output)
    (goto-char (point-min))
    (forward-line)
    (forward-line)
    (forward-line)
    (set-buffer-modified-p nil)))

(defun hdf5-preview-field-at-cursor ()
  "Display field contents at cursor in message box"
  (interactive)
  (setq field (hdf5-get-field-at-cursor))
  (hdf5-preview-field field))

(defun hdf5-preview-field (field)
  "Display selected field contents in message box"
  (interactive "sEnter path: ")
  (setq field (hdf5-fix-path field))
  (setq output (hdf5-parser-cmd "--preview-field" field hdf5-mode-file))
  (message (format "%s\n%s"
                   (propertize (format "%s:" field) 'face 'bold)
                   (gethash "data" output))))

(defun hdf5-read-field-at-cursor ()
  "Display field contents at cursor in new buffer"
  (interactive)
  (setq field (hdf5-get-field-at-cursor))
  (hdf5-read-field field))

(defun hdf5-read-field (field)
  "Display specified field contents in new buffer"
  (interactive "sEnter path: ")
  (setq field (hdf5-fix-path field))
  (if (hdf5-is-group field)
      (progn
        (setq hdf5-mode-root field)
        (hdf5-display-fields))
    (progn
      (setq output (hdf5-parser-cmd "--read-field" field hdf5-mode-file))
      (setq data (gethash "data" output))
      (with-current-buffer (get-buffer-create "*HDF5 Dataset*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "%s:\n" field) 'face 'bold))
          (insert (format "%s\n" data))
          (special-mode)
          (display-buffer (current-buffer)
                          '((display-buffer-reuse-window
                             display-buffer-pop-up-window)
                            (reusable-frames . visible))))))))

(defun hdf5-copy-field-at-cursor ()
  "Interactively put field-at-cursor into the kill ring"
  (interactive)
  (let ((field-name (hdf5-get-field-at-cursor)))
    (kill-new field-name)
    (message (format "Copied HD5 field: %s" field-name))))

(define-derived-mode hdf5-mode special-mode "HDF5"
  "Major mode for viewing HDF5 files"
  (setq buffer-read-only t)
  (setq hdf5-mode-file buffer-file-name)
  (setq hdf5-mode-root "/")
  (hdf5-display-fields))

(add-to-list 'auto-mode-alist '("\\.h5\\'" . hdf5-mode))
(add-to-list 'auto-mode-alist '("\\.hdf5\\'" . hdf5-mode))

(provide 'hdf5-mode)
