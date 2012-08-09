;;; mh-speed.el --- MH-E speedbar support

;; Copyright (C) 2002-2012  Free Software Foundation, Inc.

;; Author: Satyaki Das <satyaki@theforce.stanford.edu>
;; Maintainer: Bill Wohler <wohler@newt.com>
;; Keywords: mail
;; See: mh-e.el

;; This file is part of GNU Emacs.

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

;; Future versions should only use flists.

;;; Change Log:

;;; Code:

(require 'mh-e)
(mh-require-cl)

(require 'gnus-util)
(require 'speedbar)
(require 'timer)

;; Global variables.
(defvar mh-speed-refresh-flag nil)
(defvar mh-speed-last-selected-folder nil)
(defvar mh-speed-folder-map (make-hash-table :test #'equal))
(defvar mh-speed-flists-cache (make-hash-table :test #'equal))
(defvar mh-speed-flists-process nil)
(defvar mh-speed-flists-timer nil)
(defvar mh-speed-partial-line "")



;;; Speedbar Hook

(unless (member 'mh-speed-stealth-update
                (cdr (assoc "files" speedbar-stealthy-function-list)))
  ;; Is changing constant lists in elisp safe?
  (setq speedbar-stealthy-function-list
        (copy-tree speedbar-stealthy-function-list))
  (push 'mh-speed-stealth-update
        (cdr (assoc "files" speedbar-stealthy-function-list))))



;;; Speedbar Menus

(defvar mh-folder-speedbar-menu-items
  '("--"
    ["Visit Folder" mh-speed-view
     (with-current-buffer speedbar-buffer
       (get-text-property (mh-line-beginning-position) 'mh-folder))]
    ["Expand Nested Folders" mh-speed-expand-folder
     (and (get-text-property (mh-line-beginning-position) 'mh-children-p)
          (not (get-text-property (mh-line-beginning-position) 'mh-expanded)))]
    ["Contract Nested Folders" mh-speed-contract-folder
     (and (get-text-property (mh-line-beginning-position) 'mh-children-p)
          (get-text-property (mh-line-beginning-position) 'mh-expanded))]
    ["Refresh Speedbar" mh-speed-refresh t])
  "Extra menu items for speedbar.")

(defvar mh-show-speedbar-menu-items mh-folder-speedbar-menu-items)
(defvar mh-letter-speedbar-menu-items mh-folder-speedbar-menu-items)



;;; Speedbar Keys

(defvar mh-folder-speedbar-key-map (speedbar-make-specialized-keymap)
  "Specialized speedbar keymap for MH-E buffers.")

(gnus-define-keys mh-folder-speedbar-key-map
  "+"           mh-speed-expand-folder
  "-"           mh-speed-contract-folder
  "\r"          mh-speed-view
  "r"           mh-speed-refresh)

(defvar mh-show-speedbar-key-map mh-folder-speedbar-key-map)
(defvar mh-letter-speedbar-key-map mh-folder-speedbar-key-map)



;;; Speedbar Commands

;; Alphabetical.

(defalias 'mh-speed-contract-folder 'mh-speed-toggle)

(defalias 'mh-speed-expand-folder 'mh-speed-toggle)

(defun mh-speed-refresh ()
  "Regenerates the list of folders in the speedbar.

Run this command if you've added or deleted a folder, or want to
update the unseen message count before the next automatic
update."
  (interactive)
  (mh-speed-flists t)
  (mh-speed-invalidate-map ""))

(defun mh-speed-stealth-update (&optional force)
  "Do stealth update.
With non-nil FORCE, the update is always carried out."
  (cond ((with-current-buffer speedbar-buffer
           (get-text-property (point-min) 'mh-level))
         ;; Execute this hook and *don't* run anything else
         (mh-speed-update-current-folder force)
         nil)
        ;; Otherwise on to your regular programming
        (t t)))

(defun mh-speed-toggle (&rest ignored)
  "Toggle the display of child folders in the speedbar.
The optional arguments from speedbar are IGNORED."
  (interactive)
  (declare (ignore args))
  (beginning-of-line)
  (let ((parent (get-text-property (point) 'mh-folder))
        (kids-p (get-text-property (point) 'mh-children-p))
        (expanded (get-text-property (point) 'mh-expanded))
        (level (get-text-property (point) 'mh-level))
        (point (point))
        start-region)
    (speedbar-with-writable
      (cond ((not kids-p) nil)
            (expanded
             (forward-line)
             (setq start-region (point))
             (while (and (get-text-property (point) 'mh-level)
                         (> (get-text-property (point) 'mh-level) level))
               (let ((folder (get-text-property (point) 'mh-folder)))
                 (when (gethash folder mh-speed-folder-map)
                   (set-marker (gethash folder mh-speed-folder-map) nil)
                   (remhash folder mh-speed-folder-map)))
               (forward-line))
             (delete-region start-region (point))
             (forward-line -1)
             (speedbar-change-expand-button-char ?+)
             (add-text-properties
              (mh-line-beginning-position) (1+ (line-beginning-position))
              '(mh-expanded nil)))
            (t
             (forward-line)
             (mh-speed-add-buttons parent (1+ level))
             (goto-char point)
             (speedbar-change-expand-button-char ?-)
             (add-text-properties
              (mh-line-beginning-position) (1+ (line-beginning-position))
              `(mh-expanded t)))))))

(defun mh-speed-view (&rest ignored)
  "Visits the selected folder just as if you had used \\<mh-folder-mode-map>\\[mh-visit-folder].
The optional arguments from speedbar are IGNORED."
  (interactive)
  (declare (ignore args))
  (let* ((folder (get-text-property (mh-line-beginning-position) 'mh-folder))
         (range (and (stringp folder)
                     (mh-read-range "Scan" folder t nil nil
                                    mh-interpret-number-as-range-flag))))
    (when (stringp folder)
      (speedbar-with-attached-buffer
       (mh-visit-folder folder range)
       (delete-other-windows)))))



;;; Support Routines

;;;###mh-autoload
(defun mh-folder-speedbar-buttons (buffer)
  "Interface function to create MH-E speedbar buffer.
BUFFER is the MH-E buffer for which the speedbar buffer is to be
created."
  (unless (get-text-property (point-min) 'mh-level)
    (erase-buffer)
    (clrhash mh-speed-folder-map)
    (speedbar-make-tag-line 'bracket ?+ 'mh-speed-toggle nil " " 'ignore nil
                            'mh-speedbar-folder 0)
    (forward-line -1)
    (setf (gethash nil mh-speed-folder-map)
          (set-marker (or (gethash nil mh-speed-folder-map) (make-marker))
                      (1+ (mh-line-beginning-position))))
    (add-text-properties
     (mh-line-beginning-position) (1+ (line-beginning-position))
     `(mh-folder nil mh-expanded nil mh-children-p t mh-level 0))
    (mh-speed-stealth-update t)
    (when (> mh-speed-update-interval 0)
      (mh-speed-flists nil))))

;;;###mh-autoload
(defalias 'mh-show-speedbar-buttons 'mh-folder-speedbar-buttons)
;;;###mh-autoload
(defalias 'mh-letter-speedbar-buttons 'mh-folder-speedbar-buttons)

(defmacro mh-speed-select-attached-frame ()
  "Compatibility macro to handle speedbar versions 0.11a and 0.14beta4."
  (cond ((fboundp 'dframe-select-attached-frame)
         '(dframe-select-attached-frame speedbar-frame))
        ((boundp 'speedbar-attached-frame)
         '(select-frame speedbar-attached-frame))
        (t (error "Installed speedbar version not supported by MH-E"))))

(defun mh-speed-update-current-folder (force)
  "Update speedbar highlighting of the current folder.
The function tries to be smart so that work done is minimized.
The currently highlighted folder is cached and no highlighting
happens unless it changes.
Also highlighting is suspended while the speedbar frame is selected.
Otherwise you get the disconcerting behavior of folders popping open
on their own when you are trying to navigate around in the speedbar
buffer.

The update is always carried out if FORCE is non-nil."
  (let* ((lastf (selected-frame))
         (newcf (save-excursion
                  (mh-speed-select-attached-frame)
                  (prog1 (mh-speed-extract-folder-name (buffer-name))
                    (select-frame lastf))))
         (lastb (current-buffer))
         (case-fold-search t))
    (when (or force
              (and mh-speed-refresh-flag (not (eq lastf speedbar-frame)))
              (and (stringp newcf)
                   (equal (substring newcf 0 1) "+")
                   (not (equal newcf mh-speed-last-selected-folder))))
      (setq mh-speed-refresh-flag nil)
      (select-frame speedbar-frame)
      (set-buffer speedbar-buffer)

      ;; Remove highlight from previous match...
      (mh-speed-highlight mh-speed-last-selected-folder 'mh-speedbar-folder)

      ;; If we found a match highlight it...
      (when (mh-speed-goto-folder newcf)
        (mh-speed-highlight newcf 'mh-speedbar-selected-folder))

      (setq mh-speed-last-selected-folder newcf)
      (speedbar-position-cursor-on-line)
      (set-window-point (frame-first-window speedbar-frame) (point))
      (set-buffer lastb)
      (select-frame lastf))
    (when (eq lastf speedbar-frame)
      (setq mh-speed-refresh-flag t))))

(defun mh-speed-highlight (folder face)
  "Set FOLDER to FACE."
  (save-excursion
    (speedbar-with-writable
      (goto-char (gethash folder mh-speed-folder-map (point)))
      (beginning-of-line)
      (if (re-search-forward "([1-9][0-9]*/[0-9]+)" (mh-line-end-position) t)
          (setq face (mh-speed-bold-face face))
        (setq face (mh-speed-normal-face face)))
      (beginning-of-line)
      (when (re-search-forward "\\[.\\] " (mh-line-end-position) t)
        (put-text-property (point) (mh-line-end-position) 'face face)))))

(defun mh-speed-normal-face (face)
  "Return normal face for given FACE."
  (cond ((eq face 'mh-speedbar-folder-with-unseen-messages)
         'mh-speedbar-folder)
        ((eq face 'mh-speedbar-selected-folder-with-unseen-messages)
         'mh-speedbar-selected-folder)
        (t face)))

(defun mh-speed-bold-face (face)
  "Return bold face for given FACE."
  (cond ((eq face 'mh-speedbar-folder)
         'mh-speedbar-folder-with-unseen-messages)
        ((eq face 'mh-speedbar-selected-folder)
         'mh-speedbar-selected-folder-with-unseen-messages)
        (t face)))

(defun mh-speed-goto-folder (folder)
  "Move point to line containing FOLDER.
The function will expand out parent folders of FOLDER if needed."
  (let ((prefix folder)
        (suffix-list ())
        (last-slash t))
    (while (and (not (gethash prefix mh-speed-folder-map)) last-slash)
      (setq last-slash (mh-search-from-end ?/ prefix))
      (when (integerp last-slash)
        (push (substring prefix (1+ last-slash)) suffix-list)
        (setq prefix (substring prefix 0 last-slash))))
    (let ((prefix-position (gethash prefix mh-speed-folder-map)))
      (if prefix-position
          (goto-char prefix-position)
        (goto-char (point-min))
        (mh-speed-toggle)
        (unless (get-text-property (point) 'mh-expanded)
          (mh-speed-toggle))
        (goto-char (gethash prefix mh-speed-folder-map))))
    (while suffix-list
      ;; We always need atleast one toggle. We need two if the directory list
      ;; is stale since a folder was added.
      (when (equal prefix (get-text-property (mh-line-beginning-position)
                                             'mh-folder))
        (mh-speed-toggle)
        (unless (get-text-property (point) 'mh-expanded)
          (mh-speed-toggle)))
      (setq prefix (format "%s/%s" prefix (pop suffix-list)))
      (goto-char (gethash prefix mh-speed-folder-map (point))))
    (beginning-of-line)
    (equal folder (get-text-property (point) 'mh-folder))))

(defun mh-speed-extract-folder-name (buffer)
  "Given an MH-E BUFFER find the folder that should be highlighted.
Do the right thing for the different kinds of buffers that MH-E
uses."
  (with-current-buffer buffer
    (cond ((eq major-mode 'mh-folder-mode)
           mh-current-folder)
          ((eq major-mode 'mh-show-mode)
           (set-buffer mh-show-folder-buffer)
           mh-current-folder)
          ((eq major-mode 'mh-letter-mode)
           (when (string-match mh-user-path buffer-file-name)
             (let* ((rel-path (substring buffer-file-name (match-end 0)))
                    (directory-end (mh-search-from-end ?/ rel-path)))
               (when directory-end
                 (format "+%s" (substring rel-path 0 directory-end)))))))))

(defun mh-speed-add-buttons (folder level)
  "Add speedbar button for FOLDER which is at indented by LEVEL amount."
  (let ((folder-list (mh-sub-folders folder)))
    (mapc
     (lambda (f)
       (let* ((folder-name (format "%s%s%s" (or folder "+")
                                   (if folder "/" "") (car f)))
              (counts (gethash folder-name mh-speed-flists-cache)))
         (speedbar-with-writable
           (speedbar-make-tag-line
            'bracket (if (cdr f) ?+ ? )
            'mh-speed-toggle nil
            (format "%s%s"
                    (car f)
                    (if counts
                        (format " (%s/%s)" (car counts) (cdr counts))
                      ""))
            'mh-speed-view nil
            (if (and counts (> (car counts) 0))
                'mh-speedbar-folder-with-unseen-messages
              'mh-speedbar-folder)
            level)
           (save-excursion
             (forward-line -1)
             (setf (gethash folder-name mh-speed-folder-map)
                   (set-marker (or (gethash folder-name mh-speed-folder-map)
                                   (make-marker))
                               (1+ (mh-line-beginning-position))))
             (add-text-properties
              (mh-line-beginning-position) (1+ (mh-line-beginning-position))
              `(mh-folder ,folder-name
                          mh-expanded nil
                          mh-children-p ,(not (not (cdr f)))
                          ,@(if counts `(mh-count
                                         (,(car counts) . ,(cdr counts))) ())
                          mh-level ,level))))))
     folder-list)))

(defvar mh-speed-current-folder nil)
(defvar mh-speed-flists-folder nil)

(defmacro mh-process-kill-without-query (process)
  "PROCESS can be killed without query on Emacs exit.
Avoid using `process-kill-without-query' if possible since it is
now obsolete."
  (if (fboundp 'set-process-query-on-exit-flag)
      `(set-process-query-on-exit-flag ,process nil)
    `(process-kill-without-query ,process)))

;;;###mh-autoload
(defun mh-speed-flists (force &rest folders)
  "Execute flists -recurse and update message counts.
If FORCE is non-nil the timer is reset.

Any number of optional FOLDERS can be specified. If specified,
flists is run only for that one folder."
  (interactive (list t))
  (when force
    (when mh-speed-flists-timer
      (mh-cancel-timer mh-speed-flists-timer)
      (setq mh-speed-flists-timer nil))
    (when (and (processp mh-speed-flists-process)
               (not (eq (process-status mh-speed-flists-process) 'exit)))
      (set-process-filter mh-speed-flists-process t)
      (kill-process mh-speed-flists-process)
      (setq mh-speed-partial-line "")
      (setq mh-speed-flists-process nil)))
  (setq mh-speed-flists-folder folders)
  (unless mh-speed-flists-timer
    (setq mh-speed-flists-timer
          (run-at-time
           nil (if (> mh-speed-update-interval 0)
                   mh-speed-update-interval
                 nil)
           (lambda ()
             (unless (and (processp mh-speed-flists-process)
                          (not (eq (process-status mh-speed-flists-process)
                                   'exit)))
               (setq mh-speed-current-folder
                     (concat
                      (if mh-speed-flists-folder
                          (substring (car (reverse mh-speed-flists-folder)) 1)
                        (with-temp-buffer
                          (call-process (expand-file-name "folder" mh-progs)
                                        nil '(t nil) nil "-fast")
                          (buffer-substring (point-min) (1- (point-max)))))
                      "+"))
               (setq mh-speed-flists-process
                     (apply #'start-process "*flists*" nil
                            (expand-file-name "flists" mh-progs)
                            (if mh-speed-flists-folder "-noall" "-all")
                            "-sequence" (symbol-name mh-unseen-seq)
                            (or mh-speed-flists-folder '("-recurse"))))
               ;; Run flists on all folders the next time around...
               (setq mh-speed-flists-folder nil)
               (mh-process-kill-without-query mh-speed-flists-process)
               (set-process-filter mh-speed-flists-process
                                   'mh-speed-parse-flists-output)))))))

;; Copied from mh-make-folder-list-filter...
;; XXX Refactor to use mh-make-folder-list-filer?
(defun mh-speed-parse-flists-output (process output)
  "Parse the incremental results from flists.
PROCESS is the flists process and OUTPUT is the results that must
be handled next."
  (let ((prevailing-match-data (match-data))
        (position 0)
        line-end line folder unseen total)
    (unwind-protect
        (while (setq line-end (string-match "\n" output position))
          (setq line (format "%s%s"
                             mh-speed-partial-line
                             (substring output position line-end))
                mh-speed-partial-line "")
          (multiple-value-setq (folder unseen total)
            (values-list
             (mh-parse-flist-output-line line mh-speed-current-folder)))
          (when (and folder unseen total
                     (let ((old-pair (gethash folder mh-speed-flists-cache)))
                       (or (not (equal (car old-pair) unseen))
                           (not (equal (cdr old-pair) total)))))
            (setf (gethash folder mh-speed-flists-cache) (cons unseen total))
            (when (buffer-live-p (get-buffer speedbar-buffer))
              (with-current-buffer speedbar-buffer
                (speedbar-with-writable
                  (when (get-text-property (point-min) 'mh-level)
                    (let ((pos (gethash folder mh-speed-folder-map))
                          face)
                      (when pos
                        (goto-char pos)
                        (goto-char (mh-line-beginning-position))
                        (cond
                         ((null (get-text-property (point) 'mh-count))
                          (goto-char (mh-line-end-position))
                          (setq face (get-text-property (1- (point)) 'face))
                          (insert (format " (%s/%s)" unseen total))
                          (mh-speed-highlight 'unknown face)
                          (goto-char (mh-line-beginning-position))
                          (add-text-properties (point) (1+ (point))
                                               `(mh-count (,unseen . ,total))))
                         ((not (equal (get-text-property (point) 'mh-count)
                                      (cons unseen total)))
                          (goto-char (mh-line-end-position))
                          (setq face (get-text-property (1- (point)) 'face))
                          (re-search-backward " " (mh-line-beginning-position) t)
                          (delete-region (point) (mh-line-end-position))
                          (insert (format " (%s/%s)" unseen total))
                          (mh-speed-highlight 'unknown face)
                          (goto-char (mh-line-beginning-position))
                          (add-text-properties
                           (point) (1+ (point))
                           `(mh-count (,unseen . ,total))))))))))))
          (setq position (1+ line-end)))
      (set-match-data prevailing-match-data))
    (setq mh-speed-partial-line (substring output position))))

;;;###mh-autoload
(defun mh-speed-invalidate-map (folder)
  "Remove FOLDER from various optimization caches."
  (interactive (list ""))
  (with-current-buffer speedbar-buffer
    (let* ((speedbar-update-flag nil)
           (last-slash (mh-search-from-end ?/ folder))
           (parent (if last-slash (substring folder 0 last-slash) nil))
           (parent-position (gethash parent mh-speed-folder-map))
           (parent-change nil))
      (when parent-position
        (let ((parent-kids (mh-sub-folders parent)))
          (cond ((null parent-kids)
                 (setq parent-change ?+))
                ((and (null (cdr parent-kids))
                      (equal (if last-slash
                                 (substring folder (1+ last-slash))
                               (substring folder 1))
                             (caar parent-kids)))
                 (setq parent-change ? ))))
        (goto-char parent-position)
        (when (equal (get-text-property (mh-line-beginning-position) 'mh-folder)
                     parent)
          (when (get-text-property (mh-line-beginning-position) 'mh-expanded)
            (mh-speed-toggle))
          (when parent-change
            (speedbar-with-writable
              (mh-speedbar-change-expand-button-char parent-change)
              (add-text-properties
               (mh-line-beginning-position) (1+ (mh-line-beginning-position))
               `(mh-children-p ,(equal parent-change ?+)))))
          (mh-speed-highlight mh-speed-last-selected-folder 'mh-speedbar-folder)
          (setq mh-speed-last-selected-folder nil)
          (setq mh-speed-refresh-flag t)))
      (when (equal folder "")
        (mh-clear-sub-folders-cache)))))

;; Make it slightly more general to allow for [ ] buttons to be
;; changed to [+].
(defun mh-speedbar-change-expand-button-char (char)
  "Change the expansion button character to CHAR for the current line."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "\\[.\\]" (mh-line-end-position) t)
        (speedbar-with-writable
          (backward-char 2)
          (delete-char 1)
          (insert-char char 1 t)
          (put-text-property (point) (1- (point)) 'invisible nil)
          ;; make sure we fix the image on the text here.
          (mh-funcall-if-exists
           speedbar-insert-image-button-maybe (- (point) 2) 3)))))

;;;###mh-autoload
(defun mh-speed-add-folder (folder)
  "Add FOLDER since it is being created.
The function invalidates the latest ancestor that is present."
  (with-current-buffer speedbar-buffer
    (let ((speedbar-update-flag nil)
          (last-slash (mh-search-from-end ?/ folder))
          (ancestor folder)
          (ancestor-pos nil))
      (block while-loop
        (while last-slash
          (setq ancestor (substring ancestor 0 last-slash))
          (setq ancestor-pos (gethash ancestor mh-speed-folder-map))
          (when ancestor-pos
            (return-from while-loop))
          (setq last-slash (mh-search-from-end ?/ ancestor))))
      (unless ancestor-pos (setq ancestor nil))
      (goto-char (or ancestor-pos (gethash nil mh-speed-folder-map)))
      (speedbar-with-writable
        (mh-speedbar-change-expand-button-char ?+)
        (add-text-properties
         (mh-line-beginning-position) (1+ (mh-line-beginning-position))
         `(mh-children-p t)))
      (when (get-text-property (mh-line-beginning-position) 'mh-expanded)
        (mh-speed-toggle))
      (setq mh-speed-refresh-flag t))))

(provide 'mh-speed)

;; Local Variables:
;; indent-tabs-mode: nil
;; sentence-end-double-space: nil
;; End:

;;; mh-speed.el ends here
