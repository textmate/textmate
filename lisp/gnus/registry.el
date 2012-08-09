;;; registry.el --- Track and remember data items by various fields

;; Copyright (C) 2011-2012  Free Software Foundation, Inc.

;; Author: Teodor Zlatanov <tzz@lifelogs.com>
;; Keywords: data

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

;; This library provides a general-purpose EIEIO-based registry
;; database with persistence, initialized with these fields:

;; version: a float, 0.1 currently (don't change it)

;; max-hard: an integer, default 5000000

;; max-soft: an integer, default 50000

;; precious: a list of symbols

;; tracked: a list of symbols

;; tracker: a hashtable tuned for 100 symbols to track (you should
;; only access this with the :lookup2-function and the
;; :lookup2+-function)

;; data: a hashtable with default size 10K and resize threshold 2.0
;; (this reflects the expected usage so override it if you know better)

;; ...plus methods to do all the work: `registry-search',
;; `registry-lookup', `registry-lookup-secondary',
;; `registry-lookup-secondary-value', `registry-insert',
;; `registry-delete', `registry-prune', `registry-size' which see

;; and with the following properties:

;; Every piece of data has a unique ID and some general-purpose fields
;; (F1=D1, F2=D2, F3=(a b c)...) expressed as an alist, e.g.

;; ((F1 D1) (F2 D2) (F3 a b c))

;; Note that whether a field has one or many pieces of data, the data
;; is always a list of values.

;; The user decides which fields are "precious", F2 for example.  At
;; PRUNE TIME (when the :prune-function is called), the registry will
;; trim any entries without the F2 field until the size is :max-soft
;; or less.  No entries with the F2 field will be removed at PRUNE
;; TIME.

;; When an entry is inserted, the registry will reject new entries
;; if they bring it over the max-hard limit, even if they have the F2
;; field.

;; The user decides which fields are "tracked", F1 for example.  Any
;; new entry is then indexed by all the tracked fields so it can be
;; quickly looked up that way.  The data is always a list (see example
;; above) and each list element is indexed.

;; Precious and tracked field names must be symbols.  All other
;; fields can be any other Emacs Lisp types.

;;; Code:

(eval-when-compile (require 'cl))

(eval-when-compile
  (when (null (ignore-errors (require 'ert)))
    (defmacro* ert-deftest (name () &body docstring-keys-and-body))))

(ignore-errors
  (require 'ert))
(eval-and-compile
  (or (ignore-errors (progn
                       (require 'eieio)
                       (require 'eieio-base)))
      ;; gnus-fallback-lib/ from gnus/lisp/gnus-fallback-lib
      (ignore-errors
        (let ((load-path (cons (expand-file-name
                                "gnus-fallback-lib/eieio"
                                (file-name-directory (locate-library "gnus")))
                               load-path)))
          (require 'eieio)
          (require 'eieio-base)))
      (error
       "eieio not found in `load-path' or gnus-fallback-lib/ directory.")))

(defclass registry-db (eieio-persistent)
  ((version :initarg :version
            :initform 0.1
            :type float
            :custom float
            :documentation "The registry version.")
   (max-hard :initarg :max-hard
             :initform 5000000
             :type integer
             :custom integer
             :documentation "Never accept more than this many elements.")
   (max-soft :initarg :max-soft
             :initform 50000
             :type integer
             :custom integer
             :documentation "Prune as much as possible to get to this size.")
   (prune-factor
    :initarg :prune-factor
    :initform 0.1
    :type float
    :custom float
    :documentation "At the max-hard limit, prune size * this entries.")
   (tracked :initarg :tracked
            :initform nil
            :type t
            :documentation "The tracked (indexed) fields, a list of symbols.")
   (precious :initarg :precious
             :initform nil
             :type t
             :documentation "The precious fields, a list of symbols.")
   (tracker :initarg :tracker
            :type hash-table
            :documentation "The field tracking hashtable.")
   (data :initarg :data
         :type hash-table
         :documentation "The data hashtable.")))

(eval-and-compile
  (defmethod initialize-instance :AFTER ((this registry-db) slots)
    "Set value of data slot of THIS after initialization."
    (with-slots (data tracker) this
      (unless (member :data slots)
	(setq data
	      (make-hash-table :size 10000 :rehash-size 2.0 :test 'equal)))
      (unless (member :tracker slots)
	(setq tracker (make-hash-table :size 100 :rehash-size 2.0)))))

  (defmethod registry-lookup ((db registry-db) keys)
    "Search for KEYS in the registry-db THIS.
Returns an alist of the key followed by the entry in a list, not a cons cell."
    (let ((data (oref db :data)))
      (delq nil
	    (mapcar
	     (lambda (k)
	       (when (gethash k data)
		 (list k (gethash k data))))
	     keys))))

  (defmethod registry-lookup-breaks-before-lexbind ((db registry-db) keys)
    "Search for KEYS in the registry-db THIS.
Returns an alist of the key followed by the entry in a list, not a cons cell."
    (let ((data (oref db :data)))
      (delq nil
	    (loop for key in keys
		  when (gethash key data)
		  collect (list key (gethash key data))))))

  (defmethod registry-lookup-secondary ((db registry-db) tracksym
					&optional create)
    "Search for TRACKSYM in the registry-db THIS.
When CREATE is not nil, create the secondary index hashtable if needed."
    (let ((h (gethash tracksym (oref db :tracker))))
      (if h
	  h
	(when create
	  (puthash tracksym
		   (make-hash-table :size 800 :rehash-size 2.0 :test 'equal)
		   (oref db :tracker))
	  (gethash tracksym (oref db :tracker))))))

  (defmethod registry-lookup-secondary-value ((db registry-db) tracksym val
					      &optional set)
    "Search for TRACKSYM with value VAL in the registry-db THIS.
When SET is not nil, set it for VAL (use t for an empty list)."
    ;; either we're asked for creation or there should be an existing index
    (when (or set (registry-lookup-secondary db tracksym))
      ;; set the entry if requested,
      (when set
	(puthash val (if (eq t set) '() set)
		 (registry-lookup-secondary db tracksym t)))
      (gethash val (registry-lookup-secondary db tracksym)))))

(defun registry--match (mode entry check-list)
  ;; for all members
  (when check-list
    (let ((key (nth 0 (nth 0 check-list)))
          (vals (cdr-safe (nth 0 check-list)))
          found)
      (while (and key vals (not found))
        (setq found (case mode
                      (:member
                       (member (car-safe vals) (cdr-safe (assoc key entry))))
                      (:regex
                       (string-match (car vals)
                                     (mapconcat
                                      'prin1-to-string
                                      (cdr-safe (assoc key entry))
                                      "\0"))))
              vals (cdr-safe vals)))
      (or found
          (registry--match mode entry (cdr-safe check-list))))))

(eval-and-compile
  (defmethod registry-search ((db registry-db) &rest spec)
    "Search for SPEC across the registry-db THIS.
For example calling with :member '(a 1 2) will match entry '((a 3 1)).
Calling with :all t (any non-nil value) will match all.
Calling with :regex '\(a \"h.llo\") will match entry '((a \"hullo\" \"bye\").
The test order is to check :all first, then :member, then :regex."
    (when db
      (let ((all (plist-get spec :all))
	    (member (plist-get spec :member))
	    (regex (plist-get spec :regex)))
	(loop for k being the hash-keys of (oref db :data)
	      using (hash-values v)
	      when (or
		    ;; :all non-nil returns all
		    all
		    ;; member matching
		    (and member (registry--match :member v member))
		    ;; regex matching
		    (and regex (registry--match :regex v regex)))
	      collect k))))

  (defmethod registry-delete ((db registry-db) keys assert &rest spec)
    "Delete KEYS from the registry-db THIS.
If KEYS is nil, use SPEC to do a search.
Updates the secondary ('tracked') indices as well.
With assert non-nil, errors out if the key does not exist already."
    (let* ((data (oref db :data))
	   (keys (or keys
		     (apply 'registry-search db spec)))
	   (tracked (oref db :tracked)))

      (dolist (key keys)
	(let ((entry (gethash key data)))
	  (when assert
	    (assert entry nil
		    "Key %s does not exists in database" key))
	  ;; clean entry from the secondary indices
	  (dolist (tr tracked)
	    ;; is this tracked symbol indexed?
	    (when (registry-lookup-secondary db tr)
	      ;; for every value in the entry under that key...
	      (dolist (val (cdr-safe (assq tr entry)))
		(let* ((value-keys (registry-lookup-secondary-value
				    db tr val)))
		  (when (member key value-keys)
		    ;; override the previous value
		    (registry-lookup-secondary-value
		     db tr val
		     ;; with the indexed keys MINUS the current key
		     ;; (we pass t when the list is empty)
		     (or (delete key value-keys) t)))))))
	  (remhash key data)))
      keys))

  (defmethod registry-full ((db registry-db))
    "Checks if registry-db THIS is full."
    (>= (registry-size db)
       (oref db :max-hard)))

  (defmethod registry-insert ((db registry-db) key entry)
    "Insert ENTRY under KEY into the registry-db THIS.
Updates the secondary ('tracked') indices as well.
Errors out if the key exists already."

    (assert (not (gethash key (oref db :data))) nil
	    "Key already exists in database")

    (assert (not (registry-full db))
	    nil
	    "registry max-hard size limit reached")

    ;; store the entry
    (puthash key entry (oref db :data))

    ;; store the secondary indices
    (dolist (tr (oref db :tracked))
      ;; for every value in the entry under that key...
      (dolist (val (cdr-safe (assq tr entry)))
	(let* ((value-keys (registry-lookup-secondary-value db tr val)))
	  (pushnew key value-keys :test 'equal)
	  (registry-lookup-secondary-value db tr val value-keys))))
    entry)

  (defmethod registry-reindex ((db registry-db))
    "Rebuild the secondary indices of registry-db THIS."
    (let ((count 0)
	  (expected (* (length (oref db :tracked)) (registry-size db))))
      (dolist (tr (oref db :tracked))
	(let (values)
	  (maphash
	   (lambda (key v)
	     (incf count)
	     (when (and (< 0 expected)
			(= 0 (mod count 1000)))
	       (message "reindexing: %d of %d (%.2f%%)"
			count expected (/ (* 100 count) expected)))
	     (dolist (val (cdr-safe (assq tr v)))
	       (let* ((value-keys (registry-lookup-secondary-value db tr val)))
		 (push key value-keys)
		 (registry-lookup-secondary-value db tr val value-keys))))
	   (oref db :data))))))

  (defmethod registry-size ((db registry-db))
    "Returns the size of the registry-db object THIS.
This is the key count of the :data slot."
    (hash-table-count (oref db :data)))

  (defmethod registry-prune ((db registry-db) &optional sortfun)
    "Prunes the registry-db object THIS.
Removes only entries without the :precious keys if it can,
then removes oldest entries first.
Returns the number of deleted entries.
If SORTFUN is given, tries to keep entries that sort *higher*.
SORTFUN is passed only the two keys so it must look them up directly."
    (dolist (collector '(registry-prune-soft-candidates
                         registry-prune-hard-candidates))
      (let* ((size (registry-size db))
             (collected (funcall collector db))
             (limit (nth 0 collected))
             (candidates (nth 1 collected))
             ;; sort the candidates if SORTFUN was given
             (candidates (if sortfun (sort candidates sortfun) candidates))
             (candidates-count (length candidates))
             ;; are we over max-soft?
             (prune-needed (> size limit)))

        ;; while we have more candidates than we need to remove...
        (while (and (> candidates-count (- size limit)) candidates)
          (decf candidates-count)
          (setq candidates (cdr candidates)))

        (registry-delete db candidates nil)
        (length candidates))))

  (defmethod registry-prune-soft-candidates ((db registry-db))
    "Collects pruning candidates from the registry-db object THIS.
Proposes only entries without the :precious keys."
    (let* ((precious (oref db :precious))
	   (precious-p (lambda (entry-key)
			 (cdr (memq (car entry-key) precious))))
	   (data (oref db :data))
	   (limit (oref db :max-soft))
	   (candidates (loop for k being the hash-keys of data
			     using (hash-values v)
			     when (notany precious-p v)
			     collect k)))
      (list limit candidates)))

  (defmethod registry-prune-hard-candidates ((db registry-db))
    "Collects pruning candidates from the registry-db object THIS.
Proposes any entries over the max-hard limit minus size * prune-factor."
    (let* ((data (oref db :data))
           ;; prune to (size * prune-factor) below the max-hard limit so
           ;; we're not pruning all the time
	   (limit (max 0 (- (oref db :max-hard)
                            (* (registry-size db) (oref db :prune-factor)))))
	   (candidates (loop for k being the hash-keys of data
			     collect k)))
      (list limit candidates))))

(ert-deftest registry-instantiation-test ()
  (should (registry-db "Testing")))

(ert-deftest registry-match-test ()
  (let ((entry '((hello "goodbye" "bye") (blank))))

    (message "Testing :regex matching")
    (should (registry--match :regex entry '((hello "nye" "bye"))))
    (should (registry--match :regex entry '((hello "good"))))
    (should-not (registry--match :regex entry '((hello "nye"))))
    (should-not (registry--match :regex entry '((hello))))

    (message "Testing :member matching")
    (should (registry--match :member entry '((hello "bye"))))
    (should (registry--match :member entry '((hello "goodbye"))))
    (should-not (registry--match :member entry '((hello "good"))))
    (should-not (registry--match :member entry '((hello "nye"))))
    (should-not (registry--match :member entry '((hello)))))
  (message "Done with matching testing."))

(defun registry-make-testable-db (n &optional name file)
  (let* ((db (registry-db
              (or name "Testing")
              :file (or file "unused")
              :max-hard n
              :max-soft 0               ; keep nothing not precious
              :precious '(extra more-extra)
              :tracked '(sender subject groups))))
    (dotimes (i n)
      (registry-insert db i `((sender "me")
                              (subject "about you")
                              (more-extra) ; empty data key should be pruned
                              ;; first 5 entries will NOT have this extra data
                              ,@(when (< 5 i) (list (list 'extra "more data")))
                              (groups ,(number-to-string i)))))
    db))

(ert-deftest registry-usage-test ()
  (let* ((n 100)
         (db (registry-make-testable-db n)))
    (message "size %d" n)
    (should (= n (registry-size db)))
    (message "max-hard test")
    (should-error (registry-insert db "new" '()))
    (message "Individual lookup")
    (should (= 58 (caadr (registry-lookup db '(1 58 99)))))
    (message "Grouped individual lookup")
    (should (= 3 (length (registry-lookup db '(1 58 99)))))
    (when (boundp 'lexical-binding)
      (message "Individual lookup (breaks before lexbind)")
      (should (= 58
                 (caadr (registry-lookup-breaks-before-lexbind db '(1 58 99)))))
      (message "Grouped individual lookup (breaks before lexbind)")
      (should (= 3
                 (length (registry-lookup-breaks-before-lexbind db
                                                                '(1 58 99))))))
    (message "Search")
    (should (= n (length (registry-search db :all t))))
    (should (= n (length (registry-search db :member '((sender "me"))))))
    (message "Secondary index search")
    (should (= n (length (registry-lookup-secondary-value db 'sender "me"))))
    (should (equal '(74) (registry-lookup-secondary-value db 'groups "74")))
    (message "Delete")
    (should (registry-delete db '(1) t))
    (decf n)
    (message "Search after delete")
    (should (= n (length (registry-search db :all t))))
    (message "Secondary search after delete")
    (should (= n (length (registry-lookup-secondary-value db 'sender "me"))))
    ;; (message "Pruning")
    ;; (let* ((tokeep (registry-search db :member '((extra "more data"))))
    ;;        (count (- n (length tokeep)))
    ;;        (pruned (registry-prune db))
    ;;        (prune-count (length pruned)))
    ;;   (message "Expecting to prune %d entries and pruned %d"
    ;;            count prune-count)
    ;;   (should (and (= count 5)
    ;;                (= count prune-count))))
    (message "Done with usage testing.")))

(ert-deftest registry-persistence-test ()
  (let* ((n 100)
         (tempfile (make-temp-file "registry-persistence-"))
         (name "persistence tester")
         (db (registry-make-testable-db n name tempfile))
         size back)
    (message "Saving to %s" tempfile)
    (eieio-persistent-save db)
    (setq size (nth 7 (file-attributes tempfile)))
    (message "Saved to %s: size %d" tempfile size)
    (should (< 0 size))
    (with-temp-buffer
      (insert-file-contents-literally tempfile)
      (should (looking-at (concat ";; Object "
                                  name
                                  "\n;; EIEIO PERSISTENT OBJECT"))))
    (message "Reading object back")
    (setq back (eieio-persistent-read tempfile))
    (should back)
    (message "Read object back: %d keys, expected %d==%d"
             (registry-size back) n (registry-size db))
    (should (= (registry-size back) n))
    (should (= (registry-size back) (registry-size db)))
    (delete-file tempfile))
  (message "Done with persistence testing."))

(provide 'registry)
;;; registry.el ends here
