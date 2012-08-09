;;; image.el --- image API

;; Copyright (C) 1998-2012 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: multimedia
;; Package: emacs

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

;;; Code:


(defgroup image ()
  "Image support."
  :group 'multimedia)

(defalias 'image-refresh 'image-flush)

(defconst image-type-header-regexps
  `(("\\`/[\t\n\r ]*\\*.*XPM.\\*/" . xpm)
    ("\\`P[1-6][[:space:]]+\\(?:#.*[[:space:]]+\\)*[0-9]+[[:space:]]+[0-9]+" . pbm)
    ("\\`GIF8[79]a" . gif)
    ("\\`\x89PNG\r\n\x1a\n" . png)
    ("\\`[\t\n\r ]*#define \\([a-z0-9_]+\\)_width [0-9]+\n\
#define \\1_height [0-9]+\n\\(\
#define \\1_x_hot [0-9]+\n\
#define \\1_y_hot [0-9]+\n\\)?\
static \\(unsigned \\)?char \\1_bits" . xbm)
    ("\\`\\(?:MM\0\\*\\|II\\*\0\\)" . tiff)
    ("\\`[\t\n\r ]*%!PS" . postscript)
    ("\\`\xff\xd8" . jpeg)    ; used to be (image-jpeg-p . jpeg)
    (,(let* ((incomment-re "\\(?:[^-]\\|-[^-]\\)")
	     (comment-re (concat "\\(?:!--" incomment-re "*-->[ \t\r\n]*<\\)")))
	(concat "\\(?:<\\?xml[ \t\r\n]+[^>]*>\\)?[ \t\r\n]*<"
		comment-re "*"
		"\\(?:!DOCTYPE[ \t\r\n]+[^>]*>[ \t\r\n]*<[ \t\r\n]*" comment-re "*\\)?"
		"[Ss][Vv][Gg]"))
     . svg)
    )
  "Alist of (REGEXP . IMAGE-TYPE) pairs used to auto-detect image types.
When the first bytes of an image file match REGEXP, it is assumed to
be of image type IMAGE-TYPE if IMAGE-TYPE is a symbol.  If not a symbol,
IMAGE-TYPE must be a pair (PREDICATE . TYPE).  PREDICATE is called
with one argument, a string containing the image data.  If PREDICATE returns
a non-nil value, TYPE is the image's type.")

(defvar image-type-file-name-regexps
  '(("\\.png\\'" . png)
    ("\\.gif\\'" . gif)
    ("\\.jpe?g\\'" . jpeg)
    ("\\.bmp\\'" . bmp)
    ("\\.xpm\\'" . xpm)
    ("\\.pbm\\'" . pbm)
    ("\\.xbm\\'" . xbm)
    ("\\.ps\\'" . postscript)
    ("\\.tiff?\\'" . tiff)
    ("\\.svgz?\\'" . svg)
    )
  "Alist of (REGEXP . IMAGE-TYPE) pairs used to identify image files.
When the name of an image file match REGEXP, it is assumed to
be of image type IMAGE-TYPE.")

;; We rely on `auto-mode-alist' to detect xbm and xpm files, instead
;; of content autodetection.  Their contents are just C code, so it is
;; easy to generate false matches.
(defvar image-type-auto-detectable
  '((pbm . t)
    (xbm . nil)
    (bmp . maybe)
    (gif . maybe)
    (png . maybe)
    (xpm . nil)
    (jpeg . maybe)
    (tiff . maybe)
    (svg . maybe)
    (postscript . nil))
  "Alist of (IMAGE-TYPE . AUTODETECT) pairs used to auto-detect image files.
\(See `image-type-auto-detected-p').

AUTODETECT can be
 - t      always auto-detect.
 - nil    never auto-detect.
 - maybe  auto-detect only if the image type is available
	    (see `image-type-available-p').")

(defcustom image-load-path
  (list (file-name-as-directory (expand-file-name "images" data-directory))
        'data-directory 'load-path)
  "List of locations in which to search for image files.
If an element is a string, it defines a directory to search.
If an element is a variable symbol whose value is a string, that
value defines a directory to search.
If an element is a variable symbol whose value is a list, the
value is used as a list of directories to search."
  :type '(repeat (choice directory variable))
  :initialize 'custom-initialize-delay)


(defun image-load-path-for-library (library image &optional path no-error)
  "Return a suitable search path for images used by LIBRARY.

It searches for IMAGE in `image-load-path' (excluding
\"`data-directory'/images\") and `load-path', followed by a path
suitable for LIBRARY, which includes \"../../etc/images\" and
\"../etc/images\" relative to the library file itself, and then
in \"`data-directory'/images\".

Then this function returns a list of directories which contains
first the directory in which IMAGE was found, followed by the
value of `load-path'.  If PATH is given, it is used instead of
`load-path'.

If NO-ERROR is non-nil and a suitable path can't be found, don't
signal an error.  Instead, return a list of directories as before,
except that nil appears in place of the image directory.

Here is an example that uses a common idiom to provide
compatibility with versions of Emacs that lack the variable
`image-load-path':

    ;; Shush compiler.
    (defvar image-load-path)

    (let* ((load-path (image-load-path-for-library \"mh-e\" \"mh-logo.xpm\"))
           (image-load-path (cons (car load-path)
                                  (when (boundp 'image-load-path)
                                    image-load-path))))
      (mh-tool-bar-folder-buttons-init))"
  (unless library (error "No library specified"))
  (unless image   (error "No image specified"))
  (let (image-directory image-directory-load-path)
    ;; Check for images in image-load-path or load-path.
    (let ((img image)
          (dir (or
                ;; Images in image-load-path.
                (image-search-load-path image)
                ;; Images in load-path.
                (locate-library image)))
          parent)
      ;; Since the image might be in a nested directory (for
      ;; example, mail/attach.pbm), adjust `image-directory'
      ;; accordingly.
      (when dir
        (setq dir (file-name-directory dir))
        (while (setq parent (file-name-directory img))
          (setq img (directory-file-name parent)
                dir (expand-file-name "../" dir))))
      (setq image-directory-load-path dir))

    ;; If `image-directory-load-path' isn't Emacs's image directory,
    ;; it's probably a user preference, so use it. Then use a
    ;; relative setting if possible; otherwise, use
    ;; `image-directory-load-path'.
    (cond
     ;; User-modified image-load-path?
     ((and image-directory-load-path
           (not (equal image-directory-load-path
                       (file-name-as-directory
                        (expand-file-name "images" data-directory)))))
      (setq image-directory image-directory-load-path))
     ;; Try relative setting.
     ((let (library-name d1ei d2ei)
        ;; First, find library in the load-path.
        (setq library-name (locate-library library))
        (if (not library-name)
            (error "Cannot find library %s in load-path" library))
        ;; And then set image-directory relative to that.
        (setq
         ;; Go down 2 levels.
         d2ei (file-name-as-directory
               (expand-file-name
                (concat (file-name-directory library-name) "../../etc/images")))
         ;; Go down 1 level.
         d1ei (file-name-as-directory
               (expand-file-name
                (concat (file-name-directory library-name) "../etc/images"))))
        (setq image-directory
              ;; Set it to nil if image is not found.
              (cond ((file-exists-p (expand-file-name image d2ei)) d2ei)
                    ((file-exists-p (expand-file-name image d1ei)) d1ei)))))
     ;; Use Emacs's image directory.
     (image-directory-load-path
      (setq image-directory image-directory-load-path))
     (no-error
      (message "Could not find image %s for library %s" image library))
     (t
      (error "Could not find image %s for library %s" image library)))

    ;; Return an augmented `path' or `load-path'.
    (nconc (list image-directory)
           (delete image-directory (copy-sequence (or path load-path))))))


;; Used to be in image-type-header-regexps, but now not used anywhere
;; (since 2009-08-28).
(defun image-jpeg-p (data)
  "Value is non-nil if DATA, a string, consists of JFIF image data.
We accept the tag Exif because that is the same format."
  (setq data (ignore-errors (string-to-unibyte data)))
  (when (and data (string-match-p "\\`\xff\xd8" data))
    (catch 'jfif
      (let ((len (length data)) (i 2))
	(while (< i len)
	  (when (/= (aref data i) #xff)
	    (throw 'jfif nil))
	  (setq i (1+ i))
	  (when (>= (+ i 2) len)
	    (throw 'jfif nil))
	  (let ((nbytes (+ (lsh (aref data (+ i 1)) 8)
			   (aref data (+ i 2))))
		(code (aref data i)))
	    (when (and (>= code #xe0) (<= code #xef))
	      ;; APP0 LEN1 LEN2 "JFIF\0"
	      (throw 'jfif
		     (string-match-p "JFIF\\|Exif"
				     (substring data i (min (+ i nbytes) len)))))
	    (setq i (+ i 1 nbytes))))))))


;;;###autoload
(defun image-type-from-data (data)
  "Determine the image type from image data DATA.
Value is a symbol specifying the image type or nil if type cannot
be determined."
  (let ((types image-type-header-regexps)
	type)
    (while types
      (let ((regexp (car (car types)))
	    (image-type (cdr (car types))))
	(if (or (and (symbolp image-type)
		     (string-match-p regexp data))
		(and (consp image-type)
		     (funcall (car image-type) data)
		     (setq image-type (cdr image-type))))
	    (setq type image-type
		  types nil)
	  (setq types (cdr types)))))
    type))


;;;###autoload
(defun image-type-from-buffer ()
  "Determine the image type from data in the current buffer.
Value is a symbol specifying the image type or nil if type cannot
be determined."
  (let ((types image-type-header-regexps)
	type
	(opoint (point)))
    (goto-char (point-min))
    (while types
      (let ((regexp (car (car types)))
	    (image-type (cdr (car types)))
	    data)
	(if (or (and (symbolp image-type)
		     (looking-at-p regexp))
		(and (consp image-type)
		     (funcall (car image-type)
			      (or data
				  (setq data
					(buffer-substring
					 (point-min)
					 (min (point-max)
					      (+ (point-min) 256))))))
		     (setq image-type (cdr image-type))))
	    (setq type image-type
		  types nil)
	  (setq types (cdr types)))))
    (goto-char opoint)
    type))


;;;###autoload
(defun image-type-from-file-header (file)
  "Determine the type of image file FILE from its first few bytes.
Value is a symbol specifying the image type, or nil if type cannot
be determined."
  (unless (or (file-readable-p file)
	      (file-name-absolute-p file))
    (setq file (image-search-load-path file)))
  (and file
       (file-readable-p file)
       (with-temp-buffer
	 (set-buffer-multibyte nil)
	 (insert-file-contents-literally file nil 0 256)
	 (image-type-from-buffer))))


;;;###autoload
(defun image-type-from-file-name (file)
  "Determine the type of image file FILE from its name.
Value is a symbol specifying the image type, or nil if type cannot
be determined."
  (assoc-default file image-type-file-name-regexps 'string-match-p))


;;;###autoload
(defun image-type (source &optional type data-p)
  "Determine and return image type.
SOURCE is an image file name or image data.
Optional TYPE is a symbol describing the image type.  If TYPE is omitted
or nil, try to determine the image type from its first few bytes
of image data.  If that doesn't work, and SOURCE is a file name,
use its file extension as image type.
Optional DATA-P non-nil means SOURCE is a string containing image data."
  (when (and (not data-p) (not (stringp source)))
    (error "Invalid image file name `%s'" source))
  (unless type
    (setq type (if data-p
		   (image-type-from-data source)
		 (or (image-type-from-file-header source)
		     (image-type-from-file-name source))))
    (or type (error "Cannot determine image type")))
  (or (memq type (and (boundp 'image-types) image-types))
      (error "Invalid image type `%s'" type))
  type)


(if (fboundp 'image-metadata)           ; eg not --without-x
    (define-obsolete-function-alias 'image-extension-data
      'image-metadata' "24.1"))

(define-obsolete-variable-alias
    'image-library-alist
    'dynamic-library-alist "24.1")

;;;###autoload
(defun image-type-available-p (type)
  "Return non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'."
  (and (fboundp 'init-image-library)
       (init-image-library type dynamic-library-alist)))


;;;###autoload
(defun image-type-auto-detected-p ()
  "Return t if the current buffer contains an auto-detectable image.
This function is intended to be used from `magic-fallback-mode-alist'.

The buffer is considered to contain an auto-detectable image if
its beginning matches an image type in `image-type-header-regexps',
and that image type is present in `image-type-auto-detectable' with a
non-nil value.  If that value is non-nil, but not t, then the image type
must be available."
  (let* ((type (image-type-from-buffer))
	 (auto (and type (cdr (assq type image-type-auto-detectable)))))
    (and auto
	 (or (eq auto t) (image-type-available-p type)))))


;;;###autoload
(defun create-image (file-or-data &optional type data-p &rest props)
  "Create an image.
FILE-OR-DATA is an image file name or image data.
Optional TYPE is a symbol describing the image type.  If TYPE is omitted
or nil, try to determine the image type from its first few bytes
of image data.  If that doesn't work, and FILE-OR-DATA is a file name,
use its file extension as image type.
Optional DATA-P non-nil means FILE-OR-DATA is a string containing image data.
Optional PROPS are additional image attributes to assign to the image,
like, e.g. `:mask MASK'.
Value is the image created, or nil if images of type TYPE are not supported.

Images should not be larger than specified by `max-image-size'.

Image file names that are not absolute are searched for in the
\"images\" sub-directory of `data-directory' and
`x-bitmap-file-path' (in that order)."
  ;; It is x_find_image_file in image.c that sets the search path.
  (setq type (image-type file-or-data type data-p))
  (when (image-type-available-p type)
    (append (list 'image :type type (if data-p :data :file) file-or-data)
	    props)))


;;;###autoload
(defun put-image (image pos &optional string area)
  "Put image IMAGE in front of POS in the current buffer.
IMAGE must be an image created with `create-image' or `defimage'.
IMAGE is displayed by putting an overlay into the current buffer with a
`before-string' STRING that has a `display' property whose value is the
image.  STRING is defaulted if you omit it.
The overlay created will have the `put-image' property set to t.
POS may be an integer or marker.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area."
  (unless string (setq string "x"))
  (let ((buffer (current-buffer)))
    (unless (eq (car-safe image) 'image)
      (error "Not an image: %s" image))
    (unless (or (null area) (memq area '(left-margin right-margin)))
      (error "Invalid area %s" area))
    (setq string (copy-sequence string))
    (let ((overlay (make-overlay pos pos buffer))
	  (prop (if (null area) image (list (list 'margin area) image))))
      (put-text-property 0 (length string) 'display prop string)
      (overlay-put overlay 'put-image t)
      (overlay-put overlay 'before-string string))))


;;;###autoload
(defun insert-image (image &optional string area slice)
  "Insert IMAGE into current buffer at point.
IMAGE is displayed by inserting STRING into the current buffer
with a `display' property whose value is the image.  STRING
defaults to the empty string if you omit it.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
SLICE specifies slice of IMAGE to insert.  SLICE nil or omitted
means insert whole image.  SLICE is a list (X Y WIDTH HEIGHT)
specifying the X and Y positions and WIDTH and HEIGHT of image area
to insert.  A float value 0.0 - 1.0 means relative to the width or
height of the image; integer values are taken as pixel values."
  ;; Use a space as least likely to cause trouble when it's a hidden
  ;; character in the buffer.
  (unless string (setq string " "))
  (unless (eq (car-safe image) 'image)
    (error "Not an image: %s" image))
  (unless (or (null area) (memq area '(left-margin right-margin)))
    (error "Invalid area %s" area))
  (if area
      (setq image (list (list 'margin area) image))
    ;; Cons up a new spec equal but not eq to `image' so that
    ;; inserting it twice in a row (adjacently) displays two copies of
    ;; the image.  Don't try to avoid this by looking at the display
    ;; properties on either side so that we DTRT more often with
    ;; cut-and-paste.  (Yanking killed image text next to another copy
    ;; of it loses anyway.)
    (setq image (cons 'image (cdr image))))
  (let ((start (point)))
    (insert string)
    (add-text-properties start (point)
			 `(display ,(if slice
					(list (cons 'slice slice) image)
				      image) rear-nonsticky (display)))))


;;;###autoload
(defun insert-sliced-image (image &optional string area rows cols)
  "Insert IMAGE into current buffer at point.
IMAGE is displayed by inserting STRING into the current buffer
with a `display' property whose value is the image.  STRING is
defaulted if you omit it.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
The image is automatically split into ROWS x COLS slices."
  (unless string (setq string " "))
  (unless (eq (car-safe image) 'image)
    (error "Not an image: %s" image))
  (unless (or (null area) (memq area '(left-margin right-margin)))
    (error "Invalid area %s" area))
  (if area
      (setq image (list (list 'margin area) image))
    ;; Cons up a new spec equal but not eq to `image' so that
    ;; inserting it twice in a row (adjacently) displays two copies of
    ;; the image.  Don't try to avoid this by looking at the display
    ;; properties on either side so that we DTRT more often with
    ;; cut-and-paste.  (Yanking killed image text next to another copy
    ;; of it loses anyway.)
    (setq image (cons 'image (cdr image))))
  (let ((x 0.0) (dx (/ 1.0001 (or cols 1)))
	 (y 0.0) (dy (/ 1.0001 (or rows 1))))
    (while (< y 1.0)
      (while (< x 1.0)
	(let ((start (point)))
	  (insert string)
	  (add-text-properties start (point)
			       `(display ,(list (list 'slice x y dx dy) image)
					 rear-nonsticky (display)))
	  (setq x (+ x dx))))
      (setq x 0.0
	    y (+ y dy))
      (insert (propertize "\n" 'line-height t)))))



;;;###autoload
(defun remove-images (start end &optional buffer)
  "Remove images between START and END in BUFFER.
Remove only images that were put in BUFFER with calls to `put-image'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (let ((overlays (overlays-in start end)))
    (while overlays
      (let ((overlay (car overlays)))
	(when (overlay-get overlay 'put-image)
	  (delete-overlay overlay)))
      (setq overlays (cdr overlays)))))

(defun image-search-load-path (file &optional path)
  (unless path
    (setq path image-load-path))
  (let (element found filename)
    (while (and (not found) (consp path))
      (setq element (car path))
      (cond
       ((stringp element)
	(setq found
	      (file-readable-p
	       (setq filename (expand-file-name file element)))))
       ((and (symbolp element) (boundp element))
	(setq element (symbol-value element))
	(cond
	 ((stringp element)
	  (setq found
		(file-readable-p
		 (setq filename (expand-file-name file element)))))
	 ((consp element)
	  (if (setq filename (image-search-load-path file element))
	      (setq found t))))))
      (setq path (cdr path)))
    (if found filename)))

;;;###autoload
(defun find-image (specs)
  "Find an image, choosing one of a list of image specifications.

SPECS is a list of image specifications.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The specification whose TYPE
is supported, and FILE exists, is used to construct the image
specification to be returned.  Return nil if no specification is
satisfied.

The image is looked for in `image-load-path'.

Image files should not be larger than specified by `max-image-size'."
  (let (image)
    (while (and specs (null image))
      (let* ((spec (car specs))
	     (type (plist-get spec :type))
	     (data (plist-get spec :data))
	     (file (plist-get spec :file))
	     found)
	(when (image-type-available-p type)
	  (cond ((stringp file)
		 (if (setq found (image-search-load-path file))
		     (setq image
			   (cons 'image (plist-put (copy-sequence spec)
						   :file found)))))
		((not (null data))
		 (setq image (cons 'image spec)))))
	(setq specs (cdr specs))))
    image))


;;;###autoload
(defmacro defimage (symbol specs &optional doc)
  "Define SYMBOL as an image.

SPECS is a list of image specifications.  DOC is an optional
documentation string.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The first image
specification whose TYPE is supported, and FILE exists, is used to
define SYMBOL.

Example:

   (defimage test-image ((:type xpm :file \"~/test1.xpm\")
                         (:type xbm :file \"~/test1.xbm\")))"
  (declare (doc-string 3))
  `(defvar ,symbol (find-image ',specs) ,doc))


;;; Animated image API

(defconst image-animated-types '(gif)
  "List of supported animated image types.")

(defun image-animated-p (image)
  "Return non-nil if IMAGE can be animated.
To be capable of being animated, an image must be of a type
listed in `image-animated-types', and contain more than one
sub-image, with a specified animation delay.  The actual return
value is a cons (NIMAGES . DELAY), where NIMAGES is the number
of sub-images in the animated image and DELAY is the delay in
seconds until the next sub-image should be displayed."
  (cond
   ((memq (plist-get (cdr image) :type) image-animated-types)
    (let* ((metadata (image-metadata image))
	   (images (plist-get metadata 'count))
	   (delay (plist-get metadata 'delay)))
      (when (and images (> images 1) (numberp delay))
	(if (< delay 0) (setq delay 0.1))
	(cons images delay))))))

;; "Destructively"?
(defun image-animate (image &optional index limit)
  "Start animating IMAGE.
Animation occurs by destructively altering the IMAGE spec list.

With optional INDEX, begin animating from that animation frame.
LIMIT specifies how long to animate the image.  If omitted or
nil, play the animation until the end.  If t, loop forever.  If a
number, play until that number of seconds has elapsed."
  (let ((animation (image-animated-p image))
	timer)
    (when animation
      (if (setq timer (image-animate-timer image))
	  (cancel-timer timer))
      (run-with-timer 0.2 nil 'image-animate-timeout
		      image (or index 0) (car animation)
		      0 limit))))

(defun image-animate-timer (image)
  "Return the animation timer for image IMAGE."
  ;; See cancel-function-timers
  (let ((tail timer-list) timer)
    (while tail
      (setq timer (car tail)
	    tail (cdr tail))
      (if (and (eq (aref timer 5) 'image-animate-timeout)
	       (eq (car-safe (aref timer 6)) image))
	  (setq tail nil)
	(setq timer nil)))
    timer))

;; FIXME? The delay may not be the same for different sub-images,
;; hence we need to call image-animated-p to return it.
;; But it also returns count, so why do we bother passing that as an
;; argument?
(defun image-animate-timeout (image n count time-elapsed limit)
  "Display animation frame N of IMAGE.
N=0 refers to the initial animation frame.
COUNT is the total number of frames in the animation.
TIME-ELAPSED is the total time that has elapsed since
`image-animate-start' was called.
LIMIT determines when to stop.  If t, loop forever.  If nil, stop
 after displaying the last animation frame.  Otherwise, stop
 after LIMIT seconds have elapsed.
The minimum delay between successive frames is 0.01s."
  (plist-put (cdr image) :index n)
  (force-window-update)
  (setq n (1+ n))
  (let* ((time (float-time))
	 (animation (image-animated-p image))
	 ;; Subtract off the time we took to load the image from the
	 ;; stated delay time.
	 (delay (max (+ (cdr animation) time (- (float-time)))
		     0.01))
	 done)
    (if (>= n count)
	(if limit
	    (setq n 0)
	  (setq done t)))
    (setq time-elapsed (+ delay time-elapsed))
    (if (numberp limit)
	(setq done (>= time-elapsed limit)))
    (unless done
      (run-with-timer delay nil 'image-animate-timeout
		      image n count time-elapsed limit))))


(defcustom imagemagick-types-inhibit
  '(C HTML HTM TXT PDF)
  "ImageMagick types that should not be visited in Image mode.
This should be a list of symbols, each of which should be one of
the ImageMagick types listed in `imagemagick-types'.  These image
types are not registered by `imagemagick-register-types'.

If Emacs is compiled without ImageMagick support, this variable
has no effect."
  :type '(choice (const :tag "Let ImageMagick handle all types it can" nil)
		 (repeat symbol))
  ;; Ideally, would have a :set function that checks if we already did
  ;; imagemagick-register-types, and if so undoes it, then redoes it.
  :version "24.1"
  :group 'image)

;;;###autoload
(defun imagemagick-register-types ()
  "Register file types that can be handled by ImageMagick.
This registers the ImageMagick types listed in `imagemagick-types',
excluding those listed in `imagemagick-types-inhibit'.

Registered image types are added to `auto-mode-alist', so that
Emacs visits them in Image mode.  They are also added to
`image-type-file-name-regexps', so that the `image-type' function
recognizes these files as having image type `imagemagick'.

If Emacs is compiled without ImageMagick support, do nothing."
  (when (fboundp 'imagemagick-types)
    (let ((im-types '()))
      (dolist (im-type (imagemagick-types))
        (unless (memq im-type imagemagick-types-inhibit)
          (push (downcase (symbol-name im-type)) im-types)))
      (let ((extension (concat "\\." (regexp-opt im-types) "\\'")))
        (push (cons extension 'image-mode) auto-mode-alist)
        (push (cons extension 'imagemagick)
              image-type-file-name-regexps)))))

(provide 'image)

;;; image.el ends here
