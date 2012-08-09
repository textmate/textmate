;;; gnus-html.el --- Render HTML in a buffer.

;; Copyright (C) 2010-2012  Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: html, web

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

;; The idea is to provide a simple, fast and pretty minimal way to
;; render HTML (including links and images) in a buffer, based on an
;; external HTML renderer (i.e., w3m).

;;; Code:

(eval-when-compile (require 'cl))

(require 'gnus-art)
(eval-when-compile (require 'mm-decode))

(require 'mm-url)
(require 'url)
(require 'url-cache)
(require 'xml)
(require 'browse-url)
(require 'mm-util)
(eval-and-compile (unless (featurep 'xemacs) (require 'help-fns)))

(defcustom gnus-html-image-cache-ttl (days-to-time 7)
  "Time used to determine if we should use images from the cache."
  :version "24.1"
  :group 'gnus-art
  :type 'integer)

(defcustom gnus-html-image-automatic-caching t
  "Whether automatically cache retrieve images."
  :version "24.1"
  :group 'gnus-art
  :type 'boolean)

(defcustom gnus-html-frame-width 70
  "What width to use when rendering HTML."
  :version "24.1"
  :group 'gnus-art
  :type 'integer)

(defcustom gnus-max-image-proportion 0.9
  "How big pictures displayed are in relation to the window they're in.
A value of 0.7 means that they are allowed to take up 70% of the
width and height of the window.  If they are larger than this,
and Emacs supports it, then the images will be rescaled down to
fit these criteria."
  :version "24.1"
  :group 'gnus-art
  :type 'float)

(defvar gnus-html-image-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" 'gnus-article-copy-string)
    (define-key map "i" 'gnus-html-insert-image)
    (define-key map "v" 'gnus-html-browse-url)
    map))

(defvar gnus-html-displayed-image-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" 'gnus-html-show-alt-text)
    (define-key map "i" 'gnus-html-browse-image)
    (define-key map "\r" 'gnus-html-browse-url)
    (define-key map "u" 'gnus-article-copy-string)
    (define-key map [tab] 'widget-forward)
    map))

(eval-and-compile
  (defalias 'gnus-html-encode-url-chars
    (if (fboundp 'browse-url-url-encode-chars)
	'browse-url-url-encode-chars
      (lambda (text chars)
	"URL-encode the chars in TEXT that match CHARS.
CHARS is a regexp-like character alternative (e.g., \"[)$]\")."
	(let ((encoded-text (copy-sequence text))
	      (s 0))
	  (while (setq s (string-match chars encoded-text s))
	    (setq encoded-text
		  (replace-match (format "%%%x"
					 (string-to-char
					  (match-string 0 encoded-text)))
				 t t encoded-text)
		  s (1+ s)))
	  encoded-text)))))

(defun gnus-html-encode-url (url)
  "Encode URL."
  (gnus-html-encode-url-chars url "[)$ ]"))

(defun gnus-html-cache-expired (url ttl)
  "Check if URL is cached for more than TTL."
  (cond (url-standalone-mode
         (not (file-exists-p (url-cache-create-filename url))))
        (t (let ((cache-time (url-is-cached url)))
             (if cache-time
                 (time-less-p
                  (time-add
                   cache-time
                   ttl)
                  (current-time))
               t)))))

;;;###autoload
(defun gnus-article-html (&optional handle)
  (let ((article-buffer (current-buffer)))
    (unless handle
      (setq handle (mm-dissect-buffer t)))
    (save-restriction
      (narrow-to-region (point) (point))
      (save-excursion
	(mm-with-part handle
	  (let* ((coding-system-for-read 'utf-8)
		 (coding-system-for-write 'utf-8)
		 (default-process-coding-system
		   (cons coding-system-for-read coding-system-for-write))
		 (charset (mail-content-type-get (mm-handle-type handle)
						 'charset)))
	    (when (and charset
		       (setq charset (mm-charset-to-coding-system charset))
		       (not (eq charset 'ascii)))
	      (insert (prog1
			  (mm-decode-coding-string (buffer-string) charset)
			(erase-buffer)
			(mm-enable-multibyte))))
	    (call-process-region (point-min) (point-max)
				 "w3m"
				 nil article-buffer nil
				 "-halfdump"
				 "-no-cookie"
				 "-I" "UTF-8"
				 "-O" "UTF-8"
				 "-o" "ext_halfdump=1"
                                 "-o" "display_ins_del=2"
				 "-o" "pre_conv=1"
				 "-t" (format "%s" tab-width)
				 "-cols" (format "%s" gnus-html-frame-width)
				 "-o" "display_image=on"
				 "-T" "text/html"))))
      (gnus-html-wash-tags))))

(defvar gnus-article-mouse-face)

(defun gnus-html-pre-wash ()
  (goto-char (point-min))
  (while (re-search-forward " *<pre_int> *</pre_int> *\n" nil t)
    (replace-match "" t t))
  (goto-char (point-min))
  (while (re-search-forward "<a name[^\n>]+>" nil t)
    (replace-match "" t t)))

(defun gnus-html-wash-images ()
  "Run through current buffer and replace img tags by images."
  (let (tag parameters string start end images url alt-text
	    inhibit-images blocked-images)
    (if (buffer-live-p gnus-summary-buffer)
	(with-current-buffer gnus-summary-buffer
	  (setq inhibit-images gnus-inhibit-images
		blocked-images (gnus-blocked-images)))
      (setq inhibit-images gnus-inhibit-images
	    blocked-images (gnus-blocked-images)))
    (goto-char (point-min))
    ;; Search for all the images first.
    (while (re-search-forward "<img_alt \\([^>]*\\)>" nil t)
      (setq parameters (match-string 1)
	    start (match-beginning 0))
      (delete-region start (point))
      (when (search-forward "</img_alt>" (line-end-position) t)
	(delete-region (match-beginning 0) (match-end 0)))
      (setq end (point))
      (when (string-match "src=\"\\([^\"]+\\)" parameters)
	(gnus-message 8 "gnus-html-wash-tags: fetching image URL %s" url)
	(setq url (gnus-html-encode-url (match-string 1 parameters))
	      alt-text (when (string-match "\\(alt\\|title\\)=\"\\([^\"]+\\)"
					   parameters)
			 (xml-substitute-special (match-string 2 parameters))))
	(gnus-add-text-properties
	 start end
	 (list 'image-url url
	       'image-displayer `(lambda (url start end)
				   (gnus-html-display-image url start end
							    ,alt-text))
	       'gnus-image (list url start end alt-text)))
	(widget-convert-button
	 'url-link start (point)
	 :help-echo alt-text
	 :keymap gnus-html-image-map
	 url)
	(if (string-match "\\`cid:" url)
	    ;; URLs with cid: have their content stashed in other
	    ;; parts of the MIME structure, so just insert them
	    ;; immediately.
	    (let* ((handle (mm-get-content-id (substring url (match-end 0))))
		   (image (when (and handle
				     (not inhibit-images))
			    (gnus-create-image
			     (mm-with-part handle (buffer-string))
			     nil t))))
	      (if image
		  (gnus-add-image
		   'cid
		   (gnus-put-image
		    (gnus-rescale-image
		     image (gnus-html-maximum-image-size))
		    (gnus-string-or (prog1
					(buffer-substring start end)
				      (delete-region start end))
				    "*")
		    'cid))
		(widget-convert-button
		 'link start end
		 :action 'gnus-html-insert-image
		 :help-echo url
		 :keymap gnus-html-image-map
		 :button-keymap gnus-html-image-map)))
	  ;; Normal, external URL.
	  (if (or inhibit-images
		  (gnus-html-image-url-blocked-p url blocked-images))
	      (widget-convert-button
	       'link start end
	       :action 'gnus-html-insert-image
	       :help-echo url
	       :keymap gnus-html-image-map
	       :button-keymap gnus-html-image-map)
	    ;; Non-blocked url
	    (let ((width
		   (when (string-match "width=\"?\\([0-9]+\\)" parameters)
		     (string-to-number (match-string 1 parameters))))
		  (height
		   (when (string-match "height=\"?\\([0-9]+\\)" parameters)
		     (string-to-number (match-string 1 parameters)))))
	      ;; Don't fetch images that are really small.  They're
	      ;; probably tracking pictures.
	      (when (and (or (null height)
			     (> height 4))
			 (or (null width)
			     (> width 4)))
		(gnus-html-display-image url start end alt-text)))))))))

(defun gnus-html-display-image (url start end &optional alt-text)
  "Display image at URL on text from START to END.
Use ALT-TEXT for the image string."
  (or alt-text (setq alt-text "*"))
  (if (string-match "\\`cid:" url)
      (let ((handle (mm-get-content-id (substring url (match-end 0)))))
	(when handle
	  (gnus-html-put-image (mm-with-part handle (buffer-string))
			       url alt-text)))
    (if (gnus-html-cache-expired url gnus-html-image-cache-ttl)
	;; We don't have it, so schedule it for fetching
	;; asynchronously.
	(gnus-html-schedule-image-fetching
	 (current-buffer)
	 (list url alt-text))
      ;; It's already cached, so just insert it.
      (gnus-html-put-image (gnus-html-get-image-data url) url alt-text))))

(defun gnus-html-wash-tags ()
  (let (tag parameters string start end images url)
    (gnus-html-pre-wash)
    (gnus-html-wash-images)

    (goto-char (point-min))
    ;; Then do the other tags.
    (while (re-search-forward "<\\([^ />]+\\)\\([^>]*\\)>" nil t)
      (setq tag (match-string 1)
	    parameters (match-string 2)
	    start (match-beginning 0))
      (when (> (length parameters) 0)
	(set-text-properties 0 (1- (length parameters)) nil parameters))
      (delete-region start (point))
      (when (search-forward (concat "</" tag ">") nil t)
	(delete-region (match-beginning 0) (match-end 0)))
      (setq end (point))
      (cond
       ;; Fetch and insert a picture.
       ((equal tag "img_alt"))
       ;; Add a link.
       ((or (equal tag "a")
	    (equal tag "A"))
	(when (string-match "href=\"\\([^\"]+\\)" parameters)
	  (setq url (match-string 1 parameters))
          (gnus-message 8 "gnus-html-wash-tags: fetching link URL %s" url)
	  (gnus-article-add-button start end
				   'browse-url (mm-url-decode-entities-string url)
				   url)
	  (let ((overlay (gnus-make-overlay start end)))
	    (gnus-overlay-put overlay 'evaporate t)
	    (gnus-overlay-put overlay 'gnus-button-url url)
	    (gnus-put-text-property start end 'gnus-string url)
	    (when gnus-article-mouse-face
	      (gnus-overlay-put overlay 'mouse-face gnus-article-mouse-face)))))
       ;; The upper-case IMG_ALT is apparently just an artifact that
       ;; should be deleted.
       ((equal tag "IMG_ALT")
	(delete-region start end))
       ;; w3m does not normalize the case
       ((or (equal tag "b")
            (equal tag "B"))
        (gnus-overlay-put (gnus-make-overlay start end) 'face 'gnus-emphasis-bold))
       ((or (equal tag "u")
            (equal tag "U"))
        (gnus-overlay-put (gnus-make-overlay start end) 'face 'gnus-emphasis-underline))
       ((or (equal tag "i")
            (equal tag "I"))
        (gnus-overlay-put (gnus-make-overlay start end) 'face 'gnus-emphasis-italic))
       ((or (equal tag "s")
            (equal tag "S"))
        (gnus-overlay-put (gnus-make-overlay start end) 'face 'gnus-emphasis-strikethru))
       ((or (equal tag "ins")
            (equal tag "INS"))
        (gnus-overlay-put (gnus-make-overlay start end) 'face 'gnus-emphasis-underline))
       ;; Handle different UL types
       ((equal tag "_SYMBOL")
        (when (string-match "TYPE=\\(.+\\)" parameters)
          (let ((type (string-to-number (match-string 1 parameters))))
            (delete-region start end)
            (cond ((= type 33) (insert " "))
                  ((= type 34) (insert " "))
                  ((= type 35) (insert " "))
                  ((= type 36) (insert " "))
                  ((= type 37) (insert " "))
                  ((= type 38) (insert " "))
                  ((= type 39) (insert " "))
                  ((= type 40) (insert " "))
                  ((= type 42) (insert " "))
                  ((= type 43) (insert " "))
                  (t (insert " "))))))
       ;; Whatever.  Just ignore the tag.
       (t
	))
      (goto-char start))
    (goto-char (point-min))
    ;; The output from -halfdump isn't totally regular, so strip
    ;; off any </pre_int>s that were left over.
    (while (re-search-forward "</pre_int>\\|</internal>" nil t)
      (replace-match "" t t))
    (mm-url-decode-entities)))

(defun gnus-html-insert-image (&rest args)
  "Fetch and insert the image under point."
  (interactive)
  (apply 'gnus-html-display-image (get-text-property (point) 'gnus-image)))

(defun gnus-html-show-alt-text ()
  "Show the ALT text of the image under point."
  (interactive)
  (message "%s" (get-text-property (point) 'gnus-alt-text)))

(defun gnus-html-browse-image ()
  "Browse the image under point."
  (interactive)
  (browse-url (get-text-property (point) 'image-url)))

(defun gnus-html-browse-url ()
  "Browse the image under point."
  (interactive)
  (let ((url (get-text-property (point) 'gnus-string)))
    (cond
     ((not url)
      (message "No link under point"))
     ((string-match "^mailto:" url)
      (gnus-url-mailto url))
     (t
      (browse-url url)))))

(defun gnus-html-schedule-image-fetching (buffer image)
  "Retrieve IMAGE, and place it into BUFFER on arrival."
  (gnus-message 8 "gnus-html-schedule-image-fetching: buffer %s, image %s"
                buffer image)
  (if (fboundp 'url-queue-retrieve)
      (url-queue-retrieve (car image)
			  'gnus-html-image-fetched
			  (list buffer image) t t)
    (ignore-errors
      (url-retrieve (car image)
		    'gnus-html-image-fetched
		    (list buffer image)))))

(defun gnus-html-image-fetched (status buffer image)
  "Callback function called when image has been fetched."
  (unless (plist-get status :error)
    (when (and (or (search-forward "\n\n" nil t)
                   (search-forward "\r\n\r\n" nil t))
	       (not (eobp)))
      (when gnus-html-image-automatic-caching
	(url-store-in-cache (current-buffer)))
      (when (buffer-live-p buffer)
	(let ((data (buffer-substring (point) (point-max))))
	  (with-current-buffer buffer
	    (let ((inhibit-read-only t))
	      (gnus-html-put-image data (car image) (cadr image))))))))
  (kill-buffer (current-buffer)))

(defun gnus-html-get-image-data (url)
  "Get image data for URL.
Return a string with image data."
  (with-temp-buffer
    (mm-disable-multibyte)
    (url-cache-extract (url-cache-create-filename url))
    (when (or (search-forward "\n\n" nil t)
              (search-forward "\r\n\r\n" nil t))
      (buffer-substring (point) (point-max)))))

(defun gnus-html-maximum-image-size ()
  "Return the maximum size of an image according to `gnus-max-image-proportion'."
  (let ((edges (gnus-window-inside-pixel-edges
                (get-buffer-window (current-buffer)))))
    ;; (width . height)
    (cons
     ;; Aimed width
     (truncate
      (* gnus-max-image-proportion
         (- (nth 2 edges) (nth 0 edges))))
     ;; Aimed height
     (truncate (* gnus-max-image-proportion
                  (- (nth 3 edges) (nth 1 edges)))))))

(defun gnus-html-put-image (data url &optional alt-text)
  "Put an image with DATA from URL and optional ALT-TEXT."
  (when (gnus-graphic-display-p)
    (let* ((start (text-property-any (point-min) (point-max)
				     'image-url url))
           (end (when start
                  (next-single-property-change start 'image-url))))
      ;; Image found?
      (when start
        (let* ((image
                (ignore-errors
                  (gnus-create-image data nil t)))
               (size (and image
                          (if (featurep 'xemacs)
                              (cons (glyph-width image) (glyph-height image))
                            (image-size image t)))))
          (save-excursion
            (goto-char start)
            (let ((alt-text (or alt-text
				(buffer-substring-no-properties start end)))
		  (inhibit-read-only t))
              (if (and image
                       ;; Kludge to avoid displaying 30x30 gif images, which
                       ;; seems to be a signal of a broken image.
                       (not (and (if (featurep 'xemacs)
                                     (glyphp image)
                                   (listp image))
                                 (eq (if (featurep 'xemacs)
                                         (let ((d (cdadar
						   (specifier-spec-list
						    (glyph-image image)))))
                                           (and (vectorp d)
                                                (aref d 0)))
                                       (plist-get (cdr image) :type))
                                     'gif)
                                 (= (car size) 30)
                                 (= (cdr size) 30))))
                  ;; Good image, add it!
                  (let ((image (gnus-rescale-image image (gnus-html-maximum-image-size))))
                    (delete-region start end)
                    (gnus-put-image image alt-text 'external)
		    (widget-convert-button
		     'url-link start (point)
		     :help-echo alt-text
		     :keymap gnus-html-displayed-image-map
		     url)
                    (gnus-put-text-property start (point)
					    'gnus-alt-text alt-text)
                    (when url
		      (gnus-add-text-properties
		       start (point)
		       `(image-url
			 ,url
			 image-displayer
			 (lambda (url start end)
			   (gnus-html-display-image url start end
						    ,alt-text)))))
                    (gnus-add-image 'external image)
                    t)
                ;; Bad image, try to show something else
                (when (fboundp 'find-image)
                  (delete-region start end)
                  (setq image (find-image
			       '((:type xpm :file "lock-broken.xpm"))))
                  (gnus-put-image image alt-text 'internal)
                  (gnus-add-image 'internal image))
                nil))))))))

(defun gnus-html-image-url-blocked-p (url blocked-images)
  "Find out if URL is blocked by BLOCKED-IMAGES."
  (let ((ret (and blocked-images
                  (string-match blocked-images url))))
    (if ret
        (gnus-message 8 "gnus-html-image-url-blocked-p: %s blocked by regex %s"
                      url blocked-images)
      (gnus-message 9 "gnus-html-image-url-blocked-p: %s passes regex %s"
                    url blocked-images))
    ret))

;;;###autoload
(defun gnus-html-prefetch-images (summary)
  (when (buffer-live-p summary)
    (let (inhibit-images blocked-images)
      (with-current-buffer summary
	(setq inhibit-images gnus-inhibit-images
	      blocked-images (gnus-blocked-images)))
      (save-match-data
	(while (re-search-forward "<img[^>]+src=[\"']\\(http[^\"']+\\)" nil t)
	  (let ((url (gnus-html-encode-url
		      (mm-url-decode-entities-string (match-string 1)))))
	    (unless (or inhibit-images
			(gnus-html-image-url-blocked-p url blocked-images))
              (when (gnus-html-cache-expired url gnus-html-image-cache-ttl)
                (gnus-html-schedule-image-fetching nil
                                                   (list url))))))))))

(provide 'gnus-html)

;;; gnus-html.el ends here
