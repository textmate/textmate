;;; studly.el --- StudlyCaps (tm)(r)(c)(xxx)

;;; This is in the public domain, since it was distributed
;;; by its author in 1986 without a copyright notice.

;; This file is part of GNU Emacs.

;; Maintainer: FSF
;; Keywords: games

;;; Commentary:

;; Functions to studlycapsify a region, word, or buffer.  Possibly the
;; esoteric significance of studlycapsification escapes you; that is,
;; you suffer from autostudlycapsifibogotification.  Too bad.

;;; Code:

;;;###autoload
(defun studlify-region (begin end)
  "Studlify-case the region."
  (interactive "*r")
  (save-excursion
    (goto-char begin)
    (setq begin (point))
    (while (and (<= (point) end)
		(not (looking-at "\\W*\\'")))
      (forward-word 1)
      (backward-word 1)
      (setq begin (max (point) begin))
      (forward-word 1)
      (let ((offset 0)
	    (word-end (min (point) end))
	    c)
	(goto-char begin)
	(while (< (point) word-end)
	  (setq offset (+ offset (following-char)))
	  (forward-char 1))
	(setq offset (+ offset (following-char)))
	(goto-char begin)
	(while (< (point) word-end)
	  (setq c (following-char))
	  (if (and (= (% (+ c offset) 4) 2)
		   (let ((ch (following-char)))
		     (or (and (>= ch ?a) (<= ch ?z))
			 (and (>= ch ?A) (<= ch ?Z)))))
	      (progn
		(delete-char 1)
		(insert (logxor c ? ))))
	  (forward-char 1))
	(setq begin (point))))))

;;;###autoload
(defun studlify-word (count)
  "Studlify-case the current word, or COUNT words if given an argument."
  (interactive "*p")
  (let ((begin (point)) end rb re)
    (forward-word count)
    (setq end (point))
    (setq rb (min begin end) re (max begin end))
    (studlify-region rb re)))

;;;###autoload
(defun studlify-buffer ()
  "Studlify-case the current buffer."
  (interactive "*")
  (studlify-region (point-min) (point-max)))

(provide 'studly)

;;; studly.el ends here
