;;; zone.el --- idle display hacks

;; Copyright (C) 2000-2012  Free Software Foundation, Inc.

;; Author: Victor Zandy <zandy@cs.wisc.edu>
;; Maintainer: Thien-Thi Nguyen <ttn@gnu.org>
;; Keywords: games
;; Created: June 6, 1998

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

;; Don't zone out in front of Emacs!  Try M-x zone.
;; If it eventually irritates you, try M-x zone-leave-me-alone.

;; Bored by the zone pyrotechnics?  Write your own!  Add it to
;; `zone-programs'.  See `zone-call' for higher-ordered zoning.

;; WARNING: Not appropriate for Emacs sessions over modems or
;;          computers as slow as mine.

;; THANKS: Christopher Mayer, Scott Flinchbaugh,
;;         Rachel Kalmar, Max Froumentin, Juri Linkov,
;;         Luigi Panzeri, John Paul Wallington.

;;; Code:

(defvar zone-timer nil
  "The timer we use to decide when to zone out, or nil if none.")

(defvar zone-timeout nil
  "*Seconds to timeout the zoning.
If nil, don't interrupt for about 1^26 seconds.")

;; Vector of functions that zone out.  `zone' will execute one of
;; these functions, randomly chosen.  The chosen function is invoked
;; in the *zone* buffer, which contains the text of the selected
;; window.  If the function loops, it *must* periodically check and
;; halt if `input-pending-p' is t (because quitting is disabled when
;; Emacs idle timers are run).
(defvar zone-programs [
                       zone-pgm-jitter
                       zone-pgm-putz-with-case
                       zone-pgm-dissolve
                       ;; zone-pgm-explode
                       zone-pgm-whack-chars
                       zone-pgm-rotate
                       zone-pgm-rotate-LR-lockstep
                       zone-pgm-rotate-RL-lockstep
                       zone-pgm-rotate-LR-variable
                       zone-pgm-rotate-RL-variable
                       zone-pgm-drip
                       zone-pgm-drip-fretfully
                       zone-pgm-five-oclock-swan-dive
                       zone-pgm-martini-swan-dive
                       zone-pgm-rat-race
                       zone-pgm-paragraph-spaz
                       zone-pgm-stress
                       zone-pgm-stress-destress
                       zone-pgm-random-life
                       ])

(defmacro zone-orig (&rest body)
  `(with-current-buffer (get 'zone 'orig-buffer)
     ,@body))

(defmacro zone-hiding-modeline (&rest body)
  ;; This formerly worked by temporarily altering face `mode-line',
  ;; which did not even work right, it seems.
  `(let (mode-line-format)
     ,@body))

(defun zone-call (program &optional timeout)
  "Call PROGRAM in a zoned way.
If PROGRAM is a function, call it, interrupting after the amount
 of time in seconds specified by optional arg TIMEOUT, or `zone-timeout'
 if unspecified, q.v.
PROGRAM can also be a list of elements, which are interpreted like so:
If the element is a function or a list of a function and a number,
 apply `zone-call' recursively."
  (cond ((functionp program)
         (with-timeout ((or timeout zone-timeout (ash 1 26)))
           (funcall program)))
        ((listp program)
         (mapcar (lambda (elem)
                   (cond ((functionp elem) (zone-call elem))
                         ((and (listp elem)
                               (functionp (car elem))
                               (numberp (cadr elem)))
                          (apply 'zone-call elem))
                         (t (error "bad `zone-call' elem: %S" elem))))
                 program))))

;;;###autoload
(defun zone ()
  "Zone out, completely."
  (interactive)
  (save-window-excursion
    (let ((f (selected-frame))
          (outbuf (get-buffer-create "*zone*"))
          (text (buffer-substring (window-start) (window-end)))
          (wp (1+ (- (window-point (selected-window))
                     (window-start)))))
      (put 'zone 'orig-buffer (current-buffer))
      (put 'zone 'modeline-hidden-level 0)
      (switch-to-buffer outbuf)
      (setq mode-name "Zone")
      (erase-buffer)
      (setq buffer-undo-list t
            truncate-lines t
            tab-width (zone-orig tab-width)
            line-spacing (zone-orig line-spacing))
      (insert text)
      (untabify (point-min) (point-max))
      (set-window-start (selected-window) (point-min))
      (set-window-point (selected-window) wp)
      (sit-for 0 500)
      (let ((pgm (elt zone-programs (random (length zone-programs))))
            (ct (and f (frame-parameter f 'cursor-type)))
            (show-trailing-whitespace nil)
            (restore (list '(kill-buffer outbuf))))
        (when ct
          (modify-frame-parameters f '((cursor-type . (bar . 0))))
          (setq restore (cons '(modify-frame-parameters
                                f (list (cons 'cursor-type ct)))
                              restore)))
        ;; Make `restore' a self-disabling one-shot thunk.
        (setq restore `(lambda () ,@restore (setq restore nil)))
        (condition-case nil
            (progn
              (message "Zoning... (%s)" pgm)
              (garbage-collect)
              ;; If some input is pending, zone says "sorry", which
              ;; isn't nice; this might happen e.g. when they invoke the
              ;; game by clicking the menu bar.  So discard any pending
              ;; input before zoning out.
              (if (input-pending-p)
                  (discard-input))
              (zone-call pgm)
              (message "Zoning...sorry"))
          (error
           (funcall restore)
           (while (not (input-pending-p))
             (message "We were zoning when we wrote %s..." pgm)
             (sit-for 3)
             (message "...here's hoping we didn't hose your buffer!")
             (sit-for 3)))
          (quit
           (funcall restore)
           (ding)
           (message "Zoning...sorry")))
        (when restore (funcall restore))))))

;;;; Zone when idle, or not.

(defun zone-when-idle (secs)
  "Zone out when Emacs has been idle for SECS seconds."
  (interactive "nHow long before I start zoning (seconds): ")
  (if (timerp zone-timer)
      (cancel-timer zone-timer))
  (setq zone-timer nil)
  (or (<= secs 0)
      (setq zone-timer (run-with-idle-timer secs t 'zone))))

(defun zone-leave-me-alone ()
  "Don't zone out when Emacs is idle."
  (interactive)
  (if (timerp zone-timer)
      (cancel-timer zone-timer))
  (setq zone-timer nil)
  (message "I won't zone out any more"))


;;;; jittering

(defun zone-shift-up ()
  (let* ((b (point))
         (e (progn (forward-line 1) (point)))
         (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-max))
    (insert s)))

(defun zone-shift-down ()
  (goto-char (point-max))
  (let* ((b (point))
         (e (progn (forward-line -1) (point)))
         (s (buffer-substring b e)))
    (delete-region b e)
    (goto-char (point-min))
    (insert s)))

(defun zone-shift-left ()
  (let ((inhibit-point-motion-hooks t)
        s)
    (while (not (eobp))
      (unless (eolp)
        (setq s (buffer-substring (point) (1+ (point))))
        (delete-char 1)
        (end-of-line)
        (insert s))
      (ignore-errors (forward-char 1)))))

(defun zone-shift-right ()
  (goto-char (point-max))
  (let ((inhibit-point-motion-hooks t)
        s)
    (while (not (bobp))
      (unless (bolp)
        (setq s (buffer-substring (1- (point)) (point)))
        (delete-char -1)
        (beginning-of-line)
        (insert s))
      (end-of-line 0))))

(defun zone-pgm-jitter ()
  (let ((ops [
              zone-shift-left
              zone-shift-right
              zone-shift-down
              zone-shift-up
              ]))
    (goto-char (point-min))
    (while (not (input-pending-p))
      (funcall (elt ops (random (length ops))))
      (goto-char (point-min))
      (sit-for 0 10))))


;;;; whacking chars

(defun zone-pgm-whack-chars ()
  (let ((tbl (copy-sequence (get 'zone-pgm-whack-chars 'wc-tbl))))
    (while (not (input-pending-p))
      (let ((i 48))
        (while (< i 122)
          (aset tbl i (+ 48 (random (- 123 48))))
          (setq i (1+ i)))
        (translate-region (point-min) (point-max) tbl)
        (sit-for 0 2)))))

(put 'zone-pgm-whack-chars 'wc-tbl
     (let ((tbl (make-string 128 ?x))
           (i 0))
       (while (< i 128)
         (aset tbl i i)
         (setq i (1+ i)))
       tbl))

;;;; dissolving

(defun zone-remove-text ()
  (let ((working t))
    (while working
      (setq working nil)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "[^(){}\n\t ]")
              (let ((n (random 5)))
                (if (not (= n 0))
                    (progn
                      (setq working t)
                      (forward-char 1))
                  (delete-char 1)
                  (insert " ")))
            (forward-char 1))))
      (sit-for 0 2))))

(defun zone-pgm-dissolve ()
  (zone-remove-text)
  (zone-pgm-jitter))


;;;; exploding

(defun zone-exploding-remove ()
  (let ((i 0))
    (while (< i 5)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "[^*\n\t ]")
              (let ((n (random 5)))
                (if (not (= n 0))
                    (forward-char 1))
                (insert " ")))
          (forward-char 1)))
      (setq i (1+ i))
      (sit-for 0 2)))
  (zone-pgm-jitter))

(defun zone-pgm-explode ()
  (zone-exploding-remove)
  (zone-pgm-jitter))


;;;; putzing w/ case

;; Faster than `zone-pgm-putz-with-case', but not as good: all
;; instances of the same letter have the same case, which produces a
;; less interesting effect than you might imagine.
(defun zone-pgm-2nd-putz-with-case ()
  (let ((tbl (make-string 128 ?x))
        (i 0))
    (while (< i 128)
      (aset tbl i i)
      (setq i (1+ i)))
    (while (not (input-pending-p))
      (setq i ?a)
      (while (<= i ?z)
        (aset tbl i
              (if (zerop (random 5))
                  (upcase i)
                (downcase i)))
        (setq i (+ i (1+ (random 5)))))
      (setq i ?A)
      (while (<= i ?z)
        (aset tbl i
              (if (zerop (random 5))
                  (downcase i)
                (upcase i)))
        (setq i (+ i (1+ (random 5)))))
      (translate-region (point-min) (point-max) tbl)
      (sit-for 0 2))))

(defun zone-pgm-putz-with-case ()
  (goto-char (point-min))
  (while (not (input-pending-p))
    (let ((np (+ 2 (random 5)))
          (pm (point-max)))
      (while (< np pm)
        (funcall (if (zerop (random 2)) 'upcase-region
                   'downcase-region) (1- np) np)
        (setq np (+ np (1+ (random 5))))))
    (goto-char (point-min))
    (sit-for 0 2)))


;;;; rotating

(defun zone-line-specs ()
  (let ((ok t)
        ret)
    (save-excursion
      (goto-char (window-start))
      (while (and ok (< (point) (window-end)))
        (when (looking-at "[\t ]*\\([^\n]+\\)")
          (setq ret (cons (cons (match-beginning 1) (match-end 1)) ret)))
        (setq ok (zerop (forward-line 1)))))
    ret))

(defun zone-pgm-rotate (&optional random-style)
  (let* ((specs (apply
                 'vector
                 (let (res)
                   (mapc (lambda (ent)
			   (let* ((beg (car ent))
				  (end (cdr ent))
				  (amt (if random-style
					   (funcall random-style)
					 (- (random 7) 3))))
			     (when (< (- end (abs amt)) beg)
			       (setq amt (random (- end beg))))
			     (unless (= 0 amt)
			       (setq res
				     (cons
				      (vector amt beg (- end (abs amt)))
				      res)))))
			 (zone-line-specs))
                   res)))
         (n (length specs))
         amt aamt cut paste txt i ent)
    (while (not (input-pending-p))
      (setq i 0)
      (while (< i n)
        (setq ent (aref specs i))
        (setq amt (aref ent 0) aamt (abs amt))
        (if (> 0 amt)
            (setq cut 1 paste 2)
          (setq cut 2 paste 1))
        (goto-char (aref ent cut))
        (setq aamt (min aamt (- (point-max) (point))))
        (setq txt (buffer-substring (point) (+ (point) aamt)))
        (delete-char aamt)
        (goto-char (aref ent paste))
        (insert txt)
        (setq i (1+ i)))
      (sit-for 0.04))))

(defun zone-pgm-rotate-LR-lockstep ()
  (zone-pgm-rotate (lambda () 1)))

(defun zone-pgm-rotate-RL-lockstep ()
  (zone-pgm-rotate (lambda () -1)))

(defun zone-pgm-rotate-LR-variable ()
  (zone-pgm-rotate (lambda () (1+ (random 3)))))

(defun zone-pgm-rotate-RL-variable ()
  (zone-pgm-rotate (lambda () (1- (- (random 3))))))


;;;; dripping

(defsubst zone-cpos (pos)
  (buffer-substring pos (1+ pos)))

(defsubst zone-replace-char (count del-count char-as-string new-value)
  (delete-char (or del-count (- count)))
  (aset char-as-string 0 new-value)
  (dotimes (i count) (insert char-as-string)))

(defsubst zone-park/sit-for (pos seconds)
  (let ((p (point)))
    (goto-char pos)
    (prog1 (sit-for seconds)
      (goto-char p))))

(defun zone-fret (wbeg pos)
  (let* ((case-fold-search nil)
         (c-string (zone-cpos pos))
         (cw-ceil (ceiling (char-width (aref c-string 0))))
         (hmm (cond
               ((string-match "[a-z]" c-string) (upcase c-string))
               ((string-match "[A-Z]" c-string) (downcase c-string))
               (t (propertize " " 'display `(space :width ,cw-ceil)))))
         (wait 0.5))
    (dotimes (i 20)
      (goto-char pos)
      (delete-char 1)
      (insert (if (= 0 (% i 2)) hmm c-string))
      (zone-park/sit-for wbeg (setq wait (* wait 0.8))))
    (delete-char -1) (insert c-string)))

(defun zone-fill-out-screen (width height)
  (let ((start (window-start))
	(line (make-string width 32))
	(inhibit-point-motion-hooks t))
    (goto-char start)
    ;; fill out rectangular ws block
    (while (progn (end-of-line)
		  (let ((cc (current-column)))
		    (if (< cc width)
			(insert (substring line cc))
		      (delete-char (- width cc)))
		    (cond ((eobp) (insert "\n") nil)
			  (t (forward-char 1) t)))))
    ;; pad ws past bottom of screen
    (let ((nl (- height (count-lines (point-min) (point)))))
      (when (> nl 0)
	(setq line (concat line "\n"))
        (dotimes (i nl)
	  (insert line))))
    (goto-char start)
    (recenter 0)
    (sit-for 0)))

(defun zone-fall-through-ws (c wbeg wend)
  (let* ((cw-ceil (ceiling (char-width (aref c 0))))
         (spaces (make-string cw-ceil 32))
         (col (current-column))
         (wait 0.15)
         newpos fall-p)
    (while (when (save-excursion
                   (and (zerop (forward-line 1))
                        (progn
                          (forward-char col)
                          (= col (current-column)))
                        (setq newpos (point))
                        (string= spaces (buffer-substring-no-properties
                                         newpos (+ newpos cw-ceil)))
                        (setq newpos (+ newpos (1- cw-ceil)))))
	     (setq fall-p t)
	     (delete-char 1)
	     (insert spaces)
             (goto-char newpos)
	     (when (< (point) wend)
	       (delete-char cw-ceil)
	       (insert c)
	       (forward-char -1)
	       (zone-park/sit-for wbeg (setq wait (* wait 0.8))))))
    fall-p))

(defun zone-pgm-drip (&optional fret-p pancake-p)
  (let* ((ww (1- (window-width)))
         (wh (window-height))
         (mc 0)                         ; miss count
         (total (* ww wh))
         (fall-p nil)
         wbeg wend c)
    (zone-fill-out-screen ww wh)
    (setq wbeg (window-start)
          wend (window-end))
    (catch 'done
      (while (not (input-pending-p))
        (setq mc 0 wend (window-end))
        ;; select non-ws character, but don't miss too much
        (goto-char (+ wbeg (random (- wend wbeg))))
        (while (looking-at "[ \n\f]")
          (if (= total (setq mc (1+ mc)))
              (throw 'done 'sel)
            (goto-char (+ wbeg (random (- wend wbeg))))))
        ;; character animation sequence
        (let ((p (point)))
          (when fret-p (zone-fret wbeg p))
          (goto-char p)
          (setq c (zone-cpos p)
                fall-p (zone-fall-through-ws c wbeg wend)))
        ;; assuming current-column has not changed...
        (when (and pancake-p
                   fall-p
                   (< (count-lines (point-min) (point))
                      wh))
          (let ((cw (ceiling (char-width (aref c 0)))))
            (zone-replace-char cw   1 c ?@) (zone-park/sit-for wbeg 0.137)
            (zone-replace-char cw nil c ?*) (zone-park/sit-for wbeg 0.137)
            (zone-replace-char cw nil c ?_)))))))

(defun zone-pgm-drip-fretfully ()
  (zone-pgm-drip t))

(defun zone-pgm-five-oclock-swan-dive ()
  (zone-pgm-drip nil t))

(defun zone-pgm-martini-swan-dive ()
  (zone-pgm-drip t t))

(defun zone-pgm-rat-race ()
  (while (not (input-pending-p))
    (zone-call '((zone-pgm-rotate 10)
                 (zone-pgm-drip-fretfully 15)
                 (zone-pgm-drip 10)
                 ((lambda ()
                    (goto-char (point-min))
                    (while (re-search-forward " +$" nil t)
                      (delete-region (match-beginning 0) (match-end 0))))
                  5)))))


;;;; paragraph spazzing (for textish modes)

(defun zone-pgm-paragraph-spaz ()
  (if (memq (zone-orig major-mode)
            ;; there should be a better way to distinguish textish modes
            '(text-mode texinfo-mode fundamental-mode))
      (let ((fill-column fill-column)
            (fc-min fill-column)
            (fc-max fill-column)
            (max-fc (1- (frame-width))))
        (while (sit-for 0.1)
          (fill-paragraph 1)
          (setq fill-column (+ fill-column (- (random 5) 2)))
          (when (< fill-column fc-min)
            (setq fc-min fill-column))
          (when (> fill-column max-fc)
            (setq fill-column max-fc))
          (when (> fill-column fc-max)
            (setq fc-max fill-column))))
    (message "Zoning... (zone-pgm-rotate)")
    (zone-pgm-rotate)))


;;;; stressing and destressing

(defun zone-pgm-stress ()
  (goto-char (point-min))
  (let ((ok t)
        lines)
    (while (and ok (< (point) (point-max)))
      (let ((p (point)))
        (setq ok (zerop (forward-line 1))
              lines (cons (buffer-substring p (point)) lines))))
    (sit-for 5)
    (zone-hiding-modeline
     (let ((msg "Zoning... (zone-pgm-stress)"))
       (while (not (string= msg ""))
         (message (setq msg (substring msg 1)))
         (sit-for 0.05)))
     (while (not (input-pending-p))
       (when (< 50 (random 100))
         (goto-char (point-max))
         (forward-line -1)
         (let ((kill-whole-line t))
           (kill-line))
         (goto-char (point-min))
         (insert (nth (random (length lines)) lines)))
       (message (concat (make-string (random (- (frame-width) 5)) ? ) "grrr"))
       (sit-for 0.1)))))

(defun zone-pgm-stress-destress ()
  (zone-call 'zone-pgm-stress 25)
  (zone-hiding-modeline
   (sit-for 3)
   (erase-buffer)
   (sit-for 3)
   (insert-buffer-substring "*Messages*")
   (message "")
   (goto-char (point-max))
   (recenter -1)
   (sit-for 3)
   (delete-region (point-min) (window-start))
   (message "hey why stress out anyway?")
   (zone-call '((zone-pgm-rotate         30)
                (zone-pgm-whack-chars    10)
                zone-pgm-drip))))


;;;; the lyfe so short the craft so long to lerne --chaucer

(defvar zone-pgm-random-life-wait nil
  "*Seconds to wait between successive `life' generations.
If nil, `zone-pgm-random-life' chooses a value from 0-3 (inclusive).")

(defvar life-patterns) ; from life.el

(defun zone-pgm-random-life ()
  (require 'life)
  (zone-fill-out-screen (1- (window-width)) (1- (window-height)))
  (let ((top (progn (goto-char (window-start)) (forward-line 7) (point)))
        (bot (progn (goto-char (window-end)) (forward-line -7) (point)))
        (rtc (- (frame-width) 11))
        (min (window-start))
        (max (1- (window-end)))
        s c col)
    (delete-region max (point-max))
    (while (and (progn (goto-char min) (sit-for 0.05))
                (progn (goto-char (+ min (random max)))
                       (or (progn (skip-chars-forward " @\n" max)
                                  (not (= max (point))))
                           (unless (or (= 0 (skip-chars-backward " @\n" min))
                                       (= min (point)))
                             (forward-char -1)
                             t))))
      (unless (or (eolp) (eobp))
        (setq s (zone-cpos (point))
              c (aref s 0))
        (zone-replace-char
         (char-width c)
         1 s (cond ((or (> top (point))
                        (< bot (point))
                        (or (> 11 (setq col (current-column)))
                            (< rtc col)))
                    32)
                   ((and (<= ?a c) (>= ?z c)) (+ c (- ?A ?a)))
                   ((and (<= ?A c) (>= ?Z c)) ?*)
                   (t ?@)))))
    (sit-for 3)
    (setq col nil)
    (goto-char bot)
    (while (< top (point))
      (setq c (point))
      (move-to-column 9)
      (setq col (cons (buffer-substring (point) c) col))
;      (let ((inhibit-point-motion-hooks t))
        (end-of-line 0);)
      (forward-char -10))
    (let ((life-patterns (vector
                          (if (and col (search-forward "@" max t))
                              (cons (make-string (length (car col)) 32) col)
                            (list (mapconcat 'identity
                                             (make-list (/ (- rtc 11) 15)
                                                        (make-string 5 ?@))
                                             (make-string 10 32)))))))
      (life (or zone-pgm-random-life-wait (random 4)))
      (kill-buffer nil))))


(random t)

;;;;;;;;;;;;;;;
(provide 'zone)

;;; zone.el ends here
