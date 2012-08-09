;;; doctor.el --- psychological help for frustrated users

;; Copyright (C) 1985, 1987, 1994, 1996, 2000-2012
;;   Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: games

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

;; The single entry point `doctor', simulates a Rogerian analyst using
;; phrase-production techniques similar to the classic ELIZA demonstration
;; of pseudo-AI.

;;; Code:

(defvar doctor--**mad**)
(defvar doctor--*print-space*)
(defvar doctor--*print-upcase*)
(defvar doctor--abuselst)
(defvar doctor--abusewords)
(defvar doctor--afraidof)
(defvar doctor--arerelated)
(defvar doctor--areyou)
(defvar doctor--bak)
(defvar doctor--beclst)
(defvar doctor--bother)
(defvar doctor--bye)
(defvar doctor--canyou)			; unused?
(defvar doctor--chatlst)
(defvar doctor--continue)
(defvar doctor--deathlst)
(defvar doctor--describe)
(defvar doctor--drnk)
(defvar doctor--drugs)
(defvar doctor--eliza-flag)
(defvar doctor--elizalst)
(defvar doctor--famlst)
(defvar doctor--feared)
(defvar doctor--fears)
(defvar doctor--feelings-about)
(defvar doctor--foullst)
(defvar doctor-found)
(defvar doctor--hello)
(defvar doctor--history)
(defvar doctor--howareyoulst)
(defvar doctor--howdyflag)
(defvar doctor--huhlst)
(defvar doctor--ibelieve)
(defvar doctor--improve)
(defvar doctor--inter)
(defvar doctor--isee)
(defvar doctor--isrelated)
(defvar doctor--lincount)
(defvar doctor--longhuhlst)
(defvar doctor--lover)
(defvar doctor--machlst)
(defvar doctor--mathlst)
(defvar doctor--maybe)
(defvar doctor--moods)
(defvar doctor--neglst)
(defvar doctor-obj)
(defvar doctor-object)
(defvar doctor-owner)
(defvar doctor--please)
(defvar doctor--problems)
(defvar doctor--qlist)
(defvar doctor--random-adjective)
(defvar doctor--relation)
(defvar doctor--remlst)
(defvar doctor--repetitive-shortness)
(defvar doctor--replist)
(defvar doctor--rms-flag)
(defvar doctor--schoollst)
(defvar doctor-sent)
(defvar doctor--sexlst)
(defvar doctor--shortbeclst)
(defvar doctor--shortlst)
(defvar doctor--something)
(defvar doctor--sportslst)
(defvar doctor--stallmanlst)
(defvar doctor--states)
(defvar doctor-subj)
(defvar doctor--suicide-flag)
(defvar doctor--sure)
(defvar doctor--thing)
(defvar doctor--things)
(defvar doctor--thlst)
(defvar doctor--toklst)
(defvar doctor--typos)
(defvar doctor-verb)
(defvar doctor--want)
(defvar doctor--whatwhen)
(defvar doctor--whereoutp)
(defvar doctor--whysay)
(defvar doctor--whywant)
(defvar doctor--zippy-flag)
(defvar doctor--zippylst)

(defun doc// (x) x)

(defmacro doc$ (what)
  "Quoted arg form of doctor-$."
  `(doctor-$ ',what))

(defun doctor-$ (what)
  "Return the car of a list, rotating the list each time."
  (let* ((vv (symbol-value what))
	(first (car vv))
	(ww (append (cdr vv) (list first))))
    (set what ww)
    first))

(defvar doctor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\n" 'doctor-read-print)
    (define-key map "\r" 'doctor-ret-or-read)
    map))

(define-derived-mode doctor-mode text-mode "Doctor"
  "Major mode for running the Doctor (Eliza) program.
Like Text mode with Auto Fill mode
except that RET when point is after a newline, or LFD at any time,
reads the sentence before point, and prints the Doctor's answer."
  (make-doctor-variables)
  (turn-on-auto-fill)
  (doctor-type '(i am the psychotherapist \.
		 (doc$ doctor--please) (doc$ doctor--describe) your (doc$ doctor--problems) \.
		 each time you are finished talking\, type \R\E\T twice \.))
  (insert "\n"))

(defun make-doctor-variables ()
  (set (make-local-variable 'doctor--typos)
       (mapcar (lambda (x)
		 (put (car x) 'doctor-correction  (cadr x))
		 (put (cadr x) 'doctor-expansion (car (cddr x)))
		 (car x))
	       '((theyll they\'ll (they will))
		 (theyre they\'re (they are))
		 (hes he\'s (he is))
		 (he7s he\'s (he is))
		 (im i\'m (you are))
		 (i7m i\'m (you are))
		 (isa is\ a (is a))
		 (thier their (their))
		 (dont don\'t (do not))
		 (don7t don\'t (do not))
		 (you7re you\'re (i am))
		 (you7ve you\'ve (i have))
		 (you7ll you\'ll (i will)))))
  (set (make-local-variable 'doctor-sent) nil)
  (set (make-local-variable 'doctor-found) nil)
  (set (make-local-variable 'doctor-owner) nil)
  (set (make-local-variable 'doctor--history) nil)
  (set (make-local-variable 'doctor--inter) '((well\,)
				      (hmmm \.\.\.\ so\,)
				      (so)
				      (\.\.\.and)
				      (then)))
  (set (make-local-variable 'doctor--continue) '((continue)
						 (proceed)
						 (go on)
						 (keep going)))
  (set (make-local-variable 'doctor--relation)
       '((your relationship with)
	 (something you remember about)
	 (your feelings toward)
	 (some experiences you have had with)
	 (how you feel about)))
  (set (make-local-variable 'doctor--fears)
       '(((doc$ doctor--whysay) you are (doc$ doctor--afraidof) (doc// doctor--feared) \?)
	 (you seem terrified by (doc// doctor--feared) \.)
	 (when did you first feel (doc$ doctor--afraidof) (doc// doctor--feared) \?)))
  (set (make-local-variable 'doctor--sure) '((sure)
					     (positive)
					     (certain)
					     (absolutely sure)))
  (set (make-local-variable 'doctor--afraidof) '((afraid of)
					 (frightened by)
					 (scared of)))
  (set (make-local-variable 'doctor--areyou) '((are you)
				       (have you been)
				       (have you been)))
  (set (make-local-variable 'doctor--isrelated)
       '((has something to do with)
	 (is related to)
	 (could be the reason for)
	 (is caused by)
	 (is because of)))
  (set (make-local-variable 'doctor--arerelated) '((have something to do with)
					   (are related to)
					   (could have caused)
					   (could be the reason for)
					   (are caused by)
					   (are because of)))
  (set (make-local-variable 'doctor--moods)
       '(((doc$ doctor--areyou) (doc// doctor-found) often \?)
	 (what causes you to be (doc// doctor-found) \?)
	 ((doc$ doctor--whysay) you are (doc// doctor-found) \?)))
  (set (make-local-variable 'doctor--maybe) '((maybe)
				      (perhaps)
				      (possibly)))
  (set (make-local-variable 'doctor--whatwhen) '((what happened when)
						 (what would happen if)))
  (set (make-local-variable 'doctor--hello) '((how do you do \?)
				      (hello \.)
				      (howdy!)
				      (hello \.)
				      (hi \.)
				      (hi there \.)))
  (set (make-local-variable 'doctor--drnk)
       '((do you drink a lot of (doc// doctor-found) \?)
	 (do you get drunk often \?)
	 ((doc$ doctor--describe) your drinking habits \.)))
  (set (make-local-variable 'doctor--drugs)
       '((do you use (doc// doctor-found) often \?)
	 ((doc$ doctor--areyou) addicted to (doc// doctor-found) \?)
	 (do you realize that drugs can be very harmful \?)
	 ((doc$ doctor--maybe) you should try to quit using (doc// doctor-found) \.)))
  (set (make-local-variable 'doctor--whywant)
       '(((doc$ doctor--whysay) (doc// doctor-subj) might (doc$ doctor--want) (doc// doctor-obj) \?)
	 (how does it feel to want \?)
	 (why should (doc// doctor-subj) get (doc// doctor-obj) \?)
	 (when did (doc// doctor-subj) first (doc$ doctor--want) (doc// doctor-obj) \?)
	 ((doc$ doctor--areyou) obsessed with (doc// doctor-obj) \?)
	 (why should i give (doc// doctor-obj) to (doc// doctor-subj) \?)
	 (have you ever gotten (doc// doctor-obj) \?)))
  (set (make-local-variable 'doctor--canyou)
       '((of course i can \.)
	 (why should i \?)
	 (what makes you think i would even want to \?)
	 (i am the doctor\, i can do anything i damn please \.)
	 (not really\, it\'s not up to me \.)
	 (depends\, how important is it \?)
	 (i could\, but i don\'t think it would be a wise thing to do \.)
	 (can you \?)
	 (maybe i can\, maybe i can\'t \.\.\.)
	 (i don\'t think i should do that \.)))
  (set (make-local-variable 'doctor--want) '((want) (desire) (wish) (want) (hope)))
  (set (make-local-variable 'doctor--shortlst)
       '((can you elaborate on that \?)
	 ((doc$ doctor--please) continue \.)
	 (go on\, don\'t be afraid \.)
	 (i need a little more detail please \.)
	 (you\'re being a bit brief\, (doc$ doctor--please) go into detail \.)
	 (can you be more explicit \?)
	 (and \?)
	 ((doc$ doctor--please) go into more detail \?)
	 (you aren\'t being very talkative today\!)
	 (is that all there is to it \?)
	 (why must you respond so briefly \?)))
  (set (make-local-variable 'doctor--famlst)
       '((tell me (doc$ doctor--something) about (doc// doctor-owner) family \.)
	 (you seem to dwell on (doc// doctor-owner) family \.)
	 ((doc$ doctor--areyou) hung up on (doc// doctor-owner) family \?)))
  (set (make-local-variable 'doctor--huhlst)
       '(((doc$ doctor--whysay) (doc// doctor-sent) \?)
	 (is it because of (doc$ doctor--things) that you say (doc// doctor-sent) \?)))
  (set (make-local-variable 'doctor--longhuhlst)
       '(((doc$ doctor--whysay) that \?)
	 (i don\'t understand \.)
	 ((doc$ doctor--thlst))
	 ((doc$ doctor--areyou) (doc$ doctor--afraidof) that \?)))
  (set (make-local-variable 'doctor--feelings-about) '((feelings about)
					       (apprehensions toward)
					       (thoughts on)
					       (emotions toward)))
  (set (make-local-variable 'doctor--random-adjective)
       '((vivid)
	 (emotionally stimulating)
	 (exciting)
	 (boring)
	 (interesting)
	 (recent)
	 (random) ; how can we omit this?
	 (unusual)
	 (shocking)
	 (embarrassing)))
  (set (make-local-variable 'doctor--whysay) '((why do you say)
				       (what makes you believe)
				       (are you sure that)
				       (do you really think)
				       (what makes you think)))
  (set (make-local-variable 'doctor--isee) '((i see \.\.\.)
				     (yes\,)
				     (i understand \.)
				     (oh \.) ))
  (set (make-local-variable 'doctor--please) '((please\,)
				       (i would appreciate it if you would)
				       (perhaps you could)
				       (please\,)
				       (would you please)
				       (why don\'t you)
				       (could you)))
  (set (make-local-variable 'doctor--bye)
       '((my secretary will send you a bill \.)
	 (bye bye \.)
	 (see ya \.)
	 (ok\, talk to you some other time \.)
	 (talk to you later \.)
	 (ok\, have fun \.)
	 (ciao \.)))
  (set (make-local-variable 'doctor--something) '((something)
						  (more)
						  (how you feel)))
  (set (make-local-variable 'doctor--thing) '((your life)
				      (your sex life)))
  (set (make-local-variable 'doctor--things) '((your plans)
				       (the people you hang around with)
				       (problems at school)
				       (any hobbies you have)
				       (hangups you have)
				       (your inhibitions)
				       (some problems in your childhood)
				       (some problems at home)))
  (set (make-local-variable 'doctor--describe) '((describe)
					 (tell me about)
					 (talk about)
					 (discuss)
					 (tell me more about)
					 (elaborate on)))
  (set (make-local-variable 'doctor--ibelieve)
       '((i believe) (i think) (i have a feeling) (it seems to me that)
	 (it looks like)))
  (set (make-local-variable 'doctor--problems) '((problems)
					 (inhibitions)
					 (hangups)
					 (difficulties)
					 (anxieties)
					 (frustrations)))
  (set (make-local-variable 'doctor--bother) '((does it bother you that)
				       (are you annoyed that)
				       (did you ever regret)
				       (are you sorry)
				       (are you satisfied with the fact that)))
  (set (make-local-variable 'doctor--machlst)
       '((you have your mind on (doc// doctor-found) \, it seems \.)
	 (you think too much about  (doc// doctor-found) \.)
	 (you should try taking your mind off of (doc// doctor-found)\.)
	 (are you a computer hacker \?)))
  (set (make-local-variable 'doctor--qlist)
       '((what do you think \?)
	 (i\'ll ask the questions\, if you don\'t mind!)
	 (i could ask the same thing myself \.)
	 ((doc$ doctor--please) allow me to do the questioning \.)
	 (i have asked myself that question many times \.)
	 ((doc$ doctor--please) try to answer that question yourself \.)))
  (set (make-local-variable 'doctor--foullst)
       '(((doc$ doctor--please) watch your tongue!)
	 ((doc$ doctor--please) avoid such unwholesome thoughts \.)
	 ((doc$ doctor--please) get your mind out of the gutter \.)
	 (such lewdness is not appreciated \.)))
  (set (make-local-variable 'doctor--deathlst)
       '((this is not a healthy way of thinking \.)
	 ((doc$ doctor--bother) you\, too\, may die someday \?)
	 (i am worried by your obsession with this topic!)
	 (did you watch a lot of crime and violence on television as a child \?)))
  (set (make-local-variable 'doctor--sexlst)
       '(((doc$ doctor--areyou) (doc$ doctor--afraidof) sex \?)
	 ((doc$ doctor--describe) (doc$ doctor--something) about your sexual history \.)
	 ((doc$ doctor--please) (doc$ doctor--describe) your sex life \.\.\.)
	 ((doc$ doctor--describe) your (doc$ doctor--feelings-about) your sexual partner \.)
	 ((doc$ doctor--describe) your most (doc$ doctor--random-adjective) sexual experience \.)
	 ((doc$ doctor--areyou) satisfied with (doc// doctor--lover) \.\.\. \?)))
  (set (make-local-variable 'doctor--neglst) '((why not \?)
				       ((doc$ doctor--bother) i ask that \?)
				       (why not \?)
				       (why not \?)
				       (how come \?)
				       ((doc$ doctor--bother) i ask that \?)))
  (set (make-local-variable 'doctor--beclst)
       '((is it because (doc// doctor-sent) that you came to me \?)
	 ((doc$ doctor--bother) (doc// doctor-sent) \?)
	 (when did you first know that (doc// doctor-sent) \?)
	 (is the fact that (doc// doctor-sent) the real reason \?)
	 (does the fact that (doc// doctor-sent) explain anything else \?)
	 ((doc$ doctor--areyou) (doc$ doctor--sure) (doc// doctor-sent) \? )))
  (set (make-local-variable 'doctor--shortbeclst)
   '(((doc$ doctor--bother) i ask you that \?)
     (that\'s not much of an answer!)
     ((doc$ doctor--inter) why won\'t you talk about it \?)
     (speak up!)
     ((doc$ doctor--areyou) (doc$ doctor--afraidof) talking about it \?)
     (don\'t be (doc$ doctor--afraidof) elaborating \.)
     ((doc$ doctor--please) go into more detail \.)))
  (set (make-local-variable 'doctor--thlst)
       '(((doc$ doctor--maybe) (doc$ doctor--thing) (doc$ doctor--isrelated) this \.)
	 ((doc$ doctor--maybe) (doc$ doctor--things) (doc$ doctor--arerelated) this \.)
	 (is it because of (doc$ doctor--things) that you are going through all this \?)
	 (how do you reconcile (doc$ doctor--things) \? )
	 ((doc$ doctor--maybe) this (doc$ doctor--isrelated) (doc$ doctor--things) \?)))
  (set (make-local-variable 'doctor--remlst)
       '((earlier you said (doc$ doctor--history) \?)
	 (you mentioned that (doc$ doctor--history) \?)
	 ((doc$ doctor--whysay) (doc$ doctor--history) \? )))
  (set (make-local-variable 'doctor--toklst)
       '((is this how you relax \?)
	 (how long have you been smoking	grass \?)
	 ((doc$ doctor--areyou) (doc$ doctor--afraidof) of being drawn to using harder stuff \?)))
  (set (make-local-variable 'doctor--states)
       '((do you get (doc// doctor-found) often \?)
	 (do you enjoy being (doc// doctor-found) \?)
	 (what makes you (doc// doctor-found) \?)
	 (how often (doc$ doctor--areyou) (doc// doctor-found) \?)
	 (when were you last (doc// doctor-found) \?)))
  (set (make-local-variable 'doctor--replist) '((i . (you))
					(my . (your))
					(me . (you))
					(you . (me))
					(your . (my))
					(mine . (yours))
					(yours . (mine))
					(our . (your))
					(ours . (yours))
					(we . (you))
					(dunno . (do not know))
					;;	  (yes . ())
					(no\, . ())
					(yes\, . ())
					(ya . (i))
					(aint . (am not))
					(wanna . (want to))
					(gimme . (give me))
					(gotta . (have to))
					(gonna . (going to))
					(never . (not ever))
					(doesn\'t . (does not))
					(don\'t . (do not))
					(aren\'t . (are not))
					(isn\'t . (is not))
					(won\'t . (will not))
					(can\'t . (cannot))
					(haven\'t . (have not))
					(i\'m . (you are))
					(ourselves . (yourselves))
					(myself . (yourself))
					(yourself . (myself))
					(you\'re . (i am))
					(you\'ve . (i have))
					(i\'ve . (you have))
					(i\'ll . (you will))
					(you\'ll . (i shall))
					(i\'d . (you would))
					(you\'d . (i would))
					(here . (there))
					(please . ())
					(eh\, . ())
					(eh . ())
					(oh\, . ())
					(oh . ())
					(shouldn\'t . (should not))
					(wouldn\'t . (would not))
					(won\'t . (will not))
					(hasn\'t . (has not))))
  (set (make-local-variable 'doctor--stallmanlst)
       '(((doc$ doctor--describe) your (doc$ doctor--feelings-about) him \.)
	 ((doc$ doctor--areyou) a friend of Stallman \?)
	 ((doc$ doctor--bother) Stallman is (doc$ doctor--random-adjective) \?)
	 ((doc$ doctor--ibelieve) you are (doc$ doctor--afraidof) him \.)))
  (set (make-local-variable 'doctor--schoollst)
       '(((doc$ doctor--describe) your (doc// doctor-found) \.)
	 ((doc$ doctor--bother) your grades could (doc$ doctor--improve) \?)
	 ((doc$ doctor--areyou) (doc$ doctor--afraidof) (doc// doctor-found) \?)
	 ((doc$ doctor--maybe) this (doc$ doctor--isrelated) to your attitude \.)
	 ((doc$ doctor--areyou) absent often \?)
	 ((doc$ doctor--maybe) you should study (doc$ doctor--something) \.)))
  (set (make-local-variable 'doctor--improve)
       '((improve) (be better) (be improved) (be higher)))
  (set (make-local-variable 'doctor--elizalst)
       '(((doc$ doctor--areyou) (doc$ doctor--sure) \?)
	 ((doc$ doctor--ibelieve) you have (doc$ doctor--problems) with (doc// doctor-found) \.)
	 ((doc$ doctor--whysay) (doc// doctor-sent) \?)))
  (set (make-local-variable 'doctor--sportslst)
       '((tell me (doc$ doctor--something) about (doc// doctor-found) \.)
	 ((doc$ doctor--describe) (doc$ doctor--relation) (doc// doctor-found) \.)
	 (do you find (doc// doctor-found) (doc$ doctor--random-adjective) \?)))
  (set (make-local-variable 'doctor--mathlst)
       '(((doc$ doctor--describe) (doc$ doctor--something) about math \.)
	 ((doc$ doctor--maybe) your (doc$ doctor--problems) (doc$ doctor--arerelated) (doc// doctor-found) \.)
	 (i don\'t know much (doc// doctor-found) \, but (doc$ doctor--continue)
	    anyway \.)))
  (set (make-local-variable 'doctor--zippylst)
       '(((doc$ doctor--areyou) Zippy \?)
	 ((doc$ doctor--ibelieve) you have some serious (doc$ doctor--problems) \.)
	 ((doc$ doctor--bother) you are a pinhead \?)))
  (set (make-local-variable 'doctor--chatlst)
       '(((doc$ doctor--maybe) we could chat \.)
	 ((doc$ doctor--please) (doc$ doctor--describe) (doc$ doctor--something) about chat mode \.)
	 ((doc$ doctor--bother) our discussion is so (doc$ doctor--random-adjective) \?)))
  (set (make-local-variable 'doctor--abuselst)
       '(((doc$ doctor--please) try to be less abusive \.)
	 ((doc$ doctor--describe) why you call me (doc// doctor-found) \.)
	 (i\'ve had enough of you!)))
  (set (make-local-variable 'doctor--abusewords)
       '(boring bozo clown clumsy cretin dumb dummy
		fool foolish gnerd gnurd idiot jerk
		lose loser louse lousy luse luser
		moron nerd nurd oaf oafish reek
		stink stupid tool toolish twit))
  (set (make-local-variable 'doctor--howareyoulst)
       '((how are you) (hows it going) (hows it going eh)
	 (how\'s it going) (how\'s it going eh) (how goes it)
	 (whats up) (whats new) (what\'s up) (what\'s new)
	 (howre you) (how\'re you) (how\'s everything)
	 (how is everything) (how do you do)
	 (how\'s it hanging) (que pasa)
	 (how are you doing) (what do you say)))
  (set (make-local-variable 'doctor--whereoutp) '(huh remem rthing))
  (set (make-local-variable 'doctor-subj) nil)
  (set (make-local-variable 'doctor-verb) nil)
  (set (make-local-variable 'doctor-obj) nil)
  (set (make-local-variable 'doctor--feared) nil)
  (set (make-local-variable 'doctor--repetitive-shortness) '(0 . 0))
  (set (make-local-variable 'doctor--**mad**) nil)
  (set (make-local-variable 'doctor--rms-flag) nil)
  (set (make-local-variable 'doctor--eliza-flag) nil)
  (set (make-local-variable 'doctor--zippy-flag) nil)
  (set (make-local-variable 'doctor--suicide-flag) nil)
  (set (make-local-variable 'doctor--lover) '(your partner))
  (set (make-local-variable 'doctor--bak) nil)
  (set (make-local-variable 'doctor--lincount) 0)
  (set (make-local-variable 'doctor--*print-upcase*) nil)
  (set (make-local-variable 'doctor--*print-space*) nil)
  (set (make-local-variable 'doctor--howdyflag) nil)
  (set (make-local-variable 'doctor-object) nil))

;; Define equivalence classes of words that get treated alike.

(defun doctor-meaning (x) (get x 'doctor-meaning))

(defmacro doctor-put-meaning (symb val)
  "Store the base meaning of a word on the property list."
  `(put ',symb 'doctor-meaning ,val))

(doctor-put-meaning howdy 'howdy)
(doctor-put-meaning hi 'howdy)
(doctor-put-meaning greetings 'howdy)
(doctor-put-meaning hello 'howdy)
(doctor-put-meaning tops20 'mach)
(doctor-put-meaning tops-20 'mach)
(doctor-put-meaning tops 'mach)
(doctor-put-meaning pdp11 'mach)
(doctor-put-meaning computer 'mach)
(doctor-put-meaning unix 'mach)
(doctor-put-meaning machine 'mach)
(doctor-put-meaning computers 'mach)
(doctor-put-meaning machines 'mach)
(doctor-put-meaning pdp11s 'mach)
(doctor-put-meaning foo 'mach)
(doctor-put-meaning foobar 'mach)
(doctor-put-meaning multics 'mach)
(doctor-put-meaning macsyma 'mach)
(doctor-put-meaning teletype 'mach)
(doctor-put-meaning la36 'mach)
(doctor-put-meaning vt52 'mach)
(doctor-put-meaning zork 'mach)
(doctor-put-meaning trek 'mach)
(doctor-put-meaning startrek 'mach)
(doctor-put-meaning advent 'mach)
(doctor-put-meaning pdp 'mach)
(doctor-put-meaning dec 'mach)
(doctor-put-meaning commodore 'mach)
(doctor-put-meaning vic 'mach)
(doctor-put-meaning bbs 'mach)
(doctor-put-meaning modem 'mach)
(doctor-put-meaning baud 'mach)
(doctor-put-meaning macintosh 'mach)
(doctor-put-meaning vax 'mach)
(doctor-put-meaning vms 'mach)
(doctor-put-meaning ibm 'mach)
(doctor-put-meaning pc 'mach)
(doctor-put-meaning bitching 'foul)
(doctor-put-meaning shit 'foul)
(doctor-put-meaning bastard 'foul)
(doctor-put-meaning damn 'foul)
(doctor-put-meaning damned 'foul)
(doctor-put-meaning hell 'foul)
(doctor-put-meaning suck 'foul)
(doctor-put-meaning sucking 'foul)
(doctor-put-meaning sux 'foul)
(doctor-put-meaning ass 'foul)
(doctor-put-meaning whore 'foul)
(doctor-put-meaning bitch 'foul)
(doctor-put-meaning asshole 'foul)
(doctor-put-meaning shrink 'foul)
(doctor-put-meaning pot 'toke)
(doctor-put-meaning grass 'toke)
(doctor-put-meaning weed 'toke)
(doctor-put-meaning marijuana 'toke)
(doctor-put-meaning acapulco 'toke)
(doctor-put-meaning columbian 'toke)
(doctor-put-meaning tokin 'toke)
(doctor-put-meaning joint 'toke)
(doctor-put-meaning toke 'toke)
(doctor-put-meaning toking 'toke)
(doctor-put-meaning tokin\' 'toke)
(doctor-put-meaning toked 'toke)
(doctor-put-meaning roach 'toke)
(doctor-put-meaning pills 'drug)
(doctor-put-meaning dope 'drug)
(doctor-put-meaning acid 'drug)
(doctor-put-meaning lsd 'drug)
(doctor-put-meaning speed 'drug)
(doctor-put-meaning heroin 'drug)
(doctor-put-meaning hash 'drug)
(doctor-put-meaning cocaine 'drug)
(doctor-put-meaning uppers 'drug)
(doctor-put-meaning downers 'drug)
(doctor-put-meaning loves 'loves)
(doctor-put-meaning love 'love)
(doctor-put-meaning loved 'love)
(doctor-put-meaning hates 'hates)
(doctor-put-meaning dislikes 'hates)
(doctor-put-meaning hate 'hate)
(doctor-put-meaning hated 'hate)
(doctor-put-meaning dislike 'hate)
(doctor-put-meaning stoned 'state)
(doctor-put-meaning drunk 'state)
(doctor-put-meaning drunken 'state)
(doctor-put-meaning high 'state)
(doctor-put-meaning horny 'state)
(doctor-put-meaning blasted 'state)
(doctor-put-meaning happy 'state)
(doctor-put-meaning paranoid 'state)
(doctor-put-meaning wish 'desire)
(doctor-put-meaning wishes 'desire)
(doctor-put-meaning want 'desire)
(doctor-put-meaning desire 'desire)
(doctor-put-meaning like 'desire)
(doctor-put-meaning hope 'desire)
(doctor-put-meaning hopes 'desire)
(doctor-put-meaning desires 'desire)
(doctor-put-meaning wants 'desire)
(doctor-put-meaning desires 'desire)
(doctor-put-meaning likes 'desire)
(doctor-put-meaning needs 'desire)
(doctor-put-meaning need 'desire)
(doctor-put-meaning frustrated 'mood)
(doctor-put-meaning depressed 'mood)
(doctor-put-meaning annoyed 'mood)
(doctor-put-meaning upset 'mood)
(doctor-put-meaning unhappy 'mood)
(doctor-put-meaning excited 'mood)
(doctor-put-meaning worried 'mood)
(doctor-put-meaning lonely 'mood)
(doctor-put-meaning angry 'mood)
(doctor-put-meaning mad 'mood)
(doctor-put-meaning pissed 'mood)
(doctor-put-meaning jealous 'mood)
(doctor-put-meaning afraid 'fear)
(doctor-put-meaning terrified 'fear)
(doctor-put-meaning fear 'fear)
(doctor-put-meaning scared 'fear)
(doctor-put-meaning frightened 'fear)
(doctor-put-meaning virginity 'sexnoun)
(doctor-put-meaning virgins 'sexnoun)
(doctor-put-meaning virgin 'sexnoun)
(doctor-put-meaning cock 'sexnoun)
(doctor-put-meaning cocks 'sexnoun)
(doctor-put-meaning dick 'sexnoun)
(doctor-put-meaning dicks 'sexnoun)
(doctor-put-meaning cunt 'sexnoun)
(doctor-put-meaning cunts 'sexnoun)
(doctor-put-meaning prostitute 'sexnoun)
(doctor-put-meaning condom 'sexnoun)
(doctor-put-meaning sex 'sexnoun)
(doctor-put-meaning rapes 'sexnoun)
(doctor-put-meaning wife 'family)
(doctor-put-meaning family 'family)
(doctor-put-meaning brothers 'family)
(doctor-put-meaning sisters 'family)
(doctor-put-meaning parent 'family)
(doctor-put-meaning parents 'family)
(doctor-put-meaning brother 'family)
(doctor-put-meaning sister 'family)
(doctor-put-meaning father 'family)
(doctor-put-meaning mother 'family)
(doctor-put-meaning husband 'family)
(doctor-put-meaning siblings 'family)
(doctor-put-meaning grandmother 'family)
(doctor-put-meaning grandfather 'family)
(doctor-put-meaning maternal 'family)
(doctor-put-meaning paternal 'family)
(doctor-put-meaning stab 'death)
(doctor-put-meaning murder 'death)
(doctor-put-meaning murders 'death)
(doctor-put-meaning suicide 'death)
(doctor-put-meaning suicides 'death)
(doctor-put-meaning kill 'death)
(doctor-put-meaning kills 'death)
(doctor-put-meaning killing 'death)
(doctor-put-meaning die 'death)
(doctor-put-meaning dies 'death)
(doctor-put-meaning died 'death)
(doctor-put-meaning dead 'death)
(doctor-put-meaning death 'death)
(doctor-put-meaning deaths 'death)
(doctor-put-meaning pain 'symptoms)
(doctor-put-meaning ache 'symptoms)
(doctor-put-meaning fever 'symptoms)
(doctor-put-meaning sore 'symptoms)
(doctor-put-meaning aching 'symptoms)
(doctor-put-meaning stomachache 'symptoms)
(doctor-put-meaning headache 'symptoms)
(doctor-put-meaning hurts 'symptoms)
(doctor-put-meaning disease 'symptoms)
(doctor-put-meaning virus 'symptoms)
(doctor-put-meaning vomit 'symptoms)
(doctor-put-meaning vomiting 'symptoms)
(doctor-put-meaning barf 'symptoms)
(doctor-put-meaning toothache 'symptoms)
(doctor-put-meaning hurt 'symptoms)
(doctor-put-meaning rum 'alcohol)
(doctor-put-meaning gin 'alcohol)
(doctor-put-meaning vodka 'alcohol)
(doctor-put-meaning alcohol 'alcohol)
(doctor-put-meaning bourbon 'alcohol)
(doctor-put-meaning beer 'alcohol)
(doctor-put-meaning wine 'alcohol)
(doctor-put-meaning whiskey 'alcohol)
(doctor-put-meaning scotch 'alcohol)
(doctor-put-meaning fuck 'sexverb)
(doctor-put-meaning fucked 'sexverb)
(doctor-put-meaning screw 'sexverb)
(doctor-put-meaning screwing 'sexverb)
(doctor-put-meaning fucking 'sexverb)
(doctor-put-meaning rape 'sexverb)
(doctor-put-meaning raped 'sexverb)
(doctor-put-meaning kiss 'sexverb)
(doctor-put-meaning kissing 'sexverb)
(doctor-put-meaning kisses 'sexverb)
(doctor-put-meaning screws 'sexverb)
(doctor-put-meaning fucks 'sexverb)
(doctor-put-meaning because 'conj)
(doctor-put-meaning but 'conj)
(doctor-put-meaning however 'conj)
(doctor-put-meaning besides 'conj)
(doctor-put-meaning anyway 'conj)
(doctor-put-meaning that 'conj)
(doctor-put-meaning except 'conj)
(doctor-put-meaning why 'conj)
(doctor-put-meaning how 'conj)
(doctor-put-meaning until 'when)
(doctor-put-meaning when 'when)
(doctor-put-meaning whenever 'when)
(doctor-put-meaning while 'when)
(doctor-put-meaning since 'when)
(doctor-put-meaning rms 'rms)
(doctor-put-meaning stallman 'rms)
(doctor-put-meaning school 'school)
(doctor-put-meaning schools 'school)
(doctor-put-meaning skool 'school)
(doctor-put-meaning grade 'school)
(doctor-put-meaning grades 'school)
(doctor-put-meaning teacher 'school)
(doctor-put-meaning teachers 'school)
(doctor-put-meaning classes 'school)
(doctor-put-meaning professor 'school)
(doctor-put-meaning prof 'school)
(doctor-put-meaning profs 'school)
(doctor-put-meaning professors 'school)
(doctor-put-meaning mit 'school)
(doctor-put-meaning emacs 'eliza)
(doctor-put-meaning eliza 'eliza)
(doctor-put-meaning liza 'eliza)
(doctor-put-meaning elisa 'eliza)
(doctor-put-meaning weizenbaum 'eliza)
(doctor-put-meaning doktor 'eliza)
(doctor-put-meaning athletics 'sports)
(doctor-put-meaning baseball 'sports)
(doctor-put-meaning basketball 'sports)
(doctor-put-meaning football 'sports)
(doctor-put-meaning frisbee 'sports)
(doctor-put-meaning gym 'sports)
(doctor-put-meaning gymnastics 'sports)
(doctor-put-meaning hockey 'sports)
(doctor-put-meaning lacrosse 'sports)
(doctor-put-meaning soccer 'sports)
(doctor-put-meaning softball 'sports)
(doctor-put-meaning sports 'sports)
(doctor-put-meaning swimming 'sports)
(doctor-put-meaning swim 'sports)
(doctor-put-meaning tennis 'sports)
(doctor-put-meaning volleyball 'sports)
(doctor-put-meaning math 'math)
(doctor-put-meaning mathematics 'math)
(doctor-put-meaning mathematical 'math)
(doctor-put-meaning theorem 'math)
(doctor-put-meaning axiom 'math)
(doctor-put-meaning lemma 'math)
(doctor-put-meaning algebra 'math)
(doctor-put-meaning algebraic 'math)
(doctor-put-meaning trig 'math)
(doctor-put-meaning trigonometry 'math)
(doctor-put-meaning trigonometric 'math)
(doctor-put-meaning geometry 'math)
(doctor-put-meaning geometric 'math)
(doctor-put-meaning calculus 'math)
(doctor-put-meaning arithmetic 'math)
(doctor-put-meaning zippy 'zippy)
(doctor-put-meaning zippy 'zippy)
(doctor-put-meaning pinhead 'zippy)
(doctor-put-meaning chat 'chat)

;;;###autoload
(defun doctor ()
  "Switch to *doctor* buffer and start giving psychotherapy."
  (interactive)
  (switch-to-buffer "*doctor*")
  (doctor-mode))

(defun doctor-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence."
  (interactive "*p")
  (if (= (preceding-char) ?\n)
      (doctor-read-print)
    (newline arg)))

(defun doctor-read-print nil
  "Top level loop."
  (interactive)
  (let ((sent (doctor-readin)))
    (insert "\n")
    (setq doctor--lincount (1+ doctor--lincount))
    (doctor-doc sent)
    (insert "\n")
    (setq doctor--bak sent)))

(defun doctor-readin nil
  "Read a sentence.  Return it as a list of words."
  (let (sentence)
    (backward-sentence 1)
    (while (not (eobp))
      (setq sentence (append sentence (list (doctor-read-token)))))
    sentence))

(defun doctor-read-token ()
  "Read one word from buffer."
  (prog1 (intern (downcase (buffer-substring (point)
					     (progn
					       (forward-word 1)
					       (point)))))
    (re-search-forward "\\Sw*")))

;; Main processing function for sentences that have been read.

(defun doctor-doc (sent)
  (cond
   ((equal sent '(foo))
    (doctor-type '(bar! (doc$ doctor--please) (doc$ doctor--continue) \.)))
   ((member sent doctor--howareyoulst)
    (doctor-type '(i\'m ok \.  (doc$ doctor--describe) yourself \.)))
   ((or (member sent '((good bye) (see you later) (i quit) (so long)
		       (go away) (get lost)))
	(memq (car sent)
	      '(bye halt break quit done exit goodbye
		    bye\, stop pause goodbye\, stop pause)))
    (doctor-type (doc$ doctor--bye)))
   ((and (eq (car sent) 'you)
	 (memq (cadr sent) doctor--abusewords))
    (setq doctor-found (cadr sent))
    (doctor-type (doc$ doctor--abuselst)))
   ((eq (car sent) 'whatmeans)
    (doctor-def (cadr sent)))
   ((equal sent '(parse))
    (doctor-type (list  'subj '= doctor-subj ",  "
			'verb '= doctor-verb "\n"
			'object 'phrase '= doctor-obj ","
			'noun 'form '=  doctor-object "\n"
			'current 'keyword 'is doctor-found
			", "
			'most 'recent 'possessive
			'is doctor-owner "\n"
			'sentence 'used 'was
			"..."
			'(doc// doctor--bak))))
   ((memq (car sent) '(are is do has have how when where who why))
    (doctor-type (doc$ doctor--qlist)))
   ;;   ((eq (car sent) 'forget)
   ;;    (set (cadr sent) nil)
   ;;    (doctor-type '((doc$ doctor--isee) (doc$ doctor--please)
   ;;     (doc$ doctor--continue)\.)))
   (t
    (if (doctor-defq sent) (doctor-define sent doctor-found))
    (if (> (length sent) 12) (setq sent (doctor-shorten sent)))
    (setq sent (doctor-correct-spelling (doctor-replace sent doctor--replist)))
    (cond ((and (not (memq 'me sent)) (not (memq 'i sent))
		(memq 'am sent))
	   (setq sent (doctor-replace sent '((am . (are)))))))
    (cond ((equal (car sent) 'yow) (doctor-zippy))
	  ((< (length sent) 2)
	   (cond ((eq (doctor-meaning (car sent)) 'howdy)
		  (doctor-howdy))
		 (t (doctor-short))))
	  (t
	   (if (memq 'am sent)
	       (setq sent (doctor-replace sent '((me . (i))))))
	   (setq sent (doctor-fixup sent))
	   (if (and (eq (car sent) 'do) (eq (cadr sent) 'not))
	       (cond ((zerop (random 3))
		      (doctor-type '(are you (doc$ doctor--afraidof) that \?)))
		     ((zerop (random 2))
		      (doctor-type '(don\'t tell me what to do \. i am the
					    doctor here!))
		      (doctor-rthing))
		     (t
		      (doctor-type '((doc$ doctor--whysay) that i shouldn\'t
				     (cddr sent)
				     \?))))
	     (doctor-go (doctor-wherego sent))))))))

;; Things done to process sentences once read.

(defun doctor-correct-spelling (sent)
  "Correct the spelling and expand each word in sentence."
  (if sent
      (apply 'append (mapcar (lambda (word)
				(if (memq word doctor--typos)
				    (get (get word 'doctor-correction)
					 'doctor-expansion)
				  (list word)))
			     sent))))

(defun doctor-shorten (sent)
  "Make a sentence manageably short using a few hacks."
  (let (foo
	(retval sent)
	(temp '(because but however besides anyway until
		    while that except why how)))
    (while temp
	   (setq foo (memq (car temp) sent))
	   (if (and foo
		    (> (length foo) 3))
	       (setq retval (doctor-fixup foo)
		     temp nil)
	       (setq temp (cdr temp))))
    retval))

(defun doctor-define (sent found)
  (doctor-svo sent found 1 nil)
  (and
   (doctor-nounp doctor-subj)
   (not (doctor-pronounp doctor-subj))
   doctor-subj
   (doctor-meaning doctor-object)
   (put doctor-subj 'doctor-meaning (doctor-meaning doctor-object))
   t))

(defun doctor-defq (sent)
  "Set global var DOCTOR-FOUND to first keyword found in sentence SENT."
  (setq doctor-found nil)
  (let ((temp '(means applies mean refers refer related
		      similar defined associated linked like same)))
    (while temp
	   (if (memq (car temp) sent)
	       (setq doctor-found (car temp)
		     temp nil)
	       (setq temp (cdr temp)))))
  doctor-found)

(defun doctor-def (x)
  (doctor-type (list 'the 'word x 'means (doctor-meaning x) 'to 'me))
  nil)

(defun doctor-forget ()
  "Delete the last element of the history list."
  (setq doctor--history (reverse (cdr (reverse doctor--history)))))

(defun doctor-query (x)
  "Prompt for a line of input from the minibuffer until a noun or verb is seen.
Put dialogue in buffer."
  (let (a
	(prompt (concat (doctor-make-string x)
			" what \?  "))
	retval)
    (while (not retval)
	   (while (not a)
	     (insert ?\n
		     prompt
		     (read-string prompt)
		     ?\n)
	     (setq a (doctor-readin)))
	   (while (and a (not retval))
		  (cond ((doctor-nounp (car a))
			 (setq retval (car a)))
			((doctor-verbp (car a))
			 (setq retval (doctor-build
				       (doctor-build x " ")
				       (car a))))
			((setq a (cdr a))))))
    retval))

(defun doctor-subjsearch (sent key type)
  "Search for the subject of a sentence SENT, looking for the noun closest
to and preceding KEY by at least TYPE words.  Set global variable doctor-subj to
the subject noun, and return the portion of the sentence following it."
  (let ((i (- (length sent) (length (memq key sent)) type)))
    (while (and (> i -1) (not (doctor-nounp (nth i sent))))
      (setq i (1- i)))
    (cond ((> i -1)
	   (setq doctor-subj (nth i sent))
	   (nthcdr (1+ i) sent))
	  (t
	   (setq doctor-subj 'you)
	   nil))))

(defun doctor-nounp (x)
  "Return t if the symbol argument is a noun."
	(or (doctor-pronounp x)
	    (not (or (doctor-verbp x)
		     (equal x 'not)
		     (doctor-prepp x)
		     (doctor-modifierp x) )) ))

(defun doctor-pronounp (x)
  "Return t if the symbol argument is a pronoun."
  (memq x '(
	i me mine myself
	we us ours ourselves ourself
	you yours yourself yourselves
	he him himself she hers herself
	it that those this these things thing
	they them themselves theirs
	anybody everybody somebody
	anyone everyone someone
	anything something everything)))

(dolist (x
         '(abort aborted aborts ask asked asks am
           applied applies apply are associate
           associated ate
           be became become becomes becoming
           been being believe believed believes
           bit bite bites bore bored bores boring bought buy buys buying
           call called calling calls came can caught catch come
           contract contracted contracts control controlled controls
           could croak croaks croaked cut cuts
           dare dared define defines dial dialed dials did die died dies
           dislike disliked
           dislikes do does drank drink drinks drinking
           drive drives driving drove dying
           eat eating eats expand expanded expands
           expect expected expects expel expels expelled
           explain explained explains
           fart farts feel feels felt fight fights find finds finding
           forget forgets forgot fought found
           fuck fucked fucking fucks
           gave get gets getting give gives go goes going gone got gotten
           had harm harms has hate hated hates have having
           hear heard hears hearing help helped helping helps
           hit hits hope hoped hopes hurt hurts
           implies imply is
           join joined joins jump jumped jumps
           keep keeping keeps kept
           kill killed killing kills kiss kissed kisses kissing
           knew know knows
           laid lay lays let lets lie lied lies like liked likes
           liking listen listens
           login look looked looking looks
           lose losing lost
           love loved loves loving
           luse lusing lust lusts
           made make makes making may mean means meant might
           move moved moves moving must
           need needed needs
           order ordered orders ought
           paid pay pays pick picked picking picks
           placed placing prefer prefers put puts
           ran rape raped rapes
           read reading reads recall receive received receives
           refer refered referred refers
           relate related relates remember remembered remembers
           romp romped romps run running runs
           said sang sat saw say says
           screw screwed screwing screws scrod see sees seem seemed
           seems seen sell selling sells
           send sendind sends sent shall shoot shot should
           sing sings sit sits sitting sold studied study
           take takes taking talk talked talking talks tell tells telling
           think thinks
           thought told took tooled touch touched touches touching
           transfer transferred transfers transmit transmits transmitted
           type types types typing
           walk walked walking walks want wanted wants was watch
           watched watching went were will wish would work worked works
           write writes writing wrote use used uses using))
  (put x 'doctor-sentence-type 'verb))

(defun doctor-verbp (x) (if (symbolp x)
			    (eq (get x 'doctor-sentence-type) 'verb)))

(defun doctor-plural (x)
  "Form the plural of the word argument."
  (let ((foo (doctor-make-string x)))
    (cond ((string-equal (substring foo -1) "s")
	   (cond ((string-equal (substring foo -2 -1) "s")
		  (intern (concat foo "es")))
		 (t x)))
	   ((string-equal (substring foo -1) "y")
	    (intern (concat (substring foo 0 -1)
			    "ies")))
	   (t (intern (concat foo "s"))))))

(defun doctor-setprep (sent key)
  (let ((val)
	(foo (memq key sent)))
    (cond ((doctor-prepp (cadr foo))
	   (setq val (doctor-getnoun (cddr foo)))
	   (cond (val val)
		 (t 'something)))
	  ((doctor-articlep (cadr foo))
	   (setq val (doctor-getnoun (cddr foo)))
	   (cond (val (doctor-build (doctor-build (cadr foo) " ") val))
		 (t 'something)))
	  (t 'something))))

(defun doctor-getnoun (x)
  (cond ((null x) (setq doctor-object 'something))
	((atom x) (setq doctor-object x))
	((eq (length x) 1)
	 (setq doctor-object (cond
		       ((doctor-nounp (setq doctor-object (car x))) doctor-object)
		       (t (doctor-query doctor-object)))))
	((eq (car x) 'to)
	 (doctor-build 'to\  (doctor-getnoun (cdr x))))
	((doctor-prepp (car x))
	 (doctor-getnoun (cdr x)))
	((not (doctor-nounp (car x)))
	 (doctor-build (doctor-build (cdr (assq (car x)
						(append
						 '((a . this)
						   (some . this)
						   (one . that))
						 (list
						  (cons
						   (car x) (car x))))))
				     " ")
		       (doctor-getnoun (cdr x))))
	(t (setq doctor-object (car x))
	   (doctor-build (doctor-build (car x) " ") (doctor-getnoun (cdr x))))
	))

(defun doctor-modifierp (x)
  (or (doctor-adjectivep x)
      (doctor-adverbp x)
      (doctor-othermodifierp x)))

(defun doctor-adjectivep (x)
  (or (numberp x)
      (doctor-nmbrp x)
      (doctor-articlep x)
      (doctor-colorp x)
      (doctor-sizep x)
      (doctor-possessivepronounp x)))

(defun doctor-adverbp (xx)
  (let ((xxstr (doctor-make-string xx)))
    (and (>= (length xxstr) 2)
	 (string-equal (substring (doctor-make-string xx) -2) "ly")
	 (not (memq xx '(family fly jelly rally))))))

(defun doctor-articlep (x)
  (memq x '(the a an)))

(defun doctor-nmbrp (x)
  (memq x '(one two three four five six seven eight nine ten
		eleven twelve thirteen fourteen fifteen
		sixteen seventeen eighteen nineteen
		twenty thirty forty fifty sixty seventy eighty ninety
		hundred thousand million billion
		half quarter
		first second third fourth fifth
		sixth seventh eighth ninth tenth)))

(defun doctor-colorp (x)
  (memq x '(beige black blue brown crimson
		  gray grey green
		  orange pink purple red tan tawny
		  violet white yellow)))

(defun doctor-sizep (x)
  (memq x '(big large tall fat wide thick
		small petite short thin skinny)))

(defun doctor-possessivepronounp (x)
  (memq x '(my your his her our their)))

(defun doctor-othermodifierp (x)
  (memq x '(all also always amusing any anyway associated awesome
		bad beautiful best better but certain clear
		ever every fantastic fun funny
		good great grody gross however if ignorant
		less linked losing lusing many more much
		never nice obnoxious often poor pretty real related rich
		similar some stupid super superb
		terrible terrific too total tubular ugly very)))

(defun doctor-prepp (x)
  (memq x '(about above after around as at
		  before beneath behind beside between by
		  for from in inside into
		  like near next of on onto over
		  same through thru to toward towards
		  under underneath with without)))

(defun doctor-remember (thing)
  (cond ((null doctor--history)
	 (setq doctor--history (list thing)))
	(t (setq doctor--history (append doctor--history (list thing))))))

(defun doctor-type (x)
  (setq x (doctor-fix-2 x))
  (doctor-txtype (doctor-assm x)))

(defun doctor-fixup (sent)
  (setq sent (append
	      (cdr
	       (assq (car sent)
		     (append
		      '((me  i)
			(him  he)
			(her  she)
			(them  they)
			(okay)
			(well)
			(sigh)
			(hmm)
			(hmmm)
			(hmmmm)
			(hmmmmm)
			(gee)
			(sure)
			(great)
			(oh)
			(fine)
			(ok)
			(no))
		      (list (list (car sent)
				  (car sent))))))
	      (cdr sent)))
  (doctor-fix-2 sent))

(defun doctor-fix-2 (sent)
  (let ((foo sent))
    (while foo
      (if (and (eq (car foo) 'me)
	       (doctor-verbp (cadr foo)))
	  (rplaca foo 'i)
	(cond ((eq (car foo) 'you)
	       (cond ((memq (cadr foo) '(am be been is))
		      (rplaca (cdr foo) 'are))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))
		     ((memq (cadr foo) '(was))
		      (rplaca (cdr foo) 'were))))
	      ((equal (car foo) 'i)
	       (cond ((memq (cadr foo) '(are is be been))
		      (rplaca (cdr foo) 'am))
		     ((memq (cadr foo) '(were))
		      (rplaca (cdr foo) 'was))
		     ((memq (cadr foo) '(has))
		      (rplaca (cdr foo) 'have))))
	      ((and (doctor-verbp (car foo))
		    (eq (cadr foo) 'i)
		    (not (doctor-verbp (car (cddr foo)))))
	       (rplaca (cdr foo) 'me))
	      ((and (eq (car foo) 'a)
		    (doctor-vowelp (string-to-char
				    (doctor-make-string (cadr foo)))))
	       (rplaca foo 'an))
	      ((and (eq (car foo) 'an)
		    (not (doctor-vowelp (string-to-char
					 (doctor-make-string (cadr foo))))))
	       (rplaca foo 'a)))
	(setq foo (cdr foo))))
    sent))

(defun doctor-vowelp (x)
  (memq x '(?a ?e ?i ?o ?u)))

(defun doctor-replace (sent rlist)
  "Replace any element of SENT that is the car of a replacement
element pair in RLIST."
  (apply 'append
	 (mapcar
	   (lambda (x)
	     (cdr (or (assq x rlist)   ; either find a replacement
		      (list x x))))    ; or fake an identity mapping
	   sent)))

(defun doctor-wherego (sent)
  (cond ((null sent) (doc$ doctor--whereoutp))
	((null (doctor-meaning (car sent)))
	 (doctor-wherego (cond ((zerop (random 2))
				(reverse (cdr sent)))
			       (t (cdr sent)))))
	(t
	 (setq doctor-found (car sent))
	 (doctor-meaning (car sent)))))

(defun doctor-svo (sent key type mem)
  "Find subject, verb and object in sentence SENT with focus on word KEY.
TYPE is number of words preceding KEY to start looking for subject.
MEM is t if results are to be put on Doctor's memory stack.
Return in the global variables DOCTOR-SUBJ, DOCTOR-VERB, DOCTOR-OBJECT,
and DOCTOR-OBJ."
  (let ((foo (doctor-subjsearch sent key type)))
    (or foo
	(setq foo sent
	      mem nil))
    (while (and (null (doctor-verbp (car foo))) (cdr foo))
      (setq foo (cdr foo)))
    (setq doctor-verb (car foo))
    (setq doctor-obj (doctor-getnoun (cdr foo)))
    (cond ((eq doctor-object 'i) (setq doctor-object 'me))
	  ((eq doctor-subj 'me) (setq doctor-subj 'i)))
    (cond (mem (doctor-remember (list doctor-subj doctor-verb doctor-obj))))))

(defun doctor-possess (sent key)
  "Set possessive in SENT for keyword KEY.
Hack on previous word, setting global variable DOCTOR-OWNER to correct result."
  (let* ((i (- (length sent) (length (memq key sent)) 1))
	 (prev (if (< i 0) 'your
		 (nth i sent))))
    (setq doctor-owner
	  (if (or (doctor-possessivepronounp prev)
		  (string-equal "s"
				(substring (doctor-make-string prev)
					   -1)))
	      prev
	    'your))))

;; Output of replies.

(defun doctor-txtype (ans)
  "Output to buffer a list of symbols or strings as a sentence."
  (setq doctor--*print-upcase* t doctor--*print-space* nil)
  (mapc 'doctor-type-symbol ans)
  (insert "\n"))

(defun doctor-type-symbol (word)
  "Output a symbol to the buffer with some fancy case and spacing hacks."
  (setq word (doctor-make-string word))
  (if (string-equal word "i") (setq word "I"))
  (when doctor--*print-upcase*
    (setq word (capitalize word))
    (if doctor--*print-space* (insert " ")))
  (cond ((or (string-match "^[.,;:?! ]" word)
	     (not doctor--*print-space*))
	 (insert word))
	(t (insert ?\s word)))
  (and auto-fill-function
       (> (current-column) fill-column)
       (apply auto-fill-function nil))
  (setq doctor--*print-upcase* (string-match "[.?!]$" word)
	doctor--*print-space* t))

(defun doctor-build (str1 str2)
  "Make a symbol out of the concatenation of the two non-list arguments."
  (cond ((null str1) str2)
	((null str2) str1)
	((and (atom str1)
	      (atom str2))
	 (intern (concat (doctor-make-string str1)
			 (doctor-make-string str2))))
	(t nil)))

(defun doctor-make-string (obj)
  (cond ((stringp obj) obj)
	((symbolp obj) (symbol-name obj))
	((numberp obj) (int-to-string obj))
	(t "")))

(defun doctor-concat (x y)
  "Like append, but force atomic arguments to be lists."
  (append
   (if (and x (atom x)) (list x) x)
   (if (and y (atom y)) (list y) y)))

(defun doctor-assm (proto)
  (cond ((null proto) nil)
	((atom proto) (list proto))
	((atom (car proto))
	 (cons (car proto) (doctor-assm (cdr proto))))
	(t (doctor-concat (doctor-assm (eval (car proto))) (doctor-assm (cdr proto))))))

;; Functions that handle specific words or meanings when found.

(defun doctor-go (destination)
  "Call a `doctor-*' function."
  (funcall (intern (concat "doctor-" (doctor-make-string destination)))))

(defun doctor-desire1 ()
  (doctor-go (doc$ doctor--whereoutp)))

(defun doctor-huh ()
  (cond ((< (length doctor-sent) 9) (doctor-type (doc$ doctor--huhlst)))
	(t (doctor-type (doc$ doctor--longhuhlst)))))

(defun doctor-rthing () (doctor-type (doc$ doctor--thlst)))

(defun doctor-remem () (cond ((null doctor--history) (doctor-huh))
			     ((doctor-type (doc$ doctor--remlst)))))

(defun doctor-howdy ()
  (cond ((not doctor--howdyflag)
	 (doctor-type '((doc$ doctor--hello) what brings you to see me \?))
	 (setq doctor--howdyflag t))
	(t
	 (doctor-type '((doc$ doctor--ibelieve) we\'ve introduced ourselves already \.))
	 (doctor-type '((doc$ doctor--please) (doc$ doctor--describe) (doc$ doctor--things) \.)))))

(defun doctor-when ()
  (cond ((< (length (memq doctor-found doctor-sent)) 3) (doctor-short))
	(t
	 (setq doctor-sent (cdr (memq doctor-found doctor-sent)))
	 (setq doctor-sent (doctor-fixup doctor-sent))
	 (doctor-type '((doc$ doctor--whatwhen) (doc// doctor-sent) \?)))))

(defun doctor-conj ()
  (cond ((< (length (memq doctor-found doctor-sent)) 4) (doctor-short))
	(t
	 (setq doctor-sent (cdr (memq doctor-found doctor-sent)))
	 (setq doctor-sent (doctor-fixup doctor-sent))
	 (cond ((eq (car doctor-sent) 'of)
		(doctor-type '(are you (doc$ doctor--sure) that is the real reason \?))
		(setq doctor--things (cons (cdr doctor-sent) doctor--things)))
	       (t
		(doctor-remember doctor-sent)
		(doctor-type (doc$ doctor--beclst)))))))

(defun doctor-short ()
  (cond ((= (car doctor--repetitive-shortness) (1- doctor--lincount))
	 (rplacd doctor--repetitive-shortness
		 (1+ (cdr doctor--repetitive-shortness))))
	(t
	 (rplacd doctor--repetitive-shortness 1)))
  (rplaca doctor--repetitive-shortness doctor--lincount)
  (cond ((> (cdr doctor--repetitive-shortness) 6)
	 (cond ((not doctor--**mad**)
		(doctor-type '((doc$ doctor--areyou)
			       just trying to see what kind of things
			       i have in my vocabulary \? please try to
			       carry on a reasonable conversation!))
		(setq doctor--**mad** t))
	       (t
		(doctor-type '(i give up \. you need a lesson in creative
				 writing \.\.\.))
		)))
	(t
	 (cond ((equal doctor-sent (doctor-assm '(yes)))
		(doctor-type '((doc$ doctor--isee) (doc$ doctor--inter) (doc$ doctor--whysay) this is so \?)))
	       ((equal doctor-sent (doctor-assm '(because)))
		(doctor-type (doc$ doctor--shortbeclst)))
	       ((equal doctor-sent (doctor-assm '(no)))
		(doctor-type (doc$ doctor--neglst)))
	       (t (doctor-type (doc$ doctor--shortlst)))))))

(defun doctor-alcohol () (doctor-type (doc$ doctor--drnk)))

(defun doctor-desire ()
  (let ((foo (memq doctor-found doctor-sent)))
    (cond ((< (length foo) 2)
	   (doctor-go (doctor-build (doctor-meaning doctor-found) 1)))
	  ((memq (cadr foo) '(a an))
	   (rplacd foo (append '(to have) (cdr foo)))
	   (doctor-svo doctor-sent doctor-found 1 nil)
	   (doctor-remember (list doctor-subj 'would 'like doctor-obj))
	   (doctor-type (doc$ doctor--whywant)))
	  ((not (eq (cadr foo) 'to))
	   (doctor-go (doctor-build (doctor-meaning doctor-found) 1)))
	  (t
	   (doctor-svo doctor-sent doctor-found 1 nil)
	   (doctor-remember (list doctor-subj 'would 'like doctor-obj))
	   (doctor-type (doc$ doctor--whywant))))))

(defun doctor-drug ()
  (doctor-type (doc$ doctor--drugs))
  (doctor-remember (list 'you 'used doctor-found)))

(defun doctor-toke ()
  (doctor-type (doc$ doctor--toklst)))

(defun doctor-state ()
  (doctor-type (doc$ doctor--states)) (doctor-remember (list 'you 'were doctor-found)))

(defun doctor-mood ()
  (doctor-type (doc$ doctor--moods)) (doctor-remember (list 'you 'felt doctor-found)))

(defun doctor-fear ()
  (setq doctor--feared (doctor-setprep doctor-sent doctor-found))
  (doctor-type (doc$ doctor--fears))
  (doctor-remember (list 'you 'were 'afraid 'of doctor--feared)))

(defun doctor-hate ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (cond ((memq 'not doctor-sent) (doctor-forget) (doctor-huh))
	((equal doctor-subj 'you)
	 (doctor-type '(why do you (doc// doctor-verb) (doc// doctor-obj) \?)))
	(t (doctor-type '((doc$ doctor--whysay) (list doctor-subj doctor-verb doctor-obj))))))

(defun doctor-symptoms ()
  (doctor-type '((doc$ doctor--maybe) you should consult a medical doctor\;
		 i am a psychotherapist. \.)))

(defun doctor-hates ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (doctor-hates1))

(defun doctor-hates1 ()
  (doctor-type '((doc$ doctor--whysay) (list doctor-subj doctor-verb doctor-obj) \?)))

(defun doctor-loves ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (doctor-qloves))

(defun doctor-qloves ()
  (doctor-type '((doc$ doctor--bother) (list doctor-subj doctor-verb doctor-obj) \?)))

(defun doctor-love ()
  (doctor-svo doctor-sent doctor-found 1 t)
  (cond ((memq 'not doctor-sent) (doctor-forget) (doctor-huh))
	((memq 'to doctor-sent) (doctor-hates1))
	(t
	 (cond ((equal doctor-object 'something)
		(setq doctor-object '(this person you love))))
	 (cond ((equal doctor-subj 'you)
		(setq doctor--lover doctor-obj)
		(cond ((equal doctor--lover '(this person you love))
		       (setq doctor--lover '(your partner))
		       (doctor-forget)
		       (doctor-type '(with whom are you in love \?)))
		      ((doctor-type '((doc$ doctor--please)
				      (doc$ doctor--describe)
				      (doc$ doctor--relation)
				      (doc// doctor--lover)
				      \.)))))
	       ((equal doctor-subj 'i)
		(doctor-txtype '(we were discussing you!)))
	       (t (doctor-forget)
		  (setq doctor-obj 'someone)
		  (setq doctor-verb (doctor-build doctor-verb 's))
		  (doctor-qloves))))))

(defun doctor-mach ()
  (setq doctor-found (doctor-plural doctor-found))
  (doctor-type (doc$ doctor--machlst)))

(defun doctor-sexnoun () (doctor-sexverb))

(defun doctor-sexverb ()
  (if (or (memq 'me doctor-sent) (memq 'myself doctor-sent) (memq 'i doctor-sent))
      (doctor-foul)
    (doctor-type (doc$ doctor--sexlst))))

(defun doctor-death ()
  (cond (doctor--suicide-flag (doctor-type (doc$ doctor--deathlst)))
	((or (equal doctor-found 'suicide)
             (and (or (equal doctor-found 'kill)
                      (equal doctor-found 'killing))
                  (memq 'yourself doctor-sent)))
	 (setq doctor--suicide-flag t)
	 (doctor-type '(If you are really suicidal\, you might
			   want to contact the Samaritans via
			   E-mail: jo@samaritans.org or\, at your option\,
			   anonymous E-mail: samaritans@anon.twwells.com\ \.
                           or find a Befrienders crisis center at
			   http://www.befrienders.org/\ \.
			   (doc$ doctor--please) (doc$ doctor--continue) \.)))
	(t (doctor-type (doc$ doctor--deathlst)))))

(defun doctor-foul ()
  (doctor-type (doc$ doctor--foullst)))

(defun doctor-family ()
  (doctor-possess doctor-sent doctor-found)
  (doctor-type (doc$ doctor--famlst)))

;; I did not add this -- rms.
;; But he might have removed it.  I put it back.  --roland
(defun doctor-rms ()
  (cond (doctor--rms-flag (doctor-type (doc$ doctor--stallmanlst)))
	(t (setq doctor--rms-flag t) (doctor-type '(do you know Stallman \?)))))

(defun doctor-school nil (doctor-type (doc$ doctor--schoollst)))

(defun doctor-eliza ()
  (cond (doctor--eliza-flag (doctor-type (doc$ doctor--elizalst)))
	(t (setq doctor--eliza-flag t)
	   (doctor-type '((doc// doctor-found) \? hah !
			  (doc$ doctor--please) (doc$ doctor--continue) \.)))))

(defun doctor-sports () (doctor-type (doc$ doctor--sportslst)))

(defun doctor-math () (doctor-type (doc$ doctor--mathlst)))

(defun doctor-zippy ()
  (cond (doctor--zippy-flag (doctor-type (doc$ doctor--zippylst)))
	(t (setq doctor--zippy-flag t)
	   (doctor-type '(yow! are we interactive yet \?)))))


(defun doctor-chat () (doctor-type (doc$ doctor--chatlst)))

(random t)

(provide 'doctor)

;;; doctor.el ends here
