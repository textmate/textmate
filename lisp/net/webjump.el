;;; webjump.el --- programmable Web hotlist

;; Copyright (C) 1996-1997, 2001-2012 Free Software Foundation, Inc.

;; Author:    Neil W. Van Dyke <nwv@acm.org>
;; Created:   09-Aug-1996
;; Keywords:  comm www

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

;; WebJump provides a sort of ``programmable hotlist'' of Web sites that can
;; quickly be invoked in your Web browser.  Each Web site in the hotlist has a
;; name, and you select the desired site name via a completing string prompt in
;; the minibuffer.  The URL for each Web site is defined as a static string or
;; a built-in or custom function, allowing interactive prompting for
;; site-specific queries and options.

;; Note that WebJump was originally intended to complement your conventional
;; browser-based hotlist, not replace it.  (Though there's no reason you
;; couldn't use WebJump for your entire hotlist if you were so inclined.)

;; The `webjump-sites' variable, which defines the hotlist, defaults to some
;; example sites.  You'll probably want to override it with your own favorite
;; sites.  The documentation for the variable describes the syntax.

;; You may wish to add something like the following to your `.emacs' file:
;;
;;   (require 'webjump)
;;   (global-set-key "\C-cj" 'webjump)
;;   (setq webjump-sites
;;         (append '(
;;                   ("My Home Page" . "www.someisp.net/users/joebobjr/")
;;                   ("Pop's Site"   . "www.joebob-and-son.com/")
;;                   )
;;                 webjump-sample-sites))
;;
;; The above loads this package, binds `C-c j' to invoke WebJump, and adds your
;; personal favorite sites to the hotlist.

;; The `webjump-sample-sites' variable mostly contains some site entries that
;; are expected to be generally relevant to many users, but excluding
;; those that the GNU project would not want to recommend.

;; The `browse-url' package is used to submit URLs to the browser, so any
;; browser-specific configuration should be done there.

;;; Code:

;;-------------------------------------------------------- Package Dependencies

(require 'browse-url)

;;------------------------------------------------------------------- Constants

(defvar webjump-sample-sites
  '(
    ;; FSF, not including Emacs-specific.
    ("GNU Project FTP Archive" .
     ;; GNU FTP Mirror List from http://www.gnu.org/order/ftp.html
     [mirrors "ftp://ftp.gnu.org/pub/gnu/"
              ;; United States
              "ftp://mirrors.kernel.org/gnu"
              "ftp://gatekeeper.dec.com/pub/GNU/"
              "ftp://ftp.keystealth.org/pub/gnu/"
              "ftp://mirrors.usc.edu/pub/gnu/"
              "ftp://cudlug.cudenver.edu/pub/mirrors/ftp.gnu.org/"
              "ftp://ftp.cise.ufl.edu/pub/mirrors/GNU/"
              "ftp://uiarchive.cso.uiuc.edu/pub/ftp/ftp.gnu.org/gnu/"
              "ftp://gnu.cs.lewisu.edu/gnu/"
              "ftp://ftp.in-span.net/pub/mirrors/ftp.gnu.org/"
              "ftp://gnu.ms.uky.edu/pub/mirrors/gnu/"
              "ftp://ftp.algx.net/pub/gnu/"
              "ftp://aeneas.mit.edu/pub/gnu/"
              "ftp://ftp.egr.msu.edu/pub/gnu/"
              "ftp://ftp.wayne.edu/pub/gnu/"
              "ftp://wuarchive.wustl.edu/mirrors/gnu/"
              "ftp://gnu.teleglobe.net/ftp.gnu.org/"
              "ftp://ftp.cs.columbia.edu/archives/gnu/prep/"
              "ftp://ftp.ece.cornell.edu/pub/mirrors/gnu/"
              "ftp://ftp.ibiblio.org/pub/mirrors/gnu/"
              "ftp://ftp.cis.ohio-state.edu/mirror/gnu/"
              "ftp://ftp.club.cc.cmu.edu/gnu/"
              "ftp://ftp.sunsite.utk.edu/pub/gnu/ftp/"
              "ftp://thales.memphis.edu/pub/gnu/"
              "ftp://gnu.wwc.edu"
              "ftp://ftp.twtelecom.net/pub/GNU/"
              ;; Africa
              "ftp://ftp.sun.ac.za/mirrorsites/ftp.gnu.org"
              ;; The Americas
              "ftp://ftp.unicamp.br/pub/gnu/"
              "ftp://master.softaplic.com.br/pub/gnu/"
              "ftp://ftp.matrix.com.br/pub/gnu/"
              "ftp://ftp.pucpr.br/gnu"
              "ftp://ftp.linorg.usp.br/gnu"
              "ftp://ftp.cs.ubc.ca/mirror2/gnu/"
              "ftp://cs.ubishops.ca/pub/ftp.gnu.org/"
              "ftp://ftp.inf.utfsm.cl/pub/gnu/"
              "ftp://sunsite.ulatina.ac.cr/Mirrors/GNU/"
              "ftp://www.gnu.unam.mx/pub/gnu/software/"
              "ftp://gnu.cem.itesm.mx/pub/mirrors/gnu.org/"
              "ftp://ftp.azc.uam.mx/mirrors/gnu/"
              ;; Australia
              "ftp://mirror.aarnet.edu.au/pub/gnu/"
              "ftp://gnu.mirror.pacific.net.au/gnu/"
              ;; Asia
              "ftp://ftp.cs.cuhk.edu.hk/pub/gnu/gnu/"
              "ftp://sunsite.ust.hk/pub/gnu/"
              "ftp://ftp.gnupilgrims.org/pub/gnu"
              "ftp://www.imtech.res.in/mirror/gnuftp/"
              "ftp://kambing.vlsm.org/gnu"
              "ftp://ftp.cs.huji.ac.il/mirror/GNU/"
              "ftp://tron.um.u-tokyo.ac.jp/pub/GNU/"
              "ftp://core.ring.gr.jp/pub/GNU/"
              "ftp://ftp.ring.gr.jp/pub/GNU/"
              "ftp://mirrors.hbi.co.jp/gnu/"
              "ftp://ftp.cs.titech.ac.jp/pub/gnu/"
              "ftp://ftpmirror.hanyang.ac.kr/GNU/"
              "ftp://ftp.linux.sarang.net/mirror/gnu/gnu/"
              "ftp://ftp.xgate.co.kr/pub/mirror/gnu/"
              "ftp://ftp://gnu.xinicks.com/"
              "ftp://ftp.isu.net.sa/pub/gnu/"
              "ftp://ftp.nctu.edu.tw/UNIX/gnu/"
              "ftp://coda.nctu.edu.tw/UNIX/gnu/"
              "ftp://ftp1.sinica.edu.tw/pub3/GNU/gnu/"
              "ftp://gnu.cdpa.nsysu.edu.tw/gnu"
              "ftp://ftp.nectec.or.th/pub/mirrors/gnu/"
              ;; Europe
              "ftp://ftp.gnu.vbs.at/"
              "ftp://ftp.univie.ac.at/packages/gnu/"
              "ftp://gd.tuwien.ac.at/gnu/gnusrc/"
              "ftp://ftp.belnet.be/mirror/ftp.gnu.org/"
              "ftp://gnu.blic.net/pub/gnu/"
              "ftp://ftp.fi.muni.cz/pub/gnu/"
              "ftp://ftp.dkuug.dk/pub/gnu/"
              "ftp://sunsite.dk/mirrors/gnu"
              "ftp://ftp.funet.fi/pub/gnu/prep/"
              "ftp://ftp.irisa.fr/pub/gnu/"
              "ftp://ftp.cs.univ-paris8.fr/mirrors/ftp.gnu.org/"
              "ftp://ftp.cs.tu-berlin.de/pub/gnu/"
              "ftp://ftp.leo.org/pub/comp/os/unix/gnu/"
              "ftp://ftp.informatik.rwth-aachen.de/pub/gnu/"
              "ftp://ftp.de.uu.net/pub/gnu/"
              "ftp://ftp.freenet.de/pub/ftp.gnu.org/gnu/"
              "ftp://ftp.cs.uni-bonn.de/pub/gnu/"
              "ftp://ftp-stud.fht-esslingen.de/pub/Mirrors/ftp.gnu.org/"
              "ftp://ftp.stw-bonn.de/pub/mirror/ftp.gnu.org/"
              "ftp://ftp.math.uni-bremen.de/pub/gnu"
              "ftp://ftp.forthnet.gr/pub/gnu/"
              "ftp://ftp.ntua.gr/pub/gnu/"
              "ftp://ftp.duth.gr/pub/gnu/"
              "ftp://ftp.physics.auth.gr/pub/gnu/"
              "ftp://ftp.esat.net/pub/gnu/"
              "ftp://ftp.heanet.ie/mirrors/ftp.gnu.org"
              "ftp://ftp.lugroma2.org/pub/gnu/"
              "ftp://ftp.gnu.inetcosmos.org/pub/gnu/"
              "ftp://ftp.digitaltrust.it/pub/gnu"
              "ftp://ftp://rm.mirror.garr.it/mirrors/gnuftp"
              "ftp://ftp.nluug.nl/pub/gnu/"
              "ftp://ftp.mirror.nl/pub/mirror/gnu/"
              "ftp://ftp.nl.uu.net/pub/gnu/"
              "ftp://mirror.widexs.nl/pub/gnu/"
              "ftp://ftp.easynet.nl/mirror/GNU/"
              "ftp://ftp.win.tue.nl/pub/gnu"
              "ftp://gnu.mirror.vuurwerk.net/pub/GNU/"
              "ftp://gnu.kookel.org/pub/ftp.gnu.org/"
              "ftp://ftp.uninett.no/pub/gnu/"
              "ftp://ftp.task.gda.pl/pub/gnu/"
              "ftp://sunsite.icm.edu.pl/pub/gnu/"
              "ftp://ftp.man.poznan.pl/pub/gnu"
              "ftp://ftp.ist.utl.pt/pub/GNU/gnu/"
              "ftp://ftp.telepac.pt/pub/gnu/"
              "ftp://ftp.timisoara.roedu.net/mirrors/ftp.gnu.org/pub/gnu"
              "ftp://ftp.chg.ru/pub/gnu/"
              "ftp://gnuftp.axitel.ru/"
              "ftp://ftp.arnes.si/software/gnu/"
              "ftp://ftp.etsimo.uniovi.es/pub/gnu/"
              "ftp://ftp.rediris.es/pub/gnu/"
              "ftp://ftp.chl.chalmers.se/pub/gnu/"
              "ftp://ftp.isy.liu.se/pub/gnu/"
              "ftp://ftp.luth.se/pub/unix/gnu/"
              "ftp://ftp.stacken.kth.se/pub/gnu/"
              "ftp://ftp.sunet.se/pub/gnu/"
              "ftp://sunsite.cnlab-switch.ch/mirror/gnu/"
              "ftp://ftp.ulak.net.tr/gnu/"
              "ftp://ftp.gnu.org.ua"
              "ftp://ftp.mcc.ac.uk/pub/gnu/"
              "ftp://ftp.mirror.ac.uk/sites/ftp.gnu.org/gnu/"
              "ftp://ftp.warwick.ac.uk/pub/gnu/"
              "ftp://ftp.hands.com/ftp.gnu.org/"
              "ftp://gnu.teleglobe.net/ftp.gnu.org/"])
    ("GNU Project Home Page" . "www.gnu.org")

    ;; Emacs.
    ("Emacs Home Page" .
     "www.gnu.org/software/emacs/emacs.html")
    ("Savannah Emacs page" .
     "savannah.gnu.org/projects/emacs")
    ("Emacs Lisp List" .
     "www.damtp.cam.ac.uk/user/eglen/emacs/ell.html")
    ("Emacs Wiki" .
     [simple-query "www.emacswiki.org"
		   "www.emacswiki.org/cgi-bin/wiki/" ""])

    ;; Internet search engines.
    ("Google" .
     [simple-query "www.google.com"
		   "www.google.com/search?q=" ""])
    ("Google Groups" .
     [simple-query "groups.google.com"
		   "groups.google.com/groups?q=" ""])
    ("Yahoo" .
     [simple-query "www.yahoo.com" "search.yahoo.com/search?p=" ""])
    ("Yahoo: Reference" . "www.yahoo.com/Reference/")
    ("Wikipedia" .
     [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])

    ;; Misc. general interest.
    ("Interactive Weather Information Network" . webjump-to-iwin)
    ("Usenet FAQs" .
     "www.faqs.org/faqs/")
    ("RTFM Usenet FAQs by Group" .
     "ftp://rtfm.mit.edu/pub/usenet-by-group/")
    ("RTFM Usenet FAQs by Hierarchy" .
     "ftp://rtfm.mit.edu/pub/usenet-by-hierarchy/")
    ("X Consortium Archive" . "ftp.x.org")

    ;; Computer social issues, privacy, professionalism.
    ("Association for Computing Machinery" . "www.acm.org")
    ("Computer Professionals for Social Responsibility" . "www.cpsr.org")
    ("Electronic Frontier Foundation" . "www.eff.org")
    ("IEEE Computer Society" . "www.computer.org")
    ("Risks Digest" . webjump-to-risks)

    ;; More.
    ("Supplemental Web site list for webjump" .
     "www.neilvandyke.org/webjump/")

    )
  "Sample hotlist for WebJump.  See the documentation for the `webjump'
function and the `webjump-sites' variable.")

(defvar webjump-state-to-postal-alist
  '(("Alabama" . "al") ("Alaska" . "ak") ("Arizona" . "az") ("Arkansas" . "ar")
    ("California" . "ca") ("Colorado" . "co") ("Connecticut" . "ct")
    ("Delaware" . "de") ("Florida" . "fl") ("Georgia" . "ga") ("Hawaii" . "hi")
    ("Idaho" . "id") ("Illinois" . "il") ("Indiana" . "in") ("Iowa" . "ia")
    ("Kansas" . "ks") ("Kentucky" . "ky") ("Louisiana" . "la") ("Maine" . "me")
    ("Maryland" . "md") ("Massachusetts" . "ma") ("Michigan" . "mi")
    ("Minnesota" . "mn") ("Mississippi" . "ms") ("Missouri" . "mo")
    ("Montana" . "mt") ("Nebraska" . "ne") ("Nevada" . "nv")
    ("New Hampshire" . "nh") ("New Jersey" . "nj") ("New Mexico" . "nm")
    ("New York" . "ny") ("North Carolina" . "nc") ("North Dakota" . "nd")
    ("Ohio" . "oh") ("Oklahoma" . "ok") ("Oregon" . "or")
    ("Pennsylvania" . "pa") ("Rhode Island" . "ri") ("South Carolina" . "sc")
    ("South Dakota" . "sd") ("Tennessee" . "tn") ("Texas" . "tx")
    ("Utah" . "ut") ("Vermont" . "vt") ("Virginia" . "va")
    ("Washington" . "wa") ("West Virginia" . "wv") ("Wisconsin" . "wi")
    ("Wyoming" . "wy")))

;;------------------------------------------------------------ Option Variables

(defvar webjump-sites
  webjump-sample-sites
  "*Hotlist for WebJump.

The hotlist is represented as an association list, with the CAR of each cell
being the name of the Web site, and the CDR being the definition for the URL of
that site.  The URL definition can be a string (the URL), a vector (specifying
a special \"builtin\" which returns a URL), a symbol (name of a function which
returns a URL), or a list (which when `eval'ed yields a URL).

If the URL definition is a vector, then a \"builtin\" is used.  A builtin has a
Lisp-like syntax, with the name as the first element of the vector, and any
arguments as the following elements.  The three current builtins are `name',
which returns the name of the site as the URL, `simple-query', which
returns a URL that is a function of a query entered by the user, and `mirrors',
which allows the user to select from among multiple mirror sites for the same
content.

The first argument to the `simple-query' builtin is a static URL to use if the
user enters a blank query.  The second and third arguments are the prefix and
suffix, respectively, to add to the encoded query the user enters.  This
builtin covers Web sites that have single-string searches with the query
embedded in the URL.

The arguments to the `mirrors' builtin are URLs of mirror sites.

If the symbol of a function is given, then the function will be called with the
Web site name (the one you specified in the CAR of the alist cell) as a
parameter.  This might come in handy for various kludges.

For convenience, if the `http://', `ftp://', or `file://' prefix is missing
from a URL, WebJump will make a guess at what you wanted and prepend it before
submitting the URL.")

;;------------------------------------------------------- Sample Site Functions

(defun webjump-to-iwin (name)
  (let ((prefix "http://iwin.nws.noaa.gov/")
        (state (webjump-read-choice name "state"
                                    (append '(("Puerto Rico" . "pr"))
                                            webjump-state-to-postal-alist))))
    (if state
        (concat prefix "iwin/" state "/"
                (webjump-read-choice name "option"
                                     '(("Hourly Report" . "hourly")
                                       ("State Forecast" . "state")
                                       ("Local Forecast" . "local")
                                       ("Zone Forecast" . "zone")
                                       ("Short-Term Forecast" . "shortterm")
                                       ("Weather Summary" . "summary")
                                       ("Public Information" . "public")
                                       ("Climatic Data" . "climate")
                                       ("Aviation Products" . "aviation")
                                       ("Hydro Products" . "hydro")
                                       ("Special Weather" . "special")
                                       ("Watches and Warnings" . "warnings"))
                                     "zone")
                ".html")
      prefix)))

(defun webjump-to-risks (name)
  (let (issue volume)
    (if (and (setq volume (webjump-read-number (concat name " volume")))
	     (setq issue  (webjump-read-number (concat name " issue"))))
	(format "catless.ncl.ac.uk/Risks/%d.%02d.html" volume issue)
      "catless.ncl.ac.uk/Risks/")))

;;-------------------------------------------------------------- Core Functions

;;;###autoload
(defun webjump ()
  "Jumps to a Web site from a programmable hotlist.

See the documentation for the `webjump-sites' variable for how to customize the
hotlist.

Please submit bug reports and other feedback to the author, Neil W. Van Dyke
<nwv@acm.org>."
  (interactive)
  (let* ((completion-ignore-case t)
	 (item (assoc-string
		(completing-read "WebJump to site: " webjump-sites nil t)
		webjump-sites t))
	 (name (car item))
	 (expr (cdr item)))
    (browse-url (webjump-url-fix
		 (cond ((not expr) "")
		       ((stringp expr) expr)
		       ((vectorp expr) (webjump-builtin expr name))
		       ((listp expr) (eval expr))
		       ((symbolp expr)
			(if (fboundp expr)
			    (funcall expr name)
			  (error "WebJump URL function \"%s\" undefined"
				 expr)))
		       (t (error "WebJump URL expression for \"%s\" invalid"
				 name)))))))

(defun webjump-builtin (expr name)
  (if (< (length expr) 1)
      (error "WebJump URL builtin for \"%s\" empty" name))
  (let ((builtin (aref expr 0)))
    (cond
     ((eq builtin 'mirrors)
      (if (= (length expr) 1)
          (error
           "WebJump URL builtin \"mirrors\" for \"%s\" needs at least 1 arg"
	   name))
      (webjump-choose-mirror name (cdr (append expr nil))))
     ((eq builtin 'name)
      name)
     ((eq builtin 'simple-query)
      (webjump-builtin-check-args expr name 3)
      (webjump-do-simple-query name (aref expr 1) (aref expr 2) (aref expr 3)))
     (t (error "WebJump URL builtin \"%s\" for \"%s\" invalid"
	       builtin name)))))

(defun webjump-builtin-check-args (expr name count)
  (or (= (length expr) (1+ count))
      (error "WebJump URL builtin \"%s\" for \"%s\" needs %d args"
	     (aref expr 0) name count)))

(defun webjump-choose-mirror (name urls)
  (webjump-read-url-choice (concat name " mirror")
                           urls
                           (webjump-mirror-default urls)))

(defun webjump-do-simple-query (name noquery-url query-prefix query-suffix)
  (let ((query (webjump-read-string (concat name " query"))))
    (if query
	(concat query-prefix (webjump-url-encode query) query-suffix)
      noquery-url)))

(defun webjump-mirror-default (urls)
  ;; Note: This should be modified to apply some simple kludges/heuristics to
  ;; pick a site which is likely "close".  As a tie-breaker among candidates
  ;; judged equally desirable, randomness might be used.
  (car urls))

(defun webjump-read-choice (name what choices &optional default)
  (let* ((completion-ignore-case t)
         (choice (completing-read (concat name " " what ": ") choices nil t)))
    (if (webjump-null-or-blank-string-p choice)
        default
      (cdr (assoc choice choices)))))

(defun webjump-read-number (prompt)
  ;; Note: I should make this more robust someday.
  (let ((input (webjump-read-string prompt)))
    (if input (string-to-number input))))

(defun webjump-read-string (prompt)
  (let ((input (read-string (concat prompt ": "))))
    (if (webjump-null-or-blank-string-p input) nil input)))

(defun webjump-read-url-choice (what urls &optional default)
  ;; Note: Convert this to use `webjump-read-choice' someday.
  (let* ((completions (mapcar (function (lambda (n) (cons n n)))
                              urls))
	 (input (completing-read (concat what
                                         ;;(if default " (RET for default)" "")
                                         ": ")
                                 completions
                                 nil
                                 t)))
    (if (webjump-null-or-blank-string-p input)
        default
      (car (assoc input completions)))))

(defun webjump-null-or-blank-string-p (str)
  (or (null str) (string-match "^[ \t]*$" str)))

(defun webjump-url-encode (str)
  (mapconcat (lambda (c)
               (let ((s (char-to-string c)))
                 (cond ((string= s " ") "+")
                       ((string-match "[a-zA-Z_.-/]" s) s)
                       (t (upcase (format "%%%02x" c))))))
             (encode-coding-string str 'utf-8)
             ""))

(defun webjump-url-fix (url)
  (if (webjump-null-or-blank-string-p url)
      ""
    (webjump-url-fix-trailing-slash
     (cond
      ((string-match "^[a-zA-Z]+:" url) url)
      ((string-match "^/" url) (concat "file://" url))
      ((string-match "^\\([^\\./]+\\)" url)
       (concat (if (string= (downcase (match-string 1 url)) "ftp")
		   "ftp"
		 "http")
	       "://"
	       url))
      (t url)))))

(defun webjump-url-fix-trailing-slash (url)
  (if (string-match "^[a-zA-Z]+://[^/]+$" url)
      (concat url "/")
    url))

;;-----------------------------------------------------------------------------

(provide 'webjump)

;;; webjump.el ends here
