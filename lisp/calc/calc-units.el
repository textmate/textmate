;;; calc-units.el --- unit conversion functions for Calc

;; Copyright (C) 1990-1993, 2001-2012  Free Software Foundation, Inc.

;; Author: David Gillespie <daveg@synaptics.com>
;; Maintainer: Jay Belanger <jay.p.belanger@gmail.com>

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

;; This file is autoloaded from calc-ext.el.

(require 'calc-ext)
(require 'calc-macs)
(eval-when-compile
  (require 'calc-alg))

;;; Units operations.

;;; Units table last updated 9-Jan-91 by Ulrich Mueller (ulm@vsnhd1.cern.ch)
;;; with some additions by Przemek Klosowski (przemek@rrdstrad.nist.gov)
;;; Updated April 2002 by Jochen Küpper

;;; Updated August 2007, using
;;;     CODATA (http://physics.nist.gov/cuu/Constants/index.html)
;;;     NIST   (http://physics.nist.gov/Pubs/SP811/appenB9.html)
;;;     ESUWM  (Encyclopaedia of Scientific Units, Weights and
;;;             Measures, by François Cardarelli)
;;; All conversions are exact unless otherwise noted.

(defvar math-standard-units
  '( ;; Length
    ( m       nil                    "*Meter" )
    ( in      "254*10^(-2) cm"       "Inch"  nil
              "2.54 cm")
    ( ft      "12 in"                "Foot")
    ( yd      "3 ft"                 "Yard" )
    ( mi      "5280 ft"              "Mile" )
    ( au      "149597870691. m"      "Astronomical Unit" nil
              "149597870691 m (*)")
              ;; (approx) NASA JPL (http://neo.jpl.nasa.gov/glossary/au.html)
    ( lyr     "c yr"                 "Light Year" )
    ( pc      "3.0856775854*10^16 m" "Parsec  (**)" nil
              "3.0856775854 10^16 m (*)") ;; (approx) ESUWM
    ( nmi     "1852 m"               "Nautical Mile" )
    ( fath    "6 ft"                 "Fathom" )
    ( fur     "660 ft"               "Furlong")
    ( mu      "1 um"                 "Micron" )
    ( mil     "(1/1000) in"          "Mil" )
    ( point   "(1/72) in"            "Point  (PostScript convention)" )
    ( Ang     "10^(-10) m"           "Angstrom" )
    ( mfi     "mi+ft+in"             "Miles + feet + inches" )
    ;; TeX lengths
    ( texpt   "(100/7227) in"        "Point  (TeX convention) (**)" )
    ( texpc   "12 texpt"             "Pica  (TeX convention) (**)" )
    ( texbp   "point"                "Big point  (TeX convention) (**)" )
    ( texdd   "(1238/1157) texpt"    "Didot point  (TeX convention) (**)" )
    ( texcc   "12 texdd"             "Cicero  (TeX convention) (**)" )
    ( texsp   "(1/65536) texpt"      "Scaled TeX point (TeX convention) (**)" )

    ;; Area
    ( hect    "10000 m^2"            "*Hectare" )
    ( a       "100 m^2"              "Are")
    ( acre    "(1/640) mi^2"         "Acre" )
    ( b       "10^(-28) m^2"         "Barn" )

    ;; Volume
    ( L       "10^(-3) m^3"          "*Liter" )
    ( l       "L"                    "Liter" )
    ( gal     "4 qt"                 "US Gallon" )
    ( qt      "2 pt"                 "Quart" )
    ( pt      "2 cup"                "Pint (**)" )
    ( cup     "8 ozfl"               "Cup" )
    ( ozfl    "2 tbsp"               "Fluid Ounce" )
    ( floz    "2 tbsp"               "Fluid Ounce" )
    ( tbsp    "3 tsp"                "Tablespoon" )
    ;; ESUWM defines a US gallon as 231 in^3.
    ;; That gives the following exact value for tsp.
    ( tsp     "492892159375*10^(-11) ml" "Teaspoon" nil
              "4.92892159375 ml")
    ( vol     "tsp+tbsp+ozfl+cup+pt+qt+gal" "Gallons + ... + teaspoons" nil
              "tsp+tbsp+ozfl+cup+pt+qt+gal")
    ( galC    "galUK"                "Canadian Gallon" )
    ( galUK   "454609*10^(-5) L"     "UK Gallon" nil
              "4.54609 L") ;; NIST

    ;; Time
    ( s       nil                    "*Second" )
    ( sec     "s"                    "Second" )
    ( min     "60 s"                 "Minute" )
    ( hr      "60 min"               "Hour" )
    ( day     "24 hr"                "Day" )
    ( wk      "7 day"                "Week" )
    ( hms     "wk+day+hr+min+s"      "Hours, minutes, seconds" )
    ( yr      "36525*10^(-2) day"    "Year (Julian)" nil
              "365.25 day")
    ( Hz      "1/s"                  "Hertz" )

    ;; Speed
    ( mph     "mi/hr"                "*Miles per hour" )
    ( kph     "km/hr"                "Kilometers per hour" )
    ( knot    "nmi/hr"               "Knot" )
    ( c       "299792458 m/s"        "Speed of light" ) ;;; CODATA

    ;; Acceleration
    ( ga      "980665*10^(-5) m/s^2" "*\"g\" acceleration" nil
              "9.80665 m / s^2") ;; CODATA

    ;; Mass
    ( g       nil                    "*Gram" )
    ( lb      "16 oz"                "Pound (mass)" )
    ( oz      "28349523125*10^(-9) g" "Ounce (mass)" nil
              "28.349523125 g") ;; ESUWM
    ( ton     "2000 lb"              "Ton" )
    ( tpo     "ton+lb+oz"            "Tons + pounds + ounces (mass)" )
    ( t       "1000 kg"              "Metric ton" )
    ( tonUK   "10160469088*10^(-7) kg" "UK ton" nil
              "1016.0469088 kg") ;; ESUWM
    ( lbt     "12 ozt"               "Troy pound" )
    ( ozt     "311034768*10^(-7) g"        "Troy ounce" nil
              "31.10347680 g") ;; ESUWM, 1/12 exact value for lbt
    ( ct      "(2/10) g"             "Carat" nil
              "0.2 g") ;; ESUWM
    ( u       "1.660538782*10^(-27) kg"    "Unified atomic mass" nil
              "1.660538782 10^-27 kg (*)");;(approx) CODATA

    ;; Force
    ( N       "m kg/s^2"             "*Newton" )
    ( dyn     "10^(-5) N"            "Dyne" )
    ( gf      "ga g"                 "Gram (force)" )
    ( lbf     "ga lb"                "Pound (force)" )
    ( kip     "1000 lbf"             "Kilopound (force)" )
    ( pdl     "138254954376*10^(-12) N" "Poundal" nil
              "0.138254954376 N") ;; ESUWM

    ;; Energy
    ( J       "N m"                  "*Joule" )
    ( erg     "10^(-7) J"            "Erg" )
    ( cal     "41868*10^(-4) J"      "International Table Calorie" nil
              "4.1868 J") ;; NIST
    ( calth   "4184*10^(-3) J"       "Thermochemical Calorie" nil
              "4.184 J") ;; NIST
    ( Cal     "1000 cal"             "Large Calorie")
    ( Btu     "105505585262*10^(-8) J" "International Table Btu" nil
              "1055.05585262 J") ;; ESUWM
    ( eV      "ech V"                "Electron volt" )
    ( ev      "eV"                   "Electron volt" )
    ( therm   "105506000 J"          "EEC therm" )
    ( invcm   "h c/cm"               "Energy in inverse centimeters" )
    ( Kayser  "invcm"                "Kayser (inverse centimeter energy)" )
    ( men     "100/invcm"            "Inverse energy in meters" )
    ( Hzen    "h Hz"                 "Energy in Hertz")
    ( Ken     "k K"                  "Energy in Kelvins")
    ( Wh      "W hr"                 "Watt hour")
    ( Ws      "W s"                  "Watt second")

    ;; Power
    ( W       "J/s"                  "*Watt" )
    ( hp      "550 ft lbf/s"         "Horsepower") ;;ESUWM
    ( hpm     "75 m kgf/s"           "Metric Horsepower") ;;ESUWM

    ;; Temperature
    ( K       nil                    "*Degree Kelvin"     K )
    ( dK      "K"                    "Degree Kelvin"      K )
    ( degK    "K"                    "Degree Kelvin"      K )
    ( dC      "K"                    "Degree Celsius"     C )
    ( degC    "K"                    "Degree Celsius"     C )
    ( dF      "(5/9) K"              "Degree Fahrenheit"  F )
    ( degF    "(5/9) K"              "Degree Fahrenheit"  F )

    ;; Pressure
    ( Pa      "N/m^2"                "*Pascal" )
    ( bar     "10^5 Pa"              "Bar" )
    ( atm     "101325 Pa"            "Standard atmosphere" ) ;; CODATA
    ( Torr    "(1/760) atm"          "Torr")
    ( mHg     "1000 Torr"            "Meter of mercury" )
    ( inHg    "254*10^(-1) mmHg"     "Inch of mercury" nil
              "25.4 mmHg")
    ( inH2O   "2.490889*10^2 Pa"        "Inch of water" nil
              "2.490889 10^2 Pa (*)") ;;(approx) NIST
    ( psi     "lbf/in^2"             "Pounds per square inch" )

    ;; Viscosity
    ( P       "(1/10) Pa s"           "*Poise" )
    ( St      "10^(-4) m^2/s"         "Stokes" )

    ;; Electromagnetism
    ( A       nil                     "*Ampere" )
    ( C       "A s"                   "Coulomb" )
    ( Fdy     "ech Nav"               "Faraday" )
    ( e       "ech"                   "Elementary charge" )
    ( ech     "1.602176487*10^(-19) C"     "Elementary charge" nil
              "1.602176487 10^-19 C (*)") ;;(approx) CODATA
    ( V       "W/A"                   "Volt" )
    ( ohm     "V/A"                   "Ohm" )
    ( Ω       "ohm"                   "Ohm" )
    ( mho     "A/V"                   "Mho" )
    ( S       "A/V"                   "Siemens" )
    ( F       "C/V"                   "Farad" )
    ( H       "Wb/A"                  "Henry" )
    ( T       "Wb/m^2"                "Tesla" )
    ( Gs      "10^(-4) T"             "Gauss" )
    ( Wb      "V s"                   "Weber" )

    ;; Luminous intensity
    ( cd      nil                     "*Candela" )
    ( sb      "10000 cd/m^2"          "Stilb" )
    ( lm      "cd sr"                 "Lumen" )
    ( lx      "lm/m^2"                "Lux" )
    ( ph      "10000 lx"              "Phot" )
    ( fc      "lm/ft^2"               "Footcandle") ;; ESUWM
    ( lam     "10000 lm/m^2"          "Lambert" )
    ( flam    "(1/pi) cd/ft^2"        "Footlambert") ;; ESUWM

    ;; Radioactivity
    ( Bq      "1/s"                    "*Becquerel" )
    ( Ci      "37*10^9 Bq"             "Curie" ) ;; ESUWM
    ( Gy      "J/kg"                   "Gray" )
    ( Sv      "Gy"                     "Sievert" )
    ( R       "258*10^(-6) C/kg"       "Roentgen" ) ;; NIST
    ( rd      "(1/100) Gy"             "Rad" )
    ( rem     "rd"                     "Rem" )

    ;; Amount of substance
    ( mol     nil                      "*Mole" )

    ;; Plane angle
    ( rad     nil                      "*Radian" )
    ( circ    "2 pi rad"               "Full circle" )
    ( rev     "circ"                   "Full revolution" )
    ( deg     "circ/360"               "Degree" )
    ( arcmin  "deg/60"                 "Arc minute" )
    ( arcsec  "arcmin/60"              "Arc second" )
    ( grad    "circ/400"               "Grade" )
    ( rpm     "rev/min"                "Revolutions per minute" )

    ;; Solid angle
    ( sr      nil                      "*Steradian" )

    ;; Other physical quantities
    ;; The values are from CODATA, and are approximate.
    ( h       "6.62606896*10^(-34) J s"     "*Planck's constant" nil
              "6.62606896 10^-34 J s (*)")
    ( hbar    "h / (2 pi)"                  "Planck's constant" ) ;; Exact
    ( mu0     "4 pi 10^(-7) H/m"            "Permeability of vacuum") ;; Exact
    ( μ0      "mu0"                         "Permeability of vacuum") ;; Exact
    ( eps0    "1 / (mu0 c^2)"               "Permittivity of vacuum" )
    ( ε0      "eps0"                        "Permittivity of vacuum" )
    ( G       "6.67428*10^(-11) m^3/(kg s^2)"    "Gravitational constant" nil
              "6.67428 10^-11 m^3/(kg s^2) (*)")
    ( Nav     "6.02214179*10^(23) / mol"    "Avogadro's constant" nil
              "6.02214179 10^23 / mol (*)")
    ( me      "9.10938215*10^(-31) kg"      "Electron rest mass" nil
              "9.10938215 10^-31 kg (*)")
    ( mp      "1.672621637*10^(-27) kg"     "Proton rest mass" nil
              "1.672621637 10^-27 kg (*)")
    ( mn      "1.674927211*10^(-27) kg"     "Neutron rest mass" nil
              "1.674927211 10^-27 kg (*)")
    ( mmu     "1.88353130*10^(-28) kg"      "Muon rest mass" nil
              "1.88353130 10^-28 kg (*)")
    ( mμ      "mmu"                         "Muon rest mass" nil
              "1.88353130 10^-28 kg (*)")
    ( Ryd     "10973731.568527 /m"          "Rydberg's constant" nil
              "10973731.568527 /m (*)")
    ( k       "1.3806504*10^(-23) J/K"      "Boltzmann's constant" nil
              "1.3806504 10^-23 J/K (*)")
    ( alpha   "7.2973525376*10^(-3)"        "Fine structure constant" nil
              "7.2973525376 10^-3 (*)")
    ( α       "alpha"                        "Fine structure constant" nil
              "7.2973525376 10^-3 (*)")
    ( muB     "927.400915*10^(-26) J/T"     "Bohr magneton" nil
              "927.400915 10^-26 J/T (*)")
    ( muN     "5.05078324*10^(-27) J/T"     "Nuclear magneton" nil
              "5.05078324 10^-27 J/T (*)")
    ( mue     "-928.476377*10^(-26) J/T"    "Electron magnetic moment" nil
              "-928.476377 10^-26 J/T (*)")
    ( mup     "1.410606662*10^(-26) J/T"    "Proton magnetic moment" nil
              "1.410606662 10^-26 J/T (*)")
    ( R0      "8.314472 J/(mol K)"          "Molar gas constant" nil
              "8.314472 J/(mol K) (*)")
    ( V0      "22.710981*10^(-3) m^3/mol"   "Standard volume of ideal gas" nil
              "22.710981 10^-3 m^3/mol (*)")
    ;; Logarithmic units
    ( Np      nil    "*Neper")
    ( dB      "(ln(10)/20) Np" "decibel")))


(defvar math-additional-units nil
  "*Additional units table for user-defined units.
Must be formatted like `math-standard-units'.
If you change this, be sure to set `math-units-table' to nil to ensure
that the combined units table will be rebuilt.")

(defvar math-unit-prefixes
  '( ( ?Y  (^ 10 24)  "Yotta"  )
     ( ?Z  (^ 10 21)  "Zetta"  )
     ( ?E  (^ 10 18)  "Exa"    )
     ( ?P  (^ 10 15)  "Peta"   )
     ( ?T  (^ 10 12)  "Tera"   )
     ( ?G  (^ 10 9)   "Giga"   )
     ( ?M  (^ 10 6)   "Mega"   )
     ( ?k  (^ 10 3)   "Kilo"   )
     ( ?K  (^ 10 3)   "Kilo"   )
     ( ?h  (^ 10 2)   "Hecto"  )
     ( ?H  (^ 10 2)   "Hecto"  )
     ( ?D  (^ 10 1)   "Deka"   )
     ( 0   (^ 10 0)    nil     )
     ( ?d  (^ 10 -1)  "Deci"   )
     ( ?c  (^ 10 -2)  "Centi"  )
     ( ?m  (^ 10 -3)  "Milli"  )
     ( ?u  (^ 10 -6)  "Micro"  )
     ( ?μ  (^ 10 -6)  "Micro"  )
     ( ?n  (^ 10 -9)  "Nano"   )
     ( ?p  (^ 10 -12) "Pico"   )
     ( ?f  (^ 10 -15) "Femto"  )
     ( ?a  (^ 10 -18) "Atto"   )
     ( ?z  (^ 10 -21) "zepto"  )
     ( ?y  (^ 10 -24) "yocto"  )))

(defvar math-standard-units-systems
  '( ( base  nil )
     ( si    ( ( g   '(/ (var kg var-kg) 1000) ) ) )
     ( mks   ( ( g   '(/ (var kg var-kg) 1000) ) ) )
     ( cgs   ( ( m   '(* (var cm var-cm) 100 ) ) ) )))

(defvar math-units-table nil
  "Internal units table.
Derived from `math-standard-units' and `math-additional-units'.
Entries are (SYMBOL EXPR DOC-STRING TEMP-TYPE BASE-UNITS).")

(defvar math-units-table-buffer-valid nil)

;;; Units commands.

(defun calc-base-units ()
  (interactive)
  (calc-slow-wrapper
   (let ((calc-autorange-units nil))
     (calc-enter-result 1 "bsun" (math-simplify-units
				  (math-to-standard-units (calc-top-n 1)
							  nil))))))

(defun calc-quick-units ()
  (interactive)
  (calc-slow-wrapper
   (let* ((num (- last-command-event ?0))
	  (pos (if (= num 0) 10 num))
	  (units (calc-var-value 'var-Units))
	  (expr (calc-top-n 1)))
     (unless (and (>= num 0) (<= num 9))
       (error "Bad unit number"))
     (unless (math-vectorp units)
       (error "No \"quick units\" are defined"))
     (unless (< pos (length units))
       (error "Unit number %d not defined" pos))
     (if (math-units-in-expr-p expr nil)
	 (calc-enter-result 1 (format "cun%d" num)
			    (math-convert-units expr (nth pos units)))
       (calc-enter-result 1 (format "*un%d" num)
			  (math-simplify-units
			   (math-mul expr (nth pos units))))))))

(defun math-get-standard-units (expr)
  "Return the standard units in EXPR."
  (math-simplify-units
   (math-extract-units
    (math-to-standard-units expr nil))))

(defun math-get-units (expr)
  "Return the units in EXPR."
  (math-simplify-units
   (math-extract-units expr)))

(defun math-make-unit-string (expr)
  "Return EXPR in string form.
If EXPR is nil, return nil."
  (if expr
      (let ((cexpr (math-compose-expr expr 0)))
        (replace-regexp-in-string
         " / " "/"
         (if (stringp cexpr)
             cexpr
           (math-composition-to-string cexpr))))))

(defvar math-default-units-table
  (make-hash-table :test 'equal)
  "A table storing previously converted units.")

(defun math-get-default-units (expr)
  "Get default units to use when converting the units in EXPR."
  (let* ((units (math-get-units expr))
         (standard-units (math-get-standard-units expr))
         (default-units (gethash
                         standard-units
                         math-default-units-table)))
    (if (equal units (car default-units))
        (math-make-unit-string (cadr default-units))
      (math-make-unit-string (car default-units)))))

(defun math-put-default-units (expr)
  "Put the units in EXPR in the default units table."
  (let ((units (math-get-units expr)))
    (unless (eq units 1)
      (let* ((standard-units (math-get-standard-units expr))
         (default-units (gethash
                         standard-units
                         math-default-units-table)))
        (cond
         ((not default-units)
          (puthash standard-units (list units) math-default-units-table))
         ((not (equal units (car default-units)))
          (puthash standard-units
                   (list units (car default-units))
                   math-default-units-table)))))))


(defun calc-convert-units (&optional old-units new-units)
  (interactive)
  (calc-slow-wrapper
   (let ((expr (calc-top-n 1))
	 (uoldname nil)
	 unew
         units
         defunits)
     (unless (math-units-in-expr-p expr t)
       (let ((uold (or old-units
		       (progn
			 (setq uoldname (read-string "Old units: "))
			 (if (equal uoldname "")
			     (progn
			       (setq uoldname "1")
			       1)
			   (if (string-match "\\` */" uoldname)
			       (setq uoldname (concat "1" uoldname)))
			   (math-read-expr uoldname))))))
	 (when (eq (car-safe uold) 'error)
	   (error "Bad format in units expression: %s" (nth 1 uold)))
	 (setq expr (math-mul expr uold))))
     (unless new-units
       (setq defunits (math-get-default-units expr))
       (setq new-units
             (read-string (concat
                           (if uoldname
                               (concat "Old units: "
                                       uoldname
                                       ", new units")
                            "New units")
                           (if defunits
                               (concat
                                " (default "
                                defunits
                                "): ")
                             ": "))))

       (if (and
            (string= new-units "")
            defunits)
           (setq new-units defunits)))
     (when (string-match "\\` */" new-units)
       (setq new-units (concat "1" new-units)))
     (setq units (math-read-expr new-units))
     (when (eq (car-safe units) 'error)
       (error "Bad format in units expression: %s" (nth 2 units)))
     (math-put-default-units units)
     (let ((unew (math-units-in-expr-p units t))
	   (std (and (eq (car-safe units) 'var)
		     (assq (nth 1 units) math-standard-units-systems))))
       (if std
	   (calc-enter-result 1 "cvun" (math-simplify-units
					(math-to-standard-units expr
								(nth 1 std))))
	 (unless unew
	   (error "No units specified"))
	 (calc-enter-result 1 "cvun"
			    (math-convert-units
			     expr units
			     (and uoldname (not (equal uoldname "1"))))))))))

(defun calc-autorange-units (arg)
  (interactive "P")
  (calc-wrapper
   (calc-change-mode 'calc-autorange-units arg nil t)
   (message (if calc-autorange-units
		"Adjusting target unit prefix automatically"
	      "Using target units exactly"))))

(defun calc-convert-temperature (&optional old-units new-units)
  (interactive)
  (calc-slow-wrapper
   (let ((expr (calc-top-n 1))
	 (uold nil)
	 (uoldname nil)
	 unew
         defunits)
     (setq uold (or old-units
		    (let ((units (math-single-units-in-expr-p expr)))
		      (if units
			  (if (consp units)
			      (list 'var (car units)
				    (intern (concat "var-"
						    (symbol-name
						     (car units)))))
			    (error "Not a pure temperature expression"))
			(math-read-expr
			 (setq uoldname (read-string
					 "Old temperature units: ")))))))
     (when (eq (car-safe uold) 'error)
       (error "Bad format in units expression: %s" (nth 2 uold)))
     (or (math-units-in-expr-p expr nil)
	 (setq expr (math-mul expr uold)))
     (setq defunits (math-get-default-units expr))
     (setq unew (or new-units
                    (read-string
                     (concat
                      (if uoldname
                          (concat "Old temperature units: "
                                  uoldname
                                  ", new units")
                        "New temperature units")
                      (if defunits
                          (concat " (default "
                                  defunits
                                  "): ")
                        ": ")))))
     (setq unew (math-read-expr (if (string= unew "") defunits unew)))
     (when (eq (car-safe unew) 'error)
       (error "Bad format in units expression: %s" (nth 2 unew)))
     (math-put-default-units unew)
     (let ((ntemp (calc-normalize
                   (math-simplify-units
                    (math-convert-temperature expr uold unew
                                              uoldname)))))
       (if (Math-zerop ntemp)
           (setq ntemp (list '* ntemp unew)))
       (let ((calc-simplify-mode 'none))
         (calc-enter-result 1 "cvtm" ntemp))))))

(defun calc-remove-units ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 1 "rmun" (math-simplify-units
				(math-remove-units (calc-top-n 1))))))

(defun calc-extract-units ()
  (interactive)
  (calc-slow-wrapper
   (calc-enter-result 1 "rmun" (math-simplify-units
				(math-extract-units (calc-top-n 1))))))

;; The variables calc-num-units and calc-den-units are local to
;; calc-explain-units, but are used by calc-explain-units-rec,
;; which is called by calc-explain-units.
(defvar calc-num-units)
(defvar calc-den-units)

(defun calc-explain-units ()
  (interactive)
  (calc-wrapper
   (let ((calc-num-units nil)
	 (calc-den-units nil))
     (calc-explain-units-rec (calc-top-n 1) 1)
     (and calc-den-units (string-match "^[^(].* .*[^)]$" calc-den-units)
	  (setq calc-den-units (concat "(" calc-den-units ")")))
     (if calc-num-units
	 (if calc-den-units
	     (message "%s per %s" calc-num-units calc-den-units)
	   (message "%s" calc-num-units))
       (if calc-den-units
	   (message "1 per %s" calc-den-units)
	 (message "No units in expression"))))))

(defun calc-explain-units-rec (expr pow)
  (let ((u (math-check-unit-name expr))
	pos)
    (if (and u (not (math-zerop pow)))
	(let ((name (or (nth 2 u) (symbol-name (car u)))))
	  (if (eq (aref name 0) ?\*)
	      (setq name (substring name 1)))
	  (if (string-match "[^a-zA-Zα-ωΑ-Ω0-9']" name)
	      (if (string-match "^[a-zA-Zα-ωΑ-Ω0-9' ()]*$" name)
		  (while (setq pos (string-match "[ ()]" name))
		    (setq name (concat (substring name 0 pos)
				       (if (eq (aref name pos) 32) "-" "")
				       (substring name (1+ pos)))))
		(setq name (concat "(" name ")"))))
	  (or (eq (nth 1 expr) (car u))
	      (setq name (concat (nth 2 (assq (aref (symbol-name
						     (nth 1 expr)) 0)
					      math-unit-prefixes))
				 (if (and (string-match "[^a-zA-Zα-ωΑ-Ω0-9']" name)
					  (not (memq (car u) '(mHg gf))))
				     (concat "-" name)
				   (downcase name)))))
	  (cond ((or (math-equal-int pow 1)
		     (math-equal-int pow -1)))
		((or (math-equal-int pow 2)
		     (math-equal-int pow -2))
		 (if (equal (nth 4 u) '((m . 1)))
		     (setq name (concat "Square-" name))
		   (setq name (concat name "-squared"))))
		((or (math-equal-int pow 3)
		     (math-equal-int pow -3))
		 (if (equal (nth 4 u) '((m . 1)))
		     (setq name (concat "Cubic-" name))
		   (setq name (concat name "-cubed"))))
		(t
		 (setq name (concat name "^"
				    (math-format-number (math-abs pow))))))
	  (if (math-posp pow)
	      (setq calc-num-units (if calc-num-units
				  (concat calc-num-units " " name)
				name))
	    (setq calc-den-units (if calc-den-units
				(concat calc-den-units " " name)
			      name))))
      (cond ((eq (car-safe expr) '*)
	     (calc-explain-units-rec (nth 1 expr) pow)
	     (calc-explain-units-rec (nth 2 expr) pow))
	    ((eq (car-safe expr) '/)
	     (calc-explain-units-rec (nth 1 expr) pow)
	     (calc-explain-units-rec (nth 2 expr) (- pow)))
	    ((memq (car-safe expr) '(neg + -))
	     (calc-explain-units-rec (nth 1 expr) pow))
	    ((and (eq (car-safe expr) '^)
		  (math-realp (nth 2 expr)))
	     (calc-explain-units-rec (nth 1 expr)
				     (math-mul pow (nth 2 expr))))))))

(defun calc-simplify-units ()
  (interactive)
  (calc-slow-wrapper
   (calc-with-default-simplification
    (calc-enter-result 1 "smun" (math-simplify-units (calc-top-n 1))))))

(defun calc-view-units-table (n)
  (interactive "P")
  (and n (setq math-units-table-buffer-valid nil))
  (let ((win (get-buffer-window "*Units Table*")))
    (if (and win
	     math-units-table
	     math-units-table-buffer-valid)
	(progn
	  (bury-buffer (window-buffer win))
	  (let ((curwin (selected-window)))
	    (select-window win)
	    (switch-to-buffer nil)
	    (select-window curwin)))
      (math-build-units-table-buffer nil))))

(defun calc-enter-units-table (n)
  (interactive "P")
  (and n (setq math-units-table-buffer-valid nil))
  (math-build-units-table-buffer t)
  (message "%s" (substitute-command-keys "Type \\[calc] to return to the Calculator")))

(defun calc-define-unit (uname desc &optional disp)
  (interactive "SDefine unit name: \nsDescription: \nP")
  (if disp (setq disp (read-string "Display definition: ")))
  (calc-wrapper
   (let ((form (calc-top-n 1))
	 (unit (assq uname math-additional-units)))
     (or unit
	 (setq math-additional-units
	       (cons (setq unit (list uname nil nil nil nil))
		     math-additional-units)
	       math-units-table nil))
     (setcar (cdr unit) (and (not (and (eq (car-safe form) 'var)
				       (eq (nth 1 form) uname)))
			     (not (math-equal-int form 1))
			     (math-format-flat-expr form 0)))
     (setcar (cdr (cdr unit)) (and (not (equal desc ""))
				   desc))
     (if disp
         (setcar (cdr (cdr (cdr (cdr unit)))) disp))))
  (calc-invalidate-units-table))

(defun calc-undefine-unit (uname)
  (interactive "SUndefine unit name: ")
  (calc-wrapper
   (let ((unit (assq uname math-additional-units)))
     (or unit
	 (if (assq uname math-standard-units)
	     (error "\"%s\" is a predefined unit name" uname)
	   (error "Unit name \"%s\" not found" uname)))
     (setq math-additional-units (delq unit math-additional-units)
	   math-units-table nil)))
  (calc-invalidate-units-table))

(defun calc-invalidate-units-table ()
  (setq math-units-table nil)
  (let ((buf (get-buffer "*Units Table*")))
    (and buf
	 (with-current-buffer buf
	   (save-excursion
	     (goto-char (point-min))
	     (if (looking-at "Calculator Units Table")
		 (let ((inhibit-read-only t))
		   (insert "(Obsolete) "))))))))

(defun calc-get-unit-definition (uname)
  (interactive "SGet definition for unit: ")
  (calc-wrapper
   (math-build-units-table)
   (let ((unit (assq uname math-units-table)))
     (or unit
	 (error "Unit name \"%s\" not found" uname))
     (let ((msg (nth 2 unit)))
       (if (stringp msg)
	   (if (string-match "^\\*" msg)
	       (setq msg (substring msg 1)))
	 (setq msg (symbol-name uname)))
       (if (nth 1 unit)
	   (progn
	     (calc-enter-result 0 "ugdf" (nth 1 unit))
	     (message "Derived unit: %s" msg))
	 (calc-enter-result 0 "ugdf" (list 'var uname
					   (intern
					    (concat "var-"
						    (symbol-name uname)))))
	 (message "Base unit: %s" msg))))))

(defun calc-permanent-units ()
  (interactive)
  (calc-wrapper
   (let (pos)
     (set-buffer (find-file-noselect (substitute-in-file-name
				      calc-settings-file)))
     (goto-char (point-min))
     (if (and (search-forward ";;; Custom units stored by Calc" nil t)
	      (progn
		(beginning-of-line)
		(setq pos (point))
		(search-forward "\n;;; End of custom units" nil t)))
	 (progn
	   (beginning-of-line)
	   (forward-line 1)
	   (delete-region pos (point)))
       (goto-char (point-max))
       (insert "\n\n")
       (forward-char -1))
     (insert ";;; Custom units stored by Calc on " (current-time-string) "\n")
     (if math-additional-units
	 (progn
	   (insert "(setq math-additional-units '(\n")
	   (let ((list math-additional-units))
	     (while list
	       (insert "  (" (symbol-name (car (car list))) " "
		       (if (nth 1 (car list))
			   (if (stringp (nth 1 (car list)))
			       (prin1-to-string (nth 1 (car list)))
			     (prin1-to-string (math-format-flat-expr
					       (nth 1 (car list)) 0)))
			 "nil")
		       " "
		       (prin1-to-string (nth 2 (car list)))
		       ")\n")
	       (setq list (cdr list))))
	   (insert "))\n"))
       (insert ";;; (no custom units defined)\n"))
     (insert ";;; End of custom units\n")
     (save-buffer))))


;; The variable math-cu-unit-list is local to math-build-units-table,
;; but is used by math-compare-unit-names, which is called (indirectly)
;; by math-build-units-table.
;; math-cu-unit-list is also local to math-convert-units, but is used
;; by math-convert-units-rec, which is called by math-convert-units.
(defvar math-cu-unit-list)

(defun math-build-units-table ()
  (or math-units-table
      (let* ((combined-units (append math-additional-units
				     math-standard-units))
	     (math-cu-unit-list (mapcar 'car combined-units))
	     tab)
	(message "Building units table...")
	(setq math-units-table-buffer-valid nil)
	(setq tab (mapcar (function
			   (lambda (x)
			     (list (car x)
				   (and (nth 1 x)
					(if (stringp (nth 1 x))
					    (let ((exp (math-read-plain-expr
							(nth 1 x))))
					      (if (eq (car-safe exp) 'error)
						  (error "Format error in definition of %s in units table: %s"
							 (car x) (nth 2 exp))
						exp))
					  (nth 1 x)))
				   (nth 2 x)
				   (nth 3 x)
				   (and (not (nth 1 x))
					(list (cons (car x) 1)))
                                   (nth 4 x))))
			  combined-units))
	(let ((math-units-table tab))
	  (mapc 'math-find-base-units tab))
	(message "Building units table...done")
	(setq math-units-table tab))))

;; The variables math-fbu-base and math-fbu-entry are local to
;; math-find-base-units, but are used by math-find-base-units-rec,
;; which is called by math-find-base-units.
(defvar math-fbu-base)
(defvar math-fbu-entry)

(defun math-find-base-units (math-fbu-entry)
  (if (eq (nth 4 math-fbu-entry) 'boom)
      (error "Circular definition involving unit %s" (car math-fbu-entry)))
  (or (nth 4 math-fbu-entry)
      (let (math-fbu-base)
	(setcar (nthcdr 4 math-fbu-entry) 'boom)
	(math-find-base-units-rec (nth 1 math-fbu-entry) 1)
	'(or math-fbu-base
	    (error "Dimensionless definition for unit %s" (car math-fbu-entry)))
	(while (eq (cdr (car math-fbu-base)) 0)
	  (setq math-fbu-base (cdr math-fbu-base)))
	(let ((b math-fbu-base))
	  (while (cdr b)
	    (if (eq (cdr (car (cdr b))) 0)
		(setcdr b (cdr (cdr b)))
	      (setq b (cdr b)))))
	(setq math-fbu-base (sort math-fbu-base 'math-compare-unit-names))
	(setcar (nthcdr 4 math-fbu-entry) math-fbu-base)
	math-fbu-base)))

(defun math-compare-unit-names (a b)
  (memq (car b) (cdr (memq (car a) math-cu-unit-list))))

(defun math-find-base-units-rec (expr pow)
  (let ((u (math-check-unit-name expr)))
    (cond (u
	   (let ((ulist (math-find-base-units u)))
	     (while ulist
	       (let ((p (* (cdr (car ulist)) pow))
		     (old (assq (car (car ulist)) math-fbu-base)))
		 (if old
		     (setcdr old (+ (cdr old) p))
		   (setq math-fbu-base
                         (cons (cons (car (car ulist)) p) math-fbu-base))))
	       (setq ulist (cdr ulist)))))
	  ((math-scalarp expr))
	  ((and (eq (car expr) '^)
		(integerp (nth 2 expr)))
	   (math-find-base-units-rec (nth 1 expr) (* pow (nth 2 expr))))
	  ((eq (car expr) '*)
	   (math-find-base-units-rec (nth 1 expr) pow)
	   (math-find-base-units-rec (nth 2 expr) pow))
	  ((eq (car expr) '/)
	   (math-find-base-units-rec (nth 1 expr) pow)
	   (math-find-base-units-rec (nth 2 expr) (- pow)))
	  ((eq (car expr) 'neg)
	   (math-find-base-units-rec (nth 1 expr) pow))
	  ((eq (car expr) '+)
	   (math-find-base-units-rec (nth 1 expr) pow))
	  ((eq (car expr) 'var)
	   (or (eq (nth 1 expr) 'pi)
	       (error "Unknown name %s in defining expression for unit %s"
		      (nth 1 expr) (car math-fbu-entry))))
          ((equal expr '(calcFunc-ln 10)))
	  (t (error "Malformed defining expression for unit %s" (car math-fbu-entry))))))


(defun math-units-in-expr-p (expr sub-exprs)
  (and (consp expr)
       (if (eq (car expr) 'var)
	   (math-check-unit-name expr)
	 (and (or sub-exprs
		  (memq (car expr) '(* / ^)))
	      (or (math-units-in-expr-p (nth 1 expr) sub-exprs)
		  (math-units-in-expr-p (nth 2 expr) sub-exprs))))))

(defun math-only-units-in-expr-p (expr)
  (and (consp expr)
       (if (eq (car expr) 'var)
	   (math-check-unit-name expr)
	 (if (memq (car expr) '(* /))
	     (and (math-only-units-in-expr-p (nth 1 expr))
		  (math-only-units-in-expr-p (nth 2 expr)))
	   (and (eq (car expr) '^)
		(and (math-only-units-in-expr-p (nth 1 expr))
		     (math-realp (nth 2 expr))))))))

(defun math-single-units-in-expr-p (expr)
  (cond ((math-scalarp expr) nil)
	((eq (car expr) 'var)
	 (math-check-unit-name expr))
	((eq (car expr) '*)
	 (let ((u1 (math-single-units-in-expr-p (nth 1 expr)))
	       (u2 (math-single-units-in-expr-p (nth 2 expr))))
	   (or (and u1 u2 'wrong)
	       u1
	       u2)))
	((eq (car expr) '/)
	 (if (math-units-in-expr-p (nth 2 expr) nil)
	     'wrong
	   (math-single-units-in-expr-p (nth 1 expr))))
	(t 'wrong)))

(defun math-check-unit-name (v)
  (and (eq (car-safe v) 'var)
       (or (assq (nth 1 v) (or math-units-table (math-build-units-table)))
	   (let ((name (symbol-name (nth 1 v))))
	     (and (> (length name) 1)
		  (assq (aref name 0) math-unit-prefixes)
		  (or (assq (intern (substring name 1)) math-units-table)
		      (and (eq (aref name 0) ?M)
			   (> (length name) 3)
			   (eq (aref name 1) ?e)
			   (eq (aref name 2) ?g)
			   (assq (intern (substring name 3))
				 math-units-table))))))))

;; The variable math-which-standard is local to math-to-standard-units,
;; but is used by math-to-standard-rec, which is called by
;; math-to-standard-units.
(defvar math-which-standard)

(defun math-to-standard-units (expr math-which-standard)
  (math-to-standard-rec expr))

(defun math-to-standard-rec (expr)
  (if (eq (car-safe expr) 'var)
      (let ((u (math-check-unit-name expr))
	    (base (nth 1 expr)))
	(if u
	    (progn
	      (if (nth 1 u)
		  (setq expr (math-to-standard-rec (nth 1 u)))
		(let ((st (assq (car u) math-which-standard)))
		  (if st
		      (setq expr (nth 1 st))
		    (setq expr (list 'var (car u)
				     (intern (concat "var-"
						     (symbol-name
						      (car u)))))))))
	      (or (null u)
		  (eq base (car u))
		  (setq expr (list '*
				   (nth 1 (assq (aref (symbol-name base) 0)
						math-unit-prefixes))
				   expr)))
	      expr)
	  (if (eq base 'pi)
	      (math-pi)
	    expr)))
    (if (or
         (Math-primp expr)
         (and (eq (car-safe expr) 'calcFunc-subscr)
              (eq (car-safe (nth 1 expr)) 'var)))
	expr
      (cons (car expr)
	    (mapcar 'math-to-standard-rec (cdr expr))))))

(defun math-apply-units (expr units ulist &optional pure)
  (setq expr (math-simplify-units expr))
  (if ulist
      (let ((new 0)
	    value)
	(or (math-numberp expr)
	    (error "Incompatible units"))
	(while (cdr ulist)
	  (setq value (math-div expr (nth 1 (car ulist)))
		value (math-floor (let ((calc-internal-prec
					 (1- calc-internal-prec)))
				    (math-normalize value)))
		new (math-add new (math-mul value (car (car ulist))))
		expr (math-sub expr (math-mul value (nth 1 (car ulist))))
		ulist (cdr ulist)))
	(math-add new (math-mul (math-div expr (nth 1 (car ulist)))
				(car (car ulist)))))
    (if pure
        expr
      (math-simplify-units (list '* expr units)))))

(defvar math-decompose-units-cache nil)
(defun math-decompose-units (units)
  (let ((u (math-check-unit-name units)))
    (and u (eq (car-safe (nth 1 u)) '+)
	 (setq units (nth 1 u))))
  (setq units (calcFunc-expand units))
  (and (eq (car-safe units) '+)
       (let ((entry (list units calc-internal-prec calc-prefer-frac)))
	 (or (equal entry (car math-decompose-units-cache))
	     (let ((ulist nil)
		   (utemp units)
		   qty unit)
	       (while (eq (car-safe utemp) '+)
		 (setq ulist (cons (math-decompose-unit-part (nth 2 utemp))
				   ulist)
		       utemp (nth 1 utemp)))
	       (setq ulist (cons (math-decompose-unit-part utemp) ulist)
		     utemp ulist)
	       (while (setq utemp (cdr utemp))
		 (unless (equal (nth 2 (car utemp)) (nth 2 (car ulist)))
		   (error "Inconsistent units in sum")))
	       (setq math-decompose-units-cache
		     (cons entry
			   (sort ulist
				 (function
				  (lambda (x y)
				    (not (Math-lessp (nth 1 x)
						     (nth 1 y))))))))))
	 (cdr math-decompose-units-cache))))

(defun math-decompose-unit-part (unit)
  (cons unit
	(math-is-multiple (math-simplify-units (math-to-standard-units
						unit nil))
			  t)))

;; The variable math-fcu-u is local to math-find-compatible-unit,
;; but is used by math-find-compatible-rec which is called by
;; math-find-compatible-unit.
(defvar math-fcu-u)

(defun math-find-compatible-unit (expr unit)
  (let ((math-fcu-u (math-check-unit-name unit)))
    (if math-fcu-u
	(math-find-compatible-unit-rec expr 1))))

(defun math-find-compatible-unit-rec (expr pow)
  (cond ((eq (car-safe expr) '*)
	 (or (math-find-compatible-unit-rec (nth 1 expr) pow)
	     (math-find-compatible-unit-rec (nth 2 expr) pow)))
	((eq (car-safe expr) '/)
	 (or (math-find-compatible-unit-rec (nth 1 expr) pow)
	     (math-find-compatible-unit-rec (nth 2 expr) (- pow))))
	((and (eq (car-safe expr) '^)
	      (integerp (nth 2 expr)))
	 (math-find-compatible-unit-rec (nth 1 expr) (* pow (nth 2 expr))))
	(t
	 (let ((u2 (math-check-unit-name expr)))
	   (if (equal (nth 4 math-fcu-u) (nth 4 u2))
	       (cons expr pow))))))

;; The variables math-cu-new-units and math-cu-pure are local to
;; math-convert-units, but are used by math-convert-units-rec,
;; which is called by math-convert-units.
(defvar math-cu-new-units)
(defvar math-cu-pure)

(defun math-convert-units (expr math-cu-new-units &optional math-cu-pure)
  (if (eq (car-safe math-cu-new-units) 'var)
      (let ((unew (assq (nth 1 math-cu-new-units)
                        (math-build-units-table))))
        (if (eq (car-safe (nth 1 unew)) '+)
            (setq math-cu-new-units (nth 1 unew)))))
  (math-with-extra-prec 2
    (let ((compat (and (not math-cu-pure)
                       (math-find-compatible-unit expr math-cu-new-units)))
	  (math-cu-unit-list nil)
	  (math-combining-units nil))
      (if compat
	  (math-simplify-units
	   (math-mul (math-mul (math-simplify-units
				(math-div expr (math-pow (car compat)
							 (cdr compat))))
			       (math-pow math-cu-new-units (cdr compat)))
		     (math-simplify-units
		      (math-to-standard-units
		       (math-pow (math-div (car compat) math-cu-new-units)
				 (cdr compat))
		       nil))))
	(when (setq math-cu-unit-list (math-decompose-units math-cu-new-units))
	  (setq math-cu-new-units (nth 2 (car math-cu-unit-list))))
	(when (eq (car-safe expr) '+)
	  (setq expr (math-simplify-units expr)))
	(if (math-units-in-expr-p expr t)
	    (math-convert-units-rec expr)
	  (math-apply-units (math-to-standard-units
			     (list '/ expr math-cu-new-units) nil)
			    math-cu-new-units math-cu-unit-list math-cu-pure))))))

(defun math-convert-units-rec (expr)
  (if (math-units-in-expr-p expr nil)
      (math-apply-units (math-to-standard-units
                         (list '/ expr math-cu-new-units) nil)
			math-cu-new-units math-cu-unit-list math-cu-pure)
    (if (Math-primp expr)
	expr
      (cons (car expr)
	    (mapcar 'math-convert-units-rec (cdr expr))))))

(defun math-convert-temperature (expr old new &optional pure)
  (let* ((units (math-single-units-in-expr-p expr))
	 (uold (if old
		   (if (or (null units)
			   (equal (nth 1 old) (car units)))
		       (math-check-unit-name old)
		     (error "Inconsistent temperature units"))
		 units))
	 (unew (math-check-unit-name new)))
    (unless (and (consp unew) (nth 3 unew))
      (error "Not a valid temperature unit"))
    (unless (and (consp uold) (nth 3 uold))
      (error "Not a pure temperature expression"))
    (let ((v (car uold)))
      (setq expr (list '/ expr (list 'var v
				     (intern (concat "var-"
						     (symbol-name v)))))))
    (or (eq (nth 3 uold) (nth 3 unew))
	(cond ((eq (nth 3 uold) 'K)
	       (setq expr (list '- expr '(/ 27315 100)))
	       (if (eq (nth 3 unew) 'F)
		   (setq expr (list '+ (list '* expr '(/ 9 5)) 32))))
	      ((eq (nth 3 uold) 'C)
	       (if (eq (nth 3 unew) 'F)
		   (setq expr (list '+ (list '* expr '(/ 9 5)) 32))
		 (setq expr (list '+ expr '(/ 27315 100)))))
	      (t
	       (setq expr (list '* (list '- expr 32) '(/ 5 9)))
	       (if (eq (nth 3 unew) 'K)
		   (setq expr (list '+ expr '(/ 27315 100)))))))
    (if pure
	expr
      (list '* expr new))))



(defun math-simplify-units (a)
  (let ((math-simplifying-units t)
	(calc-matrix-mode 'scalar))
    (math-simplify a)))
(defalias 'calcFunc-usimplify 'math-simplify-units)

;; The function created by math-defsimplify uses the variable
;; math-simplify-expr, and so is used by functions in math-defsimplify
(defvar math-simplify-expr)

(math-defsimplify (+ -)
  (and math-simplifying-units
       (math-units-in-expr-p (nth 1 math-simplify-expr) nil)
       (let* ((units (math-extract-units (nth 1 math-simplify-expr)))
	      (ratio (math-simplify (math-to-standard-units
				     (list '/ (nth 2 math-simplify-expr) units) nil))))
	 (if (math-units-in-expr-p ratio nil)
	     (progn
	       (calc-record-why "*Inconsistent units" math-simplify-expr)
	       math-simplify-expr)
	   (list '* (math-add (math-remove-units (nth 1 math-simplify-expr))
			      (if (eq (car math-simplify-expr) '-)
                                  (math-neg ratio) ratio))
		 units)))))

(math-defsimplify *
  (math-simplify-units-prod))

(defun math-simplify-units-prod ()
  (and math-simplifying-units
       calc-autorange-units
       (Math-realp (nth 1 math-simplify-expr))
       (let* ((num (math-float (nth 1 math-simplify-expr)))
	      (xpon (calcFunc-xpon num))
	      (unitp (cdr (cdr math-simplify-expr)))
	      (unit (car unitp))
	      (pow (if (eq (car math-simplify-expr) '*) 1 -1))
	      u)
	 (and (eq (car-safe unit) '*)
	      (setq unitp (cdr unit)
		    unit (car unitp)))
	 (and (eq (car-safe unit) '^)
	      (integerp (nth 2 unit))
	      (setq pow (* pow (nth 2 unit))
		    unitp (cdr unit)
		    unit (car unitp)))
	 (and (setq u (math-check-unit-name unit))
	      (integerp xpon)
	      (or (< xpon 0)
		  (>= xpon (if (eq (car u) 'm) 1 3)))
	      (let* ((uxpon 0)
		     (pref (if (< pow 0)
			       (reverse math-unit-prefixes)
			     math-unit-prefixes))
		     (p pref)
		     pxpon pname)
		(or (eq (car u) (nth 1 unit))
		    (setq uxpon (* pow
				   (nth 2 (nth 1 (assq
						  (aref (symbol-name
							 (nth 1 unit)) 0)
						  math-unit-prefixes))))))
		(setq xpon (+ xpon uxpon))
		(while (and p
			    (or (memq (car (car p)) '(?d ?D ?h ?H))
				(and (eq (car (car p)) ?c)
				     (not (eq (car u) 'm)))
				(< xpon (setq pxpon (* (nth 2 (nth 1 (car p)))
						       pow)))
				(progn
				  (setq pname (math-build-var-name
					       (if (eq (car (car p)) 0)
						   (car u)
						 (concat (char-to-string
							  (car (car p)))
							 (symbol-name
							  (car u))))))
				  (and (/= (car (car p)) 0)
				       (assq (nth 1 pname)
					     math-units-table)))))
		  (setq p (cdr p)))
		(and p
		     (/= pxpon uxpon)
		     (or (not (eq p pref))
			 (< xpon (+ pxpon (* (math-abs pow) 3))))
		     (progn
		       (setcar (cdr math-simplify-expr)
			       (let ((calc-prefer-frac nil))
				 (calcFunc-scf (nth 1 math-simplify-expr)
					       (- uxpon pxpon))))
		       (setcar unitp pname)
		       math-simplify-expr)))))))

(defvar math-try-cancel-units)

(math-defsimplify /
  (and math-simplifying-units
       (let ((np (cdr math-simplify-expr))
	     (math-try-cancel-units 0)
	     n nn)
	 (setq n (if (eq (car-safe (nth 2 math-simplify-expr)) '*)
		     (cdr (nth 2 math-simplify-expr))
		   (nthcdr 2 math-simplify-expr)))
	 (if (math-realp (car n))
	     (progn
	       (setcar (cdr math-simplify-expr) (math-mul (nth 1 math-simplify-expr)
					    (let ((calc-prefer-frac nil))
					      (math-div 1 (car n)))))
	       (setcar n 1)))
	 (while (eq (car-safe (setq n (car np))) '*)
	   (math-simplify-units-divisor (cdr n) (cdr (cdr math-simplify-expr)))
	   (setq np (cdr (cdr n))))
	 (math-simplify-units-divisor np (cdr (cdr math-simplify-expr)))
	 (if (eq math-try-cancel-units 0)
	     (let* ((math-simplifying-units nil)
		    (base (math-simplify
                           (math-to-standard-units math-simplify-expr nil))))
	       (if (Math-numberp base)
		   (setq math-simplify-expr base))))
	 (if (eq (car-safe math-simplify-expr) '/)
	     (math-simplify-units-prod))
	 math-simplify-expr)))

(defun math-simplify-units-divisor (np dp)
  (let ((n (car np))
	d dd temp)
    (while (eq (car-safe (setq d (car dp))) '*)
      (when (setq temp (math-simplify-units-quotient n (nth 1 d)))
	(setcar np (setq n temp))
	(setcar (cdr d) 1))
      (setq dp (cdr (cdr d))))
    (when (setq temp (math-simplify-units-quotient n d))
      (setcar np (setq n temp))
      (setcar dp 1))))

;; Simplify, e.g., "in / cm" to "2.54" in a units expression.
(defun math-simplify-units-quotient (n d)
  (let ((pow1 1)
	(pow2 1))
    (when (and (eq (car-safe n) '^)
	       (integerp (nth 2 n)))
      (setq pow1 (nth 2 n) n (nth 1 n)))
    (when (and (eq (car-safe d) '^)
	       (integerp (nth 2 d)))
      (setq pow2 (nth 2 d) d (nth 1 d)))
    (let ((un (math-check-unit-name n))
	  (ud (math-check-unit-name d)))
      (and un ud
	   (if (and (equal (nth 4 un) (nth 4 ud))
		    (eq pow1 pow2))
               (if (eq pow1 1)
                   (math-to-standard-units (list '/ n d) nil)
                 (list '^ (math-to-standard-units (list '/ n d) nil) pow1))
	     (let (ud1)
	       (setq un (nth 4 un)
		     ud (nth 4 ud))
	       (while un
		 (setq ud1 ud)
		 (while ud1
		   (and (eq (car (car un)) (car (car ud1)))
			(setq math-try-cancel-units
			      (+ math-try-cancel-units
				 (- (* (cdr (car un)) pow1)
				    (* (cdr (car ud)) pow2)))))
		   (setq ud1 (cdr ud1)))
		 (setq un (cdr un)))
	       nil))))))

(math-defsimplify ^
  (and math-simplifying-units
       (math-realp (nth 2 math-simplify-expr))
       (if (memq (car-safe (nth 1 math-simplify-expr)) '(* /))
	   (list (car (nth 1 math-simplify-expr))
		 (list '^ (nth 1 (nth 1 math-simplify-expr))
                       (nth 2 math-simplify-expr))
		 (list '^ (nth 2 (nth 1 math-simplify-expr))
                       (nth 2 math-simplify-expr)))
	 (math-simplify-units-pow (nth 1 math-simplify-expr)
                                  (nth 2 math-simplify-expr)))))

(math-defsimplify calcFunc-sqrt
  (and math-simplifying-units
       (if (memq (car-safe (nth 1 math-simplify-expr)) '(* /))
	   (list (car (nth 1 math-simplify-expr))
		 (list 'calcFunc-sqrt (nth 1 (nth 1 math-simplify-expr)))
		 (list 'calcFunc-sqrt (nth 2 (nth 1 math-simplify-expr))))
	 (math-simplify-units-pow (nth 1 math-simplify-expr) '(frac 1 2)))))

(math-defsimplify (calcFunc-floor
		   calcFunc-ceil
		   calcFunc-round
		   calcFunc-rounde
		   calcFunc-roundu
		   calcFunc-trunc
		   calcFunc-float
		   calcFunc-frac
		   calcFunc-abs
		   calcFunc-clean)
  (and math-simplifying-units
       (= (length math-simplify-expr) 2)
       (if (math-only-units-in-expr-p (nth 1 math-simplify-expr))
	   (nth 1 math-simplify-expr)
	 (if (and (memq (car-safe (nth 1 math-simplify-expr)) '(* /))
		  (or (math-only-units-in-expr-p
		       (nth 1 (nth 1 math-simplify-expr)))
		      (math-only-units-in-expr-p
		       (nth 2 (nth 1 math-simplify-expr)))))
	     (list (car (nth 1 math-simplify-expr))
		   (cons (car math-simplify-expr)
			 (cons (nth 1 (nth 1 math-simplify-expr))
			       (cdr (cdr math-simplify-expr))))
		   (cons (car math-simplify-expr)
			 (cons (nth 2 (nth 1 math-simplify-expr))
			       (cdr (cdr math-simplify-expr)))))))))

(defun math-simplify-units-pow (a pow)
  (if (and (eq (car-safe a) '^)
	   (math-check-unit-name (nth 1 a))
	   (math-realp (nth 2 a)))
      (list '^ (nth 1 a) (math-mul pow (nth 2 a)))
    (let* ((u (math-check-unit-name a))
	   (pf (math-to-simple-fraction pow))
	   (d (and (eq (car-safe pf) 'frac) (nth 2 pf))))
      (and u d
	   (math-units-are-multiple u d)
	   (list '^ (math-to-standard-units a nil) pow)))))


(defun math-units-are-multiple (u n)
  (setq u (nth 4 u))
  (while (and u (= (% (cdr (car u)) n) 0))
    (setq u (cdr u)))
  (null u))

(math-defsimplify calcFunc-sin
  (and math-simplifying-units
       (math-units-in-expr-p (nth 1 math-simplify-expr) nil)
       (let ((rad (math-simplify-units
		   (math-evaluate-expr
		    (math-to-standard-units (nth 1 math-simplify-expr) nil))))
	     (calc-angle-mode 'rad))
	 (and (eq (car-safe rad) '*)
	      (math-realp (nth 1 rad))
	      (eq (car-safe (nth 2 rad)) 'var)
	      (eq (nth 1 (nth 2 rad)) 'rad)
	      (list 'calcFunc-sin (nth 1 rad))))))

(math-defsimplify calcFunc-cos
  (and math-simplifying-units
       (math-units-in-expr-p (nth 1 math-simplify-expr) nil)
       (let ((rad (math-simplify-units
		   (math-evaluate-expr
		    (math-to-standard-units (nth 1 math-simplify-expr) nil))))
	     (calc-angle-mode 'rad))
	 (and (eq (car-safe rad) '*)
	      (math-realp (nth 1 rad))
	      (eq (car-safe (nth 2 rad)) 'var)
	      (eq (nth 1 (nth 2 rad)) 'rad)
	      (list 'calcFunc-cos (nth 1 rad))))))

(math-defsimplify calcFunc-tan
  (and math-simplifying-units
       (math-units-in-expr-p (nth 1 math-simplify-expr) nil)
       (let ((rad (math-simplify-units
		   (math-evaluate-expr
		    (math-to-standard-units (nth 1 math-simplify-expr) nil))))
	     (calc-angle-mode 'rad))
	 (and (eq (car-safe rad) '*)
	      (math-realp (nth 1 rad))
	      (eq (car-safe (nth 2 rad)) 'var)
	      (eq (nth 1 (nth 2 rad)) 'rad)
	      (list 'calcFunc-tan (nth 1 rad))))))

(math-defsimplify calcFunc-sec
  (and math-simplifying-units
       (math-units-in-expr-p (nth 1 math-simplify-expr) nil)
       (let ((rad (math-simplify-units
		   (math-evaluate-expr
		    (math-to-standard-units (nth 1 math-simplify-expr) nil))))
	     (calc-angle-mode 'rad))
	 (and (eq (car-safe rad) '*)
	      (math-realp (nth 1 rad))
	      (eq (car-safe (nth 2 rad)) 'var)
	      (eq (nth 1 (nth 2 rad)) 'rad)
	      (list 'calcFunc-sec (nth 1 rad))))))

(math-defsimplify calcFunc-csc
  (and math-simplifying-units
       (math-units-in-expr-p (nth 1 math-simplify-expr) nil)
       (let ((rad (math-simplify-units
		   (math-evaluate-expr
		    (math-to-standard-units (nth 1 math-simplify-expr) nil))))
	     (calc-angle-mode 'rad))
	 (and (eq (car-safe rad) '*)
	      (math-realp (nth 1 rad))
	      (eq (car-safe (nth 2 rad)) 'var)
	      (eq (nth 1 (nth 2 rad)) 'rad)
	      (list 'calcFunc-csc (nth 1 rad))))))

(math-defsimplify calcFunc-cot
  (and math-simplifying-units
       (math-units-in-expr-p (nth 1 math-simplify-expr) nil)
       (let ((rad (math-simplify-units
		   (math-evaluate-expr
		    (math-to-standard-units (nth 1 math-simplify-expr) nil))))
	     (calc-angle-mode 'rad))
	 (and (eq (car-safe rad) '*)
	      (math-realp (nth 1 rad))
	      (eq (car-safe (nth 2 rad)) 'var)
	      (eq (nth 1 (nth 2 rad)) 'rad)
	      (list 'calcFunc-cot (nth 1 rad))))))


(defun math-remove-units (expr)
  (if (math-check-unit-name expr)
      1
    (if (Math-primp expr)
	expr
      (cons (car expr)
	    (mapcar 'math-remove-units (cdr expr))))))

(defun math-extract-units (expr)
  (if (memq (car-safe expr) '(* /))
      (cons (car expr)
	    (mapcar 'math-extract-units (cdr expr)))
    (if (math-check-unit-name expr) expr 1)))

(defun math-build-units-table-buffer (enter-buffer)
  (if (not (and math-units-table math-units-table-buffer-valid
		(get-buffer "*Units Table*")))
      (let ((buf (get-buffer-create "*Units Table*"))
	    (uptr (math-build-units-table))
	    (calc-language (if (eq calc-language 'big) nil calc-language))
	    (calc-float-format '(float 0))
	    (calc-group-digits nil)
	    (calc-number-radix 10)
            (calc-twos-complement-mode nil)
	    (calc-point-char ".")
	    (std nil)
	    u name shadowed)
	(save-excursion
	  (message "Formatting units table...")
	  (set-buffer buf)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Calculator Units Table:\n\n")
            (insert "(All definitions are exact unless marked with an asterisk (*).)\n\n")
            (insert "Unit    Type  Definition                  Description\n\n")
            (while uptr
              (setq u (car uptr)
                    name (nth 2 u))
              (when (eq (car u) 'm)
                (setq std t))
              (setq shadowed (and std (assq (car u) math-additional-units)))
              (when (and name
                         (> (length name) 1)
                         (eq (aref name 0) ?\*))
                (unless (eq uptr math-units-table)
                  (insert "\n"))
                (setq name (substring name 1)))
              (insert " ")
              (and shadowed (insert "("))
              (insert (symbol-name (car u)))
              (and shadowed (insert ")"))
              (if (nth 3 u)
                  (progn
                    (indent-to 10)
                    (insert (symbol-name (nth 3 u))))
                (or std
                    (progn
                      (indent-to 10)
                      (insert "U"))))
              (indent-to 14)
              (and shadowed (insert "("))
              (if (nth 5 u)
                  (insert (nth 5 u))
                (if (nth 1 u)
                    (insert (math-format-value (nth 1 u) 80))
                  (insert (symbol-name (car u)))))
              (and shadowed (insert ")"))
              (indent-to 41)
              (insert " ")
              (when name
                (insert name))
              (if shadowed
                  (insert " (redefined above)")
                (unless (nth 1 u)
                  (insert " (base unit)")))
              (insert "\n")
              (setq uptr (cdr uptr)))
            (insert "\n\nUnit Prefix Table:\n\n")
            (setq uptr math-unit-prefixes)
            (while uptr
              (setq u (car uptr))
              (insert " " (char-to-string (car u)))
              (if (equal (nth 1 u) (nth 1 (nth 1 uptr)))
                  (insert " " (char-to-string (car (car (setq uptr (cdr uptr)))))
                          "   ")
                (insert "     "))
              (insert "10^" (int-to-string (nth 2 (nth 1 u))))
              (indent-to 15)
              (insert "   " (nth 2 u) "\n")
              (while (eq (car (car (setq uptr (cdr uptr)))) 0)))
            (insert "\n\n")
            (insert "(**) When in TeX or LaTeX display mode, the TeX specific unit\n"
                     "names will not use the `tex' prefix; the unit name for a\n"
                     "TeX point will be `pt' instead of `texpt', for example.\n"
                     "To avoid conflicts, the unit names for pint and parsec will\n"
                     "be `pint' and `parsec' instead of `pt' and `pc'."))
	  (view-mode)
	  (message "Formatting units table...done"))
	(setq math-units-table-buffer-valid t)
	(let ((oldbuf (current-buffer)))
	  (set-buffer buf)
	  (goto-char (point-min))
	  (set-buffer oldbuf))
	(if enter-buffer
	    (pop-to-buffer buf)
	  (display-buffer buf)))
    (if enter-buffer
	(pop-to-buffer (get-buffer "*Units Table*"))
      (display-buffer (get-buffer "*Units Table*")))))

;;; Logarithmic units functions

(defvar math-logunits '((var dB var-dB)
                        (var Np var-Np)))

(defun math-conditional-apply (fn &rest args)
  "Evaluate f(args) unless in symbolic mode.
In symbolic mode, return the list (fn args)."
  (if calc-symbolic-mode
      (cons fn args)
    (apply fn args)))

(defun math-conditional-pow (a b)
  "Evaluate a^b unless in symbolic mode.
In symbolic mode, return the list (^ a b)."
  (if calc-symbolic-mode
      (list '^ a b)
    (math-pow a b)))

(defun math-extract-logunits (expr)
  (if (memq (car-safe expr) '(* /))
      (cons (car expr)
	    (mapcar 'math-extract-logunits (cdr expr)))
    (if (memq (car-safe expr) '(^))
        (list '^ (math-extract-logunits (nth 1 expr)) (nth 2 expr))
      (if (member expr math-logunits) expr 1))))

(defun math-logunits-add (a b neg power)
  (let ((aunit (math-simplify (math-extract-logunits a))))
    (if (not (eq (car-safe aunit) 'var))
        (calc-record-why "*Improper logarithmic unit" aunit)
      (let* ((units (math-extract-units a))
            (acoeff (math-simplify (math-remove-units a)))
            (bcoeff (math-simplify (math-to-standard-units
                                    (list '/ b units) nil))))
        (if (math-units-in-expr-p bcoeff nil)
            (calc-record-why "*Inconsistent units" nil)
          (if (and neg
                   (or (math-lessp acoeff bcoeff)
                       (math-equal acoeff bcoeff)))
              (calc-record-why "*Improper coefficients" nil)
            (math-mul
             (if (equal aunit '(var dB var-dB))
                 (let ((coef (if power 10 20)))
                   (math-mul coef
                             (math-conditional-apply 'calcFunc-log10
                              (if neg
                                  (math-sub
                                   (math-conditional-pow 10 (math-div acoeff coef))
                                   (math-conditional-pow 10 (math-div bcoeff coef)))
                                (math-add
                                 (math-conditional-pow 10 (math-div acoeff coef))
                                 (math-conditional-pow 10 (math-div bcoeff coef)))))))
               (let ((coef (if power 2 1)))
                 (math-div
                  (math-conditional-apply 'calcFunc-ln
                   (if neg
                       (math-sub
                        (math-conditional-apply 'calcFunc-exp (math-mul coef acoeff))
                        (math-conditional-apply 'calcFunc-exp (math-mul coef bcoeff)))
                     (math-add
                      (math-conditional-apply 'calcFunc-exp (math-mul coef acoeff))
                      (math-conditional-apply 'calcFunc-exp (math-mul coef bcoeff)))))
                  coef)))
             units)))))))

(defun calcFunc-lufadd (a b)
  (math-logunits-add a b nil nil))

(defun calcFunc-lupadd (a b)
  (math-logunits-add a b nil t))

(defun calcFunc-lufsub (a b)
  (math-logunits-add a b t nil))

(defun calcFunc-lupsub (a b)
  (math-logunits-add a b t t))

(defun calc-lu-plus (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
           (calc-binary-op "lu-" 'calcFunc-lufsub arg)
         (calc-binary-op "lu-" 'calcFunc-lupsub arg))
     (if (calc-is-hyperbolic)
         (calc-binary-op "lu+" 'calcFunc-lufadd arg)
       (calc-binary-op "lu+" 'calcFunc-lupadd arg)))))

(defun calc-lu-minus (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
           (calc-binary-op "lu+" 'calcFunc-lufadd arg)
         (calc-binary-op "lu+" 'calcFunc-lupadd arg))
     (if (calc-is-hyperbolic)
         (calc-binary-op "lu-" 'calcFunc-lufsub arg)
       (calc-binary-op "lu-" 'calcFunc-lupsub arg)))))

(defun math-logunits-mul (a b power)
  (let (logunit coef units number)
    (cond
     ((and
       (setq logunit (math-simplify (math-extract-logunits a)))
       (eq (car-safe logunit) 'var)
       (eq (math-simplify (math-extract-units b)) 1))
      (setq coef (math-simplify (math-remove-units a))
            units (math-extract-units a)
            number b))
     ((and
       (setq logunit (math-simplify (math-extract-logunits b)))
       (eq (car-safe logunit) 'var)
       (eq (math-simplify (math-extract-units a)) 1))
      (setq coef (math-simplify (math-remove-units b))
            units (math-extract-units b)
            number a))
     (t (setq logunit nil)))
    (if logunit
        (cond
         ((equal logunit '(var dB var-dB))
          (math-simplify
           (math-mul
            (math-add
             coef
             (math-mul (if power 10 20)
                       (math-conditional-apply 'calcFunc-log10 number)))
            units)))
         (t
          (math-simplify
           (math-mul
            (math-add
             coef
             (math-div (math-conditional-apply 'calcFunc-ln number) (if power 2 1)))
            units))))
      (calc-record-why "*Improper units" nil))))

(defun math-logunits-divide (a b power)
  (let ((logunit (math-simplify (math-extract-logunits a))))
    (if (not (eq (car-safe logunit) 'var))
        (calc-record-why "*Improper logarithmic unit" logunit)
      (if (math-units-in-expr-p b nil)
          (calc-record-why "*Improper units quantity" b)
        (let* ((units (math-extract-units a))
               (coef (math-simplify (math-remove-units a))))
          (cond
           ((equal logunit '(var dB var-dB))
            (math-simplify
             (math-mul
              (math-sub
               coef
               (math-mul (if power 10 20)
                         (math-conditional-apply 'calcFunc-log10 b)))
              units)))
         (t
          (math-simplify
           (math-mul
            (math-sub
             coef
             (math-div (math-conditional-apply 'calcFunc-ln b) (if power 2 1)))
            units)))))))))

(defun calcFunc-lufmul (a b)
  (math-logunits-mul a b nil))

(defun calcFunc-lupmul (a b)
  (math-logunits-mul a b t))

(defun calc-lu-times (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
           (calc-binary-op "lu/" 'calcFunc-lufdiv arg)
         (calc-binary-op "lu/" 'calcFunc-lupdiv arg))
     (if (calc-is-hyperbolic)
         (calc-binary-op "lu*" 'calcFunc-lufmul arg)
       (calc-binary-op "lu*" 'calcFunc-lupmul arg)))))

(defun calcFunc-lufdiv (a b)
  (math-logunits-divide a b nil))

(defun calcFunc-lupdiv (a b)
  (math-logunits-divide a b t))

(defun calc-lu-divide (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-inverse)
       (if (calc-is-hyperbolic)
           (calc-binary-op "lu*" 'calcFunc-lufmul arg)
         (calc-binary-op "lu*" 'calcFunc-lupmul arg))
     (if (calc-is-hyperbolic)
         (calc-binary-op "lu/" 'calcFunc-lufdiv arg)
       (calc-binary-op "lu/" 'calcFunc-lupdiv arg)))))

(defun math-logunits-quant (val ref power)
  (let* ((units (math-simplify (math-extract-units val)))
         (lunit (math-simplify (math-extract-logunits units))))
    (if (not (eq (car-safe lunit) 'var))
        (calc-record-why "*Improper logarithmic unit" lunit)
      (let ((runits (math-simplify (math-div units lunit)))
            (coeff (math-simplify (math-div val units))))
        (math-mul
         (if (equal lunit '(var dB var-dB))
             (math-mul
              ref
              (math-conditional-pow
               10
               (math-div
                coeff
                (if power 10 20))))
           (math-mul
            ref
            (math-conditional-apply 'calcFunc-exp
             (if power
                 (math-mul 2 coeff)
               coeff))))
         runits)))))

(defvar calc-lu-field-reference)
(defvar calc-lu-power-reference)

(defun calcFunc-lufquant (val &optional ref)
  (unless ref
    (setq ref (math-read-expr calc-lu-field-reference)))
  (math-logunits-quant val ref nil))

(defun calcFunc-lupquant (val &optional ref)
  (unless ref
    (setq ref (math-read-expr calc-lu-power-reference)))
  (math-logunits-quant val ref t))

(defun calc-lu-quant (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-option)
           (calc-binary-op "lupq" 'calcFunc-lufquant arg)
         (calc-unary-op "lupq" 'calcFunc-lufquant arg))
     (if (calc-is-option)
         (calc-binary-op "lufq" 'calcFunc-lupquant arg)
       (calc-unary-op "lufq" 'calcFunc-lupquant arg)))))

(defun math-logunits-level (val ref db power)
  "Compute the value of VAL in decibels or nepers."
      (let* ((ratio (math-simplify-units (math-div val ref)))
             (ratiou (math-simplify-units (math-remove-units ratio)))
             (units (math-simplify (math-extract-units ratio))))
        (math-mul
         (if db
             (math-mul
              (math-mul (if power 10 20)
                        (math-conditional-apply 'calcFunc-log10 ratiou))
              '(var dB var-dB))
           (math-mul
            (math-div (math-conditional-apply 'calcFunc-ln ratiou) (if power 2 1))
            '(var Np var-Np)))
         units)))

(defun calcFunc-dbfield (val &optional ref)
  (unless ref
    (setq ref (math-read-expr calc-lu-field-reference)))
  (math-logunits-level val ref t nil))

(defun calcFunc-dbpower (val &optional ref)
  (unless ref
    (setq ref (math-read-expr calc-lu-power-reference)))
  (math-logunits-level val ref t t))

(defun calcFunc-npfield (val &optional ref)
  (unless ref
    (setq ref (math-read-expr calc-lu-field-reference)))
  (math-logunits-level val ref nil nil))

(defun calcFunc-nppower (val &optional ref)
  (unless ref
    (setq ref (math-read-expr calc-lu-power-reference)))
  (math-logunits-level val ref nil t))

(defun calc-db (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-option)
           (calc-binary-op "ludb" 'calcFunc-dbfield arg)
         (calc-unary-op "ludb" 'calcFunc-dbfield arg))
     (if (calc-is-option)
         (calc-binary-op "ludb" 'calcFunc-dbpower arg)
       (calc-unary-op "ludb" 'calcFunc-dbpower arg)))))

(defun calc-np (arg)
  (interactive "P")
  (calc-slow-wrapper
   (if (calc-is-hyperbolic)
       (if (calc-is-option)
           (calc-binary-op "lunp" 'calcFunc-npfield arg)
         (calc-unary-op "lunp" 'calcFunc-npfield arg))
     (if (calc-is-option)
         (calc-binary-op "lunp" 'calcFunc-nppower arg)
       (calc-unary-op "lunp" 'calcFunc-nppower arg)))))

;;; Musical notes


(defvar calc-note-threshold)

(defun math-midi-round (num)
  "Round NUM to an integer N if NUM is within calc-note-threshold cents of N."
  (let* ((n (math-round num))
         (diff (math-abs
                (math-sub num n))))
    (if (< (math-compare diff
                         (math-div (math-read-expr calc-note-threshold) 100)) 0)
        n
      num)))

(defconst math-notes
  '(((var C var-C) . 0)
    ((var Csharp var-Csharp) . 1)
;    ((var C♯ var-C♯) . 1)
    ((var Dflat var-Dflat) . 1)
;    ((var D♭ var-D♭) . 1)
    ((var D var-D) . 2)
    ((var Dsharp var-Dsharp) . 3)
;    ((var D♯ var-D♯) . 3)
    ((var E var-E) . 4)
    ((var F var-F) . 5)
    ((var Fsharp var-Fsharp) . 6)
;    ((var F♯ var-F♯) . 6)
    ((var Gflat var-Gflat) . 6)
;    ((var G♭ var-G♭) . 6)
    ((var G var-G) . 7)
    ((var Gsharp var-Gsharp) . 8)
;    ((var G♯ var-G♯) . 8)
    ((var A var-A) . 9)
    ((var Asharp var-Asharp) . 10)
;    ((var A♯ var-A♯) . 10)
    ((var Bflat var-Bflat) . 10)
;    ((var B♭ var-B♭) . 10)
    ((var B var-B) . 11))
  "An alist of notes with their number of semitones above C.")

(defun math-freqp (freq)
  "Non-nil if FREQ is a positive number times the unit Hz.
If non-nil, return the coefficient of Hz."
  (let ((freqcoef (math-simplify-units
                   (math-div freq '(var Hz var-Hz)))))
    (if (Math-posp freqcoef) freqcoef)))

(defun math-midip (num)
  "Non-nil if NUM is a possible MIDI note number.
If non-nil, return NUM."
  (if (Math-numberp num) num))

(defun math-spnp (spn)
  "Non-nil if NUM is a scientific pitch note (note + cents).
If non-nil, return a list consisting of the note and the cents coefficient."
  (let (note cents rnote rcents)
    (if (eq (car-safe spn) '+)
        (setq note (nth 1 spn)
              cents (nth 2 spn))
      (setq note spn
            cents nil))
    (cond
     ((and  ;; NOTE is a note, CENTS is nil or cents.
       (eq (car-safe note) 'calcFunc-subscr)
       (assoc (nth 1 note) math-notes)
       (integerp (nth 2 note))
       (setq rnote note)
       (or
        (not cents)
        (Math-numberp (setq rcents
                            (math-simplify
                             (math-div cents '(var cents var-cents)))))))
      (list rnote rcents))
     ((and  ;; CENTS is a note, NOTE is cents.
       (eq (car-safe cents) 'calcFunc-subscr)
       (assoc (nth 1 cents) math-notes)
       (integerp (nth 2 cents))
       (setq rnote cents)
       (or
        (not note)
        (Math-numberp (setq rcents
                            (math-simplify
                             (math-div note '(var cents var-cents)))))))
      (list rnote rcents)))))

(defun math-freq-to-midi (freq)
  "Return the midi note number corresponding to FREQ Hz."
  (let ((midi (math-add
               69
               (math-mul
                12
                (calcFunc-log
                 (math-div freq 440)
                 2)))))
    (math-midi-round midi)))

(defun math-spn-to-midi (spn)
  "Return the MIDI number corresponding to SPN."
  (let* ((note (cdr (assoc (nth 1 (car spn)) math-notes)))
         (octave (math-add (nth 2 (car spn)) 1))
         (cents (nth 1 spn))
         (midi  (math-add
                 (math-mul 12 octave)
                 note)))
    (if cents
        (math-add midi (math-div cents 100))
      midi)))

(defun math-midi-to-spn (midi)
  "Return the scientific pitch notation corresponding to midi number MIDI."
  (let (midin cents)
    (if (math-integerp midi)
        (setq midin midi
              cents nil)
      (setq midin (math-floor midi)
            cents (math-mul 100 (math-sub midi midin))))
    (let* ((nr ;; This should be (math-idivmod midin 12), but with
               ;; better behavior for negative midin.
            (if (Math-negp midin)
                (let ((dm (math-idivmod (math-neg midin) 12)))
                  (if (= (cdr dm) 0)
                      (cons (math-neg (car dm)) 0)
                    (cons
                     (math-sub (math-neg (car dm)) 1)
                     (math-sub 12 (cdr dm)))))
              (math-idivmod midin 12)))
           (n (math-sub (car nr) 1))
           (note (car (rassoc (cdr nr) math-notes))))
      (if cents
          (list '+ (list 'calcFunc-subscr note n)
                   (list '* cents '(var cents var-cents)))
        (list 'calcFunc-subscr note n)))))

(defun math-freq-to-spn (freq)
  "Return the scientific pitch notation corresponding to FREQ Hz."
  (math-with-extra-prec 3
    (math-midi-to-spn (math-freq-to-midi freq))))

(defun math-midi-to-freq (midi)
  "Return the frequency of the note with midi number MIDI."
  (list '*
        (math-mul
         440
         (math-pow
          2
          (math-div
           (math-sub
            midi
            69)
           12)))
        '(var Hz var-Hz)))

(defun math-spn-to-freq (spn)
  "Return the frequency of the note with scientific pitch notation SPN."
  (math-midi-to-freq (math-spn-to-midi spn)))

(defun calcFunc-spn (expr)
  "Return EXPR written as scientific pitch notation + cents."
  ;; Get the coefficient of Hz
  (let (note)
    (cond
     ((setq note (math-freqp expr))
      (math-freq-to-spn note))
     ((setq note (math-midip expr))
      (math-midi-to-spn note))
     ((math-spnp expr)
      expr)
     (t
      (math-reject-arg expr "*Improper expression")))))

(defun calcFunc-midi (expr)
  "Return EXPR written as a MIDI number."
  (let (note)
    (cond
     ((setq note (math-freqp expr))
      (math-freq-to-midi note))
     ((setq note (math-spnp expr))
      (math-spn-to-midi note))
     ((math-midip expr)
      expr)
     (t
      (math-reject-arg expr "*Improper expression")))))

(defun calcFunc-freq (expr)
  "Return the frequency corresponding to EXPR."
  (let (note)
    (cond
     ((setq note (math-midip expr))
      (math-midi-to-freq note))
     ((setq note (math-spnp expr))
      (math-spn-to-freq note))
     ((math-freqp expr)
      expr)
     (t
      (math-reject-arg expr "*Improper expression")))))

(defun calc-freq (arg)
  "Return the frequency corresponding to the expression on the stack."
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "freq" 'calcFunc-freq arg)))

(defun calc-midi (arg)
  "Return the MIDI number corresponding to the expression on the stack."
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "midi" 'calcFunc-midi arg)))

(defun calc-spn (arg)
  "Return the scientific pitch notation corresponding to the expression on the stack."
  (interactive "P")
  (calc-slow-wrapper
   (calc-unary-op "spn" 'calcFunc-spn arg)))


(provide 'calc-units)

;; Local variables:
;; coding: utf-8
;; End:

;;; calc-units.el ends here
