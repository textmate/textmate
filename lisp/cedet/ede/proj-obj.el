;;; ede/proj-obj.el --- EDE Generic Project Object code generation support

;;; Copyright (C) 1998-2000, 2005, 2008-2012  Free Software Foundation, Inc.

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: project, make

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
;;
;; Handles a superclass of target types which create object code in
;; and EDE Project file.

(eval-when-compile (require 'cl))
(require 'ede/proj)
(declare-function ede-pmake-varname "ede/pmake")

(defvar ede-proj-objectcode-dodependencies nil
  "Flag specifies to do automatic dependencies.")

;;; Code:
(defclass ede-proj-target-makefile-objectcode (ede-proj-target-makefile)
  (;; Give this a new default
   (configuration-variables :initform ("debug" . (("CFLAGS" . "-g")
						  ("LDFLAGS" . "-g"))))
   ;; @TODO - add an include path.
   (availablecompilers :initform '(ede-gcc-compiler
				   ede-g++-compiler
				   ede-gfortran-compiler
				   ede-gfortran-module-compiler
				   ede-lex-compiler
				   ede-yacc-compiler
				   ;; More C and C++ compilers, plus
				   ;; fortran or pascal can be added here
				   ))
   (availablelinkers :initform '(ede-g++-linker
				 ede-cc-linker
				 ede-ld-linker
				 ede-gfortran-linker
				 ;; Add more linker thingies here.
				 ))
   (sourcetype :initform '(ede-source-c
			   ede-source-c++
			   ede-source-f77
			   ede-source-f90
			   ede-source-lex
			   ede-source-yacc
			   ;; ede-source-other
			   ;; This object should take everything that
			   ;; gets compiled into objects like fortran
			   ;; and pascal.
			   ))
   )
  "Abstract class for Makefile based object code generating targets.
Belonging to this group assumes you could make a .o from an element source
file.")

(defclass ede-object-compiler (ede-compiler)
  ((uselinker :initform t)
   (dependencyvar :initarg :dependencyvar
		  :type list
		  :custom (cons (string :tag "Variable")
				(string :tag "Value"))
		  :documentation
		  "A variable dedicated to dependency generation."))
  "Ede compiler class for source which must compiler, and link.")

;;; C/C++ Compilers and Linkers
;;
(defvar ede-source-c
  (ede-sourcecode "ede-source-c"
		  :name "C"
		  :sourcepattern "\\.c$"
		  :auxsourcepattern "\\.h$"
		  :garbagepattern '("*.o" "*.obj" ".deps/*.P" ".lo"))
  "C source code definition.")

(defvar ede-gcc-compiler
  (ede-object-compiler
   "ede-c-compiler-gcc"
   :name "gcc"
   :dependencyvar '("C_DEPENDENCIES" . "-Wp,-MD,.deps/$(*F).P")
   :variables '(("CC" . "gcc")
		("C_COMPILE" .
		 "$(CC) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)"))
   :rules (list (ede-makefile-rule
		 "c-inference-rule"
		 :target "%.o"
		 :dependencies "%.c"
		 :rules '("@echo '$(C_COMPILE) -c $<'; \\"
			  "$(C_COMPILE) $(C_DEPENDENCIES) -o $@ -c $<"
			  )
		 ))
   :autoconf '("AC_PROG_CC" "AC_PROG_GCC_TRADITIONAL")
   :sourcetype '(ede-source-c)
   :objectextention ".o"
   :makedepends t
   :uselinker t)
  "Compiler for C sourcecode.")

(defvar ede-cc-linker
  (ede-linker
   "ede-cc-linker"
   :name "cc"
   :sourcetype '(ede-source-c)
   :variables  '(("C_LINK" . "$(CC) $(CFLAGS) $(LDFLAGS) -L."))
   :commands '("$(C_LINK) -o $@ $^ $(LDDEPS)")
   :objectextention "")
  "Linker for C sourcecode.")

(defvar ede-source-c++
  (ede-sourcecode "ede-source-c++"
		  :name "C++"
		  :sourcepattern "\\.\\(c\\(pp?\\|c\\|xx\\|++\\)\\|C\\\(PP\\)?\\)$"
		  :auxsourcepattern "\\.\\(hpp?\\|hh?\\|hxx\\|H\\)$"
		  :garbagepattern '("*.o" "*.obj" ".deps/*.P" ".lo"))
  "C++ source code definition.")

(defvar ede-g++-compiler
  (ede-object-compiler
   "ede-c-compiler-g++"
   :name "g++"
   :dependencyvar '("CXX_DEPENDENCIES" . "-Wp,-MD,.deps/$(*F).P")
   :variables '(("CXX" "g++")
		("CXX_COMPILE" .
		 "$(CXX) $(DEFS) $(INCLUDES) $(CPPFLAGS) $(CFLAGS)")
		)
   :rules (list (ede-makefile-rule
		 "c++-inference-rule"
		 :target "%.o"
		 :dependencies "%.cpp"
		 :rules '("@echo '$(CXX_COMPILE) -c $<'; \\"
			  "$(CXX_COMPILE) $(CXX_DEPENDENCIES) -o $@ -c $<"
			  )
		 ))
   :autoconf '("AC_PROG_CXX")
   :sourcetype '(ede-source-c++)
   :objectextention ".o"
   :makedepends t
   :uselinker t)
  "Compiler for C sourcecode.")

(defvar ede-g++-linker
  (ede-linker
   "ede-g++-linker"
   :name "g++"
   ;; Only use this linker when c++ exists.
   :sourcetype '(ede-source-c++)
   :variables  '(("CXX_LINK" . "$(CXX) $(CFLAGS) $(LDFLAGS) -L."))
   :commands '("$(CXX_LINK) -o $@ $^ $(LDDEPS)")
   :autoconf '("AC_PROG_CXX")
   :objectextention "")
  "Linker needed for c++ programs.")

;;; LEX
(defvar ede-source-lex
  (ede-sourcecode "ede-source-lex"
		  :name "lex"
		  :sourcepattern "\\.l\\(l\\|pp\\|++\\)")
  "Lex source code definition.
No garbage pattern since it creates C or C++ code.")

(defvar ede-lex-compiler
  (ede-object-compiler
   "ede-lex-compiler"
   ;; Can we support regular makefiles too??
   :autoconf '("AC_PROG_LEX")
   :sourcetype '(ede-source-lex))
  "Compiler used for Lexical source.")

;;; YACC
(defvar ede-source-yacc
  (ede-sourcecode "ede-source-yacc"
		  :name "yacc"
		  :sourcepattern "\\.y\\(y\\|pp\\|++\\)")
  "Yacc source code definition.
No garbage pattern since it creates C or C++ code.")

(defvar ede-yacc-compiler
  (ede-object-compiler
   "ede-yacc-compiler"
   ;; Can we support regular makefiles too??
   :autoconf '("AC_PROG_YACC")
   :sourcetype '(ede-source-yacc))
  "Compiler used for yacc/bison grammar files source.")

;;; Fortran Compiler/Linker
;;
;; Contributed by David Engster
(defvar ede-source-f90
  (ede-sourcecode "ede-source-f90"
		  :name "Fortran 90/95"
		  :sourcepattern "\\.[fF]9[05]$"
		  :auxsourcepattern "\\.incf$"
		  :garbagepattern '("*.o" "*.mod" ".deps/*.P"))
  "Fortran 90/95 source code definition.")

(defvar ede-source-f77
  (ede-sourcecode "ede-source-f77"
		  :name "Fortran 77"
		  :sourcepattern "\\.\\([fF]\\|for\\)$"
		  :auxsourcepattern "\\.incf$"
		  :garbagepattern '("*.o" ".deps/*.P"))
  "Fortran 77 source code definition.")

(defvar ede-gfortran-compiler
  (ede-object-compiler
   "ede-f90-compiler-gfortran"
   :name "gfortran"
   :dependencyvar '("F90_DEPENDENCIES" . "-Wp,-MD,.deps/$(*F).P")
   :variables '(("F90" . "gfortran")
		("F90_COMPILE" .
		 "$(F90) $(DEFS) $(INCLUDES) $(F90FLAGS)"))
   :rules (list (ede-makefile-rule
		 "f90-inference-rule"
		 :target "%.o"
		 :dependencies "%.f90"
		 :rules '("@echo '$(F90_COMPILE) -c $<'; \\"
			  "$(F90_COMPILE) $(F90_DEPENDENCIES) -o $@ -c $<"
			  )
		 ))
   :sourcetype '(ede-source-f90 ede-source-f77)
   :objectextention ".o"
   :makedepends t
   :uselinker t)
  "Compiler for Fortran sourcecode.")

(defvar ede-gfortran-module-compiler
  (clone ede-gfortran-compiler
	 "ede-f90-module-compiler-gfortran"
	 :name "gfortranmod"
	 :sourcetype '(ede-source-f90)
	 :commands '("$(F90_COMPILE) -c $^")
	 :objectextention ".mod"
	 :uselinker nil)
  "Compiler for Fortran 90/95 modules.")


(defvar ede-gfortran-linker
  (ede-linker
   "ede-gfortran-linker"
   :name "gfortran"
   :sourcetype '(ede-source-f90 ede-source-f77)
   :variables  '(("F90_LINK" . "$(F90) $(CFLAGS) $(LDFLAGS) -L."))
   :commands '("$(F90_LINK) -o $@ $^")
   :objectextention "")
  "Linker needed for Fortran programs.")

;;; Generic Linker
;;
(defvar ede-ld-linker
  (ede-linker
   "ede-ld-linker"
   :name "ld"
   :variables  '(("LD" . "ld")
		 ("LD_LINK" . "$(LD) $(LDFLAGS) -L."))
   :commands '("$(LD_LINK) -o $@ $^ $(LDDEPS)")
   :objectextention "")
  "Linker needed for c++ programs.")

;;; The EDE object compiler
;;
(defmethod ede-proj-makefile-insert-variables ((this ede-object-compiler))
  "Insert variables needed by the compiler THIS."
  (call-next-method)
  (if (eieio-instance-inheritor-slot-boundp this 'dependencyvar)
      (with-slots (dependencyvar) this
	  (insert (car dependencyvar) "=")
	  (let ((cd (cdr dependencyvar)))
	    (if (listp cd)
		(mapc (lambda (c) (insert " " c)) cd)
	      (insert cd))
	    (insert "\n")))))

;;; EDE Object target type methods
;;
(defmethod ede-proj-makefile-sourcevar
  ((this ede-proj-target-makefile-objectcode))
  "Return the variable name for THIS's sources."
  (require 'ede/pmake)
  (concat (ede-pmake-varname this) "_SOURCES"))

(defmethod ede-proj-makefile-dependency-files
  ((this ede-proj-target-makefile-objectcode))
  "Return a list of source files to convert to dependencies.
Argument THIS is the target to get sources from."
  (append (oref this source) (oref this auxsource)))

(defmethod ede-proj-makefile-insert-variables ((this ede-proj-target-makefile-objectcode)
					       &optional moresource)
  "Insert variables needed by target THIS.
Optional argument MORESOURCE is not used."
  (let ((ede-proj-objectcode-dodependencies
	 (oref (ede-target-parent this) automatic-dependencies)))
    (call-next-method)))

(defmethod ede-buffer-header-file((this ede-proj-target-makefile-objectcode)
				  buffer)
  "There are no default header files."
  (or (call-next-method)
      ;; Ok, nothing obvious. Try looking in ourselves.
      (let ((h (oref this auxsource)))
	;; Add more logic here when the problem is better understood.
	(car-safe h))))

(provide 'ede/proj-obj)

;;; ede/proj-obj.el ends here
