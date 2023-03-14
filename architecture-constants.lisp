;;; Architecture-constants.lisp
;;; 05-Mar-2023 SVS

(in-package :cl-user) ;;; don't put this in a target package because this covers all targets

(require :cheap-patmatch)

(defparameter n-bits-in-word '((:X8664 . 64)
                               (:X8632 . 32)
                               (:ARM32 . 32)
                               (:ARM64 . 64)
                               (:PPC32. 32)
                               (:PPC64 . 64) ;; called nbits-in-word on this platform
                               )
  "Number of bits in a word")
 
(defparameter *arch-keywords* '(:X8664 :X8632 :ARM32 :ARM64 :PPC32 :PPC64))

(defparameter *arch-keywords-to-ignore* '(:ARM64) "Doesn't exist yet.")

(defparameter *arch-pathnames* '((:X8664 . #P"ccl:compiler;X86;X8664;x8664-arch.lisp")
                                 (:X8632 . #P"ccl:compiler;X86;X8632;x8632-arch.lisp")
                                 (:ARM32 . #P"ccl:compiler;ARM;arm-arch.lisp")
                                 ;(:ARM64 . [doesn't exist])
                                 (:PPC32 . #P"ccl:compiler;PPC;PPC32;ppc32-arch.lisp")
                                 (:PPC64 . #P"ccl:compiler;PPC;PPC64;ppc64-arch.lisp")))

(defparameter *def-lines-pattern*
  `(((:zero-or-more cpat:whitep)
         (:one #\()
    (:zero-or-more cpat:whitep)
    (:capture defform
                   (:string "def")
                   (:one-or-more cpat:non-whitep))
     (:one-or-more cpat:whitep)
     (:not (:one-or-more #\,)) ; because sometimes the def occurs in a macro where the named thing starts with a comma
     ))
  "Cheap-patmatch pattern that captures lines that start with '(def'")

(defparameter *single-balanced-paren-matcher*
  `( ; match-parens starts with pos pointing at a #\( and should end with pos pointing one beyond #\)
    (:named match-parens
            (:one-nongreedy #\() ; skip past the #\(
            (:named match-loop
                    (:or (:seq (:lookahead-string "(")
                               match-parens
                               match-loop)
                         (:one-nongreedy #\))
                         (:seq (:one-or-more ,(cpat:any-char-but "()"))
                               match-loop)))))
  "Use this when you know current string position is at #\( and you want it to consume string until balancing #\)")

(defparameter *defconstant-pattern*
  `((:zero-or-more cpat:whitep)
    (:one #\()
    (:zero-or-more cpat:whitep)
    (:capture defform
              (:string "defconstant"))
    (:one-or-more cpat:whitep)
    ;; Not gonna worry about commas here because we assume *def-lines-pattern* has already excluded those
    (:capture defname
              (:one-or-more cpat:non-whitep))
    (:one-or-more cpat:whitep)
    (:capture value
              (:or (:seq (:zero-or-more ,(cpat:any-char-but "("))
                          ,*single-balanced-paren-matcher*)
                   (:one-or-more ,(lambda (char)
                                    (and (cpat:non-whitep char)
                                         (not (char= #\) char)))))))
    (:zero-or-more cpat:whitep)
    (:or (:seq (:capture docstring
                         (:one #\")
                         (:zero-or-more ,(cpat:any-char-but #\")) ; <-- easier syntax than docstring-with-seq1
                         (:one #\"))
               (:zero-or-more cpat:whitep)
               (:one #\)))
         (:one #\)))
    (:zero-or-more cpat:whitep)
    (:capture comment
              (:zero-or-more cpat:any-char))) 
  "Cheap-patmatch pattern that should match one-line defconstant defs.")

(defclass defconstant-lines ()
  ((arch-keyword :initarg :arch-keyword :initform nil :accessor arch-keyword
                 :documentation "One of *arch-keywords*")
   (pathname :initarg :pathname :initform nil :accessor get-pathname)
   (lines :initarg :lines :initform nil :accessor lines))
  (:documentation "A collection of lines for given arch-keyword that start with '(defconstant'."))

(defun get-def-lines-in-file (pathname defstring)
  "Returns all lines in file that start with open-paren followed by defstring.
   Each line will be an alist of (line . file-position) so we can refer
   back to the file as needed."
  (with-open-file (stream pathname :direction :input)
    (loop as pos = (file-position stream)
      and line = (read-line stream nil nil nil)
      while line
      when (and line
                 (plusp (length line))
                 (multiple-value-bind (success? captures)
                                      (cpat:ppatmatch line *def-lines-pattern*)
                   (let ((defform (assoc 'defform captures)))
                     (and success?
                          (cdr defform)
                          (equalp defstring (cdr defform))))))
      collect (cons line pos))))

; (get-def-lines-in-file #P"ccl:compiler;X86;X8664;x8664-arch.lisp" "defconstant")

(defun get-arch-defconstant-lines (arch-keyword)
  "Collect defconstant-lines for one arch-keyword."
  (let ((pathname (cdr (assoc arch-keyword *arch-pathnames*))))
    (cond (pathname
           (make-instance 'defconstant-lines
                            :arch-keyword arch-keyword
                            :pathname pathname
                            :lines (get-def-lines-in-file pathname "defconstant")))
          (t (warn "No definitions filename found for ~S" arch-keyword)))))

; (get-arch-defconstant-lines :X8664)

(defparameter *defconstant-line-objects* nil "Just a cache so we don't have to repeatedly look these up.")

(defun get-defconstant-line-objects ()
  "Collect defconstant-lines objects for all arch-keywords."
  (remove-if #'null
             (mapcar (lambda (arch-keyword)
                       (get-arch-defconstant-lines arch-keyword))
                     *arch-keywords*)))

; (get-defconstant-line-objects) ; get a collection of defconstant lines from various architectures
; Now we need to correlate constant names across various architectures and compare their values

(defun defconstant-line-objects ()
  "Memoized collection of line-objects" 
  (or *defconstant-line-objects*
      (setf *defconstant-line-objects* (get-defconstant-line-objects))))


(defun deconstruct-constant-def (linestring)
  (multiple-value-bind (success? captures)
                       (cpat:ppatmatch linestring *defconstant-pattern*)
    (when success?
      (let ((defname (cdr (assoc 'defname captures)))
            (value (cdr (assoc 'value captures)))
            (comment (cdr (assoc 'comment captures))))
        ; convert values from strings to numbers where possible, since numbers are what we primarily care about
        (unless (or (search "#+" value) ; don't try to parse these things; preserve all information
                    (search "#-" value))
          (let* ((*read-eval* nil)
                 (read-value (ignore-errors (read-from-string value nil nil)))) ; because we might try to read a package designator that's not defined
            (when (numberp read-value)
              (setf value read-value))))
        (values defname value comment)))))

(defclass constant-def ()
  ((cname :initarg :cname :initform nil :accessor cname
          :documentation "This constant's name as a string.")
   (cvalues :initarg :cvalues :initform nil :accessor cvalues
            :documentation "A-list of (arch . value) for this constant def.")
   (ccomments :initarg :ccomments :initform nil :accessor ccomments
              :documentation "A-list of (arch . comments) for this constant def.")
   (line-numbers :initarg :line-numbers :initform nil :accessor line-numbers
                 :documentation "A-list of (arch . line-number) for this constant def.
              (Pathnames of the defs can be found in *arch-pathnames*.)"))
  (:documentation "For every constant name, record an alist of its values, comments, and line numbers
              across all architectures."))

; Collect the names of all the constants in a hash table
(defun get-all-constants ()
  "Collect info about all constants in a hash table and return it."
  (let ((table (make-hash-table :test #'equalp))
        (line-objects (defconstant-line-objects)))
    (dolist (lineob line-objects)
      (dolist (linepair (lines lineob)) ; string for line consed with position
        (multiple-value-bind (defname value comment)
                             (deconstruct-constant-def (car linepair))
          (when defname
            (let ((constant-def (gethash defname table)))
              (unless constant-def
                (setf (gethash defname table)
                      (setf constant-def (make-instance 'constant-def
                                           :cname defname))))
              (let ((arch-keyword (arch-keyword lineob)))
                (push (cons arch-keyword value) (cvalues constant-def))
                (when (and comment
                           (> (length comment) 0))
                  (push (cons arch-keyword comment) (ccomments constant-def)))
                (push (cons arch-keyword (cdr linepair)) (line-numbers constant-def))))))))
    table))


; (get-all-constants) ; Works! Now to turn it into a printed table...

(defun print-table (stream)
  "Calls #'get-all-constants and produces an org-mode table from it"
  (let ((real-arch-keywords (remove-if (lambda (item)
                                         (member item *arch-keywords-to-ignore*))
                                       *arch-keywords*)))
    (labels ((print-hr ()
               ;; horizontal rule
               (format stream "~%|--|") ; skip past first column
               (dolist (kwd real-arch-keywords)
                 (let ((lenk (length (symbol-name kwd))))
                   (dotimes (i (+ 2 lenk))
                     (write-char #\- stream)))
                 (write-char #\| stream)))
             
             (print-header ()
               (format stream "|  ") ; skip past first column
               (dolist (kwd real-arch-keywords)
                 (format stream "| ~A " kwd))
               (format stream "|") ; final |
               (print-hr))
             
             (print-row (constant-def)
               (format stream "~%| ~A " (cname constant-def))
               (dolist (kwd real-arch-keywords)
                 (let ((value (cdr (assoc kwd (cvalues constant-def))))
                       (comment (cdr (assoc kwd (ccomments constant-def)))))
                   (if value
                       (format stream "| ~A ~@[~A~]" value (when comment (string-trim '(#\Space #\Tab #\Return #\Linefeed) comment)))
                       (format stream "|   "))))
               (format stream "|") ; final 
               ;; add a soft row if there are any comments
               #+IGNORE ; no don't make a new row. Too confusing.
               (when (ccomments constant-def) ; when there are any comments at all
                 (format stream "~%|  ") ; skip past first column
                 (dolist (kwd real-arch-keywords)
                   (let ((comment (cdr (assoc kwd (ccomments constant-def)))))
                     (if comment
                         (format stream "| ~A " (string-trim '(#\Space #\Tab #\Return #\Linefeed) comment))
                         (format stream "|   "))))
                 (format stream "|")) ; final |
               (print-hr)))
      
      (let ((table (get-all-constants))
            (sorted-constant-defs nil))
        (setf sorted-constant-defs
              (sort (loop for cdef being each hash-value of table
                      collect cdef)
                    #'string-lessp
                    :key #'cname))
        (print-header)
        
        (dolist (cdef sorted-constant-defs)
          (print-row cdef))))))

; (with-open-file (stream "ccl:constant-table.org" :direction :output :if-exists :supersede) (print-table stream))

; Convert file to markdown via
;  pandoc --from=org --to=gfm "ccl:constant-table.org" > "ccl:constant-table.md"
; See https://francopasut.netlify.app/post/org-to-markdown-gfm/


