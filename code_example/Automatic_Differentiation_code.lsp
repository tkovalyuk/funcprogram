;; Automatic Differentiation code for Common Lisp
;; Author Richard Fateman, November, 2005
;; This is all provided in the context of a Generic Arithmetic Package.
;; Package based in part on code posted on comp.lang.functional newsgroup by
;; Ingvar Mattsson <ing...@cathouse.bofh.se> 09 Oct 2003

;; Modified by Mark Clements <mark...@ki.se> 09 Feb 2017 
;; again modified a little by RJF 
(defpackage :ga ;generic arithmetic
  (:shadow + - / *       ;binary arith
           = /= > < <= >=   ;binary comparisons
           sin cos tan            ;... more trig
           atan asin acos         ;... more inverse trig
           sinh cosh tanh atanh ;... more hyperbolic
           expt log exp sqrt    ;... more exponential, powers
           1- 1+ abs
           )
  (:use :common-lisp))

(in-package :ga)

;;  df structure for f,d: f is function value, and d derivative, default 0
(defstruct (df (:constructor df (f &optional (d 0)))) f d )

;;  print df structures with < , >
(defmethod print-object ((a df) stream)(format stream "<~a, ~a>" (df-f a)(df-d a)))

;;function ARITHMETIC-IDENTITY: When fed an operator and a non-nil
;;argument, it returns a value for unary application. What does (+ a) mean?
;;A nil arg means there were NO operands. What does (+ ) mean.
;;It is used only by defarithmetic, which in turn helps 
;; us to write out + * - / of arbitrary number of args.

(defmacro arithmetic-identity (op arg)
  `(case ,op
    (+ (or ,arg 0))
    (- (if ,arg (two-arg-* -1 ,arg) 0))
    (* (or ,arg 1))
    (/ (or ,arg (error "/ given no arguments")))
    (expt (or ,arg (error "expt given no arguments")))
    (otherwise nil))) ;binary comparisons?

(defun tocl(n)                          ; get corresponding name in cl-user package
  (find-symbol (symbol-name n) :cl-user))

(defmacro defarithmetic (op)
  (let ((two-arg
           (intern (concatenate 'string "TWO-ARG-" (symbol-name op))
                   :ga ))
        (cl-op (tocl op)))
    `(progn
      (defun ,op (&rest args)
         (cond ((null args) (arithmetic-identity ',op nil))
               ((null (cdr args))(arithmetic-identity ',op (car args)))
               (t (reduce (function ,two-arg)
                          (cdr args)
                          :initial-value (car args)))))
      (defgeneric ,two-arg (arg1 arg2))
      (defmethod ,two-arg ((arg1 number) (arg2 number))
        (,cl-op arg1 arg2))
      (compile ',two-arg)
      (compile ',op)
      ',op)))

(defarithmetic +) ;; defines some of + programs. See below for more
(defarithmetic -)
(defarithmetic *)
(defarithmetic /)
(defarithmetic expt)

;; defcomparison helps us generate numeric comparisons: 2 args
;; and n-arg. CL requires they be monotonic. 
;; That is in Lisp, (> 3 2 1) is true.

(defun monotone (op a rest)(or (null rest)
                               (and (funcall op a (car rest))
                                    (monotone op (car rest)(cdr rest)))))

(defmacro defcomparison (op)
  (let ((two-arg (intern (concatenate 'string "TWO-ARG-" 
                                      (symbol-name op))    :ga ))
        (cl-op (tocl op)))
    `(progn
        (defun ,op (&rest args)
         (cond ((null args) (error "~s wanted at least 1 arg"  ',op))
               ((null (cdr args)) t) ;; one arg e.g. (> x) is true
               (t (monotone (function ,two-arg)
                            (car args)
                             (cdr args)))))
           
      (defgeneric ,two-arg (arg1 arg2))
      (defmethod ,two-arg ((arg1 number) (arg2 number)) (,cl-op arg1 arg2))
      (defmethod ,two-arg ((arg1 df) (arg2 df))    (,cl-op (df-f arg1)(df-f arg2)))
      (defmethod ,two-arg ((arg1 number) (arg2 df))(,cl-op arg1(df-f arg2)))
      (defmethod ,two-arg ((arg1 df) (arg2 number))(,cl-op (df-f arg1) arg2 ))
      (compile ',two-arg)
      (compile ',op)
      ',op)))

(defcomparison >) ;;provides ALL the comparison methods
(defcomparison =)
(defcomparison /=)
(defcomparison <)
(defcomparison <=)
(defcomparison >=) ;; that's all

;; extra + methods specific to df
(defmethod ga::two-arg-+ ((a df) (b df)) 
   (df  (cl:+ (df-f a)(df-f b))
             (cl:+ (df-d a)(df-d b))))
(defmethod ga::two-arg-+ ((b df)(a number)) 
  (df  (cl:+ a (df-f b))    (df-d b)))
(defmethod ga::two-arg-+ ((a number)(b df)) 
  (df  (cl:+ a (df-f b))    (df-d b)))

;;extra - methods
(defmethod ga::two-arg-- ((a df) (b df)) 
   (df  (cl:- (df-f a)(df-f b))
        (cl:- (df-d a)(df-d b))))
(defmethod ga::two-arg-- ((b df)(a number)) 
  (df  (cl:-  (df-f b) a)    (df-d b)))
(defmethod ga::two-arg-- ((a number)(b df)) 
  ;;(df  (cl:- a (df-f b))    (df-d (cl:- b))) 2/9/2016 clements
  (df  (cl:- a (df-f b))    (cl:-(df-d  b))))

;;extra * methods
(defmethod ga::two-arg-* ((a df) (b df)) 
  (df  (cl:* (df-f a)(df-f b))
       (cl:+ (cl:* (df-d a) (df-f b)) (cl:* (df-d b) (df-f a)))))
(defmethod ga::two-arg-* 
    ((b df)(a number))  (df  (cl:* a (df-f b))  (cl:* a (df-d b))))
(defmethod ga::two-arg-* 
    ((a number) (b df)) (df  (cl:* a (df-f b))  (cl:* a (df-d b))))

;; extra divide methods
(defmethod ga::two-arg-/  ((u df) (v df)) 
  (df  (cl:/ (df-f u)(df-f v))
            (cl:/ (cl:+ (cl:* -1 (df-f u)(df-d v))
                          (cl:* (df-f v)(df-d u)))
                    (cl:* (df-f v)(df-f v)))))
(defmethod ga::two-arg-/  ((u number) (v df)) 
  (df  (cl:/ u (df-f v))
            (cl:/ (cl:* -1 u (df-d v)) ;fixed 
                    (cl:* (df-f v)(df-f v)))))
(defmethod ga::two-arg-/  ((u df) (v number)) 
  (df  (cl:/ (df-f u) v)
            (cl:/ (df-d u) v)))

;; extra expt methods
(defmethod ga::two-arg-expt  ((u df) (v number))
  (df  (cl:expt (df-f u) v)
            (cl:* v (cl:expt (df-f u) (cl:1- v)) (df-d u))))
(defmethod ga::two-arg-expt ((u df) (v df))
  (let* ((z (cl:expt (df-f u) (df-f v))) ;;z=u^v
         (w   ;;   u(x)^v(x)*(dv*log(u(x))+du*v(x)/u(x)) 
              ;;   = z*(dv*log(u(x))+du*v(x)/u(x))
          (cl:* z (cl:+
                   (cl:* (cl:log (df-f u)) ;log(u)
                         (df-d v))      ;dv
                    (cl:/ (cl:* (df-f v)(df-d u)) ;v*du/ u
                          (df-f u))))))
    (df  z  w)))
(defmethod ga::two-arg-expt ((u number) (v df))
  (let* ((z (cl:expt u (df-f v))) ;;z=u^v
         (w   ;;    z*(dv*LOG(u(x))
          (cl:* z (cl:* (cl:log u) ;log(u)
                         (df-d v)))))
    (df  z  w)))

;; A rule to define rules, a new method for df, the old method for numbers
(defmacro r (op s)
  `(progn
    (defmethod ,op ((a df))
           (df  (,(tocl op) (df-f a))
                     (,(tocl '*) (df-d a) ,(subst '(df-f a) 'x s))))
    (defmethod ,op ((a number)) (,(tocl op) a))))

;; Add rules for every built-in numeric program.
;; Must insert the name in the shadow list too.
;; This is just a sampler.
(r sin (cos x))  ;; provides EVERYTHING ADIL needs about sin
(r cos (* -1 (sin x)))
(r asin (expt (+ 1 (* -1 (expt x 2))) -1/2))
(r acos (* -1 (expt (+ 1 (* -1 (expt x 2))) -1/2)))
(r atan (expt (+ 1 (expt x 2)) -1))
(r sinh (cosh x))
(r cosh (sinh x))
(r tanh (expt (cosh x) -2))
(r atanh (expt (1+ (* -1 (expt x 2))) -1))
(r log (expt x -1))
(r exp (exp x))
(r sqrt (* 1/2 (expt x -1/2)))
(r 1-  1)
(r 1+  1)
(r abs x);; hm does this matter?

(defun re-intern(s p) ;; move expression to :ga package
  (cond ((or (null s)(numberp s)) s)
        ((symbolp s)(intern (symbol-name s) p))
        (t(cl-user::cons (re-intern (car s) p)
                         (re-intern (cdr s) p)))))


(defun fact(x) (if (= x 1) (df 1 0.422784335098d0) (* x (fact (1- x)))))


;; in :ga   (setf one (df  1  1))
;; in :ga   (dotimes (i 10)(print (fact (+ i one))))

(defun s(x) (if (< (abs x) 1.0d-5) x 
              (let ((z (s (* -1/3 x))))
                (-(* 4 (expt z 3))
                  (* 3 z)))))


;; newton iteration  (ni fun guess)
;; usage:  fun is a function of one arg.
;;         guess is an estimate of solution of fun(x)=0
;; output: a new guess. (Not a df structure, just a number

(defun ni (f z) ;one newton step
  (let* ((pt (if (df-p z) z (df z 1)))  ; make sure init point is a df
         (v (funcall f pt))) ;compute f, f'
    (df-f (- pt (/ (df-f v)(df-d v))))))


(defun ni2 (f z) ;one newton step, give more info
  (let* ((pt (if (df-p z) z (df z 1)))
         (v (funcall f pt)))            ;compute f, f' at pt
    (format t "~%v = ~s" v)
  (values
   (df-f (- pt (/ (df-f v)(df-d v)))) ;the next guess
    v)))


(defun run-newt1(f guess &optional (count 18)) ;; Solve f=0
  ;; Look only at the guesses.
  ;; though if the derivative goes to 0, we are stuck
  ;; in the newton step.
  (let ((guesses (list guess))
        (reltol #.(* 100 double-float-epsilon))
        (abstol #.(* 100 least-positive-double-float)))
    (dotimes (i 6)(push (ni f (car guesses))
                        guesses)) ;; make 6 iterations.
    (incf count -6)
    (cond ((< (abs (car guesses)) abstol)
           (car guesses))
          ((< (abs(/ (-(car guesses)(cadr guesses))(car guesses))) reltol)
           (car guesses))
          ((<= count 0)
           (format t "~%newt1 failed to converge; guess =~s" (car guesses))
           (car guesses))
          (t (run-newt1 f (car guesses) count)))))

(defun run-newt2(f guess &key (abstol 1.0d-8) (count 18)) ;; Solve f=0
  ;; Looks only at the residual.
    (dotimes (i count 
               (format t  "~%Newton iteration not convergent after ~s iterations: ~s" count guess))
      (multiple-value-bind
          (newguess v)
          (ni2 f guess)
        (if (< (abs (df-f v)) abstol) (return newguess)
          (setf guess newguess)))))

;;try (run-newt2 'sin 3.0d0)

;; dcomp, AD compiler
;; Richard Fateman 11/2005

(defvar *difprog* nil);; the text of the program
(defvar *bindings* nil) ;; bindings used for program

(defun dcomp (f x p) 
  ;; the main program
  (let ((*difprog* nil)
        (*bindings* nil))
    (multiple-value-bind
        (val dif)
       (dcomp1 f x p)
      (emit `(values ,val ,dif))
      `(let ,*bindings* 
         ,(format nil "~s wrt ~s" f x)
         ,@(nreverse *difprog*)))))

;; Assist in compilation of a function f and its derivative at a point x=p
;; dcomp1 changes *bindings* and *difprog* as it munches on the expression f.

(defun dcomp1 (f x p)
  (declare (special *v*))
  (cond((atom f)
        (cond ((eq f x) (values p 1.0d0))
              ((numberp f)(values f 0.0d0))
              (t (let ((r (make-dif-name f x)))
                   (push r  *bindings*)
                   (emit `(setf ,r 0.0d0)); unnecessary? already bound to 0.0d0
		   (values f r)))))
       (t       (let* ((op (first f))
                       (program (get op 'dopcomp)));otherwise, look up the operator's d/dx
                  (if program
                      (funcall program (cdr f) x p)
                    (error "~% dcomp cannot do ~s" f);; for now
                    )))))
                   
(defun gentempb(r)
  (push (gentemp (string r)) *bindings*)   (car *bindings*))

(defun emit(item)(unless (and (eq (car item) 'setf)
                              (eq (cadr item)(caddr item)))
                   (push item *difprog*)))

;; simple unary  f(x) programs.
;;We define them all with the same template.

(defmacro dr(op s)
  (setf(get op 'dopcomp) ;;define a rule for returning v, d for dcomp
    (dr2 op (treefloat s))))

(defun dr2 (op s) ;; a rule to make rules for dcomp
   (coerce `(lambda(l x p)              
                  (cond ((cdr l)
                         (error "~&too many arguments to ~s: ~s" ',op l))
                        (t (let ((v (gentempb 'f))
                                 (d (gentempb 't)))
                             (multiple-value-bind
                                 (theval thedif)
                                 (dcomp1 (car l) x p)
                               (emit (list 'setf v (list ',op theval)))
                               (emit (list 'setf d (*chk thedif (subst theval 'x ',s)))))
                             (values v d)))))
	   'function))

(defun treefloat(x) ;; convert all numbers to double-floats
                    (cond ((null x) nil)
                          ((numberp x)(coerce x 'double-float))
                          ((atom x) x)
                          (t (mapcar #'treefloat x))))


;; Here's how the rule definition program works.
;; Set up rules.
(dr tan (power (cos x) -2)) 
(dr sin (cos x))
(dr cos (* -1 (sin x)))
(dr asin (power (+ 1 (* -1 (power x 2))) -1/2))
(dr asin (power (+ 1 (* -1 (power x 2))) -1/2))
(dr acos (* -1 (power (+ 1 (* -1 (power x 2))) -1/2)))
(dr atan (power (+ 1 (power x 2)) -1))
(dr sinh (cosh x))
(dr cosh (sinh x))
(dr log (power x -1))
(dr exp (exp x))
(dr sqrt (* 1/2 (power x -1/2)))
;; etc etc

;; Next, functions of several arguments depending on x 
;; These include + - * / expt, power

(setf (get '* 'dopcomp)  '*rule)
(setf (get '+ 'dopcomp)  '+rule)
(setf (get '/ 'dopcomp)  '/rule)
(setf (get '- 'dopcomp)  '-rule)
(setf (get 'power 'dopcomp) 'power-rule)
(setf (get 'expt 'dopcomp)  'expt-rule)

(defun +rule (l x p)
  (let ((valname (gentempb 'f))
        (difname (gentempb t)))
   (multiple-value-bind (v d) (dcomp1 (car l) x p)
                   (emit `(setf ,difname ,d ,valname ,v)))
    (dolist (i (cdr l))(multiple-value-bind (v d) (dcomp1 i x p)
                   (emit `(setf ,difname ,(+chk d difname)))
                   (emit `(setf  ,valname ,(+chk v valname)))))
    (values valname difname)))

(defun -rule (l x p)
  (let ((valname (gentempb 'f))
        (difname (gentempb t)))
    (cond ((cdr l)
           (multiple-value-bind (v d) (dcomp1 (car l) x p)
             (emit `(setf ,difname ,d ,valname ,v)))
           (dolist (i (cdr l))(multiple-value-bind (v d) (dcomp1 i x p)
                                (emit `(setf ,difname (- ,difname ,d)))
                                (emit `(setf  ,valname (- ,valname ,v ))))) )
          ;; just one arg
          (t (multiple-value-bind (v d) (dcomp1 (car l) x p)
               (emit `(setf ,difname (- ,d) ,valname (- ,v))))))
    (values valname difname)))

;; (- x y z) means (+ x (* -1 y) (* -1  z)).  (- x) means (* -1 x)

(defun *rule (l x p)
  (let ((valname (gentempb 'f))
        (difname (gentempb t)))
    (multiple-value-bind (v d) (dcomp1 (car l) x p)
                   (emit `(setf ,difname ,d ,valname ,v)))
    (dolist (i (cdr l))(multiple-value-bind (v d) (dcomp1 i x p)
                   (emit `(setf ,difname ,(+chk (*chk v difname)
                                                (*chk d valname))))
                   (emit `(setf 
                                ,valname ,(*chk v valname)))))
    (values valname difname)))

(defun /rule (l x p)
  (let ((vname (gentempb 'f))
                  (dname (gentempb t)))
              (multiple-value-bind (v d) (dcomp1 (car l) x p)
                (emit `(setf ,dname ,d ,vname ,v)))
              (multiple-value-bind (v d) (dcomp1 (cadr l) x p)
                (emit `(setf ,dname (/ (- (* ,v ,dname)(* ,vname ,d))
                                       (* ,v ,v))
                             ,vname (/ ,vname ,v))))
              (values vname dname)))

(defun power-rule (l x p)
  (cond ((cddr l) (error "~&too many arguments to power: ~s" l))
        ;; now we assume that everything is a-ok
        ;; i.e. power has only two arguments, the second argument
        ;; which is the power to be raised to is independent of x
        ;; so we can use for sqrt, cube-root etc.
        (t  (let ((vname (gentempb 'f))
                  (dname (gentempb t)))
              (multiple-value-bind (v d) (dcomp1 (car l) x p)
                ;;; We could use +chk and *chk in the following emissions
                ;;; but they are really inessential.
                (emit `(setf ,vname (power ,v ,(cadr l))))
                (emit `(setf ,dname (* ,d (power ,v (1- ,(cadr l))) ,(cadr l)))))
              (values vname dname)))))

(defun expt-rule (l x p)
  "this is the general power rule where the exponent could be an 
    arbitrary function of x"
  (cond ((cddr l) (error "~&too many arguments to expt : ~s" l))
        ((numberp (cadr l))(power-rule l x p))
        ;; we had z = (expt f g)
        ;; z' = z*(g*log f)' = z*(g*f'/f + g'*log f)
        (t  (let ((vname (gentempb 'f))
                  (dname (gentempb t)))
              (multiple-value-bind (v d) (dcomp1 (cadr l) x p)
                (emit `(setf ,vname ,v ,dname ,d)))
              (multiple-value-bind (v d) (dcomp1 (car l) x p)
                (emit `(setf ,dname ,(+chk `(/ ,(*chk vname d) ,v)
                                           (*chk dname `(log ,v)))))
                (emit `(setf ,vname (expt ,v ,vname)))
                (emit `(setf ,dname ,(*chk vname dname))))
              (values vname dname)))))


;; the next two programs, "optimize": 
;; generated code so as to not add 0 or mult by 0 or 1
(defun +chk (a b)(cond ((and (numberp a)(= a 0)) b)
                       ((and (numberp b)(= b 0)) a)
                       (t `(+ ,a ,b))))

(defun *chk (a b)(cond ((and (numberp a)(= a 1)) b)
                       ((and (numberp b)(= b 1)) a)
                       ((and (numberp a)(= a 0)) a)
                       ((and (numberp b)(= b 0)) b)
                       (t `(* ,a ,b))))

;;;; this is the main program for compiling

(defun dc (f x &optional (otherargs nil)) 
  ;; produce a program, p(v) ready to go into the compiler to
  ;; compute f(v), f'(v), returning result as a structure, a df
  (let ((*difprog* nil)
        (*bindings* nil)
        (*v* (gentemp "g")))
    (declare (special *v*))
    (multiple-value-bind
        (val dif)
        (dcomp1 f x *v*)
      (emit `(df ,val ,dif))
      `(lambda (,*v* ,@otherargs)
                 ,(format nil "~s wrt ~s" f x)
         ;;; comment out these declares  if you prefer v's type to be unknown
          (declare (double-float ,*v*))
          (declare (optimize (speed 3)(debug 0)(safety 0)))
          ;    (assert (typep ,*v* 'double-float))
         (let ,(mapcar #'(lambda (r)(list r 0d0)) *bindings*)
         (declare (double-float ,@*bindings*))
           ,@(nreverse *difprog*))))))

;; A plausible way to use  dc  is as follows:

(defmacro defdiff (name arglist body) ;; put the pieces together
  (progn
    (setf (get name 'defdiff) name)
  (let ((r (dc body (car arglist) (cdr arglist)))) ;; returns a df.
    `(defun  ,name , (cadr r) ,@(cddr r)))))

;; usage (defdiff f (x) (* x (sin x)))
;; Then you can call (f 3.0d0)

;; a few more pieces to allow inside defdiff: if, progn, setf.
;; we could try for a few more.  Oh, > < = etc. work "automagically."

(defun if-rule (l x p)
  (let ((valname (gentempb 'f))
        (difname (gentempb t)))
    (emit `(multiple-value-setq
               (,valname ,difname)
             ;; assume only variables, not derivatives in condition
             (if ,(subst p x (car l))
                 (funcall (function ,(dc (cadr l) x)) ,p)
               (funcall (function ,(dc (caddr l) x)) ,p))))
    (values valname difname)))

(defun progn-rule(l x p)
  (mapc #'(lambda (k)(dcomp1 k x p)) (butlast l))
  (dcomp1 (car (last l)) x p))

(defun setf-rule(l x p)
  (if (not(symbolp (car l)))(error "dcomp cannot do setf ~s" (car l)))
  (multiple-value-bind (v d)
      (dcomp1 (cadr l) x p)
      (emit `(setf ,(car l) ,v))
      (let ((dname (make-dif-name (car l) x)))
        (push dname *bindings*)
        (emit `(setf ,dname ,d)))
      (values v d)))

(defun make-dif-name(s x) ;s is a symbol. make a new one like s_dif_x
  (intern (concatenate 'string (symbol-name s) "_DIF_" (symbol-name x))))

(setf (get 'if 'dopcomp)     'if-rule)
(setf (get 'progn 'dopcomp)  'progn-rule)
(setf (get 'setf 'dopcomp)   'setf-rule)


;; derivative by complex evaluation..
;;returns function and derivative of f at point x, sortof

(defun fd(f x) (let ((c (funcall f (complex x 1.0d-8))))
   (values (realpart c)(*(imagpart c) 1.0d8))))

(defun niX(f x &optional (h 1.0d-8)) 
  (let ((c (funcall f (complex x h))))
     (- x (* h(/ (realpart c) (imagpart c) )))))

(in-package :ga)


