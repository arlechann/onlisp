(in-package :cl-user)
(defpackage :onlisp
  (:use :cl))
(in-package :onlisp)

;;;
;;; ユーティリティ関数
;;;

;;; リストに対する操作

(declaim (inline last1 single append1 conc1 mklist))

(defun last1 (ls)
  (car (last ls)))

(defun single (ls)
  (and (consp ls) (not (cdr ls))))

(defun append1 (ls obj)
  (append ls (list obj)))

(defun conc1 (ls obj)
  (nconc ls (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn ls)
  (labels ((rec (ls acc)
             (if (null ls)
                 (nreverse acc)
                 (let ((val (funcall fn (car ls))))
                   (rec (cdr ls) (if val (cons val acc) acc))))))
    (rec ls nil)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

;;; 検索

(defun find2 (fn ls)
  (if (null ls)
      nil
      (let ((val (funcall fn (car ls))))
        (if val
            (values (car ls) val)
            (find2 fn (cdr ls))))))

(defun before (x y ls &key (test #'eql))
  (and ls
       (let ((first (car ls)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) ls)
               (t (before x y (cdr ls) :test test))))))

(defun after (x y ls &key (test #'eql))
  (let ((rest (before y x ls :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj ls &key (test #'eql))
  (member obj (cdr (member obj ls :test test)) :test test))

(defun split-if (fn ls)
  (labels ((rec (src acc)
             (if (or (null src) (funcall fn (car src)))
                 (values (nreverse acc) src)
                 (rec (cdr src) (cons (car src) acc)))))
    (rec ls nil)))

(defun most (fn ls)
  (labels ((rec (ls wins max)
             (if (null ls)
                 (values wins max)
                 (let ((score (funcall fn (car ls))))
                   (if (> score max)
                       (rec (cdr ls) (car ls) score)
                       (rec (cdr ls) wins max))))))
    (if (null ls)
        (values nil nil)
        (rec (cdr ls) (car ls) (funcall fn (car ls))))))

(defun best (fn ls)
  (labels ((rec (ls wins)
             (if (null ls)
                 wins
                 (if (funcall fn (car ls) wins)
                     (rec (cdr ls) (car ls))
                     (rec (cdr ls) wins)))))
    (if (null ls)
        nil
        (rec (cdr ls) (car ls)))))

(defun mostn (fn ls)
  (labels ((rec (ls result max)
             (if (null ls)
                 (values (nreverse result) max)
                 (let ((score (funcall fn (car ls))))
                   (cond ((> score max) (rec (cdr ls) (list (car ls)) score))
                         ((= score max) (rec (cdr ls) (cons (car ls) result) max))
                         (t (rec (cdr ls) result max)))))))
    (if (null ls)
        (values nil nil)
        (rec (cdr ls) (list (car ls)) (funcall fn (car ls))))))

;;; 対応付け

(defun map0-n (fn n)
  (mapa-b fn 0 n 1))

(defun map1-n (fn n)
  (mapa-b fn 1 n 1))

(defun mapa-b (fn a b &optional (step 1))
  (map-> fn
         a
         (lambda (x) (> x b))
         (lambda (x) (+ x step))))

(defun map-> (fn start test succ)
  (labels ((rec (fn start test succ acc)
             (if (funcall test start)
                 (nreverse acc)
                 (rec fn (funcall succ start) test succ (cons (funcall fn start) acc)))))
    (rec fn start test succ nil)))

(defun mappend (fn &rest lss)
  (apply #'append (apply #'mapcar fn lss)))

(defun mapcars (fn &rest lss)
  (labels ((outer-rec (fn lss acc)
             (if (null lss)
                 (nreverse acc)
                 (outer-rec fn (cdr lss) (inner-rec fn (car lss) acc))))
           (inner-rec (fn ls acc)
             (if (null ls)
                 acc
                 (inner-rec fn (cdr ls) (cons (funcall fn (car ls)) acc)))))
    (outer-rec fn lss nil)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             (lambda (&rest args)
               (apply #'rmapcar fn args))
             args)))

;;; 入出力

(defun readlist (&rest args)
  (values (read-from-string
           (concatenate 'string
                        "("
                        (apply #'read-line args)
                        ")"))))

(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
    (let ((in (apply #'prompt args)))
      (if (funcall quit in)
          (return)
          (format *query-io* "~A~%" (funcall fn in))))))


;;; シンボルとストリング

(defun mkstr (&rest args)
  (labels ((rec (ls s)
             (unless (null ls)
               (princ (car ls) s)
               (rec (cdr ls) s))))
    (with-output-to-string (s)
      (rec args s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  (map 'list
       (lambda (c)
         (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))

;;;
;;; 返り値としての関数
;;;

;;; 直交性

(defvar *!equivs* (make-hash-table))

(defun ! (fn)
  (or (gethash fn *!equivs*) fn))

(defun def! (fn fn!)
  (setf (gethash fn *!equivs*) fn!))

;;; 関数の値のメモワイズ

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind (val win) (gethash args cache)
        (if win
            val
            (setf (gethash args cache)
                  (apply fn args)))))))

;;; 関数を合成する

(defun compose (&rest fns)
  (if (null fns)
      #'identity
      (let ((fns (nreverse fns)))
        (lambda (&rest args)
          (reduce (lambda (acc fn) (funcall fn acc))
                  (cdr fns)
                  :initial-value (apply (car fns) args))))))

(defun fif (if then &optional else)
  (lambda (x)
    (if (funcall if x)
        (funcall then x)
        (if else (funcall else x)))))

(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        (lambda (x)
          (and (funcall fn x) (funcall chain x))))))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        (lambda (x)
          (or (funcall fn x) (funcall chain x))))))

;;; Cdr部での再帰

(defun lrec (rec &optional base)
  (labels ((self (ls)
             (if (null ls)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec
                          (car ls)
                          (lambda ()
                            (self (cdr ls)))))))
    #'self))

;;; 部分ツリーでの再帰

(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                          (if (cdr tree)
                              (self (cdr tree)))))))
    #'self))

(defun trec (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec tree
                          (lambda () (self (car tree)))
                          (lambda () (if (cdr tree) (self (cdr tree))))))))
    #'self))

;;;
;;; マクロ
;;;

;;; 逆クォート

(defmacro nil! (var)
  `(setf ,var nil))

(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;;; マクロ展開の確認

(defmacro mac (expr)
  `(pprint (macroexpand-1 ',expr)))

;;;
;;; 古典的なマクロ
;;;

;;; コンテキストの生成

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar (lambda (s)
                   `(,s (gensym)))
                 syms)
     ,@body))

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar (lambda (v) (cons v (gensym)))
                      (remove-duplicates
                       (mapcar #'car
                               (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                ,@body))
       (cond ,@(mapcar (lambda (cl)
                         (condlet-clause vars cl bodfn))
                       clauses)))))

(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar (lambda (bindform)
            (if (consp bindform)
                (cons (cdr (assoc (car bindform) vars))
                      (cdr bindform))))
          (cdr cl)))

;;; 条件付き評価

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?-case)
     (t ,t-case)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar (lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar (lambda (a) `',a) args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar (lambda (c) `(funcall ,fnsym ,c))
                     choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar (lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl))
        (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

;;; 反復

(defmacro forever (&body body)
  `(do ()
       (nil)
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

(defmacro do-tuples/o (parms source &body body)
  (if parms
      (let ((src (gensym)))
        `(prog ((,src ,source))
           (mapc (lambda ,parms ,@body)
             ,@(map0-n (lambda (n)
                         `(nthcdr ,n ,src))
                 (1- (length parms))))))))

(defmacro do-tuples/c (parms source &body body)
  (if parms
      (with-gensyms (src rest bodfn)
        (let ((len (length parms)))
          `(let ((,src ,source))
             (when (nthcdr ,(1- len) ,src)
               (labels ((,bodfn ,parms ,@body))
                 (do ((,rest ,src (cdr ,rest)))
                     ((not (nthcdr ,(1- len) ,rest))
                      ,@(mapcar (lambda (args) `(,bodfn ,@args))
                                (dt-args len rest src))
                      nil)
                   (,bodfn ,@(map1-n (lambda (n) `(nth ,(1- n) ,rest))
                                     len))))))))))

(defun dt-args (len rest src)
  (map0-n (lambda (m)
            (map1-n (lambda (n)
                      (let ((x (+ m n)))
                        (if (>= x len)
                            `(nth ,(- x len) ,src)
                            `(nth ,(1- x) ,rest))))
                    len))
          (- len 2)))

;;; 複数の値に渡る反復

(defmacro mvdo* (parm-cl test-cl &body body)
  (mvdo-gen parm-cl parm-cl test-cl body))

(defun mvdo-gen (binds rebinds test body)
  (if (null binds)
      (let ((label (gensym)))
        `(prog nil
           ,label
           (if ,(car test)
               (return (progn ,@(cdr test))))
           ,@body
           ,@(mvdo-rebind-gen rebinds)
           (go ,label)))
      (let ((rec (mvdo-gen (cdr binds) rebinds test body)))
        (let ((var/s (caar binds)) (expr (cadar binds)))
          (if (atom var/s)
              `(let ((,var/s ,expr)) ,rec)
              `(multiple-value-bind ,var/s ,expr ,rec))))))

(defun mvdo-rebind-gen (rebinds)
  (cond ((null rebinds) nil)
        ((< (length (car rebinds)) 3)
         (mvdo-rebind-gen (cdr rebinds)))
        (t (cons (list (if (atom (caar rebinds))
                           'setq
                           'multiple-value-setq)
                       (caar rebinds)
                       (third (car rebinds)))
                 (mvdo-rebind-gen (cdr rebinds))))))

(defmacro mvpsetq (&rest args)
  (let* ((pairs (group args 2))
         (syms (mapcar (lambda (p)
                         (mapcar (lambda (x) (declare (ignore x)) (gensym))
                                 (mklist (car p))))
                       pairs)))
    (labels ((rec (ps ss)
               (if (null ps)
                   `(setq ,@(mapcan (lambda (p s)
                                      (shuffle (mklist (car p))
                                               s))
                                    pairs syms))
                   (let ((body (rec (cdr ps) (cdr ss))))
                     (let ((var/s (caar ps))
                           (expr (cadar ps)))
                       (if (consp var/s)
                           `(multiple-value-bind ,(car ss)
                                ,expr
                              ,body)
                           `(let ((,@(car ss) ,expr))
                              ,body)))))))
      (rec pairs syms))))

(defun shuffle (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

(defmacro mvdo (binds (test &rest result) &body body)
  (let ((label (gensym))
        (temps (mapcar (lambda (b)
                         (if (listp (car b))
                             (mapcar (lambda (x)
                                       (declare (ignore x))
                                       (gensym))
                                     (car b))
                             (gensym)))
                       binds)))
    `(let ,(mappend #'mklist temps)
       (mvpsetq ,@(mapcan (lambda (b var)
                            (list var (cadr b)))
                          binds
                          temps))
       (prog ,(mapcar (lambda (b var) (list b var))
                (mappend #'mklist (mapcar #'car binds))
                (mappend #'mklist temps))
         ,label
         (if ,test
             (return (progn ,@result)))
         ,@body
         (mvpsetq ,@(mapcan (lambda (b)
                              (if (third b)
                                  (list (car b)
                                        (third b))))
                      binds))
         (go ,label)))))

;;;
;;; 汎変数
;;;

;;; 新しいユーティリティ

(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan (lambda (a) (list a val))
                       args)))))

(defmacro nilf (&rest args) `(allf nil ,@args))

(defmacro tf (&rest args) `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn ,@(mapcar (lambda (a) `(toggle2 ,a))
                    args)))

(define-modify-macro toggle2 () not)

(define-modify-macro concf (obj) nconc)

(define-modify-macro conc1f (obj) conc1)

(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete ,g ,access ,@args)))
         ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
              ,@(mapcar #'list vars forms)
              (,(car var) (delete-if ,g ,access ,@args)))
         ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar (lambda (p)
                          (multiple-value-list
                           (get-setf-expansion p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan (lambda (m)
                              (append (first m)
                                      (third m)))
                            meths)
                    (mapcan (lambda (m)
                              (append (second m)
                                      (list (fifth m))))
                            meths))
       ,@(mapcon (lambda (rest)
                   (mapcar (lambda (arg)
                             `(unless (,op ,(car rest) ,arg)
                                (rotatef ,(car rest) ,arg)))
                           (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))

;;;
;;; コンパイル時の計算処理
;;;

;;; 新しいユーティリティ

(defun nthmost (n ls)
  (nth n (sort (copy-list ls) #'>)))

;;;
;;; アナフォリックマクロ
;;;

;;; アナフォリックな変種オペレータ

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym))
                 (declare (ignorable it))
                 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                 (case (length args)
                   (0 nil)
                   (1 (car args))
                   (t `(let ((it ,(car args)))
                         ,(self (cdr args))))))
               args)))

;;; 失敗

(defmacro aif2 (test &optional then else)
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  `(aif ,test
        (progn ,@body)))

(defmacro awhile2 (test &body body)
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setq ,flag nil))))))

(defmacro acond2 (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (val (gensym))
            (win (gensym)))
        `(multiple-value-bind (,val ,win) ,(car cl1)
           (if (or ,val ,win)
               (let ((it ,val))
                 (declare (ignorable it))
                 ,@(cdr cl1))
               (acond2 ,@(cdr clauses)))))))

(let ((g (gensym)))
  (defun read2 (&optional (str *standard-input*))
    (let ((val (read str nil g)))
      (unless (equal val g) (values val t)))))

(defmacro do-file (filename &body body)
  (let ((str (gensym)))
    `(with-open-file (,str ,filename)
       (awhile2 (read2 ,str)
         ,@body))))

;;;
;;; 関数を返すマクロ
;;;

(defmacro fn (expr) `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar (lambda (f)
                        `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

;;; Cdr部での再帰

(defmacro alrec (rec &optional base)
  (let ((gfn (gensym)))
    `(lrec (lambda (it ,gfn)
             (symbol-macrolet ((rec (funcall ,gfn)))
               ,rec))
           ,base)))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec (lambda () ,base)) ,@lsts))

(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersections it rec) (car sets) (cdr sets))))

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))

;;; 部分ツリーでの再帰

(defmacro atrec (rec &optional (base 'it))
  (let ((lfn (gensym)) (rfn (gensym)))
    `(trec (lambda (it ,lfn ,rfn)
             (symbol-macrolet ((left (funcall ,lfn))
                               (right (funcall ,rfn)))
               ,rec))
           (lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

;;; 遅延評価

(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced nil)))
       (setf (delay-closure ,self)
             (lambda ()
               (setf (delay-forced ,self) t
                     (delay-closure ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (delay-forced x)
          (delay-closure x)
          (funcall (delay-closure x)))
      x))

;;;
;;; マクロを定義するマクロ
;;;

;;; 省略

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defmacro abbrevs (&rest names)
  `(progn ,@(mapcar (lambda (pair)
                      `(abbrev ,@pair))
                    (group names 2))))

;;; 属性

(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn (mapcar (lambda (p) `(propmacro ,p))
                  ,props)))

;;; アナフォリックマクロ

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

(defmacro defanaph (name &optional calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun anaphex1 (args expr)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           (declare (ignorable it))
           ,(anaphex1 (cdr args)
                     (cons sym expr))))
      (reverse expr)))

(defun anaphex2 (op args)
  `(let ((it ,(car args)))
     (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))

;;;
;;; リードマクロ
;;;

;;; マクロ文字のディスパッチング

(set-dispatch-macro-character #\# #\?
                              (lambda (stream char1 char2)
                                (declare (ignore char1 char2))
                                (let ((sym (gensym)))
                                  `(lambda (&rest ,sym)
                                     (declare (ignore ,sym))
                                     ,(read stream t nil t)))))

;;; デリミタ

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right (lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\))))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character
     #\#
     left
     (lambda (stream char1 char2)
       (declare (ignorable stream char1 char2))
       (apply fn
              (read-delimited-list right stream t))))))

(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))

;;;
;;; 構造化代入
;;;

;;; 他の構造

(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,@(dbind-ex (destruc pat gseq #'atom) body))))

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
        (if rest
            `((,rest (subseq ,seq ,n)))
            (let ((p (car pat))
                  (rec (destruc (cdr pat) seq atom? (1+ n))))
              (if (funcall atom? p)
                  (cons `(,p (elt ,seq ,n))
                        rec)
                  (let ((var (gensym)))
                    (cons (cons `(,var (elt ,seq ,n))
                                (destruc p var atom?))
                          rec))))))))

(defun dbind-ex (binds body)
  (if (null binds)
      body
      `((let ,(mapcar (lambda (b)
                       (if (consp (car b))
                           (car b)
                           b))
                     binds)
         ,@(dbind-ex (mapcan (lambda (b)
                              (if (consp (car b))
                                  (cdr b)))
                            binds)
                    body)))))

(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1) col)
               (mapcan (lambda (pat)
                         (incf row)
                         (setq col -1)
                         (mapcar (lambda (p)
                                   `(,p (aref ,gar ,row ,(incf col))))
                                 pat))
                       pats))
         ,@body))))

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar (lambda (p)
                       `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar (lambda (f)
                       `(,f (,(symb name f) ,gs)))
                     fields)
         ,@body))))

;;; 参照

(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar (lambda (b)
                                   (if (consp (car b))
                                       (car b)
                                       b))
                                 binds)
         ,(wplac-ex (mapcan (lambda (b)
                              (if (consp (car b))
                                  (cdr b)))
                            binds)
                    body))))

;;; マッチング

(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defmacro if-match (pat seq then &optional else)
  (let ((gsym (gensym "UNDEFINED")))
    `(let ,(mapcar (lambda (v) `(,v ',gsym))
                   (vars-in pat #'simple?))
       (pat-match ,pat ,seq ,then ,else))))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq)
                             (destruc pat gseq #'simple?))
                       then
                       `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defun match1 (refs then else)
  (dbind ((pat expr) . rest) refs
    (cond ((gensym? pat)
           `(let ((,pat ,expr))
              (if (and (typep ,pat 'sequence)
                       ,(length-test pat rest))
                  ,then
                  ,else)))
          ((eq pat '_) then)
          ((var? pat)
           (let ((ge (gensym)))
             `(let ((,ge ,expr))
                (if (or (gensym? ,pat) (equal ,pat ,ge))
                    (let ((,pat ,ge)) ,then)
                    ,else))))
          (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))
