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
