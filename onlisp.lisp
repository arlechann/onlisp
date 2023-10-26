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

(defun nconc1 (ls obj)
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

