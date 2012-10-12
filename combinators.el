(require 'cl)

(defun cur (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))

(defun comp (&rest funcs)
  (lambda (arg)
    (reduce (lambda (a fun) (funcall fun a))
            (reverse funcs)
            :initial-value arg)))

(defmacro cut (&rest stuff)
  (let ((args (mapcar
               (lambda (x) (gensym))
               (remove-if-not (cur 'eq '<>) stuff))))
    `(lambda ,args
       ,(mapcar (lambda (x)
                  (if (eq x '<>) (pop args) x))
                stuff))))

(defun orfn (&rest fns)
  "Returns a function whose result is non-nil if the result of
any of the given FNs applied to the argument is non-nil."
  (lambda (x)
    (some (cut funcall <> x) fns)))

(defun andfn (&rest fns)
  "Returns a function whose result is non-nil if all of the
given FNS return non-nil when applied to the argument."
  (lambda (x)
    (every (cut funcall <> x) fns)))

(defun juxt (&rest funcs)
  (lambda (&rest args) (mapcar (cut apply <> args) funcs)))

(defun juxtcons (a b)
  (lambda (&rest args) 
    (cons (apply a args) (apply b args))))

(defun maybe (fun pred)
  "Returns a function that calls FUN on its argument if the result of
PRED on its argument is non-nil, else returns nil"
  (lambda (arg)
    (if (funcall pred arg) (funcall fun arg) nil)))

(defun const (c)
  "Returns a function that returns C regardless of arguments."
  (lambda (&rest args) c))
