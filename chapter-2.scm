(define arity-table (make-key-weak-eqv-hash-table))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc)))
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

(define (compose f g)
  (let ((t (get-arity g)))
    (define (the-composition . args)
      (assert (and 'arg-count-incompatible (= (length args) t)))
      (f (apply g args)))
    (restrict-arity the-composition t)))

(define (identity x) x)

(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))

(define (parallel-combine h f g)
  (let ((t1 (get-arity f))
        (t2 (get-arity g)))
    (define (the-combination . args)
      (assert (and 'arg-count-must-match (length args) t1 t2))
      (h (apply f args) (apply g args)))
    (restrict-arity the-combination t1)))

(define (spread-combine h f g)
  (let ((n (get-arity f))
        (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (and 'sum-of-args-incompatible (= (length args) t)))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t))))
