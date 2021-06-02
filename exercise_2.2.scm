;; Make our arity extension match the more general arity extension in scheme
;;
;; Scheme's built in arity has some flaws, a procedure can advertise a
;; particular arity but still blow up if arguments in that range are provided
;;
;; In the case of the exercise we should be able to handle that with a little
;; more rigor, at least in the case of these combinators.
;;
;; It's probably worth following the pattern of the built in. Get arity should
;; return a pair and we'll add get-arity-min and get-arity-max.
;;
;; Equivalently restrict arity should store the pair (min . max)
;; get-arity will need to lose the assertion that the min and max are equivalent.
;;

(define arity-table (make-key-weak-eqv-hash-table))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

;; get arity returns a tuple now
(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (procedure-arity proc)))

;; overkill but it does make it readable
(define (get-arity-min arity)
  (car arity))

(define (get-arity-max arity)
  (cdr arity))

;; compose needs to get the arity of g
;; then simply make sure g's arguments fall in the range of min and max
;; and that f has a min and max arity of 1
(define (compose f g)
  (let ((t (get-arity g))
        (f-arity (get-arity f)))
    (assert (and 'f-must-take-1-arg (= 1 (get-arity-max f-arity) (get-arity-min f-arity))))
    (define (the-composition . args)
      (f (apply g args)))
    (restrict-arity the-composition t)))

(define (identity x) x)

(define ((iterate n) f)
  (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))

;; parallel combine we just need to make sure the arities of f and g match
(define (parallel-combine h f g)
  (let ((t1 (get-arity f))
        (t2 (get-arity g)))
    (assert (equal? t1 t2))
    (define (the-combination . args)
      (h (apply f args) (apply g args)))
    (restrict-arity the-combination t1)))

(define (max-of-arities f-arity g-arity)
  (let ((f-max (get-arity-max f-arity))
        (g-max (get-arity-max g-arity)))
    (cond ((or (eq? #f f-max)
               (eq? #f g-max)) #f)
          (else (max f-max g-max)))))

(define (min-of-arities f-arity g-arity)
  (+ (get-arity-min f-arity) (get-arity-min g-arity)))

;; do we have a compatible arity for spread combine
(define (spread-combine-compatible-arity? args f g)
  (let* ((f-arity (get-arity f))
         (g-arity (get-arity g))
         (min-args (min-of-arities f-arity g-arity))
         (max-args (max-of-arities f-arity g-arity)))
    (>= (length args) min-args)))

;; does x fit into the arity of f
(define (arity-match? f x)
  (let ((minimum (get-arity-min (get-arity f)))
        (maximum (get-arity-max (get-arity f))))
    (and (>= x minimum)
         (or (eq? maximum #f)
             (<= x maximum)))))

;; builds a list of arities from x .. arg-count that when arity x can be used
;; for (f x) the arg-count - x can be used for g, i.e. (g (- arg-count x))
;; presumably this is always used after we've checked that the arg count is
;; compatible for these functions vis `spread-combine-compatible-arity?'
(define (arity-range-check* f g x acc arg-count)
  (cond ((> x arg-count) acc)
        ((and (arity-match? f x)
              (arity-match? g (- arg-count x)))
         (arity-range-check* f g (+ x 1) (cons x acc) arg-count))
        (else (arity-range-check* f g (+ x 1) acc arg-count))))

(define (arity-range-check f g args)
  (arity-range-check* f g 0 '() (length args)))

(define (grab-central-value xs)
  (car (drop xs (quotient (length xs) 2))))

(define (spread-combine h f g)
  (define (the-combination . args)
    (assert (and 'incompatible-arities (spread-combine-compatible-arity? args f g)))
    (let ((pivot (grab-central-value (arity-range-check f g args))))
        (h (apply f (list-head args pivot))
           (apply g (list-tail args pivot)))))
  (restrict-arity the-combination (cons (min-of-arities (get-arity f) (get-arity g))
                                        (max-of-arities (get-arity f) (get-arity g)))))
