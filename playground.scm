(define (combine f g)
  (define (combo . x)
    (f (apply g x)))
  combo)

(let ((h (combine (lambda (x) (list 1 x)) (lambda (x) (list 2 x)))))
  (h (list 3)))

(let ((h (combine (lambda (x) (cons 1 x)) (lambda (x) (cons 2 x)))))
  (h (cons 3 ())))


(define (identity x) x)

(define ((iterate n) f)
  (if (= n 0)
      identity
      (combine f ((iterate (- n 1)) f))))

(square 2)
(((iterate 2) square) 2)

(define (parallel-combine h f g)
  (define (combo . xs)
    (h (apply f xs) (apply g xs)))
  combo)

(let ((h (parallel-combine list (lambda (x) (list 1 x)) (lambda (x) (list 2 x)))))
  (h 'a))

;; resisting an itch to rename symbols in code i just wrote: if i keep doing it the complexity of writing my code is quadratic! i should just append forms, not change them.

;; so far i have sequential composition with combine (think chaining layers) and parallel composition with parallel-combine (think executing multiple dot products in parallel)

;; first we'll need to keep track of function arity in a global table

;; sticky notes for functions
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

(get-arity combine) ; 2
(get-arity parallel-combine) ; 3

;; let's do spread-combine: run parts of inputs through different function heads
;; new: get-arity , list-head, list-tail

(define (spread-combine h f g)
  (let ((n (get-arity f)))
    (define (spread-combo . args)
      (h (apply f (list-head args n))
	 (apply g (list-tail args n))))
    spread-combo))

(list-head '('a 'b 'c) 2)

(define (partial f . head)
  (define (partially-applied . tail)
    (apply f (append head tail)))
  partially-applied)

((partial + 2) 3)
    

;; (let* ((f (restrict-arity (partial cons 1) 2))
;;        (g (restrict-arity (partial cons 2) 2))
;;        (j (spread-combine + f g)))
;;   (j (list 0 0)))

;; this code causes an error:
;; The object (), passed as the first argument to cdr, is not the correct type.

;; (let* ((f (restrict-arity (partial cons 1) 1))
;;        (g (restrict-arity (partial cons 2) 1))
;;        (j (spread-combine (partial apply +) f g)))
;;   (j (list 0) (list 0)))

;The object (1 0), passed as the second argument to integer-add, is not the correct type.

(let* ((f (restrict-arity (partial cons 1) 1))
       (g (restrict-arity (partial cons 2) 1))
       (add (lambda (a b) (+ (apply + a) (apply + b))))
       (j (spread-combine add f g)))
  (j (list 0) (list 0)))

;Value: 3


;; safer version that propagates arities:

(define (spread-combine h f g)
  (let* ((n (get-arity f))
	 (m (get-arity g))
	 (t (+ n m)))
    (define (spread-combo . args)
      (assert (= (length args) t))
      (h (apply f (list-head args n))
	 (apply g (list-tail args n))))
    (restrict-arity spread-combo t)))

((spread-combine list
		 (lambda (x y) (list 'foo x y))
		 (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

;; use multiple return values

(define (spread-apply f g)
  (let* ((n (get-arity f))
	 (m (get-arity g))
	 (t (+ n m)))
    (define (spread-combo . args)
      (assert (= (length args) t))
      (values (apply f (list-head args n))
	      (apply g (list-tail args n))))
    (restrict-arity spread-combo t)))

((spread-apply
  (lambda (x y) (list 'foo x y))
  (lambda (u v w) (list 'bar u v w)))
 'a 'b 'c 'd 'e)

(define (compose f g)
  (define (composition . args)
    (call-with-values (lambda () (apply g args))
      f))
  (restrict-arity composition (get-arity g)))

((compose list
	  (spread-apply
	  (lambda (x y) (list 'foo x y))
	  (lambda (u v w) (list 'bar u v w))))
 'a 'b 'c 'd 'e)


;; a recurrent neural network
;; projects a state and projects an input
;; adds projections together and takes a nonlinearity
;; e.g. https://arxiv.org/abs/1504.00941

(define (identity x) x)

(define (relu x) (max 0 x))

(define (linear-add trans obs state input)
  (+ (trans state) (obs input)))

(define (relu-add trans obs state input)
  (relu (+ (trans state) (obs input))))

(define (dot x w)
  (+ (* x w)))


;; xor:
;; 0 ^ 1 = 1
;; 1 ^ 0 = 1
;; 1 ^ 1 = 0
;; 0 ^ 0 = 1

(define (xor a b)
  (modulo (+ a b) 2))

(define (xor a b)
  (if (not (= a b)) 1 0))

(define xor-table #(1 1 1 0))

(define (xor a b)
  (vector-ref xor-table (+ (* a 2) b)))

;; how to linearize the expression tree? convert it to continuation passing style

;; first let's try call/cc

(define foo)
(set! foo
      (+ 1
	 (call/cc
	  (lambda (continue)
	    (continue (* 2 3)))) ;; return
	 (/ 8 2)))
	    

(define foo)
(define bar)
(set! foo
      (+ 1
	 (call/cc
	  (lambda (continue)
	    (set! bar continue)
	    (continue (* 2 3)))) ;; return
	 (/ 8 2)))

;; call/cc saves the future of the computation
;; this computation sets foo

(bar -2)

foo

(define (cart1 xs ys)
  (if (null? xs)
      '()
      (append
       (map (lambda (y) (cons (car xs) y))
	    ys)
       (cart1 (cdr xs) ys))))

(cart1 '(1 2) '(a b))
