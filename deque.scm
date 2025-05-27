(define (make-empty-deque)
  (let ((cell (cons '() '())))   ; initially both front and rear are ()
    cell))

(define (deque-empty? dq)
  (null? (car dq)))   ; front is '(), so deque is empty

(define (prepend! dq x)
  (let ((new-cell (cons x (car dq)))) ; push at front, like a stack
    (if (null? (car dq))
	(set-cdr! dq new-cell))  ; if empty, update rear too
    (set-car! dq new-cell)))     ; set front to new cell

(define (append! dq x)
  (let ((new-cell (cons x '())))
    (if (null? (car dq))
	(begin
	  (set-car! dq new-cell)    ; update front
	  (set-cdr! dq new-cell))   ; update rear
	(begin
	  (set-cdr! (cdr dq) new-cell)  ; set cdr of current rear to new cell
	  (set-cdr! dq new-cell)))))    ; update rear pointer

(define (popleft! dq)
  (let ((front (car dq)))
    (if (null? front)
	(error "popleft!: deque is empty")
	(begin
	  (set-car! dq (cdr front))                 ; move front pointer to rest
	  (when (null? (car dq)) (set-cdr! dq '())) ; if now empty, update rear pointer
	  (car front)))))                           ; return popped value

(define (popright! dq)
  (let ((front (car dq)))
    (cond
      ((null? front)
       (error "popright!: deque is empty"))
      ((null? (cdr front))
       ;; Only one element: update front and rear to empty
       (set-car! dq '())
       (set-cdr! dq '())
       (car front))
      (else
       ;; More than one element -- walk to penultimate
       (let loop ((prev front) (curr (cdr front)))
         (if (null? (cdr curr))
             (begin
               (set-cdr! prev '())      ; Remove last cell
               (set-cdr! dq prev)       ; Update rear pointer
               (car curr))              ; Return last value
             (loop (cdr prev) (cdr curr))))))))

(define (deque-to-list dq)
  (let ((front (car dq)))
    (if (null? front)
	'()
	(let loop ((node front) (acc '()))
	  (if (null? node)
	      (reverse acc)
	      (loop (cdr node) (cons (car node) acc)))))))

(begin
  (define D (make-empty-deque))
  
  (assert (deque-empty? D)) ; D: empty
  
  (prepend! D 'a)                               ; D: a
  (assert (equal? (deque-to-list D) '(a)))
  
  (append! D 'b)                                ; D: a b
  (assert (equal? (deque-to-list D) '(a b)))
 
  (prepend! D 'c)                                ; D: c a b
  (assert (equal? (deque-to-list D) '(c a b)))

  (assert (eq? (popleft! D) 'c))                 ; pop 'c, D: a b
  (assert (equal? (deque-to-list D) '(a b)))

  (assert (eq? (popleft! D) 'a))                 ; pop 'a, D: b
  (assert (equal? (deque-to-list D) '(b)))

  (assert (eq? (popleft! D) 'b))                 ; pop 'b, D: empty again
  (assert (equal? (deque-to-list D) '()))

  (assert (deque-empty? D))                      ; D: still empty at end

  'deque-is-solid)

(begin
  (define D (make-empty-deque))
  
  (assert (deque-empty? D)) ; D: empty
  
  (prepend! D 'a)                               ; D: a
  (assert (equal? (deque-to-list D) '(a)))
  
  (append! D 'b)                                ; D: a b
  (assert (equal? (deque-to-list D) '(a b)))
 
  (prepend! D 'c)                                ; D: c a b
  (assert (equal? (deque-to-list D) '(c a b)))

  (assert (eq? (popleft! D) 'c))                 ; pop 'c, D: a b
  (assert (equal? (deque-to-list D) '(a b)))

  (assert (eq? (popright! D) 'b))                ; popright 'b, D: a
  (assert (equal? (deque-to-list D) '(a)))

  (append! D 'x)     ; D: a x
  (prepend! D 'z)                                ; D: z a x
  (assert (equal? (deque-to-list D) '(z a x)))

  (assert (eq? (popright! D) 'x))                ; popright 'x, D: z a
  (assert (equal? (deque-to-list D) '(z a)))

  (assert (eq? (popleft! D) 'z))                 ; popleft 'z, D: a
  (assert (equal? (deque-to-list D) '(a)))

  (assert (eq? (popright! D) 'a))                ; popright 'a, D: empty
  (assert (equal? (deque-to-list D) '()))

  (assert (deque-empty? D))                      ; D: still empty

  'deque-is-solid)
