;; amb stores the current continuation in a deque once for each of its arguments.
;; this allows to decouple the search procedure from the definition of the search space

;; we'll need a deque
(load "deque.scm")

(define *empty-ambs* 0)

(define identity (lambda (x) x))

(define *search-schedule* (make-parameter #f))
(define discover (make-parameter #f))
(define map-discovery (make-parameter #f))
(define *top-level* (make-parameter #f))

(define (amb-list xs)
  (if (null? xs)
      (set! *empty-ambs* (+ *empty-ambs* 1)))
  (call/cc
   (lambda (k)
     (for-each
      (lambda (x)
	((discover) (*search-schedule*)
	 (lambda thunk () (within-continuation k x))))
      ((map-discovery) xs))
     (yield))))

(define (search discover-fn map-discovery-fn problem-thunk)
  (call/cc
   (lambda (k)
     (parameterize ((discover discover-fn)
		    (map-discovery map-discovery-fn)
                    (*search-schedule* (make-empty-deque))
                    (*top-level* k))
       (set! *empty-ambs* 0)
       (problem-thunk)))))

(define (yield)
  (if (deque-empty? (*search-schedule*))
      ((*top-level*) 'stop)
      ((popleft! (*search-schedule*)))))

(define-syntax amb
  (syntax-rules ()
    ((amb exp ...)
     (amb-list (list (lambda () exp) ...)))))

(define (require p)
  (if (not p) (amb) 'ok))

;; example problem

(define (range lo hi)
  (require (<= lo hi))
  (amb lo (range (+ lo 1) hi)))

(define triples-tested)

(define (triples-between lo hi)
  (let* ((i (range lo hi))
	 (j (range lo hi))
	 (k (range lo hi)))
    (set! triples-tested (+ triples-tested 1))
    (require (= (+ (* i i) (* j j))
		(* k k)))
    (list i j k)))

(begin
  (set! triples-tested 0)
  (search append! identity ;; bfs
          (lambda ()
            (pp (triples-between 1 20))))
  triples-tested)

(begin
  (set! triples-tested 0)
  (search prepend! reverse ;; dfs
          (lambda ()
            (pp (triples-between 1 20))))
  triples-tested)

(search
 append! identity ;; bfs
 (lambda ()
   (let ((x (amb 1 2)))
     (pp (list x))
     (let ((y (amb 'a 'b)))
       (pp (list x y))
       (amb)))))

(search
 prepend! reverse ;; dfs
 (lambda ()
   (let ((x (amb 1 2)))
     (pp (list x))
     (let ((y (amb 'a 'b)))
       (pp (list x y))))
   (amb)))
