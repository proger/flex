
;; uniform[-0.5/d, 0.5/d]
(define (randomize-embedding-matrix! matrix)
  (let* ((num-entries (vector-length matrix))
         (dim (vector-length (vector-ref matrix 0)))
         (scale (/ 0.5 dim)))
    (do ((i 0 (+ i 1)))
        ((= i num-entries))
      (let ((row (vector-ref matrix i)))
        (do ((j 0 (+ j 1)))
            ((= j dim))
          (vector-set! row j (- (* (random-real) 2 scale) scale))))
      (vector-set! matrix i row))
    matrix))

(define (initialize-cbow-model vocab-size embedding-dim)
  ;; Make and randomize embedding matrices for input and output
  (let* ((embedding-matrix (make-zero-embedding-matrix vocab-size embedding-dim))
         (output-matrix (make-zero-embedding-matrix vocab-size embedding-dim)))
    (randomize-embedding-matrix! embedding-matrix)
    (randomize-embedding-matrix! output-matrix)
    ;; Return as a state record, here as an assoc list
    `((embedding-matrix . ,embedding-matrix)
      (output-matrix . ,output-matrix)
      (vocab-size . ,vocab-size)
      (embedding-dim . ,embedding-dim))))

(define model-state (initialize-cbow-model 10000 100))

(define embedding-matrix (assoc-ref model-state 'embedding-matrix))
(define output-matrix (assoc-ref model-state 'output-matrix))

(define (train-autoregressive-cbow-neg-sample-ngram
          corpus window embedding-matrix output-matrix learning-rate
          vocab-size negative-size min-n max-n ngram-table-size id->word)
  (let ((corpus-length (vector-length corpus)))
    (do ((i window (+ i 1)))
        ((>= i (- corpus-length window)))
      (let* ((context-ids (extract-context corpus i window))
             (center-id (vector-ref corpus i))
             ;; 1. Get n-grams for each context word and build mean context vector
             (context-ngram-lists (map (lambda (wid)
                                         (let ((word (vector-ref id->word wid)))
                                           (word-ngrams word min-n max-n)))
                                       context-ids))
             (context-vecs (map (lambda (ngram-list)
                                  (lookup-ngram-embedding embedding-matrix
                                                          (map (lambda (ng) (ngram->index ng ngram-table-size))
                                                               ngram-list)))
                                context-ngram-lists))
             (context-mean (average-vectors context-vecs))

             ;; 2. Get n-grams for center word & negatives
             (center-word (vector-ref id->word center-id))
             (center-ngram-list (word-ngrams center-word min-n max-n))
             (center-ngram-indices (map (lambda (ng) (ngram->index ng ngram-table-size)) center-ngram-list))
             (neg-ids (sample-negatives vocab-size center-id negative-size))
             (neg-ngram-indices-lists
               (map (lambda (wid)
                      (let ((w (vector-ref id->word wid)))
                        (map (lambda (ng) (ngram->index ng ngram-table-size))
                             (word-ngrams w min-n max-n))))
                    neg-ids))
             ;; 3. Compose ngram embeddings for output vectors
             (center-output-vec
               (lookup-ngram-embedding output-matrix center-ngram-indices))
             (neg-output-vecs
               (map (lambda (ngram-indices)
                      (lookup-ngram-embedding output-matrix ngram-indices))
                    neg-ngram-indices-lists))
             ;; 4. Forward pass: scores, sigmoids
             (pos-dot (dot center-output-vec context-mean))
             (pos-sig (sigmoid pos-dot))
             (neg-dots (map (lambda (v) (dot v context-mean)) neg-output-vecs))
             (neg-sigs(map (lambda (x) (sigmoid (- x))) neg-dots))
             ;; 5. Gradient for context vector
             (grad-context (compute-cbow-neg-sample-gradients context-mean center-output-vec neg-output-vecs pos-sig neg-sigs)))
        ;; 6. Update all context n-gram embeddings
        (for-each (lambda (ngram-list)
                    (update-ngram-embeddings! embedding-matrix
                                              (map (lambda (ng) (ngram->index ng ngram-table-size))
                                                   ngram-list)
                                              grad-context learning-rate))
                  context-ngram-lists)
        ;; 7. Update center word n-gram output embeddings
        (update-ngram-embeddings! output-matrix center-ngram-indices
                                  (vector-scale context-mean (- 1 pos-sig))
                                  learning-rate)
        ;; 8. Update negative samples' n-gram output embeddings
        (for-each2
          (lambda (ngram-indices neg-sig)
            (update-ngram-embeddings! output-matrix ngram-indices
                                      (vector-scale context-mean (- neg-sig 1))
                                      learning-rate))
          neg-ngram-indices-lists neg-sigs)))))

(define (sigmoid x) (/ 1.0 (+ 1.0 (exp (- x)))))

(define corpus (vector 12 45 101 2)) ;; word indices

(define (random-real) (/ (random 1000000) 1000000.0))

(define (compute-frequencies corpus vocab-size)
  (let ((counts (make-vector vocab-size 0))
        (total (vector-length corpus)))
    (vector-for-each (lambda (wid)
                       (vector-set! counts wid (+ 1 (vector-ref counts wid))))
                     corpus)
    (map (lambda (cnt) (/ cnt total))
         (vector->list counts))))

;; Compute the keep probability for each word
(define (compute-keep-probsfreqs t)
  (map (lambda (f)
         (min 1.0 (+ (sqrt (/ t f)) (/ t f))))
       freqs))

;; Subsampling function: returns a filtered copy of corpus vector
(define (subsample-corpus corpus keep-probs)
  (list->vector
   (filter (lambda (wid)
             (> (random-real) (- 1 (list-ref keep-probs wid))))
           (vector->list corpus))))

;; for-each2 applies fun to each element in all given lists (like Python's zip)
(define (for-each2 fun l1 l2 l3)
  (let loop ((l1 l1) (l2 l2) (l3 l3))
    (if (null? l1) 'done
        (begin
          (fun (car l1) (car l2) (car l3))
          (loop (cdr l1) (cdr l2) (cdr l3))))))

(define (word-ngrams word min-n max-n)
  ;; Returns a list of n-grams for word, including word boundaries
  (let ((w (string-append "<" word ">")))
    (apply append
           (map (lambda (n)
                  (let loop ((i 0) (lst '()))
                    (if (<= (+ i n) (string-length w))
                        (loop (+ i 1)
                              (cons (substring w i (+ i n)) lst))
                        lst)))
                (iota (- (+ 1 max-n) min-n) min-n)))))

(define (build-corpus-ngrams word-list min-n max-n)
  ;; Returns an association list: ((word . (ngram ...)) ...)
  (map (lambda (word)
         (cons word (word-ngrams word min-n max-n)))
       word-list))

(define (build-ngram-table word-list min-n max-n)
  ;; Map ngram string -> set of word indices that contain it (if desired)
  (let ((table (make-hash-table)))
    (for-each (lambda (word-id)
                (let ((ngrams (word-ngrams (list-ref word-list word-id) min-n max-n)))
                  (for-each (lambda (ng)
                              (hash-table-update!/default table ng
                                (lambda (lst) (cons word-id lst))
                                '()))
                            ngrams)))
              (iota (length word-list)))
    table))

(define (id->ngrams word-id id->word min-n max-n)
  (word-ngrams (vector-ref id->word word-id) min-n max-n))

(define (extract-context corpus i window)
  ;; Returns a list of word-ids in the context window around position i (excluding i itself)
  (let ((left (max 0 (- i window)))
        (right (min (vector-length corpus) (+ i window 1))))
    (append (vector->list (vector-copy corpus left i))
            (vector->list (vector-copy corpus (+ i 1) right)))))

(define (ngram->index ngram ngram-table-size)
  (modulo (string-hash ngram) ngram-table-size))

(define (word-embedding-indices word min-n max-n ngram-table-size)
  (map (lambda (ng)
         (ngram->index ng ngram-table-size))
       (word-ngrams word min-n max-n)))

(define (average-vectors vec-list)
  (let ((n (length vec-list)))
    (if (zero? n)
        (error "No vectors to average")
        (let ((sum (foldl vector-add (make-zero-vector (vector-length (car vec-list))) vec-list)))
          (vector-scale sum (/ 1.0 n))))))

(define (sample-negatives vocab-size center-id k)
  ;; Returns a list of k negative word-ids (≠ center-id)
  (let loop ((result '()))
    (if (= (length result) k)
        result
        (let ((cand (random vocab-size)))
          (if (or (= cand center-id)
                  (member cand result))
              (loop result)
              (loop (cons cand result)))))))

(define (compute-sigmoid-scores context-mean output-matrix center-id neg-ids)
  (let* ((center-vec (lookup-embedding output-matrix center-id))
         (neg-vecs (map (lambda (wid) (lookup-embedding output-matrix wid)) neg-ids))
         (pos-dot (dot center-vec context-mean))
         (pos-sig (sigmoid pos-dot))
         (neg-dots (map (lambda (vec) (dot vec context-mean)) neg-vecs))
         (neg-sigs (map (lambda (x) (sigmoid (- x))) neg-dots)))
    (list pos-sig neg-sigs center-vec neg-vecs)))

(define (compute-cbow-neg-sample-gradients context-mean center-vec neg-vecs pos-sig neg-sigs)
  ;; context-mean, center-vec: vectors
  ;; neg-vecs: list of vectors
  ;; pos-sig: positive sigmoid
  ;; neg-sigs: list of negative sigmoids
  (let ((grad-context
         (vector-sub
           (vector-scale center-vec (- 1 pos-sig))
           (foldl vector-add (make-zero-vector (vector-length center-vec))
                  (map (lambda (neg vec-neg-sig)
                         (vector-scale neg (- vec-neg-sig 1)))
                       neg-vecs neg-sigs)))))
    grad-context))


(define (update-context-embeddings! embedding-matrix context-ids grad-context learning-rate)
  (for-each (lambda (wid)
              (update-embedding! embedding-matrix wid grad-context learning-rate))
            context-ids))

(define (update-output-embeddings! output-matrix center-id neg-ids context-mean pos-sig neg-sigs learning-rate)
  ;; Update center word
  (update-embedding! output-matrix center-id (vector-scale context-mean (- 1 pos-sig)) learning-rate)
  ;; Update negative samples
  (for-each2
   (lambda (neg-id neg-sig)
     (update-embedding! output-matrix neg-id (vector-scale context-mean (- neg-sig 1)) learning-rate))
   neg-ids neg-sigs))

(define (update-ngram-embeddings! matrix ngram-indices grad-vec lr)
  (for-each (lambda (i)
              (update-embedding! matrix i grad-vec lr))
            ngram-indices))

(define (make-zero-vector) ...)
(define (vector-add . vectors) ...)
(define (vector-sub u v) ...)
(define (vector-scale v alpha) ...)
(define (average-vectors vector-list) ...)
(define (dot u v) ...)
(define (make-zero-embedding-matrix num-entries dim) ...)
(define (lookup-embedding embedding-matrix word-id) ...)
(define (lookup-ngram-embedding matrix ngram-indices)
  ;; sum/average vecs for ngram indices from matrix
  (average-vectors (map (lambda (i) (vector-ref matrix i)) ngram-indices)))
(define (matrix-mul matrix vector) ...)
(define (softmax scores) ...)
(define (cross-entropy-loss probs target-id) ...)
(define (compute-grads context-mean center-id pred-probs) ...)
(define (update-output-matrix! output-matrix grads lr) ...)
(define (update-embedding-matrix! embedding-matrix context-ids grads lr) ...)
(define (average-vectors vlist) ...)
(define (update-embedding! matrix word-id grad-vector lr) 
  ;; update matrix[word-id] += lr * grad-vector
  ...)
(define (sample-negatives vocab-size center-id k) ...) ; returns list of k word-ids ≠ center-id

(define (save-model! model-state epoch)
  (display (string-append "Saving model at epoch " (number->string epoch) "\n"))
  ;; Code to serialize and write model-state
  'ok)

;; Load model state from disk (stub: not required in loop but for completeness)
(define (load-model path)
  (display (string-append "Loading model from " path "\n"))
  ;; Code to load model
  'mock-model-state)

(define (compose-word-embedding word embedding-matrix min-n max-n ngram-table-size)
  (let* ((ngrams (word-ngrams word min-n max-n))
         (ngram-indices (map (lambda (ng) (ngram->index ng ngram-table-size)) ngrams))
         (vecs (map (lambda (i) (vector-ref embedding-matrix i)) ngram-indices)))
    (average-vectors vecs))) ; or sum, but average for numerical stability

;; Computes log-likelihood over evaluation set
(define (evaluate-model model-state eval-corpus window min-n max-n ngram-table-size)
  (let* ((embedding-matrix (assoc-ref model-state 'embedding-matrix))
         (output-matrix (assoc-ref model-state 'output-matrix))
       (id->word (assoc-ref model-state 'id->word))
         (vocab-size (assoc-ref model-state 'vocab-size))
         (corpus-length (vector-length eval-corpus))
         (ll-sum 0.0)
         (count 0))
    (do ((i window (+ i 1)))
        ((>= i (- corpus-length window)))
      (let* ((context-ids (extract-context eval-corpus i window))
             (center-id (vector-ref eval-corpus i))
             ;; Compose embedding for each context word via n-grams:
             (context-vecs
               (map (lambda (wid)
                      (compose-word-embedding (vector-ref id->word wid)
                                              embedding-matrix min-n max-n ngram-table-size))
                    context-ids))
             (context-mean (average-vectors context-vecs))
             ;; Compose output embedding for all vocab words:
             (output-vecs
               (map (lambda (wid)
                      (compose-word-embedding (vector-ref id->word wid)
                                   output-matrix min-n max-n ngram-table-size))
                    (iota vocab-size)))
             ;; Full softmax scores:
             (scores (map (lambda (v) (dot v context-mean)) output-vecs))
             (logsumexp
               (let ((m (apply max scores)))
                 (+ m (log (apply + (map (lambda (x) (exp (- x m))) scores))))))
             (ll (- (list-ref scores center-id) logsumexp)))
        (set! ll-sum (+ ll-sum ll))
        (set! count (+ count 1))))
    (let ((avgll (/ ll-sum count)))
      (display (string-append "Average Log-Likelihood: " (number->string avgll) "\n"))
      avgll)))

;; Returns a list of vectors (batches) of given batch-size from a larger vector
(define (batches data batch-size)
  (let ((len (vector-length data)))
    (let loop ((i 0) (out '()))
      (if (>= i len)
          (reverse out)
          (loop (+ i batch-size)
                (cons (vector-copy data i (min len (+ i batch-size))) out))))))

(define (train-cbow-model
          train-corpus          ;; vector of word-ids
          eval-data             ;; evaluation corpus or data
          model-state           ;; assoc/record containing model variables
          window
          learning-rate
          batch-size
          epochs
          negative-size
          min-n
          max-n
          ngram-table-size)

  (let ((vocab-size (assoc-ref model-state 'vocab-size))
        (id->word   (assoc-ref model-state 'id->word)))
    (do ((epoch 0 (+ epoch 1)))
        ((>= epoch epochs))
      (display (string-append "Epoch " (number->string epoch) "\n"))
      ;; Shuffle (optional: for SGD stochasticity)
      (let* ((corpus (shuffle-vector train-corpus))  ; or just train-corpus if you don't implement shuffle now
             (batch-list (batches corpus batch-size)))
        (for-each
         (lambda (batch)
           ;; Train model on this batch
           (train-autoregressive-cbow-neg-sample-ngram
            batch window
            (assoc-ref model-state 'embedding-matrix)
            (assoc-ref model-state 'output-matrix)
            learning-rate vocab-size negative-size
            min-n max-n ngram-table-size id->word))
         batch-list))
      ;; Save after each epoch
      (save-model! model-state epoch)
      ;; Evaluate
  (evaluate-model model-state eval-data))))

;; In-place Fisher-Yates shuffle for vectors
(define (shuffle-vector vec)
  (let ((n (vector-length vec)))
    (do ((i 0 (+ i 1)))
        ((>= i n) vec)
      (let ((j (+ i (random (- n i)))))
        ;; swap i and j
        (let ((tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp)))) vec))

(define model-state (initialize-cbow-model vocab-size embedding-dim)) ; your initializer
(assoc-set! model-state 'id->word id->word-vector) ; set up id->word if not present

(train-cbow-model
  train-corpus        ; e.g. (vector ...)
  eval-data
  model-state
  5                   ; window
  0.025               ; learning-rate
  128                 ; batch-size
  5                   ; epochs
  5                   ; negative-size
  3                   ; min-n
  6                   ; max-n
  2000000)            ; ngram-table-size


;; Helper: split a string into words (space-separated)
(define (split-words s)
  (let loop ((i 0) (start 0) (lst '()))
    (let ((n (string-length s)))
      (cond
        ((= i n)
         (if (= start i) (reverse lst)
             (reverse (cons (substring s start i) lst))))
        ((char-whitespace? (string-ref s i))
         (if (= start i)
             (loop (+ i 1) (+ i 1) lst) ; skip extra whitespace
             (loop (+ i 1) (+ i 1) (cons (substring s start i) lst))))
        (else
         (loop (+ i 1) start lst))))))

;; Main function
(define (read-corpus-and-build-vocab filename)
  (let* ((word->id (make-string-hash-table))
         (id->word (make-equal-hash-table))
         (words '())
         (next-id 0))
    ;; Read file, build list of words, and vocab maps
    (call-with-input-file filename
      (lambda (in)
        (do ((line (read-line in) (read-line in)))
            ((eof-object? line))
          (let ((line-words (split-words line)))
            (for-each
             (lambda (w)
	       (display w)
	       (newline)
               (unless (hash-table/get word->id w #f)
                 (hash-table/put! word->id w next-id)
                 (hash-table/put! id->word next-id w)
                 (set! next-id (+ next-id 1))))
             line-words)
            (set! words (append words line-words))))))
    ;; Build word-id vector for entire corpus
    (let ((corpus-ids
           (list->vector
              (map (lambda (w) (hash-table/get word->id w #f)) words)))
          (vocab-size next-id)
          (id->word-vector (let ((vec (make-vector next-id)))
                             (do ((i 0 (+ i 1)))
                                 ((= i next-id) vec)
                               (vector-set! vec i (hash-table-ref id->word i))))))
      (list word->id id->word-vector corpus-ids))))


(define corpus (read-corpus-and-build-vocab "corpus-tiny.txt"))
(define word->id (list-ref corpus 0))
(define id->word-vector (list-ref corpus 1))
(define corpus-ids (list-ref corpus 2))

(display (hash-table->alist word->id))
(display id->word-vector)
