;
; queue.scm
;
; Example of using QueueValues to communicate results from one thread
; to another.
;
; QueueValues provide a thread-safe way to pass values between concurrent
; processes. Unlike LinkValues, QueueValues support push/pop operations.
;
(use-modules (opencog) (opencog exec))

; QueueValues are much like LinkValues, in that they can hold sequences
; of values. Here's a static sequence of values.
(cog-set-value!
	(Concept "Aybe Sea") (Predicate "key")
	(QueueValue 
		(Concept "E") (Concept "A") (Concept "D")
		(Concept "G") (Concept "B") (Concept "E")))

(cog-execute! (StreamValueOf (Concept "Aybe Sea") (Predicate "key")))

; Generate some ConceptNodes...
(define generator
	(let ((str "a"))
		(lambda () (set! str (string-concatenate (list str "b")))
			(Concept str))))

(define (create) (generator))

(define (prt atom)
	(format #t "Got this atom: ~A\n" atom)
	(stv 1 1))

; An atom with a queue
(cog-set-value!  (Concept "abc") (Predicate "key") (QueueValue))

; Helper function to push values to a queue
(define (push-to-queue atom key value)
	(let ((queue (cog-value atom key)))
		(if (and queue (cog-value? queue))
			(begin
				; Get current queue contents
				(let ((current-vals (cog-value->list queue)))
					; Create new queue with added value
					(cog-set-value! atom key 
						(apply QueueValue (append current-vals (list value)))))
				value)
			(begin
				; Create new queue with single value
				(cog-set-value! atom key (QueueValue value))
				value))))

; Producer function that pushes to queue
(define (produce-and-push)
	(let ((new-atom (create)))
		(push-to-queue (Concept "abc") (Predicate "key") new-atom)
		(format #t "Produced and pushed: ~A\n" new-atom)
		new-atom))

; Consumer function that pops from queue
(define (consume-from-queue)
	(let ((queue (cog-value (Concept "abc") (Predicate "key"))))
		(if (and queue (cog-value? queue))
			(let ((vals (cog-value->list queue)))
				(if (not (null? vals))
					(begin
						; Pop first value
						(let ((first-val (car vals))
							  (rest-vals (cdr vals)))
							; Update queue with remaining values
							(cog-set-value! (Concept "abc") (Predicate "key")
								(apply QueueValue rest-vals))
							(prt first-val)
							first-val))
					(begin
						(format #t "Queue is empty\n")
						#f)))
			(begin
				(format #t "No queue found\n")
				#f))))

; Producer, consumer threads using proper queue operations
(define mt
	(ThreadJoin
		; Producer thread
		(SequentialAnd
			(True (ExecutionOutput (GroundedSchema "scm: produce-and-push") (List)))
			(True (Sleep (Number 1)))
			(True (ExecutionOutput (GroundedSchema "scm: produce-and-push") (List)))
			(True (Sleep (Number 1)))
			(True (ExecutionOutput (GroundedSchema "scm: produce-and-push") (List)))
			(True (Sleep (Number 1)))
			(True (ExecutionOutput (GroundedSchema "scm: produce-and-push") (List)))
		)
		; Consumer thread
		(SequentialAnd
			(True (Sleep (Number 0.5))) ; Let producer get ahead
			(True (ExecutionOutput (GroundedSchema "scm: consume-from-queue") (List)))
			(True (Sleep (Number 1)))
			(True (ExecutionOutput (GroundedSchema "scm: consume-from-queue") (List)))
			(True (Sleep (Number 1)))
			(True (ExecutionOutput (GroundedSchema "scm: consume-from-queue") (List)))
			(True (Sleep (Number 1)))
			(True (ExecutionOutput (GroundedSchema "scm: consume-from-queue") (List)))
		)
	))

; Execute the producer-consumer example
; (cog-execute! mt)

; Alternative approach using QueueValueOf for streaming
(define stream-consumer
	(SequentialAnd
		(Evaluation (GroundedPredicate "scm: prt")
			(List (QueueValueOf (Concept "stream-source") (Predicate "data"))))
		(Evaluation (GroundedPredicate "scm: prt")
			(List (QueueValueOf (Concept "stream-source") (Predicate "data"))))
		(Evaluation (GroundedPredicate "scm: prt")
			(List (QueueValueOf (Concept "stream-source") (Predicate "data"))))))

; Test the queue operations
(display "Testing queue operations:\n")

; Push some values
(push-to-queue (Concept "test") (Predicate "q") (Number 1))
(push-to-queue (Concept "test") (Predicate "q") (Number 2))
(push-to-queue (Concept "test") (Predicate "q") (Number 3))

; Display queue contents
(display "Queue contents: ")
(display (cog-value->list (cog-value (Concept "test") (Predicate "q"))))
(newline)

; Pop values
(cog-set-value! (Concept "abc") (Predicate "key") 
	(cog-value (Concept "test") (Predicate "q")))
(consume-from-queue)
(consume-from-queue)
(consume-from-queue)
(consume-from-queue) ; Should report empty

; The ThreadJoin example can be run by uncommenting:
; (cog-execute! mt)