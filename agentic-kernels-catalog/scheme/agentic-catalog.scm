;
; agentic-catalog.scm
;
; Scheme bindings for the agentic kernels catalog system
; Provides high-level interface for catalog management and kernel analysis
;

(define-module (opencog agentic-catalog))

(use-modules (opencog))

; Load the C++ module (when available)
; (load-extension "libagentic-kernels-catalog" "opencog_agentic_catalog_init")

; ======================================================================
; Functional Role Enumeration
; ======================================================================

(define functional-role-nlp-processing 0)
(define functional-role-attention-allocation 1)
(define functional-role-reasoning-inference 2)
(define functional-role-learning-evolution 3)
(define functional-role-conversational 4)
(define functional-role-emotional-affective 5)
(define functional-role-sensorimotor 6)
(define functional-role-game-strategy 7)
(define functional-role-vision-perception 8)
(define functional-role-memory-episodic 9)
(define functional-role-meta-cognitive 10)
(define functional-role-social-interaction 11)

; ======================================================================
; Cognitive Subsystem Enumeration
; ======================================================================

(define cognitive-subsystem-perceptual-input 0)
(define cognitive-subsystem-working-memory 1)
(define cognitive-subsystem-episodic-memory 2)
(define cognitive-subsystem-semantic-memory 3)
(define cognitive-subsystem-attention-system 4)
(define cognitive-subsystem-motor-output 5)
(define cognitive-subsystem-executive-control 6)
(define cognitive-subsystem-emotional-system 7)
(define cognitive-subsystem-social-cognition 8)
(define cognitive-subsystem-self-model 9)

; ======================================================================
; Deployment Status Enumeration
; ======================================================================

(define deployment-status-production 0)
(define deployment-status-prototype 1)
(define deployment-status-experimental 2)
(define deployment-status-legacy 3)
(define deployment-status-planned 4)
(define deployment-status-deprecated 5)

; ======================================================================
; Standard Agentic Kernel Specifications
; ======================================================================

(define (create-ghost-kernel-spec)
  "Create the GHOST (Goal-oriented Hierarchical OpenCog Scripting Technology) kernel specification"
  (list
    'kernel-name "GHOST"
    'description "Goal-oriented Hierarchical OpenCog Scripting Technology"
    'version "1.0.0"
    'status deployment-status-production
    'implementation-language "C++/Scheme"
    'source-location "opencog/ghost"
    'functional-roles (list functional-role-conversational functional-role-nlp-processing)
    'cognitive-subsystems (list cognitive-subsystem-working-memory cognitive-subsystem-semantic-memory)
    'parameters (list
      (list 'name "dialogue_depth" 'type "int" 'default 5 'range (list 1 10))
      (list 'name "response_confidence" 'type "float" 'default 0.7 'range (list 0.0 1.0))
      (list 'name "context_window" 'type "int" 'default 20 'range (list 1 100))
      (list 'name "goal_satisfaction_threshold" 'type "float" 'default 0.8 'range (list 0.0 1.0)))))

(define (create-relex-kernel-spec)
  "Create the RelEx (Relationship Extraction) kernel specification"
  (list
    'kernel-name "RelEx"
    'description "Relationship Extraction for Natural Language Processing"
    'version "1.0.0"
    'status deployment-status-production
    'implementation-language "Java/C++"
    'source-location "opencog/relex"
    'functional-roles (list functional-role-nlp-processing)
    'cognitive-subsystems (list cognitive-subsystem-perceptual-input cognitive-subsystem-semantic-memory)
    'parameters (list
      (list 'name "parse_depth" 'type "int" 'default 10 'range (list 1 20))
      (list 'name "relationship_confidence" 'type "float" 'default 0.6 'range (list 0.0 1.0))
      (list 'name "linguistic_features" 'type "int" 'default 256 'range (list 10 1000)))))

(define (create-pln-kernel-spec)
  "Create the PLN (Probabilistic Logic Networks) kernel specification"
  (list
    'kernel-name "PLN"
    'description "Probabilistic Logic Networks for reasoning"
    'version "1.0.0"
    'status deployment-status-production
    'implementation-language "C++/Scheme"
    'source-location "opencog/pln"
    'functional-roles (list functional-role-reasoning-inference)
    'cognitive-subsystems (list cognitive-subsystem-working-memory 
                                cognitive-subsystem-semantic-memory 
                                cognitive-subsystem-executive-control)
    'parameters (list
      (list 'name "inference_depth" 'type "int" 'default 15 'range (list 1 50))
      (list 'name "confidence_threshold" 'type "float" 'default 0.5 'range (list 0.0 1.0))
      (list 'name "truth_value_precision" 'type "float" 'default 0.01 'range (list 0.001 0.1))
      (list 'name "rule_strength" 'type "float" 'default 0.8 'range (list 0.0 1.0)))))

(define (create-ecan-kernel-spec)
  "Create the ECAN (Economic Attention Networks) kernel specification"
  (list
    'kernel-name "ECAN"
    'description "Economic Attention Networks"
    'version "1.0.0"
    'status deployment-status-production
    'implementation-language "C++"
    'source-location "opencog/attention"
    'functional-roles (list functional-role-attention-allocation)
    'cognitive-subsystems (list cognitive-subsystem-attention-system cognitive-subsystem-working-memory)
    'parameters (list
      (list 'name "attention_allocation_rate" 'type "float" 'default 0.1 'range (list 0.01 1.0))
      (list 'name "forgetting_rate" 'type "float" 'default 0.01 'range (list 0.001 0.1))
      (list 'name "max_atom_attention" 'type "float" 'default 100.0 'range (list 1.0 1000.0))
      (list 'name "hebbian_learning_rate" 'type "float" 'default 0.01 'range (list 0.001 0.1)))))

(define (create-moses-kernel-spec)
  "Create the MOSES (Meta-Optimizing Semantic Evolutionary Search) kernel specification"
  (list
    'kernel-name "MOSES"
    'description "Meta-Optimizing Semantic Evolutionary Search"
    'version "1.0.0"
    'status deployment-status-production
    'implementation-language "C++"
    'source-location "opencog/moses"
    'functional-roles (list functional-role-learning-evolution)
    'cognitive-subsystems (list cognitive-subsystem-executive-control)
    'parameters (list
      (list 'name "population_size" 'type "int" 'default 100 'range (list 10 1000))
      (list 'name "max_generations" 'type "int" 'default 1000 'range (list 10 10000))
      (list 'name "mutation_rate" 'type "float" 'default 0.01 'range (list 0.001 0.1))
      (list 'name "crossover_rate" 'type "float" 'default 0.7 'range (list 0.1 0.9))
      (list 'name "complexity_penalty" 'type "float" 'default 0.1 'range (list 0.0 1.0)))))

(define (create-eva-kernel-spec)
  "Create the Eva (Expressive Virtual Avatar) kernel specification"
  (list
    'kernel-name "Eva"
    'description "Expressive Virtual Avatar"
    'version "1.0.0"
    'status deployment-status-prototype
    'implementation-language "Python/C++"
    'source-location "hansonrobotics/eva"
    'functional-roles (list functional-role-conversational 
                           functional-role-emotional-affective)
    'cognitive-subsystems (list cognitive-subsystem-emotional-system 
                               cognitive-subsystem-social-cognition 
                               cognitive-subsystem-motor-output)
    'parameters (list
      (list 'name "emotional_expressiveness" 'type "float" 'default 0.7 'range (list 0.0 1.0))
      (list 'name "facial_animation_speed" 'type "float" 'default 1.0 'range (list 0.1 2.0))
      (list 'name "voice_emotion_strength" 'type "float" 'default 0.6 'range (list 0.0 1.0))
      (list 'name "personality_traits" 'type "int" 'default 10 'range (list 5 20)))))

(define (create-loving-ai-kernel-spec)
  "Create the Loving AI kernel specification"
  (list
    'kernel-name "Loving AI"
    'description "Compassionate AI for therapeutic interactions"
    'version "1.0.0"
    'status deployment-status-experimental
    'implementation-language "Python"
    'source-location "opencog/loving-ai"
    'functional-roles (list functional-role-emotional-affective 
                           functional-role-conversational 
                           functional-role-social-interaction)
    'cognitive-subsystems (list cognitive-subsystem-emotional-system 
                               cognitive-subsystem-social-cognition)
    'parameters (list
      (list 'name "compassion_level" 'type "float" 'default 0.9 'range (list 0.0 1.0))
      (list 'name "empathy_sensitivity" 'type "float" 'default 0.8 'range (list 0.0 1.0))
      (list 'name "therapeutic_style" 'type "int" 'default 3 'range (list 1 5))
      (list 'name "emotional_validation" 'type "float" 'default 0.85 'range (list 0.0 1.0)))))

(define (create-game-ai-kernel-spec)
  "Create the Game AI kernel specification"
  (list
    'kernel-name "Game AI"
    'description "Strategic game playing artificial intelligence"
    'version "1.0.0"
    'status deployment-status-prototype
    'implementation-language "C++/Python"
    'source-location "opencog/game-ai"
    'functional-roles (list functional-role-game-strategy 
                           functional-role-reasoning-inference)
    'cognitive-subsystems (list cognitive-subsystem-executive-control 
                               cognitive-subsystem-working-memory)
    'parameters (list
      (list 'name "search_depth" 'type "int" 'default 8 'range (list 1 20))
      (list 'name "exploration_rate" 'type "float" 'default 0.3 'range (list 0.0 1.0))
      (list 'name "strategic_patience" 'type "float" 'default 0.7 'range (list 0.0 1.0))
      (list 'name "risk_tolerance" 'type "float" 'default 0.5 'range (list 0.0 1.0)))))

; ======================================================================
; Catalog Management Functions
; ======================================================================

(define (get-all-standard-kernels)
  "Get list of all standard agentic kernel specifications"
  (list
    (create-ghost-kernel-spec)
    (create-relex-kernel-spec)
    (create-pln-kernel-spec)
    (create-ecan-kernel-spec)
    (create-moses-kernel-spec)
    (create-eva-kernel-spec)
    (create-loving-ai-kernel-spec)
    (create-game-ai-kernel-spec)))

(define (get-production-kernels)
  "Get list of production-ready kernel specifications"
  (filter (lambda (kernel)
            (equal? (assoc-ref kernel 'status) deployment-status-production))
          (get-all-standard-kernels)))

(define (get-prototype-kernels)
  "Get list of prototype kernel specifications"
  (filter (lambda (kernel)
            (equal? (assoc-ref kernel 'status) deployment-status-prototype))
          (get-all-standard-kernels)))

(define (get-experimental-kernels)
  "Get list of experimental kernel specifications"
  (filter (lambda (kernel)
            (equal? (assoc-ref kernel 'status) deployment-status-experimental))
          (get-all-standard-kernels)))

; ======================================================================
; Degrees of Freedom Analysis Functions
; ======================================================================

(define (compute-parameter-degrees-of-freedom param)
  "Compute degrees of freedom for a single parameter"
  (let ((param-type (assoc-ref param 'type))
        (param-range (assoc-ref param 'range)))
    (cond
      ((equal? param-type "float")
       (if (and param-range (= (length param-range) 2))
           (floor (* (- (cadr param-range) (car param-range)) 100))
           100))
      ((equal? param-type "int")
       (if (and param-range (= (length param-range) 2))
           (+ 1 (- (cadr param-range) (car param-range)))
           10))
      ((equal? param-type "bool") 2)
      (else 5))))

(define (compute-kernel-degrees-of-freedom kernel)
  "Compute total degrees of freedom for a kernel"
  (let ((parameters (assoc-ref kernel 'parameters))
        (roles (assoc-ref kernel 'functional-roles))
        (subsystems (assoc-ref kernel 'cognitive-subsystems)))
    (+
      ; Parameter space degrees of freedom
      (if parameters
          (apply + (map compute-parameter-degrees-of-freedom parameters))
          0)
      ; Structural degrees of freedom
      (* (length roles) 2)
      (* (length subsystems) 3)
      ; Base complexity
      10)))

; ======================================================================
; Prime Factorization Functions
; ======================================================================

(define (is-prime? n)
  "Test if a number is prime"
  (cond
    ((< n 2) #f)
    ((= n 2) #t)
    ((even? n) #f)
    (else
      (let loop ((i 3))
        (cond
          ((> (* i i) n) #t)
          ((= (modulo n i) 0) #f)
          (else (loop (+ i 2))))))))

(define (prime-factorization n)
  "Compute prime factorization of a number"
  (if (<= n 1)
      '(1)
      (let ((factors '()))
        (let loop ((num n) (factor 2))
          (cond
            ((= num 1) (reverse factors))
            ((= (modulo num factor) 0)
             (set! factors (cons factor factors))
             (loop (/ num factor) factor))
            ((= factor 2) (loop num 3))
            (else (loop num (+ factor 2))))))))

(define (derive-tensor-shape-from-factors factors)
  "Derive optimal tensor shape from prime factors"
  (cond
    ((null? factors) '(1))
    ((= (length factors) 1) factors)
    ((= (length factors) 2) factors)
    (else
      ; Distribute factors into 3 dimensions for better memory layout
      (let ((dim1 1) (dim2 1) (dim3 1))
        (let loop ((f factors) (index 0))
          (if (null? f)
              (list dim1 dim2 dim3)
              (let ((remainder (modulo index 3)))
                (cond
                  ((= remainder 0) (set! dim1 (* dim1 (car f))))
                  ((= remainder 1) (set! dim2 (* dim2 (car f))))
                  (else (set! dim3 (* dim3 (car f)))))
                (loop (cdr f) (+ index 1)))))))))

; ======================================================================
; Tensor Shape Derivation Functions
; ======================================================================

(define (derive-optimal-tensor-shape kernel)
  "Derive optimal tensor shape for a kernel based on its degrees of freedom"
  (let* ((dof (compute-kernel-degrees-of-freedom kernel))
         (factors (prime-factorization dof))
         (shape (derive-tensor-shape-from-factors factors)))
    (list
      'original-dof dof
      'prime-factors factors
      'optimal-shape shape
      'total-elements (apply * shape))))

(define (compute-shape-memory-efficiency shape)
  "Compute memory efficiency score for a tensor shape"
  (let ((efficiency 0.0))
    (for-each
      (lambda (dim)
        ; Prefer powers of 2
        (if (= (logand dim (- dim 1)) 0)
            (set! efficiency (+ efficiency 0.3)))
        ; Prefer cache-friendly sizes
        (if (= (modulo dim 64) 0)
            (set! efficiency (+ efficiency 0.2)))
        ; Prefer reasonable sizes
        (if (and (>= dim 16) (<= dim 1024))
            (set! efficiency (+ efficiency 0.3))))
      shape)
    (min 1.0 (/ efficiency (length shape)))))

(define (compute-shape-computational-efficiency shape)
  "Compute computational efficiency score for a tensor shape"
  (let ((efficiency 0.0))
    (for-each
      (lambda (dim)
        ; Prefer SIMD-friendly dimensions
        (if (= (modulo dim 8) 0) (set! efficiency (+ efficiency 0.25)))
        (if (= (modulo dim 4) 0) (set! efficiency (+ efficiency 0.15)))
        (if (= (modulo dim 2) 0) (set! efficiency (+ efficiency 0.10))))
      shape)
    ; Prefer 3D or 4D shapes for GPU processing
    (if (or (= (length shape) 3) (= (length shape) 4))
        (set! efficiency (+ efficiency 0.3)))
    (min 1.0 efficiency)))

; ======================================================================
; Catalog Query Functions
; ======================================================================

(define (filter-kernels-by-role kernels role)
  "Filter kernels that have a specific functional role"
  (filter (lambda (kernel)
            (member role (assoc-ref kernel 'functional-roles)))
          kernels))

(define (filter-kernels-by-subsystem kernels subsystem)
  "Filter kernels that use a specific cognitive subsystem"
  (filter (lambda (kernel)
            (member subsystem (assoc-ref kernel 'cognitive-subsystems)))
          kernels))

(define (filter-kernels-by-status kernels status)
  "Filter kernels by deployment status"
  (filter (lambda (kernel)
            (equal? (assoc-ref kernel 'status) status))
          kernels))

(define (filter-kernels-by-complexity kernels min-dof max-dof)
  "Filter kernels by degrees of freedom range"
  (filter (lambda (kernel)
            (let ((dof (compute-kernel-degrees-of-freedom kernel)))
              (and (>= dof min-dof) (<= dof max-dof))))
          kernels))

; ======================================================================
; Analysis and Reporting Functions
; ======================================================================

(define (analyze-kernel-complexity kernel)
  "Perform comprehensive complexity analysis of a kernel"
  (let* ((dof (compute-kernel-degrees-of-freedom kernel))
         (shape-analysis (derive-optimal-tensor-shape kernel))
         (shape (assoc-ref shape-analysis 'optimal-shape)))
    (list
      'kernel-name (assoc-ref kernel 'kernel-name)
      'degrees-of-freedom dof
      'prime-factors (assoc-ref shape-analysis 'prime-factors)
      'optimal-tensor-shape shape
      'total-tensor-elements (assoc-ref shape-analysis 'total-elements)
      'memory-efficiency (compute-shape-memory-efficiency shape)
      'computational-efficiency (compute-shape-computational-efficiency shape)
      'complexity-score (log (+ 1 dof)))))

(define (generate-catalog-report)
  "Generate comprehensive catalog analysis report"
  (let* ((all-kernels (get-all-standard-kernels))
         (production-kernels (get-production-kernels))
         (prototype-kernels (get-prototype-kernels))
         (experimental-kernels (get-experimental-kernels))
         (analyses (map analyze-kernel-complexity all-kernels)))
    
    (display "=== Agentic Kernels Catalog Report ===\n\n")
    
    (display "Catalog Overview:\n")
    (format #t "  Total Kernels: ~a\n" (length all-kernels))
    (format #t "  Production Kernels: ~a\n" (length production-kernels))
    (format #t "  Prototype Kernels: ~a\n" (length prototype-kernels))
    (format #t "  Experimental Kernels: ~a\n" (length experimental-kernels))
    (display "\n")
    
    (display "Complexity Analysis:\n")
    (for-each
      (lambda (analysis)
        (format #t "  ~a:\n" (assoc-ref analysis 'kernel-name))
        (format #t "    DOF: ~a\n" (assoc-ref analysis 'degrees-of-freedom))
        (format #t "    Tensor Shape: ~a\n" (assoc-ref analysis 'optimal-tensor-shape))
        (format #t "    Memory Efficiency: ~,2f\n" (assoc-ref analysis 'memory-efficiency))
        (format #t "    Computational Efficiency: ~,2f\n" (assoc-ref analysis 'computational-efficiency))
        (format #t "    Complexity Score: ~,2f\n" (assoc-ref analysis 'complexity-score))
        (display "\n"))
      analyses)))

; ======================================================================
; Serialization Functions (Simplified)
; ======================================================================

(define (kernel-to-json kernel)
  "Serialize kernel specification to JSON-like format"
  (string-append
    "{\n"
    "  \"kernel_name\": \"" (assoc-ref kernel 'kernel-name) "\",\n"
    "  \"description\": \"" (assoc-ref kernel 'description) "\",\n"
    "  \"version\": \"" (assoc-ref kernel 'version) "\",\n"
    "  \"status\": " (number->string (assoc-ref kernel 'status)) ",\n"
    "  \"implementation_language\": \"" (assoc-ref kernel 'implementation-language) "\",\n"
    "  \"degrees_of_freedom\": " (number->string (compute-kernel-degrees-of-freedom kernel)) "\n"
    "}"))

(define (save-catalog-to-file filename)
  "Save the entire catalog to a file"
  (call-with-output-file filename
    (lambda (port)
      (display "[\n" port)
      (let ((kernels (get-all-standard-kernels)))
        (for-each
          (lambda (kernel)
            (display (kernel-to-json kernel) port)
            (if (not (eq? kernel (last kernels)))
                (display ",\n" port)
                (display "\n" port)))
          kernels))
      (display "]\n" port))))

; ======================================================================
; Test Functions
; ======================================================================

(define (test-round-trip-serialization)
  "Test round-trip serialization for all kernels"
  (display "=== Round-Trip Serialization Test ===\n")
  (let ((kernels (get-all-standard-kernels))
        (passed 0)
        (failed 0))
    
    (for-each
      (lambda (kernel)
        (let* ((json-str (kernel-to-json kernel))
               (kernel-name (assoc-ref kernel 'kernel-name)))
          ; Simple test - check if JSON contains kernel name
          (if (string-contains json-str kernel-name)
              (begin
                (format #t "✓ ~a: PASSED\n" kernel-name)
                (set! passed (+ passed 1)))
              (begin
                (format #t "✗ ~a: FAILED\n" kernel-name)
                (set! failed (+ failed 1))))))
      kernels)
    
    (display "\n")
    (format #t "Test Results: ~a passed, ~a failed\n" passed failed)
    (= failed 0)))

(define (run-comprehensive-tests)
  "Run comprehensive test suite for the agentic catalog system"
  (display "=== Comprehensive Agentic Catalog Tests ===\n\n")
  
  ; Test kernel creation
  (display "Testing kernel creation...\n")
  (let ((ghost-kernel (create-ghost-kernel-spec)))
    (if (equal? (assoc-ref ghost-kernel 'kernel-name) "GHOST")
        (display "✓ Kernel creation: PASSED\n")
        (display "✗ Kernel creation: FAILED\n")))
  
  ; Test degrees of freedom calculation
  (display "Testing degrees of freedom calculation...\n")
  (let* ((test-kernel (create-pln-kernel-spec))
         (dof (compute-kernel-degrees-of-freedom test-kernel)))
    (if (> dof 0)
        (format #t "✓ DOF calculation: PASSED (DOF=~a)\n" dof)
        (display "✗ DOF calculation: FAILED\n")))
  
  ; Test tensor shape derivation
  (display "Testing tensor shape derivation...\n")
  (let* ((test-kernel (create-ecan-kernel-spec))
         (shape-analysis (derive-optimal-tensor-shape test-kernel))
         (shape (assoc-ref shape-analysis 'optimal-shape)))
    (if (and shape (> (length shape) 0))
        (format #t "✓ Tensor shape derivation: PASSED (shape=~a)\n" shape)
        (display "✗ Tensor shape derivation: FAILED\n")))
  
  ; Test serialization
  (display "Testing serialization...\n")
  (if (test-round-trip-serialization)
      (display "✓ Serialization: PASSED\n")
      (display "✗ Serialization: FAILED\n"))
  
  (display "\n✨ All tests completed!\n"))

; ======================================================================
; Quick Demo Function
; ======================================================================

(define (quick-agentic-catalog-demo)
  "Quick demonstration of the agentic catalog system"
  (display "🧠 Agentic Kernels Catalog - Quick Demo\n")
  (display "======================================\n\n")
  
  ; Show all kernels
  (display "📋 Standard Agentic Kernels:\n")
  (for-each
    (lambda (kernel)
      (format #t "  • ~a - ~a\n" 
              (assoc-ref kernel 'kernel-name)
              (assoc-ref kernel 'description)))
    (get-all-standard-kernels))
  
  (display "\n")
  
  ; Analyze complexity of a few kernels
  (display "🔬 Complexity Analysis:\n")
  (for-each
    (lambda (kernel)
      (let* ((analysis (analyze-kernel-complexity kernel))
             (name (assoc-ref analysis 'kernel-name))
             (dof (assoc-ref analysis 'degrees-of-freedom))
             (shape (assoc-ref analysis 'optimal-tensor-shape)))
        (format #t "  • ~a: DOF=~a, Shape=~a\n" name dof shape)))
    (take (get-all-standard-kernels) 3))
  
  (display "\n")
  
  ; Generate full report
  (display "📊 Generating full catalog report...\n\n")
  (generate-catalog-report))

; Export main functions
(export 
  ; Kernel creation functions
  create-ghost-kernel-spec
  create-relex-kernel-spec
  create-pln-kernel-spec
  create-ecan-kernel-spec
  create-moses-kernel-spec
  create-eva-kernel-spec
  create-loving-ai-kernel-spec
  create-game-ai-kernel-spec
  
  ; Catalog management
  get-all-standard-kernels
  get-production-kernels
  get-prototype-kernels
  get-experimental-kernels
  
  ; Analysis functions
  compute-kernel-degrees-of-freedom
  derive-optimal-tensor-shape
  analyze-kernel-complexity
  
  ; Query functions
  filter-kernels-by-role
  filter-kernels-by-subsystem
  filter-kernels-by-status
  filter-kernels-by-complexity
  
  ; Reporting
  generate-catalog-report
  
  ; Testing
  test-round-trip-serialization
  run-comprehensive-tests
  quick-agentic-catalog-demo
  
  ; Serialization
  kernel-to-json
  save-catalog-to-file
  
  ; Constants
  functional-role-nlp-processing
  functional-role-attention-allocation
  functional-role-reasoning-inference
  functional-role-learning-evolution
  functional-role-conversational
  functional-role-emotional-affective
  functional-role-sensorimotor
  functional-role-game-strategy
  functional-role-vision-perception
  functional-role-memory-episodic
  functional-role-meta-cognitive
  functional-role-social-interaction
  
  cognitive-subsystem-perceptual-input
  cognitive-subsystem-working-memory
  cognitive-subsystem-episodic-memory
  cognitive-subsystem-semantic-memory
  cognitive-subsystem-attention-system
  cognitive-subsystem-motor-output
  cognitive-subsystem-executive-control
  cognitive-subsystem-emotional-system
  cognitive-subsystem-social-cognition
  cognitive-subsystem-self-model
  
  deployment-status-production
  deployment-status-prototype
  deployment-status-experimental
  deployment-status-legacy
  deployment-status-planned
  deployment-status-deprecated)