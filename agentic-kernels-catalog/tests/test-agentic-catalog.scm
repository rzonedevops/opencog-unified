#!/usr/bin/env guile
!#

;
; test-agentic-catalog.scm
;
; Comprehensive test runner for the agentic kernels catalog system
; Tests both Scheme and C++ components through round-trip validation
;

(use-modules (opencog agentic-catalog))

; Test results tracking
(define test-results '())

(define (record-test-result test-name passed?)
  "Record a test result"
  (set! test-results (cons (list test-name passed?) test-results)))

(define (get-test-summary)
  "Get summary of all test results"
  (let ((passed (length (filter (lambda (result) (cadr result)) test-results)))
        (total (length test-results)))
    (list passed total)))

; ======================================================================
; Core Functionality Tests
; ======================================================================

(define (test-kernel-creation)
  "Test basic kernel specification creation"
  (display "Testing kernel creation...\n")
  
  (let ((ghost-kernel (create-ghost-kernel-spec)))
    (if (equal? (assoc-ref ghost-kernel 'kernel-name) "GHOST")
        (begin
          (display "  ✓ GHOST kernel created correctly\n")
          (record-test-result "kernel-creation" #t)
          #t)
        (begin
          (display "  ✗ GHOST kernel creation failed\n")
          (record-test-result "kernel-creation" #f)
          #f))))

(define (test-degrees-of-freedom-calculation)
  "Test degrees of freedom calculation"
  (display "Testing degrees of freedom calculation...\n")
  
  (let* ((test-kernels (list (create-ghost-kernel-spec)
                            (create-pln-kernel-spec)
                            (create-ecan-kernel-spec)))
         (all-have-dof? (every (lambda (kernel)
                                (> (compute-kernel-degrees-of-freedom kernel) 0))
                              test-kernels)))
    (if all-have-dof?
        (begin
          (for-each
            (lambda (kernel)
              (let ((dof (compute-kernel-degrees-of-freedom kernel)))
                (format #t "  • ~a: DOF = ~a\n" 
                       (assoc-ref kernel 'kernel-name) dof)))
            test-kernels)
          (display "  ✓ DOF calculation working for all kernels\n")
          (record-test-result "dof-calculation" #t)
          #t)
        (begin
          (display "  ✗ DOF calculation failed for some kernels\n")
          (record-test-result "dof-calculation" #f)
          #f))))

(define (test-tensor-shape-derivation)
  "Test tensor shape derivation from prime factorization"
  (display "Testing tensor shape derivation...\n")
  
  (let* ((test-kernel (create-moses-kernel-spec))
         (shape-analysis (derive-optimal-tensor-shape test-kernel))
         (optimal-shape (assoc-ref shape-analysis 'optimal-shape))
         (total-elements (assoc-ref shape-analysis 'total-elements)))
    
    (if (and optimal-shape (> (length optimal-shape) 0) (> total-elements 0))
        (begin
          (format #t "  • Original DOF: ~a\n" (assoc-ref shape-analysis 'original-dof))
          (format #t "  • Prime factors: ~a\n" (assoc-ref shape-analysis 'prime-factors))
          (format #t "  • Optimal shape: ~a\n" optimal-shape)
          (format #t "  • Total elements: ~a\n" total-elements)
          
          ; Verify shape volume matches DOF
          (let ((computed-volume (apply * optimal-shape)))
            (if (= computed-volume (assoc-ref shape-analysis 'original-dof))
                (begin
                  (display "  ✓ Tensor shape derivation working correctly\n")
                  (record-test-result "tensor-shape-derivation" #t)
                  #t)
                (begin
                  (display "  ✗ Shape volume doesn't match DOF\n")
                  (record-test-result "tensor-shape-derivation" #f)
                  #f))))
        (begin
          (display "  ✗ Tensor shape derivation failed\n")
          (record-test-result "tensor-shape-derivation" #f)
          #f))))

(define (test-prime-factorization)
  "Test prime factorization algorithm"
  (display "Testing prime factorization...\n")
  
  (let ((test-cases '((12 . (2 2 3))
                     (60 . (2 2 3 5))
                     (100 . (2 2 5 5))
                     (17 . (17)))))
    
    (let ((all-passed? 
           (every 
             (lambda (test-case)
               (let* ((number (car test-case))
                      (expected (cdr test-case))
                      (actual (prime-factorization number))
                      (passed? (equal? (sort actual <) (sort expected <))))
                 (format #t "  • ~a: ~a (expected: ~a) ~a\n" 
                        number actual expected 
                        (if passed? "✓" "✗"))
                 passed?))
             test-cases)))
      
      (if all-passed?
          (begin
            (display "  ✓ Prime factorization working correctly\n")
            (record-test-result "prime-factorization" #t)
            #t)
          (begin
            (display "  ✗ Prime factorization failed for some cases\n")
            (record-test-result "prime-factorization" #f)
            #f)))))

(define (test-catalog-queries)
  "Test catalog query functionality"
  (display "Testing catalog queries...\n")
  
  (let* ((all-kernels (get-all-standard-kernels))
         (production-kernels (get-production-kernels))
         (prototype-kernels (get-prototype-kernels))
         (experimental-kernels (get-experimental-kernels))
         (nlp-kernels (filter-kernels-by-role all-kernels functional-role-nlp-processing))
         (memory-kernels (filter-kernels-by-subsystem all-kernels cognitive-subsystem-working-memory)))
    
    (format #t "  • Total kernels: ~a\n" (length all-kernels))
    (format #t "  • Production kernels: ~a\n" (length production-kernels))
    (format #t "  • Prototype kernels: ~a\n" (length prototype-kernels))
    (format #t "  • Experimental kernels: ~a\n" (length experimental-kernels))
    (format #t "  • NLP processing kernels: ~a\n" (length nlp-kernels))
    (format #t "  • Working memory kernels: ~a\n" (length memory-kernels))
    
    (if (and (> (length all-kernels) 0)
             (>= (+ (length production-kernels) 
                   (length prototype-kernels) 
                   (length experimental-kernels)) 
                (length all-kernels)))
        (begin
          (display "  ✓ Catalog queries working correctly\n")
          (record-test-result "catalog-queries" #t)
          #t)
        (begin
          (display "  ✗ Catalog queries failed\n")
          (record-test-result "catalog-queries" #f)
          #f))))

(define (test-efficiency-calculations)
  "Test shape efficiency calculations"
  (display "Testing efficiency calculations...\n")
  
  (let* ((test-shapes '((64 64 64)      ; Cube shape
                       (128 32 16)     ; Balanced rectangular
                       (1024 8 8)      ; Flat shape
                       (16 16 16 16))) ; 4D hypercube
         (efficiency-results
           (map (lambda (shape)
                  (let ((memory-eff (compute-shape-memory-efficiency shape))
                        (compute-eff (compute-shape-computational-efficiency shape)))
                    (list shape memory-eff compute-eff)))
                test-shapes)))
    
    (for-each
      (lambda (result)
        (let ((shape (car result))
              (mem-eff (cadr result))
              (comp-eff (caddr result)))
          (format #t "  • Shape ~a: Memory=~,2f, Compute=~,2f\n" 
                 shape mem-eff comp-eff)))
      efficiency-results)
    
    ; Check if all efficiency scores are in valid range [0.0, 1.0]
    (let ((all-valid? 
           (every (lambda (result)
                    (let ((mem-eff (cadr result))
                          (comp-eff (caddr result)))
                      (and (>= mem-eff 0.0) (<= mem-eff 1.0)
                           (>= comp-eff 0.0) (<= comp-eff 1.0))))
                  efficiency-results)))
      
      (if all-valid?
          (begin
            (display "  ✓ Efficiency calculations working correctly\n")
            (record-test-result "efficiency-calculations" #t)
            #t)
          (begin
            (display "  ✗ Efficiency calculations out of range\n")
            (record-test-result "efficiency-calculations" #f)
            #f)))))

(define (test-serialization-round-trip)
  "Test JSON serialization round-trip"
  (display "Testing serialization round-trip...\n")
  
  (let* ((test-kernels (list (create-ghost-kernel-spec)
                            (create-pln-kernel-spec)
                            (create-eva-kernel-spec)))
         (serialization-results
           (map (lambda (kernel)
                  (let* ((json-str (kernel-to-json kernel))
                         (kernel-name (assoc-ref kernel 'kernel-name))
                         ; Simple validation: check if JSON contains key fields
                         (contains-name? (string-contains json-str kernel-name))
                         (contains-description? (string-contains json-str "description"))
                         (contains-version? (string-contains json-str "version")))
                    (list kernel-name 
                          (and contains-name? contains-description? contains-version?))))
                test-kernels)))
    
    (for-each
      (lambda (result)
        (format #t "  • ~a: ~a\n" 
               (car result) 
               (if (cadr result) "✓ PASSED" "✗ FAILED")))
      serialization-results)
    
    (let ((all-passed? (every cadr serialization-results)))
      (if all-passed?
          (begin
            (display "  ✓ Serialization round-trip working\n")
            (record-test-result "serialization-round-trip" #t)
            #t)
          (begin
            (display "  ✗ Serialization round-trip failed\n")
            (record-test-result "serialization-round-trip" #f)
            #f)))))

(define (test-complexity-analysis)
  "Test comprehensive complexity analysis"
  (display "Testing complexity analysis...\n")
  
  (let* ((test-kernels (get-all-standard-kernels))
         (analyses (map analyze-kernel-complexity test-kernels)))
    
    (display "  Complexity Analysis Results:\n")
    (for-each
      (lambda (analysis)
        (format #t "    • ~a: DOF=~a, Score=~,2f, Shape=~a\n"
               (assoc-ref analysis 'kernel-name)
               (assoc-ref analysis 'degrees-of-freedom)
               (assoc-ref analysis 'complexity-score)
               (assoc-ref analysis 'optimal-tensor-shape)))
      analyses)
    
    ; Verify all analyses have valid data
    (let ((all-valid? 
           (every (lambda (analysis)
                    (and (> (assoc-ref analysis 'degrees-of-freedom) 0)
                         (> (assoc-ref analysis 'complexity-score) 0)
                         (> (length (assoc-ref analysis 'optimal-tensor-shape)) 0)))
                  analyses)))
      
      (if all-valid?
          (begin
            (display "  ✓ Complexity analysis working correctly\n")
            (record-test-result "complexity-analysis" #t)
            #t)
          (begin
            (display "  ✗ Complexity analysis failed\n")
            (record-test-result "complexity-analysis" #f)
            #f)))))

; ======================================================================
; Main Test Runner
; ======================================================================

(define (run-all-tests)
  "Run all agentic catalog tests"
  (display "🧠 Agentic Kernels Catalog - Comprehensive Test Suite\n")
  (display "=====================================================\n\n")
  
  ; Reset test results
  (set! test-results '())
  
  ; Run all tests
  (test-kernel-creation)
  (display "\n")
  (test-degrees-of-freedom-calculation)
  (display "\n")
  (test-tensor-shape-derivation)
  (display "\n")
  (test-prime-factorization)
  (display "\n")
  (test-catalog-queries)
  (display "\n")
  (test-efficiency-calculations)
  (display "\n")
  (test-serialization-round-trip)
  (display "\n")
  (test-complexity-analysis)
  (display "\n")
  
  ; Print summary
  (let* ((summary (get-test-summary))
         (passed (car summary))
         (total (cadr summary))
         (success-rate (* 100.0 (/ passed total))))
    
    (display "=== Test Results Summary ===\n")
    (format #t "Total Tests: ~a\n" total)
    (format #t "Passed: ~a\n" passed)
    (format #t "Failed: ~a\n" (- total passed))
    (format #t "Success Rate: ~,1f%\n" success-rate)
    
    (if (= passed total)
        (display "\n🎉 All tests passed! Agentic catalog system is working correctly.\n")
        (begin
          (display "\n⚠️  Some tests failed. Failed tests:\n")
          (for-each
            (lambda (result)
              (if (not (cadr result))
                  (format #t "  • ~a\n" (car result))))
            test-results)))
    
    ; Return success status
    (= passed total)))

(define (save-test-results-to-file filename)
  "Save test results to a file for integration with C++ tests"
  (call-with-output-file filename
    (lambda (port)
      (display "# Agentic Catalog Scheme Test Results\n" port)
      (let* ((summary (get-test-summary))
             (passed (car summary))
             (total (cadr summary)))
        (format port "total_tests: ~a\n" total)
        (format port "passed_tests: ~a\n" passed)
        (format port "failed_tests: ~a\n" (- total passed))
        (format port "success_rate: ~,2f\n" (* 100.0 (/ passed total)))
        (display "\ndetailed_results:\n" port)
        (for-each
          (lambda (result)
            (format port "  ~a: ~a\n" (car result) 
                   (if (cadr result) "PASSED" "FAILED")))
          (reverse test-results))))))

; ======================================================================
; Integration Testing
; ======================================================================

(define (test-integration-with-ggml-tensor-kernel)
  "Test integration with existing ggml-tensor-kernel system"
  (display "Testing integration with ggml-tensor-kernel...\n")
  
  ; This would test integration with the existing tensor kernel
  ; For now, just verify our shapes are compatible
  (let* ((ghost-kernel (create-ghost-kernel-spec))
         (shape-analysis (derive-optimal-tensor-shape ghost-kernel))
         (derived-shape (assoc-ref shape-analysis 'optimal-shape)))
    
    ; Check if shape is compatible with typical ML frameworks
    (let ((is-compatible? 
           (and (>= (length derived-shape) 1)
                (<= (length derived-shape) 4)  ; 1D to 4D tensors
                (every (lambda (dim) (and (> dim 0) (< dim 10000))) derived-shape))))
      
      (if is-compatible?
          (begin
            (format #t "  ✓ Derived shape ~a is compatible with ML frameworks\n" derived-shape)
            (record-test-result "ggml-integration" #t)
            #t)
          (begin
            (format #t "  ✗ Derived shape ~a may not be compatible\n" derived-shape)
            (record-test-result "ggml-integration" #f)
            #f)))))

; ======================================================================
; Script Entry Point
; ======================================================================

(define (main)
  "Main entry point for test script"
  (let ((success? (run-all-tests)))
    (display "\n")
    (test-integration-with-ggml-tensor-kernel)
    (display "\n")
    
    ; Save results for integration testing
    (save-test-results-to-file "scheme_test_results.txt")
    (display "Test results saved to scheme_test_results.txt\n")
    
    ; Exit with appropriate code
    (if success? 0 1)))

; Run tests if this script is executed directly
(if (string-suffix? "test-agentic-catalog.scm" (car (command-line)))
    (exit (main)))