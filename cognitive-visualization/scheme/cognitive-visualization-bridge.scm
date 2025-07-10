;
; cognitive-visualization-bridge.scm
;
; Scheme bridge for real-time AtomSpace visualization
; Connects live cognitive data to the visualization system
;

(use-modules (opencog))
(use-modules (opencog exec))
(use-modules (srfi srfi-1))
(use-modules (ice-9 format))

; Global visualization state
(define cognitive-visualization-state (make-hash-table))

; AtomSpace introspection functions
(define (get-atomspace-statistics)
  "Get current AtomSpace statistics for visualization"
  (let ((all-atoms (cog-get-atoms 'Atom)))
    (list 
      (cons 'total-atoms (length all-atoms))
      (cons 'node-count (length (filter cog-node? all-atoms)))
      (cons 'link-count (length (filter cog-link? all-atoms)))
      (cons 'attention-focus-size (length (get-attentional-focus)))
      (cons 'cognitive-load (calculate-cognitive-load all-atoms)))))

(define (get-attentional-focus . threshold)
  "Get atoms currently in attentional focus based on STI values"
  (let ((min-sti (if (null? threshold) 0 (car threshold)))
        (all-atoms (cog-get-atoms 'Atom)))
    (filter (lambda (atom)
              (let ((sti (cog-av-sti atom)))
                (and sti (> sti min-sti))))
            all-atoms)))

(define (calculate-cognitive-load atoms)
  "Calculate overall cognitive load metric"
  (if (null? atoms)
      0.0
      (let ((total-sti (fold + 0 (map cog-av-sti atoms)))
            (total-lti (fold + 0 (map cog-av-lti atoms))))
        (+ (* 0.7 (/ total-sti (length atoms)))
           (* 0.3 (/ total-lti (length atoms)))))))

(define (extract-attention-values atoms)
  "Extract attention values for visualization"
  (map (lambda (atom)
         (list (cog-name atom)
               (cog-av-sti atom)
               (cog-av-lti atom)
               (cog-av-vlti atom)))
       atoms))

(define (get-hypergraph-structure . max-depth)
  "Extract hypergraph structure for visualization"
  (let ((depth (if (null? max-depth) 3 (car max-depth)))
        (nodes (cog-get-atoms 'Node))
        (links (cog-get-atoms 'Link)))
    
    ; Create node data
    (define node-data
      (map (lambda (node)
             (list 'node
                   (cog-name node)
                   (cog-type node)
                   (cog-av-sti node)
                   (length (cog-incoming-set node))
                   (length (cog-outgoing-set node))))
           nodes))
    
    ; Create link data
    (define link-data
      (map (lambda (link)
             (list 'link
                   (cog-name link)
                   (cog-type link)
                   (map cog-name (cog-outgoing-set link))
                   (cog-av-sti link)))
           links))
    
    (list (cons 'nodes node-data)
          (cons 'links link-data))))

(define (detect-cognitive-patterns)
  "Detect emergent cognitive patterns in the AtomSpace"
  (let ((patterns '()))
    
    ; Pattern 1: High attention clusters
    (let ((high-attention-atoms (get-attentional-focus 100)))
      (when (> (length high-attention-atoms) 2)
        (set! patterns (cons (list 'attention-cluster high-attention-atoms) patterns))))
    
    ; Pattern 2: Frequently co-occurring concepts
    (let ((concept-nodes (cog-get-atoms 'ConceptNode)))
      (define co-occurrence-patterns
        (filter (lambda (pair)
                  (> (count-co-occurrences (car pair) (cadr pair)) 3))
                (combinations concept-nodes 2)))
      (when (not (null? co-occurrence-patterns))
        (set! patterns (cons (list 'co-occurrence co-occurrence-patterns) patterns))))
    
    ; Pattern 3: Inheritance chains
    (let ((inheritance-links (cog-get-atoms 'InheritanceLink)))
      (define long-chains (find-inheritance-chains inheritance-links 3))
      (when (not (null? long-chains))
        (set! patterns (cons (list 'inheritance-chains long-chains) patterns))))
    
    patterns))

(define (count-co-occurrences atom1 atom2)
  "Count how often two atoms appear together in links"
  (let ((links1 (cog-incoming-set atom1))
        (links2 (cog-incoming-set atom2)))
    (length (lset-intersection equal? links1 links2))))

(define (combinations lst k)
  "Generate all k-combinations of a list"
  (cond ((= k 0) '(()))
        ((null? lst) '())
        (else (append (map (lambda (x) (cons (car lst) x))
                          (combinations (cdr lst) (- k 1)))
                     (combinations (cdr lst) k)))))

(define (find-inheritance-chains links min-length)
  "Find inheritance chains of at least min-length"
  (define (build-chain link visited)
    (let ((target (cadr (cog-outgoing-set link))))
      (if (member target visited)
          (list link)  ; Cycle detected, stop
          (let ((next-links (filter (lambda (l)
                                     (equal? (car (cog-outgoing-set l)) target))
                                   links)))
            (if (null? next-links)
                (list link)  ; End of chain
                (cons link (apply append 
                                 (map (lambda (l) (build-chain l (cons target visited)))
                                      next-links))))))))
  
  (filter (lambda (chain) (>= (length chain) min-length))
          (map (lambda (link) (build-chain link '())) links)))

(define (simulate-agent-interactions agents)
  "Simulate cognitive agent interactions"
  (map (lambda (agent)
         (let ((agent-state (random-state-vector 3))
               (agent-frequency (+ 5.0 (* 10.0 (random:uniform)))))
           (list 'agent
                 (format #f "agent_~a" agent)
                 agent-state
                 agent-frequency
                 (simulate-agent-activity agent-state))))
       agents))

(define (random-state-vector size)
  "Generate random state vector"
  (map (lambda (_) (random:uniform)) (iota size)))

(define (simulate-agent-activity state)
  "Simulate agent activity based on state"
  (let ((activity-level (fold + 0 state)))
    (> activity-level 1.5)))

(define (export-visualization-data)
  "Export complete visualization data as association list"
  (let ((atomspace-stats (get-atomspace-statistics))
        (hypergraph-structure (get-hypergraph-structure))
        (attention-data (extract-attention-values (get-attentional-focus)))
        (cognitive-patterns (detect-cognitive-patterns))
        (agent-data (simulate-agent-interactions '(alpha beta gamma))))
    
    (list (cons 'timestamp (current-time))
          (cons 'atomspace-statistics atomspace-stats)
          (cons 'hypergraph-structure hypergraph-structure)
          (cons 'attention-data attention-data)
          (cons 'cognitive-patterns cognitive-patterns)
          (cons 'agent-data agent-data)
          (cons 'system-load (calculate-system-load)))))

(define (calculate-system-load)
  "Calculate current system computational load"
  ; Simplified system load calculation
  (let ((total-atoms (length (cog-get-atoms 'Atom)))
        (active-processes 3))  ; Simplified metric
    (/ total-atoms (+ 100 active-processes))))

; WebSocket/HTTP bridge functions for real-time updates
(define (start-visualization-bridge port)
  "Start the visualization data bridge on specified port"
  (format #t "🚀 Starting cognitive visualization bridge on port ~a~%" port)
  
  ; In a full implementation, this would start a web server
  ; For now, we simulate periodic data export
  (call-with-new-thread
    (lambda ()
      (let loop ()
        (let ((viz-data (export-visualization-data)))
          (hash-set! cognitive-visualization-state 'latest-data viz-data)
          (format #t "📊 Visualization data updated: ~a atoms, ~a patterns~%"
                 (assoc-ref (assoc-ref viz-data 'atomspace-statistics) 'total-atoms)
                 (length (assoc-ref viz-data 'cognitive-patterns))))
        (sleep 1)  ; Update every second
        (loop)))))

(define (get-latest-visualization-data)
  "Get the latest visualization data"
  (hash-ref cognitive-visualization-state 'latest-data '()))

; Pattern matcher integration
(define (visualize-pattern-matching query)
  "Visualize pattern matching execution"
  (format #t "🔍 Visualizing pattern match for: ~a~%" query)
  
  ; Execute pattern match with step-by-step tracking
  (let ((start-time (get-internal-real-time))
        (result '())
        (steps '()))
    
    ; Simulate pattern matching steps
    (set! steps (cons "Parsing query pattern" steps))
    (set! steps (cons "Finding candidate atoms" steps))
    (set! steps (cons "Applying unification" steps))
    (set! steps (cons "Validating bindings" steps))
    
    ; Execute the actual query (simplified)
    (set! result (cog-execute! query))
    
    (let ((end-time (get-internal-real-time))
          (execution-time (/ (- end-time start-time) internal-time-units-per-second)))
      
      (list (cons 'query query)
            (cons 'result result)
            (cons 'execution-steps steps)
            (cons 'execution-time execution-time)
            (cons 'candidate-count (length (cog-get-atoms 'Atom)))
            (cons 'match-count (if (cog-atom? result) 1 0))))))

; ECAN economic simulation for visualization
(define (simulate-ecan-economics cycles)
  "Simulate ECAN economic dynamics for visualization"
  (format #t "💰 Simulating ECAN economics for ~a cycles~%" cycles)
  
  (let ((sti-bank 100000)
        (economic-history '()))
    
    (do ((i 0 (+ i 1)))
        ((>= i cycles))
      
      ; Simulate one economic cycle
      (let ((wage-distribution (distribute-wages sti-bank))
            (rent-collection (collect-rent))
            (spreading-activation (apply-spreading-activation)))
        
        ; Update STI bank
        (set! sti-bank (- (+ sti-bank rent-collection) wage-distribution))
        
        ; Record economic state
        (set! economic-history 
              (cons (list 'cycle i
                         'sti-bank sti-bank
                         'wages-distributed wage-distribution
                         'rent-collected rent-collection
                         'spreading-factor spreading-activation)
                    economic-history))))
    
    (reverse economic-history)))

(define (distribute-wages bank-sti)
  "Simulate STI wage distribution"
  (* 0.1 bank-sti (random:uniform)))

(define (collect-rent)
  "Simulate STI rent collection"
  (* 500 (random:uniform)))

(define (apply-spreading-activation)
  "Simulate spreading activation factor"
  (+ 0.5 (* 0.3 (random:uniform))))

; Initialize the bridge
(define (init-cognitive-visualization-bridge)
  "Initialize the cognitive visualization bridge"
  (format #t "🎨 Initializing Cognitive Visualization Bridge~%")
  (format #t "   AtomSpace size: ~a atoms~%" (length (cog-get-atoms 'Atom)))
  (format #t "   Attentional focus: ~a atoms~%" (length (get-attentional-focus)))
  (format #t "   Bridge ready for real-time visualization~%"))

; Export main interface functions
(export get-atomspace-statistics
        get-attentional-focus
        get-hypergraph-structure
        detect-cognitive-patterns
        export-visualization-data
        start-visualization-bridge
        visualize-pattern-matching
        simulate-ecan-economics
        init-cognitive-visualization-bridge)