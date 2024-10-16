; Construction of a ProB file from the composition of reusable components.
; Author: Richard St-Denis, Universite de Sherbrooke, 2023.
#lang racket
(require racket/string)
(require json)

; Auxiliary functions
(define (str2sym-transition t)
  (list (list (string->symbol(car t)))        ; source state
        (string->symbol(cadr t))              ; event
        (list (string->symbol(caddr t)))) )   ; target state

; Read a component from a JSON file
 (define (read-automaton directoryname filename)
;  (let* ([ip (open-input-file filename)]
  (let* ([ip (open-input-file (string-append directoryname "/" filename ".json"))]
         [h (read-json ip)]
         [n (list (hash-ref h 'name))]
         [s (map list (map string->symbol (hash-ref h 'states)))]
         [a (flatten s)]
         [e (map string->symbol (hash-ref h 'events))]
         [i (list (string->symbol (hash-ref h 'istate)))]
         [m (map list (map string->symbol (hash-ref h 'mstates)))]
         [c (map string->symbol (hash-ref h 'cevents))]
         [t (map str2sym-transition (hash-ref h 'transitions))] )
    (close-input-port ip)
    (values n a s i m e c t) ) )

; Convert a list of atomic states <s1 s2 ... sn> to a single string "s1-s2- ... -sn"
(define (state2str s)
  (string-join (map (lambda (x) (symbol->string x)) s) "_") )

(define display-states
  (lambda (op ls i)
    (cond [(eq? (length ls) 1)
           (fprintf op "~a" (state2str (car ls))) ]
          [(eq? (remainder i 3 ) 0)
           (fprintf op "~a,\n           " (state2str (car ls)))
           (display-states op (cdr ls) (+ i 1)) ]
          [else (fprintf op "~a, " (state2str (car ls)))
                (display-states op (cdr ls) (+ i 1)) ] ) ) )

(define display-transitions
  (lambda (op lt i)
    (cond [(eq? (length lt) 1)
           (fprintf op "(~a,~a,~a)" (state2str (caar lt)) (cadar lt) (state2str (caddar lt))) ]
          [(eq? (remainder i 3 ) 0)
           (fprintf op "(~a,~a,~a),\n                 " (state2str (caar lt)) (cadar lt) (state2str (caddar lt)))
           (display-transitions op (cdr lt) (+ i 1)) ]
          [else (fprintf op "(~a,~a,~a), " (state2str (caar lt)) (cadar lt) (state2str (caddar lt)))
                (display-transitions op (cdr lt) (+ i 1)) ] ) ) )

(define display-a-state-projector
  (lambda (op ls n i)
    (cond [(eq? (length ls) 1)
           (fprintf op "(~a,~a)" (state2str (car ls)) (list-ref (car ls) n)) ]
          [(eq? (remainder i 3 ) 0)
           (fprintf op "(~a,~a),\n      " (state2str (car ls)) (list-ref (car ls) n))
           (display-a-state-projector op (cdr ls) n (+ i 1)) ]
          [else (fprintf op "(~a,~a)," (state2str (car ls)) (list-ref (car ls) n))
                (display-a-state-projector op (cdr ls) n (+ i 1)) ] ) ) )

(define display-state-projectors
  (lambda (op lc ls n)
    (cond [(eq? lc '()) (void)]
          [else (fprintf op " &\n ~a = {\n      " (car lc))
                (display-a-state-projector op ls n 1)
                (fprintf op " }\n")
                (display-state-projectors op (cdr lc) ls (+ n 1)) ] ) ) )

(define (create-ProB-file directoryname filename lc la ls le lt is ms ce)
  (let* ([op (open-output-file (string-append directoryname "/" filename ".mch") #:mode 'text #:exists 'replace)])
    (fprintf op "// copyright Richard St_Denis, Universite de Sherbrooke, 2023.\n")
    (fprintf op "MODEL ~a\n\n" (string-append "SB" filename))
                                                       ; SETS section
    (fprintf op "SETS\n States = {")                     ; set of states
    (display-states op (remove-duplicates ls) 1)
    (fprintf op " };\n")
    (fprintf op " Events = {~a" (car le))                ; set of events
    (map (lambda (x) (fprintf op ", ~a" x)) (cdr le))
    (fprintf op "};\n")
    (fprintf op " LocalStates = {~a" (car la))           ; set of local states
    (map (lambda (x) (fprintf op ", ~a" x)) (cdr la))
    (fprintf op "}\n\n")
                                                       ; CONSTANTS section
    (fprintf op "CONSTANTS\n")
    (fprintf op " G_FB,\n")                              ; automaton modeling the free behavior
    (fprintf op " ~a" (car lc))                          ; atomic component projectors
    (map (lambda (x) (fprintf op ", ~a" x)) (cdr lc))
    (fprintf op ",\n")
    (fprintf op " Qpred, BadStates\n\n")                 ; bad states
                                                       ; PROPERTIES section
    (fprintf op "PROPERTIES\n G_FB = rec(       // Automaton modeling the free behavior\n")
    (fprintf op "            states: States,\n            events: Events,\n")
    (fprintf op "            tf: {")
    (display-transitions op (remove-duplicates lt) 1)
    (fprintf op " },\n")
    (fprintf op "            x_0: ~a,\n" (state2str is))
    (fprintf op "            Xm: {")
    (display-states op ms 1)
    (fprintf op "},\n")
    (cond [(eq? (length ce) 0)
           (fprintf op "            ctrlEvents: {} )\n")
           (fprintf op " &\n G_FB'ctrlEvents <: G_FB'events\n")]
          [else (fprintf op "            ctrlEvents: {~a" (car ce))
                (map (lambda (x) (fprintf op ", ~a" x)) (cdr ce))
                (fprintf op "} )\n") ] )
    (display-state-projectors op lc ls 0)
    (fprintf op " &\n BadStates =  States    // to be completed\n")
    (fprintf op " &\n Qpred = States - BadStates\nEND")
    (close-output-port op) ) )

; Cartesian product of two states represented by a list of symbols
; (one per componnent)
(define product-cartesian
  (lambda (ls1 ls2)
    (if (or (null? ls1) (null? ls2))
        '()
        (cons (append (car ls1) (car ls2))
              (append (product-cartesian (list (car ls1)) (cdr ls2))
                      (product-cartesian (cdr ls1) ls2) ) ) ) ) )

; Rough product of two list of transitions w.r.t. a set of shared events
(define product-transition
  (lambda (lt1 lt2 lse)
    (if (or (null? lt1) (null? lt2))
        '()
        (append (cond
          ; synchonization on a shared event
          [(eq? (cadar lt1) (cadar lt2))
           (list (list (append (caar lt1) (caar lt2))               ; source state
                       (cadar lt1)                                  ; event
                       (append (caddar lt1) (caddar lt2)))) ]       ; target state
          ; not a shared event w.r.t. the 1st component
          [(and (not (member (cadar lt1) lse)) (member (cadar lt2) lse))
           (list (list (append (caar lt1) (caar lt2))               ; source state
                       (cadar lt1)                                  ; event
                       (append (caddar lt1) (caar lt2)))) ]         ; target state
          ; not a shared event w.r.t. the 2nd component
          [(and (not (member (cadar lt2) lse)) (member (cadar lt1) lse))
           (list (list (append(caar lt1) (caar lt2))                ; source state
                       (cadar lt2)                                  ; event
                       (append (caar lt1) (caddar lt2)))) ]         ; target state
          ; not a shared event w.r.t. both components
          [(and (not (member (cadar lt2) lse)) (not (member (cadar lt1) lse)))

;           [else (list (list (append (caar lt1) (caar lt2))          ; source state
                (list (list (append (caar lt1) (caar lt2))          ; source state
                            (cadar lt1)                             ; event
                            (append (caddar lt1) (caar lt2)))       ; target state
                      (list (append (caar lt1) (caar lt2))          ; source state
                            (cadar lt2)                             ; event
                            (append (caar lt1) (caddar lt2))) )]    ; target state
          [else '()])
          (append (product-transition (list (car lt1)) (cdr lt2) lse)
                  (product-transition (cdr lt1) lt2 lse) ) ) ) ) )

; Collect accessible states
(define accessible-states
  (lambda (open close t)
    (if (equal? open '())
      '()
      (append (list (car open))
        (let ([new-states (filter-map (lambda(x) (and (equal? (car open) (car x)) (caddr x))) t)])
             (accessible-states (remove-duplicates (append (cdr open) (remove* close new-states)))
                                (append (list (car open)) close) t ) ) ) ) ) )

; Collect accessible transitions w.r.t. accessible states
(define accessible-transitions
  (lambda (acc-states lt)
    (if (equal? acc-states '())
      '()
      (append (filter-map (lambda(x) (and (equal? (car acc-states) (car x)) x)) lt)
              (accessible-transitions (cdr acc-states) lt) ) ) ) )

; Synchronous product of two automata
(define (synch-product n1 las1 ls1 is1 lms1 le1 lce1 lt1
                       n2 las2 ls2 is2 lms2 le2 lce2 lt2)
  (let* ([n (append n1 n2)]
         [a (append las1 las2)]
         [share-events (remove* (remove* le1 le2) le2)]
         [t0 (product-transition lt1 lt2 share-events)]
         [i (append is1 is2)]
         [s (accessible-states (list i) '() t0)]
         [m0 (product-cartesian lms1 lms2)]
         [m (remove* (remove* s m0) m0)]
         [e (remove-duplicates (append le1 le2))]
         [c (remove-duplicates (append lce1 lce2))]
         [t (remove-duplicates (accessible-transitions s t0))] )
    (values n a s i m e c t) ) )

; main program
(display "Enter the number of reusable components: ")
(let* ([N (string->number (read-line))])
  (display "Enter a directory name for the composite component: ")
  (let* ([directory-name (read-line)])
    (display "Enter a file name for a reusable component (without extension): ")
    (define-values (name1 lAstates1 lStates1 iState1 lMstates1 lEvents1 lCtrlEvents1 lTransitions1)
      (read-automaton directory-name (read-line)) )
      (for ([i (- N 1)])
        (display "Enter a file name for a reusable component (without extension): ")
        (define-values (name2 lAstates2 lStates2 iState2 lMstates2 lEvents2 lCtrlEvents2 lTransitions2)
          (read-automaton directory-name (read-line)) )
          (display "Computing a product of labeled transition systems ...\n")
          (define-values (name lAstates lStates iState lMstates lEvents lCtrlEvents lTransitions)
            (synch-product name1 lAstates1 lStates1 iState1 lMstates1 lEvents1 lCtrlEvents1 lTransitions1
                           name2 lAstates2 lStates2 iState2 lMstates2 lEvents2 lCtrlEvents2 lTransitions2) )
        (if (eq? i (- N 2))
          (begin (printf "Number of states: ~a\n" (length (remove-duplicates lStates)))
                 (printf "Number of events: ~a\n" (length lEvents))
                 (printf "Number of transitions: ~a\n" (length lTransitions))
                 (display "\nCreating the B machine for the new component from the data...\n")
;                (printf "Name: ~a\n" name)
;                (printf "Atomic states: ~a\n" lAstates)
;                (printf "States: ~a\n" lStates)
;                (printf "Initial state: ~a\n" iState)
;                (printf "Marked states: ~a\n" lMstates)
;                (printf "Events: ~a\n" lEvents)
;                (printf "Controllable events: ~a\n" lCtrlEvents)
;                (printf "Transitions: ~a\n" lTransitions)
                 (display "\nEnter a file name for the B machine (without extension): ")
                 (create-ProB-file directory-name (read-line)
                   name (remove-duplicates lAstates) (remove-duplicates lStates) lEvents lTransitions iState (remove-duplicates lMstates) lCtrlEvents)
                 (display "End of processing.\n") )
          (begin 
                 (printf "Number of states: ~a\n" (length (remove-duplicates lStates)))
                 (printf "Number of events: ~a\n" (length lEvents))
                 (printf "Number of transitions: ~a\n" (length lTransitions))
                 (set! name1 name)
                 (set! lAstates1 (remove-duplicates lAstates))
                 (set! lStates1 (remove-duplicates lStates))
                 (set! iState1 iState)
                 (set! lMstates1 (remove-duplicates lMstates))
                 (set! lEvents1 (remove-duplicates lEvents))
                 (set! lCtrlEvents1 (remove-duplicates lCtrlEvents))
                 (set! lTransitions1 (remove-duplicates lTransitions)) ) ) ) ) )