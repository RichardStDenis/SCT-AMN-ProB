; Instanciation of a reusable component.
; Author: Richard St-Denis, Universite de Sherbrooke, 2023.
#lang racket
(require racket/string)
(require json)

; Auxiliary functions
(define (str2sym-transition t)
  (list (list (string->symbol(car t)))        ; source state
        (string->symbol(cadr t))              ; event
        (list (string->symbol(caddr t)))) )   ; target state

(define (str2sym-relabeling r)
  (list (string->symbol(car r))               ; source name
        (string->symbol(cadr r)) ) )          ; target name

; Read a component from a JSON file
(define (read-automaton filename)
  (let* ([ip (open-input-file filename)]
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

(define (read-instantiation directoryname filename)
  (let* ([ip (open-input-file (string-append directoryname "/" filename ".json"))]
         [h (read-json ip)]
         [n (list (hash-ref h 'name))]
         [s (map str2sym-relabeling (hash-ref h 'states))]
         [e (map str2sym-relabeling (hash-ref h 'events))] )
    (close-input-port ip)
    (values n s e) ) )

(define (create-ProB-file directoryname filename lps lpe lt is ms ce)
  (let* ([op (open-output-file (string-append directoryname "/" filename ".json") #:mode 'text #:exists 'replace)]
         [mps (make-immutable-hash (map (lambda (p) (cons (first p) (second p))) lps))]
         [mpe (make-immutable-hash (map (lambda (p) (cons (first p) (second p))) lpe))] )
    (fprintf op "{\n")
    (fprintf op " \"name\" : \"~a\",\n" filename)
    (fprintf op " \"states\" : [\"~a\"" (cadar lps))
    (map (lambda (x) (fprintf op ", \"~a\"" (cadr x))) (cdr lps))
    (fprintf op "],\n")
    (fprintf op " \"istate\" : \"~a\",\n" (hash-ref mps (car is)))
    (fprintf op " \"mstates\" : [\"~a\"" (hash-ref mps (caar ms)))
    (map (lambda (x) (fprintf op ", \"~a\"" (hash-ref mps (car x)))) (cdr ms))
    (fprintf op "],\n")
    (fprintf op " \"events\" : [\"~a\"" (cadar lpe))
    (map (lambda (x) (fprintf op ", \"~a\"" (cadr x))) (cdr lpe))
    (fprintf op "],\n")
    (fprintf op " \"cevents\" : [\"~a\"" (hash-ref mpe (car ce)))
    (map (lambda (x) (fprintf op ", \"~a\"" (hash-ref mpe x))) (cdr ce))
    (fprintf op "],\n")
    (fprintf op " \"transitions\" : [\n   [\"~a\",\"~a\",\"~a\"]"
      (hash-ref mps (caaar lt)) (hash-ref mpe (cadar lt)) (hash-ref mps (car (caddar lt))))
    (map (lambda (x) (fprintf op ",\n   [\"~a\",\"~a\",\"~a\"]"
      (hash-ref mps (caar x)) (hash-ref mpe (cadr x)) (hash-ref mps (car (caddr x))))) (cdr lt)) 
    (fprintf op " ]\n")
    (fprintf op "}")
    (close-output-port op) ) )

; main program
(display "Enter a directory name for the composite component: ")
(let* ([directory-name (read-line)])
  (display "Enter a directory name for the component library: ")
  (let* ([library-name (read-line)])
    (display "Enter a file name for a reusable component (without extension): ")
    (define-values (name1 lAstates1 lStates1 iState1 lMstates1 lEvents1 lCtrlEvents1 lTransitions1)
      (read-automaton (string-append library-name "/"(read-line)".json")) )
;    (read-automaton (string-append directory-name "/" (read-line)".json")) )
  (printf "Name: ~a\n" name1)
  (printf "Atomic states: ~a\n" lAstates1)
  (printf "States: ~a\n" lStates1)
  (printf "Initial state: ~a\n" iState1)
  (printf "Marked states: ~a\n" lMstates1)
  (printf "Events: ~a\n" lEvents1)
  (printf "Controllable events: ~a\n" lCtrlEvents1)
  (printf "Transitions: ~a\n" lTransitions1)
  (display "Enter a file name for instantiation (without extension): ")
  (define-values (name2 lStates2 lEvents2)
    (read-instantiation directory-name (read-line)) )
  (printf "Name: ~a\n" name2)
  (printf "States: ~a\n" lStates2)
  (printf "Events: ~a\n" lEvents2)
  (display "\nEnter a file name for the instantiated component (without extension): ")
  (create-ProB-file directory-name (read-line)
    lStates2 lEvents2 lTransitions1 iState1 lMstates1 lCtrlEvents1)
  (display "End of processing.\n") ) )