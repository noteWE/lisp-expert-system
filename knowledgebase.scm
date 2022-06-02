(define facts (make-hash-table))

(define questions (make-hash-table))

(define rules `())

(define (make-question name)
    (string-append (symbol->string name) "?"))

(define (assert! fact status)
    (hash-table-put! facts fact status))

(define (retracktAll!)
    (hash-table-clear! facts))

(define (make-fact name)
    (lambda (m)
        (cond ((eq? m `suggest) (suggest))
              ((eq? m `active?) (hash-table-exists? facts name))
              ((eq? m `question) (hash-table-get questions name (make-question name)))
              ((eq? m `probe) (hash-table-get facts name))
              ((eq? m `activate-positive!) (assert! name #t))
              ((eq? m `activate-negative!) (assert! name #f))
              ((eq? m `type) `fact)
              (else (error "Неизветсная операция -- MAKE-FACT" m)))))

(define (get-fact-question name)
    ((make-fact name) `question))

(define (and-clause p1 p2)
    (define (active?)
        (and (p1 `active?)
             (or (not (p1 `probe))
                 (p2 `active?))))
    (lambda (m)
        (cond ((eq? m `active?) (active?))
              ((eq? m `probe) (and (p1 `probe) (p2 `probe)))
              ((eq? m `next-fact) (cond ((not (p1 `active?)) (if (eq? (p1 `type) `fact)
                                                                 p1
                                                                 (p1 `next-fact)))
                                        ((not (p2 `active?)) (if (eq? (p2 `type) `fact)
                                                                 p2
                                                                 (p2 `next-fact)))
                                        (else
                                            (error "Выражение уже активировано -- AND-CLAUSE"))))
              ((eq? m `type) `predicate)    
              (else (error "Неизветсная операция -- AND-CLAUSE" m)))))

(define (or-clause p1 p2)
    (define (active?)
        (and (p1 `active?)
             (or (p1 `probe)
                 (p2 `active?))))
    (lambda (m)
        (cond ((eq? m `active?) (active?))
              ((eq? m `probe) (or (p1 `probe) (p2 `probe)))
              ((eq? m `next-fact) (cond ((not (p1 `active?)) (if (eq? (p1 `type) `fact)
                                                                 p1
                                                                 (p1 `next-fact)))
                                        ((not (p2 `active?)) (if (eq? (p2 `type) `fact)
                                                                 p2
                                                                 (p2 `next-fact)))
                                        (else
                                            (error "Выражение уже активировано -- OR-CLAUSE"))))
              ((eq? m `type) `predicate)
              (else (error "Неизветсная операция -- OR-CLAUSE" m)))))

(define (add-rule! rule)
    (set! rules (cons rule rules)))

(define (make-rule predicate result semantics)
    (define (probe)
        (if (predicate `active?)
            (predicate `probe)
            (error "Правило не активно -- MAKE-RULE")))
    (lambda (m)
        (cond ((eq? m `fire)
                (if (probe)
                    result))
              ((eq? m `probe) (probe))
              ((eq? m `active?) (predicate `active?))
              ((eq? m `next-fact) (predicate `next-fact))
              ((eq? m `semantics) semantics)
              (else (error "Неизветсная операция -- MAKE-RULE" m)))))

(define pred1 (and-clause (and-clause
                            (and-clause (make-fact `chinese-food)
                                        (make-fact `main-course))
                            (and-clause (make-fact `meat-dish)
                                        (make-fact `pork)))
                          (or-clause
                            (or-clause (make-fact `carrot)
                                       (make-fact `bell-paper))
                            (or-clause (make-fact `garlic)
                                       (make-fact `hot-paper)))))
                                       
(define pred2 (and-clause (and-clause
                            (and-clause (make-fact `chinese-food)
                                        (make-fact `main-course))
                            (and-clause (make-fact `fowl-dish)
                                        (make-fact `duck)))
                          (or-clause
                            (or-clause (make-fact `rice-vinegar)
                                       (make-fact `ginger))
                            (make-fact `soy-sauce))))

(define rule1 (make-rule pred1 "Свинина в кисло-сладком соусе" "Если кухня китайская и основное блюдо, и из мяса, и из свинины и в нем так же есть марковь или сладкий перец, или чеснок, или острый перец, то вам подходит свинина в кисло-сладком соусе."))

(define rule2 (make-rule pred2 "Утка по-пекински" "Если кухня китайская и основное блюдо, и из птицы, и из утки, и в нем так же есть рисовый уксус или имбирь, или соевый соус, то вам подходит утка по-пекински."))

(define (next-fact)
    (let ((activated-rules (filter (lambda (rule) (rule `active?)) rules)))
         (if (null? activated-rules)
             ((car rules) `next-fact)
             (let ((fired-rules (filter (lambda (rule) (rule `probe)) activated-rules)))
                  (if (null? fired-rules)
                      ((find (lambda (rule) (not (rule `active?))) rules) `next-fact)
                      ((car fired-rules) `fire))))))

(define (make-knowledgebase)
    (define (get-trace)
        (define (bool->symbol v)
            (if v `yes `no))
        (cons (hash-table-map facts (lambda (fact ans)
                                (cons (get-fact-question fact) (bool->symbol ans))))
              (let* ((active-rules (filter (lambda (rule) (rule `active?)) rules))
                     (fired-rule (find (lambda (rule) (rule `probe)) active-rules))
                     (failed-rules (list (map (lambda (rule) (rule `semantics)) (filter (lambda (rule) (not (rule `probe))) active-rules)))))
                    (if fired-rule
                        (cons (fired-rule `semantics)
                              failed-rules)
                        (cons "-" failed-rules)))))
    (let ((current-fact (next-fact))
          (end #f))
        (define (receive-request req)
            (if end
                (cons #t current-fact)
                (cond ((eq? req `yes)
                       (current-fact `activate-positive!)
                       (set! current-fact (next-fact))
                       (if (string? current-fact)
                           (begin
                                (set! end #t)
                                (cons #t current-fact))
                           (cons #f (current-fact `question))))
                      ((eq? req `no)
                       (current-fact `activate-negative!)
                       (set! current-fact (next-fact))
                       (if (string? current-fact)
                           (begin
                               (set! end #t)
                               (cons #t current-fact))
                           (cons #f (current-fact `question))))
                      (else (cons #f (current-fact `question))))))
        (lambda (m)
            (cond ((eq? m `receive-request)
                   (lambda (req) (receive-request req)))
                  ((eq? m `reset)
                    (retracktAll!)
                    (set! end #f)
                    (set! current-fact (next-fact)))
                  ((eq? m `trace) (get-trace))
                  (else (error "Неизвестная операция -- MAKE-KNOWLEDBASE"))))))

(add-rule! rule2)
(add-rule! rule1)

;(define knowledgebase (make-knowledgebase))
;
;
;(display (knowledgebase `receive-request `()))
;(newline)
;
;(define (run)
;    (let ((answer (knowledgebase `receive-request (read))))
;         (display (cdr answer))
;         (newline)
;         (if (not (car answer))
;            (run))))
;
;(run)