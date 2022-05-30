

(define facts (make-hash-table))
(define questions (make-hash-table))

(define rules `())

(define (make-question name)
    (string-append (symbol->string name) "?"))

(define (assert fact status)
    (hash-table-put! facts fact status))

(define (ask-console question positive-answer)
    (display question)
    (newline)
    (equal? (read) positive-answer))

(define (answer-console answer)
    (display "Для вас подходит: ")
    (display answer)
    (newline))

(define (make-fact name)
    (define (exists?)
        (hash-table-exists? facts name))
    (define (ask-about)
        (cond ((ask-console
                (hash-table-get questions name (make-question name)) `Да)
               (assert name #t)
               #t)
              (else
               (assert name #f)
               #f)))
    (define (suggest)
        (if (exists?)
            (hash-table-get facts name)
            (ask-about)))
    (lambda (m)
        (cond ((eq? m `suggest) (suggest))
              (else (error "Неизветсная операция -- MAKE-FACT" m)))))

(define (and-clause p1 p2)
    (define (suggest)
        (and (p1 `suggest) (p2 `suggest)))
    (lambda (m)
        (cond ((eq? m `suggest) (suggest))
              (else (error "Неизветсная операция -- AND-CLAUSE" m)))))

(define (or-clause p1 p2)
    (define (suggest)
        (or (p1 `suggest) (p2 `suggest)))
    (lambda (m)
        (cond ((eq? m `suggest) (suggest))
              (else (error "Неизветсная операция -- OR-CLAUSE" m)))))

(define (add-rule! rule)
    (set! rules (cons rule rules)))

(define (make-rule predicate result)
    (lambda (m)
        (cond ((eq? m `fire)
                (cond ((predicate `suggest)
                       (answer-console result)
                       #t)
                      (else #f)))
              (else (error "Неизветсная операция -- MAKE-RULE" m)))))

(define (fire-rules)
    (unless (fold
                (lambda (rule res) (or (rule `fire) res))
                #f
                rules)
            (newline)
            (display "К сожалению мы не смогли вам ничего подобрать")))

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

(define rule1 (make-rule pred1 "Свинина в кисло-сладком соусе"))

(define rule2 (make-rule pred2 "Утка по-пекински"))

(add-rule! rule2)
(add-rule! rule1)

(fire-rules)