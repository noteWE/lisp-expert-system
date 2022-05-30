(use makiki)
(use text.html-lite)
(use gauche.threads)

(define (main args)
    (start-http-server
        :access-log #t
        :error-log #t
        :port 8080
        :app-data (atom `())))

(define-http-handler "/"
    (lambda (req app)
        (respond/ok req
            (html:html
                (html:head (html:title "Экспертная система \"Официант\""))
                (html:p "Эта экспертная система помогает подобрать блюдо по вашим предпочтениям.")
                (html:p "Нужно ответить на несколько вопросов и вам будет предложено блюдо наиболее подходящее вашим требованиям")
                (html:a :href "/es" "Подобрать")))))

(define-http-handler "/es"
    (lambda (req app)
        (let-params req ([ans "q"])
            (cond ((equal? ans "Yes")
                )))))