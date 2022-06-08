(use makiki)
(use text.html-lite)
(use gauche.threads)

(load "./knowledgebase.scm")

(add-load-path ".." :relative)

(define (main args)
    (start-http-server
        :access-log #t
        :error-log #t
        :port 8080
        :app-data (atom (make-knowledgebase))))

(define (html-head)
    (html:head (html:meta :charset "UTF-8")
               (html:meta :http-equiv "X-UA-Compatible" :content "IE=edge")
               (html:meta :name "viewport" :content "width=device-width, initial-scale=1.0")
               (html:link :rel "stylesheet" :href "/style.css")
               (html:link :rel "shortcut icon" :href "/icon.webp")
               (html:title "Экспертная система \"Кулинар\"")))

(define-http-handler "/"
    (lambda (req app)
        (respond/ok req
            (html:html
                (html-head)
                (html:body
                    (html:main :id "space-around"
                        (html:div :id "border"
                            (html:div :id "content"
                                (html:p :class "plain-text" "Эта экспертная система помогает подобрать блюдо по вашим предпочтениям.")
                                (html:p :class "plain-text" "Нужно ответить на несколько вопросов, и вам будет предложено блюдо наиболее подходящее вашим требованиям.")
                                (html:a :class "link-button" :href "/es" "Подобрать")))))))))

(define-http-handler "/es"
    (lambda (req app)
        (let-params req ([ans "q" :convert string->symbol])
            (respond/ok req
                (html:html
                    (html-head)
                    (html:body
                        (html:main :id "space-around"
                            (html:div :id "border"
                                (let ((es-ans (((atom-ref app) `receive-request) ans)))
                                     (if (car es-ans)
                                         (html:div :id "content"
                                            (html:p :class "plain-text" (string-append "Мы подобрали для вас: " (cdr es-ans)))
                                            (html:a :class "link-button" :href "/reset" "Сбросить")
                                            (html:a :class "link-button" :href "/trace" "Получить объяснения о выводе"))
                                         (begin
                                            (html:div :id "content"
                                                (html:p :class "plain-text" (cdr es-ans))
                                                (html:div :id "buttons-container"
                                                    (html:a :class "link-button" :href "/es?ans=yes" "Да")
                                                    (html:a :id "no-button" :class "link-button" :href "/es?ans=no" "Нет"))))))))))))))

(define-http-handler "/reset"
    (lambda (req app)
        ((atom-ref app) `reset)
        (respond/ok req
            (html:html
                (html-head)
                (html:body
                    (html:main :id "space-around"
                        (html:div :id "border"
                            (html:div :id "content"
                                (html:p :class "plain-text" "Предыдущий ответ сброшен.")
                                (html:p :class "plain-text" "Можете попробовать ещё раз, ответить на несколько вопросов, и подобрать блюдо наиболее подходящее вашим требованиям.")
                                (html:a :class "link-button" :href "/es" "Подобрать")))))))))

(define-http-handler "/trace"
    (lambda (req app)
        (let ((trace ((atom-ref app) `trace)))
             (respond/ok req
                 (html:html
                     (html-head)
                     (html:body
                         (html:main
                             (html:div :id "border-trace"
                                 (html:div :id "trace-content"
                                    (html:div :id "trace"
                                        (html:h2 "Анализ вывода системы")
                                        (html:h3 "Вы ответили на следующие вопросы:")
                                        (html:ul
                                           (map (lambda (q&a)
                                                   (html:li (car q&a)
                                                       (html:li :class "sub-li" (cdr q&a)))) (car trace)))
                                        (html:h3 "Следующие правила были провалены:")
                                        (html:ul
                                           (map (lambda (rule-semantics)
                                                   (html:li rule-semantics)) (caddr trace)))
                                        (html:h3 "Следующее правило было выполнено:")
                                        (html:p (cadr trace))
                                        (html:a :href "/es" "Обратно к результату")))))))))))


(define-http-handler "/style.css"
    (file-handler))

(define-http-handler "/background.webp"
    (file-handler))

(define-http-handler "/icon.webp"
    (file-handler))

(define-http-handler "/favicon.ico" (^[req app] (respond/ng req 404)))