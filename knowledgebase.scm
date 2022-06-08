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
        (cond ((eq? m `active?) (hash-table-exists? facts name))
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

(define pred3 (and-clause (and-clause
                            (and-clause (make-fact `chinese-food)
                                        (make-fact `main-course))
                            (and-clause (make-fact `fowl-dish)
                                        (make-fact `chicken-thighs)))
                          (or-clause
                            (or-clause (make-fact `chicken-broth)
                                       (make-fact `sesame-oil))
                            (make-fact `fried-peanut))))

(define pred4 (and-clause (and-clause (make-fact `chinese-food)
                                      (make-fact `dessert))
                          (or-clause (make-fact `fruit)
                                     (make-fact `caramel))))

(define pred5 (and-clause (and-clause
                            (and-clause (make-fact `chinese-food)
                                        (make-fact `dessert))
                            (and-clause (make-fact `rice-flour)
                                        (make-fact `corn-syrup)))
                          (or-clause
                            (or-clause (make-fact `peanut)
                                       (make-fact `coconut))
                            (make-fact `sesame))))

(define pred6 (and-clause (and-clause
                            (and-clause (make-fact `japanese-food)
                                        (or-clause (make-fact `appetizer)
                                                   (make-fact `street-food)))
                            (and-clause (make-fact `seafood)
                                        (make-fact `octopus)))
                          (and-clause
                            (and-clause (make-fact `flour)
                                        (make-fact `eggs))
                            (or-clause (make-fact `pickled-ginger)
                                       (make-fact `soy-sauce)))))

(define pred7 (and-clause (and-clause (make-fact `japanese-food)
                                      (or-clause (make-fact `appetizer)
                                                 (make-fact `street-food)))
                          (and-clause
                              (and-clause (and-clause (make-fact `white-cabbage)
                                                      (make-fact `flour))
                                          (and-clause (make-fact `eggs)
                                                      (and-clause (make-fact `meat-dish)
                                                                  (make-fact `pork-belly))))
                              (or-clause (make-fact `japanese-mayonnaise)
                                         (or-clause (make-fact `dashi-powder)
                                                    (make-fact `okonomiyaki-sauce))))))

(define pred8 (and-clause (and-clause
                            (and-clause (make-fact `japanese-food)
                                        (make-fact `main-course))
                            (and-clause (make-fact `fowl-dish)
                                        (make-fact `chicken-fillet)))
                          (and-clause
                            (make-fact `udon-noodles)
                            (or-clause (or-clause 
                                           (or-clause (make-fact `green-onion)
                                                      (make-fact `onion))
                                           (or-clause (make-fact `soy-sauce)
                                                      (make-fact `ginger)))
                                       (or-clause (or-clause (make-fact `garlic)
                                                             (make-fact `carrot))
                                                  (make-fact `bell-paper))))))

(define pred9 (and-clause (and-clause (make-fact `japanese-food)
                                      (make-fact `dessert))
                          (or-clause (make-fact `crushed-ice)
                                     (make-fact `syrup))))

(define pred10 (and-clause
                  (and-clause (make-fact `french-food)
                              (make-fact `main-course))
                  (and-clause
                      (and-clause (make-fact `fowl-dish)
                                  (make-fact `duck-legs))
                      (or-clause (or-clause (make-fact `duck-fat)
                                            (make-fact `fresh-rosemary))
                                 (or-clause (make-fact `fresh-thyme)
                                            (make-fact `garlic))))))

(define pred11 (and-clause
                  (and-clause (make-fact `french-food)
                              (or-clause (make-fact `breakfast)
                                         (make-fact `appetizer)))
                  (and-clause (make-fact `buckwheat-flour)
                              (or-clause
                                  (or-clause (make-fact `cheese)
                                             (make-fact `butter))
                                  (make-fact `boiled-ham)))))

(define pred12 (and-clause
                  (and-clause (make-fact `french-food)
                              (make-fact `main-course))
                  (and-clause (and-clause
                                  (and-clause (make-fact `meat-dish)
                                              (make-fact `smoked-sausage))
                                  (and-clause (make-fact `smoked-collar)
                                              (make-fact `sausage)))
                              (and-clause (make-fact `bacon)
                                          (make-fact `sauerkraut)))))

(define pred13 (and-clause
                  (and-clause (make-fact `french-food)
                              (make-fact `main-course))
                  (and-clause (or-clause (make-fact `seafood)
                                         (make-fact `fish-dish))
                              (and-clause (and-clause
                                            (and-clause (make-fact `salmon-fillet)
                                                        (make-fact `flounder-fillet))
                                            (and-clause (make-fact `cod-fillet)
                                                        (make-fact `shrimps)))
                                          (or-clause (or-clause
                                                        (or-clause (make-fact `dry-white-wine)
                                                                   (make-fact `fennel))
                                                        (or-clause (make-fact `potato)
                                                                   (make-fact `leek)))
                                                      (or-clause
                                                        (or-clause (make-fact `canned-tomatoes)
                                                                   (make-fact `parsley))
                                                        (make-fact `carrot)))))))

(define pred14 (and-clause
                  (and-clause (make-fact `french-food)
                              (make-fact `dessert))
                  (and-clause
                    (and-clause
                        (and-clause (make-fact `flour)
                                    (make-fact `eggs))
                        (and-clause (make-fact `milk)
                                    (make-fact `cocoa-powder)))
                    (and-clause (make-fact `brandy)
                                (make-fact `sugar)))))

(define pred15 (and-clause
                  (and-clause (make-fact `italian-food)
                              (make-fact `main-course))
                  (and-clause
                    (and-clause (make-fact `olive-oil)
                                (make-fact `fresh-basil))
                    (and-clause (make-fact `tomatoes)
                                (make-fact `mozzarella)))))

(define pred16 (and-clause
                  (and-clause (make-fact `italian-food)
                              (make-fact `main-course))
                  (and-clause
                    (and-clause (make-fact `meat-dish)
                                (make-fact `chopped-meat))
                    (and-clause (make-fact `bolognese-sauce)
                                (make-fact `lasagna-sheets)))))

(define pred17 (and-clause
                  (and-clause (make-fact `italian-food)
                              (or-clause (make-fact `appetizer)
                                         (make-fact `salad)))
                  (and-clause
                    (and-clause
                        (and-clause (make-fact `baked-white-bread)
                                    (make-fact `red-onion))
                        (and-clause (make-fact `tomatoes)
                                    (make-fact `bell-paper)))
                    (and-clause
                        (make-fact `wine-vinegar)
                        (or-clause
                            (or-clause (make-fact `olives)
                                       (make-fact `canned-olives))
                            (make-fact `fresh-basil))))))

(define pred18 (and-clause
                  (and-clause (make-fact `italian-food)
                              (make-fact `main-course))
                  (and-clause (make-fact `meat-dish)
                              (and-clause
                                (and-clause
                                  (and-clause (make-fact `cream)
                                              (make-fact `spaghetti))
                                  (and-clause (make-fact `ham)
                                              (make-fact `parmesan)))
                                (or-clause (make-fact `olive-oil)
                                           (make-fact `garlic))))))

(define pred19 (and-clause
                  (and-clause (make-fact `russian-food)
                              (make-fact `main-course))
                  (and-clause (and-clause (make-fact `meat-dish)
                                          (and-clause (make-fact `beef)
                                                      (and-clause (make-fact `potato)
                                                                  (and-clause (make-fact `carrot)
                                                                              (and-clause (make-fact `onion)
                                                                                          (make-fact `sour-cream))))))
                              (or-clause (make-fact `dill)
                                         (or-clause (make-fact `parsley)
                                                    (or-clause (make-fact `butter)
                                                               (make-fact `garlic)))))))

(define pred20 (and-clause
                   (and-clause (make-fact `russian-food)
                               (make-fact `main-course))
                   (and-clause (or-clause (make-fact `cabbage)
                                          (make-fact `sauerkraut))
                               (and-clause
                                 (or-clause (and-clause (make-fact `meat-dish)
                                                        (make-fact `beef))
                                            (and-clause (make-fact `fowl-dish)
                                                        (make-fact `chicken)))
                                 (or-clause (make-fact `potato)
                                            (or-clause (make-fact `carrot)
                                                       (make-fact `onion)))))))

(define pred21 (and-clause
                   (and-clause (make-fact `russian-food)
                               (make-fact `main-course))
                   (and-clause (or-clause (make-fact `kvass)
                                          (make-fact `kefir))
                               (and-clause (make-fact `sour-cream)
                                           (and-clause (make-fact `cucumber)
                                                       (and-clause (make-fact `potato)
                                                                   (and-clause (make-fact `green-onion)
                                                                               (and-clause (make-fact `dill)
                                                                                           (make-fact `eggs)))))))))

(define pred22 (and-clause
                   (and-clause (make-fact `russian-food)
                               (make-fact `main-course))
                   (and-clause
                       (and-clause (make-fact `meat-dish)
                                   (make-fact `pork-tenderloin))
                       (and-clause (make-fact `garlic)
                                   (or-clause (make-fact `mustard)
                                              (or-clause (make-fact `rosemary)
                                                         (make-fact `thyme)))))))

(define rule1 (make-rule pred1 "Свинина в кисло-сладком соусе" "Если кухня китайская и основное блюдо, и из мяса, и из свинины и в нем так же есть марковь или сладкий перец, или чеснок, или острый перец, то вам подходит свинина в кисло-сладком соусе."))

(define rule2 (make-rule pred2 "Утка по-пекински" "Если кухня китайская и основное блюдо, и из птицы, и из утки, и в нем так же есть рисовый уксус или имбирь, или соевый соус, то вам подходит утка по-пекински."))

(define rule3 (make-rule pred3 "Курица гунбао" "Если кухня китайская и основное блюдо, и из птицы, и из куриных бедер, и нем так же есть или куриный бульон, или кунжутное масло, или жареный арахис, то вам подходит курица гунбао."))

(define rule4 (make-rule pred4 "Фрукты в карамели" "Если кухня китайская и десерт, и в нем есть или фрукты, или карамель, то вам подходят фрукты в карамели."))

(define rule5 (make-rule pred5 "Борода дракона" "Если кухня китайская и десерт, и в нем есть рисовая мука и кукурузный сироп, а также арахис или кокос, или кунжут, то вам подходит борода дракона."))

(define rule6 (make-rule pred6 "Такояки" "Если кухня японская и это закуска или уличная еда, и морепродукты, и осьминог, в нем есть мука и яйца, а также маринованный имбирь или соевый соус, то такояки."))

(define rule7 (make-rule pred7 "Окономияки" "Если кухня японская и это закуска или уличная еда, в нем есть белокочанная капуста и мука, и яйца, и оно из мяса, и из свиной грудки, а также японский майонез или порошок даси, или соус окономияки, то окономияки."))

(define rule8 (make-rule pred8 "Удон" "Если кухня японская и это основное блюдо, и из птицы, и из куриного филе, и в нем есть лапша удон, а также зеленый лук или репчатый лук, или соевый соус, или имбирь, или чеснок, или морковь, или перец сладкий, то вам подходит удон."))

(define rule9 (make-rule pred9 "Какигори" "Если кухня японская и десерт, и в нем есть колотый лед или сироп, то вам подходит какигори."))

(define rule10 (make-rule pred10 "Конфи из утки" "Если кухня французская и основное блюдо, и из птицы, и из утиных ножек, и в нем так же есть утиный жир или свежий розмарин, или свежий тимьян, или чеснок, то вам подходит конфи из утки."))

(define rule11 (make-rule pred11 "Бретонские галеты" "Если кухня французская и завтрак или закуска, из гречневой муки, и в нем так же есть сыр или масло сливочное, или ветчина вареная, то вам подходят бретонские галеты."))

(define rule12 (make-rule pred12 "Шукрут" "Если кухня французская и основное блюдо, и из мяса, и в нем есть копченая колбаса, и копченый ошеек, и сосиски, и бекон, и квашенная капуста, то вам подходит шукрут."))

(define rule13 (make-rule pred13 "Буйабес" "Если кухня французская и основное блюдо, и из морепродуктов или рыбы, и в нем есть филе лосося и филе камбалы, и филе трески, и креветки, а также белое сухое вино или фенхель, или картофель, или лук-порей, или помидоры консервированные, или петрушка, или морковь, то вам подходит буйабес."))

(define rule14 (make-rule pred14 "Птифур" "Если кухня французская и десерт, и в нем есть мука и яйца, и молоко, и какао-порошок, и коньяк, и сахар, то вам подойдет птифур."))

(define rule15 (make-rule pred15 "Неаполитанская пицца" "Если кухня итальянская и основное блюдо, и в нем есть оливковое масло и свежий базилик, и помидоры, и моцарелла, то неаполитанская пицца."))

(define rule16 (make-rule pred16 "Лазанья" "Если кухня итальянская и основное блюдо, из мяса, и в нем есть мясной фарш и соус болоньезе, и листы лазаньи, то вам подходит лазанья."))

(define rule17 (make-rule pred17 "Панцанелла" "Если кухня итальянская и закуска или салат, и в нем есть запеченный белый хлеб и красный лук, и помидоры, и сладкий перец, и винный уксус, а также оливки или маслины, или базилик свежий, то вам подходит панцанелла."))

(define rule18 (make-rule pred18 "Паста карбонара" "Если кухня итальянская и основное блюдо, и из мяса, и в нем есть сливки и спагетти, и ветчина, и пармезан, а также оливковое масло или чеснок, то паста карбонара."))

(define rule19 (make-rule pred19 "Русское жаркое с говядиной" "Если кухня русская и основное блюдо, и из мяса, и из говядины, и в нем есть картофель и морковь, и репчатый лук, и сметана, а также укроп или петрушка, или сливочное масло, или чеснок, то вам подходит русское жаркое с говядиной."))

(define rule20 (make-rule pred20 "Щи" "Если кухня русская и основное блюдо, и из квашенной или свежей капусты, и из мяса и говядины или из птицы и курицы, и в нем так же есть картофель или морковь, или репчатый лук, то вам подходят щи."))

(define rule21 (make-rule pred21 "Окрошка" "Если кухня русская и основное блюдо, в нем есть квас или кефир, и сметана, а также огурцы, и картофель, и зеленый лук, и укроп, и яйца, то вам подходит окрошка."))

(define rule22 (make-rule pred22 "Буженина с чесноком" "Если кухня русская и основное блюдо, из мяса и из свиной вырезки, и в нем есть чеснок, а также горчица или розмарин, или тимьян, то буженина с чесноком."))

(hash-table-put! questions `chinese-food "Вы хотите блюдо китайской кухни?")
(hash-table-put! questions `french-food "Вы хотите блюдо французской кухни?")
(hash-table-put! questions `italian-food "Вы хотите блюдо итальянской кухни?")
(hash-table-put! questions `japanese-food "Вы хотите блюдо японской кухни?")
(hash-table-put! questions `russian-food "Вы хотите блюдо русской кухни?")
(hash-table-put! questions `meat-dish "Вы хотите блюдо из мяса?")
(hash-table-put! questions `meat-fowl "Вы хотите блюдо из птицы?")
(hash-table-put! questions `fish-dish "Вы хотите блюдо из рыбы?")
(hash-table-put! questions `seafood "Вы хотите блюдо из морепродуктов?")
(hash-table-put! questions `main-course "Вы хотите основное блюдо?")
(hash-table-put! questions `appetizer "Вы хотите закуску?")
(hash-table-put! questions `breakfast "Вы хотите завтрак?")
(hash-table-put! questions `dessert "Вы хотите десерт?")

(hash-table-put! questions `pork "Вы хотите блюдо из свинины?")
(hash-table-put! questions `chicken "Вы хотите блюдо  из курицы?")
(hash-table-put! questions `duck "Вы хотите блюдо из утки?")
(hash-table-put! questions `carrot "В блюде есть марковь?")
(hash-table-put! questions `bell-paper "В блюде есть сладкий перец?")
(hash-table-put! questions `garlic "В блюде есть чеснок?")
(hash-table-put! questions `hot-paper "В блюде есть острый перец?")
(hash-table-put! questions `rice-vinegar "В блюде есть рисовый уксус?")
(hash-table-put! questions `ginger "В блюде есть имбирь?")
(hash-table-put! questions `soy-sauce "В блюде есть соевый соус?")
(hash-table-put! questions `chicken-thighs "Вы хотите блюдо из куриных бедер?")
(hash-table-put! questions `chicken-broth "В блюде есть куриный бульон?")
(hash-table-put! questions `sesame-oil "В блюде есть кунжутное масло?")
(hash-table-put! questions `fried-peanut	"В блюде есть жареный арахис?")
(hash-table-put! questions `fruit "В блюде есть фрукты?")
(hash-table-put! questions `caramel "В блюде есть карамель?")
(hash-table-put! questions `rice-flour "Вы хотите блюдо из рисовой муки?")
(hash-table-put! questions `corn-syrup "Вы хотите блюдо из кукурузного сиропа?")
(hash-table-put! questions `peanut "В блюде есть арахис?")
(hash-table-put! questions `coconut "В блюде есть кокос?")
(hash-table-put! questions `sesame "В блюде есть кунжут?")
(hash-table-put! questions `octopus "Вы хотите блюдо из осьминога?")
(hash-table-put! questions `flour "Вы хотите блюдо из муки?")
(hash-table-put! questions `eggs "Вы хотите блюдо из яиц?")
(hash-table-put! questions `pickled-ginger "В блюде есть маринованный имбирь?")
(hash-table-put! questions `white-cabbage "Вы хотите блюдо из белокачанной капусты?")
(hash-table-put! questions `pork-belly "Вы хотите блюдо из свиной грудинки?")
(hash-table-put! questions `japanese-mayonnaise "В блюде есть японский майонез?")
(hash-table-put! questions `dashi-powder "В блюде есть порошок даси?")
(hash-table-put! questions `okonomiyaki-sauce "В блюде есть соус окономияки?")
(hash-table-put! questions `chicken-fillet "Вы хотите блюдо из куриного филе?")
(hash-table-put! questions `udon-noodles "Вы хотите блюдо из лапши удон?")
(hash-table-put! questions `green-onion "В блюде есть зеленый лук?")
(hash-table-put! questions `onion "В блюде есть репчатый лук?")
(hash-table-put! questions `crushed-ice "В блюде есть колотый лед?")
(hash-table-put! questions `syrup "В блюде есть сироп?")
(hash-table-put! questions `duck-legs "Вы хотите блюдо из утиных ножек?")
(hash-table-put! questions `duck-fat	"В блюде есть утиный жир?")
(hash-table-put! questions `fresh-rosemary "В блюде есть свежий розмарин?")
(hash-table-put! questions `fresh-thyme "В блюде есть свежий тимьян?")
(hash-table-put! questions `buckwheat-flour "Вы хотите блюдо из гречневой муки?")
(hash-table-put! questions `cheese "В блюде есть сыр?")
(hash-table-put! questions `butter "В блюде есть сливочное масло?")
(hash-table-put! questions `boiled-ham "В блюде есть вареная ветчина?")
(hash-table-put! questions `smoked-sausage "Вы хотите блюдо из капченой колбасы?")
(hash-table-put! questions `smoked-collar "Вы хотите блюдо из капченого ошеека?")
(hash-table-put! questions `sausage "Вы хотите блюдо из сосисок?")
(hash-table-put! questions `bacon "Вы хотите блюдо из бекона?")
(hash-table-put! questions `sauerkraut "Вы хотите блюдо из квашеной капусты?")
(hash-table-put! questions `salmon-fillet "Вы хотите блюдо из филе ласося?")
(hash-table-put! questions `flounder-fillet "Вы хотите блюдо из филе камбалы?")
(hash-table-put! questions `cod-fillet "Вы хотите блюдо из филе трески?")
(hash-table-put! questions `shrimps "Вы хотите блюдо из креветок?")
(hash-table-put! questions `dry-white-wine "В блюде есть сухое белое вино?")
(hash-table-put! questions `fennel "В блюде есть фенхель?")
(hash-table-put! questions `potato "В блюде есть картофель?")
(hash-table-put! questions `leek "В блюде есть лук-порей?")
(hash-table-put! questions `canned-tomatoes "Есть консервированные помидоры?")
(hash-table-put! questions `parsley "В блюде есть петрушка?")
(hash-table-put! questions `milk "Вы хотите блюдо из молока?")
(hash-table-put! questions `cocoa-powder "Вы хотите блюдо из какао-порошка?")
(hash-table-put! questions `brandy "Вы хотите блюдо из коньяка?")
(hash-table-put! questions `sugar "Вы хотите блюдо из сахара?")
(hash-table-put! questions `olive-oil "Вы хотите блюдо из оливкового масла?")
(hash-table-put! questions `fresh-basil "Вы хотите блюдо из свежего базилика?")
(hash-table-put! questions `mozzarella "Вы хотите блюдо из моцареллы?")
(hash-table-put! questions `tomatoes "Вы хотите блюдо из помидор?")
(hash-table-put! questions `chopped-meat "Вы хотите блюдо из мясного фарша?")
(hash-table-put! questions `bolognese-sauce "Вы хотите блюдо из соуса болоньезе?")
(hash-table-put! questions `lasagna-sheets "Вы хотите блюдо из листов лазаньи?")
(hash-table-put! questions `baked-white-bread "Вы хотите блюдо из белого запеченного хлеба?")
(hash-table-put! questions `red-onion "Вы хотите блюдо из красного лука?")
(hash-table-put! questions `wine-vinegar "Вы хотите блюдо из винного уксуса?")
(hash-table-put! questions `olives "В блюде есть оливки?")
(hash-table-put! questions `canned-olives "В блюде есть маслины?")
(hash-table-put! questions `cream "Вы хотите блюдо из сливок?")
(hash-table-put! questions `spaghetti "Вы хотите блюдо из спагетти?")
(hash-table-put! questions `ham "Вы хотите блюдо из ветчины?")
(hash-table-put! questions `parmesan "Вы хотите блюдо из пармезана?")
(hash-table-put! questions `beef "Вы хотите блюдо из говядины?")
(hash-table-put! questions `sour-cream "Вы хотите блюдо из сметаны?")
(hash-table-put! questions `dill "В блюде есть укроп?")
(hash-table-put! questions `cabbage "Вы хотите блюдо из свежей капусты?")
(hash-table-put! questions `kvass "Вы хотите блюдо из кваса?")
(hash-table-put! questions `kefir "Вы хотите блюдо из кефира?")
(hash-table-put! questions `pork-tenderloin "Вы хотите блюдо из свиной вырезки?")
(hash-table-put! questions `mustard "В блюде есть гочица?")
(hash-table-put! questions `rosemary "В блюде есть розмарин?")
(hash-table-put! questions `thyme "В блюдеЕсть тимьян?")

(define (next-fact)
    (let ((activated-rules (filter (lambda (rule) (rule `active?)) rules)))
         (if (null? activated-rules)
             ((car rules) `next-fact)
             (let ((fired-rules (filter (lambda (rule) (rule `probe)) activated-rules)))
                  (if (null? fired-rules)
                      (let ((first-rule (find (lambda (rule) (not (rule `active?))) rules)))
                        (if first-rule
                            (first-rule `next-fact)
                            "Ничего"))
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
            (cond (end (cons #t current-fact))
                  ((eq? req `yes)
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
                  (else (cons #f (current-fact `question)))))
        (lambda (m)
            (cond ((eq? m `receive-request)
                   (lambda (req) (receive-request req)))
                  ((eq? m `reset)
                    (retracktAll!)
                    (set! end #f)
                    (set! current-fact (next-fact)))
                  ((eq? m `trace) (get-trace))
                  (else (error "Неизвестная операция -- MAKE-KNOWLEDGBASE"))))))

(add-rule! rule2)
(add-rule! rule1)
(add-rule! rule3)
(add-rule! rule4)
(add-rule! rule5)
(add-rule! rule6)
(add-rule! rule7)
(add-rule! rule8)
(add-rule! rule9)
(add-rule! rule10)
(add-rule! rule11)
(add-rule! rule12)
(add-rule! rule13)
(add-rule! rule14)
(add-rule! rule15)
(add-rule! rule16)
(add-rule! rule17)
(add-rule! rule18)
(add-rule! rule19)
(add-rule! rule20)
(add-rule! rule21)
(add-rule! rule22)

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