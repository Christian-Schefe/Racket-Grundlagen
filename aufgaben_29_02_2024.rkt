;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname aufgaben_29_02_2024) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Aufgabe 22

;; Addiert 42 zu jedem Element der Liste.
;; add-42: (list-of number) -> (list-of number)
(check-expect (add-42 '()) '())
(check-expect (add-42 '(42)) '(84))
(check-expect (add-42 '(1 2 3)) '(43 44 45))

(define add-42
  (λ [lvz]
    (map (λ [n] (+ n 42)) lvz)))

;; Addiert 42 zu jedem Element der Liste und gibt alle geraden Zahlen zurück.
;; add-42-even: (list-of number) -> (list-of number)
(check-expect (add-42-even '(1 2 3)) '(44))
(check-expect (add-42-even '()) '())
(check-expect (add-42-even '(1 3 5)) '())

(define add-42-even
  (λ [lvz]
    (filter even? (add-42 lvz))))

;; Addiert 42 zu jedem Element der Liste und gibt das Produkt der geraden Zahlen zurück.
;; add-42-even-mul: (list-of number) -> number
(check-expect (add-42-even-mul '()) 1)
(check-expect (add-42-even-mul '(1 2 3 4 5)) 2024)

(define add-42-even-mul
  (λ [lvz]
    (foldr * 1 (add-42-even lvz))))

;; Gibt alle Elemente einer Liste zurück, die durch 4 oder 5 teilbar sind.
;; div-4-or-5: (list-of number) -> (list-of number)
(check-expect (div-4-or-5 '()) '())
(check-expect (div-4-or-5 '(1 4 5 8 11)) '(4 5 8))

(define div-4-or-5
  (λ [lvz]
    (filter (λ [n] (or (= (modulo n 4) 0) (= (modulo n 5) 0))) lvz)))

;; Gibt die Summe der Quadrate der Elemente der Liste zurück.
;; square-sum: (list-of number) -> number
(check-expect (square-sum '()) 0)
(check-expect (square-sum '(2)) 4)
(check-expect (square-sum '(1 2 3 4 5)) 55)
(check-expect (square-sum '(1 0.5 4 2)) 21)

(define square-sum
  (λ [lvz]
    (foldr + 0 (map sqr (filter (λ [n] (and (integer? n) (not (negative? n)))) lvz)))))

;; Gibt #true zurück, wenn alle Elemente der Liste #true sind.
;; and*: (list-of boolean) -> boolean
(check-expect (and* '()) #true)
(check-expect (and* '(#false #false #true #true)) #false)
(check-expect (and* '(#true #true #true)) #true)

(define and*
  (λ [lvw]
    (foldr (λ [a b] (and a b)) #true lvw)))

;; Gibt zwei Listen zurück: Die Liste aller Elemente, für die die Funktion #true zurückgibt,
;; und die Liste aller Elemente, für die die Funktion #false zurückgibt.
;; partition: (X -> boolean) (list-of X) -> (list (list-of X) (list-of X))
(check-expect (partition (λ [n] (>= n 5)) '()) '(() ()))
(check-expect (partition (λ [n] (>= n 5)) '(1 3 8 9 -2)) '((8 9) (1 3 -2)))

(define partition
  (λ [fn lst]
    (list (filter fn lst) (filter (λ [w] (not (fn w))) lst))))

;; Sortiert eine Liste von Zahlen absteigend.
;; sort-quick: (list-of number) -> (list-of number)
(check-expect (sort-quick '()) '())
(check-expect (sort-quick '(1 3 8 9 -2)) '(9 8 3 1 -2))

(define sort-quick
  (λ [lvz]
    (cond [(empty? lvz) empty]
          [else 
           (let* [(pivot (first lvz))
                  (less-than (filter (λ [x] (< x pivot)) lvz))
                  (equal-to (filter (λ [x] (= x pivot)) lvz))
                  (greater-than (filter (λ [x] (> x pivot)) lvz))]
             (append (sort-quick greater-than)
                     equal-to
                     (sort-quick less-than)))])))

;; Sortiert eine Liste von Zahlen auf- oder absteigend
;; entsprechend der angegebenen Reihenfolge.
;; sort-dir: (list-of number) -> (list-of number)
(check-expect (sort-dir '() #t) '())
(check-expect (sort-dir '(1 3 8 9 -2) #t) '(-2 1 3 8 9))
(check-expect (sort-dir '(1 3 8 9 -2) #f) '(9 8 3 1 -2))

(define sort-dir
  (λ [lvz reversed?]
    (cond [reversed? (reverse (sort-quick lvz))]
          [else (sort-quick lvz)])))


;; Aufgabe 23

;; Bildet alle Elemente der Liste nacheinander durch zwei Funktionen ab.
;; map-two: (number -> number) (number -> number) (list-of number) -> (list-of number)
;; map-two: (X -> Y) (Y -> Z) (list-of X) -> (list-of Z)
(check-expect (map-two (λ [n] (+ n 5)) (λ [n] (* n 2)) '()) '())
(check-expect (map-two (λ [n] (+ n 5)) (λ [n] (* n 2)) '(1 2 3 4)) '(12 14 16 18))

(define map-two
  (λ [first-fn second-fn lvs]
    (map second-fn (map first-fn lvs))))

;; Findet die kleinste natürliche Zahl kleiner gleich 1000,
;; für die beide Funktionen denselben Wert zurückgeben.
;; find-equal: (number -> X) (number -> X) -> number | boolean
(check-expect (find-equal (λ [n] (+ n 50)) (λ [n] (* n 2))) 50)
(check-expect (find-equal (λ [n] (- n 5)) (λ [n] (* n 2))) #f)

(define find-equal
  (λ [first-fn second-fn]
    (letrec [(find-equal-n
              (λ [n max-n] (cond [(> n max-n) #f]
                                 [(eq? (first-fn n) (second-fn n)) n]
                                 [else (find-equal-n (+ n 1) max-n)])))]
      (find-equal-n 0 1000))))


;; Aufgabe 24

;; Gibt eine Funktion zurück, die eine Liste von Werten
;; mithilfe der gegebenen Funktion einzeln abbildet.
;; ((mapp fn) lst) und (map fn lst) sind äquivalent.
;; e) mapp ist eine Funktion von Typ 2, da eine Funktion zurückgegeben wird.
;; mapp: (X -> Y) -> ((list-of X) -> (list-of Y))
(check-expect ((mapp abs) '()) '())
(check-expect ((mapp abs) '(1 -2 6 -4)) '(1 2 6 4))

(define mapp
  (λ [fn]
    (λ [n] (map fn n))))
;;  (letrec
;;      [(maplocal
;;        (λ [lst] (cond [(empty? lst) empty]
;;                       [else (cons (fn (first lst))
;;                                   (maplocal (rest lst)))])))]
;;    maplocal)))

;; a) Bildet alle Elemente auf ihren Betrag ab.
(check-expect ((mapp abs) '(4 -7 3)) '(4 7 3))

;; b) Die zurückgegebene Funktion ist ein Datum, kann also
;; einem Bezeichner zugewiesen werden.
(define betraege (mapp abs))

;; c) wie a)
(check-expect (betraege '(4 -7 -3)) '(4 7 3))

;; d) Bildet das Produkt der Beträge aller Elemente.
(check-expect (foldr * 1 ((mapp abs) '(4 -7 3))) 84)