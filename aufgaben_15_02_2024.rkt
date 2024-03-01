;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname aufgaben_15_02_2024) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Ersetzt alle Elemente s1 in lvs mit s2.
;; exchange: (list-of symbol) symbol symbol -> (list-of symbol)
(check-expect (exchange '() 'a 'b) '())
(check-expect (exchange '(a b c) 'd 'e) '(a b c))
(check-expect (exchange '(a b c a c) 'a 'f) '(f b c f c))

(define exchange
  (λ [lvs s1 s2]
    (cond [(empty? lvs) empty]
          [else (cons (cond [(eq? (first lvs) s1) s2]
                            [else (first lvs)])
                      (exchange (rest lvs) s1 s2))])))

;; Leitet eine als Symbolischer Ausdruck dargestellte Funktion ab.
;; diff: (list-of symbol) symbol -> (list-of symbol)
(check-expect (diff 1 'x) 0)
(check-expect (diff 'y 'x) 0)
(check-expect (diff 'x 'x) 1)
(check-expect (diff '(ADD x 1) 'x) '(ADD 1 0))
(check-expect (diff '(MUL x x) 'x) '(ADD (MUL x 1) (MUL x 1)))
(check-expect (diff '(ADD x x) 'x) '(ADD 1 1))

(define diff
  (λ [lvs var]
    (cond [(eq? lvs var) 1]
          [(not (list? lvs)) 0]
          [(empty? lvs) 'ERROR]
          [else (let* ([op (first lvs)]
                       [left-op (first (rest lvs))]
                       [right-op (first (rest (rest lvs)))]
                       [left-diff (diff left-op var)]
                       [right-diff (diff right-op var)])
                  (cond [(eq? op 'ADD)
                         (list 'ADD left-diff right-diff)]
                        [(eq? op 'MUL)
                         (list 'ADD
                               (list 'MUL left-op right-diff)
                               (list 'MUL right-op left-diff))]
                        [else 'ERROR]))])))

;; Berechnet die Summe aller Elemente der Liste mithilfe von Endrekursion.
;; Akkumulatorinvariante: Die Summe der Elemente von lvz ist
;; die Summe der Elemente von lvz2 + Akkumulator
;; sum: (list-of number) -> number
(check-expect (sum '()) 0)
(check-expect (sum '(4)) 4)
(check-expect (sum '(1 5 -3)) 3)

(define sum
  (λ [lvz]
    (letrec [(sum-iter
              (λ [lvz2 acc]
                (cond [(empty? lvz2) acc]
                      [else (sum-iter (rest lvz2) (+ acc (first lvz2)))])))]
      (sum-iter lvz 0))))

;; Wandelt eine Liste relativer Distanzen in eine Liste absoluter Distanzen um.
;; Benutzt keinen Akkumulator, Komplexität: O(n^2)
;; abs-dist: (list-of number) -> (list-of number)
(check-expect (abs-dist '()) '())
(check-expect (abs-dist '(10)) '(10))
(check-expect (abs-dist '(120 90 70 65)) '(120 210 280 345))

(define abs-dist
  (λ [lvz]
    (letrec [(increase-by
              (λ [lvz2 n]
                (cond [(empty? lvz2) empty]
                      [else (cons (+ n (first lvz2))
                                  (increase-by (rest lvz2) n))])))]
      (cond [(empty? lvz) empty]
            [else (cons (first lvz)
                        (increase-by (abs-dist (rest lvz)) (first lvz)))]))))

;; Wandelt eine Liste relativer Distanzen in eine Liste absoluter Distanzen um.
;; Benutzt einen Akkumulator, Komplexität: O(n)
;; abs-dist-acc: (list-of number) -> (list-of number)
(check-expect (abs-dist-acc '()) '())
(check-expect (abs-dist-acc '(10)) '(10))
(check-expect (abs-dist-acc '(120 90 70 65)) '(120 210 280 345))

(define abs-dist-acc
  (λ [lvz]
    (letrec [(abs-dist-iter
              (λ [lvz2 acc]
                (cond [(empty? lvz2) acc]
                      [else (abs-dist-iter
                             (rest lvz2)
                             (cons (+ (cond [(empty? acc) 0]
                                            [else (first acc)])
                                      (first lvz2)) acc))])))]
      (reverse (abs-dist-iter lvz empty)))))

;; Entfernt alle Duplikate der Elemente der Liste.
;; singletons: (list-of symbol) -> (list-of symbol)
(check-expect (singletons '()) '())
(check-expect (singletons '(4)) '(4))
(check-expect (singletons '(4 2 1 a b)) '(4 2 1 a b))
(check-expect (singletons '(4 4 2 2 1 3 2)) '(1 3))

(define singletons
  (λ [lvs]
    (letrec [(occurs?
              (λ [lvs el]
                (cond [(empty? lvs) #f]
                      [(eq? (first lvs) el) #t]
                      [else (occurs? (rest lvs) el)])))
             (remove-el
              (λ [lvs el]
                (cond [(empty? lvs) empty]
                      [(eq? (first lvs) el) (remove-el (rest lvs) el)]
                      [else (cons (first lvs) (remove-el (rest lvs) el))])))]
      (cond [(empty? lvs) empty]
            [(occurs? (rest lvs) (first lvs)) (singletons (remove-el (rest lvs) (first lvs)))]
            [else (cons (first lvs) (singletons (rest lvs)))]))))

;; Entfernt alle Duplikate der Elemente der Liste.
;; Verwendet zwei Akkumulatoren.
;; singletons-acc: (list-of symbol) -> (list-of symbol)
;(check-expect (singletons-acc '()) '())
;(check-expect (singletons-acc '(4)) '(4))
;(check-expect (singletons-acc '(4 2 1 a b)) '(4 2 1 a b))
;(check-expect (singletons-acc '(4 4 2 2 1 3 2)) '(4 2 1 3))
;
;(define singletons-acc
;  (λ [lvs]
;    (letrec [(singletons-iter
;              (λ [lvs acc-unique acc-duplicate]
;                (cond [(empty? lvs) acc-unique]
;                      [else (singletons-iter
;                             (rest lvs)
;                             (cond [(member (first lvs) acc-unique) acc-unique]
;                                   [else (cons (first lvs) acc-unique)])
;                             (cond [(member (first lvs) acc-duplicate) acc-duplicate]
;                                   [(member (first lvs) acc-unique) (cons (first lvs) acc-duplicate)]
;                                   [else acc-duplicate]))])))]
;      (reverse (singletons-iter lvs empty empty)))))