;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname aufgaben_01_02_2024) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Aufgabe 15 - Mitarbeiter

;; Die monatliche Sollarbeitszeit in Minuten
(define sollarbeitszeit 160)

;; Der Überstundenfaktor
(define überstundenfaktor 1.25)


;; Ein festangestellter ist ein Wert
;;     (make-festangestellter name grundgehalt arbeitsstunden)
;; wobei name der Name, grundgehalt das Grundgehalt
;; und arbeitsstunden die im letzten Monat gearbeiteten Arbeitsstunden des Festangestellten sind.
(define-struct festangestellter [name grundgehalt arbeitsstunden])


;; Berechnet den Bruttolohn eines Festangestellten
(check-expect (festangestellter-bruttolohn (make-festangestellter "Bob" 400000 160)) 400000)
(check-expect (festangestellter-bruttolohn (make-festangestellter "Bob" 400000 161)) 403125)
(check-expect (festangestellter-bruttolohn (make-festangestellter "Bob" 400000 162)) 406250)

(define festangestellter-bruttolohn
  (λ [arbeiter]
    (+ (festangestellter-grundgehalt arbeiter)
       (* (festangestellter-grundgehalt arbeiter) (/ 1 sollarbeitszeit)
          (- (festangestellter-arbeitsstunden arbeiter) sollarbeitszeit) überstundenfaktor))))


;; Ein werkstudent ist ein Wert
;;     (make-werkstudent name stundenlohn arbeitsstunden)
;; wobei name der Name, stundenlohn der Stundenlohn
;; und arbeitsstunden die im letzten Monat gearbeiteten Arbeitsstunden des Werkstundenten sind.
(define-struct werkstudent [name stundenlohn arbeitsstunden])


;; Berechnet den Bruttolohn eines Werkstudenten
(check-expect (werkstudent-bruttolohn (make-werkstudent "Alice" 1500 160)) 240000)
(check-expect (werkstudent-bruttolohn (make-werkstudent "Alice" 1500 161)) 241875)
(check-expect (werkstudent-bruttolohn (make-werkstudent "Alice" 1500 162)) 243750)

(define werkstudent-bruttolohn
  (λ [arbeiter]
    (* (werkstudent-stundenlohn arbeiter)
       (+ sollarbeitszeit
          (* überstundenfaktor
             (- (werkstudent-arbeitsstunden arbeiter) sollarbeitszeit))))))


;; Ein Mitarbeiter ist entweder
;; - ein Festangestellter oder
;; - ein Werkstudent
;; Name: mitarbeiter


;; Berechnet den Bruttolohn eines Mitarbeiters entsprechend seiner Arbeitsform
(check-expect (bruttolohn (make-werkstudent "Alice" 1500 162)) 243750)
(check-expect (bruttolohn (make-festangestellter "Bob" 400000 162)) 406250)

(define bruttolohn
  (λ [mitarbeiter]
    (cond [(festangestellter? mitarbeiter) (festangestellter-bruttolohn mitarbeiter)]
          [(werkstudent? mitarbeiter) (werkstudent-bruttolohn mitarbeiter)])))


;; Aufgabe 17

;; Berechnet die Summe aller Zahlen in einer Liste
(check-expect (sum '(1 2 3 4 5)) 15)
(check-expect (sum '(1 -1 2 -2)) 0)
(check-expect (sum '()) 0)
(check-expect (sum '(3)) 3)

(define sum
  (λ [x]
    (cond [(empty? x) 0]
          [else (+ (first x)
                   (sum (rest x)))])))


;; Berechnet das Produkt aller Zahlen in einer Liste
(check-expect (prod '(1 2 3 4 5)) 120)
(check-expect (prod '(1 -1 2 -2)) 4)
(check-expect (prod '()) 1)
(check-expect (prod '(3)) 3)

(define prod
  (λ [x]
    (cond [(empty? x) 1]
          [else (* (first x)
                   (prod (rest x)))])))


;; Berechnet das Maximum aller Zahlen in einer Liste
;; maximum: liste-von-zahlen -> zahl
(check-expect (maximum '(-3)) -3)
(check-expect (maximum '(1 2 3 4 5)) 5)
(check-expect (maximum '(1 -1 2 -2)) 2)
(check-error (maximum '()) "Maximum einer leeren Liste ist nicht definiert")

(define maximum
  (λ [lvz]
    (cond [(empty? lvz) (error "Maximum einer leeren Liste ist nicht definiert")]
          [(empty? (rest lvz)) (first lvz)]
          [else (max (first lvz)
                     (maximum (rest lvz)))])))


;; Überprüft, ob das Element in der Liste vorhanden ist
;; enthält? liste-von-unbestimmt, unbestimmt -> wahrheitswert
(check-expect (enthält? '() 3) #f)
(check-expect (enthält? '(A) 'A) #t)
(check-expect (enthält? '(1 2 3 4 5) 3) #t)
(check-expect (enthält? '(1 -1 2 -2) -3) #f)

(define enthält?
  (λ [lvu el]
    (cond [(empty? lvu) #f]
          [else (or (equal? (first lvu) el)
                    (enthält? (rest lvu) el))])))


;; Gibt eine Liste zürck, dessen Elemente jeweils um 1
;; kleiner sind als die der gegebenen Liste
;; declist: liste-von-zahlen -> liste-von-zahlen
(check-expect (declist '()) '())
(check-expect (declist '(0)) '(-1))
(check-expect (declist '(1 2 3 4 5)) '(0 1 2 3 4))
(check-expect (declist '(1 -1 2 -2)) '(0 -2 1 -3))

(define declist
  (λ [lvz]
    (cond [(empty? lvz) empty]
          [else (cons (- (first lvz) 1)
                      (declist (rest lvz)))])))


;; Fügt zwei Listen zu einer Liste zusammen
;; concat: liste-von-unbestimmt, liste-von-unbestimmt -> liste-von-unbestimmt
(check-expect (concat '() '()) '())
(check-expect (concat '() '(A B)) '(A B))
(check-expect (concat '(1 2) '()) '(1 2))
(check-expect (concat '(1 2 3) '(4 5)) '(1 2 3 4 5))
(check-expect (concat '(1 -1 2) '(-2)) '(1 -1 2 -2))

(define concat
  (λ [lvu-1 lvu-2]
    (cond [(empty? lvu-1) lvu-2]
          [(empty? lvu-2) lvu-1]
          [else (cons (first lvu-1)
                      (concat (rest lvu-1) lvu-2))])))


;; Gibt eine Liste zürck, die alle Elemente
;; der geschachtelten Liste in ihrer auftretenden Reihenfolge enthält
;; flatten: liste-von-unbestimmt -> liste-von-unbestimmt
(check-expect (flatten '()) '())
(check-expect (flatten '(1 2 3 4)) '(1 2 3 4))
(check-expect (flatten '(1 (2 3) (4) 5)) '(1 2 3 4 5))
(check-expect (flatten '((10 11) ((-1 2) -2))) '(10 11 -1 2 -2))

(define flatten
  (λ [lvu]
    (cond [(empty? lvu) empty]
          [(list? (first lvu))
           (concat (flatten (first lvu))
                   (flatten (rest lvu)))]
          [else (cons (first lvu)
                      (flatten (rest lvu)))])))


(check-expect (flatten-no-concat '()) '())
(check-expect (flatten-no-concat '(1 2 3 4)) '(1 2 3 4))
(check-expect (flatten-no-concat '(1 (2 3) (4) 5)) '(1 2 3 4 5))
(check-expect (flatten-no-concat '(1 ((-1 2) -2))) '(1 -1 2 -2))
(check-expect (flatten-no-concat '((10 11) ((-1 2) -2))) '(10 11 -1 2 -2))

(define flatten-no-concat
  (λ [lvu]
    (letrec [(internal-flatten
              (λ [lvu-1 lvu-2]
                (cond [(empty? lvu-1) lvu-2]
                      [(list? (first lvu-1))
                       (internal-flatten (first lvu-1)
                                         (internal-flatten (rest lvu-1) lvu-2))]
                      [else (cons (first lvu-1)
                                  (internal-flatten (rest lvu-1) lvu-2))])))]
      (internal-flatten lvu empty))))

;; Zählt die Häufigkeit eines Elements
;; vorkommen: liste-von-unbestimmt, unbestimmt -> zahl
(check-expect (vorkommen '() 3) 0)
(check-expect (vorkommen '(1) 1) 1)
(check-expect (vorkommen '(1 2 8 4) 3) 0)
(check-expect (vorkommen '(1 2 3 3) 3) 2)
(check-expect (vorkommen '(A B B C B) 'B) 3)

(define vorkommen
  (λ [lvu el]
    (cond [(empty? lvu) 0]
          [else (+ (cond [(equal? (first lvu) el) 1]
                         [else 0])
                   (vorkommen (rest lvu) el))])))


;; Gibt eine Liste mit allen Elementen = el entfernt
;; ohne: liste-von-unbestimmt, unbestimmt -> liste-von-unbestimmt
(check-expect (ohne '() 'B) '())
(check-expect (ohne '(A A B A) 'C) '(A A B A))
(check-expect (ohne '(A B B A B) 'B) '(A A))
(check-expect (ohne '(1 1 -1 -2) -1) '(1 1 -2))

(define ohne
  (λ [lvu el]
    (cond [(empty? lvu) empty]
          [(equal? (first lvu) el) (ohne (rest lvu) el)]
          [else (cons (first lvu)
                      (ohne (rest lvu) el))])))


;; Gibt eine Liste von Paaren zürck, die die Häufigkeit der Atome angeben
;; frequencies: liste-von-unbestimmt -> liste-von-(unbestimmt, zahl)
(check-expect (frequencies '()) '())
(check-expect (frequencies '(ONE)) '((ONE 1)))
(check-expect (frequencies '(A B B A B)) '((A 2) (B 3)))
(check-expect (frequencies '(1 1 2 (1 2))) '((1 2) (2 1) ((1 2) 1)))

(define frequencies
  (λ [lvu]
    (cond [(empty? lvu) empty]
          [else (cons (cons (first lvu)
                            (cons (vorkommen lvu (first lvu)) empty))
                      (frequencies (ohne (rest lvu) (first lvu))))])))


;; Aufgabe 18

;; Berechnet das größte n, für das die Summe der ersten n Zahlen kleiner als ein Wert ist
;; anzahl-bevor-summe-erreicht: liste-von-zahlen, zahl -> zahl
(check-expect (anzahl-bevor-summe-erreicht '(2) 1) 0)
(check-expect (anzahl-bevor-summe-erreicht '(1 2 3 4 5) 10) 3)
(check-expect (anzahl-bevor-summe-erreicht '(1 2 3 4 5) 15) 4)
(check-error (anzahl-bevor-summe-erreicht '(1 2 3 4 5) 16) "Summe der Listenelemente zu klein")
(check-error (anzahl-bevor-summe-erreicht '() 0) "Summe der Listenelemente zu klein")

(define anzahl-bevor-summe-erreicht
  (λ [lvz summe]
    (cond [(empty? lvz) (error "Summe der Listenelemente zu klein")]
          [(<= summe (first lvz)) 0]
          [else (+ 1 (anzahl-bevor-summe-erreicht (rest lvz) (- summe (first lvz))))])))


;; Multipliziert zwei gleich lange Listen Element-weise
;; mult-2-num-lists: liste-von-zahlen, liste-von-zahlen -> liste-von-zahlen
(check-expect (mult-2-num-lists '() '()) '())
(check-expect (mult-2-num-lists '(3) '(-2)) '(-6))
(check-expect (mult-2-num-lists '(1 2 3) '(4 5 0)) '(4 10 0))
(check-expect (mult-2-num-lists '(1 -1 2 -1) '(-2 0 5 -1)) '(-2 0 10 1))

(define mult-2-num-lists
  (λ [lvz-1 lvz-2]
    (cond [(or (empty? lvz-1) (empty? lvz-2)) empty]
          [else (cons (* (first lvz-1) (first lvz-2))
                      (mult-2-num-lists (rest lvz-1) (rest lvz-2)))])))


;; Fügt zwei sortierte Listen zu einer sortierten Liste zusammen
;; merge: liste-von-zahlen, liste-von-zahlen -> liste-von-zahlen
(check-expect (merge '() '()) '())
(check-expect (merge '(2 1 -1) '()) '(2 1 -1))
(check-expect (merge '(1 2 3) '(0 4 5)) '(0 1 2 3 4 5))
(check-expect (merge '(-1 -1 1 2) '(-2 -1 0 5)) '(-2 -1 -1 -1 0 1 2 5))

(define merge
  (λ [lvz-1 lvz-2]
    (cond [(empty? lvz-1) lvz-2]
          [(empty? lvz-2) lvz-1]
          [(< (first lvz-1) (first lvz-2)) (cons (first lvz-1) (merge (rest lvz-1) lvz-2))]
          [else  (cons (first lvz-2) (merge lvz-1 (rest lvz-2)))])))
