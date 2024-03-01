;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname aufgaben_18_01_2024) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Test-Framework

;; Gibt den absoluten Unterschied (aka. Distanz) zwischen zwei Zahlen zurück
(define err
  (λ [antwort loesung]
    (abs (- antwort loesung))))


(define epsilon 0.000001)

;; Testet, ob zwei Zahlen gleich sind
(define test
  (λ [antwort loesung]
    (if (= antwort loesung) #true (format "failed: ~a is not ~a" antwort loesung))))

;; Testet, ob der Unterschied zweier Zahlen geringer als epsilon ist.
(define test-inexact
  (λ [antwort loesung]
    (if (< (err antwort loesung) epsilon) #true (format "failed: ~a is not ~a" antwort loesung))))

(test (err 5 5.001) 0.001)
(test (err 5 4.999) 0.001)


; Aufgabe 5

;; Berechnet die Miete, die bezahlt werden muss,
;; wenn ein Spiele auf einem gegnerischen Bahnhof landet.
;; Diese Miete ist abhängig von der Anzahl der Bahnhöfe, die dem Gegner gehören.
(define miete
  (λ [bahnhof-anzahl]
    (* 250 (expt 2 bahnhof-anzahl))))

(test (miete 1) 500)
(test (miete 2) 1000)
(test (miete 3) 2000)
(test (miete 4) 4000)


; Aufgabe 6

(define kartenpreis 500)
(define fixkosten 2000)
(define kosten-pro-besucher 50)

;; Berechnet den Profit des Kinos anhand der Besucherzahl.
(define profit
  (λ [besucherzahl]
    (- (* kartenpreis besucherzahl)
       (+ fixkosten (* kosten-pro-besucher besucherzahl)))))

(test (profit 100) 43000)
(test (profit 10) 2500)


; Aufgabe 7a

; (define fixkosten 0)
; (define kosten-pro-besucher 65)


; Aufgabe 7b

;; Berechnet den Profit des Kinos anhand der Besucherzahl.
;; [AUS VORLESUNG ÜBERNOMMEN]
(define profit-unmodifiziert
  (λ [kartenpreis]
    (- (* (+ 120 (* (/ 15 10) (- 500 kartenpreis))) kartenpreis)
       (+ 18000 (* 5 (+ 120 (* (/ 15 10) (- 500 kartenpreis))))))))

(test (profit-unmodifiziert 500) 41400)
(test (profit-unmodifiziert 490) 47475)

;; Berechnet den Profit des Kinos anhand der Besucherzahl.
;; Keine Fixkosten, Kosten pro Besucher um 15 Cent erhöht.
(define profit-modifiziert
  (λ [kartenpreis]
    (- (* (+ 120 (* (/ 15 10) (- 500 kartenpreis))) kartenpreis)
       (* 20 (+ 120 (* (/ 15 10) (- 500 kartenpreis)))))))

(test (profit-modifiziert 500) 57600)
(test (profit-modifiziert 490) 63450)


; Aufgabe 8

;; Berechnet das Volumen eines Zylinders
;; anhand seines Radiuses und seiner Höhe.
(define volumen
  (λ [radius hoehe]
    (* pi radius radius hoehe)))

(test-inexact (volumen 10 10) (* pi 1000))
(test-inexact (volumen 5 25) (* pi 625))

;; Berechnet die Mantelfläche eines Zylinders
;; anhand seines Radiuses und seiner Höhe.
(define mantelflaeche
  (λ [radius hoehe]
    (* 2 pi radius hoehe)))

(test-inexact (mantelflaeche 10 10) (* pi 200))
(test-inexact (mantelflaeche 5 25) (* pi 250))

;; Berechnet die Fläche eines Kreises
;; anhand seines Radiuses.
(define kreisflaeche
  (λ [radius]
    (* pi radius radius)))

(test-inexact (kreisflaeche 10) (* pi 100))
(test-inexact (kreisflaeche 5) (* pi 25))

;; Berechnet die Oberfläche eines Zylinders
;; anhand seines Radiuses und seiner Höhe
(define oberflaeche-zylinder
  (λ [radius hoehe]
    (+ (mantelflaeche radius hoehe)
       (* 2 (kreisflaeche radius)))))

(test-inexact (oberflaeche-zylinder 10 10) (* pi 400))
(test-inexact (oberflaeche-zylinder 5 25) (* pi 300))

;; Berechnet die Oberfläche eines Rohrs
;; anhand seines Innenradiuses, seiner Wandstärke und seiner Länge
(define oberflaeche-rohr
  (λ [innenradius wandstaerke laenge]
    (+ (mantelflaeche innenradius laenge)
       (mantelflaeche (+ innenradius wandstaerke) laenge)
       (* 2
          (- (kreisflaeche (+ innenradius wandstaerke))
             (kreisflaeche innenradius))))))

(test-inexact (oberflaeche-rohr 10 1 10) (* pi 462))
(test-inexact (oberflaeche-rohr 5 0.1 25) (* pi 507.02))
