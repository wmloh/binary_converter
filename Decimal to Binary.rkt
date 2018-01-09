;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Decimal to Binary|) (read-case-sensitive #t) (teachpacks ((lib "image.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.ss" "teachpack" "htdp")) #t)))
;; (dec->bin x) produces the binary value of the decimal x
;; Nat -> Nat
;; Examples:
(check-expect (dec->bin 5) 101)
(check-expect (dec->bin 691235) 10101000110000100011)

(define (dec->bin x)
  (local [(define (dec->bin/acc y sum start degree)
            (cond [(= start degree) (+ sum y)]
                  [else (dec->bin/acc (floor (/ y 2))
                                      (+ sum (* (expt 10 start)
                                                (remainder y 2)))
                                      (add1 start)
                                      degree)]))
          (define (find-digits z)
            (cond [(<= z 1) 1]
                  [else (+ 1 (find-digits (quotient z 2)))]))]
    (dec->bin/acc x 0 0 (find-digits x))))

;; (dec->base x b) produces the base b value of the decimal x
;; Nat Nat -> Nat
;; requires: 2 <= b <= 10
;; Examples:
(check-expect (dec->base 5 2) 101)
(check-expect (dec->base 31 9) 34)
(define (dec->base x b)
  (local [(define (dec->bin/acc y sum start degree)
            (cond [(= start degree) (+ sum y)]
                  [else (dec->bin/acc (floor (/ y b))
                                      (+ sum (* (expt 10 start)
                                                (remainder y b)))
                                      (add1 start)
                                      degree)]))
          (define (find-digits z)
            (cond [(<= z (sub1 b)) 1]
                  [else (+ 1 (find-digits (quotient z b)))]))]
    (dec->bin/acc x 0 0 (find-digits x))))







         