;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Boolean-and_if_statements) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

;true
;false


(define WIDTH 100)
(define HEIGHT 100)

;(> WIDTH HEIGHT)
;(>= WIDTH HEIGHT)

;(> 1 2)
;(< 1 2)
;(= 1 1)
;(> 9 3)

;(string=? "foo" "foo")

(define I1 (rectangle 10 20 "solid" "red"))
(define I2 (rectangle 20 10 "solid" "red"))

(< (image-width I1)
   (image-width I2))

(if (< (image-width I1)
       (image-width I2)
     )
    "not wide"
    "wide"
)