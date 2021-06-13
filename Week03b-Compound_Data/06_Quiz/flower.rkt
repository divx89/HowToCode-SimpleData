;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname flower) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; An animation of a flower growing from a spot where the mouse is clicked on the screen

;==================
; Constants
;==================
(define WIDTH 400)
(define HEIGHT 400)

(define MTS (empty-scene WIDTH HEIGHT "lightgreen"))

(define PETAL-COLOR "red")
(define PETAL-OUTLINE-COLOR "yellow")
(define CENTER-COLOR PETAL-OUTLINE-COLOR)

(define PETAL (overlay (ellipse 15 (* 15 3) "outline" PETAL-OUTLINE-COLOR)
                       (ellipse 15 (* 15 3) "solid" PETAL-COLOR)))
(define CENTER (circle (/ 15 2) "solid" CENTER-COLOR))
(define FLOWER (overlay CENTER
                        PETAL
                        (rotate 45 PETAL)
                        (rotate (* 2 45) PETAL)
                        (rotate (* 3 45) PETAL)))

;===================
; Data definitions:
;===================

(define-struct flower (x y scale))
;; flower is (make-flower Natural[0,WIDTH] Natural[0,HEIGHT] Integer)
;; interp. A flower at position (x, y) on the screen, and of scale "scale", which refers to the scale (out of 100) of the flower's size

(define flower1 (make-flower 20 20 15))

#;
(define (fn-for-flower f)
  (... (flower-x f)         ;Natural[0,WIDTH]
       (flower-y f)         ;Natural[0,HEIGHT]
       (flower-scale f)))   ;Integer[0,...)
;; Template rules used:
;;  - compound: 3 fields

;===================
; Functions:
;===================

;; flower -> flower
;; Run the animation with an initial flower f.
;; Start with (main (make-flower 0 0 0))

;; <no tests for main functions>

(define (main f)
  (big-bang f                     ; flower
    (on-tick   next-flower)       ; flower -> flower
    (to-draw   render-flower)     ; flower -> Image
    (on-mouse  handle-mouse)))    ; flower Integer Integer MouseEvent -> WS

;; flower -> flower
;; produce the next flower from the given flower, with the scale increasing by 1 every time, except when starting from 0

(check-expect (next-flower (make-flower   0   0  0)) (make-flower   0   0       0 ))
(check-expect (next-flower (make-flower 100 100 10)) (make-flower 100 100 (+ 10 1)))

; (define (next-flower f) f) ;Stub
; <Template taken from Data Definition>

(define (next-flower f)
  (cond [(= (flower-scale f) 0) f]
        [else (make-flower (flower-x f) (flower-y f) (+ 1 (flower-scale f)))]))

;; flower -> Image
;; Given a flower data-structure, create an image of it, if the size > 0 

(check-expect (render-flower (make-flower   0   0  0)) (place-image empty-image 0 0 MTS))
(check-expect (render-flower (make-flower 100 100 15)) (place-image (rotate (-(modulo 15 360)) (scale (/ 15 100) FLOWER))
                                                                    100
                                                                    100
                                                                    MTS))

; (define (render-flower f) FLOWER)
; <Template taken from data definition>

(define (render-flower f)
  (cond [(= (flower-scale f) 0) (place-image empty-image (flower-x f)
                                             (flower-y f) MTS)]
        [else (place-image (rotate (- (modulo (flower-scale f) 360)) (scale (/ (flower-scale f) 100) FLOWER))
                           (flower-x f)
                           (flower-y f)
                           MTS)]))

;; flower Integer Integer MouseEvent -> flower
;; on mouse click, initialize a flower at its x,y coordinates, with a scale of 1 

(check-expect (handle-mouse (make-flower 0 0 0) 100 100 "button-down") (make-flower 100 100 1))
(check-expect (handle-mouse (make-flower 0 0 0) 100 100 "button-up"  ) (make-flower   0   0 0))

;(define (handle-mouse f x y me) f)

#;
(define (handle-mouse ws x y me)
  (cond [(mouse=? me "button-down") (... ws x y)]
        [else
         (... ws x y)]))
;; Template Rule Used:
;;  -Mouse Event Template


(define (handle-mouse f x y me)
  (cond [(mouse=? me "button-down") (make-flower x y 1)]
        [else f]))