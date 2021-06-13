;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ImageExamples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;The following is like an import statement. The library below is needed for image functions
;This library is the How to Design Programs library version 2
(require 2htdp/image)

;The following creates a solid green circle of radius 20
(circle 20 "solid" "green")

;The following creates a rectangle outlines with blue
(rectangle 30 60 "outline" "blue")

;The following makes an image out of a string
(text "Hello" 24 "orange")

;The following stacks circles above each other. Sort of seeing them from the side
(above (circle 10 "solid" "red")
       (circle 20 "solid" "green")
       (circle 30 "solid" "blue"))

;The following stacks circles besides each other
(beside (circle 10 "solid" "red")
        (circle 20 "solid" "green")
        (circle 30 "solid" "blue"))

;The following stacks circles within each other.
(overlay (circle 10 "solid" "red")
         (circle 20 "solid" "green")
         (circle 30 "solid" "blue"))