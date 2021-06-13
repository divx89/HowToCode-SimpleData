;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname 01Lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(cons 1 empty)                               ; List with 1 item, 1, on top of an empty list
(define L1 (cons 1 (cons 2 (cons 3 empty)))) ; Define a list of 3 items (over an empty list) and set it within variable L1

(first L1)                                   ; Display the 1st element of list L1
(rest L1)                                    ; Display all except the 1st element of L1
(first (rest L1))                            ; Display the 2nd element of L1
(first (rest (rest L1)))                     ; Display the 3rd element of L1
;(first (rest (rest (rest L1))))             ; This fails because non-empty list is expected

(cons "UBC"
      (cons "McGill"
            (cons "The team that shall not be named"
                  empty)))