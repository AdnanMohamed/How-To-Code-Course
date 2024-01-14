;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname tree2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

(define W 37)
(define H (* 3.5 W))
(define SIDE_LEN (* 2.5 W))

(define (tree n)
  (cond
    [(= n  0)
        (rectangle W H "solid" "brown")
        ]
    [(> n 0)
        (underlay/offset (tree (- n 1))
                        0 (* n -0.4  W)
                        (triangle SIDE_LEN "solid" "seagreen")
                        )
        ]
  ))

(tree 2)
        