;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname maze-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct junction (left straight right))
       
;; Maze is one of:
;; - false
;; - "finish"
;; - (make-junction Maze Maze Maze)
;; a maze where at each junction you can either go straight, left or right. 
;; false means dead end, "finish" means you've reached the end of the maze
(define M0 false)      ; a maze that is a dead end
(define M1 "finish")   ; a maze where you are already at the finish
(define M2 (make-junction 
            (make-junction false 
                           (make-junction false
                                          false 
                                          (make-junction false
                                                         false 
                                                         false))
                           (make-junction false
                                          (make-junction 
                                           (make-junction false
                                                          false 
                                                          false)
                                           (make-junction false 
                                                          "finish"
                                                          false)
                                           false)
                                          false))
            (make-junction false 
                           false 
                           (make-junction false
                                          false
                                          false))
            false))    ; a maze