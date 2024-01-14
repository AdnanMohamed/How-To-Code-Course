;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname simple-text-editor-starter2.bak) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; simple-text-editor-starter2.rkt

;
;  In this problem, you will be designing a simple one-line text editor.
;
;  The constants and data definitions are provided for you, so make sure
;  to take a look through them after completing your own Domain Analysis.
;
;  Your text editor should have the following functionality:
;  - when you type, characters should be inserted on the left side of the cursor
;  - when you press the left and right arrow keys, the cursor should move accordingly
;  - when you press backspace (or delete on a mac), the last character on the left of
;    the cursors should be deleted
;



(require 2htdp/image)
(require 2htdp/universe)

;; A simple editor

;; Constants
;; ===============

(define WORLD-WIDTH  600)
(define WORLD-HEIGHT 300)
(define MTS (empty-scene WORLD-WIDTH WORLD-HEIGHT))

(define CURSOR-W (/ WORLD-WIDTH  200))
(define CURSOR-H (/ WORLD-HEIGHT 10))
(define CURSOR-COLOR "black")
(define CTR-X    (/ WORLD-WIDTH   2))    ; x position of the editor's content
(define CTR-Y    (/ WORLD-HEIGHT  2))    ; y position of the editor's content

(define CURSOR (rectangle CURSOR-W CURSOR-H "solid" CURSOR-COLOR))

(define TEXT-COLOR "blue")
(define TEXT-SIZE  30)

;; DATA DEFINITIONS
;; ========================================

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor, post is the text after
(define E0 (make-editor "" ""))    ; |
(define E1 (make-editor "a" ""))   ; a|
(define E2 (make-editor "" "b"))   ; |b
(define E3 (make-editor "k" "b"))  ; k|b

#;
(define (fn-for-editor e)
  (... (editor-pre e)
       (editor-post e)))

;; template rules used:
;;  compound data: 2 fields

;; FUNCTIONS
;; ===========================

;; Editor->Editor
;; Runs big-bang with supplied Editor
(define (main e)
  (big-bang e
    [on-draw render]          ; Editor->Image
    [on-key  key-handler]))   ; Editor KeyEvent->Editor

;; Editor->Image
;; draws (editor-pre e) to the left of the cursor and
;; (editor-pre e) to the right of the cursor
(check-expect (render (make-editor "A" ""))
              (place-image (editor-img (make-editor "A" ""))
                           CTR-X CTR-Y
                           MTS))

(check-expect (render (make-editor "" "b"))
              (place-image (editor-img (make-editor "" "b"))
                           CTR-X CTR-Y
                           MTS))

(check-expect (render (make-editor "j" "c"))
              (place-image (editor-img (make-editor "j" "c"))
                           CTR-X CTR-Y
                           MTS))
;(define (render e) CURSOR)  ;stub

(define (render e)
  (place-image (editor-img e)
               CTR-X CTR-Y
               MTS))

;; Editor->Image
;; produce the image of the cursor between (editor-pre e) and (editor-post e)
(check-expect (editor-img (make-editor "A" ""))(beside
                                                (text "A" TEXT-SIZE TEXT-COLOR)
                                                CURSOR
                                                (text "" TEXT-SIZE TEXT-COLOR)))

(check-expect (editor-img (make-editor "" ""))(beside
                                               (text "" TEXT-SIZE TEXT-COLOR)
                                               CURSOR
                                               (text "" TEXT-SIZE TEXT-COLOR)))

(check-expect (editor-img (make-editor "" "h"))(beside
                                                (text "" TEXT-SIZE TEXT-COLOR)
                                                CURSOR
                                                (text "h" TEXT-SIZE TEXT-COLOR)))

(check-expect (editor-img (make-editor "A" "d"))(beside
                                                 (text "A" TEXT-SIZE TEXT-COLOR)
                                                 CURSOR
                                                 (text "d" TEXT-SIZE TEXT-COLOR)))
;(define (editor-img e) CURSOR)  ;stub
(define (editor-img e)
  (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOR)
          CURSOR
          (text (editor-post e) TEXT-SIZE TEXT-COLOR)))

;; Editor KeyEvent->Editor
;; If a character is pressed, then that character replaces (editor-pre e)
;; else if 'backspace' is pressed, then (editor-pre e) = ""
;; else if left arrow or right arrow is moved, then the cursor is moved accordingly
(check-expect (key-handler (make-editor "" "") "A")
              (make-editor "A" ""))
(check-expect (key-handler (make-editor "Ad" "an") "n")
              (make-editor "Adn" "an"))
(check-expect (key-handler (make-editor "" "oo") "m")
              (make-editor "m" "oo"))
(check-expect (key-handler (make-editor "Adj" "nan") "\b")
              (make-editor "Ad" "nan"))
(check-expect (key-handler (make-editor "" "Adnan") "\b")
              (make-editor "" "Adnan"))
(check-expect (key-handler (make-editor "Wond" "erful") "right")
              (make-editor "Wonde" "rful"))
(check-expect (key-handler (make-editor "live is sh" "ort") "left")
              (make-editor "live is s" "hort"))
(check-expect (key-handler (make-editor "" "A") "left")
              (make-editor "" "A"))
(check-expect (key-handler (make-editor "I am at the end" "") "right")
              (make-editor "I am at the end" ""))

;(define (key-handler e ke) e)  ;stub

(define (key-handler e ke)
  (cond [(and (string=? ke "left")
              (> (string-length (editor-pre e)) 0))
         (make-editor (substring (editor-pre e) 0 (sub1 (string-length (editor-pre e))))
                      (string-append (string-ith (editor-pre e) (sub1 (string-length (editor-pre e))))
                                     (editor-post e)))]

        [(and (string=? ke "right")
              (> (string-length (editor-post e)) 0))
         (make-editor (string-append (editor-pre e)
                                     (string-ith (editor-post e) 0))
                      (substring (editor-post e) 1))]
        

        [(and (string=? ke "\b")
              (> (string-length (editor-pre e)) 0))
         (make-editor
          (substring (editor-pre e) 0 (sub1 (string-length (editor-pre e))))
          (editor-post e))]

        [(not (or (string=? ke "left")
                  (string=? ke "right")
                  (string=? ke "\b")))
         (make-editor
          (string-append (editor-pre e) ke)
          (editor-post e))]

        [else e]))

(main (make-editor "" ""))