;; Spades, clubs, Hearts, Diamonds
;; Coding cards
(require racket/list)  ;; shuffle

;--------------------------;
;       F1 The cards       ;

; i) Card? returns true if x is a card, false otherwise

(define card?                   ; val: boolean
  (lambda (x)                   ; x: any 
    (cond
      ((and
        (pair? x)
        (or                     ; check if numeral or face is correct
         (equal? (car x) 1)
         (equal? (car x) 2)
         (equal? (car x) 3)
         (equal? (car x) 4)
         (equal? (car x) 5)
         (equal? (car x) 6)
         (equal? (car x) 7)
         (equal? (car x) #\J)
         (equal? (car x) #\Q)
         (equal? (car x) #\K))
        (or                     ; check if suit is correct
         (equal? (cdr x) #\H)
         (equal? (cdr x) #\S)
         (equal? (cdr x) #\C)
         (equal? (cdr x) #\D))) true)
      (else false))
    ))


; ii) suite returns the suite of x
;     numeral returns the numeral of x
 
(define suite    ; val: char
  (lambda (x)    ; x: card
    (cdr x)
    ))


(define numeral  ; val: number or char
  (lambda (x)    ; x: card 
    (car x)
    ))


; iii) face? returns true if x is a face card or false if it is not.

(define face?        ; val: boolean
  (lambda (x)        ; x: card
    (char? (car x))
    ))


; iv) value returns the value of x

(define value      ; val: number
  (lambda (x)      ; x: card
    (if (face? x)
        0.5
        (numeral x))
    ))


; v) card->string returns a readable string of a card

(define card->string  ; val: string
  (lambda (x)         ; x: card
    (if (face? x)
        (string (numeral x) (suite x))
        (string-append
         (number->string (numeral x))
         (string (suite x))))
    ))


;--------------------------;
;       F2 The deck        ;


; i) deck? returns true if x is a valid deck or false otherwise

(define deck?        ; val: boolean
  (lambda (x)        ; x: list of values
    (cond
      ((null? x) true)
      ((card? (car x)) (deck? (cdr x)))
      (else false))
    ))


; ii) valueOf calculate the sum of the value of the cards in a deck

(define valueOf    ; val: number
  (lambda (x)      ; x: deck
    (if (null? x)
        0
        (+ (valueOf (cdr x)) (value (car x))))
    ))

; iii) do-suite returns the whole serie of a valid suit

(define do-suite   ; val: deck
  (lambda (x)      ; x: suit
    (map cons
         '(1 2 3 4 5 6 7 #\J #\Q #\K)
         (list x x x x x x x x x x))
    ))

; iv) deck includes the entire list of all the valid cards

(define deck      ; deck: deck of cards
  (append
   (do-suite #\H)
   (do-suite #\S)
   (do-suite #\C)
   (do-suite #\D)
   ))

; v) deck->strings returns a human representation of a deck

(define deck->strings    ; val: list of strings
  (lambda (x)             ; x: deck
    (map card->string x)
    ))

; vi) strings->deck returns a deck given a list of formatted strings

;     string->card returns a card of a readable string

(define string->card  ; val: card
  (lambda (x)         ; x: string
    (cons
     (if (char-numeric? (string-ref x 0))
         (- (char->integer (string-ref x 0)) 48)
         (string-ref x 0))
     (string-ref x 1))
    ))


(define strings->deck   ; val: deck
  (lambda (x)           ; x: list of strings
    (map string->card x)
    ))
  


;--------------------------;
;    F3 Probabilities      ;


; i) probability returns the number of cards in list having values less, greater or equal than the provided number

(define probability         ; val: number
  (lambda (comp num list)   ; comp: procedure, num: number, list: deck
    (cond
      ((null? list) 0)
      ((comp (value (car list)) num)
       (+ 1 (probability comp num (cdr list))))
      (else (probability comp num (cdr list))))
    ))

(define cheat #f)




;; F4.- Game.
;; DO NO CHANGE THE  FUNCTIONS BELOW THIS LINE
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
(define (show-statistics deck hand)
  (let
      ([toCheck (- 7.5 (valueOf hand))])
    (display
     (format
      "P(>7.5):~a/~a\nP(<7.5):~a/~a\nP(=7.5):~a/~a\nHAND:~a~nVALUE:~a\nDECK:~a...\n"
      (probability > toCheck deck)
      (length deck)
      (probability < toCheck deck)
      (length deck)
      (probability = toCheck deck)
      (length deck)                     
      (deck->strings hand)
      (valueOf hand)
      (if cheat (deck->strings (take deck
                                     (max 0 
                                          (min 4 (length deck) )))) "****")
      )
     )))
  
;; Human interaction.
(define (play deck hand)
  (begin      
    (show-statistics deck hand)
    ;; Control
    (cond
      [(= (valueOf hand) 7.5) (display "WIN")]
      [(> (valueOf hand) 7.5) (display "LOST")]
      [(empty? deck) (display "NO CARDS LEFT") ]
      [(let
           ([ command (read)])
         (cond
           [(equal? command 'accept)
            (play (rest deck) (cons (first deck) hand))]
           [(equal? command 'pass)
            (play (drop deck 1) hand)]
           [(equal? command 'end) (void)]
           [else (play deck hand)]))])))