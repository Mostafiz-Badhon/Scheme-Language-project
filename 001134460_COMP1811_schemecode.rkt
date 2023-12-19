;; Coding cards
(require racket/list)  ;; shuffle

;; F1 - The cards
;; Returns true if val is a pair and contains numbers 1-7, J, Q, K and the suite
(define (card? val) ;; val is the card (pair)
  (and
   (pair? val)
   (list? (member (car val) '( 1 2 3 4 5 6 7 #\J #\Q #\K)))
   (list? (member (cdr val) '( #\S #\C #\H #\D)))
   )
  )

;; Returns the suite of the card (S, C, H, D)
(define (suite card) 
  (cdr card)
  )

;; Returns the number or face of the card
(define (numeral card)
  (car card)
  )

;; Checks if the card is a Jack, Queen or King
(define (face? card)
  (list? (member (car card) '( #\J #\Q #\K)))
  )

;; Turns the card formatted from a dotted pair to a human readable string
(define (card->string val)
  (format "~a~a" (car val)(cdr val))
  ) 

;; Returns the value of the card
(define (value card)
  (cond
    [(face? card) 0.5] ;; If it is a face, the value is 0.5
    [else (car card)] ;; Otherwise, the value is the number on the card
  ))

;; F2 .- The deck.
;; Check if it is a list of cards (deck)
(define (deck? deck)
  (cond
    [(empty? deck) #f] ;; Without this, an empty deck would return true.
    [else
     (andmap card? deck)] ;; Checks every member of the deck if it is a card
   )
  )

;; Value of the deck of cards given
(define (valueOf deck) 
  (cond
    [(empty? deck) 0] ;; If deck is empty, value is 0
    [else ;; Otherwise, recursively adds the value of the first card of the deck
     (+
      (value (first deck)) 
      (valueOf (rest deck)))])
  )

;; 
(define (do-suite val) ;; val is the suite
  (map
   (lambda (m) (cons m val))
   '( 1 2 3 4 5 6 7 #\J #\Q #\K)))

;; Generate a deck of cards
(define deck
  (append (do-suite #\S) (do-suite #\C) (do-suite #\H) (do-suite #\D)))
  
;; Turns the deck into  human readable string
(define (deck->strings deck) ;; hrdeck = human readable deck
  (map card->string deck))

;;(define (playS deck hand strategy)
;;F3 - Probabilities.

(define (count comp num card)
  (if (comp (value card) num)
      1
      0))

(define (probability comp num deck)
  (apply +
         (map (lambda (x)
                (count comp num x)) deck)))

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