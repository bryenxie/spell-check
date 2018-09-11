;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname suggest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; OPTIONAL -- spellcheck.rkt provides the following function:
;;
;; (spellcheck? s) determines if s is spelled correctly
;;   (according to a medium-sized wordlist)
;; spellcheck: Str -> Bool
;;
;; You may use this function for your own experiments
;; (and to show off your program to your friends & family)

;; Do NOT leave this require in your file when you submit your code.
;; (require "spellcheck.rkt")
;; [this file will be available after the A07 deadline]
;; NOTE: You do not need to open spellcheck.rkt in DrRacket to use it
;;       (opening the file in DrRacket may slow down your computer).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Place your identifying information here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: you should complete the documentation & tests (design recipe)
;; for all functions (except remove-at and remove-letters)
;; But remember, because most of your functions will not have a cond
;; or much logic, exhaustive tests may not be required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; A Word is a Str
;; requires: only lowercase letters appear in the word
;;           (no spaces, punctuation, etc.)

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))

;; 4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(remove-dups slst) remove all the duplicates elements in slst
;; remove-dups: (listof Word) -> (listof Word)
;; requires: slst is sorted in non-decreasing order
;; Examples
(check-expect (remove-dups (list 1 1 2 2 3 3)) (list 1 2 3))
              
(define (remove-dups slst)
  (foldr (lambda (elem r-lst)
            (cond
              [(not(member elem r-lst)) (cons elem r-lst)]
              [else r-lst]))
            empty slst))

;; Test
(check-expect (remove-dups empty) empty)
(check-expect (remove-dups (list 1 2 3)) (list 1 2 3))

;; 4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(ifoldr combine base lst) a foldr that can track the index
;; ifoldr: (Nat X Y -> Y) Y (listof X) -> Y
;; Example
(check-expect (remove-at 0 '()) '())

(define (ifoldr combine base lst)
  (local
    [(define index-lst (build-list (length lst) identity))]
    (ifoldr-helper combine base lst index-lst)))

;;(ifoldr-helper combine base lst index-lst) can helper function
;; for foldr that have a index-lst which track the index
;; Example
(check-expect (remove-at 0 '(a b c d e f)) '(b c d e f))

(define (ifoldr-helper combine base lst index-lst)
  (cond
    [(empty? lst) base]
    [else (combine (first index-lst) (first lst)
                   (ifoldr-helper combine base (rest lst) (rest index-lst)))]))

;;tested below by using other functions
 
;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-at i lst) removes element with index i from lst
;; remove-at: Nat (listof Any) -> (listof Any)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-at 0 '(a b c d)) '(b c d))
(check-expect (remove-at 3 '(a b c d)) '(a b c))
(check-expect (remove-at 0 '()) '())

(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))

;; (remove-letters s) produces a list of Words,
;;    each with one letter removed from s
;; remove-letters: Word -> (listof Word)
;; Examples:
;;;; COMMENTED OUT FOR NOW: THESE EXAMPLES RELY ON ifoldr [above]
(check-expect (remove-letters "abc") '("bc" "ac" "ab"))
(check-expect (remove-letters "") '())

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc) (lambda (i) (list->string (remove-at i loc))))))

;;test
(check-expect (remove-letters "java") '("ava" "jva" "jaa" "jav"))

;; 4c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------------------insert-------------------------------

;;(insert-letters s) produce a list that contains every possible letter (a..z)
;; inserted before each letter in the word
;; insert-letters: Word -> (listof Word)
;; Example
(check-expect (insert-letters "a" )
              (list "aa" "ba" "ca" "da" "ea" "fa"
                    "ga" "ha" "ia" "ja" "ka" "la"
                    "ma" "na" "oa" "pa" "qa" "ra"
                    "sa" "ta" "ua" "va" "wa" "xa"
                    "ya" "za"))

(define (insert-letters s)
  (local [(define loc (string->list s))
          (define alpha-list (string->list "abcdefghijklmnopqrstuvwxyz"))]
         (build-list (* 26 (length loc))
                (lambda (i) (list->string (insert loc (floor (/ i 26))
                                                  (list-ref alpha-list (remainder i 26))))))))

;;test
(check-expect (insert-letters "") empty)
          
;;(insert loc index alpha)  produce a list that insert alpha in loc at index
;; insert : (Listof Char) Nat Char -> (Listof Char)
;; Example
(check-expect (insert (list 'a 'b 'c 'd) 3 'd) (list 'a 'b 'c 'd 'd))

(define (insert loc index alpha)
  (ifoldr (lambda (k x y)
            (cond
              [(= k index) (cons alpha (cons x y))]
              [else (cons x y)]))
              empty loc))
;;test
(check-expect (insert (list 'a 'b 'c 'd) 0 'd) (list 'd 'a 'b 'c 'd))

;;-------------------trailing--------------------------------------

;;(trailing-letters s) produces a list every possible letter inserted
;; at the end of the word
;; trailing-letters: Word -> (listof Word)
;; Example
(check-expect (trailing-letters "jave") (list
 "javea" "javeb" "javec" "javed" "javee"
 "javef" "javeg" "javeh" "javei" "javej"
 "javek" "javel" "javem" "javen" "javeo"
 "javep" "javeq" "javer" "javes" "javet"
 "javeu" "javev" "javew" "javex" "javey"
 "javez"))

(define (trailing-letters s)
  (local [(define loc (string->list s))
          (define alpha-lst (string->list "abcdefghijklmnopqrstuvwxyz"))]
            (build-list 26 (lambda (i) (list->string (trail loc (sub1 (length loc))
                                                    (list-ref alpha-lst i)))))))

;;test
(check-expect (trailing-letters "jc") (list
 "jca" "jcb" "jcc" "jcd" "jce" "jcf"
 "jcg" "jch" "jci" "jcj" "jck"
 "jcl" "jcm" "jcn" "jco" "jcp"
 "jcq" "jcr" "jcs" "jct" "jcu"
 "jcv" "jcw" "jcx" "jcy" "jcz"))

;;(trail loc length alpha) produce a list that insert alpha
;; at end of loc
;; trail : (Listof Char) Num Char -> (Listof Char)
;; Example
(check-expect (trail '(a b c d e) 4 'm) '(a b c d e m))
 
(define (trail loc length alpha)
  (ifoldr (lambda (k x y)
            (cond
              [(= k length) (cons x (cons alpha empty))]
              [else (cons x y)]))
              empty loc))

;;test
(check-expect (trail empty 4 'm) '())

;;-------------------replace-------------------------------------

;;(replace-letters s) produce a list that contains words
;; that each letter of s is raplaced with every possible letter
;; replace-letters: Word -> (listof Word)
;; Example
(check-expect (replace-letters "joe")
(list "aoe" "boe""coe"
 "doe" "eoe" "foe" "goe" "hoe" "ioe" "koe" "loe" "moe" "noe" "ooe" "poe"
 "qoe" "roe" "soe" "toe" "uoe" "voe" "woe" "xoe" "yoe" "zoe" "jae" "jbe"
 "jce" "jde" "jee" "jfe" "jge" "jhe" "jie" "jje" "jke" "jle" "jme" "jne"
 "jpe" "jqe" "jre" "jse" "jte" "jue" "jve" "jwe" "jxe" "jye" "jze" "joa"
 "job" "joc" "jod" "jof" "jog" "joh" "joi" "joj" "jok" "jol" "jom" "jon"
 "joo" "jop" "joq" "jor" "jos" "jot" "jou" "jov" "jow" "jox" "joy" "joz"))

(define (replace-letters s)
   (local [(define loc (string->list s))
          (define alpha-lst (string->list "abcdefghijklmnopqrstuvwxyz"))]
     (remove-all s (build-list (* 26 (length loc)) (lambda (i) (list->string
                                                   (replace loc (floor (/ i 26)) (list-ref alpha-lst (remainder i 26)))))))))

;;test
(check-expect (replace-letters "a")(list
 "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"
 "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

;;(replace loc index alpha) replace the element at index in
;; loc with alpha
;; replace : (Listof Char) Nat Char -> (Listof Char)
;; Example
(check-expect (replace '(a b c d e) 2 'f) '(a b f d e)) 

(define (replace loc index alpha)
  (ifoldr (lambda (k x y)
            (cond
              [(= k index) (cons alpha y)]
              [else (cons x y)]))
          empty loc))

;; test
(check-expect (replace '(a b c d e) 4 'f) '(a b c d f)) 

;;-------------------swap---------------------------------------

;;(define swap-letters s) produce a list that contains word
;; that each adjacent pait of letters in s
;; swap-letters: Word -> (listof Word)
;; Example
(check-expect (swap-letters "jack") (list "ajck" "jcak" "jakc"))

(define (swap-letters s)
  (local [(define loc (string->list s))]
    (remove-all s (build-list (sub1 (length loc)) (lambda (i) (list->string
                                                 (swap loc i)))))))

;; test
(check-expect (swap-letters "aoe") (list "oae" "aeo"))

;;(swap loc index) produce a list that swap the element at index and
;; the element after index for loc
;; swap : (Listof Char) Nat -> (Listof Char)
;; Example
(check-expect (swap '(a b c d e f) 2) (list 'a 'b 'd 'c 'e 'f))
 
(define (swap loc index)
  (ifoldr (lambda (k x y)
            (cond
              [(= k index) (cons (first y) (cons x (rest y)) )]
              [else (cons x y)]))
          empty loc))

;; test
(check-expect (swap '(a b c d e f) 4) (list 'a 'b 'c 'd 'f 'e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You are not required to modify the definition of suggest,
;; but you may if you wish

;;(suggest s valid?) produces a list of correcty spelled
;; words that are "close" to the consumed word
;; suggest: Word (Word -> Bool) -> (listof Word)
;; Example
(check-expect (suggest "right" (lambda (x) (member x '("rights" "right" "fight" "aardvark" "fhqwhgads" "bright"))))
              '("bright" "fight" "rights"))
              
(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))

;;test
(check-expect (suggest "racket" (lambda (x) (member x '("good" "smart" "no" "acket" "racke" "racketa"))))
              '("acket" "racke" "racketa"))
