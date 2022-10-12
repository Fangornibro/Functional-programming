;14.1
;Метод простої ітерації
;e**x + ln(x) - 10 * x = 0
;x = (e**x + ln(x))/10
;fi'(x) = (e**x)/10 + 1/(10*x)
;fi''(x) = (e**x)/10 - 1/(10*x**2)
(display "14.1")
(display "\nМетод простої iтерацiї")
;Розрахунок функцiї
(define (f x0)
    (/ (+ (expt (exp 1) x0) (log x0 (exp 1))) 10)
  )
;Розрахунок похiдної вiд функцiї
(define (df x0)
    (+ (/ (expt (exp 1) x0) 10) (/ 1 (* 10 x0)))
  )
(define (iterations x0)
  (display "\nx0 = \n")
  (display x0)
  (display "\nx1 = \n")
  (display (f x0))
  (cond
    ((real? (f x0))
     (display "\ndifference = ")
     (display (abs (- x0 (f x0))))
     (if (> (abs (- x0 (f x0))) 0.001)
         (iterations (f x0)))))
)        

(define a 0.5)
(define b 1)
;Як початкове наближення виберемо:
(define x0 (/ (+ a b) 2))
;Перевірка на збіжність
(define check (df x0))
(cond
  ((< check 1)
   (display "\nСonvergence check: ")
   (display check)
   (display " < 1       Success")
   (iterations x0)
   )
  (else
   (display "\nСonvergence check: ")
   (display check)
   (display " < 1       Fail")
   )
  )
(display "\nМетод дотичних\n")

;Розрахунок подвiйної похiдної вiд функцiї
(define (d2f x0)
    (- (/ (expt (exp 1) x0) 10) (/ 1 (* 10 (* x0 x0))))
  )
;Рекурсивна функцiя з формулою дотичних
(define (tangent xn i)
  (display "\n")
  (display (+ i 1))
  (display  "-th iteration = ")
  (display (- xn (/ (f xn) (df xn))))
  (display  "; difference = ")
  (display (abs (- xn (- xn (/ (f xn) (df xn))))))
  (if (> (abs (- xn (- xn (/ (f xn) (df xn))))) 0.001)
      (tangent (- xn (/ (f xn) (df xn))) (+ i 1))
      )
  )
;Початкове приближення
(define xn (- x0 (/ (f x0) (df x0))))
(tangent xn 0)
(display "\n14.2")
;Розрахунок функцiї
(define (f1 x0)
    (* x0 (expt (exp 1) (* -1 x0)))
  )
;Цикл лiвого прямокутника
(define (left_rect_cycle a h n i sum)
  (cond
    ((<= i (- n 1))
     (left_rect_cycle a h n (+ i 1) (+ sum (* h (f1 (+ a (* i h))))))
     )
    (else
     sum
     )
    )
  )
;Цикл правого прямокутника
(define (right_rect_cycle a h n i sum)
  (cond
    ((<= i n)
     (left_rect_cycle a h n (+ i 1) (+ sum (* h (f1 (+ a (* i h))))))
     )
    (else
     sum
     )
    )
  )
;Цикл Сiмпсона
(define (Sympson_cycle a h n i sum)
  (cond
    ((<= i (- n 1))
     (Sympson_cycle a h n (+ i 1) (+ sum (* (+ 2 (* 2 (remainder (+ i 1) 2))) (f1 (+ a (* i h))))))
     )
    (else
     sum
     )
    )
  )
(define a1 1)
(define b1 2)
(define n 1000)
(define h (/ (- b1 a1) n))
(display "\nLeft rectangle ")
(display (left_rect_cycle a1 h n 0 0))
(display "\nRight rectangle ")
(display (right_rect_cycle a1 h n 1 0))
(display "\nSympson ")
(display (/ (Sympson_cycle a1 h n 1 (* (+ (f1 a1) (f1 b1)) (/ h 3))) 3000))