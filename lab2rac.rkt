(display "14.1")
;14.1
;Рекурсивна функцiя розвитку кореня у ряд Маклорена 
(define (sqrtx x n t)
  (cond
   ((> n 0)
    (cond
      ((= t 1)
       (* 0.5 (+ (sqrtx x (- n 1) t) (/ (- 15 (* x x)) (sqrtx x (- n 1) t))))
       )
      (else
       (* 0.5 (+ (sqrtx x (- n 1) t) (/ (+ x (* x x)) (sqrtx x (- n 1) t))))
       )
      )
    )
   (else
    1
    )
   )
  )
;Функцiя, що змінює x від -2 до 2 з кроком 0.5
(define (step x)
  (cond
      ((>= x 1)
       (display "\n")
       (display "x = ")
       (display x)
       (display "\n")
       (display (/ 1 (sqrtx x 6 1)))
       (display "\n")
       (display "Похибка: ")
       (display (- (sqrtx x 6 1) (sqrt (- 15 (* x x)))))
       (step (- x 0.5))
       )
      ((>= x -1)
       (display "\n")
       (display "x = ")
       (display x)
       (display "\n")
       (display (/ 1 (sqrtx x 6 2)))
       (display "\n")
       (display "Похибка: ")
       (display (- (sqrtx x 6 2) (/ 1 (sqrt (+ x (* x x))))))
       (step (- x 0.5))
       )
      )
  )

(step 2)
(display "\n14.2\n")
;14.2
;Функцiя для зчитування чисел написаних користувачем
(define (userInput s)
  (display "Please enter number ")
  (display s)
  (display ": ")
  (define n (read))
  n)

;Функцiя, що виписує всi числа починаючи вiд заданого й до 0
(define (findall n)
  (cond
      ((not (= n 0))
       (findall (- n 1))
       (display n)
       (display " ")
       )
      )
  )

(define (sportPos n1 n2 n3)
  (cond  
    ((not (= n1 0))
     (display "Перший вид спорту: \n")
     (findall n1)
     (display "\n")
     (display "Другий вид спорту: \n")
     (findall n2)
     (display "\n")
     (display "Третiй вид спорту: \n")
     (findall n3)
     (display "\n")
     (display "\n")
     ;Вiднiмаємо вiд першого й додаємо до другого й визиваємо функцiю ще поки перший не стане 0
     (sportPos (- n1 1) (+ n2 1) n3)
        )
        )
  (cond  
    ((not (= n2 0))
     ;Вiднiмаємо вiд другого й додаємо до третього й визиваємо функцiю ще поки перший не стане 0
     (sportPos n1 (- n2 1) (+ n3 1))
        )
        )
)
(define n (userInput "n"))
(sportPos n 0 0)

