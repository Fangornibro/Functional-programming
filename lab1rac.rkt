;14.1
(display "14.1\n")
;Функцiя для зчитування чисел написаних користувачем
(define (userInput s)
  (display "Please enter number ")
  (display s)
  (display ": ")
  (define n (read))
  n)
;Функцiя знаходження mod
(define (mod pow m)
  (define res (remainder pow m))
  res)
;Рекурсивна функцiя знаходження степеню
(define (powFunc b p)
  (cond
   ((= p 0) 1)
   ((= (remainder p 2) 0) (* (powFunc b (/ p 2)) (powFunc b (/ p 2))))
   (else (* b (powFunc b (- p 1)))))
  )
(define b (userInput "b"))
(define p (userInput "p"))
(define m (userInput "m"))
;Перевiрка чи користувач ввiв числа
(if (and (and (number? b) (number? p)) (number? m))
    (and
    (display "Result = ")
    (display (mod (powFunc b p) m))))
;14.2
(display " \n14.2\n")
(define (findAllUpaired n)
  (cond
    ((and (= (remainder n 2) 0) (> n 0)) (findAllUpaired (- n 1)))
    ((and (= (remainder n 2) 1) (> n 0)) (and (and (display n) (display " ")) (findAllUpaired (- n 2))))
   ))
(define n (userInput "n"))
(findAllUpaired n)