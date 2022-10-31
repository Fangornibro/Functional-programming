;13.1
(display "13.1\n")
;Функцiя для зчитування чисел написаних користувачем
(define (userInput s)
  (display "Please enter variable ")
  (display s)
  (display ": ")
  (define n (read))
  n)
;Множення двух векторiв
(define vectorMultiplication
  (lambda (vec-1 vec-2)
    (let* ((len (vector-length vec-1))
           (result (make-vector len)))
      (do ((index 0 (+ index 1)))
          ((= index len) result) 
        (vector-set! result index
                     (* (vector-ref vec-1 index)
                        (vector-ref vec-2 index)))))))
;Сума елементiв вектора
(define vectorSum
  (lambda (vec)
    (let ((len (vector-length vec))
          (result 0))        
      (do ((index 0 (+ index 1)))
          ((= index len) result)
        (set! result (+ result (vector-ref vec index)))))))
(display "Dot product: ")
(define a1x (userInput "a1x"))
(define a1y (userInput "a1y"))
(define a2x (userInput "a2x"))
(define a2y (userInput "a2y"))
(define multVectors (vectorMultiplication (vector a1x a1y) (vector a2x a2y)))
(define scalarVector (vectorSum multVectors))
(display scalarVector)
(display "\nAngle: ")
(display (/ scalarVector (* (sqrt (vectorSum (vectorMultiplication (vector a1x a1y) (vector a1x a1y)))) (sqrt (vectorSum (vectorMultiplication (vector a2x a2y) (vector a2x a2y)))))))
;13.2
(display "\n13.2\n")
;Iнiцiалiзуємо чергу
(define (make-queue)
 (define p (cons '() '() ) )
 (cons p p)
)
;Перевірка черги на пустоту
(define (null-queue? q)
 (and
  (eq? (front q) (rear q)) (eq? (car (front q)) '() ))
)
;Селектор (доступ) до першого елемента черги
(define (front q)
 (car q)) 
;Селектор (доступ) до останнього елемента черги
(define (rear q)
 (cdr q))
;Додавання нового елемента в чергу
(define (push q e)
 (define p (cons e '()))
 (if (null-queue? q)
  (begin (set-car! q p)
   (set-cdr! q p)
  )
  (begin
   (set-cdr! (rear q) p)
   (set-cdr! q p)
  ) ) )
;Вилучення елемента з черги
(define (pop q)
 (define x 0)
 (if (null-queue? q)
  'Empty
  (if (and (eq? (front q) (rear q))  (eq? '() (cdr (front q)))   )
   (begin
    (set! x (car (front q)))
    (set-car! (front q) '() )
    x ) 
   (begin
    (set! x (car (front q)))
    (set-car! q (cdr (front q)) )
    x ))))
;Додавання двох списків
(define (my-append lis1 lis2)
  (cond ((null? lis1)
         lis2)
        (else
         (cons (car lis1)
               (my-append (cdr lis1) lis2)))))
;Кількість елементів списку
(define (my-length lis)
   (cond ((null? lis)
          0)
         (else
          (+ 1 (my-length (cdr lis))))))
;Задачi та прiорiтети
(define (listFilling nstart nend lis1)
  (cond
    ((not (= nstart nend))
     (listFilling (+ nstart 1) nend (my-append lis1 (list (userInput (+ nstart 1)))))
     )
    (else
     lis1
     )
    ))
;Заповнення черги
(define (queueFilling nstart nend q empty)
  (cond
    ((not (= nstart nend))
     (push q (list (listFilling 0 2 empty)))
     (queueFilling (+ nstart 1) nend q empty)
     )
    (else
     q
     )
    ))
;Елемент черги
(define (El q t)
  (cond
    ((not (= t 0))
     (pop q)
     (El q (- t 1))
     )
    (else
     (front (front q))
     )
    )
  )
(define n (userInput "n"))
(define empty (list))
(define q (make-queue))
(define q (queueFilling 0 n q empty))
(display "\nQueue: \n")
(display (front q))
(define s (userInput "element to find"))
(display "\nElement ")
(display s)
(display ":")
(El q s)