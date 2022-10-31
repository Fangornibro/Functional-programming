;Запис рядків у файл
(let ((port (open-output-file "C:\\Users\\kuzmi\\Desktop\\Функціональне програмування\\Files\\output.txt"))) 
(write '(When I find myself in times of trouble) port)
(write '(Mother Mary comes to me and i get) port)
(close-output-port port))

;Виведення рядків файлу в заданій послідовності
(define (get-song n)  ; вибрати номер рядка файлу
  (let ((port (open-input-file "C:\\Users\\kuzmi\\Desktop\\Функціональне програмування\\Files\\output.txt")))  ; відкрити файл
    (skip-songs (- n 1) port)  ; пропустити рядок
    (let ((answer (read port)))
      (close-input-port port)
      answer)))

(define (skip-songs n port)   ; пропустити рядки
  (if (= n 0)
      'done
      (begin (read port)
             (skip-songs (- n 1) port))))

;Виведення файлу на екран як в блокноті
(define (print-file name) 
  (let ((port (open-input-file name))) ;відкрити порт
    (print-file-helper port)           ;вивести рядки файлу
    (close-input-port port)            ; закрити порт
    'done))

(define (print-file-helper port)     ; допоміжна процедура друку файлу
  (let ((action (read port)))         ; читати порт    
      (if (eof-object? action)        ;якщо кінець файлу 
        'done                         ; робота звкінчена
        (begin (write action)         ; інакше писати файл
              (print-file-helper port)))))

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
;Непарнi
(define (unpairedElements lis newlis i)
  (cond
    ((> i 0)
     (cond
       ((= (remainder i 2) 1)
        (unpairedElements (cdr (cdr lis)) (my-append newlis (list (car lis))) (- i 2)))
       (else
        (unpairedElements (cdr lis) newlis (- i 1))))
     )
    (else
     newlis
     ))
  )
;Парнi
(define (pairedElements lis newlis i)
  (cond
    ((> i 0)
     (cond
       ((= (remainder i 2) 0)
        (pairedElements (cdr (cdr lis)) (my-append newlis (list (car lis))) (- i 2)))
       (else
        (pairedElements (cdr lis) newlis (- i 1))))
     )
    (else
     (my-append newlis (list (car lis)))
     ))
  )
;Створення списку-результату 
(define (combine lis1 lis2 res)
  (cond
    ((not (null? lis1))
     (combine (cdr lis1) (cdr lis2) (my-append (my-append res (list (car lis1))) (list (car lis2)))))
    (else
     res))
  )


(display "Printed file:") ; виклик процедур 
(newline)
(print-file "C:\\Users\\kuzmi\\Desktop\\Функціональне програмування\\Files\\output.txt")
(define empty (list))
(define lis1 (get-song 1))
(define lis2 (get-song 2))
(define s1 (unpairedElements lis1 empty (- (my-length lis1) 1)))
(define s2 (unpairedElements lis2 empty (- (my-length lis2) 1)))
(display (combine s1 s2 empty))