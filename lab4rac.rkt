;14.1
(display "14.1")
;Функцiя для зчитування чисел написаних користувачем
(define (userInput s)
  (display "Please enter number ")
  (display s)
  (display ": ")
  (define n (read))
  n)
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
;Знаходимо усi шестикутнi числа й додаємо їх до списку
(define (listFilling nstart nend lis1)
  (cond
    ((not (= nstart nend))
     ;Додаємо до списку новий список, що складається з наступного шестикутного числа
     (listFilling (+ nstart 1) nend (my-append lis1 (list (- (* (* (+ nstart 1) (+ nstart 1)) 2) (+ nstart 1)))))
     )
    (else
     lis1
     )
    ))
;Знаходимо суму парних чисел списку
(define (evenNumbersSum lis1 sum)
  (cond
    ((not (= (my-length lis1) 0))
     (cond
       ((= (remainder (car lis1) 2) 0)
        (evenNumbersSum (cdr lis1) (+ sum (car lis1)))
        )
       (else
        (evenNumbersSum (cdr lis1) sum)
        )
       )
     )
    (else
     sum
     )
    )
  )
;Створюємо список в якому замінили значення, кратні 5, на значення, помножені на 10.
(define (changingMultiplesOfFive lis1 resultlist)
  (cond
    ((not (= (my-length lis1) 0))
     (cond
       ((= (remainder (car lis1) 5) 0)
        (changingMultiplesOfFive (cdr lis1) (my-append (list (* (car lis1) 10)) resultlist))
        )
       (else
        (changingMultiplesOfFive (cdr lis1) (my-append(list (car lis1)) resultlist))
        )
       )
     )
    (else
     resultlist
     )
    )
  )
;Створюємо пiдсписок зі значеннями, кратними 10.
(define (sublistCreate lis1 resultlist)
  (cond
    ((not (= (my-length lis1) 0))
     (cond
       ((= (remainder (car lis1) 10) 0)
        (sublistCreate (cdr lis1) (my-append (list (car lis1)) resultlist))
        )
       (else
        (sublistCreate (cdr lis1) resultlist)
        )
       )
     )
    (else
     resultlist
     )
    )
  )
(define n (userInput "n"))
(define lis1 (list))
(define empty (list))
(define lis1 (listFilling 0 n lis1))
(display "\nList of hexagonal numbers: \n")
(display lis1)
(display "\nEven numbers sum = \n")
(define sum (evenNumbersSum lis1 0))
(display sum)
(display "\nList of hexagonal numbers with changed multiples of five numbers: \n")
(define lis1 (changingMultiplesOfFive lis1 empty))
(define lis1 (reverse lis1))
(display lis1)
(display "\nSublist with multiples of ten numbers: \n")
(define lis2 (sublistCreate lis1 empty))
(define lis2 (reverse lis2))
(display lis2)


;14.2
(display "\n14.2\n")
;Створюємо список з кiлькiстю товарiв у точках
(define (allDemands points demandList num)
  (cond
    ((not (= points 0))
     (allDemands (- points 1) (my-append (list num) demandList) num)
     )
    (else
     demandList
     )
    )
  )
;Перевiрка на те чи достатньо товару у магазинi
(define (isThereEnoughProduct curProduct newcurProduct)
  (cond
    ((< newcurProduct 0)
     curProduct
     )
    (else
      newcurProduct
     )
    )
  )
;Перевiрка на те чи достатньо товару для точки
(define (isThereEnoughProductSpot curProduct newcurProduct newcurProductShop)
  (cond
    ((or (< newcurProduct 0) (< newcurProductShop 0))
     (display "\nНевдоволенний запит")
     curProduct
     )
    (else
     (display "\nЗапит вдоволено")
      newcurProduct
     )
    )
  )
;Функцiя виклику наступного дня
(define (nextDay day maxDays minusEachDay daysForDeliverToSpot daysForDeliverToShop expetcDaysForDeliver curProductSpot numberOfSpots curProductShop) 
    (cond
      ((not (= day maxDays))
        (display "\nДень ")
        (display day)
        (display ": ")
        (display "\nКожна точка витратила по ")
        (display minusEachDay)
        (display " товарiв")
        (cond
          ((= daysForDeliverToSpot 1)
           (display "\nКожна точка отримала по ")
           (display (* minusEachDay expetcDaysForDeliver))
           (display " товарiв")
           (cond
             ((= daysForDeliverToShop 1)
                (display "\nМагазин отримав ")
                (display (* 104 (* minusEachDay numberOfSpots)))
                (display " товарiв з заводу\n")
                (display "\nТовару на точках: ")
                (display (allDemands numberOfSpots empty (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver))))
                (display "\nТовару у магазинi: ")
                (display (+ (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver))) (* 104 (* minusEachDay numberOfSpots))))
                (nextDay (+ day 1) maxDays minusEachDay expetcDaysForDeliver 104 expetcDaysForDeliver (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) numberOfSpots (+ (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver))) (* 104 (* minusEachDay numberOfSpots))))
                )
             (else
                (display "\nТовару на точках: ")
                (display (allDemands numberOfSpots empty (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver))))))
                (display "\nТовару у магазинi: ")
                (display (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver)))))
                (nextDay (+ day 1) maxDays minusEachDay expetcDaysForDeliver (- daysForDeliverToShop 1) expetcDaysForDeliver (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver)))) numberOfSpots (isThereEnoughProductSpot (- curProductSpot minusEachDay) (+ (- curProductSpot minusEachDay) (* minusEachDay expetcDaysForDeliver)) (- curProductShop (* numberOfSpots (* minusEachDay expetcDaysForDeliver)))))
                )
             )
           )
          (else
            (display "\nТовари на точках: " )
            (display (allDemands numberOfSpots empty (- curProductSpot minusEachDay)))
            (cond
             ((= daysForDeliverToShop 1)
                (display "\nТовару у магазинi: ")
                (display (+ curProductShop (*(* 104 minusEachDay) numberOfSpots)))
                (display "\nМагазин отримав ")
                (display (* (* 104 minusEachDay) numberOfSpots))
                (display " товарiв з заводу\n")
                (nextDay (+ day 1) maxDays minusEachDay (- daysForDeliverToSpot 1) 104 expetcDaysForDeliver (- curProductSpot minusEachDay) numberOfSpots (+ curProductShop (*(* 104 minusEachDay) numberOfSpots)))
                )
            (else
                (nextDay (+ day 1) maxDays minusEachDay (- daysForDeliverToSpot 1) (- daysForDeliverToShop 1) expetcDaysForDeliver (- curProductSpot minusEachDay) numberOfSpots curProductShop)
                )
            )
            )
          )
        )
      )
  )
;Кiлькicть днiв симуляцiї            
(define maxDays 360)
;Витрати кожного дня однiєї точки
(define minusEachDay 10)
;Скiльки днiв очiкується доставка
(define expetcDaysForDeliver 6)
;Кiлькiсть точок
(define numberOfSpots 6)
(nextDay 1 maxDays minusEachDay 1 1 expetcDaysForDeliver 0 numberOfSpots 0)