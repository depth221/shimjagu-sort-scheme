; 리스트의 마지막 요소를 반환
(define (Last L)
  (cond ((null? L) (display 'error))
        ((null? (cdr L)) (car L))
        (else (Last (cdr L)))
  )
)

; 리스트에서 마지막 요소를 제외한 부분을 반환
(define (ListWithoutLast L)
  (cond ((null? L) (display 'error))
        ((null? (cdr L)) '())
        (else (cons (car L) (ListWithoutLast (cdr L))))
  )
)

; 리스트에서 가장 큰 요소를 맨 오른쪽으로 옮기는 함수
(define (Bubble L)
  (cond ((null? L)                 L)
        ((null? (cdr L))           L)
        ((> (car L) (car (cdr L))) (cons (car (cdr L)) (Bubble (cons (car L) (cdr (cdr L))))))
        (else                      (cons (car L) (Bubble (cdr L))))
  )
)

; 가장 큰 요소가 맨 오른쪽에 있는(= Bubble 함수를 실행한) L1 리스트와
; 이미 정렬이 끝난 L2 리스트를 받아
; L1 리스트의 맨 오른쪽 요소를 L2의 맨 왼쪽에 넣고
; L1 리스트의 나머지 부분에 다시 Bubble 함수를 적용한 후 재귀 실행
(define (BubbleSort_ L1 L2)
  (cond ((null? L1) L2)
        ((null? (cdr L1)) (cons (car L1) L2))
        (else (BubbleSort_ (Bubble (ListWithoutLast L1)) (cons (Last L1) L2)))
  )
)

; BubbleSort_ 함수가 리스트 2개를 인자로 받으므로
; 리스트 1개만을 인자로 받아 BubbleSort_에 대신 전달하는 함수
(define (BubbleSort L)
  (BubbleSort_ (Bubble L) '())
)