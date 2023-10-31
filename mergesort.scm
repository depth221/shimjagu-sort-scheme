; 리스트의 홀수 번째 요소를 반환
(define (SplitOdd L)
  (cond ((null? L)          '())
        ((null? (cdr L))    L)
        (else (cons (car L) (SplitOdd (cdr (cdr L)))))
  )
)

; 리스트의 짝수 번째 요소를 반환
(define (SplitEven L)
  (cond ((null? L)                '())
        ((null? (cdr L))          '())
        ((null? (cdr (cdr L)))    (list (car (cdr L))))
        (else (cons (car (cdr L)) (SplitEven (cdr (cdr L)))))
  )
)

; 이미 정렬이 된 리스트 L1, L2를 받아서 병합을 수행
(define (Merge L1 L2)
  (cond ((null? L1)            L2)
        ((null? L2)            L1)
        ((< (car L1) (car L2)) (cons (car L1) (Merge (cdr L1) L2)))
        (else                  (cons (car L2) (Merge (cdr L2) L1)))
  )
)

; Devide-and-Conquer를 통해 Merge Sort를 실행
(define (MergeSort L)
  (cond ((null? L) '())
        ((null? (cdr L)) L)
        (else (Merge (MergeSort (SplitOdd L)) (MergeSort (SplitEven L))))
  )
)