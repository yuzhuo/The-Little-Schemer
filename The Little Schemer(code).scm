#lang scheme

(define atom?
  (lambda (x)
    (and (not (pair? x) (not (null? x))))))

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))


(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      (else (cond
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat)
                          (rember a (cdr lat)))))))))


(define firsts
  (lambda (x)
    (cond
      ((null? x) (quote ()))
      (else (cons (car (car x))
                  (firsts (cdr x)))))))


(define insertR
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat))
           (cons old
                 (cons new (cdr lat))))
          (else
           (cons (car lat)
                 (insertR new old (cdr lat)))))))

;(insertR 'a 'b (list 'z 'b 'c 'd))

(define insertL
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat))
           (cons new lat))
          (else
           (cons (car lat)
                 (insertL new old (cdr lat)))))))

;(insertL 'a 'b (list 'z 'b 'c 'd))

(define subst
  (lambda (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat))
           (cons new (cdr lat)))
          (else
           (cons (car lat)
                 (subst new old (cdr lat)))))))

;(subst 'a 'b (list 'z 'b 'c 'd))

(define subst2
  (lambda (new o1 o2 lat)
    (cond ((null? lat) (quote ()))
          ((eq? o1 (car lat))
           (cons new (cdr lat)))
          ((eq? o2 (car lat))
           (cons new (cdr lat)))
          (else
           (cons (car lat)
                 (subst2 new o1 o2 (cdr lat)))))))

;(subst2 'a 'b 'c (list 'z 'b 'c 'd))
;(subst2 'a 'b 'c (list 'z 'c 'b 'd))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? a (car lat)) (multirember a (cdr lat)))
      (else
       (cons (car lat)
             (multirember a (cdr lat)))))))

;(multirember 'a (list 'b 'c 'a 'd 'a))


(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat))
       (cons old
             (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertR new old (cdr lat)))))))

;(multiinsertR 'a 'b (list 'b 'c 'd 'b 'b 'e 'b))                   

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat))
       (cons new
             (cons old (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat)
             (multiinsertL new old (cdr lat)))))))

;(multiinsertL 'a 'b (list 'b 'c 'd 'b 'b 'e 'b))   

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? old (car lat))
       (cons new (multisubst new old (cdr lat))))
      (else
       (cons (car lat)
             (multisubst new old (cdr lat)))))))

;(multisubst 'a 'b (list 'b 'c 'd 'b 'b 'e 'b))

(define add1
  (lambda (n)
    (+ n 1)))

;(add1 68)

(define sub1
  (lambda (n)
    (- n 1)))

;(sub1 68)

;(zero? 0)
;(zero? 1)

(define o+
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else
       (add1 (o+ m (sub1 n)))))))

;(o+ 10 9)

(define o-
  (lambda (m n)
    (cond
      ((zero? n) m)
      (else
       (sub1 (o- m (sub1 n)))))))

;(o- 10 9)

(define addup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else
       (+ (car tup)
          (addup (cdr tup)))))))

;(addup (list 1 2 3 4))

