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
			(null? lat) (quote ())
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