(assert t)
(assert (= 0 0))
(assert (= () (not t)))
(assert (not (not t)))
(assert (= (+ 1 2) 3))

; test quote
(assert (= 'a (car '(a b))))
(assert (= '(b) (cdr '(a b))))
(assert (= '(a b c) '(a b c)))
(assert (= (cons 'a 'b) '(a . b)))

; test if
(assert (= 'a (if t 'a 'b)))
(assert (= 'b (if () 'a 'b)))
(assert (= 'a (if t 'a)))
(assert (= () (if () 'a)))

; test setq
(setq a 10)
(assert (= a 10))

; test defun
(defun succ (n) (+ n 1))
(assert (= (succ 1) 2))

(defun fact (n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))
(assert (= (fact 5) 120))

(defun map (proc lst)
  (if lst
      (cons (proc (car lst))
            (map proc (cdr lst)))))
(assert (= '(2 3 4) (map succ '(1 2 3))))

; test lambda
(assert (= '(3 4 5)
           (map (lambda (n) (+ n 2))
                '(1 2 3))))

; test let
(setq a 10)
(let ((a 20))
  (assert (= a 20)))
(assert (= a 10))

(assert (= '(4 5 6)
           (let ((k 3))
             (map (lambda (n) (+ n k))
                  '(1 2 3)))))

; This will fail.
; (setq f (let ((k 3)) (lambda (n) (+ n k))))
; (map f '(1 2 3))

; This will fail, because of funarg problem.
;(assert (= '(4 5 6)
;           (let ((lst 3))
;             (map (lambda (n) (+ n lst))
;                  '(1 2 3)))))

(display "OK\n")
