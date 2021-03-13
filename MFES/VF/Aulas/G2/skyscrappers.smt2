(set-logic QF_UFLIA)

(declare-fun x11 () Int)
(declare-fun x12 () Int)
(declare-fun x13 () Int)

(declare-fun x21 () Int)
(declare-fun x22 () Int)
(declare-fun x23 () Int)

(declare-fun x31 () Int)
(declare-fun x32 () Int)
(declare-fun x33 () Int)

(define-fun visible ((a1 Int) (a2 Int) (a3 Int)) Int
    (ite (> a1 a2) (ite (> a1 a3) 1 2) (ite (> a2 a3) 2 3))
)

(assert (distinct x11 x12 x13))
(assert (distinct x21 x22 x23))
(assert (distinct x31 x32 x33))

(assert (distinct x11 x21 x31))
(assert (distinct x12 x22 x32))
(assert (distinct x13 x23 x33))

(assert (and (<= 1 x11) (<= x11 3)))
(assert (and (<= 1 x12) (<= x12 3)))
(assert (and (<= 1 x13) (<= x13 3)))
(assert (and (<= 1 x21) (<= x21 3)))
(assert (and (<= 1 x22) (<= x22 3)))
(assert (and (<= 1 x23) (<= x23 3)))
(assert (and (<= 1 x31) (<= x31 3)))
(assert (and (<= 1 x32) (<= x32 3)))
(assert (and (<= 1 x33) (<= x33 3)))

(assert (= 1 (visible x13 x12 x11)))
(assert (= 2 (visible x31 x32 x33)))
(assert (= 3 (visible x33 x23 x13)))

(check-sat)
(get-model)
