; vars
(declare-const xa1 Int)
(declare-const xb1 Int)
(declare-const xc1 Int)
(declare-const xd1 Int)
(declare-const xa2 Int)
(declare-const xb2 Int)
(declare-const xc2 Int)
(declare-const xd2 Int)
(declare-const xa3 Int)
(declare-const xb3 Int)
(declare-const xc3 Int)
(declare-const xd3 Int)

; all vars are between 0 and 12
(assert (and (> xa1 0) (<= xa1 12)))
(assert (and (> xa2 0) (<= xa2 12)))
(assert (and (> xa3 0) (<= xa3 12)))
(assert (and (> xb1 0) (<= xb1 12)))
(assert (and (> xb2 0) (<= xb2 12)))
(assert (and (> xb3 0) (<= xb3 12)))
(assert (and (> xc1 0) (<= xc1 12)))
(assert (and (> xc2 0) (<= xc2 12)))
(assert (and (> xc3 0) (<= xc3 12)))
(assert (and (> xd1 0) (<= xd1 12)))
(assert (and (> xd2 0) (<= xd2 12)))
(assert (and (> xd3 0) (<= xd3 12)))

; all distinct
(assert (distinct xa1 xa2 xa3 xb1 xb2 xb3 xc1 xc2 xc3 xd1 xd2 xd3))

; initial values
(assert (= xb1 6))
(assert (= xa2 8))
(assert (= xc3 3))

; line conditions
; 1 = 30
(assert (= 30 (+ xa1 (+ xb1 (+ xc1 xd1)))))

; 2 = 18
(assert (= 18 (+ xa2 (+ xb2 (+ xc2 xd2)))))

; 3 = 30
(assert (= 30 (+ xa3 (+ xb3 (+ xc3 xd3)))))

;column conditions

; a = 27
(assert (= 27 (+ xa1 (+ xa2 xa3))))

; b = 16
(assert (= 16 (+ xb1 (+ xb2 xb3))))

; c = 10
(assert (= 10 (+ xc1 (+ xc2 xc3))))

; d = 25
(assert (= 25 (+ xd1 (+ xd2 xd3))))

(check-sat)
(get-model)
