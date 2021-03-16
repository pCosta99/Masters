(set-logic QF_AUFLIA)

(declare-const i0 Int)
(declare-const i1 Int)
(declare-const i2 Int)

(declare-const j0 Int)
(declare-const j1 Int)
(declare-const j2 Int)

(declare-const m0 (Array Int (Array Int Int)))
(declare-const m1 (Array Int (Array Int Int)))
(declare-const m2 (Array Int (Array Int Int)))
(declare-const m3 (Array Int (Array Int Int)))
(declare-const m4 (Array Int (Array Int Int)))
(declare-const m5 (Array Int (Array Int Int)))
(declare-const m6 (Array Int (Array Int Int)))
(declare-const m7 (Array Int (Array Int Int)))
(declare-const m8 (Array Int (Array Int Int)))
(declare-const m9 (Array Int (Array Int Int)))

(assert (= i0 1))
(assert (= j0 1))
(assert (= m1 (store m0 i0 (store (select m0 i0) j0 (+ i0 j0)))))

(assert (= j1 (+ j0 1)))
(assert (= m2 (store m1 i0 (store (select m1 i0) j1 (+ i0 j1)))))

(assert (= j2 (+ j1 1)))
(assert (= m3 (store m2 i0 (store (select m2 i0) j2 (+ i0 j2)))))

(assert (= i1 (+ i0 1)))
(assert (= m4 (store m3 i1 (store (select m3 i1) j0 (+ i1 j0)))))

(assert (= m5 (store m4 i1 (store (select m4 i1) j1 (+ i1 j1)))))

(assert (= m6 (store m5 i1 (store (select m5 i1) j2 (+ i1 j2)))))

(assert (= i2 (+ i1 1)))
(assert (= m7 (store m6 i2 (store (select m6 i2) j0 (+ i2 j0)))))

(assert (= m8 (store m7 i2 (store (select m7 i2) j1 (+ i2 j1)))))

(assert (= m9 (store m8 i2 (store (select m8 i2) j2 (+ i2 j2)))))

(check-sat)
(get-model)
