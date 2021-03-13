(declare-const a (Array Int (Array Int Int)))

(assert (= 2 (select (select a 1) 1)))
(assert (= 3 (select (select a 2) 1)))
(assert (= 4 (select (select a 3) 1)))
(assert (= 3 (select (select a 1) 2)))
(assert (= 4 (select (select a 2) 2)))
(assert (= 5 (select (select a 3) 2)))
(assert (= 4 (select (select a 1) 3)))
(assert (= 5 (select (select a 2) 3)))
(assert (= 6 (select (select a 3) 3)))

(check-sat)
(get-model)
