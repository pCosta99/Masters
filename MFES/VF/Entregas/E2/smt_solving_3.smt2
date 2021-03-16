(set-logic QF_AUFLIA)

(declare-const i Int)
(declare-const j Int)

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

; a) i = j => M[i][j] != 3
(push)
(assert (not (=> (= i j) (not (= 3 (select (select m9 j) i))))))

(check-sat)
;(get-model)

(echo "a) Obtemos SAT na procura dum contra-exemplo uma vez que não especifamos o scope! Logo, não podemos garantir nada para valores que não pertencem ao intervalo [1,3] para i e j.")
(echo "Um contra-exemplo possível, de acordo com o solver, seria o caso de M[0][0].")

(pop)

; b) forall i,j in [1,3], M[i][j] = M[j][i]
(push)

(assert (and (> i 0) (<= i 3)))
(assert (and (> j 0) (<= j 3)))

(assert (not (= (select (select m9 j) i) (select (select m9 i) j))))

(check-sat)
;(get-model)

(echo "b) Obtemos UNSAT quando procuramos por um contra-exemplo pelo que se conclui que a propriedade é válida.")

(pop)

; c) forall i,j in [1,3], i < j => M[i][j] < 6

(push)

(assert (and (> i 0) (<= i 3)))
(assert (and (> j 0) (<= j 3)))

(assert (not (=> (< i j) (< (select (select m9 j) i) 6))))

(check-sat)
;(get-model)

(echo "c) Obtemos UNSAT quando procuramos por um contra-exemplo pelo que se conclui que a propriedade é válida.")

(pop)

; d) forall i,x,y in [1,3], x > y => M[i][x] > M[i][y]
(push)
(declare-const x Int)
(declare-const y Int)

(assert (and (> i 0) (<= i 3)))
(assert (and (> x 0) (<= x 3)))
(assert (and (> y 0) (<= y 3)))

(assert (not (=> (> x y) (> (select (select m9 i) x) (select (select m9 i) y)))))

(check-sat)
;(get-model)

(echo "d) Obtemos UNSAT quando procuramos por um contra-exemplo pelo que se conclui que a propriedade é válida.")

(pop)

; e) forall i,j in [1,3], M[i][j] + M[i+1][j+1] = M[i+1][j] + M[i][j+1]
(push)

(assert (and (> i 0) (<= i 3)))
(assert (and (> j 0) (<= j 3)))

(assert (not (= (+ (select (select m9 i) j) (select (select m9 (+ i 1)) (+ j 1))) (+ (select (select m9 (+ i 1)) j) (select (select m9 i) (+ j 1))))))

(check-sat)
;(get-model)

(echo "e) Obtemos SAT quando procuramos por um contra-exemplo pelo que a propriedade é inválida. Um exemplo em que falha é o caso limite i = 3 ou j = 3.")

(pop)
