(set-logic QF_AUFLIA)
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

(declare-const i Int)
(declare-const j Int)

; a) i = j => M[i][j] != 3
(push)
(assert (not (=> (= i j) (not (= 3 (select (select a j) i))))))

(check-sat)
;(get-model)

(echo "a) Obtemos SAT na procura dum contra-exemplo uma vez que não especifamos o scope! Logo, não podemos garantir nada para valores que não pertencem ao intervalo [1,3] para i e j.")
(echo "Um contra-exemplo possível, de acordo com o solver, seria o caso de M[0][0].")

(pop)

; b) forall i,j in [1,3], M[i][j] = M[j][i]
(push)

(assert (and (> i 0) (<= i 3)))
(assert (and (> j 0) (<= j 3)))

(assert (not (= (select (select a j) i) (select (select a i) j))))

(check-sat)
;(get-model)

(echo "b) Obtemos UNSAT quando procuramos por um contra-exemplo pelo que se conclui que a propriedade é válida.")

(pop)

; c) forall i,j in [1,3], i < j => M[i][j] < 6

(push)

(assert (and (> i 0) (<= i 3)))
(assert (and (> j 0) (<= j 3)))

(assert (not (=> (< i j) (< (select (select a j) i) 6))))

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

(assert (not (=> (> x y) (> (select (select a i) x) (select (select a i) y)))))

(check-sat)
;(get-model)

(echo "d) Obtemos UNSAT quando procuramos por um contra-exemplo pelo que se conclui que a propriedade é válida.")

(pop)

; e) forall i,j in [1,3], M[i][j] + M[i+1][j+1] = M[i+1][j] + M[i][j+1]
(push)

(assert (and (> i 0) (<= i 3)))
(assert (and (> j 0) (<= j 3)))

(assert (not (= (+ (select (select a i) j) (select (select a (+ i 1)) (+ j 1))) (+ (select (select a (+ i 1)) j) (select (select a i) (+ j 1))))))

(check-sat)
(get-model)

(echo "e) Obtemos UNSAT quando procuramos por um contra-exemplo pelo que se conclui que a propriedade é válida.")

(pop)
