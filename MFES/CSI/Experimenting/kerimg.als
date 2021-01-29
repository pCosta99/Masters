
---- Understanding "kernel" and "image"
--- (c) EM/1415

open RelCalc

sig A { R : set B, K: set A}

sig B { I: set B }

fact {
   K = ker[R]
   I = img[R]
}

run {
  some R
  Entire[R,A]
  Simple[R,B]
  not Injective[R,A]
  Surjective[R,B]
} for exactly 3 A, exactly 2 B
