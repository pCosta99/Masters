module RelCalc
/*
 * Authors:
 * 	Miguel Ferreira	<pg10961@alunos.uminho.pt>
 * 	Samuel Silva   	<pg11034@alunos.uminho.pt>
 * Description:
 *		This library was developed in the context of the
 *		FMSE (Formal Methods and Software Engineering)
 *		classes, see eg. www.di.uminho.pt/~jno/html/mfes-0708.html
 * Conventions:
 * 	- All relations begin with capital letters
 * 	- All functions begin with lowercase letters
 */

/***********************************************/

/**Id Definition*/
fun id[S: univ] : univ -> univ {
	(S -> S) & iden
}

/**Kernel Definition*/
fun ker [R : univ -> univ] : univ -> univ {
	R . (~R)
}

/**Image Definition*/
fun img [R : univ -> univ] : univ -> univ {
	(~R) . R
}

/**Domain Definition*/
fun dom [R: univ->univ] : set (R.univ) {
	R.univ
}

/**Domain as coreflexive */
fun delta [R : univ -> univ] : univ -> univ {
	ker[R] & iden
}

/**Range Definition*/
fun rng [R: univ->univ] : set (univ.R) {
	univ.R
}

/**Range as coreflexive */
fun rho [R : univ -> univ] : univ -> univ {
	img[R] & iden
}

/***********************************************/

/**Reflexive Definition*/
pred Reflexive [R : univ -> univ, S: set univ] {
	id[S] in R
--	S <: iden in R
}

/**Correflexive Definition*/
pred Coreflexive [R : univ -> univ, S: set univ] {
	R in id[S]
--	S <: R in iden
}

/**Five Relations*/
pred Symmetric [R : univ -> univ] {
	R in ~R
}
pred Transitive [R : univ -> univ] {
	R.R in R
}
pred Cotransitive [R: univ -> univ] {
	R in R.R
}
pred Antisymmetric [R: univ -> univ, S: univ] {
	R & ~R in id[S]
}

/**Four Relations*/
pred Per [R: univ -> univ] {
	Symmetric[R]
	Transitive[R]
	Cotransitive[R]
}
pred Preorder [R: univ -> univ, S: set univ] {
	Transitive[R]
	Reflexive[R,S]
}
pred Equivalence [R: univ -> univ, S: set univ] {
	Per[R]
	Preorder[R,S]
}
pred Partialorder [R: univ -> univ, S: set univ] {
	Preorder[R,S]
	Antisymmetric[R,S]
}

/**Two Relations*/
pred Id [R: univ -> univ, S: set univ] {
	Coreflexive[R,S]
	Equivalence[R,S]
	Partialorder[R,S]
}
pred Linearorder [R: univ -> univ, S: set univ] {
	Partialorder[R,S]
	Connected[R,S]
}
pred Connected [R : univ -> univ, S: set univ] {
	(R + ~R) = (S -> S)
}

/** Four Relations */
pred Simple [R: univ -> univ, S: set univ] {
--	img[R] in id[S]}
	Coreflexive[img[R],S]}
pred Entire [R: univ -> univ, S: set univ] {
--	id[S] in ker[R]}
	Reflexive[ker[R],S]}
pred Surjective [R: univ -> univ, S: set univ] {
	id[S] in img[R]
--	Reflexive[img[R],S]
}
pred Injective [R: univ -> univ, S: set univ] {
--	ker[R] in id[S]}
	Coreflexive[ker[R],S]}

/** Four Relations */
pred Function [R: univ -> univ, A,B: set univ] {
	Simple[R,B]
	Entire[R,A]
}
pred Representation [R: univ -> univ, A: set univ] {
	Injective[R,A]
	Entire[R,A]
}
pred Abstraction [R: univ -> univ, B: set univ] {
	Simple[R,B]
	Surjective[R,B]
}
pred Bijection [R: univ -> univ, A, B: set univ] {
	Representation[R,A]
	Abstraction[R,B]
}

/***********************************************/

/** More Relations */
pred Assymetric [R: univ -> univ] {
	~R not in R
}
pred Acyclic [R: univ -> univ, S: set univ] {
	no ^R & id[S]
}

/***********************************************/

/** Galois Conections */
/*pred Galois [A,B: univ, f: A->B, g: B -> A] {
	Function[f]
	Function[g]
	f[A] in B <=> A in g[B]
--	f[Dom[f]] in Dom[g] <=> Dom[f] in g[Dom[g]]
--	A in (f.g)[A]
--	(g.f)[B] in B
}
*/
/***********************************************/

/**Utilities*/
fun converse [R: univ -> univ] : univ -> univ { ~R}
--fun dot [f,g: univ -> univ] : univ -> univ {g.f}

fun oplus[R,S: univ -> univ] : univ -> univ
{
--  S + (R - (dom[S] <: R))
    S + (univ - dom[S]) <: R
}

fun flip[R:univ -> univ -> univ]: univ -> univ -> univ {
   { b: univ, a: univ, c: univ | c in b.(a.R) }
}

/***********************************************/

run {}

