var abstract sig Person {
	var parents : set Person,
	var spouse : lone Person
}

var sig Man extends Person {}
var lone sig Adam extends Man {}

var sig Woman extends Person {}
var lone sig Eve extends Woman {}

// Specify some of the expected invariants

assert Acyclic {
	// Nobody can be their own ancestor
	let r = parents.~parents | no ^r & iden
}

assert Spouses {
	// The spouse relationship is reciprocal
	spouse = ~spouse
}

assert Parents {
	// Everyone except Adam and Eve has one female and one male parent
	all p : Person - Adam - Eve | one p.parents  & Man and one p.parents & Woman
}

assert Legal {
	// Nobody can have an ancestors as spouse
//	let r = parents.~parents | no ^r & spouse
	all p : Person | no p.parents & p.spouse
}

assert NoIncest {
	all z : Person | all disj x, y : z.parents |  no (x->y + y ->x) & parents 
	//all x : Person | no (x.parents->x.parents) & parents
}

// Specify the following events

// Reproduction
pred reproduce [m : Man, w : Woman] {
	// Some future person originated from the reproduction of m and w
	some p : Person' | {
		p not in Person
		Person' = Person + p
		parents' = parents + (p->m + p->w)
	}

	// Rest stays equal
	spouse' = spouse
}

// Marriage
pred marry [x : Person, y : Person] {
	// No one can marry him/herself
	x != y

	// They were both single
	no x.spouse
	no y.spouse
	
	// Now they are each other spouse
	spouse' = spouse + x->y + y->x

	// Rest stays equal
	parents' = parents
	Person' = Person
}

pred nop {
	Person' = Person
	parents' = parents
	spouse' = spouse
}

// Specify the initial state

fact init {
	no parents
	no spouse
	no Person - Adam - Eve
}

fact events {
	always (
		nop or
		(some m : Man, w : Woman | reproduce[m,w]) or
		(some x,y : Person | marry[x,y])
	)
}

// Checking the expected invariants

check Acyclic for 10 Person
check Spouses for 10 Person
check Parents for 10 Person
check Legal for 10 Person
check NoIncest for 10 Person

// Specify a scenario where Adam and Eve marry and only afterwards have 
// children and grandchildren
// Configure the theme to color differently man and woman and layout
// the geneology as a tree

run Genesis {
	no parents until (Adam->Eve in spouse)
	eventually some parents
} for 10 Person

// Find a scenario where someone ends up being their own "grandparent"

run OwnGrandparent {
	some p : Person | p->p in ^(parents.~parents)
} for 10 Person

run example {
	Adam + Eve in Person
	eventually some x : Person | no (parents.x->parents.x) & parents
} for 10 Person
