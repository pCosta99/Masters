/* 
Complete o seguinte modelo de uma colónia de camaleões onde o número de 
camaleões é fixo mas onde a cor de cada camaleão pode mudar de acordo com
as seguintes regras: 
- As cores possíveis são Verde, Azul e Amarelo
- Se 2 camaleões de cores diferentes se encontram mudam ambos para a terceira cor
- As cores só se alteram na situação acima 
*/


sig Camaleao {
	var color : one Color
}

abstract sig Color {}
one sig Verde, Amarelo, Azul extends Color {}

pred nop {
	color' = color
}

pred encontro[x,y : Camaleao] {
	x.color != y.color
	let newc = Color - (x.color + y.color) {
		//color' = color ++ x->newc ++ y->newc
		color' = color - (x->x.color) - (y->y.color) + x->newc + y->newc
	}
}

fact Comportamento {
	always (nop or some x,y : Camaleao | encontro[x,y])
}

// Especifique as seguintes propriedades desta colónia

assert Estabilidade {
	// Se os camaleoes ficarem todos da mesma cor, as cores nunca mais mudam
	always (one Camaleao.color implies always color' = color)
}

check Estabilidade for 5

assert NaoConvergencia {
	// Se inicialmente há um camaleao verde e nenhum azul então não é possível
	// que a colónia fique toda amarela
	one color.Verde and no color.Azul implies always Camaleao.color != Amarelo
}

check NaoConvergencia for 5

// Especifique um cenário onde existe um camaleao que não para de mudar de cor
// tomando recorrentemente todas as cores possíveis

run Exemplo {
	some c: Camaleao { 
		eventually c.color = Verde
		eventually c.color = Amarelo
		eventually c.color = Azul
		always c.color' != c.color
	}
}

run example {} for exactly 6 Camaleao

// Theme related
fun allBlue : set Camaleao {
	{ c : Camaleao | c.color = Azul }
}

fun allGreen : set Camaleao {
	{ c : Camaleao | c.color = Verde }
}

fun allYellow : set Camaleao {
	{ c : Camaleao | c.color = Amarelo }
}
