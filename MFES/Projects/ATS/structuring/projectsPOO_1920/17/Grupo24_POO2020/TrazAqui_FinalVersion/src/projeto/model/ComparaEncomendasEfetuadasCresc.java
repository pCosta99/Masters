package projeto.model;

import java.io.Serializable;
import java.util.Comparator;

public class ComparaEncomendasEfetuadasCresc implements Comparator<Utilizador>, Serializable{

	@Override
	public int compare(Utilizador o1, Utilizador o2) {
		if(o1.getNumEncomendasEfetuadas() < o2.getNumEncomendasEfetuadas()) return -1;
        if(o1.getNumEncomendasEfetuadas() == o2.getNumEncomendasEfetuadas()) return 0;
        return 1;
	}

	
	

}
