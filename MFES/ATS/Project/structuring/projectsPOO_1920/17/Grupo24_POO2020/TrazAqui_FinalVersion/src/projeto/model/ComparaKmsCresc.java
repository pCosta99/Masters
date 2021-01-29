package projeto.model;

import java.io.Serializable;
import java.util.Comparator;

public class ComparaKmsCresc implements Comparator<Empresa>, Serializable{

	
	public int compare(Empresa o1, Empresa o2) {
		if(o1.getKmsPercorridos() < o2.getKmsPercorridos()) return -1;
        if(o1.getKmsPercorridos() == o2.getKmsPercorridos()) return 0;
		return 1;
	}

}
