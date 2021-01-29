package Model;

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class EncomendasAceites implements IEncomendasAceites, Serializable {

	private List<Aceite> encomendasAceites;

	public EncomendasAceites() {
		this.encomendasAceites = new ArrayList<>();
	}

	public EncomendasAceites(List<Aceite> a) {
		setEncAceites(a);
	}

	public EncomendasAceites(EncomendasAceites ea) {
		setEncAceites(ea.getEncAceites());
	}

	public List<Aceite> getEncAceites() {
		List<Aceite> aux = new ArrayList<>();
		for(Aceite a : this.encomendasAceites)
			aux.add(a.clone());
		return aux;
	}

	public void setEncAceites(List<Aceite> a) {
		this.encomendasAceites = new ArrayList<>();
		a.forEach(as -> this.encomendasAceites.add(as.clone()));
	}

	public EncomendasAceites clone() {
		return new EncomendasAceites(this);
	}

	public boolean equals(Object o) {
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		EncomendasAceites ea = (EncomendasAceites) o;
		return this.encomendasAceites.equals(ea.getEncAceites());
	}

	public String toString() {
		StringBuilder sb = new StringBuilder();
		sb.append("EncomendasAceites: " + this.encomendasAceites + "\n");

		return sb.toString();
	}

	@Override
	public void add(Aceite a) {
		this.encomendasAceites.add(a);
	}
}