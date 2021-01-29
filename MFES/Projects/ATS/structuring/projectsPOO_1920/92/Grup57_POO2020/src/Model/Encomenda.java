package Model;

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;


public class Encomenda implements IEncomenda, Serializable {

	private String codEncomenda;
	private String codUtilizador;
	private String codLoja;
	private double peso;
	private List<LinhaEncomenda> linhasEncomenda;

	public Encomenda() {
		this.codEncomenda = new String();
		this.codUtilizador = new String();
		this.codLoja = new String();
		this.peso = 0.0;
		this.linhasEncomenda = new ArrayList<>();
	}

	public Encomenda(String codE, String codU, String codL, double p, List<LinhaEncomenda> le) {
		setCodE(codE);
		setCodU(codU);
		setCodL(codL);
		setPeso(p);
		setLinhasEncomenda(le);
	}

	public Encomenda(Encomenda l) {
		setCodE(l.getCodE());
		setCodU(l.getCodU());
		setCodL(l.getCodL());
		setPeso(l.getPeso());
		setLinhasEncomenda(l.getLinhasEncomenda());
	}

	public String getCodE() {
		return this.codEncomenda;
	}

	public String getCodU() {
		return this.codUtilizador;
	}

	public String getCodL() {
		return this.codLoja;
	}

	public double getPeso() {
		return this.peso;
	}

	public List<LinhaEncomenda> getLinhasEncomenda() {
		List<LinhaEncomenda> aux = new ArrayList<>();
		for(LinhaEncomenda e : this.linhasEncomenda)
			aux.add(e.clone());
		return aux;
	}

	public void setCodE(String codE) {
		this.codEncomenda = codE;
	}

	public void setCodU(String codU) {
		this.codUtilizador = codU;
	}

	public void setCodL(String codL) {
		this.codLoja = codL;
	}

	public void setPeso(double p) {
		this.peso = p;
	}

	public void setLinhasEncomenda(List<LinhaEncomenda> e) {
		this.linhasEncomenda = new ArrayList<>();
		e.forEach(le -> this.linhasEncomenda.add(le.clone()));
	}

	public Encomenda clone() {
		return new Encomenda(this);
	}

	public boolean equals(Object o) {
		if (o == this) return true;
		if (o == null || o.getClass() != this.getClass()) return false;
		Encomenda t = (Encomenda) o;
		return this.codEncomenda.equals(t.getCodE()) 
			&& this.codUtilizador.equals(t.getCodU())
			&& this.codLoja.equals(t.getCodL())
			&& this.linhasEncomenda.equals(t.getLinhasEncomenda());	
	}

	public String toString() {
		String formattedString = new String();
		StringBuilder sb = new StringBuilder();
		sb.append(codEncomenda).append("\n")
		  .append("Encomendada Por: ").append(codUtilizador).append("\n")
		  .append("Loja Onde Foi Adquirida: ").append(codLoja).append("\n")
		  .append("Peso da Encomenda: ").append(peso).append("\n").append("\n")
		  .append("Conteúdo da Encomenda: ").append("\n").append("\n")
		  .append(linhasEncomenda).append("\n").append("\n");
		formattedString = sb.toString().replace(",","").replace("[","").
				replace("]","");
		return formattedString;
	}

	@Override
	public void adicionaLinhaEncomenda (LinhaEncomenda l) {
		this.linhasEncomenda.add(l.clone());
	}
}