/**
* Classe AuxiliarUtilizadorEncomendas
* serve como classe auxiliar à consulta
* dos 10 utilizadores que mais utilizam
* o sistema.
* @author grupo60
* @version 1.0
*/

import java.util.Comparator;
/**
	* Comparator de AuxiliarEmpresaKms.
	*/
	class AuxiliarUtilizadorEncomendasComparator implements Comparator<AuxiliarUtilizadorEncomendas>{
 
	    @Override
    	public int compare(AuxiliarUtilizadorEncomendas e1, AuxiliarUtilizadorEncomendas e2) {
        	return -1* Double.compare(e1.getNumEncomendas(), e2.getNumEncomendas());
	} 
}

public class AuxiliarUtilizadorEncomendas {

	private String utilizador;
	private double numEncomendas;

	public AuxiliarUtilizadorEncomendas(){

		this.utilizador = "";
		this.numEncomendas = 0;
	}

	public AuxiliarUtilizadorEncomendas(String utilizador, double numEncomendas){
		this.utilizador = utilizador;
		this.numEncomendas = numEncomendas;
	}

	public AuxiliarUtilizadorEncomendas(AuxiliarUtilizadorEncomendas aux){
		this.setUtilizador(aux.getUtilizador());
		this.setNumEncomendas(aux.getNumEncomendas());
	}

	public String getUtilizador(){
		return this.utilizador;
	}

	public double getNumEncomendas(){
		return this.numEncomendas;
	}

	public void setUtilizador(String u){
		this.utilizador = u;
	}

	public void setNumEncomendas(double numEncomendas){
		this.numEncomendas = numEncomendas;
	}

	public String toString(){
		StringBuilder sb = new StringBuilder();

		sb.append(this.getUtilizador()).append("\n");
		sb.append(this.getNumEncomendas()).append("\n");

		return sb.toString();
	}

	public AuxiliarUtilizadorEncomendas clone(){
		return new AuxiliarUtilizadorEncomendas(this);
	}


}