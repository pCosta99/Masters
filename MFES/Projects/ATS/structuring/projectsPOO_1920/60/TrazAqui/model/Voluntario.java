package model;

import interfaces.IEncomenda;
import interfaces.IEntregas;
import interfaces.IVoluntario;

import java.io.Serializable;
import java.util.Map;

/**
 * Classe que implementa os voluntários
 */
public class Voluntario extends Entregas implements IVoluntario, IEntregas, Serializable {

	/**
	 * Construtor vazio do voluntário
	 */
	public Voluntario(){
		super();
	}

	/**
	 * Construtor do voluntário
	 * @param c código
	 * @param n nome
	 * @param lat latitude
	 * @param lon longitude
	 * @param raio raio
	 * @param v velocidade
	 * @param med transporta medicamentos?
	 * @param rate rating
	 * @param livre está livre?
	 * @param encs histórico das empresas
	 */
	public Voluntario(String c, String n, double lat, double lon, double raio,
					  double v, boolean med, Double rate, boolean livre,
					  Map<String, IEncomenda> encs){
		super(c,n,lat,lon,raio,v,med,rate,livre,encs);
	}

	/**
	 * Construtor do voluntário
	 * @param v voluntário
	 */
	public Voluntario(Voluntario v){
		super(v.getCode(),v.getNome(),v.getLatitude(),v.getLongitude(),v.getRaio(),
				v.getVelocidade(),v.aceitoTransporteMedicamentos(),v.getRating(),
				v.isLivre(),v.getEncomendas());
	}

	/**
	 * @return clone de um voluntário
	 */
	public Voluntario clone() {
		return new Voluntario(this);
	}

	/**
	 * @return representa um voluntário em string
	 */
	public String toString() {
		StringBuilder s = new StringBuilder();

		s.append("Voluntário ").append(this.getCode());
		s.append(":\n	Nome: ").append(this.getNome());
		//s.append("\n	GPS = (").append(this.getLatitude()).append(", ").append(this.getLongitude()).append(")");
		s.append("\n	Raio de deslocação: ").append(this.getRaio()).append(" Kms");
		//s.append("\n    Velocidade de deslocação: ").append(this.getVelocidade()).append(" Kms/h");
		s.append("\n    Rating: ").append(this.getRating());
		if (this.aceitoTransporteMedicamentos()) s.append("\n    Aceita transporte de medicamentos");
		s.append("\n    Disponível: ").append(this.isLivre());
		//s.append("\n    Encomendas: ").append(this.getEncomendas());
		s.append("\n");

		return s.toString();
	}

	/**
	 * Substitui o valor do rating
	 * @param rate rating
	 */
	public void setRating(double rate) {
		super.setRating(rate);
	}
}
