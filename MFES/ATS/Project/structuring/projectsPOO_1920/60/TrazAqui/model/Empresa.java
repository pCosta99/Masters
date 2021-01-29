package model;

import interfaces.*;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Classe que implementa as empresas
 */
public class Empresa extends Entregas implements IEmpresa, IEntregas, Serializable {

    private final String nif;
	private double taxa;
	private final boolean apenasUmaEnc;
	private final int numEnc;

	/**
	 * Construtor vazio da empresa
	 */
	public Empresa() {
		super();

		this.nif = "";
		this.taxa = 0.0;
        this.apenasUmaEnc = false;
		this.numEnc = 0;

	}

	/**
	 * Construtor da empresa
	 * @param c código
	 * @param n nome
	 * @param nif nif
	 * @param taxa taxa
	 * @param lat latitude
	 * @param lon longitude
	 * @param raio raio
	 * @param v velocidade
	 * @param med transporta medicamentos?
	 * @param rate rating
	 * @param livre está livre?
	 * @param umaEnc transporta apenas uma encomenda?
	 * @param numEnc número de encomendas que está a transportar
	 * @param encs histórico das encomendas
	 */
	public Empresa(String c, String n, String nif, double taxa, double lat, double lon,
				   double raio, double v, boolean med, Double rate, boolean livre,
				   boolean umaEnc, int numEnc, Map<String,IEncomenda> encs) {
		super(c,n,lat,lon,raio,v,med,rate,livre,encs);

        this.nif = nif;
		this.taxa = taxa;
        this.apenasUmaEnc = umaEnc;
		this.numEnc = numEnc;
	}

	/**
	 * Construtor da empresa
	 * @param v empresa
	 */
	public Empresa(Empresa v) {
		super(v.getCode(),v.getNome(),v.getLatitude(),v.getLongitude(),v.getRaio(),
				v.getVelocidade(),v.aceitoTransporteMedicamentos(),v.getRating(),
				v.isLivre(),v.getEncomendas());

        this.nif = v.getNif();
        this.taxa = v.getTaxa();
        this.apenasUmaEnc = v.apenasUmaEncomenda();
		this.numEnc = v.getNumEnc();
	}

	/**
	 * Devolve o nif
	 * @return nif
	 */
	public String getNif(){
		return this.nif;
	}

	/**
	 * Devolve a taxa
	 * @return taxa
	 */
	public double getTaxa() {
		return this.taxa;
	}

	/**
	 * Substitui o valor da taxa
	 * @param tax taxa
	 */
	public void setTaxa(double tax) {
		this.taxa = tax;
	}

	/**
	 * Devolve se transporta apenas uma encomenda
	 * @return se transporta apenas uma encomenda
	 */
	public boolean apenasUmaEncomenda() {
		return this.apenasUmaEnc;
	}

	/**
	 * Devolve o número de encomendas
	 * @return número de encomendas
	 */
	public int getNumEnc() {
		return this.numEnc;
	}

	/**
	 * Determina o custo da entrega da encomenda
	 * @param s sistema
	 * @param e encomenda
	 * @return custo
	 */
    public double custoEntrega(ISistema s, String e) {
		IEncomenda encomenda = s.getEncomendas().get(e);
		IUser u = s.getUtilizadores().get(encomenda.getComprador());
		ILoja l = s.getLojas().get(encomenda.getVendedor());

		double custo = (0.05*encomenda.getPeso() + 0.1*distancia(l) + 0.1*l.distancia(u)) * this.taxa;
		if(l.hasFila()) custo += 0.01 * l.getFilaSize();

		return custo;
    }

	/**
	 * @return clone de uma empresa
	 */
	public Empresa clone() {
		return new Empresa(this);
	}

	/**
	 * @return representa uma empresa em string
	 */
	public String toString(){
		StringBuilder s = new StringBuilder();

		s.append("Transportadora ").append(this.getCode());
		s.append(":\n	Nome: ").append(this.getNome());
		s.append("\n	NIF: ").append(this.nif);
		s.append("\n	GPS = (").append(this.getLatitude()).append(", ").append(this.getLongitude()).append(")");
		s.append("\n	Raio de deslocação: ").append(this.getRaio()).append(" Kms");
		s.append("\n    Velocidade de deslocação: ").append(this.getVelocidade()).append(" kms/h");
		s.append("\n	Preço por km: ").append(this.taxa);
		//s.append("\n    Encomendas: ").append(getEncomendas());
		s.append("\n");

		return s.toString();
	}

	/**
	 * Devolve a faturação da empresa num determinado período de tempo
	 * @param inicio início
	 * @param fim fim
	 * @param s sistema
	 * @return faturação
	 */
	public double faturacao(ISistema s, LocalDateTime inicio, LocalDateTime fim){
		double res = 0;

		for(IEncomenda encomenda : getEncomendas().values())
			if (encomenda.getData().isAfter(inicio) && encomenda.getData().isBefore(fim)) {
				res += custoEntrega(s, encomenda.getCode());
			}
		return res;
	}
}
