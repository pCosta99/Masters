package model;

import interfaces.IEncomenda;
import interfaces.IEntregas;
import interfaces.ILoja;
import interfaces.IUser;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que implementa as entregas
 */
public abstract class Entregas implements IEntregas, Serializable {
    private String code;
    private String nome;
    private final double latitude;
	private double longitude;
	private final double raio;
    private final double velocidade;
    private boolean aceitaMedicamentos;
    private Double rating;
    private boolean livre;
    private Map<String,IEncomenda> listaEnc;

    /**
     * Construtor vazio das entregas
     */
    public Entregas() {
        this.code = "";
        this.nome = "";
        this.latitude = 0.0;
		this.longitude = 0.0;
		this.raio = 0.0;
        this.velocidade = 0.0;
        this.aceitaMedicamentos = false;
        this.rating = 0.0;
        this.livre = true;
		this.listaEnc = new HashMap<>();
    }

    /**
     * Construtor das entregas
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
    public Entregas(String c, String n, double lat, double lon, double raio,
				 	  double v, boolean med, Double rate, boolean livre,
					  Map<String,IEncomenda> encs){
		this.code = c;
        this.nome = n;
        this.latitude = lat;
		this.longitude = lon;
		this.raio = raio;
        this.velocidade = v;
        this.aceitaMedicamentos = med;
        this.rating = rate;
        this.livre = livre;
		setEncomendas(encs);
	}

    /**
     * Construtor das entregas
     * @param v entregas
     */
    public Entregas(Entregas v){
		this.code = v.getCode();
        this.nome = v.getNome();
        this.latitude = v.getLatitude();
		this.longitude = v.getLongitude();
		this.raio = v.getRaio();
        this.velocidade = v.getVelocidade();
        this.aceitaMedicamentos = v.aceitoTransporteMedicamentos();
        this.rating = v.getRating();
        this.livre = v.isLivre();
		setEncomendas(v.getEncomendas());
	}

    /**
     * Devolve o código
     * @return código
     */
    public String getCode() {
		return this.code;
	}

    /**
     * Substitui o código
     * @param code código
     */
	public void setCode(String code) {
		this.code = code;
	}

    /**
     * Devolve o nome
     * @return nome
     */
	public String getNome() {
		return this.nome;
	}

    /**
     * Substitui o nome
     * @param nome nome
     */
	public void setNome(String nome) {
		this.nome = nome;
	}

    /**
     * Devolve a latitude
     * @return latitude
     */
    public double getLatitude() {
		return this.latitude;
	}

    /**
     * Devolve a longitude
     * @return longitude
     */
	public double getLongitude() {
		return this.longitude;
	}

    /**
     * Substitui o valor da longitude
     * @param lon longitude
     */
    public void setLongitude(double lon) {
        this.longitude = lon;
    }

    /**
     * Devolve o raio
     * @return raio
     */
	public double getRaio() {
		return this.raio;
	}

    /**
     * Devolve a velocidade
     * @return velocidade
     */
    public double getVelocidade() {
        return this.velocidade;
    }

    /**
     * Devolve se aceita o transporte de medicamentos
     * @return se aceita o transporte de medicamentos
     */
    public boolean aceitoTransporteMedicamentos() {
        return this.aceitaMedicamentos;
    }

    /**
     * Decide se aceita transportar medicamentos ou não
     */
    public void aceitaMedicamentos(boolean state) {
        this.aceitaMedicamentos = state;
    }

    /**
     * Devolve o rating
     * @return rating
     */
    public Double getRating() {
        return this.rating;
    }

    /**
     * Substitui o valor do rating
     * @param rate rating
     */
    public void setRating(double rate) {
        this.rating += rate / this.listaEnc.size();
    }

    /**
     * Devolve se está livre ou não
     * @return se está livre ou não
     */
    public boolean isLivre() {
		return this.livre;
	}

    /**
     * Decide se está livre ou não
     * @param state estado
     */
	public void setLivre(boolean state) {
		this.livre = state;
	}

    /**
     * Devolve as encomendas
     * @return encomendas
     */
	public Map<String,IEncomenda> getEncomendas() {
		Map<String,IEncomenda> aux = new HashMap<>();

		this.listaEnc.forEach((key, value) -> aux.put(key, value.clone()));

		return aux;
	}

    /**
     * Substitui as encomendas
     * @param le encomendas
     */
	public void setEncomendas(Map<String,IEncomenda> le) {
		this.listaEnc = new HashMap<>();
		le.forEach((key, value) -> this.listaEnc.put(key, value.clone()));
	}

    /**
     * Adiciona uma encomenda
     * @param e encomenda
     */
	public void addEncomenda(IEncomenda e) {
        this.listaEnc.put(e.getCode(),e.clone());
    }

    /**
     * Calcula a distância
     * @param o object
     * @return distância
     */
    public double distancia(Object o) {
        double x1 = 0.0,x2,y1 = 0.0,y2;

        if(o.getClass().getSimpleName().equals("Loja")) {
            ILoja l = (Loja) o;
            x1 = l.getLatitude();
            y1 = l.getLongitude();
        } else if(o.getClass().getSimpleName().equals("User")) {
            IUser u = (User) o;
            x1 = u.getLatitude();
            y1 = u.getLongitude();
        }
        x2 = this.getLatitude();
        y2 = this.getLongitude();

        return Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1));
    }

    /**
     * Vê se está no raio
     * @param o object
     * @return boolean
     */
    public boolean inRaio(Object o) {
        return (distancia(o) <= this.getRaio());
    }

    /**
     * Compara o voluntário/empresa a outro/a pelo código
     * @param e voluntário/empresa
     * @return resultado da comparação
     */
    public int compareTo(IEntregas e) {
        return e.getCode().compareTo(this.code);
    }

}
