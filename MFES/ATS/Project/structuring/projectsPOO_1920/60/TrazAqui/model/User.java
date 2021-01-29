package model;

import interfaces.IEncomenda;
import interfaces.IUser;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que implementa os utilizadores
 */
public class User implements IUser, Serializable {
	private final String code;
	private final String nome;
    private final double latitude;
	private final double longitude;
	private Map<String,IEncomenda> encomendas;

	/**
	 * Construtor vazio do utilizador
	 */
	public User(){
		this.code = "";
		this.nome = "";
		this.latitude = 0.0;
		this.longitude = 0.0;
		this.encomendas = new HashMap<>();
	}

	/**
	 * Construtor do utilizador
	 * @param nome nome
	 * @param lat latitude
	 * @param lon longitude
	 * @param encomendas encomendas
	 * @param code código
	 */
	public User(String nome, double lat, double lon, Map<String,IEncomenda> encomendas, String code) {
		this.nome = nome;
		this.latitude = lat;
		this.longitude = lon;
		this.code = code;
		setEncomendas(encomendas);
	}

	/**
	 * Construtor do utilizador
	 * @param u user
	 */
	public User(User u) {
		this.code = u.getCode();
		this.nome = u.getNome();
		this.latitude = u.getLatitude();
		this.longitude = u.getLongitude();
		setEncomendas(u.getEncomendas());
	}

	/**
	 * Devolve o nome
	 * @return nome
	 */
	public String getNome() {
		return this.nome;
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
	 * Devolve o código
	 * @return código
	 */
	public String getCode() {
		return this.code;
	}

	/**
	 * Devolve as encomendas do utilizador
	 * @return encomendas
	 */
    public Map<String,IEncomenda> getEncomendas() {
        Map<String,IEncomenda> aux = new HashMap<>();

        this.encomendas.forEach((key,value) -> aux.put(key,value.clone()));

        return aux;
    }

	/**
	 * Substitui as encomendas
	 * @param t encomendas
	 */
    public void setEncomendas(Map<String,IEncomenda> t) {
        this.encomendas = new HashMap<>();

        t.forEach((key,value) -> this.encomendas.put(key,value.clone()));
    }

	/**
	 * Adiciona uma encomenda ao utilizador
	 * @param e encomenda
	 */
	public void addEncomenda(IEncomenda e){
		this.encomendas.put(e.getCode(),e.clone());
	}

	/**
	 * @return clone de um user
	 */
	public User clone() {
		return new User(this);
	}

	/**
	 * @return representa um utilizador em string
	 */
	public String toString() {
		StringBuilder s = new StringBuilder();

		s.append("Utilizador ").append(this.code);
		s.append(":\n	Nome: ").append(this.nome);
		s.append("\n	GPS = (").append(this.latitude).append(", ").append(this.longitude).append(")");
		//s.append("\n    Encomendas: ").append(this.encomendas);
		s.append("\n");

		return s.toString();
	}

	/**
	 * Compara o user a outro pelo código
	 * @param u utilizador
	 * @return resultado da comparação
	 */
	public int compareTo(IUser u) {
		return u.getCode().compareTo(this.code);
	}
}
