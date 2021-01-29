package model;

import interfaces.ILoja;
import interfaces.IProduto;
import interfaces.IUser;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que implementa as lojas
 */
public class Loja implements ILoja, Serializable {
	private String code;
	private String nome;
	private boolean fila;
	private int fila_size;
	private double latitude;
	private double longitude;
	private Map<String, IProduto> listaProdutos;

	/**
	 * Construtor vazio da loja
	 */
	public Loja(){
		this.code = "";
		this.nome = "";
		this.fila = false;
		this.fila_size = 0;
		this.latitude = 0.0;
		this.longitude = 0.0;
		this.listaProdutos = new HashMap<>();
	}

	/**
	 * Construtor da loja
	 * @param code código
	 * @param nome nome
	 * @param fila tem fila de espera?
	 * @param fila_size tamanho da fila
	 * @param latitude latitude
	 * @param longitude longitude
	 * @param lista lista de produtos
	 */
	public Loja(String code, String nome, boolean fila, int fila_size,
				double latitude, double longitude, Map<String,IProduto> lista){
		this.code = code;
		this.nome = nome;
		this.fila = fila;
		this.fila_size = fila_size;
		this.latitude = latitude;
		this.longitude = longitude;
		setProdutos(lista);
	}

	/**
	 * Construtor da loja
	 * @param l loja
	 */
	public Loja(Loja l){
		this.code = l.getCode();
		this.nome = l.getNome();
		this.fila = l.hasFila();
		this.fila_size = l.getFilaSize();
		this.latitude = l.getLatitude();
		this.longitude = l.getLongitude();
		setProdutos(l.getProdutos());
	}

	/**
	 * Devolve o código
	 * @return código
	 */
	public String getCode(){
		return this.code;
	}

	/**
	 * Devolve o nome
	 * @return nome
	 */
	public String getNome(){
		return this.nome;
	}

	/**
	 * Devolve se tem fila de espera
	 * @return se tem fila de espera
	 */
	public boolean hasFila(){
		return this.fila;
	}

	/**
	 * Devolve o tamanho da fila de espera
	 * @return tamanho da fila de espera
	 */
	public int getFilaSize(){
		return this.fila_size;
	}

	/**
	 * Devolve a latitude
	 * @return latitude
	 */
	public double getLatitude(){
		return this.latitude;
	}

	/**
	 * Devolve a longitude
	 * @return longitude
	 */
	public double getLongitude(){
		return this.longitude;
	}

	/**
	 * Devolve os produtos da loja
	 * @return produtos
	 */
	public Map<String,IProduto> getProdutos(){
		Map<String,IProduto> aux = new HashMap<>();

		this.listaProdutos.forEach((key, value) -> aux.put(key, value.clone()));
        return aux;

	}

	/**
	 * Substitui o código
	 * @param code código
	 */
	public void setCode(String code){
		this.code = code;
	}

	/**
	 * Substitui o nome
	 * @param nome nome
	 */
	public void setNome(String nome){
		this.nome = nome;
	}

	/**
	 * Decide se tem fila de espera ou não
	 * @param fila estado da fila
	 */
	public void setFila(boolean fila){
		this.fila = fila;
	}

	/**
	 * Substitui o valor do tamanho da fila
	 * @param fila_size tamanho da fila
	 */
	public void setFilaSize(int fila_size){
		this.fila_size = fila_size;
	}

	/**
	 * Substitui o valor da latitude
	 * @param latitude latitude
	 */
	public void setLatitude(float latitude){
		this.latitude = latitude;
	}

	/**
	 * Substitui o valor da longitude
	 * @param longitude longitude
	 */
	public void setLongitude(float longitude){
		this.longitude = longitude;
	}

	/**
	 * Substitui os produtos
	 * @param prods produtos
	 */
	public void setProdutos(Map<String,IProduto> prods){
		this.listaProdutos = new HashMap<>();
		prods.forEach((key, value) -> this.listaProdutos.put(key, value.clone()));
	}

	/**
	 * @return clone de uma loja
	 */
	public Loja clone() {
		return new Loja(this);
	}

	/**
	 * @return representa uma loja em string
	 */
	public String toString() {
		StringBuilder s = new StringBuilder();

		s.append("Loja ").append(this.code);
		s.append(" -- ").append(this.nome);
		if(hasFila()) s.append(" -- Tamanho da fila: ").append(getFilaSize());
		//s.append("\n	GPS = (").append(this.latitude).append(", ").append(this.longitude).append(")");
		//s.append("\n Produtos:\n ").append(this.listaProdutos);
		s.append("\n");

		return s.toString();
	}

	/**
	 * Adiciona um novo cliente à fila de espera
	 */
	public void updateFila(){
		int ant = this.fila_size;
		if(this.fila) this.fila_size = ant + 1;
	}

	/**
	 * Retira um cliente da fila de espera
	 */
	public void diminuiFila(){
		int ant = this.fila_size;
		if(this.fila && ant > 0) this.fila_size = ant - 1;
	}

	/**
	 * Calcula a distância
	 * @param l utilizador
	 * @return resultado da comparação
	 */
	public double distancia(IUser l) {

		double x1 = l.getLatitude();
		double y1 = l.getLongitude();
		double x2 = this.getLatitude();
		double y2 = this.getLongitude();

		return Math.sqrt((y2 - y1) * (y2 - y1) + (x2 - x1) * (x2 - x1));
	}

	/**
	 * Calcula o tempo de espera da fila
	 * @return tempo de espera
	 */
	public double tempoEspera() {
		return fila_size * (double) RandomGenerator.generateInt(5,20); //em minutos
	}
}
