package model;

import interfaces.IEncomenda;
import interfaces.ILoja;
import interfaces.IProduto;
import interfaces.IUser;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que implementa as encomendas
 */
public class Encomenda implements IEncomenda, Serializable {
	private String code;
	private String comprador;
	private String vendedor;
	private LocalDateTime data;
	private double peso;
	private boolean medicine;
	private boolean jaTransportada;
	private Map<String,IProduto> produtos;

	/**
	 * Construtor da encomenda
	 * @param code código
	 */
	public Encomenda(String code){
		this.code = code;
		this.comprador = "";
		this.vendedor = "";
		this.data = LocalDateTime.now();
		this.peso = 0.0;
		this.medicine = false;
		this.jaTransportada = false;
		this.produtos = new HashMap<>();
	}

	/**
	 * Construtor da encomenda
	 * @param code código
	 * @param vendedor vendedor
	 * @param comprador comprador
	 * @param data data
	 * @param peso peso
	 * @param medicine contêm medicamentos?
	 * @param jaTransportada já foi transportada?
	 * @param produtos lista de produtos
	 */
	public Encomenda(String code, String vendedor, LocalDateTime data, double peso, boolean medicine, Map<String, IProduto> produtos, String comprador, boolean jaTransportada){
		this.code = code;
		this.comprador = comprador;
		this.vendedor = vendedor;
		this.data = data;
		this.peso = peso;
		this.medicine = medicine;
		this.jaTransportada = jaTransportada;
		setProdutos(produtos);
	}

	/**
	 * Construtor das encomendas
	 * @param e encomenda
	 */
	public Encomenda(Encomenda e){
		this.code = e.getCode();
		this.comprador = e.getComprador();
		this.vendedor = e.getVendedor();
		this.data = e.getData();
		this.peso = e.getPeso();
		this.medicine = e.getMedicine();
		this.jaTransportada = e.getJaTransportada();
		setProdutos(e.getProdutos());
	}

	/**
	 * Devolve o código
	 * @return código
	 */
	public String getCode() {
		return this.code;
	}

	/**
	 * Devolve o comprador
	 * @return comprador
	 */
	public String getComprador(){
		return this.comprador;
	}

	/**
	 * Devolve o vendedor
	 * @return vendedor
	 */
	public String getVendedor(){
		return this.vendedor;
	}

	/**
	 * Devolve a data da encomenda
	 * @return data
	 */
	public LocalDateTime getData(){
		return this.data;
	}

	/**
	 * Devolve o peso
	 * @return peso
	 */
	public double getPeso(){
		return this.peso;
	}

	/**
	 * Devolve se contêm medicamentos
	 * @return se contêm medicamentos
	 */
	public boolean getMedicine(){
		return this.medicine;
	}

	/**
	 * Devolve se já foi transportada
	 * @return se já foi transportada
	 */
	public boolean getJaTransportada() {
		return this.jaTransportada;
	}

	/**
	 * Devolve os produtos
	 * @return produtos
	 */
	public Map<String,IProduto> getProdutos(){
		Map<String,IProduto> aux = new HashMap<>();

		this.produtos.forEach((key, value) -> aux.put(key,
				    								  value.clone()));

        return aux;
	}

	/**
	 * Substitui o valor do código
	 * @param code código
	 */
	public void setCode(String code) {
		this.code = code;
	}

	/**
	 * Substitui o comprador
	 * @param comprador comprador
	 */
	public void setComprador(String comprador){
		this.comprador = comprador;
	}

	/**
	 * Substitui o vendedor
	 * @param vendedor vendedor
	 */
	public void setVendedor(String vendedor){
		this.vendedor = vendedor;
	}

	/**
	 * Substitui o valor da data
	 * @param data data
	 */
	public void setData(LocalDateTime data){
		this.data = data;
	}

	/**
	 * Substitui o valor do peso
	 * @param peso peso
	 */
	public void setPeso(double peso){
		this.peso = peso;
	}

	/**
	 * Decide se a encomenda transporta medicamentos ou não
	 * @param medicine decisão
	 */
	public void setMedicine(boolean medicine){
		this.medicine = medicine;
	}

	/**
	 * Decide se a encomenda já foi transportada ou não
	 * @param state estado
	 */
	public void setJaTransportada(boolean state) { this.jaTransportada = state; }

	/**
	 * Substitui os produtos
	 * @param produtos produtos
	 */
	public void setProdutos(Map<String,IProduto> produtos){
		this.produtos = new HashMap<>();
		produtos.forEach((key, value) -> this.produtos.put(key,
													       value.clone()));
	}

	/**
	 * @return clone de uma encomenda
	 */
	public Encomenda clone() {
		return new Encomenda(this);
	}

	/**
	 * @return representa uma encomenda em string
	 */
	public String toString() {
		StringBuilder s = new StringBuilder();

		s.append(this.code);
		s.append(":\n	Comprador: ").append(this.comprador);
		s.append("\n	Vendedor: ").append(this.vendedor);
		//s.append("\n	Data: ").append(this.data);
		s.append("\n	Peso: ").append(this.peso).append(" Kg");
		//s.append("\n	Produtos:\n").append(this.produtos);
		if(jaTransportada) s.append("\n    Encomenda já transportada\n");
		s.append("\n");

		return s.toString();
	}

	/**
	 * Adiciona produto à encomenda
	 * @param loja loja
	 * @param user user
	 * @param p produto
	 */
	public void addProduto(ILoja loja, IUser user, IProduto p) {

		setComprador(user.getCode());
		setVendedor(loja.getCode());
		setData(LocalDateTime.now());
		setPeso(p.getPeso());
		setMedicine(p.getMedicine());
		setJaTransportada(false);

		if(!this.produtos.containsKey(p.getCode()))
			this.produtos.put(p.getCode(),p.clone());

		else
			this.produtos.get(p.getCode()).update(p.getQtd(),p.getValor(),p.getPeso());

	}

}
