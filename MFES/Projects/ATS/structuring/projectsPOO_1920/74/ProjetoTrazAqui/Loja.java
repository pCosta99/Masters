import java.util.List;
import java.util.ArrayList;
import java.util.Random;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.io.Serializable;

public class Loja extends Entidade implements Serializable {
	private List<String> fila;
	private boolean infoFila;
	private double tempoMedio;
	private int tamFila;
	private Map<String,Produto> produtos;
	/**
	 * Construtor por omissao da classe 
	 */
	public Loja(){
		super();
		this.fila = new ArrayList<>();
		this.infoFila = false;
		Random r = new Random();
		this.tempoMedio = 0;
		this.tamFila = 0;
		this.produtos = new HashMap<>();
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, Coordenada gps, String email, 
     * String pass, List<String> fila, boolean infoFila 
     */ 
	public Loja(String nome, String codigo, Coordenada gps, String email, String pass, List<String> fila, boolean infoFila){
		super(nome,codigo,gps,email,pass);
		this.fila = new ArrayList<>(fila);
		this.infoFila = infoFila;
		Random r = new Random();
		this.tempoMedio = 0;
		this.tamFila = 0;
		this.produtos = new HashMap<>();
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String codigo, String nome, Coordenada gps  
     */
	public Loja(String codigo, String nome, Coordenada gps){
		super(nome,codigo,gps,codigo + "@mail.com",codigo);
		this.fila = new ArrayList<>();
		this.infoFila = false;
		Random r = new Random();
		this.tempoMedio = 0;
		this.tamFila = 0;
		this.produtos = new HashMap<>();
	}
	/**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
	public Loja(Loja l){
		super(l);
		this.fila = l.getFila();
		this.infoFila = l.getInfoFila();
		this.tempoMedio = l.getTempoMedio();
		this.tamFila = l.getTamFila();
		this.produtos = l.getProdutos();
	}
	/**
     * Devolve a List<String> fila da classe
     * @return List<String> fila
     */ 
	public List<String> getFila(){
		return new ArrayList<>(this.fila);
	}
	/**
     * Devolve boolean infoFila da classe
     * @return boolean infoFila
     */ 
	public boolean getInfoFila(){
		return this.infoFila;
	}
	/**
     * Devolve o int tamFila da classe
     * @return int tamFila
     */ 
	public int getTamFila(){
		return this.tamFila;
	}
	/**
     * Devolve o double tempoMedia da classe
     * @return double tempoMedio
     */ 
	public double getTempoMedio(){
		return this.tempoMedio;
	}
	/**
     * Devolve o Map<String,Produto> da classe
     * @return Map<String,Produto> produtos
     */ 
	public Map<String,Produto> getProdutos(){
		return this.produtos.values().stream().map (Produto::clone).collect(Collectors.toMap(v->v.getCodigo(), v->v));
	}
	/**
	 * Atualiza o Map<String,Produto> da classe
	 * @param s novo produtos da classe
	 */
	public void setProdutos(Map<String,Produto> s){
		this.produtos = s.values().stream().map (Produto::clone).collect(Collectors.toMap(v->v.getCodigo(), v->v));
	}
	/**
	 * Atualiza a List<String> da classe
	 * @param novaFila nova fila da classe
	 */
	public void setFila(List<String> novaFila){
		this.fila = new ArrayList<>(novaFila);
	}
	/**
	 * Atualiza o boolean da classe
	 * @param infoFila novo infoFila da classe
	 */
	public void setInfoFila(boolean infoFila){
		this.infoFila = infoFila;
	}
	/**
	 * Atualiza o int da classe
	 * @param tam novo tamFila da classe
	 */
	public void setTamFila(int tam){
		this.tamFila = tam;
	}
	/**
	 * Atualiza o double da classe
	 * @param tempo novo tempoMedio da classe
	 */
	public void setTempoMedio(double tempo){
		this.tempoMedio = tempo;
	}
	/**
	 * Metodo que devolve o tempodeEspera 
	 * @return double 
	 */
	public long tempoDeEspera(){
		Random r = new Random();
		if(!this.infoFila) return (long) r.nextInt(30);
		else return (long)(this.tempoMedio * this.tamFila);
	}
	/**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
	public boolean equals(Object o){
		if(o == this) return true;
		if(!super.equals(o)) return false;
		if(o == null || o.getClass() != this.getClass()) return false;

		Loja l = (Loja) o;

		return this.fila.equals(l.getFila());
	}
	/**
     * Faz clone do objecto da classe
     * @return  objecto clonizada
     */
	public Loja clone(){
		return new Loja(this);
	}
	 /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
	 	sb.append("Fila: ");
		sb.append(this.fila);
		
		sb.append("\n");
   	    return sb.toString();
	}
	/**
	 * Metodo que adiciona uma encomenda à classe
	 * @param Encomenda
	 */
	public void adicionaEncomenda(Encomenda encomenda){
		this.fila.add(encomenda.getCodEncomenda());
	}
	/**
	 * Metodo que adiciona uma encomenda à classe
	 * @param String
	 */
	public void adicionaEncomenda(String encomenda){
		this.fila.add(encomenda);
	}
	/**
	 * Verifica se uma encomenda se encontra na Loja
	 * @param Encomenda 
	 * @return boolean resultante da verificacao
	 */
	public boolean existeEncomenda(Encomenda encomenda){
		return this.fila.contains(encomenda.getCodEncomenda());
	}
	/**
	 * Metodo que remove uma encomenda da Loja
	 * @param String
	 */
	public void processarProximaEnc(String cod){
		this.fila.remove(cod);
	}
	/**
	 * Adiciona um produto à loja
	 * @param Produto
	 */
	public void adicionaProduto(Produto p){
		this.produtos.putIfAbsent(p.getCodigo(), p.clone());
	}
	/**
	 * Metodo que devolve um produto caso este exista na loja
	 * @param String
	 * @return Produto
	 */
	public Produto getProduto(String cod) throws ProdutoInexistenteException{
		Produto p = this.produtos.get(cod);
		if(p == null) throw new ProdutoInexistenteException("Produto não existe na loja!");
		return p.clone();
	}
}
