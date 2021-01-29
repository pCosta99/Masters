import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.util.List;
import java.util.AbstractMap.SimpleEntry;
import java.util.ArrayList;
import java.io.Serializable;
import java.util.Iterator;
public class Utilizador extends Entidade implements Serializable{ 
	private Set<RegistoEntregas> historicoEntregas;
	private List<String> encomendasPorAceitar;
	/**
     * Construtor por omissao da classe
     */
	public Utilizador(){
		super();
		this.historicoEntregas = new TreeSet<>();
		this.encomendasPorAceitar = new ArrayList<>();
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String email, String codigo
     */
	public Utilizador(String nome, String email, String codigo){
		super(nome, codigo, new Coordenada(), email,codigo);
		this.historicoEntregas = new TreeSet<>();
		this.historicoEntregas = new TreeSet<>();
		this.encomendasPorAceitar = new ArrayList<>();
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, String email,String pass, Coordenada gps, Set<RegistoEntregas> p
     */ 
	public Utilizador(String nome, String codigo, String email,String pass, Coordenada gps, Set<RegistoEntregas> p){
		super(nome,codigo,gps,email,pass);
		this.historicoEntregas = new TreeSet<>();
		for(RegistoEntregas x : p)
			this.historicoEntregas.add(x.clone());
		this.encomendasPorAceitar = new ArrayList<>();
	}
	/**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
	public Utilizador(Utilizador p){
		super(p);
		this.historicoEntregas = p.getHistoricoEntregas();
		this.encomendasPorAceitar = p.getEncomendasPorAceitar();
	}
	/**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
 	public String toString(){
 		StringBuilder sb = new StringBuilder();
 		sb.append(super.toString()).append("\nRegistos:\n");
 		for(RegistoEntregas p : this.historicoEntregas)
 			sb.append(p).append("\n");
 		return sb.toString();
 	}
 	/**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
 	public boolean equals(Object o){
 		if(o == this)
 			return true;
 		if(!super.equals(o))
 			return false;
 		if(o.getClass() != this.getClass())
 			return false;
 		Utilizador p = (Utilizador) o;
 		return this.historicoEntregas.stream().allMatch(x -> p.getHistoricoEntregas().contains(x)) ;
 	}
 	/**
     * Faz clone da classe
     * @return o clone da classe
     */
	public Utilizador clone(){
		return new Utilizador(this);
	}
	/**
	 * Devolve o Set<RegistoEntregas> historicoEntregas da classe
	 * @return  Set<RegistoEntregas> historicoEntregas
	 */
	public Set<RegistoEntregas> getHistoricoEntregas(){
		TreeSet<RegistoEntregas> n = new TreeSet<>();
		for(RegistoEntregas p : this.historicoEntregas)
			n.add(p.clone());
		return n;
	}
	/**
	 * Devolve o List<RegistoEntregas> historicoEntregas da classe
	 * @return  List<RegistoEntregas> historicoEntregas
	 */
	public List<RegistoEntregas> getRegistos(){
		return this.historicoEntregas.stream().map(RegistoEntregas::clone).collect(Collectors.toList());
	}
	/**
	 * Devolve o List<String> encomendasporaceitar da classe
	 * @return  List<String> encomendasporaceitar
	 */
	public List<String> getEncomendasPorAceitar(){
		return new ArrayList<>(this.encomendasPorAceitar);
	}
	/**
	 * Metodo que valida as credenciais do login do Utilizador
	 * @param String codigo, String email, String pass
	 * @return boolean resultante da validaçao
	 */
	public boolean validaCredenciaisUtilizador(String codigo, String email, String pass){
		return this.getEmail().equals("email");
	}	
	/**
	 * Atualiza o Set<RegistoEntregas> historicoEntregas da classe
	 * @param p novo  historicoEntregas da classe
	 */
	public void setHistoricoEntregas(Set<RegistoEntregas> p){
		this.historicoEntregas = new TreeSet<>();
		for(RegistoEntregas x: p)
			this.historicoEntregas.add(x.clone());
	}
	/**
	 * Atualiza o List<String> encomendasPorAceitar da classe
	 * @param l novo encomendasPorAceitar da classe
	 */
	public void setEncomendasPorAceitar(List<String> l){
		this.encomendasPorAceitar = new ArrayList<>(l);
	}
	/**
	 * Metodo que devolve o numero de entregas que ja foram feitas 
	 * @return int que representa o numero de entregas
	 */
	public int nrEntregas(){
		return this.historicoEntregas.size();
	}
	/**
	 * Metodo que devolve um List<Encomenda> com todas as encomendas feitas pelo Utilizador
	 * @return List<Encomenda> resultante
	 */ 
	public List<Encomenda> getEncomendasFeitas(){
		return this.historicoEntregas.stream().map(RegistoEntregas::getEnc).collect(Collectors.toList());
	}
	/**
	 * Metodo que adiociona à variavel encomendasPorAceitar uma String que representa uma encomenda ainda nao aceite 
	 * @param String p que representa o codigo da encomenda
	 */
	public void adicionaEncomendaPorAceitar(String p){
		this.encomendasPorAceitar.add(p);
	}
	/**
	 * Metodo que remove uma encomenda da variavel da à classe encomendasPorAceitar
	 * @param String que representa o codigo da Encomenda que queremos remover
	 */
	public void removeEncomenda(String enc){
		this.encomendasPorAceitar.remove(enc);
	}
	/**
	 * Metodo que adiciona um RegistoEntregas à variavel historicoEntregas da classe
	 * @param RegistoEntregas p que se pretende adicionar
	 */  
	public void adicionaRegistoEntrega(RegistoEntregas a){
		this.historicoEntregas.add(a.clone());
	}
}