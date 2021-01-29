import java.util.ArrayList;
import java.util.Set;
import java.util.List;
import java.util.TreeSet;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.Map;
import java.util.HashMap;
import java.util.TreeMap;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.time.LocalDateTime;
import java.time.Duration;
import java.util.Iterator;
import java.io.Serializable;
public class Transportadora extends Entidade implements ITransporte, Serializable{
	private double velocidade;
	private String nif;
	private double raio;
	private double precoKm;
	private List<String> enc;
	private Map<String,Set<RegistosTransporte>> registos;
	private boolean livre;
	private List<Integer> classificacoes;
	private int capacidade;
	/**
     * Construtor por omissao da classe
     */
	public Transportadora(){
		super();
		Random r = new Random();
		this.velocidade = r.nextInt(50)+30;
		this.nif = "";
		this.raio = 0.0;
		this.precoKm = 0.0;
		this.enc = new ArrayList<>();
		this.registos = new HashMap<>();
		this.livre = true;
		this.capacidade = 5;
		this.classificacoes = new ArrayList<>();
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, Coordenada gps,String email,String pass, double velocidade, String nif, double raio, double precoKm, List<String> enc, boolean certificado, Map<String,Set<RegistosTransporte>> registos
     */ 
	public Transportadora(String nome, String codigo, Coordenada gps,String email,String pass, int capacidade, double velocidade, String nif, double raio, double precoKm, List<String> enc, Map<String,Set<RegistosTransporte>> registos){
		super(nome, codigo, gps, email,pass);
		Random r = new Random();
		this.velocidade = velocidade;
		this.nif = nif;
		this.raio = raio;
		this.precoKm = precoKm;
		this.setEncomendas(enc);
		this.setRegistos(registos);
		this.livre = true; 
		this.capacidade = capacidade;
		this.classificacoes = new ArrayList<>();
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, Coordenada gps,String nif, double raio, double precoKm)
     */ 
	public Transportadora(String nome, String codigo, Coordenada gps,String nif, double raio, double precoKm){
		super(nome, codigo, gps, codigo + "@mail.com", codigo);
		Random r = new Random();
		this.velocidade = r.nextInt(50)+30;
		this.nif = nif;
		this.raio = raio;
		this.precoKm = precoKm;
		this.enc = new ArrayList<>();
		this.registos = new HashMap<>();
		this.livre = true;
		this.capacidade = 5;
		this.classificacoes = new ArrayList<>();
	}
	/**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
	public Transportadora(Transportadora t){
		super(t);
		this.velocidade = t.getVelocidade();
		this.nif = t.getNif();
		this.raio = t.getRaio();
		this.precoKm = t.getPrecoKm();
		this.setEncomendas(t.getEncomendas());
		this.registos = t.getRegistos();
		this.livre = t.getLivre();
		this.capacidade = t.getCapacidade(); 
		this.classificacoes = t.getClassificacoes();
	}
	/**
     * Devolve o double velocidade da classe
     * @return double velocidade
     */
	public double getVelocidade(){
		return this.velocidade;
	}
	/**
     * Devolve a String nif da classe
     * @return String nif
     */
	public String getNif(){
		return this.nif;
	}
	/**
     * Devolve o double raio da classe
     * @return double raio
     */
	public double getRaio(){
		return this.raio;
	}
	/**
     * Devolve o double precoKm da classe
     * @return double precoKm
     */
	public double getPrecoKm(){
		return this.precoKm;
	}
	/**
     * Devolve a List<String> enc da classe
     * @return List<String> enc
     */
	public List<String> getEncomendas(){
		return new ArrayList<>(this.enc);
	}
	/**
     * Devolve o boolean certificado da classe
     * @return boolean certificado
     */
	//public boolean getCertificado(){
	//	return this.certificado;
	//}
	/**
     * Devolve o int capacidade da classe
     * @return int capacidade
     */
	public int getCapacidade(){
		return this.capacidade;
	}
	/**
     * Devolve o Map<String,Set<RegistosTransporte>> registos  da classe
     * @return Map<String,Set<RegistosTransporte>> ret
     */
	public Map<String,Set<RegistosTransporte>> getRegistos(){
		Map<String,Set<RegistosTransporte>> ret = new HashMap<>();
		Set<RegistosTransporte> res = new TreeSet<>();
		for(Map.Entry<String,Set<RegistosTransporte>> r : registos.entrySet()){
			for(RegistosTransporte p: r.getValue())
				res.add(p.clone());
			ret.put(r.getKey(),res);
		}
		return ret;
	}
	/**
     * Devolve o boolean livre da classe
     * @return boolean livre
     */
	public boolean getLivre(){
		return this.livre;
	}
	/**
     * Devolve a String codigo da classe
     * @return String codigo
     */
	public String getCodigo(){
		return super.getCodigo();
	}
	/**
     * Devolve a List<Integer> classificaçoes da classe
     * @return List<Integer> classificaçoes
     */
	public List<Integer> getClassificacoes(){
		return this.classificacoes.stream().collect(Collectors.toList());
	}
	/**
     * Atualiza o double velocidade da classe
     * @param velocidade novo velocidade da classe
     */
	public void setVelocidade(double velocidade){
		this.velocidade = velocidade;
	}
	/**
     * Atualiza a String nif da classe
     * @param nif novo nif da classe
     */
	public void setNif(String nif){
		this.nif = nif;
	}
	/**
     * Atualiza o double raio da classe
     * @param raio novo raio da classe
     */
	public void setRaio(double raio){
		this.raio = raio;
	}
	/**
     * Atualiza o double precoKm da classe
     * @param precoKm novo precoKm da classe
     */
	public void setPrecoKm(double precoKm){
		this.precoKm = precoKm;
	}
	/**
     * Atualiza o List<String> enc da classe
     * @param enc novo enc da classe
     */
	public void setEncomendas(List<String> enc){
		this.enc = new ArrayList<>(enc);
	}
	/**
     * Atualiza o Map<String,Set<RegistosTransporte>> registos da classe
     * @param registos novo registos da classe
     */
	public void setRegistos(Map<String,Set<RegistosTransporte>> registos){
		this.registos = new HashMap<>();
		Set<RegistosTransporte> res = new TreeSet<>();
		for(Map.Entry<String,Set<RegistosTransporte>> r : registos.entrySet()){
			for(RegistosTransporte p: r.getValue())
				res.add(p.clone());
			this.registos.put(r.getKey(),res);
		}
	}
    /**
     * Atualiza o List<Integer> classificacoes da classe
     * @param l novo classificacoes da classe
     */
	public void setClassificacoes(List<Integer> l){
		this.classificacoes = l.stream().collect(Collectors.toList());
	}
	/**
     * Atualiza o boolean livre da classe
     * @param livre novo livre da classe
     */
	public void setLivre(boolean livre){
		this.livre = livre;
	}
	/**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
	public boolean equals(Object o){
		if(o == this) return true;
		if(!super.equals(o)) return false;
		if(o == null || o.getClass() != this.getClass()) return false;
		Transportadora t = (Transportadora) o;
		if(this.velocidade != t.getVelocidade() || !this.nif.equals(t.getNif()) || this.raio != t.getRaio() || this.precoKm != t.getPrecoKm() || !this.enc.equals(t.getEncomendas())) 
			return false;
		return this.registos.equals(t.getRegistos());
	}
	/**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(super.toString())
		  .append("Velocidade: ").append(this.velocidade).append(" ")
		  .append("NIF: ").append(this.nif).append(" ")
		  .append("Raio: ").append(this.raio).append(" ")
		  .append("Preco por Km: ").append(this.precoKm).append(" ")
		  .append("Encomenda: ").append(this.enc).append(" ")
		  .append("Registos: ");
		return sb.toString();
	}
	/**
     * Faz clone da classe
     * @return o clone da classe
     */
	public Transportadora clone(){
		return new Transportadora(this);
	}
	/**
	 * Metodo que calcula o preco de cada Encomenda em funçao da distancia e do peso
	 * @param Encomenda p
	 * @return double custo
	 */
	public double custoDeEncomenda(Encomenda p){
		double distTV = super.getGps().distancia(p.getLocalizacaoLoja());
		double distVC = p.getLocalizacaoUtilizador().distancia(p.getLocalizacaoLoja());
		double distTotal = distVC + distTV;
		double pesoEnc = p.getPeso();
		double custoTotal = distTotal * this.precoKm + pesoEnc/20;
		return custoTotal;
	}
	/**
	 * Metodo que adiciona um RegistosTransporte à estrutura
	 * @param RegistosTransporte r
	 */
	public void adicionaRegistoTransporte(RegistosTransporte r){
		Set<RegistosTransporte> p = this.registos.get(r.getUtilizador());
		if(p != null) 
			p.add(r.clone());
		else{
			Set<RegistosTransporte> ne = new TreeSet<>();
			ne.add(r.clone());
			this.registos.put(r.getUtilizador(), ne);
		}	
	}

	/**
	 * Metodo que calcula o tempo total gasto para fazer uma entrega
	 * @param Encomenda p
	 * @return int calculado
	 */ 
	public long tempoTotal(Encomenda p){
		return (long)(((this.distPercorrida(p))/this.velocidade) * 60);
	}
	/**
	 * Metodo que calcula a distancia pecorrida para fazer uma entrega
	 * @param Encomenda p
	 * @return double calculado
	 */
	public double distPercorrida(Encomenda p){
		double distTV = super.getGps().distancia(p.getLocalizacaoLoja());
		double distVC = p.getLocalizacaoUtilizador().distancia(p.getLocalizacaoLoja());
		double distTotal = distVC + distTV;
		return distTotal;
	}
	/**
	 * Metodo que calculo o Total de kms percorridos pela Transportadora
	 * @return double calculado
	 */
	public double kmPercorridos(){
		return this.registos.values().stream()
									 .flatMap(v -> v.stream())
									 .mapToDouble(RegistosTransporte::getKilometros)
									 .sum();
	}
	/**
	 * Metodo que calculo o Total faturado pela Transportadora
	 * @return double calculado
	 */
	public double totalFaturado(){
		return this.registos.values().stream()
									 .flatMap(v->v.stream())
									 .mapToDouble(RegistosTransporte::getCusto)
									 .sum();
	}
	/**
	 * Metodo que calculo o Total faturado pela Transportadora num dado intervalo de tempo
	 * @return double calculado
	 */
	public double totalFaturado(LocalDateTime t1,LocalDateTime t2){
		return this.registos.values().stream()
									 .flatMap(v->v.stream())
									 .filter(v->v.getHoraEntrega().isAfter(t1) && v.getHoraEntrega().isBefore(t2))
									 .mapToDouble(RegistosTransporte::getCusto)
									 .sum();
	}
	/**
	 * Metodo que devolve a List<RegistosTransporte> com todos os RegistosTransporte num dado intervalo de tempo
	 * @param LocalDateTime t1,LocalDateTime t2
	 * @return List<RegistosTransporte>
	 */ 
	public List<RegistosTransporte> encomendasFeitas(LocalDateTime t1,LocalDateTime t2){
		return this.registos.values().stream()
									 .flatMap(v->v.stream())
									 .filter(v->v.getHoraEntrega().isAfter(t1) && v.getHoraEntrega().isBefore(t2))
									 .map(RegistosTransporte::clone)
									 .collect(Collectors.toList());
	}
	/**
	 * Metodo que devolve a List<RegistosTransporte> com todos os RegistosTransporte 
	 * @return List<RegistosTransporte>
	 */
	public List<RegistosTransporte> encomendasFeitas(){
		return this.registos.values().stream()
									 .flatMap(v->v.stream())
									 .map(RegistosTransporte::clone)
									 .collect(Collectors.toList());
	}
	/**
	 * Metodo que adiciona uma Encomenda à classe
	 * @param Encomenda e
	 */
	public void addEnc(Encomenda e){
		this.enc.add(e.getCodEncomenda());
	}
	/**
	 * Metodo que altera para true o valor do boolean livre da classe
	 */
	public void livre(){
		this.livre = true;
	}
	/**
	 * Metodo que altera para false o valor do boolean livre da classe
	 */
	public void ocupado(){
		this.livre = false;
	}
	/**
	 * Metodo que adiciona uma nota à classe
	 * @param int nota
	 */
	public void adicionaClassificacao(int nota){
		this.classificacoes.add(nota);
		
	}
	/**
	 * Metodo que remove uma encomenda da classe
	 * @param String cod
	 */
	public void removeEncomenda(String cod){
		this.enc.remove(cod);
	}	
	/**
	 * Metodo que adiciona encomenda uma encomenda à classe
	 * @param String e
	 * @return boolean resultante da inserçao
	 */
	public boolean adicionaEncomenda(String e){
		return this.enc.add(e);
	}
	/**
	 * Metodo que cria o RegistosTransporte apos a entrega de uma Encomenda
	 * @param Encomenda e
	 * @return Duration que representa o tempo envolvido no transporte
	 */
	public Duration entregaEncomenda(Encomenda e, long tempoAdicionalLoja){
		long tempo = 0;
		double custo = 0.0, dist = 0.0;
		tempo = this.tempoTotal(e);
        custo = this.custoDeEncomenda(e);
        dist = this.distPercorrida(e);
        LocalDateTime t1 = LocalDateTime.now();
        LocalDateTime t2 = t1.plusMinutes(tempo+tempoAdicionalLoja);
        RegistosTransporte rt = new RegistosTransporte(e.getCodUtilizador(), t1,t2,custo,dist,e);
        this.enc.remove(e.getCodEncomenda());
        this.adicionaRegistoTransporte(rt);
        return Duration.between(t1,t2);
	}
}