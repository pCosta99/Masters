import java.util.Set;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.time.LocalDateTime;
import java.util.List;
import java.util.TreeSet;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.time.Duration;
import java.util.Random;
public class Voluntario extends Entidade implements ITransporte{
	private String encomenda;
	//private boolean certificado;
	private boolean livre;
	private double raio;
	private Map<String,Set<RegistosTransporte>> registos;
	private double velocidade;
	private List<Integer> classificacoes;
	/**
     * Construtor por omissao da classe
     */
	public Voluntario(){
		super();
		this.encomenda = "";
		//this.certificado = false;
		this.livre = true;
		this.registos = new HashMap<>();
		Random r = new Random();
		this.velocidade = r.nextInt(40)+30;
		this.classificacoes = new ArrayList<>();
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String codigo, String nome, Coordenada gps, double raio
     */
	public Voluntario(String codigo, String nome, Coordenada gps, double raio){
		super(nome,codigo,gps,codigo+"@mail.com",codigo);
		this.encomenda = "";
		//this.certificado = false;
		this.livre = true;
		this.raio = raio;
		this.registos = new HashMap<>();
		Random r = new Random();
		this.velocidade = r.nextInt(40)+30;
		this.classificacoes = new ArrayList<>();
	}	
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String codigo, String nome,String email,String pass, Coordenada gps, double raio, boolean cert
     */
	public Voluntario(String codigo, String nome,String email,String pass, Coordenada gps, double raio){
		super(nome,codigo,gps,email,pass);
		this.encomenda = "";
		//this.certificado = cert;
		this.livre = true;
		this.raio = raio;
		this.registos = new HashMap<>();
		Random r = new Random();
		this.velocidade = r.nextInt(40)+30;
		this.classificacoes = new ArrayList<>();
	}	

    /**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, double raio, boolean certificado
     */
	public Voluntario(String nome, String codigo, double raio){
		super(nome,codigo, new Coordenada(), codigo+"@mail.com");
		this.encomenda = "";
		this.raio = raio;
		this.registos = new HashMap<>();
		//this.certificado = false;
		Random r = new Random();
		this.velocidade = r.nextInt(40)+30;
		this.classificacoes = new ArrayList<>();
	}

	/**
     * Construtor de cópia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  

	public Voluntario(Voluntario v){
		super(v);
		this.encomenda = v.getEncomenda();
		this.livre = v.getLivre();
		this.raio = v.getRaio();
		this.velocidade = v.getVelocidade();
		this.classificacoes = v.getClassificacoes();
		this.registos = new HashMap<>();
	}
	/**
	 * Devolve a String encomenda da classe
	 * @return String encomenda
	 */
	public String getEncomenda(){
		return this.encomenda;
	}

	/**
	 * Devolve o boolean livre da classe
	 * @return boolean livre
	 */
	public boolean getLivre(){
		return this.livre;
	}
	/**
	 * Devolve o double raio da classe
	 * @return double raio
	 */
	public double getRaio(){
		return this.raio;
	}
	/**
	 * Devolve o Map<String,Set<RegistosTransporte>> registos
	 * @return Map<String,Set<RegistosTransporte>> p
	 */ 
	public Map<String,Set<RegistosTransporte>> getRegistos(){
		Map<String,Set<RegistosTransporte>> p = new HashMap<>();
		for(Map.Entry<String,Set<RegistosTransporte>> t : this.registos.entrySet()){
			Set<RegistosTransporte> aux = new TreeSet<>();
			for(RegistosTransporte x : t.getValue())
				aux.add(x.clone());
			p.put(t.getKey(),aux);
		}
		return p;
	}
	/**
	 * Devolve o double velocidade da classe
	 * @return double velocidade
	 */
	public double getVelocidade(){
		return this.velocidade;
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
     * Atualiza a Encomenda encomenda da classe
     * @param nenc novo encomenda da classe
     */
	public void setEncomenda(String nenc){
		this.encomenda = nenc;
	}
	/**
     * Devolve o boolean livre da classe
     * @return boolean livre
     */
	public void setLivre(boolean l){
		this.livre = l;
	}
	/**
     * Devolve o double raio da classe
     * @return double raio
     */
	public void setRaio(double raio){
		this.raio = raio;
	}
	/**
     * Atualiza o Map<String,Set<RegistosTransporte>> registos da classe
     * @param registos novo registos da classe
     */
	public void setRegistos(Map<String,Set<RegistosTransporte>> c){
		this.registos = new HashMap<>();
		for(Map.Entry<String,Set<RegistosTransporte>> p : this.registos.entrySet()){
			Set<RegistosTransporte> aux = new TreeSet<>();
			for(RegistosTransporte x : p.getValue())
				aux.add(x.clone());
			this.registos.put(p.getKey(),aux);
		}
	}
	/**
     * Atualiza o double velocidade da classe
     * @param velocidade novo velocidade da classe
     */
	public void setVelocidade(double velocidade){
		this.velocidade = velocidade;
	}
	/**
     * Atualiza o List<Integer> classificacoes da classe
     * @param l novo classificacoes da classe
     */
	public void setClassificacoes(List<Integer> l){
		this.classificacoes = l.stream().collect(Collectors.toList());
	}
	/**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
	public boolean equals(Object o){
		return super.equals(o);
	}
	/**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(super.toString());
		sb.append("Está livre? ").append(this.livre).append("\n");
		sb.append("Encomenda: ").append(this.encomenda).append("\n");
		sb.append("Raio: ").append(this.raio);
		return sb.toString();
	}
	/**
     * Faz clone da classe
     * @return o clone da classe
     */
	public Voluntario clone(){
		return new Voluntario(this);
	}
	/**
	 * Metodo que indica quantos serviços de transporte a Transportadora fez num dado intervalo de tempo
	 * @param LocalDateTime t1,LocalDateTime t2
	 * @return List<RegistosTransporte> resultante
	 */ 
	public List<RegistosTransporte> encomendasFeitas(LocalDateTime t1,LocalDateTime t2){
		return this.registos.values().stream()
									 .flatMap(v->v.stream())
									 .filter(v->v.getHoraEntrega().isAfter(t1) && v.getHoraEntrega().isBefore(t2))
									 .map(RegistosTransporte::clone)
									 .collect(Collectors.toList());
	}
	/**
	 * Metodo que indica quantos serviços de transporte a Transportadora fez 
	 * @return List<RegistosTransporte> resultante
	 */
	public List<RegistosTransporte> encomendasFeitas(){
		return this.registos.values().stream()
									 .flatMap(v->v.stream())
									 .map(RegistosTransporte::clone)
									 .collect(Collectors.toList());
	}
	/**
	 * Metodo que dado um codigo de encomenda o adiciona 
	 * @param String e
	 */
	public void addEnc(String e){
		this.encomenda = e;
	}
	/**
	 * Metodo que coloca o boolean livre a true
	 */
	public void livre(){
		this.livre = true;
	}
	/**
	 * Metodo que coloca o boolean livre a false
	 */
	public void ocupado(){
		this.livre = false;
	}
	/**
	 * Metodo que calcula o tempo total gasto para fazer uma entrega
	 * @param Encomenda p
	 * @return int calculado
	 */ 
	public long tempoTotal(Encomenda p){
		return (long) Math.round((this.distPercorrida(p)/this.velocidade)*60);
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
	 * Metodo que adiciona uma nota à classe
	 * @param int nota
	 */
	public void adicionaClassificacao(int nota){
		this.classificacoes.add(nota);

	}
	/**
	 * Metodo que dado um codigo de encomenda o adiciona 
	 * @param String e
	 */
	public void adicionaEncomenda(String enc){
		this.encomenda = enc;
	}
	/**
	 * Metodo que remove uma encomenda da classe
	 * @param String cod
	 */
	public void removeEncomenda(){
		this.encomenda = "";
	}
	/**
	 * Metodo que cria o RegistosTransporte apos a entrega de uma Encomenda
	 * @param Encomenda e
	 * @return Duration que representa o tempo envolvido no transporte
	 */
	public Duration entregaEncomenda(Encomenda e, long tempoInLoja){
		double custo = 0.0, dist = 0.0;
		long tempo = this.tempoTotal(e);
        dist = this.distPercorrida(e);
        LocalDateTime t1 = LocalDateTime.now();
        LocalDateTime t2 = t1.plusMinutes(tempo+tempoInLoja);
        RegistosTransporte rt = new RegistosTransporte(e.getCodUtilizador(), t1,t2,0,dist,e);
        this.encomenda = "";
        this.adicionaRegistoTransporte(rt);
        return Duration.between(t1,t2);
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
         * Metodo que devolve a capacidade de um Voluntario(Sempre 1)
	 * @return int
         */
	public int getCapacidade(){
		return 1;
	}
	/**
         * Metodo que coloca o codigo da encomenda do Voluntario numa Lista singular
         * @return List<String>
         */
	public List<String> getEncomendas(){
		List<String> ret = new ArrayList<>();
		if(this.encomenda != null && this.encomenda != "") ret.add(this.getEncomenda());
		return ret;
	}
}
