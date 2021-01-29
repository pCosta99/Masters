import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Classe que contem a informaçao relativamente a uma encomenda
 */
public class Encomenda implements Serializable {

	// variàveis de instância
	private String codEncomenda;		  // número da encomenda
	private String codUtilizador;		  // código do utilizador que fez a encomenda
	private String codLoja;				  // Código da Loja que recebeu a encomenda
	private String codEntregador;   	  // Codigo de quem entrega -> Trasnportadora/Voluntario
	private double peso;				  // peso total da encomenda
	private double portes;       		  // portes da encomenda
	private LocalDateTime dataDeSaida;	  // dataDeSaida da encomenda
	private LocalDateTime dataDeChegada;  // dataDeChegada da encomenda
	private double intervalo;	  		  // intervalo entre saida e chegada
	private boolean aceite;				  // Venda tem um entregador associado
	private boolean entregue;			  // Venda foi entregue ou nao
	private boolean classificada;   	  // Encomenda classificada ou nao
	private boolean medica;				  // Encomenda medica ou nao
	private List<Produto> produtos;		  // produtos da encomenda

	/**
   	* Construtores
   	*/
  
  	/**
   	* Omissão
   	*/
   	public Encomenda() {
   		codEncomenda = null;
   		codUtilizador = null;
   		codLoja = null;
   		codEntregador = null;
   		peso = 0.0;
   		portes = 0;
   		dataDeSaida = LocalDateTime.now();
   		dataDeChegada = null;
   		intervalo = 0;
   		aceite = false;
   		entregue = false;
   		classificada = false;
   		medica = false;
   		produtos = new ArrayList<>();
   	}

	/**
	 * Cópia
	 */
	public Encomenda(Encomenda e){
		this.codEncomenda = e.getCodEncomenda();
		this.codUtilizador = e.getCodUtilizador();
		this.codLoja = e.getCodLoja();
		this.codEntregador = e.getCodEntregador();
		this.peso = e.getPeso();
		this.portes = e.getPortes();
		this.dataDeSaida = e.getDataSaida();
		this.dataDeChegada = e.getDataChegada();
		this.intervalo = e.getIntervalo();
		this.aceite = e.getAceite();
		this.entregue = e.getEntregue();
		this.classificada = e.getclassificada();
		this.medica = e.getMedica();
		this.produtos = e.getProdutos();
	}

	/**
   	 * Parametrizado
   	 */
   	public Encomenda(String codEncomenda, String codUtilizador, String codLoja, String codEntregador, double peso, boolean aceite, boolean entregue, boolean classificada, boolean medica, List<Produto> produtos) {
		this.codEncomenda = codEncomenda;
		this.codUtilizador = codUtilizador;
		this.codLoja = codLoja;
		this.codEntregador = codEntregador;
		this.peso = peso;
		this.dataDeSaida = null;
		this.dataDeChegada = null;
		this.intervalo = 0;
		this.aceite = aceite;
		this.entregue = entregue;
		this.classificada = classificada;
		this.medica = medica;
		this.produtos = produtos;
		this.portes = 0;
   	}

	/**
	 * Clone
	 */
	public Encomenda clone(){
		return new Encomenda(this);
	}

	/**
	 * Metodo toString
	 */
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append("Numero Encomenda: ").append(codEncomenda).append(",")
				.append("Codigo Utilizador: ").append(codUtilizador).append(",")
				.append("Codigo Loja: ").append(codLoja).append(",")
				.append("Codigo Entregador: ").append(codEntregador).append(",")
				.append("Data de Saida: ").append(dataDeSaida).append(",")
				.append("Data de Chegada: ").append(dataDeChegada).append(",")
				.append("Intervalo de tempo: ").append(intervalo).append(",")
				.append("Peso: ").append(peso).append(",")
				.append("Preco: ").append(portes).append(",")
				.append("Aceite: ").append(aceite).append(",")
				.append("Entregue: ").append(entregue).append(",")
				.append("Classificada: ").append(classificada).append(",")
				.append("Encomenda medica: ").append(medica).append(",")
				.append("Produtos: ").append(produtos.toString());

		return sb.toString();
	}

	/**
	 * Metodo Equals
	 */
	@Override
	public boolean equals(Object o) {
		if (this == o) return true;
		if (o == null || getClass() != o.getClass()) return false;
		Encomenda encomenda = (Encomenda) o;
		return Double.compare(encomenda.peso, peso) == 0 &&
				Double.compare(encomenda.portes, portes) == 0 &&
				Objects.equals(codEncomenda, encomenda.codEncomenda) &&
				Objects.equals(codUtilizador, encomenda.codUtilizador) &&
				Objects.equals(codLoja, encomenda.codLoja) &&
				Objects.equals(codEntregador, encomenda.codEntregador) &&
				Objects.equals(dataDeSaida, encomenda.dataDeSaida) &&
				Objects.equals(dataDeChegada, encomenda.dataDeChegada) &&
				Double.compare(intervalo, encomenda.intervalo) == 0 &&
				Objects.equals(aceite, encomenda.aceite) &&
				Objects.equals(entregue, encomenda.entregue) &&
				Objects.equals(classificada, encomenda.classificada) &&
				Objects.equals(medica, encomenda.medica) &&
				Objects.equals(produtos, encomenda.produtos);
	}

	/**
	 * Metodo Hashcode
	 */
	@Override
	public int hashCode() {
		return Objects.hash(codEncomenda, codUtilizador, codLoja, codEntregador, peso, dataDeSaida, dataDeChegada, intervalo,  aceite, entregue, classificada, medica, produtos);
	}

	/**
   	 * Gets
   	 */
	public String getCodUtilizador() {
		return codUtilizador;
	}

	public String getCodLoja() {
		return codLoja;
	}

	public String getCodEntregador() {
		return codEntregador;
	}

	public double getPeso() {
		return peso;
	}

	public double getPortes(){
		return portes;
	}

	public LocalDateTime getDataSaida() {
		return dataDeSaida;
	}

	public LocalDateTime getDataChegada(){
		return dataDeChegada;
	}

	public double getIntervalo() {
		return intervalo;
	}

	public String getCodEncomenda() {
		return codEncomenda;
	}

	public boolean getAceite(){
		return aceite;
	}

	public boolean getEntregue(){
		return entregue;
	}

	public boolean getclassificada(){
		return classificada;
	}

	public boolean getMedica(){
		return medica;
	}

	public List<Produto> getProdutos() {
		return produtos;
	}

	/**
   	 * Sets
   	 */
	public void setCodEncomenda(String codEncomenda) {
		this.codEncomenda = codEncomenda;
	}

	public void setCodUtilizador(String codUtilizador) {
		this.codUtilizador = codUtilizador;
	}

	public void setCodLoja(String codLoja) {
		this.codLoja = codLoja;
	}

	public void setCodEntregador(String codEntregador) {
		this.codEntregador = codEntregador;
	}

	public void setPeso(double peso) {
		this.peso = peso;
	}

	public void setPortes(double preco) {
		this.portes = preco;
	}

	public void setDataDeSaida(LocalDateTime dataDeSaida) {
		this.dataDeSaida = dataDeSaida;
	}

	public void setDataDeChegada(LocalDateTime dataDeChegada) {
		this.dataDeChegada = dataDeSaida;
	}

	public void setIntervalo(double intervalo) {
		this.intervalo = intervalo;
	}

	public void setAceite(boolean aceite) {
		this.aceite = aceite;
	}

	public void setEntregue(boolean entregue) {
		this.entregue = entregue;
	}

	public void setClassificada(boolean classificada) {
		this.classificada = classificada;
	}

	public void setMedica(boolean medica){
		this.medica = medica;
	}

	public void setProdutos(List<Produto> ps) {
		this.produtos = ps;
	}
}