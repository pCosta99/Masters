import java.util.Set;
import java.util.List;
import java.util.Map;
import java.io.Serializable;
public class TransportadoraMedica extends Transportadora implements ITransporteMedico,Serializable{
	private boolean aceitaEncMedica;
	/**
     * Construtor por omissao da classe
     */
	public TransportadoraMedica(){
		super();
		this.aceitaEncMedica = true;
	}
	 /**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, Coordenada gps,String email, String pass,int capacidade,double velocidade, String nif, double raio, double precoKm, List<String> enc,  Map<String,Set<RegistosTransporte>> registos, boolean aceitaEncMedica
     */ 
	public TransportadoraMedica(String nome, String codigo, Coordenada gps,String email, String pass,int capacidade,double velocidade, String nif, double raio, double precoKm, List<String> enc,  Map<String,Set<RegistosTransporte>> registos, boolean aceitaEncMedica){
		super(nome, codigo, gps, email,pass, capacidade,velocidade, nif, raio, precoKm, enc, registos);
		this.aceitaEncMedica = aceitaEncMedica;
	}
	/**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */ 
	public TransportadoraMedica(TransportadoraMedica transMed){
		super(transMed);
		this.aceitaEncMedica = transMed.aceitoTransporteMedicamentos();
	}
	/**
	 * Devolve o boolean aceitaEncMedica da classe
	 * @return boolean aceitaEncMedica da classe
	 */
	public boolean aceitoTransporteMedicamentos(){
		return this.aceitaEncMedica;
	}

	public void aceitaMedicamentos(boolean state){
		this.aceitaEncMedica = state;
	}
	/**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
	public boolean equals(Object o){
		if(o == this) return true;
		if(!super.equals(o)) return false;
		if(o == null || o.getClass() != this.getClass()) return false;

		TransportadoraMedica t = (TransportadoraMedica) o;

		if(this.aceitaEncMedica == t.aceitoTransporteMedicamentos()) return true;
		else return false;
	}
	 /**
     * Metodo que coloca numa String algumas variaveis de instancia da Encomenda
     */ 
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(super.toString()).append("Aceita encomenda medica? ").append(this.aceitaEncMedica).append("\n");
		return sb.toString();
	}
	/**
     * Faz clone da classe
     * @return o clone da classe
     */
	public TransportadoraMedica clone(){
		return new TransportadoraMedica(this);
	}
}