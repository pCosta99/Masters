import java.util.Map;
import java.io.Serializable;
import java.util.HashMap;
public class VoluntarioMedico extends Voluntario implements ITransporteMedico, Serializable{
	private boolean aceitaEncMedica;
	/**
     * Construtor por omissao da classe
     */
	public VoluntarioMedico(){
		super();
		this.aceitaEncMedica = true;
	}

	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String codigo, String nome,String email, String pass, Coordenada gps, double raio,  boolean aceitaEncMedica
     */ 
	public VoluntarioMedico(String codigo, String nome,String email, String pass, Coordenada gps, double raio,  boolean aceitaEncMedica){
		super(codigo,nome,email,pass,gps,raio);
		this.aceitaEncMedica = true;
	}	
	/**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */ 
	public VoluntarioMedico(VoluntarioMedico volMed){
		super(volMed);
		this.aceitaEncMedica = volMed.aceitoTransporteMedicamentos();
	}

	public boolean aceitoTransporteMedicamentos(){
		return this.aceitaEncMedica;
	}
	/**
	 * Devolve o boolean aceitaEncMedica da classe
	 * @return boolean aceitaEncMedica da classe
	 */
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

		VoluntarioMedico v = (VoluntarioMedico) o;

		if(this.aceitaEncMedica == v.aceitoTransporteMedicamentos()) return true;
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
	public VoluntarioMedico clone(){
		return new VoluntarioMedico(this);
	}
}
