import java.io.Serializable;
public abstract class Entidade implements Serializable{
	private String nome;
	private String codigo;
	private Coordenada gps;
	private String email;
	private String password;

	/**
     * Construtor por omissao da classe
     */
	public Entidade(){
		this.nome = "";
		this.codigo = "";
		this.gps = new Coordenada();
		this.email = "";
		this.password = "";
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, Coordenada gps,String email
     */ 
	public Entidade(String nome, String codigo, Coordenada gps,String email){
		this.nome = nome;
		this.codigo = codigo;
		this.gps = gps.clone();
		this.email = email;
		this.password = "";
	}
	/**
     * Construtor parametrizado da classe
     * Aceita como parametros String nome, String codigo, Coordenada gps,String email, String pass
     */ 
	public Entidade(String nome, String codigo, Coordenada gps,String email, String pass){
		this.nome = nome;
		this.codigo = codigo;
		this.gps = gps.clone();
		this.email = email;
		this.password = pass;
	}
	/**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
	public Entidade(Entidade e){
		this.nome = e.getNome();
		this.codigo = e.getCodigo();
		this.gps = e.getGps();
		this.email = e.getEmail();
		this.password = e.getPassword();
	}
	/**
     * Devolve a String nome da classe
     * @return String nome
     */
	public String getNome(){
		return this.nome;
	}
	/**
     * Devolve a String codigo da classe
     * @return String codigo
     */
	public String getCodigo(){
		return this.codigo;
	}
	/**
     * Devolve a String password da classe
     * @return String password
     */
	public String getPassword(){
		return this.password;
	}
	/**
     * Devolve a Coordenada gps da classe
     * @return Coordenada gps
     */
	public Coordenada getGps(){
		return this.gps.clone();
	}
	/**
     * Devolve a String email da classe
     * @return String email
     */
	public String getEmail(){
		return this.email;
	}
	/**
     * Atualiza a String nome da classe
     * @param nomem novo nome da classe
     */
	public void setNome(String nomem){
		this.nome = nomem;
		
	}
	/**
     * Atualiza a String codigo da classe
     * @param codigo novo codigo da classe
     */
	public void setCodigo(String codigo){
		this.codigo = codigo;
	}
	/**
     * Atualiza a Coordenada gps da classe
     * @param gps novo cod da classe
     */
	public void setGps(Coordenada gps){
		this.gps = gps.clone();
	}
	
	/**
     * Atualiza a String email da classe
     * @param email novo email da classe
     */
	public void setEmail(String email){
		this.email = email;
	}
	/**
     * Atualiza a String v da classe
     * @param password nova password da classe
     */
	public void setPassword(String password){
		this.password 	= password;
	}
	/**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
	public boolean equals(Object o){
		if(o == this) return true;
		if(o == null || o.getClass() != this.getClass()) return false;
		Entidade e = (Entidade) o;
		return this.nome.equals(e.getNome()) 
			&& this.codigo.equals(e.getCodigo()) 
			&& this.gps.equals(e.getGps()) 
			&& this.email.equals(e.getEmail())
			&& this.password.equals(e.getPassword());
	}

	public abstract Entidade clone();
	 /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
	public String toString(){
		StringBuilder sb = new StringBuilder();
	 	sb.append("Nome: ").append(this.nome).append("\n")
		  .append("Codigo: ").append(this.codigo).append("\n")
		  .append("GPS: ").append(this.gps.toString())
		  .append("Email: ").append(this.email).append("\n");
   	    return sb.toString();
	}
	/**
	 * Metodo que coloca numa String algumas variaveis de instancia da classe
	 */
	public String info(){
		StringBuilder sb = new StringBuilder();
	 	sb.append("Nome: ").append(this.nome).append("\n")
		  .append("Codigo: ").append(this.codigo).append("\n")
		  .append("Email: ").append(this.email).append("\n");
   	    return sb.toString();		
	}

	public String getNomeCodigo(){
		StringBuilder sb = new StringBuilder();
		sb.append("Nome: ").append(this.getNome()).append(" -> Codigo: ").append(this.getCodigo());
		return sb.toString();
	}
}