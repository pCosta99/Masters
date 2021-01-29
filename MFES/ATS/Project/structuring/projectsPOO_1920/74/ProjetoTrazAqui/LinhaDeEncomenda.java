//<CodProduto>, <DescriÃ§Ã£o>, <Quantidade>, <ValorUnitÃ¡rio>
import java.io.Serializable;
public class LinhaDeEncomenda implements Serializable{
	private String codigo;
	private String descricao;
	private double quantidade;
	private double preco; //preco unitario
	private boolean medic;
	/**
	 * Construtor por omissao da classe
	 */
	public LinhaDeEncomenda(){
		this.codigo = "e0000";
		this.descricao = "";
		this.quantidade = 0;
		this.preco = 0.0;
	}
	/**
	 * Construtor parametrizado da classe
	 * Aceita como parametros String codigo, String descricao, double quantidade e double preco
	 */
	public LinhaDeEncomenda(String codigo, String descricao, double quantidade, double preco){
		this.codigo = codigo;
		this.descricao = descricao;
		this.quantidade = quantidade;
		this.preco = preco;
	}
	/**
	 * Construtor parametrizado da classe
	 * Aceita como parametros String codigo, String descricao, double quantidade, double preco, boolean med
	 */
	public LinhaDeEncomenda(String codigo, String descricao, double quantidade, double preco, boolean med){
		this.codigo = codigo;
		this.descricao = descricao;
		this.quantidade = quantidade;
		this.preco = preco;
		this.medic = med;
	}
	/**
	 * Construtor de copia da classe
	 * Aceita como paramtros um objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */
	public LinhaDeEncomenda(LinhaDeEncomenda umProduto){
		this.codigo = umProduto.getCodigo();
		this.descricao = umProduto.getDescricao();
		this.quantidade = umProduto.getQuantidade();
		this.preco = umProduto.getPreco();
		this.medic = umProduto.getMedic();
	}
	/**
	 * Devolve a String codigo da classe
	 * @return String codigo
	 */
	public String getCodigo(){
		return this.codigo;
	}
	/** 
	 * Devolve o boolean medic da classe
         * @return boolean medic
         */
	public boolean getMedic(){
		return this.medic;
	}
	/**
	 * Atualiza o boolean medic da classe
	 * @param m novo medic da classe
	 */
	public void setMedic(boolean m){
		this.medic = m;
	}
	/**
	 * Devolve o double preco da classe
	 * @return double preco
	 */
	public double getPreco(){
		return this.preco;
	}
	/**
	 * Devolve a String descricao da classe
	 * @return String descricao
	 */
	public String getDescricao(){
		return this.descricao;
	}
	/**
	 * Devolve a double quantidade da classe
	 * @return double quantidade
	 */
	public double getQuantidade(){
		return this.quantidade;
	}
	/**
	 * Atualiza o double preco da classe
	 * @param preco novo preco da classe
	 */
	public void setPreco(double preco){
		this.preco = preco;
	}
	/**
	 * Atualiza a String codigo da classe
	 * @param cod novo codigo da classe
	 */
	public void setCodigo(String cod){
		this.codigo = cod;
	}
	/**
	 * Atualiza a String descricao da classe
	 * @param descricao nova descricao da classe
	 */
	public void setDescricao(String descricao){
		this.descricao = descricao;
	}
	/**
	 * Atualiza o double quantidade da classe
	 * @param quantidade nova quantidade da classe
	 */
	public void setQuantidade(double quantidade){
		this.quantidade = quantidade;
	}
	/**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
	public boolean equals(Object obj){
    	if(obj==this) return true;
    	if(obj== null || obj.getClass() !=this.getClass()) return false;
   		LinhaDeEncomenda p = (LinhaDeEncomenda) obj;
   		return p.getCodigo() == this.codigo;
	}
	/**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append("\nCodigo: ").append(this.codigo)
		  .append("\nDescricao: ").append(this.descricao)
		  .append("\nPreco: ").append(this.preco)
		  .append("\nQuantidade: ").append(this.quantidade);
		return sb.toString();
    }
    /**
     * Faz clone do objecto da classe
     * @return  objecto clonizada
     */
	public LinhaDeEncomenda clone(){
		return new LinhaDeEncomenda(this);
	}

}
