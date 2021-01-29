/**
 * A classe Encomenda representa uma encomenda
 * no contexto do enunciado do projecto.
 * @author grupo60
 * @version 1.0
 */
import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;

public class Encomenda implements Serializable{
	
    private String codigoEncomenda;
	private String codigoUtilizador;
	private String codigoLoja;
	private double peso;
	private List<LinhaEncomenda> linhas;

	/**
    * Construtor por omissão para a classe Encomenda.
    */
	public Encomenda() {
		this.codigoEncomenda = "n/a";
		this.codigoUtilizador = "n/a";
		this.codigoLoja = "n/a";
		this.peso = 0;
		this.linhas = new ArrayList<LinhaEncomenda>();
	}

	 /**
     * Construtor parametrizado para a classe Encomenda.
     */
    public Encomenda(String codigoEncomenda, String codigoUtilizador, 
        String codigoLoja, double peso,
        ArrayList<LinhaEncomenda> linhas) {
        this.codigoEncomenda = codigoEncomenda;
        this.codigoUtilizador = codigoUtilizador;
        this.codigoLoja = codigoLoja;
        this.peso = peso;
        this.linhas = new ArrayList<LinhaEncomenda>(linhas);

        
     }

    /**
     * Construtor de cópia de Encomenda.
     * Aceita como parâmetro outra Encomenda e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     * @param e Uma instância da classe Encomenda.
     */
    public Encomenda(Encomenda e) {
        this.codigoEncomenda = e.getCodigoEncomenda();
        this.codigoUtilizador = e.getCodigoUtilizador();
        this.codigoLoja = e.getCodigoLoja();
        this.peso = e.getPeso();
        this.linhas = new ArrayList<LinhaEncomenda>();

        for (LinhaEncomenda l: e.getLinhas()) {
        	this.linhas.add(l.clone());
        }

    }

	/**
     * Devolve o valor do código de encomenda.
     * 
     * @return valor do código de encomenda.
     */
    public String getCodigoEncomenda() {
        return this.codigoEncomenda;
    }
    
    /**
     * Actualiza o valor do código de encomenda.
     * 
     * @param codigoEncomenda novo valor do código de encomenda.
     */
     public void setCodigoEncomenda(String codigoEncomenda) {
        this.codigoEncomenda = codigoEncomenda;
    }

    /**
     * Devolve o valor do código de utilizador.
     * 
     * @return valor do código de utilizador.
     */
    public String getCodigoUtilizador() {
        return this.codigoUtilizador;
    }
    
    /**
     * Actualiza o valor do código de utilizador.
     * 
     * @param codigoUtilizador novo valor do código de utilizador.
     */
     public void setCodigoUtilizador(String codigoUtilizador) {
        this.codigoUtilizador = codigoUtilizador;
    }

    /**
     * Devolve o valor do código de loja.
     * 
     * @return valor do código de loja.
     */
    public String getCodigoLoja() {
        return this.codigoLoja;
    }
    
    /**
     * Actualiza o valor do código de loja.
     * 
     * @param codigoLoja novo valor do código de loja.
     */
     public void setCodigoLoja(String codigoLoja) {
        this.codigoLoja = codigoLoja;
    }

    /**
     * Devolve o valor do peso.
     * 
     * @return valor do peso.
     */
    public double getPeso() {
        return this.peso;
    }
    
    /**
     * Actualiza o valor do peso.
     * 
     * @param peso novo valor do peso.
     */
     public void setPeso(double peso) {
        this.peso = peso;
    }


    /**
     * Devolve o valor de linhas.
     * 
     * @return valor de linhas.
     */
    public List<LinhaEncomenda> getLinhas() {
        
        List<LinhaEncomenda> newLinhas = new ArrayList<LinhaEncomenda>();

        for (LinhaEncomenda e: this.linhas) {
        	newLinhas.add(e.clone());
        }

        return newLinhas;
    }
    
    /**
     * Actualiza o valor de linhas.
     * 
     * @param linha linha de encomenda a inserir.
     */
     public void setLinhas(LinhaEncomenda linha) {
        this.linhas.add(linha.clone());
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

	public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CodigoEncomenda: ").append(this.codigoEncomenda).append("\n")
                .append("CodigoUtilizador: ").append(this.codigoUtilizador).append("\n")
                .append("CodigoLoja: ").append(this.codigoLoja).append("\n")
                .append("Peso: ").append(this.peso).append("\n");

        
   		for (LinhaEncomenda e: this.linhas) {
        	sb.append (e.toString());
        }

        return sb.toString();
    } 

}