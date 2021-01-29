
/**
 * Escreva a descrição da classe LinhaEncomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class LinhaEncomenda
{
    private String codproduto;
    private String desc;
    private double qt;
    private double preco;
    
    /**
     * COnstrutor para objetos da classe user
     */
    public LinhaEncomenda()
    {
        this.codproduto= new String();
        this.desc= new String();
        this.qt=0;
        this.preco=0;
        
    }
    public LinhaEncomenda(LinhaEncomenda le)
    {
        this.codproduto= le.getcod();
        this.desc= le.getdesc();
        this.qt=le.getqt();
        this.preco=le.getpreco();
        
    }
    public LinhaEncomenda(String codproduto,String descricao,double qt,double preco)
    {
        this.codproduto= codproduto;
        this.desc= descricao;
        this.qt=qt;
        this.preco=preco;
        
    }
    public String getcod() { 
      return this.codproduto; 
    }
    public String getdesc() { 
      return this.desc; 
    }
    public double getqt() { 
      return this.qt; 
    }
    public double getpreco() { 
      return this.preco; 
    }
    public LinhaEncomenda clone() { 
      return new LinhaEncomenda(this);
    }
}
