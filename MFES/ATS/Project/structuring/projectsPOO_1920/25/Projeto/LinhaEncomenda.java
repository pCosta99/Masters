
import java.io.Serializable;
public class LinhaEncomenda implements Serializable
{
    //variáveis de instância 
    private String codProduto;
    private String descricao;
    private double precoproduto;
    private double quantidade;
    private int regimposto;
    private int desconto; 
    
    /**
     * Construtor por omissão da classe LinhaEncomenda.
     */
    public LinhaEncomenda(){
        this.codProduto = "";
        this.descricao = ""; 
        this.precoproduto = 0;
        this.quantidade = 0;
        this.regimposto = 0;
        this.desconto = 0;
    }
    
    /**
     * Construtor parametrizado da classe LinhaEncomenda.
     */
    public LinhaEncomenda(String codP,String descP,double precoP,double quantE,int imposto,int descont){
       this.codProduto = codP;
       this.descricao = descP; 
       this.precoproduto = precoP;
       this.quantidade = quantE;
       this.regimposto = imposto;
       this.desconto = descont;
    }
    
    /**
     * Construtor de cópia da classe Lojas.
     */
    public LinhaEncomenda(LinhaEncomenda t){
       this.codProduto = t.getCP();;
       this.descricao = t.getDP(); 
       this.precoproduto = t.getPP();
       this.quantidade = t.getQE();
       this.regimposto = t.getRI();
       this.desconto = t.getDesconto();  
    }
    
    //GETTERS
    public String getCP(){
        return this.codProduto;
    }
    
    public String getDP(){
        return this.descricao;
    }
    
    public double getPP(){
        return this.precoproduto;
    }
    
    public double getQE(){ 
        return this.quantidade;
    }
    
    public int getRI(){
        return this.regimposto;
    }
    
    public int getDesconto(){
        return this.desconto;
    }
    
    //SETTERS
    public void setCP(String refP){
        this.codProduto = refP;
    }
    
    public void setDP(String  descP){
        this.descricao = descP;
    }
    
    public void setPP(double precoP){
        this.precoproduto = precoP;
    }
    
    public void setQE(double quantE){
        this.quantidade = quantE;
    }
    
    public void setRI(int imposto){
        this.regimposto = imposto;
    }
    
    public void setDesconto(int descont){
        this.desconto = descont;
    }
    
    /**
     * Metodo que faz uma copia do objecto receptor da mensagem.
     * Para tal invoca o construtor de copia.
     */
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
    
    /**
     *  Metodo que devolve a representaçao em String da Linha de Encomenda.
     */
    public String toString(){
       StringBuilder sb = new StringBuilder();
       sb.append("\nLinha de Encomenda: \n");
       sb.append("Produto:    " + this.codProduto + "\n");
       sb.append("Descrição:    " + this.descricao + "\n");
       sb.append("Preço Bruto:    " + this.precoproduto + "\n");
       sb.append("Quantidade:    " + this.quantidade + "\n");
       sb.append("Imposto:    " + this.regimposto + "\n");
       sb.append("Desconto:    " + this.desconto + "\n");
       sb.append("\n");
       return sb.toString();
    }
    
    /**
     * Metodo que determina se duas linhas de encomenda sao iguais.
     *
     */
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getCP().equals(this.codProduto) &&
              le.getDP().equals(this.descricao) && 
              le.getPP() == this.precoproduto &&
              le.getQE() == this.quantidade &&
              le.getRI() == this.regimposto &&
              le.getDesconto() == this.desconto;
    }
    
    /**
     * Método que determina o valor da venda já considerados os impostos e os descontos.
     */
    public double calculaValorLinhaEnc(){
        double valor = this.quantidade * this.precoproduto;
        valor -= valor * this.desconto;
        valor *= 1 + this.regimposto;
        return valor;
    }
    
    /**
     * método que determina o valor numérico (em euros) do desconto.
     */
    public double calculaValorDesconto(){
        double valor = this.quantidade * this.precoproduto;
        valor *= this.regimposto;
        return this.calculaValorLinhaEnc() - valor;
    }
}
