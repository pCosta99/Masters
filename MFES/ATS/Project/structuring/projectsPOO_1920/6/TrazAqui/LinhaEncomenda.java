    
/**
 * Classe das Linhas de Encomenda
 * 
 * @author (Jo„o Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.io.Serializable;
public class LinhaEncomenda implements Serializable
{
    // vari·veis de inst‚ncia de uma Linha de Encomenda
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUnitario;
    
    // construtor por omiss„o
    public LinhaEncomenda(){
        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.quantidade = 0.0;
        this.valorUnitario = 0.0;
    }
    
    // construtor parametrizado
    public LinhaEncomenda(String cod, String descr, double qt, double vU){
        this.codProduto = cod;
        this.descricao = descr;
        this.quantidade = qt;
        this.valorUnitario = vU;
    }
    
    // construtor de copia
    public LinhaEncomenda(LinhaEncomenda le){
        this.codProduto = le.getCodP();
        this.descricao = le.getDescr();
        this.quantidade = le.getQt();
        this.valorUnitario = le.getValorU();
    }
    
    // metodo que devolve o codigo de um produto
    public String getCodP(){
        return this.codProduto;
    }
    
    // metodo que devolve a descricao de um produto
    public String getDescr(){
        return this.descricao;
    }
    
    // metodo que devolve a quantidade de um produto
    public double getQt(){
        return this.quantidade;
    }
    
    // metodo que devolve o valor unitario de um produto
    public double getValorU(){
        return this.valorUnitario;
    }
    
    // metodo para definir o codigo de um produto
    public void setCodP(String codP){
        this.codProduto = codP;
    }
    
    // metodo para definir a descricao de um produto
    public void setDescr(String descricao){
        this.descricao = descricao;
    }
    
    // metodo para definir a quantidade de um produto
    public void setQt(double quantidade){
        this.quantidade = quantidade;
    }
    
    // metodo para definir o valor unitario de um produto
    public void setValorU(double valorUnitario){
        this.valorUnitario = valorUnitario;
    }
    
    // metodo que coloca toda a informa√ß√£o sobre uma Linha de Encomenda numa string
    public String toString(){
        StringBuffer sb = new StringBuffer();
        sb.append(this.codProduto+","+this.descricao+","+this.quantidade+","+this.valorUnitario);
        return sb.toString();
    }
    
    // metodo de copia de uma Linha de Encomenda
    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }
    
    // metodo que compara se duas Linhas de Encomenda sao iguais
    public boolean equals(Object o){
        if (o==this) return true;
        if ((o.getClass()!=this.getClass())||o==null) return false;
        LinhaEncomenda le = (LinhaEncomenda) o;
        return this.codProduto.equals(le.getCodP()) &&
               this.descricao.equals(le.getDescr()) &&
               this.quantidade == le.getQt() &&
               this.valorUnitario == le.getValorU();
    }
}
