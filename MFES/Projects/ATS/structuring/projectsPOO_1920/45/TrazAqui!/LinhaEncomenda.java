import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    
    /* Variaveis de instância */
    private String codProduto;
    private String descricao;
    private double quantidade;
    private double valorUni;
    
    /* Construtores */
    public LinhaEncomenda(){ 
        codProduto = null;
        descricao = null;
        quantidade = 0.0;
        valorUni = 0.0;
       }
    
    public LinhaEncomenda(LinhaEncomenda l){
        this.codProduto = l.getCodProduto();
        this.descricao = l.getDescricao();
        this.quantidade = l.getQuantidade();
        this.valorUni = l.getvalorUni();
    }
    
    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double valorUni){
        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUni = valorUni;
    }

    /* Métodos de instância */
    
    public String getCodProduto(){
        return codProduto;
    }

    public String getDescricao(){
        return descricao;
    }

    public double getQuantidade(){
        return quantidade;
    }

    public double getvalorUni(){
        return valorUni;
    }

    public void setCodProduto(String codProduto){
        this.codProduto = codProduto;
    }

    public void setDescricao(String descricao){
        this.descricao = descricao;
    }

    public void setQuantidade(double quantidade){
        this.quantidade = quantidade;
    }

    public void setValorUni(double valorUni){
        this.valorUni = valorUni;
    }

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

    public String toString(){
        StringBuilder s = new StringBuilder();
        s.append(codProduto); s.append(",");
        s.append(descricao); s.append(",");
        s.append(quantidade); s.append(",");
        s.append(valorUni);
        return s.toString();
    }

     public boolean equals(Object obj){
        if(this == obj)
            return true;
        else if((obj == null) || (this.getClass() != obj.getClass()))
            return false;
        else{
            LinhaEncomenda ft = (LinhaEncomenda) obj;
            return codProduto == ft.getCodProduto() && descricao == ft.getDescricao() && quantidade == ft.getQuantidade() && valorUni == ft.getvalorUni();
        }
    }
    
    public int hashCode(){
        final int primo = 31;
        int result = 1;
        result = primo * result + ((codProduto == null) ? 0 : codProduto.hashCode());
        result = primo * result + ((descricao == null) ? 0 : descricao.hashCode());
        long aux = Double.doubleToLongBits(quantidade);
        result = primo * result + (int)(aux ^ (aux >>> 32));
        long aux1 = Double.doubleToLongBits(valorUni);
        result = primo * result + (int)(aux1 ^ (aux1 >>> 32));
        return result;
    }
}