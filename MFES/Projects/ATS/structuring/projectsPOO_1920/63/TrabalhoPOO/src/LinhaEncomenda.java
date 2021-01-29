import java.io.Serializable;
import java.util.List;

public class LinhaEncomenda implements Serializable {
    /** variaveis de instancia */
    private String codProduto;
    private String descricao;
    private double precoUnitario;
    private double quantidade;

    /** constructores de classe */
    /** vazio */
    public LinhaEncomenda(){
        this.codProduto = "";
        this.descricao = "";
        this.precoUnitario = 0.0;
        this.quantidade = 0.0;
    }

    /** parametrico */
    public LinhaEncomenda(String newCodProduto, String newDescricao, double newPrecoUnitario,
                          double newQuantidade){
        this.codProduto = newCodProduto;
        this.descricao = newDescricao;
        this.precoUnitario = newPrecoUnitario;
        this.quantidade = newQuantidade;
    }

    /** copia */
    public LinhaEncomenda(LinhaEncomenda newLinhaEncomenda){
        this.codProduto = newLinhaEncomenda.getCodProduto();
        this.descricao = newLinhaEncomenda.getDescricao();
        this.precoUnitario = newLinhaEncomenda.getPrecoUnitario();
        this.quantidade = newLinhaEncomenda.getQuantidade();
    }

    /** gets/sets das variaveis de instancia */
    public String getCodProduto(){ return this.codProduto; }
    public void setCodProduto(String newCodProduto){ this.codProduto = newCodProduto; }

    public String getDescricao(){ return this.descricao; }
    public void setDescricao(String newDescricao){ this.descricao = newDescricao; }

    public double getPrecoUnitario(){ return this.precoUnitario; }
    public void setPrecoUnitario(Double newPrecoUnitario){ this.precoUnitario = newPrecoUnitario; }

    public double getQuantidade(){ return this.quantidade; }
    public void setQuantidade(Double newQuantidade){ this.quantidade = newQuantidade; }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        LinhaEncomenda passed = (LinhaEncomenda) o;
        return (this.codProduto.equals(passed.getCodProduto()) &&
                this.descricao.equals(passed.getDescricao()) &&
                this.precoUnitario == passed.getPrecoUnitario() &&
                this.quantidade == passed.getQuantidade());
    }

    public String toString(){
        return this.codProduto + "," + this.descricao + "," +
               this.precoUnitario + "," + this.quantidade;
    }

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

    /** metodos especificos */

    /**
     * devolve o valor faturado durante o periodo indicado.
     * NOTA: as datas a passar estao no formato (dd/mm/aa)
     */
    //float facturacao(date inicio, date fim){};
}
