import java.io.Serializable;
/**
 * clase utilizada para guardar as informaçoes de uma linha de encomenda
 */
public class LinhaEncomenda implements Serializable
{
    private String codProduto;//codigo do produto a encomendado
    private String descricao;//descriçao do produto
    private double quantidade;//quantidade do produto
    private double valorUnitario;// preço do produto por unidade

    public LinhaEncomenda() {

        this.codProduto = "n/a";
        this.descricao = "n/a";
        this.quantidade = 0;
        this.valorUnitario = 0;
    }

    public LinhaEncomenda(String codProduto, String descricao, double quantidade, double valorUnitario) {

        this.codProduto = codProduto;
        this.descricao = descricao;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
    }

    public LinhaEncomenda (LinhaEncomenda l){

        this.codProduto = l.getCodProduto();
        this.descricao = l.getDescricao();
        this.quantidade = l.getQuantidade();
        this.valorUnitario = l.getValorUnitario();

    }

    public String getCodProduto() {
        return this.codProduto;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public String getDescricao() {
        return this.descricao;
    }

    public void setDescricao(String descricao) {
        this.descricao = descricao;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public double getValorUnitario() {
        return this.valorUnitario;
    }

    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }



    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }


    public boolean equals(Object obj) {

        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;

        LinhaEncomenda le = (LinhaEncomenda) obj;

        return this.codProduto.equals(le.getCodProduto()) &&
               this.descricao.equals(le.getDescricao()) && 
               this.quantidade == le.getQuantidade() &&
               this.valorUnitario == le.getValorUnitario();

    }


    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("Codigo de Produto: ").append(this.codProduto + "  ");
        sb.append("Descricao: ").append(this.descricao + "  ");
        sb.append("Quantidade: ").append(this.quantidade + "  ");
        sb.append("Valor Unitario: ").append(this.valorUnitario + "  ");

        return sb.toString();
    }

    /**
     * Usado para imprimir a LinhaEncomenda para um ficheiro CSV
     */
    public String paraCSV (){

        StringBuilder sb = new StringBuilder();

        sb.append(this.codProduto).append(",");
        sb.append(this.descricao).append(",");
        sb.append(this.quantidade).append(",");
        sb.append(this.valorUnitario);

        return sb.toString();
    }



    /**
     * devolve o preço da linha de encomenda
     */
    public double valorTotalLinhaEncomenda (){

        return this.quantidade * this.valorUnitario;

    }

}
