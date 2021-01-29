import java.io.Serializable;
import java.util.Objects;

public class Produto implements Serializable {

    private String codProduto;
    private String nomeProduto;
    private double quantidade;
    private double valorUnitario;
    private boolean tipoMedico;
    private double peso;


    public Produto(){
        this.codProduto = "";
        this.nomeProduto = "";
        this.quantidade = 0;
        this.valorUnitario = 0;
        this.peso = 0.0;
        this.tipoMedico = false;
    }

    public Produto(String codProduto, String nomeProduto, double quantidade, double valorUnitario, boolean b,double pes){
        this.codProduto = codProduto;
        this.nomeProduto = nomeProduto;
        this.quantidade = quantidade;
        this.valorUnitario = valorUnitario;
        this.tipoMedico = b;
        this.peso = pes;
    }

    public Produto(Produto p){
        this.codProduto = p.getCodProduto();
        this.nomeProduto = p.getNomeProduto();
        this.quantidade = p.getQuantidade();
        this.valorUnitario = p.getValorUnitario();
        this.tipoMedico = p.getTipoMedico();
        this.peso = p.getPeso();
    }
    public double getPeso(){
        return this.peso;
    }

    public void setPeso(double pes){
        this.peso = pes;
    }
    public String getCodProduto() {
        return this.codProduto;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public String getNomeProduto() {
        return this.nomeProduto;
    }

    public void setNomeProduto(String nomeProduto) {
        this.nomeProduto = nomeProduto;
    }

    public double getValorUnitario() {
        return this.valorUnitario;
    }

    public void setValorUnitario(double valorUnitario) {
        this.valorUnitario = valorUnitario;
    }

    public double getQuantidade() {
        return this.quantidade;
    }

    public void setQuantidade(double quantidade) {
        this.quantidade = quantidade;
    }

    public boolean getTipoMedico(){
        return this.tipoMedico;
    }

    public void setTipoMedico(boolean b){
        this.tipoMedico = b;
    }

    public void retiraQuantidade(double quantidade) throws QuantidadeEmExcessoException{
        if(this.quantidade > quantidade)
            this.quantidade -= quantidade;
        else throw new QuantidadeEmExcessoException("Nao é possivel comprar a quatidade desejada");
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Produto that = (Produto) o;
        return  Double.compare(that.valorUnitario, valorUnitario) == 0 &&
                Double.compare(that.quantidade, quantidade) == 0 &&
                Objects.equals(codProduto, that.codProduto) &&
                Objects.equals(nomeProduto, that.nomeProduto)&&
                Objects.equals(that.tipoMedico,tipoMedico);
    }

    @Override
    public int hashCode() {
        return Objects.hash(codProduto, nomeProduto, quantidade, valorUnitario);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nCódigo produto: ").append(this.codProduto);
        sb.append(", Nome produto: ").append(this.nomeProduto);
        sb.append(", Quantidade: ").append(this.quantidade);
        sb.append(", Valor unitário: ").append(this.valorUnitario);
        sb.append(", Tipo Medico: ").append(this.tipoMedico);
        sb.append("PEso: ").append(this.peso);
        return sb.toString();
    }

    public Produto clone(){
        return new Produto(this);
    }

}