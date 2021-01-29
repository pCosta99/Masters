package Encomendas;


import java.io.Serializable;
import java.util.Objects;
import java.util.Random;

public class LinhaEncomenda implements Serializable {

    /*
    Variaveis
     */

    private String codProduto;
    private String nome;
    private Double precoUnit;
    private Double quantidade;
    private Double peso;

    /*
    Construtores
     */

    //LinhaEncomenda <CodProduto>, <DescriÃ§Ã£o>, <Quantidade>, <ValorUnitÃ¡rio>
    public LinhaEncomenda(String codProduto, String nome, double precoUnit, double quantidade, double peso) {
        this.codProduto = codProduto;
        this.nome = nome;
        this.precoUnit = precoUnit;
        this.quantidade = quantidade;
        this.peso = peso;
    }

    public LinhaEncomenda(){
        this.codProduto = "";
        this.nome = "";
        this.precoUnit = randomPreco();
        this.quantidade = 0.0;
        this.peso = randomPeso();
    }

    public LinhaEncomenda(LinhaEncomenda a){
        this.codProduto = a.getCodProduto();
        this.nome = a.getNome();
        this.quantidade = a.getQuantidade();
        this.precoUnit = a.getPrecoUnit();
        this.peso = a.getPeso();
    }

    /*
    Gets e Sets
     */

    public String getCodProduto() {
        return codProduto;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }


    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public Double getPrecoUnit() {
        return precoUnit;
    }

    public void setPrecoUnit(Double precoUnit) {
        this.precoUnit = precoUnit;
    }

    public Double getQuantidade() {
        return quantidade;
    }

    public void setQuantidade(Double quantidade) {
        this.quantidade = quantidade;
    }

    public Double getPeso() {
        return peso;
    }

    public void setPeso(Double peso) {
        this.peso = peso;
    }

    /*
    Métodos
     */

    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("Código do Produto: ").append(this.codProduto).append("\n")
                .append("Descrição: ").append(this.nome).append("\n")
                .append("Quantidade: ").append(this.quantidade).append("\n")
                .append("Valor Unitário: ").append(this.precoUnit).append("\n");
        //.append("Tipo de Encomenda: ").append(this.tipo);

        return sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LinhaEncomenda linhaEncomenda = (LinhaEncomenda) o;
        return Objects.equals(getCodProduto(), linhaEncomenda.getCodProduto()) &&
                Objects.equals(getNome(), linhaEncomenda.getNome()) &&
                Objects.equals(getPrecoUnit(), linhaEncomenda.getPrecoUnit());
    }

    public Double randomPeso(){
        double max = 5.00000;
        double min = 0.10000;
        Random rng = new Random();

        return rng.nextDouble()*max+min;
    }

    public Double randomPreco(){
        Random rng = new Random();
        double max = 50.00000;
        double min = 0.99000;
        return rng.nextDouble()*max+min;
    }

    public LinhaEncomenda clone(){
        return new LinhaEncomenda(this);
    }

    public double precoLinha(){
        return this.precoUnit *this.quantidade;
    }

    public double pesoLinha(){
        return this.peso * this.quantidade;
    }

    public String toStringGrava(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getCodProduto()).append(",")
                .append(this.getNome()).append(",")
                .append(this.getQuantidade()).append(",")
                .append(this.getPrecoUnit());
        
        return sb.toString();
    }
}
