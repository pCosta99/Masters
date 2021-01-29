
public class Linha_Encomenda implements Linha_EncomendaI{
    // Variaveis de instancia
    private String codProduto;
    private String desc;
    private double qnt;
    private double valor;

    /**
     * Construtores para objetos da classe Linha_Encomenda
     */
    public Linha_Encomenda(){
        this.codProduto = "";
        this.desc = "";
        this.qnt = 0;
        this.valor = 0;
    }

    public Linha_Encomenda(String codeEnc, String desc, double qnt, double valor) {
        this.codProduto = codeEnc;
        this.desc = desc;
        this.qnt = qnt;
        this.valor = valor;
    }

    public Linha_Encomenda(Linha_Encomenda le){
        this.codProduto = le.getCodProduto();
        this.desc = le.getDesc();
        this.qnt = le.getQnt();
        this.valor = le.getValor();

    }

    /**
     * Metodos gets e sets,
     * clone, equals e toString
     */
    public String getCodProduto() {
        return this.codProduto;
    }

    public void setCodProduto(String codProduto) {
        this.codProduto = codProduto;
    }

    public String getDesc() {
        return this.desc;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public double getQnt() {
        return this.qnt;
    }

    public void setQnt(double qnt) {
        this.qnt = qnt;
    }

    public double getValor() {
        return this.valor;
    }

    public void setValor(double valor) {
        this.valor = valor;
    }

    public Linha_Encomenda clone(){
        return new Linha_Encomenda(this);
    }

    public String toString() {
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Produto\n")
                .append("Codigo do produto: ").append(this.codProduto).append("\n")
                .append("Descrição: ").append(this.desc).append("\n")
                .append("Quantidade: ").append(this.qnt).append("\n")
                .append("Valor: ").append(this.valor).append("\n");
        return  sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Linha_Encomenda le = (Linha_Encomenda) o;
        return Double.compare(le.getValor(), this.qnt) == 0 &&
                Double.compare(le.getValor(), this.valor) == 0 &&
                le.getCodProduto().equals(this.codProduto) &&
                le.getDesc().equals(this.desc);
    }
}
