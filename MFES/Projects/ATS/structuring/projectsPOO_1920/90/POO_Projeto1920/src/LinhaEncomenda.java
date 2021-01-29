import java.io.Serializable;

public class LinhaEncomenda implements Serializable {
    private String cod;
    private String desc;
    private double qtd;
    private double peso;
    private double valorUnitario;

    /**
     * Construtor por omissao de LinhaEncomenda.
     */
    public LinhaEncomenda() {
        this.cod = "";
        this.desc = "";
        this.peso = 0.0;
        this.qtd = 0.0;
        this.valorUnitario = 0.0;
    }

    /**
     * Construtor parametrizado de LinhaEncomenda.
     */
    public LinhaEncomenda(String ref, String desc, double preco, double qtd) {
        this.cod = ref;
        this.desc = desc;
        this.valorUnitario = preco;
        this.qtd = qtd;
    }

    public LinhaEncomenda(String ref, String desc, double preco, double qtd, double peso) {
        this.cod = ref;
        this.desc = desc;
        this.valorUnitario = preco;
        this.peso = peso;
        this.qtd = qtd;
    }

    /**
     * Construtor de copia de LinhaEncomenda.
     */
    public LinhaEncomenda(LinhaEncomenda le) {
        this.cod = le.getCod();
        this.desc = le.getDesc();
        this.valorUnitario = le.getValorUnitario();
        this.peso = le.getPeso();
        this.qtd = le.getQtd();
    }

    /**
     * Metodos de instancia
     */

    public String getCod() {
        return this.cod;
    }

    public String getDesc() {
        return this.desc;
    }

    public double getValorUnitario() {
        return this.valorUnitario;
    }

    public double getQtd() {
        return this.qtd;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public void setCod(String ref) {
        this.cod = ref;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public void setValorUnitario(double v) {
        this.valorUnitario = v;
    }

    public void setQtd(double q) {
        this.qtd = q;
    }

    public double calculaValorLinhaEnc() {
        return (this.valorUnitario * this.qtd);
    }

    public double calculaPeso() {
        return (this.peso * this.qtd);
    }

    public LinhaEncomenda clone() {
        return new LinhaEncomenda(this);
    }

    public boolean equals(Object obj) {
        if (obj == this) return true;
        if (obj == null || obj.getClass() != this.getClass()) return false;
        LinhaEncomenda le = (LinhaEncomenda) obj;
        return le.getCod().equals(this.cod) &&
                le.getDesc().equals(this.desc) &&
                Double.compare(le.getValorUnitario(), this.valorUnitario) == 0 &&
                Double.compare(le.getPeso(), this.peso) == 0 &&
                Double.compare(le.getQtd(), this.qtd) == 0;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("CodProd: ").append(this.cod).append(" ");
        sb.append("Descricao: ").append(this.desc).append(" ");
        sb.append("Preco: ").append(String.format("%.2f", this.valorUnitario)).append(" ");
        sb.append("Peso: ").append(String.format("%.2f", this.peso)).append(" ");
        sb.append("Quantidade: ").append(this.qtd).append(" ");

        return sb.toString();
    }
}
