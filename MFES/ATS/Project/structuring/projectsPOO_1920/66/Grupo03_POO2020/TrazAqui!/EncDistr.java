import java.io.Serializable;

/**
 * Classe que associa uma encomenda a uma distribuidora
 */
public class EncDistr implements Serializable {
    private Encomenda encomenda;
    private String distribuidora;

    /**
     * Contrutor vazio
     */
    public EncDistr() {
        this.encomenda = new Encomenda();
        this.distribuidora = "";
    }

    /**
     * Construtor com argumentos
     * @param encomenda Encomenda
     * @param distribuidora Distribuidora
     */
    public EncDistr(Encomenda encomenda, String distribuidora) {
        this.encomenda = encomenda.clone();
        this.distribuidora = distribuidora;
    }

    /**
     * Contrutor com um EncDistr
     * @param e EncDistr
     */
    public EncDistr(EncDistr e) {
        this.encomenda = e.encomenda.clone();
        this.distribuidora = e.distribuidora;
    }

    /**
     * Devolve a Encomenda
     * @return Encomenda
     */
    public Encomenda getEncomenda() {
        return encomenda.clone();
    }

    /**
     * Introduz a Encomenda
     * @param encomenda Encomenda
     */
    public void setEncomenda(Encomenda encomenda) {
        this.encomenda = encomenda.clone();
    }

    /**
     * Devolve o código distribuidora
     * @return String
     */
    public String getDistribuidora() {
        return distribuidora;
    }

    /**
     * Introduz a código distribuidora
     * @param dist Código distribuidora
     */
    public void setDistribuidora(String dist) {
        this.distribuidora = dist;
    }

    /**
     * Método equals
     * @param o Object
     * @return true se as EncDistr forem iguais ou false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EncDistr encDistr = (EncDistr) o;
        return getEncomenda().equals(encDistr.getEncomenda()) && getDistribuidora().equals(encDistr.getDistribuidora());
    }

    /**
     * Método toString
     * @return String com informação relativa a uma EncDistr
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("--------------------------------------------------").append("\n")
                .append(encomenda).append("Distribuidora:").append(distribuidora).append("\n")
                .append("--------------------------------------------------");;
        return sb.toString();
    }

    /**
     * Método clone
     * @return Clone de uma EncDistr
     */
    public EncDistr clone () {
        return new EncDistr(this);
    }
}
