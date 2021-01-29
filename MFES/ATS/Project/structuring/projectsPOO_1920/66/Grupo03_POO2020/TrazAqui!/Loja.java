import java.io.Serializable;
import java.util.List;
import java.util.TreeMap;

/**
 * Classe com informação de uma loja
 */
public class Loja extends RegistoTULV {
    private double tempoMedioEspera;

    /**
     * Construtor vazio
     */
    public Loja() {
        super();
        this.tempoMedioEspera = 0;
    }

    /**
     * Construtor com argumentos
     * @param c Código
     * @param n Nome
     * @param gps GPS
     * @param pw Password
     * @param e Encomendas
     * @param tempoMedioEspera Tempo média de espera
     * @param mail Email
     */
    public Loja(String c, String n, Ponto gps, String pw, TreeMap<String, EncDistr> e, double tempoMedioEspera, String mail) {
        super(c,n,gps,pw,e,mail);
        this.tempoMedioEspera = tempoMedioEspera;
    }

    /**
     * Construtor que recebe uma loja
     * @param l Loja
     */
    public Loja(Loja l) {
        super(l);
        this.tempoMedioEspera = l.tempoMedioEspera;
    }

    /**
     * Devolve o tempo médio de espera
     * @return Tempo médio de espera
     */
    public double getTempoMedioEspera () { return this.tempoMedioEspera; }

    /**
     * Introduz o tempo médio de espera
     * @param d Tempo médio de espera
     */
    public void setTempoMedioEspera (double d) { this.tempoMedioEspera = d; }

    /**
     * Método clone
     * @return Cópia de um objeto da classe Loja
     */
    public Loja clone () {
        return new Loja(this);
    }

    /**
     * Método equals
     * @param o Loja
     * @return true se os objetos forem iguais ou false caso contrário
     */
    public boolean equals (Object o) {
       if (this == o) return true;
       if (o == null || this.getClass()!= o.getClass()) return false;
       Loja p = (Loja) o;
       return (this.tempoMedioEspera == p.getTempoMedioEspera() && super.equals(p));
    }

    /**
     * Método toString
     * @return String com informações da loja
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("L\n" ).append(super.toString())
                .append("Tempo Médio de Espera: ").append(this.tempoMedioEspera).append("\n");
        return sb.toString();
    }
}

