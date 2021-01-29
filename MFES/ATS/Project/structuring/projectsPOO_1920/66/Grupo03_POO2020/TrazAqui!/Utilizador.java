import java.util.*;

/**
 * Classe com informações do utilizador
 */
public class Utilizador extends RegistoTULV {
    private List<EncDistr> encsPorAvaliar;

    /**
     * Construtor vazio
     */
    public Utilizador() {
        super();
        this.encsPorAvaliar = new ArrayList<>();
    }

    /**
     * Construtor com argumentos
     * @param cod Código
     * @param nome Nome
     * @param gps GPS
     * @param e Encomendas
     * @param pass Password
     * @param mail Email
     * @param enav Encomendas por avaliar
     */
    public Utilizador(String cod, String nome, Ponto gps, TreeMap<String, EncDistr> e, String pass, String mail, List<EncDistr> enav){
        super(cod,nome,gps,pass,e,mail);
        this.encsPorAvaliar = new ArrayList<>();
        for(EncDistr enc : enav) {
            this.encsPorAvaliar.add(enc.clone());
        }
    }

    /**
     * Construtor que recebe um objeto Utilizador
     * @param u Utilizador
     */
    public Utilizador(Utilizador u) {
        super(u);
        if (u.encsPorAvaliar.size() != 0) {
            for (EncDistr enc : u.encsPorAvaliar) {
                this.adicionaMyEncAv(enc.clone());
            }
        }
        else this.encsPorAvaliar = new ArrayList<>();
    }

    /**
     * Devolve encomendas por avaliar
     * @return Encomendas por avaliar
     */
    public List<EncDistr> getMyEncsAv() {
        ArrayList<EncDistr> res = new ArrayList<>();
        for(EncDistr enc : this.encsPorAvaliar) {
            res.add(enc.clone());
        }
        return res;
    }

    /**
     * Introduz encomendas por avaliar
     * @param u Encomendas por avaliar
     */
    public void setMyEncsAv(List<EncDistr> u) {
        for(EncDistr enc : u) {
            this.encsPorAvaliar.add(enc.clone());
        }
    }

    /**
     * Devolve número de encomendas por avaliar
     * @return Número de encomendas por avaliar
     */
    public int getMyEncsAvSize() {
        return this.encsPorAvaliar.size();
    }

    /**
     * Método equals
     * @param o Utilizador
     * @return true se os objetos forem iguais ou false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Utilizador that = (Utilizador) o;
        return encsPorAvaliar.equals(that.encsPorAvaliar);
    }

    /**
     * Método clone
     * @return Cópia de um objeto da classe Utilizador
     */
    public Utilizador clone (){
        return new Utilizador(this);
    }

    /**
     * Método toString
     * @return String com informações do utilizador
     */
    public String toString() {
        return  "U\n" + super.toString() + "Encomendas por Avaliar: " + this.encsPorAvaliar;
    }

    /**
     * Adicona objeto EncDistr à lista de encomendas por avaliar
     * @param a EncDistr
     */
    public void adicionaMyEncAv (EncDistr a) {
        try {
            this.encsPorAvaliar.add(a.clone());
        } catch (NullPointerException e) {
            this.encsPorAvaliar = new ArrayList<>();
            this.encsPorAvaliar.add(a.clone());
        }
    }


    /**
     * Remove objeto EncDistr da lista de encomendas por avaliar
     * @param a EncDistr
     */
    public void removeMyEncAv (EncDistr a) {
        try {
            this.encsPorAvaliar.remove(a);
        } catch (NullPointerException e) {
            this.encsPorAvaliar = new ArrayList<>();
        }
    }
}
