import java.io.Serializable;
import java.util.ArrayList;

public class Encomenda implements Serializable {
    private String cod;
    private String user;
    private String loja;
    private double peso;
    private ArrayList<LinhaEncomenda> lin;

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
    }

    public String getLoja() {
        return loja;
    }

    public void setLoja(String loja) {
        this.loja = loja;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    /** Método que adiciona uma linha de encomenda
     *
     * @param a Linha encomenda
     */
    public void adicionaLEnco(LinhaEncomenda a){
        this.lin.add(a);
    }

    public ArrayList<LinhaEncomenda> getLin() {
        ArrayList<LinhaEncomenda> a = new ArrayList<>();
        for (LinhaEncomenda b: this.lin) {
            a.add(b.clone());
        }
        return a;
    }

    public Encomenda(String cod, String user, String loja, double peso) {
        this.cod = cod;
        this.user = user;
        this.loja = loja;
        this.peso = peso;
        this.lin = new ArrayList<>();
    }

    public Encomenda(Encomenda a) {
        this.cod = a.getCod();
        this.user = a.getUser();
        this.loja = a.getLoja();
        this.peso = a.getPeso();
        this.lin = a.getLin();
    }

    public Encomenda clone(){
        return new Encomenda(this);
    }

    /** Método que retorna o custo de uma encomenda*/
    public double custo (){
        int i=0;
        for (LinhaEncomenda a: this.lin){
            i+=a.getPreco();
        }
        return i;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda: ").append(this.cod).append(", ")
                .append(this.user).append(", ")
                .append(this.loja).append(", ")
                .append(this.peso).append(", ")
                .append(this.lin);
        return sb.toString();
    }
}
