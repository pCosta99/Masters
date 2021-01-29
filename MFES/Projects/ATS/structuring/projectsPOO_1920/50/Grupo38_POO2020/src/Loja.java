import java.io.Serializable;
import java.util.concurrent.ThreadLocalRandom;

public class Loja implements Serializable {
    private String cod;
    private String nome;
    private Coordenadas cord;
    private int fila;
    private String email;
    private String pass;

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public Coordenadas getCord() {
        return cord;
    }

    public void setCord(Coordenadas cord) {
        this.cord = cord;
    }

    public int getFila() {
        return fila;
    }

    public void setFila(int fila) {
        this.fila = fila;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPass() {
        return pass;
    }

    public void setPass(String pass) {
        this.pass = pass;
    }

    public Loja(String cod, String nome, Coordenadas cord,int fila, String email, String pass) {
        this.cod = cod;
        this.nome = nome;
        this.cord = cord;
        this.fila = fila;
        this.email = email;
        this.pass = pass;
    }

    public Loja(Loja a){
        this.cod = a.getCod();
        this.nome = a.getNome();
        this.cord = a.getCord();
        this.fila = a.getFila();
        this.email = a.getEmail();
        this.pass = a.getPass();
    }

    public Loja clone(){
        return new Loja(this);
    }

    /** Método que indica se uma loja esta no range
     *
     * @param a Coordenadas
     * @param b Distância
     */
    public boolean rangeLoja (Coordenadas a,double b){
        return this.getCord().isRange(a,b);
    }

    /** Método que cálcula o tempo de fila*/
    public double calcTmp (){
        if (this.fila==-1) return ThreadLocalRandom.current().nextDouble(1, 120);
        else return this.fila*4;
    }
}
