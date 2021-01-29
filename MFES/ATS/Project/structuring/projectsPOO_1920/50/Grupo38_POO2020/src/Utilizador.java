import java.io.Serializable;

public class Utilizador implements Serializable {
    private String cod;
    private String nome;
    private String pass;
    private String email;
    private Coordenadas cord;
    private double range;

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
        return cord.clone();
    }

    public void setCord(Coordenadas cord) {
        this.cord = cord.clone();
    }

    public String getPass() {
        return pass;
    }

    public void setPass(String pass) {
        this.pass = pass;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public Utilizador(String cod, String nome, Coordenadas cord, String pass, String email) {
        this.cod = cod;
        this.nome = nome;
        this.cord = cord;
        this.pass = pass;
        this.email = email;
    }

    public Utilizador(Utilizador a) {
        this.cod = a.getCod();
        this.nome = a.getNome();
        this.cord = a.getCord();
        this.pass = a.getPass();
        this.email = a.getEmail();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("User: ").append(this.cod).append("-")
                           .append(this.nome).append("-")
                           .append(this.cord.toString()).append("-")
                           .append(this.pass).append("-")
                           .append(this.email);
        return sb.toString();
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }

}
