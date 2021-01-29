import java.io.Serializable;

public class Utilizador implements Serializable {
    private String codUtilizador;
    private String nome;
    private GPS gps;

    public Utilizador() {
        this.codUtilizador = "n/a";
        this.nome = "n/a";
        this.gps = new GPS();
    }

    public Utilizador(String codUtilizador, String nome, GPS gps) {
        this.codUtilizador = codUtilizador;
        this.nome = nome;
        this.gps = new GPS(gps);
    }

    public Utilizador(Utilizador u){
        this.codUtilizador = u.getCodUtilizador();
        this.nome = u.getNome();
        this.gps = new GPS(u.getGps());
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public GPS getGps() {
        return gps.clone();
    }

    public void setGps(GPS gps) {
        this.gps = new GPS(gps);
    }

    public boolean equals(Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Utilizador a = (Utilizador) o;
        return this.codUtilizador.equals(a.getCodUtilizador())
                && this.nome.equals(a.getNome())
                && this.gps.equals(a.getGps());
    }
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo:").append(codUtilizador)
                .append("\nNome:").append(nome)
                .append("\nGPS:").append(gps.toString()).append("\n");
        return sb.toString();
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }
}
