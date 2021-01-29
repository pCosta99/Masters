import java.io.Serializable;

public class Loja implements Serializable {
    private String codLoja;
    private String nomeLoja;
    private GPS gps;

    public Loja() {
        this.codLoja = "n/a";
        this.nomeLoja = "n/a";
        this.gps = new GPS();
    }

    public Loja(String codLoja, String nomeLoja,GPS gps) {
        this.codLoja = codLoja;
        this.nomeLoja = nomeLoja;
        this.gps = new GPS(gps);
    }

    public Loja(Loja l){
        this.codLoja = l.getCodLoja();
        this.nomeLoja = l.getNomeLoja();
        this.gps = new GPS(l.getGps());
    }

    public String getCodLoja() {
        return codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public String getNomeLoja() {
        return nomeLoja;
    }

    public void setNomeLoja(String nomeLoja) {
        this.nomeLoja = nomeLoja;
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
        Loja a = (Loja) o;
        return this.codLoja.equals(a.getCodLoja())
                && this.nomeLoja.equals(a.getNomeLoja())
                && this.gps.equals(a.getGps());
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo:").append(codLoja)
                .append("\nNome:").append(nomeLoja)
                .append("\nGPS:").append(gps);
        return sb.toString();
    }

    public Loja clone(){
        return new Loja(this);
    }
}
