import java.io.Serializable;

public class Voluntario implements Serializable {
    private String codVoluntario;
    private String nome;
    private GPS gps;
    private double raio;

    public Voluntario() {
        this.codVoluntario = "n/a";
        this.nome = "n/a";
        this.gps = new GPS();
        this.raio = 0;
    }

    public Voluntario(String codVoluntario, String nome, GPS gps, double raio) {
        this.codVoluntario = codVoluntario;
        this.nome = nome;
        this.gps = new GPS(gps);
        this.raio = raio;
    }

    public Voluntario(Voluntario v){
        this.codVoluntario = v.getCodVoluntario();
        this.nome = v.getNome();
        this.gps = new GPS(v.getGps());
        this.raio = v.getRaio();
    }

    public String getCodVoluntario() {
        return codVoluntario;
    }

    public void setCodVoluntario(String codVoluntario) {
        this.codVoluntario = codVoluntario;
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

    public double getRaio() {
        return raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public boolean equals(Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Voluntario a = (Voluntario) o;
        return this.codVoluntario.equals(a.getCodVoluntario())
                && this.nome.equals(a.getNome())
                && this.gps.equals(a.getGps())
                && this.raio == (a.getRaio());
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nCodigo:").append(codVoluntario)
        .append("\nNome: ").append(nome)
        .append("\nGPS: ").append(gps)
        .append("Raio: ").append(raio);

                
        return sb.toString();
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }
}
