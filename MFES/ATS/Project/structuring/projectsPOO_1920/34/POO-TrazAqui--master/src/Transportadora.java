import java.io.Serializable;

public class Transportadora implements Serializable {
    private String codEmpresa;
    private String nomeEmpresa;
    private GPS gps;
    private String nif;
    private double raio;
    private double precoPorKm;

    public Transportadora() {
        this.codEmpresa = "n/a";
        this.nomeEmpresa = "n/a";
        this.gps = new GPS();
        this.nif = "n/a";
        this.raio = 0;
        this.precoPorKm = 0;
    }

    public Transportadora(String codEmpresa, String nomeEmpresa, GPS gps, String nif, double raio, double precoPorKm) {
        this.codEmpresa = codEmpresa;
        this.nomeEmpresa = nomeEmpresa;
        this.gps = new GPS(gps);
        this.nif = nif;
        this.raio = raio;
        this.precoPorKm = precoPorKm;
    }

    public Transportadora(Transportadora t){
        this.codEmpresa = t.getCodEmpresa();
        this.nomeEmpresa = t.getNomeEmpresa();
        this.gps = new GPS(t.getGps());
        this.nif = t.getNif();
        this.raio = t.getRaio();
        this.precoPorKm = t.getPrecoPorKm();
    }

    public String getCodEmpresa() {
        return codEmpresa;
    }

    public void setCodEmpresa(String codEmpresa) {
        this.codEmpresa = codEmpresa;
    }

    public String getNomeEmpresa() {
        return nomeEmpresa;
    }

    public void setNomeEmpresa(String nomeEmpresa) {
        this.nomeEmpresa = nomeEmpresa;
    }

    public GPS getGps() {
        return gps.clone();
    }

    public void setGps(GPS gps) {
        this.gps = new GPS(gps);
    }

    public String getNif() {
        return nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public double getRaio() {
        return raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public double getPrecoPorKm() {
        return precoPorKm;
    }

    public void setPrecoPorKm(double precoPorKm) {
        this.precoPorKm = precoPorKm;
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Transportadora a = (Transportadora) o;
        return this.codEmpresa.equals(a.getCodEmpresa())
                && this.nomeEmpresa.equals(a.getNomeEmpresa())
                && this.gps.equals(a.getGps())
                && this.nif.equals(a.getNif())
                && this.raio == a.getRaio()
                && this.precoPorKm == a.getPrecoPorKm();
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Codigo:").append(codEmpresa)
                .append("\nNome:").append(nomeEmpresa)
                .append("\nGPS:").append(gps)
                .append("NIF:").append(nif)
                .append("\nRaio:").append(raio).append("\n");
        return sb.toString();
    }

    public Transportadora clone(){
        return new Transportadora(this);
    }
}
