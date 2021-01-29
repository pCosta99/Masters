import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class Transportadora extends Empresa implements Serializable {
    private String nif;
    private double raio;
    private double precoKm;
    private Classificacoes classificacoes;
    private int nrEncomendas;
    private double kms;


    public Transportadora() {
        super();
        this.nif = "";
        this.raio = 0;
        this.precoKm = 0;
        this.classificacoes = new Classificacoes();
        this.nrEncomendas = 0;
        this.kms = 0;
    }

    public Transportadora(String codTransportadora,
                          String nomeTransportadora,
                          double gpsx,
                          double gpsy,
                          String nif,
                          double raio,
                          double precoKm,
                          String email,
                          String password,
                          Classificacoes c,
                          int nrEncomendas,
                          double kms){
        super(codTransportadora,nomeTransportadora,gpsx,gpsy,email,password);
        this.nif = nif;
        this.raio = raio;
        this.precoKm = precoKm;
        this.classificacoes = new Classificacoes(c);
        this.nrEncomendas = nrEncomendas;
        this.kms = kms;
    }

    public Transportadora(Transportadora t){
        super(t);
        this.nif = t.getNif();
        this.raio = t.getRaio();
        this.precoKm = t.getPrecoKm();
        this.classificacoes = new Classificacoes(t.getclassificacoes());
        this.nrEncomendas = t.getNrEncomendas();
        this.kms = t.getKms();
    }

    public int getNrEncomendas() {
        return this.nrEncomendas;
    }

    public void setNrEncomendas(int nrEncomendas) {
        this.nrEncomendas = nrEncomendas;
    }

    public String getNif() {
        return this.nif;
    }

    public void setNif(String nif) {
        this.nif = nif;
    }

    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public double getPrecoKm() {
        return this.precoKm;
    }

    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }

    public Classificacoes getclassificacoes(){
        return new Classificacoes(this.classificacoes);
    }

    public void setClassificacoes(Classificacoes classificacoes) {
        this.classificacoes = new Classificacoes(classificacoes);
    }

    public double getKms() {
        return this.kms;
    }

    public void setKms(double kms) {
        this.kms = kms;
    }

    public void addEncomenda(){
        this.nrEncomendas++;
    }


    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(" Transportadora{ \n");
        sb.append(super.toString());
        sb.append(", Nif: ").append(this.nif);
        sb.append(", Raio: ").append(this.raio);
        sb.append(", Preco por Km: ").append(this.precoKm).append(", ");
        sb.append(this.classificacoes.toString()).append(", ");
        sb.append(", Número de encomendas: ").append(this.nrEncomendas);
        sb.append(", Kms realizados: ").append(this.kms).append("\n");
        sb.append("}\n");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Transportadora that = (Transportadora) o;
        return Double.compare(that.raio, raio) == 0 &&
                Double.compare(that.precoKm, precoKm) == 0 &&
                Double.compare(that.kms, kms) == 0 &&
                Objects.equals(nif, that.nif) &&
                Objects.equals(classificacoes, that.classificacoes) &&
                Objects.equals(nrEncomendas, that.nrEncomendas);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), nif, raio, precoKm);
    }

    public Transportadora clone(){
        return new Transportadora(this);
    }
}