import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class Voluntario extends Pessoa implements Serializable {
    private double raio;
    private Classificacoes classificacoes;
    private int nrEncomendas;

    public Voluntario(){
        super();
        this.raio = 0;
        this.classificacoes = new Classificacoes();
        int nrEncomendas = 0;

    }

    public Voluntario(String codigo,
                      String nome,
                      double gpsx,
                      double gpsy,
                      double raio,
                      String email,
                      String password,
                      Classificacoes c,
                      int nrEncomendas){
        super(codigo, nome, gpsx, gpsy, email, password);
        this.raio = raio;
        this.classificacoes = new Classificacoes(c);
        this.nrEncomendas = nrEncomendas;
    }

    public Voluntario(Voluntario v){
        super(v);
        this.raio = v.getRaio();
        this.classificacoes = new Classificacoes(v.getclassificacoes());
        this.nrEncomendas = v.getNrEncomendas();
    }
    public Classificacoes getclassificacoes(){
        return new Classificacoes(this.classificacoes);
    }

    public void addEncomenda(){
        this.nrEncomendas++;
    }
    public int getNrEncomendas() {
        return this.nrEncomendas;
    }

    public void setNrEncomendas(int nrEncomendas) {
        this.nrEncomendas = nrEncomendas;
    }

    public void setClassificacoes(Classificacoes c){
        this.classificacoes = new Classificacoes(c);
    }

    public double getRaio() {
        return this.raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }



    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Voluntario {\n");
        sb.append(super.toString());
        sb.append(", Raio: ").append(this.raio).append(", ");
        sb.append(this.classificacoes.toString()).append("\n");
        sb.append(", Número de encomendas:").append(this.nrEncomendas).append("\n");
        sb.append("}\n");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Voluntario that = (Voluntario) o;
        return Double.compare(that.raio, raio) == 0 &&
                nrEncomendas == that.nrEncomendas &&
                Objects.equals(classificacoes, that.classificacoes);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), raio, classificacoes, nrEncomendas);
    }
}
