import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Distribuidora extends Entidade implements Serializable {
    private String nif;
    private double raio;
    private float taxa;
    private List<String> estafetas; // talvez meter um HashMap com key = estafeta e value = atividade(true/false)

    public Distribuidora() {
        super();
        this.nif = "n/a";
        this.raio = 0;
        this.taxa = 0;
        this.estafetas = new ArrayList<>();
    }

    public Distribuidora(String umCodigo, String umNome, GPS umaLocalizacao, String umNif, double umRaio, float umaTaxa, List<String> unsEstafetas){
        super(umCodigo, umNome, umaLocalizacao);
        this.nif = umNif;
        this.raio = umRaio;
        this.taxa = umaTaxa;
        this.estafetas = unsEstafetas;
    }

    /**
     * Método de criação de Empresas para os Logs fornecidos
     * @param umCodigo
     * @param umNome
     * @param umaLocalizacao
     * @param umNif
     * @param umRaio
     * @param umaTaxa
     */
    public Distribuidora(String umCodigo, String umNome, GPS umaLocalizacao, String umNif, double umRaio, float umaTaxa){
        super(umCodigo, umNome, umaLocalizacao);
        this.nif = umNif;
        this.raio = umRaio;
        this.taxa = umaTaxa;
        this.estafetas = new ArrayList<>();
    }

    public Distribuidora(Distribuidora umaDistribuidora){
        super(umaDistribuidora);
        this.nif = umaDistribuidora.getNif();
        this.raio = umaDistribuidora.getRaio();
        this.taxa = umaDistribuidora.getTaxa();
        this.estafetas = umaDistribuidora.getEstafetas();
    }

    public Distribuidora clone(){
        return new Distribuidora((this));
    }

    public String getNif() {
        return this.nif;
    }

    public double getRaio() {
        return this.raio;
    }

    public float getTaxa() {
        return this.taxa;
    }

    public List<String> getEstafetas() {
        return this.estafetas;
    }

    public void setNif(String novoNif) {
        this.nif = novoNif;
    }

    public void setRaio(double novoRaio) {
        this.raio = novoRaio;
    }

    public void setTaxa(float novaTaxa) {
        this.taxa = novaTaxa;
    }

    public void setEstafetas(List<String> novosEstafetas) {
        this.estafetas = novosEstafetas;
    }



    public String toString(){
        return super.toString() +
                "\n\t" + "Códigos de Trabalhadores: " + getEstafetas()
                +'\n' + '}';
    }

    public String toCSV(){
        return super.toCSV() + ", [" + this.getEstafetas() + "]";
    }

    public String toLog(){
        return super.toLog() +  ',' + this.nif +  ',' + this.raio +  ',' + this.taxa;
    }

    public boolean isTrabalhador(String codEstafeta){
        return this.estafetas.contains(codEstafeta);
    }

    public void addEstafeta(Estafeta e) {
        if (e != null) {
            if (e.isVoluntario())
                e.setVoluntario(false);
            this.estafetas.add(e.getCodigo());
        }
    }
}
