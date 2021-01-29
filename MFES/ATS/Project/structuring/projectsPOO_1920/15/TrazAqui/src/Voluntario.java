import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Voluntario extends Utilizador implements Serializable {

    private double raio;
    private boolean disponibilidade;
    private int classificacoes;
    private double classificar;
    private List<Encomenda> historico;
    private boolean aceitaMedicamentos;


    public Voluntario(String codUtilizador, String nome, double gpsx, double gpsy, double raio) {
        super(codUtilizador, nome, gpsx, gpsy);
        this.raio = raio;
        this.disponibilidade = true;
        this.classificacoes = 0;
        this.classificar = 0;
        this.historico = new ArrayList<>();
        this.aceitaMedicamentos = false;

    }

    public Voluntario(String codUtilizador, String nome, double gpsx, double gpsy, String password, double raio, boolean disponibilidade, int classificacoes, double classificar, List<Encomenda> hist, boolean aceitaMedicamentos) {
        super(codUtilizador, nome, gpsx, gpsy,password);
        this.raio = raio;
        this.disponibilidade = disponibilidade;
        this.classificacoes = classificacoes;
        this.classificar = classificar;
        this.historico = hist.stream().map(Encomenda::clone).collect(Collectors.toList());
        this.aceitaMedicamentos = aceitaMedicamentos;
    }

    public Voluntario(Voluntario v) {
        super(v);
        this.raio = v.getRaio();
        this.disponibilidade = v.getDisponibilidade();
        this.classificacoes = v.getClassificacoes();
        this.classificar = v.getClassificar();
        this.historico = v.getHistorico();
        this.aceitaMedicamentos = v.getAceitaMedicamentos();
    }

    private Voluntario(){
        super();
    }

    public List<Encomenda> getHistorico() {
        List<Encomenda> hist = new ArrayList<>();
        if (this.historico != null){
            for(Encomenda e : this.historico){
                hist.add(e.clone());
            }
            return hist;
        }
        return hist;
    }

    public void setHistorico(List<Encomenda> historico) {
        this.historico = historico.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public double getClassificar() {
        return classificar;
    }

    public void setClassificar(double classificar) {
        this.classificar = classificar;
    }

    public int getClassificacoes() {
        return classificacoes;
    }

    public void setClassificacoes(int classificacoes) {
        this.classificacoes = classificacoes;
    }

    public double getRaio() {
        return raio;
    }

    public void setRaio(double raio) {
        this.raio = raio;
    }

    public boolean getDisponibilidade() {
        return disponibilidade;
    }

    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    public boolean getAceitaMedicamentos() {
        return aceitaMedicamentos;
    }

    public void setAceitaMedicamentos(boolean aceitaMedicamentos) {
        this.aceitaMedicamentos = aceitaMedicamentos;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if((o == null) || o.getClass() != this.getClass()) return false;
        Voluntario p = (Voluntario) o;
        return(super.equals(o) && this.raio == p.getRaio() && this.disponibilidade == p.getDisponibilidade());
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Voluntário ");
        sb.append(super.toString()).append(", ");
        sb.append("Raio: ").append(this.raio).append(", ");
        sb.append("Disponibilidade: ").append(this.disponibilidade);
        sb.append("Classificar: ").append(this.classificar).append(", ");
        sb.append("Classificacoes: ").append(this.classificacoes).append(", ");
        sb.append("Lista de Encomenda: ").append(this.historico.toString()).append("\n");
        return sb.toString();
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }

    public void atualiza_Class(double pontos){
        this.classificacoes +=1;
        this.classificacoes += pontos;
        setClassificacoes(this.classificacoes);
        setClassificar(this.classificar / this.classificacoes);
    }

    public void adicionaEnc(Encomenda enc){
        if (!this.historico.contains(enc)) {
            this.historico.add(enc.clone());
        }
    }
}
