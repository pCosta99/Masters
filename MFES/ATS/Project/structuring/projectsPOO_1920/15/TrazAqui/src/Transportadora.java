import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Transportadora extends Utilizador implements Serializable {
    private String nif;
    private double raio;
    private double precoKm;
    private boolean disponibilidade;
    private double classificar;
    private int classificacoes;
    private List<Encomenda> historico;
    private double km_Percorridos;
    private boolean aceitoTransporteMedicamentos;

    public Transportadora(Transportadora t){
        super(t);
        this.nif = t.getNif();
        this.raio = t.getRaio();
        this.precoKm = t.getPrecoKm();
        this.disponibilidade = t.getDisponibilidade();
        this.classificacoes = t.getClassificacoes();
        this.classificar = t.getClassificar();
        this.historico = t.getHistorico();
        this.km_Percorridos = t.getKm_Percorridos();
        this.aceitoTransporteMedicamentos = t.getAceitoTransporteMedicamentos();
    }


    public Transportadora(String codUtilizador, String nome, double gpsx, double gpsy, String nif, double raio, double precoKm) {
        super(codUtilizador, nome, gpsx, gpsy);
        this.nif = nif;
        this.raio = raio;
        this.precoKm = precoKm;
        this.disponibilidade = true;
        this.classificacoes = 0;
        this.classificar = 0;
        this.historico = new ArrayList<>();
        this.km_Percorridos = 0;
        this.aceitoTransporteMedicamentos = false;
    }

    public Transportadora(String codUtilizador, String nome, double gpsx, double gpsy, String password, String nif, double raio, double precoKm, boolean disponibilidade, double classificar, int classificacoes, List<Encomenda> hist, double km_Percorridos, boolean aceitoTransporteMedicamentos) {
        super(codUtilizador, nome, gpsx, gpsy, password);
        this.nif = nif;
        this.raio = raio;
        this.precoKm = precoKm;
        this.disponibilidade = disponibilidade;
        this.classificacoes = classificacoes;
        this.classificar = classificar;
        this.historico = hist.stream().map(Encomenda::clone).collect(Collectors.toList());
        this.km_Percorridos = km_Percorridos;
        this.aceitoTransporteMedicamentos = aceitoTransporteMedicamentos;
    }

    private Transportadora(){
        super();
    }

    public List<Encomenda> getHistorico() {
        List<Encomenda> hist = new ArrayList<>();
        for(Encomenda e : this.historico){
            hist.add(e.clone());
        }
    return hist;
    }

    public void setHistorico(List<Encomenda> historico) {
        this.historico = historico.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public double getKm_Percorridos() {
        return km_Percorridos;
    }

    public void setKm_Percorridos(double km_Percorridos) {
        this.km_Percorridos = km_Percorridos;
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

    public boolean getDisponibilidade() {
        return disponibilidade;
    }

    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
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

    public double getPrecoKm() {
        return precoKm;
    }

    public void setPrecoKm(double precoKm) {
        this.precoKm = precoKm;
    }

    public boolean getAceitoTransporteMedicamentos() {
        return aceitoTransporteMedicamentos;
    }

    public void setAceitoTransporteMedicamentos(boolean aceitoTransporteMedicamentos) {
        this.aceitoTransporteMedicamentos = aceitoTransporteMedicamentos;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Transportadora ");
        sb.append(super.toString()).append(", ");
        sb.append("Nif: ").append(this.nif).append(", ");
        sb.append("Raio: ").append(this.raio).append(", ");
        sb.append("Preco por KM: ").append(this.precoKm).append(", ");
        sb.append("Disponibilidade: ").append(this.disponibilidade).append(", ");
        sb.append("Classificar: ").append(this.classificar).append(", ");
        sb.append("Classificacoes: ").append(this.classificacoes).append(", ");
        sb.append("Lista de Encomenda: ").append(this.historico.toString()).append("\n");
        return sb.toString();
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if((o == null) || o.getClass() != this.getClass()) return false;
        Transportadora p = (Transportadora) o;
        return(super.equals(o) && p.getNif().equals(this.nif) && p.getRaio() == this.raio && p.getPrecoKm() == this.precoKm && p.getDisponibilidade() == this.disponibilidade &&  p.getAceitoTransporteMedicamentos() == this.aceitoTransporteMedicamentos);
    }

    public Transportadora clone(){
        return new Transportadora(this);
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
