import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.io.Serializable;

public class Servico implements Serializable{
    private String codServico;
    private double classificacao;
    private boolean concluido;
    private Encomenda encomenda;
    private LocalDateTime dataInicio;
    private LocalDateTime dataFim;
    private String codTranportador;
    private String codUtilizador;
    private double kmPercorridos;
    private double preco;
    private int dias;


    public Servico() {
        this.codServico = "n/a";
        this.classificacao = -1;
        this.concluido = false;
        this.encomenda = new Encomenda();
        this.dataInicio = null;
        this.dataFim = null;
        this.codTranportador = "n/a";
        this.codUtilizador = "n/a";
        this.kmPercorridos = 0;
        this.preco = 0;
        this.dias = 0;
    }

    public Servico( String codServico, boolean concluido, Encomenda encomenda, LocalDateTime dataInicio,String codRecetor,String cod,double km, double preco,int dias) {
        this.codServico = codServico;
        this.classificacao=-1;
        this.concluido = concluido;
        this.encomenda = new Encomenda(encomenda);
        this.dataInicio = dataInicio;
        this.dataFim = this.dataInicioMaisDias(dataInicio,dias);
        this.codTranportador=cod;
        this.codUtilizador = codRecetor;
        this.kmPercorridos = km;
        this.preco = preco;
        this.dias = dias;
    }

    public Servico(Servico p){
        this.codServico = p.getCodServico();
        this.concluido = p.isConcluido();
        this.classificacao = p.getClassificacao();
        this.encomenda = new Encomenda(p.getEncomenda());
        this.dataInicio = p.getDataInicio();
        this.dataFim = p.getDataFim();
        this.codTranportador=p.getCodTranportador();
        this.codUtilizador = p.getCodUtilizador();
        this.kmPercorridos = p.getKmPercorridos();
        this.preco = p.getPreco();
        this.dias = p.getDias();
    }

    public void setCodServico(String codServico){
        this.codServico = codServico;
    }
    public String getCodServico() {
        return codServico;
    }

    public double getClassificacao() {
        return classificacao;
    }

    public void setClassificacao(double classificacao) {
        this.classificacao = classificacao;
    }

    public boolean isConcluido() {
        return concluido;
    }

    public void setConcluido(boolean concluido) {
        this.concluido = concluido;
    }

    public Encomenda getEncomenda() {
        return this.encomenda.clone();
    }

    public void setEncomenda(Encomenda encomenda) {
        this.encomenda = new Encomenda(encomenda);
    }

    public LocalDateTime getDataInicio() {
        return dataInicio;
    }

    public void setDataInicio(LocalDateTime dataInicio) {
        this.dataInicio = dataInicio;
    }

    public LocalDateTime getDataFim() {
        return dataFim;
    }

    public void setDataFim(LocalDateTime dataFim) {
        this.dataFim = dataFim;
    }

    public String getCodTranportador() {
        return codTranportador;
    }

    public void setCodTranportador(String codTranportador){
        this.codTranportador = codTranportador;
    }

    public String getCodUtilizador() {
        return this.codUtilizador ;
    }

    public void setCodUtilizador(String codUtilizador){
        this.codUtilizador = codUtilizador;
    }

    public double getPreco() {
        return preco;
    }

    public void setPreco(double preco) {
        this.preco = preco;
    }

    public double getKmPercorridos() {
        return kmPercorridos;
    }

    public void setKmPercorridos(double km){
        this.kmPercorridos = km;
    }

    public int getDias() {
        return dias;
    }

    public void setDias(int dias) {
        this.dias = dias;
    }

    public String toString(){
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-YYYY HH:mm");
        DecimalFormat fmt = new DecimalFormat("0.00");
        StringBuilder sb = new StringBuilder();
        sb.append(this.getCodServico())
                .append(",").append(this.getEncomenda())
                .append("\n Destinitario:").append(this.codUtilizador)
                .append("\nTransportador:").append(this.codTranportador)
                .append("\nKm:").append(fmt.format(this.getKmPercorridos()))
                .append("\nPreco:").append(fmt.format(this.getPreco()))
                .append("\nClassificao: ").append(this.getClassificacao());

        return sb.toString();
    }

    public boolean equals(Object o){
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Servico a = (Servico) o;
        return this.encomenda.equals(a.getEncomenda()) && this.codServico.equals(a.getCodServico()) && this.codTranportador.equals(a.getCodTranportador()) && this.codUtilizador.equals(a.getCodUtilizador())
                && this.classificacao==(a.getClassificacao())
                && this.concluido==(a.isConcluido())
                && this.dataInicio.equals(a.getDataInicio())
                && this.dataFim.equals(a.getDataFim())
                && this.kmPercorridos==(a.getKmPercorridos())
                && this.preco==(a.getPreco())
                && this.dias == (a.getDias());
    }

    public boolean equalsCode(String codServico){
        if (this.getCodServico().equals(codServico)) return true;
        return false;
    }

    public Servico clone(){
        return new Servico(this);
    }


    public boolean isEntreDatas(LocalDateTime data1,LocalDateTime data2){
        if((this.dataFim.isAfter(data1) || this.dataFim.isEqual(data1))&& (this.dataFim.isBefore(data2) || this.dataFim.isEqual(data2))) return true;
        else return false;
    }

    public LocalDateTime dataInicioMaisDias(LocalDateTime dataInicio , int dias){
        if(dataInicio == null) return null;
        LocalDateTime dataFim = dataInicio.plusDays(dias);
        return dataFim;
    }

    public boolean hasClassificacao(){
        if(this.classificacao == -1) return false ;
        return true;
    }
}
