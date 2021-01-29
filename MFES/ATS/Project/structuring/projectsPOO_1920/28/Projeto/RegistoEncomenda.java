import java.io.Serializable;
import java.time.LocalDate;

/**
 * Classe usada para guardar os registos de uma dada entrega,
 * Vai ser colocada em uma lista na classe RegistoEncomendaLista
 */


public class RegistoEncomenda implements Serializable {
    
    private String entregador;    // quem entregou a encomenda
    private Encomenda encomenda;  // a encomenda que foi entregue
    private int classificacao;    /* a classificação recebida pelo Utilizador, vai de 1-5, 
                                     se tiver 0 quer dizer que ainda nao foi avalidado */
    private LocalDate dataDaEntrega;   // a data de quando foi entregue a encomenda ao Utilizador
    private double kmsPercorridos;     //kms percorridos totais: tranportador -> loja + loja -> utilizador
    private double taxaDeEntrega;      /*so usado para as transportadoras quando cobrarem algo na entrega,
                                         se for voluntario vai ter 0 */
    private double tempoDeEntrega;     //tempo demorado a entregar a encomenda
    
    public RegistoEncomenda() {
        this.entregador = "";
        this.encomenda = new Encomenda();
        this.classificacao = 0;
        this.dataDaEntrega = LocalDate.now();
        this.kmsPercorridos = 0;
        this.taxaDeEntrega = 0;
        this.tempoDeEntrega = 0;
    }

    public RegistoEncomenda(String entregador, Encomenda encomenda, int classificacao, LocalDate dataDaEntrega, double kmsPercorridos, double taxaDeEntrega, double tempoDeEntrega) {
        this.entregador = entregador;
        this.encomenda = encomenda;
        this.classificacao = classificacao;
        this.dataDaEntrega = dataDaEntrega;
        this.kmsPercorridos = kmsPercorridos;
        this.taxaDeEntrega = taxaDeEntrega;
        this.tempoDeEntrega = tempoDeEntrega;
    }

    public RegistoEncomenda (RegistoEncomenda e){
        this.entregador = e.getEntregador();
        this.encomenda = e.getEncomenda();
        this.classificacao = e.getClassificacao();
        this.dataDaEntrega = e.getDataDaEntrega();
        this.kmsPercorridos = e.getKmsPercorridos();
        this.taxaDeEntrega = e.getTaxaDeEntrega();
        this.tempoDeEntrega = e.getTempoDeEntrega();
    }

    public void setTempoDeEntrega (double x) {
        this.tempoDeEntrega = x;
    }

    public double getTempoDeEntrega (){
        return this.tempoDeEntrega;
    }
    
    public void setKmsPercorridos (double kmsPercorridos){
        this.kmsPercorridos = kmsPercorridos;
    }

    public double getKmsPercorridos(){
        return this.kmsPercorridos;
    }

    public void setTaxaDeEntrega (double taxaDeEntrega){
        this.taxaDeEntrega = taxaDeEntrega;
    }

    public double getTaxaDeEntrega() {
        return this.taxaDeEntrega;
    }
    public String getEntregador() {
        return this.entregador;
    }

    public void setEntregador(String entregador) {
        this.entregador = entregador;
    }

    public Encomenda getEncomenda() {
        return this.encomenda;
    }

    public void setEncomenda(Encomenda encomenda) {
        this.encomenda = encomenda;
    }

    public int getClassificacao() {
        return this.classificacao;
    }

    public void setClassificacao(int classificacao) {
        this.classificacao = classificacao;
    }

    public LocalDate getDataDaEntrega() {
        return this.dataDaEntrega;
    }

    public void setDataDaEntrega(LocalDate dataDaEntrega) {
        this.dataDaEntrega = dataDaEntrega;
    }

    /**
     * Comparamos todas as variáveis neste equals
     */
    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof RegistoEncomenda)) {
            return false;
        }
        RegistoEncomenda re = (RegistoEncomenda) o;
        return this.entregador.equals(re.getEntregador()) &&
                this.encomenda.equals(re.getEncomenda()) &&
                this.classificacao == re.getClassificacao() &&
                this.dataDaEntrega.equals(re.getDataDaEntrega()) &&
                this.kmsPercorridos == (re.getKmsPercorridos()) &&
                this.taxaDeEntrega == (re.getTaxaDeEntrega()) &&
                this.tempoDeEntrega == (re.getTempoDeEntrega());
    }


    public String toString() {

        StringBuilder sb = new StringBuilder();
        sb.append("\nEntregador = ").append(getEntregador())
        .append("\nEncomenda = ").append(getEncomenda())
        .append("\nClassificacao = ").append(getClassificacao())
        .append("\nData da entrega = ").append(getDataDaEntrega());

        return sb.toString();
        
    }

 
    public RegistoEncomenda clone () {
        return new RegistoEncomenda(this);
    }
}