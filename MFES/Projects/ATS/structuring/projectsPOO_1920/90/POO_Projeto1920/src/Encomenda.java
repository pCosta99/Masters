import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Classe que representa uma encomenda
 */
public class Encomenda implements Comparable<Encomenda>, Serializable {
    // variáveis de instância
    private String cod_enc;
    private String cod_user;
    private String cod_loja;
    private double peso;
    private boolean encomendaMedica;
    private String transportador;
    private final ServicoEntrega servicoEntrega;
    private List<LinhaEncomenda> linhas;

    /**
     * Construtores para objetos da classe Encomenda
     */
    public Encomenda() {
        this.cod_enc = "";
        this.cod_user = "";
        this.cod_loja = "";
        this.peso = 0.0;
        this.encomendaMedica = false;
        this.transportador = "";
        this.servicoEntrega = new ServicoEntrega();
        this.linhas = new ArrayList<>();
    }

    public Encomenda(String enc, String user, String loja, double peso, boolean encomendaMedica, String transportador, Collection<LinhaEncomenda> linhas) {
        this.cod_enc = enc;
        this.cod_user = user;
        this.cod_loja = loja;
        this.peso = peso;
        this.encomendaMedica = encomendaMedica;
        this.transportador = transportador;
        this.servicoEntrega = new ServicoEntrega();
        this.setLinhas(linhas);
    }

    public Encomenda(String enc, String user, String loja, double peso, String transportador, boolean encomendaMedica) {
        this.cod_enc = enc;
        this.cod_user = user;
        this.cod_loja = loja;
        this.peso = peso;
        this.encomendaMedica = encomendaMedica;
        this.transportador = transportador;
        this.servicoEntrega = new ServicoEntrega();
        this.linhas = new ArrayList<>();
    }

    public Encomenda(Encomenda e) {
        this.cod_enc = e.getCodEnc();
        this.cod_user = e.getCodUser();
        this.cod_loja = e.getCodLoja();
        this.peso = e.getPeso();
        this.encomendaMedica = e.isEncomendaMedica();
        this.transportador = e.getTransportador();
        this.servicoEntrega = e.getServicoEntrega();
        this.linhas = e.getLinhas();
    }

    /**
     * Metodos de instancia
     */

    public String getCodEnc() {
        return this.cod_enc;
    }

    public String getCodUser() {
        return this.cod_user;
    }

    public String getCodLoja() {
        return this.cod_loja;
    }

    public double getPeso() {
        return this.peso;
    }

    public boolean isEncomendaMedica() {
        return this.encomendaMedica;
    }

    public String getTransportador() {
        return this.transportador;
    }

    public ServicoEntrega getServicoEntrega() {
        return servicoEntrega;
    }

    public EstadoEncomenda getEstado() {
        return servicoEntrega.getEstado();
    }

    public Integer getClassificacao() {
        return servicoEntrega.getClassificacao();
    }

    public List<LinhaEncomenda> getLinhas() {
        List<LinhaEncomenda> novo;

        novo = this.linhas.stream()
                .map(LinhaEncomenda::clone).collect(Collectors.toCollection(ArrayList::new));

        return novo;
    }

    public void setCodEnc(String code) {
        this.cod_enc = code;
    }

    public void setCodUser(String code) {
        this.cod_user = code;
    }

    public void setCodLoja(String code) {
        this.cod_loja = code;
    }

    public void setPeso(double p) {
        this.peso = p;
    }

    public void setEncomendaMedica(boolean encomendaMedica) {
        this.encomendaMedica = encomendaMedica;
    }

    public void setTransportador(String transportador) {
        this.transportador = transportador;
    }

    public void setEstado(EstadoEncomenda ee) {
        this.servicoEntrega.setEstado(ee);
    }

    public void setLinhas(Collection<LinhaEncomenda> linhas) {
        this.linhas = new ArrayList<>();

        for (LinhaEncomenda le : linhas) this.linhas.add(le.clone());
    }

    public void addLinha(LinhaEncomenda le) {
        this.linhas.add(le.clone());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return Double.compare(encomenda.getPeso(), getPeso()) == 0 &&
                isEncomendaMedica() == encomenda.isEncomendaMedica() &&
                cod_enc.equals(encomenda.cod_enc) &&
                cod_user.equals(encomenda.cod_user) &&
                cod_loja.equals(encomenda.cod_loja) &&
                getTransportador().equals(encomenda.getTransportador()) &&
                getServicoEntrega().equals(encomenda.getServicoEntrega()) &&
                getLinhas().equals(encomenda.getLinhas());
    }

    public Encomenda clone() {
        return new Encomenda(this);
    }

    @Override
    public String toString() {
        return "Encomenda{" +
                "cod_enc='" + cod_enc + '\'' +
                ", cod_user='" + cod_user + '\'' +
                ", cod_loja='" + cod_loja + '\'' +
                ", peso=" + peso +
                ", encomendaMedica=" + encomendaMedica +
                ", transportador='" + transportador + '\'' +
                ", servicoEntrega=" + servicoEntrega +
                ", linhas=" + linhas +
                '}';
    }

    public Collection<String> produtosEncomendados() {
        return linhas.stream().map(LinhaEncomenda::getDesc)
                .collect(Collectors.toCollection(TreeSet::new));
    }


    public LocalDateTime dataEncomenda() {
        return servicoEntrega.getDataNova();
    }

    public int compareTo(Encomenda e) {
        return this.cod_enc.compareTo(e.getCodEnc());
    }

    /**
     * Muda estado de uma encomenda
     * @param novoEstado estado novo
     */
    public void mudaEstado(EstadoEncomenda novoEstado, int minutos) {
        if (novoEstado != EstadoEncomenda.NOVA) {
            this.servicoEntrega.setEstado(novoEstado);
            switch (novoEstado) {
                case PRONTA_A_SER_ENTREGUE:
                    this.servicoEntrega.setDataProntaASerEntregue(LocalDateTime.now());
                    break;
                case EM_ACEITACAO:
                    this.servicoEntrega.setDataEmAceitacao(LocalDateTime.now());
                    break;
                case EM_TRANSPORTE:
                    this.servicoEntrega.setDataEmTransporte(LocalDateTime.now());
                    break;
                case ENTREGUE:
                    this.servicoEntrega.setDataEntregue(servicoEntrega.getDataNova().plusMinutes(minutos));
                    break;
            }
        }
    }

    public void setClassificacaoDeTransporte(int classificacao) {
        this.servicoEntrega.setClassificacao(classificacao);
    }


    /**
     * Calcula o custo de transporte de uma encomenda
     * @param distancia total do percurso
     * @param peso peso da encomenda
     * @param taxaDistancia taxa da distância da empresa transportadora
     * @param taxaPeso taxa de peso da empresa transportadora
     */
    public void setCustoDeTransporte(double distancia, double peso, double taxaDistancia, double taxaPeso){
        double preco = taxaDistancia * distancia + taxaPeso * peso;
        this.servicoEntrega.setCusto(preco);
    }

    /**
     * Calcula o tempo de transporte de uma encomenda(em min)
     * @param tempoEsperaNaLojaMin tempo de espera na loja
     * @param distanciaTotal Distância total do percurso de entrega
     * @param velocidadeKmHora Velocidade do transportador
     * @return Tempo total do transporte
     */
    public int  calculaTempoDeTransporteEncomenda(double tempoEsperaNaLojaMin,double distanciaTotal,double velocidadeKmHora){
        return (int) (tempoEsperaNaLojaMin + (distanciaTotal / (velocidadeKmHora)) * 60);
    }

}
