package Stock;

import java.io.Serializable;
import java.time.LocalDateTime;

public class EncomendaRealizadaTransportadora implements Serializable {

    private String code_enc;
    private String code_utilizador;
    private String loja_onde_comprou;
    private double tempo_entrega;
    private double custo_entrega;
    private double custo_transporte;
    private LocalDateTime data_entrega;

    /**
     * Construtor por omissão.
     */
    public EncomendaRealizadaTransportadora(){
        this.code_enc = "";
        this.code_utilizador = "";
        this.loja_onde_comprou = "";
        this.tempo_entrega = 0;
        this.custo_entrega = 0;
        this.custo_transporte = 0;
        this.data_entrega = null;
    }

    /**
     * Construtor por parâmetros.
     * @param ce Código de encomenda.
     * @param cu Código de utilizador.
     * @param l Código da loja.
     * @param te Tempo de entrega.
     * @param cenc Custo de entrega.
     */
    public EncomendaRealizadaTransportadora(String ce, String cu, String l, double te, double ct, double cenc, LocalDateTime data){
        this.code_enc = ce;
        this.code_utilizador = cu;
        this.loja_onde_comprou = l;
        this.tempo_entrega = te;
        this.custo_entrega = cenc;
        this.custo_transporte = ct;
        this.data_entrega = data;
    }

    /**
     * Construtor por clonagem.
     * @param encomendaRealizada que vai ser clonada.
     */
    public EncomendaRealizadaTransportadora(EncomendaRealizadaTransportadora encomendaRealizada) {
        this.code_enc = encomendaRealizada.getCode_enc();
        this.code_utilizador = encomendaRealizada.getCode_utilizador();
        this.loja_onde_comprou = encomendaRealizada.getLoja_onde_comprou();
        this.tempo_entrega = encomendaRealizada.getTempo_entrega();
        this.custo_entrega = encomendaRealizada.getCusto_entrega();
        this.custo_transporte = encomendaRealizada.getCusto_transporte();
        this.data_entrega = encomendaRealizada.getData_entrega();
    }

    /**
     * Método que devolve o código de encomenda.
     * @return Código de encomenda.
     */
    public String getCode_enc() {
        return code_enc;
    }

    /**
     * Método que define o código de encomenda.
     * @param code_enc Código de encomenda.
     */
    public void setCode_enc(String code_enc) {
        this.code_enc = code_enc;
    }

    /**
     * Método que devolve o código de utilizador.
     * @return Código de utilizador.
     */
    public String getCode_utilizador() {
        return code_utilizador;
    }

    /**
     * Método que define o código de utilizador.
     * @param code_utilizador  Código de utilizador.
     */
    public void setCode_utilizador(String code_utilizador) {
        this.code_utilizador = code_utilizador;
    }

    /**
     * Método que devolve o código de loja.
     * @return Código de loja.
     */
    public String getLoja_onde_comprou() {
        return loja_onde_comprou;
    }

    /**
     * Método que define o código de loja.
     * @return Código de loja.
     */
    public void setLoja_onde_comprou(String loja_onde_comprou) {
        this.loja_onde_comprou = loja_onde_comprou;
    }

    /**
     * Método que devolve o tempo de entrega de uma encomenda.
     * @return Tempo de entrega.
     */
    public double getTempo_entrega() {
        return tempo_entrega;
    }

    /**
     * Método que define o tempo de entrega.
     * @param tempo_entrega que é o tempo que demorou a entregar a encomenda.
     */
    public void setTempo_entrega(double tempo_entrega) {
        this.tempo_entrega = tempo_entrega;
    }

    /**
     * Método que devolve o custo de entrega de uma encomenda.
     * @return Custo de entrega.
     */
    public double getCusto_entrega() {
        return custo_entrega;
    }

    /**
     * Método que define o custo de entrega de uma encomenda.
     * @param custo_entrega que é o custo de entrega da encomenda.
     */
    public void setCusto_entrega(double custo_entrega) {
        this.custo_entrega = custo_entrega;
    }

    /**
     * Método que devolva a data de entrega da encomenda.
     * @return Data de entrega.
     */
    public LocalDateTime getData_entrega() {
        return data_entrega;
    }

    /**
     * Método que define a data de entrega de  uma encomenda.
     * @param data_entrega
     */
    public void setData_entrega(LocalDateTime data_entrega) {
        this.data_entrega = data_entrega;
    }

    /**
     * Método que devolve o custo de transporte de uma encomenda.
     * @return Custo de transporte.
     */
    public double getCusto_transporte() {
        return custo_transporte;
    }

    /**
     * Método que define o custo de transporte de uma encomenda.
     * @param custo_transporte Custo de transporte.
     */
    public void setCusto_transporte(double custo_transporte) {
        this.custo_transporte = custo_transporte;
    }

    /**
     * Método que copia uma Stock.EncomendaRealizada.
     * @return Cópia da Stock.EncomendaRealizada.
     */
    public EncomendaRealizadaTransportadora clone(){
        return new EncomendaRealizadaTransportadora(this);
    }
}
