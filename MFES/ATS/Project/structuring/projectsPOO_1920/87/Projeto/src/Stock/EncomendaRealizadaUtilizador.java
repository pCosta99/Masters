package Stock;

import java.io.Serializable;
import java.time.LocalDateTime;

public class EncomendaRealizadaUtilizador implements Serializable {

    private String code_enc;
    private String code_utilizador;
    private String loja_onde_comprou;
    private String code_entregador;
    private double tempo_entrega;
    private LocalDateTime data_entrega;

    /**
     * Construtor por omissão.
     */
    public EncomendaRealizadaUtilizador(){
        this.code_enc = "";
        this.code_utilizador = "";
        this.loja_onde_comprou = "";
        this.code_entregador = "";
        this.tempo_entrega = 0;
        this.data_entrega = null;
    }

    /**
     * Construtor por parâmetros.
     * @param ce Código de encomenda.
     * @param cu Código de utilizador.
     * @param l Código da loja.
     * @param te Tempo de entrega.
     */
    public EncomendaRealizadaUtilizador(String ce, String cu, String l, String cod_ent, double te, LocalDateTime data){
        this.code_enc = ce;
        this.code_utilizador = cu;
        this.loja_onde_comprou = l;
        this.code_entregador = cod_ent;
        this.tempo_entrega = te;
        this.data_entrega = data;
    }

    /**
     * Construtor por clonagem.
     * @param encomendaRealizada que vai ser clonada.
     */
    public EncomendaRealizadaUtilizador(EncomendaRealizadaUtilizador encomendaRealizada) {
        this.code_enc = encomendaRealizada.getCode_enc();
        this.code_utilizador = encomendaRealizada.getCode_utilizador();
        this.loja_onde_comprou = encomendaRealizada.getLoja_onde_comprou();
        this.code_entregador = encomendaRealizada.getCode_entregador();
        this.tempo_entrega = encomendaRealizada.getTempo_entrega();
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
     * Método que devolve a data de entrega de uma encomenda.
     * @return Data de entrega.
     */
    public LocalDateTime getData_entrega() {
        return data_entrega;
    }

    /**
     * Método que define a data de entrega de uma encomenda.
     * @param data_entrega Data de entrega.
     */
    public void setData_entrega(LocalDateTime data_entrega) {
        this.data_entrega = data_entrega;
    }

    /**
     * Método que devolve o código do entregador.
     * @return Código.
     */
    public String getCode_entregador() {
        return code_entregador;
    }

    /**
     * Método que define o código de entregador.
     * @param code_entregador Código.
     */
    public void setCode_entregador(String code_entregador) {
        this.code_entregador = code_entregador;
    }

    /**
     * Método que copia uma EncomendaRealizada por um Voluntario.
     * @return Cópia da EncomendaRealizadaVoluntario.
     */
    public EncomendaRealizadaUtilizador clone(){
        return new EncomendaRealizadaUtilizador(this);
    }

    /**
     * Método que converte numa String a informação de uma EncomendaRealizadaUtilizador.
     * @return String com a informação.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda:").append(this.code_enc).append(",").append(this.code_utilizador).append(",").append(this.loja_onde_comprou).append(",").append(this.code_entregador);
        return sb.toString();
    }
}
