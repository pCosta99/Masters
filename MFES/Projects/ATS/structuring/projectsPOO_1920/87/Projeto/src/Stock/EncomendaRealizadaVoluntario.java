package Stock;

import java.io.Serializable;
import java.time.LocalDateTime;

public class EncomendaRealizadaVoluntario implements Serializable {

    private String code_enc;
    private String code_utilizador;
    private String loja_onde_comprou;
    private double tempo_entrega;
    private LocalDateTime data_entrega;

    /**
     * Construtor por omissão.
     */
    public EncomendaRealizadaVoluntario() {
        this.code_enc = "";
        this.code_utilizador = "";
        this.loja_onde_comprou = "";
        this.tempo_entrega = 0;
        this.data_entrega = null;
    }

    /**
     * Construtor por parâmetros.
     * @param ce Código de encomenda.
     * @param cu Código de utilizador.
     * @param l  Código da loja.
     * @param te Tempo de entrega.
     */
    public EncomendaRealizadaVoluntario(String ce, String cu, String l, double te, LocalDateTime data) {
        this.code_enc = ce;
        this.code_utilizador = cu;
        this.loja_onde_comprou = l;
        this.tempo_entrega = te;
        this.data_entrega = data;
    }

    /**
     * Construtor por clonagem.
     * @param encomendaRealizada que vai ser clonada.
     */
    public EncomendaRealizadaVoluntario(EncomendaRealizadaVoluntario encomendaRealizada) {
        this.code_enc = encomendaRealizada.getCode_enc();
        this.code_utilizador = encomendaRealizada.getCode_utilizador();
        this.loja_onde_comprou = encomendaRealizada.getLoja_onde_comprou();
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
     * @param code_utilizador Código de utilizador.
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
     * Método que devolve a data de entrega de uma encomanda.
     * @return Data de entrega.
     */
    public LocalDateTime getData_entrega() {
        return data_entrega;
    }

    /**
     * Método que define a data de entrega.
     * @param data_entrega Data de entrega.
     */
    public void setData_entrega(LocalDateTime data_entrega) {
        this.data_entrega = data_entrega;
    }

    /**
     * Método que copia uma EncomendaRealizada por um Voluntario.
     * @return Cópia da EncomendaRealizadaVoluntario.
     */
    public EncomendaRealizadaVoluntario clone() {
        return new EncomendaRealizadaVoluntario(this);
    }

}
