package app.models;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Objects;

import app.enums.EstadosEncomendaEnum;

public class RegistoEncomenda implements Comparable<RegistoEncomenda>, Serializable {

    // #region variables

    /**
    *
    */
    private static final long serialVersionUID = -5517986941561036979L;
    private Encomenda encomenda;
    private EstadosEncomendaEnum estadoEncomenda;
    private String codTransportador;
    private int classificacao;
    private LocalDateTime dataAguardaEnvio;
    private LocalDateTime dataEnviada;
    private LocalDateTime dataEntregue;
    private double preco;

    // #endregion

    // #region Construtores

    public RegistoEncomenda() {
        this.encomenda = new Encomenda();
        this.estadoEncomenda = EstadosEncomendaEnum.ABERTA;
        this.codTransportador = "";
        this.classificacao = -1;
        this.dataAguardaEnvio = null;
        this.dataEnviada = null;
        this.dataEntregue = null;
        this.preco = 0;

    }

    /**
     * @param encomenda
     * @param estadoEncomenda
     * @param codTransportador
     * @param classificacao
     * @param dataAguardaEnvio
     * @param dataEnviada
     * @param dataEntregue
     * @param preco
     */
    public RegistoEncomenda(Encomenda encomenda, EstadosEncomendaEnum estadoEncomenda,
            String codTransportador, int classificacao, LocalDateTime dataAguardaEnvio,
            LocalDateTime dataEnviada, LocalDateTime dataEntregue, double preco) {
        this.encomenda = encomenda;
        this.estadoEncomenda = estadoEncomenda;
        this.codTransportador = codTransportador;
        this.classificacao = classificacao;
        this.dataAguardaEnvio = dataAguardaEnvio;
        this.dataEnviada = dataEnviada;
        this.dataEntregue = dataEntregue;
        this.preco = preco;
    }

    /**
     * @param r
     */
    public RegistoEncomenda(RegistoEncomenda r) {
        this.setEncomenda(r.encomenda);
        this.estadoEncomenda = r.estadoEncomenda;
        this.codTransportador = r.codTransportador;
        this.classificacao = r.classificacao;
        this.dataAguardaEnvio = r.dataAguardaEnvio;
        this.dataEnviada = r.dataEnviada;
        this.dataEntregue = r.dataEntregue;
        this.preco = r.preco;
    }

    // #endregion

    // #region getters setter

    /**
     * @return the encomenda
     */
    public Encomenda getEncomenda() {
        return encomenda.clone();
    }

    /**
     * @return the dataEntregue
     */
    public LocalDateTime getDataEntregue() {
        return dataEntregue;
    }

    /**
     * @param dataEntregue the dataEntregue to set
     */
    public void setDataEntregue(LocalDateTime dataEntregue) {
        this.dataEntregue = dataEntregue;
    }

    /**
     * @return the dataEnvida
     */
    public LocalDateTime getDataEnviada() {
        return dataEnviada;
    }

    /**
     * @param dataEnvida the dataEnvida to set
     */
    public void setDataEnviada(LocalDateTime dataEnvida) {
        this.dataEnviada = dataEnvida;
    }

    /**
     * @return the dataAguardaEnvio
     */
    public LocalDateTime getDataAguardaEnvio() {
        return dataAguardaEnvio;
    }

    /**
     * @param dataAguardaEnvio the dataAguardaEnvio to set
     */
    public void setDataAguardaEnvio(LocalDateTime dataAguardaEnvio) {
        this.dataAguardaEnvio = dataAguardaEnvio;
    }

    /**
     * @return the estadoEncomenda
     */
    public EstadosEncomendaEnum getEstadoEncomenda() {
        return estadoEncomenda;
    }

    /**
     * @param estadoEncomenda the estadoEncomenda to set
     */
    public void setEstadoEncomenda(EstadosEncomendaEnum estadoEncomenda) {
        this.estadoEncomenda = estadoEncomenda;
    }

    /**
     * @return the classificacao
     */
    public int getClassificacao() {
        return classificacao;
    }

    /**
     * @param classificacao the classificacao to set
     */
    public void setClassificacao(int classificacao) {
        this.classificacao = classificacao;
    }

    /**
     * @return the codTransportador
     */
    public String getCodTransportador() {
        return codTransportador;
    }

    /**
     * @param codTransportador the codTransportador to set
     */
    public void setCodTransportador(String codTransportador) {
        this.codTransportador = codTransportador;
    }

    /**
     * @param encomenda the encomenda to set
     */
    public void setEncomenda(Encomenda encomenda) {
        this.encomenda = encomenda.clone();
    }

    /**
     * @return the preco
     */
    public double getPreco() {
        return preco;
    }

    /**
     * @param preco the preco to set
     */
    public void setPreco(double preco) {
        this.preco = preco;
    }

    // #endregion

    // #region Overrrides

    @Override
    public boolean equals(Object o) {
        // self check
        if (this == o) {
            return true;
        }
        // null check
        if (o == null) {
            return false;
        }
        // type check and cast
        if (getClass() != o.getClass()) {
            return false;
        }
        RegistoEncomenda registo = (RegistoEncomenda) o;
        // field comparison
        return Objects.equals(this.encomenda, registo.encomenda)
                && Objects.equals(this.estadoEncomenda, registo.estadoEncomenda)
                && Objects.equals(this.codTransportador, registo.codTransportador)
                && Objects.equals(this.classificacao, registo.classificacao)
                && Objects.equals(this.dataAguardaEnvio, registo.dataAguardaEnvio)
                && Objects.equals(this.dataEnviada, registo.dataEnviada)
                && Objects.equals(this.dataEntregue, registo.dataEntregue)
                && Objects.equals(this.preco, registo.preco);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda: ");
        sb.append(this.encomenda.toString());
        sb.append('\n');
        sb.append("Estado de encomenda: ");
        sb.append(this.estadoEncomenda.name());
        sb.append('\n');
        sb.append("Classificação: ");
        sb.append(this.classificacao);
        sb.append('\n');
        sb.append("Data Aguarda Envio: ");
        sb.append(this.dataAguardaEnvio);
        sb.append('\n');
        sb.append("Data Enviada: ");
        sb.append(this.dataEnviada);
        sb.append('\n');
        sb.append("Data Entregue: ");
        sb.append(this.dataEntregue);
        sb.append('\n');
        sb.append("Preço: ");
        sb.append(this.preco);
        sb.append('\n');
        return sb.toString();
    }

    public RegistoEncomenda clone() {
        return new RegistoEncomenda(this);
    }

    @Override
    public int compareTo(RegistoEncomenda re) {
        return this.encomenda.getCodEnc().compareTo(re.encomenda.getCodEnc());
    }

    // #endregion

    // #region Methods
    // #endregion

}
