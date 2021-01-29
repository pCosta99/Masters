package app.models;

import java.util.Objects;

import app.enums.EstadosTransportadorEnum;
import app.interfaces.ITransportador;

public abstract class Transportador extends Info implements ITransportador {

    // #region variables

    /**
    *
    */
    private static final long serialVersionUID = -4562620774113486492L;
    private EstadosTransportadorEnum estado;
    private boolean aceitaMed;
    private double raioAccao;
    private double velocidadeMedia;

    // #endregion

    // #region Construtores

    public Transportador() {
        super();
        this.estado = EstadosTransportadorEnum.OCUPADO;
        this.aceitaMed = false;
        this.raioAccao = 0;
        this.velocidadeMedia = 0;
    }

    /**
     * @param email
     * @param password
     * @param nome
     * @param localizacao
     * @param estado
     * @param aceitaMed
     * @param raioAccao
     * @param velocidadeMedia
     */
    public Transportador(String email, String password, String nome, Localizacao localizacao,
            EstadosTransportadorEnum estado, boolean aceitaMed, double raioAccao,
            double velocidadeMedia) {
        super(email, password, nome, localizacao);
        this.estado = estado;
        this.aceitaMed = aceitaMed;
        this.raioAccao = raioAccao;
        this.velocidadeMedia = velocidadeMedia;
    }

    /**
     * @param T
     */
    public Transportador(Transportador t) {
        super(t);
        this.estado = t.estado;
        this.aceitaMed = t.aceitaMed;
        this.raioAccao = t.raioAccao;
        this.velocidadeMedia = t.velocidadeMedia;
    }

    // #endregion

    // #region getters setter

    /**
     * @return the estado
     */
    public EstadosTransportadorEnum getEstado() {
        return estado;
    }

    /**
     * @return the raioAccao
     */
    public double getRaioAccao() {
        return raioAccao;
    }

    /**
     * @param raioAccao the raioAccao to set
     */
    public void setRaioAccao(double raioAccao) {
        this.raioAccao = raioAccao;
    }

    /**
     * @return the aceitaMed
     */
    public boolean getAceitaMed() {
        return aceitaMed;
    }

    /**
     * @param aceitaMed the aceitaMed to set
     */
    public void setAceitaMed(boolean aceitaMed) {
        this.aceitaMed = aceitaMed;
    }

    /**
     * @param estado the estado to set
     */
    public void setEstado(EstadosTransportadorEnum estado) {
        this.estado = estado;
    }

    /**
     * @return the velocidadeMedia
     */
    public double getVelocidadeMedia() {
        return velocidadeMedia;
    }

    /**
     * @param velocidadeMedia the velocidadeMedia to set
     */
    public void setVelocidadeMedia(double velocidadeMedia) {
        this.velocidadeMedia = velocidadeMedia;
    }

    // #endregion

    // #region Overrrides

    @Override
    public boolean equals(Object o) {
        // super check
        if (!super.equals(o)) {
            return false;
        }
        Transportador transportador = (Transportador) o;
        // field comparison
        return Objects.equals(this.estado, transportador.estado)
                && Objects.equals(this.aceitaMed, transportador.aceitaMed)
                && Objects.equals(this.raioAccao, transportador.raioAccao)
                && Objects.equals(this.velocidadeMedia, transportador.velocidadeMedia);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Estado: ");
        sb.append(this.estado.name());
        sb.append('\n');
        sb.append("Licença Médica: ");
        sb.append(this.aceitaMed);
        sb.append('\n');
        sb.append("Raio de Acção: ");
        sb.append(this.raioAccao);
        sb.append('\n');
        sb.append("Velocidade Média: ");
        sb.append(this.velocidadeMedia);
        sb.append('\n');
        return sb.toString();
    }

    @Override
    public boolean aceitoTransporteMedicamentos() {
        return this.getAceitaMed();
    }

    @Override
    public void aceitaMedicamentos(boolean state) {
        this.setAceitaMed(state);
    }

    // #endregion

    // #region Methods

    public boolean dentroRaioAccao(Localizacao l) {
        return this.getLocalizacao().distancia(l) <= this.raioAccao;
    }

    // #endregion

}
