package app.models;

import java.util.Objects;

import app.enums.EstadosTransportadorEnum;

public class EmpresaTransportadora extends Transportador {

    // #region variables

    /**
    *
    */
    private static final long serialVersionUID = -2918787625076544925L;
    private String nif;
    private double precoPorKM;

    // #endregion

    // #region Construtores
    public EmpresaTransportadora() {
        super();
        this.precoPorKM = 0;
        this.nif = "";

    }


    /**
     * @param email
     * @param password
     * @param nome
     * @param localizacao
     * @param estado
     * @param aceitaMed
     * @param precoPorKM
     * @param raioAccao
     * @param nif
     * @param velocidadeMedia
     */
    public EmpresaTransportadora(String email, String password, String nome,
            Localizacao localizacao, EstadosTransportadorEnum estado, boolean aceitaMed,
            double precoPorKM, double raioAccao, String nif, double velocidadeMedia) {
        super(email, password, nome, localizacao, estado, aceitaMed, raioAccao, velocidadeMedia);
        this.precoPorKM = precoPorKM;
        this.nif = nif;

    }

    /**
     * @param e
     */
    public EmpresaTransportadora(EmpresaTransportadora e) {
        super(e);
        this.precoPorKM = e.getPrecoPorKM();
        this.nif = e.getNIF();
    }
    // #endregion

    // #region getters setter

    /**
     * @return the precoPorKM
     */
    public double getPrecoPorKM() {
        return precoPorKM;
    }

    /**
     * @param precoPorKM the precoPorKM to set
     */
    public void setPrecoPorKM(double precoPorKM) {
        this.precoPorKM = precoPorKM;
    }

    /**
     * @return the nIF
     */
    public String getNIF() {
        return nif;
    }

    /**
     * @param nIF the nIF to set
     */
    public void setNIF(String nIF) {
        this.nif = nIF;
    }

    // #endregion

    // #region Overrrides

    @Override
    public boolean equals(Object o) {
        // super check
        if (!super.equals(o)) {
            return false;
        }
        EmpresaTransportadora transportador = (EmpresaTransportadora) o;
        // field comparison
        return Objects.equals(this.precoPorKM, transportador.precoPorKM)
                && Objects.equals(this.nif, transportador.nif);

    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("NIF: ");
        sb.append(this.nif);
        sb.append('\n');
        sb.append("Preço por Km: ");
        sb.append(this.precoPorKM);
        sb.append('\n');

        return sb.toString();
    }

    @Override
    public EmpresaTransportadora clone() {
        return new EmpresaTransportadora(this);
    }
    // #endregion

    // #region Methods

    public double precoTransporte(Localizacao loja, Localizacao cliente, double peso, int tempoFila,
            double velocidade) {
        double precoDistanciaLoja = this.getLocalizacao().distancia(loja) * this.precoPorKM;
        double precoDistanciaLojaCliente = loja.distancia(cliente) * this.precoPorKM;
        if (peso > 10) {
            precoDistanciaLojaCliente *= (peso * 0.1);
        } else {
            precoDistanciaLojaCliente *= (peso * 0.05);
        }
        double precoFilaEspera = tempoFila < 0 ? 0 : tempoFila * 0.005;

        double tempoDistanciaLoja =
                this.getLocalizacao().distancia(loja) / (this.getVelocidadeMedia() * velocidade);
        double tempoDistanciaLojaCliente =
                loja.distancia(cliente) / (this.getVelocidadeMedia() * velocidade);
        double precoTempoTransporte = (tempoDistanciaLoja + tempoDistanciaLojaCliente) * 60 * 0.05;
        return (precoDistanciaLoja + precoDistanciaLojaCliente + precoFilaEspera
                + precoTempoTransporte);

    }
    // #endregion

}
