package app.models;

import app.enums.EstadosTransportadorEnum;

public class Voluntario extends Transportador {

    // #region variables
    // #endregion

    /**
    *
    */
    private static final long serialVersionUID = -7066446778591698764L;

    // #region Construtores
    public Voluntario() {
        super();
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
    public Voluntario(String email, String password, String nome, Localizacao localizacao,
            EstadosTransportadorEnum estado, boolean aceitaMed, double raioAccao,
            double velocidadeMedia) {
        super(email, password, nome, localizacao, estado, aceitaMed, raioAccao, velocidadeMedia);
    }

    /**
     * @param v
     */
    public Voluntario(Voluntario v) {
        super(v);
    }
    // #endregion

    // #region getters setter
    // #endregion

    // #region Overrrides

    @Override
    public Voluntario clone() {
        return new Voluntario(this);
    }
    // #endregion

    // #region Methods
    // #endregion

}
