package app.models;

public class Utilizador extends Info {

    // #region variables

    // #endregion

    // #region Construtores

    /**
    *
    */
    private static final long serialVersionUID = 3222805074253022629L;

    public Utilizador() {
        super();
    }

    /**
     * @param email
     * @param password
     * @param nome
     * @param localizacao
     */
    public Utilizador(String email, String password, String nome, Localizacao localizacao) {
        super(email, password, nome, localizacao);
    }

    /**
     * @param u
     */
    public Utilizador(Utilizador u) {
        super(u);
    }
    // #endregion

    // #region getters setters

    // #endregion

    // #region Overrides

    @Override
    public boolean equals(Object o) {
        // super check
        return super.equals(o);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public Utilizador clone() {
        return new Utilizador(this);
    }
    // #endregion

    // #region Methods

    // #endregion

}
