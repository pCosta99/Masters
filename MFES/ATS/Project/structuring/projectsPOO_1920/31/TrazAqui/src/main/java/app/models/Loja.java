package app.models;

import java.util.Objects;

public class Loja extends Info {

    /**
    *
    */
    private static final long serialVersionUID = -8690893597446009504L;
    // #region variables
    private int filaEspera;
    // #endregion

    // #region Construtores

    public Loja() {
        this.filaEspera = -1;
    }

    /**
     * @param email
     * @param password
     * @param nome
     * @param localizacao
     * @param filaEspera
     */
    public Loja(String email, String password, String nome, Localizacao localizacao,
            int filaEspera) {
        super(email, password, nome, localizacao);
        this.filaEspera = filaEspera;
    }

    /**
     * @param l
     */
    public Loja(Loja l) {
        super(l);
        this.filaEspera = l.filaEspera;
    }

    // #endregion

    // #region getters setter

    /**
     * @return the filaEspera
     */
    public boolean temFilaEspera() {
        return filaEspera != -1;
    }

    /**
     * @return the filaEspera
     */
    public int getFilaEspera() {
        return filaEspera;
    }

    /**
     * @param filaEspera the filaEspera to set
     */
    public void setFilaEspera(int filaEspera) {
        this.filaEspera = filaEspera;
    }
    // #endregion

    // #region Overrrides

    @Override
    public boolean equals(Object o) {
        // super check
        if (!super.equals(o)) {
            return false;
        }
        Loja loja = (Loja) o;
        // field comparison
        return Objects.equals(this.filaEspera, loja.filaEspera);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Fila de Espera: ");
        sb.append(this.filaEspera);
        sb.append('\n');
        return sb.toString();
    }

    @Override
    public Loja clone() {
        return new Loja(this);
    }
    // #endregion

    // #region Methods
    // #endregion

}
