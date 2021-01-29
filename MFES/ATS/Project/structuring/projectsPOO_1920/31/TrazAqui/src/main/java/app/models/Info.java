package app.models;

import java.util.Objects;

import app.exceptions.PasswordErradaException;
import app.interfaces.IInfo;

public class Info implements IInfo, Comparable<Info> {

    // #region variables

    /**
    *
    */
    private static final long serialVersionUID = 4375088766723383920L;
    private String email;
    private String password;
    private Localizacao localizacao;
    private String nome;
    private static String errorMessagePasswordErrada = "Password Inválida !!\n";
    // #endregion

    // #region Construtores

    public Info() {
        this.email = "";
        this.password = "";
        this.nome = "";
        this.localizacao = new Localizacao();
    }

    /**
     * @param email
     * @param password
     * @param nome
     * @param localizacao
     */
    public Info(String email, String password, String nome, Localizacao localizacao) {
        this.email = email;
        this.password = password;
        this.nome = nome;
        this.setLocalizacao(localizacao);
    }

    /**
     * @param i
     */
    public Info(Info i) {
        this.email = i.email;
        this.password = i.password;
        this.nome = i.nome;
        this.setLocalizacao(i.localizacao);
    }

    // #endregion

    // #region getters setter

    /**
     * @return the email
     */
    public String getEmail() {
        return this.email;
    }

    /**
     * @param password the password to set
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * @return the localizacao
     */
    public Localizacao getLocalizacao() {
        return this.localizacao;
    }

    /**
     * @param localizacao the localizacao to set
     */
    public void setLocalizacao(Localizacao localizacao) {
        this.localizacao = localizacao;
    }

    /**
     * @param email the email to set
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * @return the nome
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * @param nome the nome to set
     */
    public void setNome(String nome) {
        this.nome = nome;
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
        Info info = (Info) o;
        // field comparison
        return Objects.equals(this.email, info.email)
                && Objects.equals(this.password, info.password)
                && Objects.equals(this.nome, info.nome)
                && Objects.equals(this.localizacao, info.localizacao);
    }

    @Override
    public int hashCode() {
        return this.toString().hashCode();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Email: ");
        sb.append(this.email);
        sb.append('\n');
        sb.append("Password: ");
        sb.append(this.password.hashCode()); // vai mostrar um numero
        sb.append('\n');
        sb.append("Nome: ");
        sb.append(this.nome);
        sb.append('\n');
        sb.append(this.localizacao.toString());
        return sb.toString();
    }

    public Info clone() {
        return new Info(this);
    }

    @Override
    public int compareTo(Info p) {
        return this.email.compareTo(p.email);
    }
    // #endregion

    // #region Methods

    // metodo para verificar se string passada por parametro é igual a password

    public boolean passOK(String pass) throws PasswordErradaException {
        boolean igual = this.password.equals(pass);

        if (!igual) {
            throw new PasswordErradaException(errorMessagePasswordErrada);
        }
        return igual;

    }


    // #endregion

}
