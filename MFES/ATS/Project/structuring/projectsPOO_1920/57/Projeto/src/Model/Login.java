/**
 * classe que representa um login
 */
package Model;

import java.io.Serializable;

public class Login implements Serializable {
    private String code;
    private String password;
    private String tipoConta;
    private String nome;

    public Login() {
        this.code = "";
        this.password = "";
        this.tipoConta = "";
        this.nome = "";
    }

    public Login(String code,String password,String tipoConta,String nome) {
        this.code = code;
        this.password = password;
        this.tipoConta = tipoConta;
        this.nome = nome;
    }

    public Login(Login l) {
        this.code = l.getCode();
        this.password = l.getPassword();
        this.tipoConta = l.getTipoConta();
        this.nome = l.getNome();
    }


    /**
     * devolve tipo de conta
     * @return tipo
     */
    public String getTipoConta() {
        return tipoConta;
    }

    /**
     * devolve pass
     * @return pass
     */
    public String getPassword() {
        return password;
    }

    /**
     * devolve loginCode
     * @return loginCode
     */
    public String getCode() {
        return code;
    }

    /**
     * devolve nome
     * @return nome
     */
    public String getNome() {
        return nome;
    }

    /**
     * altera pass
     * @param password pass
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * altera loginCode
     * @param code loginCode
     */
    public void setCode(String code) {
        this.code = code;
    }

    /**
     * altera tipo de conta
     * @param tipoConta tipo
     */
    public void setTipoConta(String tipoConta) {
        this.tipoConta = tipoConta;
    }

    /**
     * altera nome
     * @param nome nome
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    public Login clone() {
        return new Login(this);
    }

    @Override
    public String toString() {
        return "Login{" +
                "code='" + code + '\'' +
                ", password='" + password + '\'' +
                ", tipoConta='" + tipoConta + '\'' +
                ", nome='" + nome + '\'' +
                '}';
    }
}
