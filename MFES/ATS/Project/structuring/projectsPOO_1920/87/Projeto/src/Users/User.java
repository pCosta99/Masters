package Users;

import java.io.Serializable;

public class User implements Serializable {
    private String code;
    private String password;

    /**
     * Construtor por omissão.
     */
    public User() {
        this.code = "";
        this.password = "";
    }

    /**
     * Construtor por parâmeotros.
     * @param code Código do user.
     * @param pass Password do user.
     */
    public User(String code, String pass) {
        this.code = code;
        this.password = pass;
    }

    /**
     * Construtor por cópia.
     * @param u User a ser copiado.
     */
    public User(User u){
        this.code = u.getCode();
        this.password = u.getPassword();
    }

    /**
     * Método que devolve o código de um User.
     * @return Código do User.
     */
    public String getCode() {
        return code;
    }

    /**
     * Método que define o código de um User.
     * @param code Código do User.
     */
    public void setCode(String code) {
        this.code = code;
    }

    /**
     * Método que devolve a password de um User.
     * @return Password do User.
     */
    public String getPassword() {
        return password;
    }

    /**
     * Método que define a password de um User.
     * @param pass Password do User.
     */
    public void setPassword(String pass) {
        this.password = pass;
    }

    /**
     * Método que devolve a informação de um User como String.
     * @return String com a informação do User.
     */
    public String toString(){
        return this.code +":"+ this.password;
    }

    /**
     * Método que faz uma cópia de um User.
     * @return Cópia do User.
     */
    public User clone(){
        return new User(this);
    }

}
