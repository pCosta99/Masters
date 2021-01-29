package Model;

import java.io.Serializable;

/**
 * Classe que define um login
 */
public class Login implements Serializable {

    private String email;
    private String password;
    private String codigo;

    /**
     * Construtor da classe
     */
    public Login(){
        this.email = "";
        this.password = "";
        this.codigo = "";
    }

    /**
     * Construtor da classe
     * @param email O email do login
     * @param password A password do login
     * @param codigo O código do login
     */
    public Login(String email, String password, String codigo){
        this.email = email;
        this.password = password;
        this.codigo = codigo;
    }

    /**
     * Construtor da classe
     * @param l Um login de um login
     */
    public Login(Login l){
        this.email = l.getEmail();
        this.password = l.getPassword();
        this.codigo = l.getCodigo();
    }

    /**
     * Indica o email de um login
     * @return O email do login
     */
    public String getEmail(){
        return this.email;
    }

    /**
     * Define o email de um login
     * @param email O email do login
     */
    public void setEmail(String email){
        this.email = email;
    }

    /**
     * Indica a password de um login
     * @return A password de um login
     */
    public String getPassword(){
        return this.password;
    }

    /**
     * Define a password de um login
     * @param pass A password do login
     */
    public void setPassword(String pass){
        this.password = pass;
    }

    /**
     * Indica o código de um login
     * @return O código do login
     */
    public String getCodigo(){
        return this.codigo;
    }

    /**
     * Define o código de um login
     * @param cod O código do login
     */
    public void setCodigo(String cod){
        this.codigo = cod;
    }

    /**
     * Indica se um dado objeto é igual a um login
     * @param o O objeto com o qual se pretende comparar a loja
     * @return True caso afirmativo e false caso contrário
     */
    public boolean equals(Object o){
        if(this == o) return true;
        if((o == null) || (this.getClass() != o.getClass())) return false;
        Login t = (Login) o;
        return this.email.equals(t.getEmail())
                && this.password.equals(t.getPassword())
                && this.codigo.equals(t.getCodigo());
    }

    /**
     * Coloca as informações de um login numa string
     * @return A string com as informações do login
     */
    public String toSrting(){
        StringBuffer sb = new StringBuffer();
        sb.append("Código: ").append(this.codigo).append("; ")
                .append("Email: ").append(this.email).append("; ")
                .append("Password: ").append(this.password).append("; ");
        return sb.toString();
    }

    /**
     * Cria um clone de um login
     * @return O clone do login
     */
    public Login clone(){
        return new Login(this);
    }

    /**
     * Verifica se as credenciais inseridas são iguais às do login
     * @param email O email inserido
     * @param password A password inserida
     * @return True caso afirmativo e false caso negativo
     */
    public boolean checksLoginEmail(String email, String password){
        return this.email.equals(email) && this.password.equals(password);
    }

}
