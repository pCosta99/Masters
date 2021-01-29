package TrazAqui;


import java.io.Serializable;

/**
 * Classe que deverá representar Qualquer tipo de User da aplicação TrazAqui: Voluntário, Loja, Utilizador, Empresa Transportadora.
 */
public abstract class Login implements Serializable {

    /**
     * String que representa o código de um User
     */
    private String codUser;
    /**
     * String que representa o nome completo de um User
     */
    private String username;
    /**
     * String que representa a palavra-passe de um User
     */
    private String password;
    /**
     * Coordenadas de localização de um User
     */
    private Ponto ponto;

    /**
     * Construtor vazio de um objeto Login
     */
    public Login(){
        this.codUser = "";
        this.username = "";
        this.password = "";
        this.ponto = new Ponto();
    }

    /**
     * Construtor parametrizado de um objeto Login
     * @param codUser Código no formato 'uxx' em que 'x' são inteiros de 0 a 9.
     * @param username Nome completo do User
     * @param password Palavra-Passe do User
     * @param ponto Localização do User
     */
    public Login(String codUser, String username, String password, Ponto ponto){
        this.codUser = codUser;
        this.username = username;
        this.password = password;
        this.ponto = ponto;
    }

    /**
     * Construtor de cópia de um objeto da classe Login.
     * @param l login a ser copiado
     */
    public Login(Login l){
        this.codUser = l.getCodUser();
        this.username = l.getUsername();
        this.password = l.getPassword();
        this.ponto = l.getPonto();
    }

    /**
     * Getter da variável codUser
     * @return codUser do objeto sob o qual o método é aplicado.
     */
    public String getCodUser() {
        return codUser;
    }

    /**
     * Setter da variável codUser
     * @param codUser para o qual se pretende alterar
     */
    public void setCodUser(String codUser) {
        this.codUser = codUser;
    }

    /**
     * Getter da variável username.
     * @return username do objeto sob o qual o método é aplicado.
     */
    public String getUsername() {
        return username;
    }

    /**
     * Setter da variável username
     * @param username para o qual se pretende alterar.
     */
    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * Getter da variável password.
     * @return password do objeto sob o qual o método é aplicado.
     */
    public String getPassword() {
        return password;
    }

    /**
     *  Setter da variável password.
     * @param password para o qual se pretende alterar.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Getter da variável ponto
     * @return ponto do objeto sob o qual o método é aplicado.
     */
    public Ponto getPonto() {
        return this.ponto.clone();
    }

    /**
     * Setter da variável ponto
     * @param ponto para o qual se pretende alterar.
     */
    public void setPonto(Ponto ponto) {
        this.ponto = ponto;
    }

    /**
     * Método abstrato para forçar a implementação nas subclases
     * @return clone de um Login
     */
    public abstract Login clone();

    /**
     * Converte todas as variáveis de instância de um objeto Login em Strings
     * @return String representativa de um objeto Login
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código: ").append(this.codUser).append("\n");
        sb.append("Username: ").append(this.username).append("\n");
        sb.append("Password: ").append(this.password).append("\n");
        sb.append("GPS: ").append(this.ponto.toString());
        return sb.toString();
    }

    /**
     * Verifica se um outro objeto é igual a um Login sob o qual o método é aplicado
     * @param object outro qualquer objeto a comparar
     * @return true caso sejam iguais, falso caso contrário
     */
    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        Login login = (Login) object;
        return java.util.Objects.equals(codUser, login.codUser) &&
                java.util.Objects.equals(username, login.username) &&
                java.util.Objects.equals(password, login.password) &&
                java.util.Objects.equals(ponto, login.ponto); // faz sentido retirar uma vez que os users podem mudar de sitio
    }

}



