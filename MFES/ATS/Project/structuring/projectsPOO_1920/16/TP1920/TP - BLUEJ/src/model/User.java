package src.model;

import java.io.Serializable;

/**
 * Classe que cria e regista atores do sistema TrazAqui
 */

public abstract class User implements Serializable {

    //Variáveis de Instância
    private String username;
    private String password;
    private String nome;
    private String email;
    private Ponto localizacao;

    /**
     * Construtores da classe model.User.
     * Declaração dos construtores por omissão (vazio),
     * parametrizado e de cópia.
     */

    /**
     * Construtor por omissão de model.User.
     */
    public User(){
        this.username = "";
        this.email = "";
        this.nome = "";
        this.password = "";
        this.localizacao = new Ponto();
    }

    /**
     * Construtor parametrizado de model.User.
     * Aceita como parâmetros os valores para cada variável de instância.
     */

    public User(String username, String password, String nome, String email, Ponto p){
        this.username = username;
        this.password = password;
        this.nome = nome;
        this.email = email;
        this.localizacao = p.clone();
    }

    /**
     * Construtor de cópia de model.User.
     * Aceita como parâmetro outro model.User e utiliza os métodos
     * de acesso aos valores das variáveis de instância.
     */

    public User(User u){
        this.username = u.getUsername();
        this.email = u.getEmail();
        this.nome = u.getNome();
        this.password = u.getPassword();
        this.localizacao = u.getLocalizacao();
    }

    /**
     * Métodos de Instância
     */

    /**
     * Devolve o nome do user
     * @return nome do user
     */

    public String getNome() {
        return this.nome;
    }

    /**
     * Devolve o email do user
     * @return email do user
     */

    public String getEmail() {
        return this.email;
    }

    /**
     * Devolve a password do user
     * @return password do user
     */

    public String getPassword() {
        return this.password;
    }

    /**
     * Devolve o username do user
     * @return username do user
     */

    public String getUsername() {
        return this.username;
    }


    /**
     * Devolve o ponto da localização do user
     * @return ponto da localização do user
     */
    public Ponto getLocalizacao() {
        return this.localizacao.clone();
    }


    /**
     * atualiza o ponto da localização do user
     * @param p novo ponto da localização do user
     */
    public void setLocalizacao(Ponto p){
        this.localizacao = p.clone();
    }

    /**
     * Atualiza o nome do user
     * @param nome novo nome do user
     */


    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Atualiza o email do user
     * @param email novo email do user
     */

    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Atualiza a password do user
     * @param password nova password do user
     */

    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Atualiza o username do user
     * @param username novo username do user
     */

    public void setUsername(String username) {
        this.username = username;
    }

    /**
     * Método que determina se um dado Object é igual ao model.User
     * @param o Objeto qualquer
     * @return true caso o seja igual ao model.User, false caso sejam diferentes
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        User user = (User) o;
        return this.username.equals(user.getUsername()) &&
                this.password.equals( user.getPassword()) &&
                this.nome.equals( user.getNome()) &&
                this.localizacao.equals(user.getLocalizacao()) &&
                this.email.equals(user.getEmail());
    }

    /**
     * Método que transforma um model.User numa string
     * @return string com a informação do model.User
     */

    public String toString() {
        final StringBuilder sb = new StringBuilder("model.User{");
        sb.append("username='").append(this.username).append('\'');
        sb.append(", password='").append(this.password).append('\'');
        sb.append(", nome='").append(this.nome).append('\'');
        sb.append(", email='").append(this.email).append('\'');
        sb.append(", localizacao='").append(this.localizacao.toString()).append('\'');
        sb.append('}');
        return sb.toString();
    }

    /**
     * Método que clona um model.User
     * Para tal invoca o construtor de cópia
     *
     * @return cópia de model.User
     */

    public abstract User clone();
}
