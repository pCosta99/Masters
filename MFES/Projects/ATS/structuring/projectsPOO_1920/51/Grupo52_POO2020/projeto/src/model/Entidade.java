package model;

import java.io.Serializable;
import java.time.LocalDateTime;

public abstract class Entidade implements Serializable {
    private String password;
    private String email;
    private String nome;
    private GPS gps;

    /**
     * Construtor parametrizado de um Entidade.
     * Aceita como parâmetros cada componente necessária.
     */

    public Entidade( String email, String password, String nome, GPS gps) {
        this.email = email;
        this.password = password;
        this.nome = nome;
        this.gps = gps;
    }
    /**
     * Construtor parametrizado de uma Entidade.
     * Necessário para a criação das diferentes entidades através da leitura do ficheiro de logs.
     */
    public Entidade(String cod , String nome, GPS gps){
        this.nome=nome;
        this.email = cod + "@email.com";
        this.nome=nome;
        this.password = cod.substring(1);
        this.gps = gps;
    }

    /**
     * Construtor de cópia de uma Entidade.
     * Aceita como parâmetro uma Entidade e utiliza os métodos de acesso aos valores das variáveis de instância.
     */
    public Entidade(Entidade entidade) {
        this.email = entidade.getEmail();
        this.nome = entidade.getNome();
        this.password = entidade.getPassword();
        this.gps = entidade.getGps().clone();
    }

    /**
     * Método de instância (get).
     * Devolve o email associado ao Ator.
     *
     * @return email do Ator.
     */
    public String getEmail() {
        return this.email;
    }

    /**
     * Método de instância (set).
     * Atualiza o email do Ator.
     *
     * @param email novo email do Ator.
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Método de instância (get).
     * Devolve a password associada ao Ator.
     *
     * @return password do Ator.
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Método de instância (set).
     * Atualiza a password do Ator.
     *
     * @param password nova password do Ator.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Método de instância (get).
     * Devolve o nome associado ao Ator.
     *
     * @return nome do Ator.
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * Método de instância (set).
     * Atualiza o email do utilizador.
     *
     * @param nome novo nome do Ator.
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Método de instância (get).
     * Devolve o gps associado ao Ator.
     *
     * @return GPS do Ator.
     */
    public GPS getGps() {
        return this.gps;
    }

    /**
     * Método de instância (set).
     * Atualiza o gps do Ator.
     *
     * @param gps novo GPS do utilizador.
     */
    public void setGps(GPS gps) {
        this.gps = gps;
    }


    /**
     * Método que devolve a representação em String do Ator.
     * @return String com toda a informação do Ator.
     */
    public String toString(){
        return "Nome: " + this.nome +
                "\nE-mail: " + this.email +
                "\nPassword: " + this.password +
                "\nGPS: " + this.gps.toString();
    }

    /**
     * Método que compara dois objetos.
     * @return booleano que dá verdadeiro se forem iguais.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || this.getClass() != o.getClass()) return false;
        Entidade entidade = (Entidade) o;
        return (this.email.equals(entidade.getEmail()) &&
                this.nome.equals(entidade.getNome()) &&
                this.password.equals(entidade.getPassword()) &&
                this.gps.equals(entidade.getGps()));
    }

    /**
     * Método que cria uma cópia de uma Entidade passada como parâmetro.
     * @return clone da Entidade.
     */
    public abstract Entidade clone();
}

