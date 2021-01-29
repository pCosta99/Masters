package Models;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Utilizador implements Serializable {

    private String id;
    private String nome;
    private GPS g;
    private List<Encomenda> encomendas;
    private String email;
    private String password;

    /**
     * Construtor por omissão.
     */
    public Utilizador() {
        this.id = "";
        this.nome = "";
        this.g = new GPS();
        this.encomendas = new ArrayList<>();
        this.email = "";
        this.password = "";
    }

    /**
     * Construtor parametrizado.
     *
     * @param id String correspondente ao código de um Utilizador.
     * @param n String que corresponde ao nome de um Utilizador.
     * @param g objeto da classe GPS corresponde às coordenadas.
     * @param encomendas lista de encomendas.
     * @param email String representa um email.
     * @param password String representa uma password.
     */
    public Utilizador(String id, String n, GPS g,List<Encomenda> encomendas,String email, String password) {
        this.id = id;
        this.nome = n;
        this.g = new GPS(g);
        this.encomendas = new ArrayList<>();
        for(Encomenda e : encomendas)
            this.encomendas.add(e);
        this.email = email;
        this.password = password;
    }

    /**
     * Construtor por cópia.
     *
     * @param u Recebe um objeto da classe Utilizador.
     */
    public Utilizador(Utilizador u) {
        this.id = u.getId();
        this.nome = u.getNome();
        this.g = new GPS(u.getGps());
        this.encomendas = u.getEncomendas();
        this.email =  u.getEmail();
        this.password = u.getPassword();
    }

    /**
     * Método que dá o código de um Utilizador.
     *
     * @return String que representa o código.
     */
    public String getId() {
        return this.id;
    }

    /**
     * Método que define o código de um Utilizador.
     *
     * @param id String representante do código de um Utilizador.
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * Método que dá o nome de um Utilizador.
     *
     * @return Devolve uma String que representa o nome.
     */
    public String getNome() {
        return this.nome;
    }

    /**
     * Método que define o nome de um Utilizador.
     *
     * @param n String representante do nome.
     */
    public void setNome(String n) {
        this.nome = n;
    }

    /**
     * Método que dá as coordenadas de um Utilizador.
     *
     * @return Devolve GPS que corrresponde às coordenadas.
     */
    public GPS getGps() {
        return this.g;
    }

    /**
     * Método que define as coordenadas de um Utilizador.
     *
     * @param g Recebe um objeto da classe GPS.
     */
    public void setGps(GPS g) {
        this.g = g;
    }

    /**
     * Método que dá a lista de Encomendas.
     * @return Devolve a lista.
     */
    public List<Encomenda> getEncomendas() {
        List<Encomenda> ret = new ArrayList<>();
        for(Encomenda e : ret)
            ret.add(e);
        return ret;
    }

    /**
     * Método que dá o email.
     * @return Devolve o email.
     */
    public String getEmail() {
        return this.email;
    }

    /**
     * Método que dá a password.
     * @return Devolve a password.
     */
    public String getPassword() {
        return this.password;
    }

    /**
     * Método que define o email.
     * @param email Recebe um email.
     */
    public void setEmail(String email) {
        this.email = email;
    }

    /**
     * Método que define uma password.
     * @param password Recebe uma password.
     */
    public void setPassword(String password) {
        this.password = password;
    }

    /**
     * Método que define a lista de Encomendas.
     * @param encomendas Recebe a lista.
     */
    public void setEncomendas(List<Encomenda> encomendas) {
        this.encomendas = encomendas;
    }

    /**
     * Função que verifica se o objeto recebido é idêntico ao da classe Utilizador.
     *
     * @param o Recebe o objeto.
     * @return Devolve um boolean que corresponde à verificação.
     */
    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Utilizador u = (Utilizador) o;
        return u.getId().equals(this.id) &&
                u.getNome().equals(this.nome) &&
                u.getGps().equals(this.g) &&
                u.getEncomendas().equals(this.encomendas) &&
                u.getEmail().equals(this.email) &&
                u.getPassword().equals(this.password);
    }

    /**
     * Função que traduz a classe Utilizador.
     *
     * @return Devolve uma String que representa a tradução.
     */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código de utilizador:  ").append(this.id)
                .append("\nNome do utlizador:  ").append(this.nome)
                .append("\nGPS:  ").append(this.g)
                .append("\nLista de Encomendas:  ").append(this.encomendas);
        return sb.toString();
    }

    /**
     * Função que faz um clone da classe Utilizador.
     *
     * @return Devolve esse clone.
     */
    @Override
    public Utilizador clone() {
        return new Utilizador(this);
    }

    /**
     * Função que adiciona uma encomenda à lista de encomendas.
     * @param e Recebe uma encomenda.
     */
    public void addEncomenda(Encomenda e) {
        this.encomendas.add(e);
    }

    public void leUtil(String cod, String[] p) {
        this.setId(cod);
        this.setNome(p[1]);
        this.g.setX(Double.parseDouble(p[2]));
        this.g.setY(Double.parseDouble(p[3]));
    }
}
