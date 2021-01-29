package MVC.Models.BaseModels;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Abstract class User:
 * Class criada para ser a fundação de todos os tipos de utilizadores nesta API.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */
public abstract class User implements Serializable {
    private String cod;
    private String pass;
    private String name;
    private GPS gps;
    private List<String> codencomendas;

    /**
     * Construtor de User por Defeito.
     */
    public User(){
        this.cod = "";
        this.name = "";
        this.pass = "";
        this.gps = new GPS();
        this.codencomendas = new ArrayList<>();
    }

    /**
     * Construtor de User parametrizado.
     * @param c Código do User.
     * @param n Nome do User.
     * @param x Coordenada X da Localização do User.
     * @param y Coordenada Y da Localização do User.
     */
    public User(String c, String n, double x, double y) {
        this.cod = c;
        this.name = n;
        this.pass = c;
        this.gps = new GPS(x, y);
        this.codencomendas = new ArrayList<>();
    }

    /**
     * Construtor de User parametrizado.
     * @param c Código do User.
     * @param n Nome do User.
     * @param g GPS do User.
     */
    public User(String c, String n, GPS g){
        this.cod = c;
        this.name = n;
        this.pass = c;
        this.gps = g.clone();
        this.codencomendas = new ArrayList<>();
    }

    /**
     * Construtor de User de Cópia.
     * @param u User a copiar.
     */
    public User(User u){
        this.cod = u.getCod();
        this.name = u.getName();
        this.gps = u.getGPS();
        this.codencomendas = u.getCodencomendas();
        this.pass = u.getPass();
    }

    /**
     * Método que retorna o Código do User.
     * @return Código do User.
     */
    public String getCod() {
        return this.cod;
    }

    /**
     * Método que retorna o Nome do User.
     * @return Nome do User.
     */
    public String getName() {
        return this.name;
    }

    /**
     * Método que retorna o GPS do User.
     * @return GPS do User.
     */
    public GPS getGPS() {
        return this.gps.clone();
    }

    /**
     * Método que retorna a Password do Login do User.
     * @return Password do User.
     */
    public String getPass() {
        return pass;
    }

    /**
     * Método que retorna a Lista dos Códigos de Encomendas do User.
     * @return Lista dos Códigos de Encomendas.
     */
    public List<String> getCodencomendas() {
        return new ArrayList<>(this.codencomendas);
    }

    /**
     * Método que define o Código do User.
     * @param cod Código do User.
     */
    public void setCod(String cod) {
        this.cod = cod;
    }

    /**
     * Método que define a Password de Login do User.
     * @param pass Password do User.
     */
    public void setPass(String pass) {
        this.pass = pass;
    }

    /**
     * Método que define o Nome do User.
     * @param name Nome do User.
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Método que define o GPS do User.
     * @param gps GPS do User.
     */
    public void setGps(GPS gps) {
        this.gps = gps.clone();
    }

    /**
     * Método que define o GPS do User.
     * @param x Coordenada X do User.
     * @param y Coordenada Y do User.
     */
    public void setGps(double x, double y) {
        this.gps.setX(x);
        this.gps.setY(y);
    }

    /**
     * Método que adiciona o Código de uma Encomenda ao User.
     * @param s Código da Encomenda.
     */
    public void addEncomenda(String s){
        this.codencomendas.add(s);
    }

    /**
     * Método que verifica se a String inserida é a Password do User.
     * @param s Password.
     * @return True caso seja, false caso contrário.
     */
    public boolean isSenha(String s){
        return this.pass.equals(s);
    }

    /**
     * Método que determina a distância entre dois Users.
     * @param u User.
     * @return Distância resultante.
     */
    public double distanciaUser(User u){
        return this.gps.distancia(u.gps);
    }

    /**
     * Método Equals
     * @param o Object a comparar.
     * @return True caso seja igual, false caso contrário.
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof User)) return false;
        User user = (User) o;
        return getCod().equals(user.getCod());
    }

    /**
     * Método toString
     * @return String com os dados do User.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código: ").append(this.getCod())
        .append("\nNome: ").append(this.getName()).append("\n").append(getGPS());
        return sb.toString();
    }
}