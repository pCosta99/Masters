package Model;

import java.io.Serializable;
import java.util.Objects;

/**
 * Classe que define um user do programa
 */
public class LocalCodeName implements Serializable {

    private String code;
    private String nome;
    private GPS gps;

    /**
     * Construtor da classe
     */
    public LocalCodeName() {
        this.nome = "";
        this.code = "";
        this.gps = new GPS();
    }

    /**
     * Construtor de classe
     *
     * @param code O código do user
     * @param nome O nome do user
     * @param gps  As coordenadas GPS do user
     */
    public LocalCodeName(String code, String nome, GPS gps) {
        this.nome = nome;
        this.code = code;
        this.gps = gps;
    }

    /**
     * Construtor da classe
     *
     * @param l O user do qual se pretende extrair a informação
     */
    public LocalCodeName(LocalCodeName l) {
        this.nome = l.getNome();
        this.code = l.getCode();
        this.gps = l.getGps();
    }

    /**
     * Define o nome do user
     *
     * @param nome O nome a definir
     */
    public void setNome(String nome) {
        this.nome = nome;
    }

    /**
     * Define o código do user
     *
     * @param code O código do user a definir
     */
    public void setCode(String code) {
        this.code = code;
    }

    /**
     * Define as coordenadas GPS de um user
     *
     * @param gps
     */
    public void setGps(GPS gps) {
        this.gps = gps.clone();
    }

    /**
     * Indica o nome de um user
     *
     * @return O nome do user
     */
    public String getNome() {
        return nome;
    }

    /**
     * Indica o código de um user
     *
     * @return O código de um user
     */
    public String getCode() {
        return code;
    }

    /**
     * Indica as coordenadas GPS de um user
     *
     * @return As coordenadas GPS de um user
     */
    public GPS getGps() {
        return gps;
    }

    /**
     * Verifica se um dado objeto é igual a um user
     *
     * @param o O objeto com o qual se pretende comparar
     * @return True caso sejam iguais e false caso contrário
     */
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof LocalCodeName)) return false;
        LocalCodeName that = (LocalCodeName) o;
        return getCode().equals(that.getCode());
    }

    /**
     * Cria um clone de um user
     *
     * @return O clone do user
     */
    public LocalCodeName clone() {
        return new LocalCodeName(this);
    }

    /**
     * Transforma um user numa String
     *
     * @return A String correspondente ao user
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getClass().getSimpleName());
        sb.append("{code='");
        sb.append(code);
        sb.append(", nome='");
        sb.append(nome);
        sb.append(", gps=");
        sb.append(gps);
        sb.append("}");
        return sb.toString();
    }
}
