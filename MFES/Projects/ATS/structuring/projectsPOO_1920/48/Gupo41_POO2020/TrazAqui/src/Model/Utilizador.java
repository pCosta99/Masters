package Model;

import java.util.Set;
import java.util.TreeSet;

public class Utilizador extends AtorSistema implements java.io.Serializable{

    /**
     * Construtor parametrizado
     * @param cod
     * @param email
     * @param nif
     * @param nome
     * @param password
     * @param localizacao
     */
    public Utilizador(String cod, String email, String nif, String nome, String password, Coordenadas localizacao){
        super(cod, email, nif, nome, password, localizacao);
    }

    /**
     * Construtor parametrizado
     * @param cod
     * @param email
     * @param nif
     * @param nome
     * @param password
     * @param localizacao
     * @param encomendas
     */
    public Utilizador(String cod, String email, String nif, String nome, String password, Coordenadas localizacao, Set<String> encomendas){
        super(cod, email, nif, nome, password, localizacao, encomendas);
    }

    /**
     * Construtor por cópia
     * @param utilizador
     */
    public Utilizador(Utilizador utilizador){
        super(utilizador);
    }


    /**
     * Cria uma cópia da instância
     * @return
     */
    @Override
    public Utilizador clone() {
        return new Utilizador(this);
    }

    @Override
    public boolean equals(Object o) {
        return super.equals(o);
    }

    /**
     * Método que adiciona o código de uma encomenda a
     * uma coleção de encomendas
     * @param encomenda
     */
    public void addEncomenda(String encomenda){
        super.addEncomenda(encomenda);
    }


}
