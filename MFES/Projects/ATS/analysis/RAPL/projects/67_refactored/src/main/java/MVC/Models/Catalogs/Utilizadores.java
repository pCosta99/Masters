package MVC.Models.Catalogs;

import MVC.Models.BaseModels.Utilizador;

import java.io.Serializable;
import java.util.*;

/**
 * Class para guardar todos os Utilizadores
 *
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */

public class Utilizadores implements Serializable {
    private Map<String,Utilizador> dataUtilizadores;

    /**
     * Constructor de Utilizadores por defeito.
     */
    public Utilizadores(){
        this.dataUtilizadores = new HashMap<>();
    }

    /**
     * Construtor de Utilizadores por Cópia.
     * @param d Utilizadores a copiar.
     */
    public Utilizadores(Utilizadores d){
        setDataUtilizadores(d.getDataUtilizadores());
    }

    /**
     * Método que define o Catálogo de Utilizador.
     * @param data Catálogo de Utilizador.
     */
    public void setDataUtilizadores(Map<String,Utilizador> data){
        this.dataUtilizadores = new HashMap<>();
        data.forEach((key, value) -> this.dataUtilizadores.put(key, value.clone()));
    }

    /**
     * Método que retorna uma cópia de Catálogo de Utilizador.
     * @return Cópia de Catálogo de Utilizador.
     */
    public Map<String,Utilizador> getDataUtilizadores(){
        Map<String, Utilizador> r = new HashMap<>();
        for (Map.Entry<String,Utilizador> e : this.dataUtilizadores.entrySet())
            r.put(e.getKey(), e.getValue().clone());
        return r;
    }

    /**
     * Adiciona um Utilizador ao Catálogo de Utilizador.
     * @param u Utilizador a adicionar.
     */
    public void addUtilizador(Utilizador u){
        this.dataUtilizadores.put(u.getCod(), u.clone());
    }

    /**
     * Verificar se existe um Utilizador no Catálogo de
     * @param cod Código de Utilizador.
     * @return True caso exista, false caso contrário.
     */
    public boolean existeUtilizador(String cod){
        return this.dataUtilizadores.containsKey(cod);
    }

    /**
     * Método que retorna um Utilizador com um determinado Código.
     * @param cod Código de Utilizador.
     * @return Utilizador
     */
    public Utilizador getUtilizador(String cod){
        return this.dataUtilizadores.get(cod);
    }

    /**
     * Adiciona uma Encomenda a um Utilizador.
     * @param codE Código da Encomenda.
     * @param codU Código do Utilizador.
     */
    public void addEncomendaUtilizador(String codE, String codU){
        this.dataUtilizadores.get(codU).addKeyPorAceitar(codE);
    }

    /**
     * Método em que um Utilizador aceita uma Encomenda.
     * @param codU Código do Utilizador.
     * @param cod Código da Encomenda.
     * @param b True caso aceite a Encomenda, false caso contrário.
     */
    public void aceitaEncomendaUtilizador(String codU, String cod,boolean b){
        Utilizador aux = this.dataUtilizadores.get(codU);
        aux.aceitaEncomenda(cod, b);
    }

    /**
     * Método em que um Utilizador classifica uma Encomenda.
     * @param codU Código do Utilizador.
     * @param cod Código da Encomenda.
     */
    public void classificaEncomenda(String codU,String cod){
        Utilizador aux = this.dataUtilizadores.get(codU);
        aux.classificaEncomenda(cod);
    }

    /**
     * Método toString.
     * @return String
     */
    public String toString(){
    StringBuilder sb = new StringBuilder();
    Collection<Utilizador> values = this.dataUtilizadores.values();
    sb.append("Utilizadores: \n").append(values);
    return sb.toString();
    }

    /**
     * Método Clone.
     * @return Utilizadores Clonado.
     */
    public Utilizadores clone(){
        return new Utilizadores(this);
    }

}