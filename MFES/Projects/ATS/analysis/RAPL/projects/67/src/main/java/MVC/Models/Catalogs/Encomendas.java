package MVC.Models.Catalogs;

import MVC.Models.BaseModels.Encomenda;

import java.io.Serializable;
import java.util.*;

/**
 * Write a description of class Encomendas here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Encomendas implements Serializable {
    private Map<String, Encomenda> encomendas;

    /**
     * Construtor Encomendas por defeito.
     */
    public Encomendas(){
        this.encomendas = new HashMap<>();
    }

    /**
     * Construtor Encomendas parametrizado.
     * @param enc Catálogo de Encomendas.
     */
    public Encomendas(Map<String, Encomenda> enc){
        setEncomendas(enc);
    }

    /**
     * Construtor Encomendas por cópia.
     * @param e Encomendas a copiar.
     */
    public Encomendas(Encomendas e){
        this.setEncomendas(e.getEncomendas());
    }

    /**
     * Método que devolve o Catálogo de Encomendas.
     * @return Catálogo de Encomendas.
     */
    public Map<String, Encomenda> getEncomendas() {
        Map<String, Encomenda> aux = new HashMap<>();
        for(Map.Entry<String, Encomenda> e : this.encomendas.entrySet())
            aux.put(e.getKey(), e.getValue().clone());
        return aux;
    }

    /**
     * Método que define o Catálogo de Encomendas.
     * @param cat Catálogo de Encomendas.
     */
    public void setEncomendas(Map<String, Encomenda> cat) {
        this.encomendas = new HashMap<>();
        cat.entrySet().forEach(e -> this.encomendas.put(e.getKey(), e.getValue().clone()));
    }

    /**
     * Método que devolve uma Lista de Encomenda correspondente à Lista de Códigos dada.
     * @param l Lista de Códigos de Encomenda.
     * @return Lista de Encomenda resultante.
     */
    public List<Encomenda> getListaEncomendas(List<String> l){
        List<Encomenda> r = new ArrayList<>();
        for (String s : l)
            r.add(this.getEncomenda(s));
        return r;
    }

    /**
     * Método que devolve uma Encomenda a partir do Código da mesma.
     * @param e Código da Encomenda.
     * @return Encomenda resultante.
     */
    public Encomenda getEncomenda(String e) {
        return this.encomendas.get(e).clone();
    }

    /**
     * Método que adiciona uma Encomenda ao Catálogo de Encomenda.
     * @param e Encomenda a adicionar.
     */
    public void addEncomenda(Encomenda e){
        this.encomendas.put(e.getCodEnc(), e.clone());
    }

    /**
     * Método que remove uma Encomenda ao Catálogo e devolve-a.
     * @param code Código de Encomenda a remover.
     * @return Encomenda removida.
     */
    public Encomenda removeEncomenda(String code){
        return this.encomendas.remove(code);
    }

    /**
     * Método que verifica se uma Encomenda existe no Catálogo de Encomendas.
     * @param code Código de Encomenda a verficar.
     * @return True caso exista, false caso contrário.
     */
    public boolean existsEncomenda(String code){
        return this.encomendas.containsKey(code);
    }

    /**
     * Método que adiciona uma Classificação numa Encomenda.
     * @param cod Codigo da Encomenda.
     * @param nota Classificação da Encomenda.
     */
    public void classificaEncomenda(String cod, int nota){
        this.encomendas.get(cod).setClassificacao(nota);
    }

    /**
     * Método toString.
     * @return String que contém todos os dados de Encomendas.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        Collection<Encomenda> values = this.encomendas.values();
        sb.append("Encomendas:\n").append(values);
        return sb.toString();
    }

    /**
     * Método Clone
     * @return Encomendas Clonado.
     */
    public Encomendas clone(){
        return new Encomendas();
    }
}
