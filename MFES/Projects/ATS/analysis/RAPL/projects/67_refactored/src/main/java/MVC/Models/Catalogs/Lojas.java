package MVC.Models.Catalogs;

import MVC.Models.BaseModels.LinhaEncomenda;
import MVC.Models.BaseModels.Loja;
import MVC.Models.BaseModels.Produto;

import java.io.Serializable;
import java.util.*;

/**
 * Write a description of class Lojas here.
 *
 * @author 89510-89561-89501
 * @version 01/05/2020
 */
public class Lojas implements Serializable {
    private Map<String, Loja> lojas;

    /**
     * Construtor de Lojas por defeito.
     */
    public Lojas(){
        this.lojas = new HashMap<>();
    }

    /**
     * Método toString.
     * @return String com os dados do Lojas.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        Collection<Loja> values = this.lojas.values();
        sb.append("Lojas:\n").append(values);
        return sb.toString();
    }

    /**
     * Método que adiciona um Produto a uma Loja.
     * @param p Produto a adicionar
     * @param cod Código da Loja.
     */
    public void addProdutoLoja(Produto p,String cod){
        this.lojas.get(cod).addProduto(p);
    }

    /**
     * Método que retorna uma Lista que contém todas as Lojas
     * @return Lista de Lojas.
     */
    public List<Loja> getListaLojas(){
        List<Loja> r = new ArrayList<>();
        this.lojas.values().forEach(l -> r.add(l.clone()));
        return r;
    }

    /**
     * Método que adiciona uma Loja ao Catálogo de Loja.
     * @param l Loja a adicionar.
     */
    public void addLoja(Loja l){
        this.lojas.put(l.getCod(), l.clone());
    }

    /**
     * Método que verifica se existe uma Loja no Catálogo de Loja.
     * @param code Código da Loja.
     * @return True caso exista, false caso contrário.
     */
    public boolean existeLoja(String code){
        return this.lojas.containsKey(code);
    }

    /**
     * Método que adiciona uma Encomenda a uma Loja.
     * @param codE Código da Encomenda.
     * @param codL Código da Loja.
     */
    public void addEncomendaLoja(String codE, String codL){
        this.lojas.get(codL).addEncomenda(codE);
    }

    /**
     * Método que retorna uma Loja a partir do Código dado.
     * @param s Código da Loja.
     * @return Loja Resultante.
     */
    public Loja getLoja(String s){
        return this.lojas.get(s);
    }

    /**
     * Método que devolve um Produto de uma Determinada Loja.
     * @param prod Código do Produto.
     * @param loja Código da Loja.
     * @return Produto Resultante.
     */
    public Produto getProdutoLoja(String prod, String loja){
        Loja aux = this.lojas.get(loja);
        return aux.getProduto(prod);
    }

    /**
     * Método que verfica se a Encomenda é uma Encomenda Médica.
     * @param les Lista com as LinhaEncomenda da Encomenda.
     * @param cod Código da Encomenda.
     * @return True caso seja, false caso contrário.
     */
    public boolean isEncomendaMed(List<LinhaEncomenda> les, String cod){
        boolean res = false;
        Loja aux = this.lojas.get(cod);
        LinhaEncomenda leaux;
        for (int i = 0; i < les.size() && !res; i++) {
            leaux = les.get(i);
            res = aux.getProduto(leaux.getCodigo()).isMedicamento();
        }
        return res;
    }

    /**
     * Método Clone.
     * @return Lojas Clonado.
     */
    public Lojas clone(){
        return new Lojas();
    }
}