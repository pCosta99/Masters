package MVC.Models.BaseModels;

import java.io.Serializable;
import java.util.*;
import java.util.concurrent.locks.Lock;

/**
 * Class para um User do tipo Loja e onde as funcionalidades
 * que este poderá usar.
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */
public class Loja extends User implements Serializable {

    transient Map<String,Produto> produtos;

    /**
     * Construtor de Loja parametrizado.
     * @param c Código da Loja.
     * @param n Nome da Loja.
     * @param x Coordenada X da Localização da Loja.
     * @param y Coordenada Y da Localização da Loja.
     */
    public Loja(String c, String n, double x, double y) {
        super(c, n, x, y);
        this.produtos = new HashMap<>();
    }

    /**
     * Construtor de Loja por Cópia.
     * @param user Loja a copiar.
     */
    public Loja(Loja user) {
        super(user);
        this.produtos = new HashMap<>();
        this.addProdutos(user.produtos.values());
    }

    /**
     * Método que adiciona um Produto à Loja.
     * @param p Produto a adicionar.
     */
    public void addProduto(Produto p){
        this.produtos.put(p.getCod(),p.clone());
    }

    /**
     * Método que adiciona Produtos à Loja.
     * @param c Produtos a adicionar.
     */
    public void addProdutos(Collection<Produto> c){
        for(Produto p : c)
            this.addProduto(p);
    }

    /**
     * Método que retorna a Lista de Produto da Loja.
     * @return Lista de Produto.
     */
    public List<Produto> getListaProdutos(){
        List<Produto> r = new ArrayList<>();
        this.produtos.values().forEach(p->r.add(p.clone()));
        return r;
    }

    /**
     * Método que devolve o número de Ocupação da fila.
     * @return Ocupação da Fila.
     */
    private int getFila(){
        return new Random().nextInt(10);
    }

    /**
     * Método que devolve a Duração Média da Fila por Cliente.
     * @return Duração Média da Fila.
     */
    private double getDuracaoMediaFila(){
        return ((new Random().nextDouble())/4);
    }

    /**
     * Método que devolve a Duração Total da Fila.
     * @return Duração Total da Fila.
     */
    public double getDuracaoFila(){
        return this.getDuracaoMediaFila() * this.getFila();
    }

    /**
     * Método que retorna um Produto existente na Loja.
     * @param p Código do Produto.
     * @return Produto resultante.
     */
    public Produto getProduto(String p){
        return this.produtos.get(p).clone();
    }


    /**
     * Método que verifica se existe um determinado Produto na Loja.
     * @param codP Código do Produto.
     * @return True caso exista, false caso contrário.
     */
    public boolean existeProduto(String codP){
        return this.produtos.containsKey(codP);
    }

    /**
     * Método toString.
     * @return String que contém os dados da Loja.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Loja{\n").append(super.toString()).append("\n}");
        return sb.toString();
    }


    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof User)) return false;
        Loja l = (Loja) o;
        return l.getCod().equals(this.getCod());
    }

    /**
     * Método Clone.
     * @return Loja Clonada.
     */
    public Loja clone(){
        return new Loja(this);
    }
}