package Model;

import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 * Classe que guarda a informação de todas as Lojas
 */
public class Lojas implements Serializable {
    private Map<String,Loja> lojas;

    /**
     * Construtor sem paramettros
     */
    public Lojas(){
        this.lojas = new TreeMap<>();
    }

    /**
     * Construtor parametrizado
     * @param lojas     Map com lojas a copiar
     */
    public Lojas(Map<String,Loja> lojas){
        this.setLojas(lojas);
    }

    /**
     * Construtor por copia
     * @param l     Lojas a copiar
     */
    public Lojas(Lojas l){
        this.setLojas(l.getLojas());
    }

    /**
     * Get da variavel lojas do objeto
     * @return      Map com os lojas
     */
    public Map<String,Loja> getLojas(){
        Map<String,Loja> aux = new TreeMap<>();
        for(Map.Entry<String,Loja> l : this.lojas.entrySet())
            aux.put(l.getKey(),l.getValue());
        return aux;
    }

    /**
     * Set da variavel lojas do objeto
     * @param lojas      lojas a copiar
     */
    public void setLojas(Map<String,Loja> lojas){
        this.lojas = new TreeMap<>();
        for(Map.Entry<String,Loja> l : lojas.entrySet())
            this.lojas.put(l.getKey(),l.getValue().clone());
    }
    /**
     * Get de um transportadora
     * @param codL        String com codigo do transportadora
     * @return            lojas com codigo correspondente
     */

    public Loja getLoja(String codL){
        return lojas.get(codL);
    }

    /**
     * Método que adiciona uma transportadora a variavel lojas
     * @param l     Transportadora a adicionar
     */
    public void addLoja(Loja l){
        this.lojas.putIfAbsent(l.getCod(),l.clone());
    }

    /**
     * Método que remove um transportadora da variavel lojas
     * @param l     transportadora a remover
     */
    public void rmLoja(Loja l){
        this.lojas.remove(l.getCod());
    }

    public void addEncomenda(String s, String l, String u, String[]ps, int []qt){
        this.lojas.get(l).addEncomenda(s,u,ps,qt);
    }


    /**
     * Método que invoca a addEncomendaParde numa loja
     * @param l     String com codigo loja
     * @param e     Encomenda a adicionar
     */
    public void addEncomendaParse(String l, Encomenda e){
        this.lojas.get(l).addEncomendaParse(e);
    }


    /**
     * Método que verifica se o ID e pass do Login são validos para aceder
     * @param s     String com ID
     * @param p     String com a pass
     * @return      boolean
     */
    public boolean verificaLogin(String s, String p){
        if(lojas.containsKey(s))
            return lojas.get(s).validaPass(p);
        else return false;
    }


    /**
     * Método que verifica se a variavel lojas contem um loja
     * @param c     String com codigo de loja
     * @return      boolean
     */
    public boolean existeLoja(String c){
        return this.lojas.containsKey(c);
    }

    /**
     * Métodos que adiciona produtos a uma loja
     * @param cod       String codigo loja
     * @param p         Set com produtos a adicionar
     */

    public void addProdutosLoja(String cod, TreeSet<Produto> p){
        lojas.get(cod).addProdutos(p);
    }

}
