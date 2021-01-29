package Model;

import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;

/**
 * Classe que guarda todas as transportadoras da aplicação
 */
public class Transportadoras implements Serializable {
    private Map<String,Transportadora> transp;

    /**
     * Construtor sem parametros
     */
    public Transportadoras(){
        this.transp = new TreeMap<>();
    }

    /**
     * Construtor parametrizado
     * @param t     Map a copiar
     */
    public Transportadoras(Map<String,Transportadora> t){
        this.setTransportadoras(t);
    }

    /**
     * Construtor por copia
     * @param l     Transportadoras a copiar
     */
    public Transportadoras(Transportadoras l){
        this.setTransportadoras(l.getTransportadoras());
    }

    /**
     * Get da variavel transportadoras do objeto
     * @return      Map com os transportadoras
     */
    public Map<String,Transportadora> getTransportadoras(){
        Map<String,Transportadora> aux = new TreeMap<>();
        for(Map.Entry<String,Transportadora> l : this.transp.entrySet())
            aux.put(l.getKey(),l.getValue());
        return aux;
    }

    /**
     * Set da variavel transportadoras do objeto
     * @param transportadoras      transportadoras a copiar
     */
    public void setTransportadoras(Map<String,Transportadora> transportadoras){
        this.transp = new TreeMap<>();
        for(Map.Entry<String,Transportadora> l : transportadoras.entrySet())
            this.transp.put(l.getKey(),l.getValue().clone());
    }

    /**
     * Método que adiciona uma transportadora a variavel transportadoras
     * @param l     Transportadora a adicionar
     */
    public void addTransportadora(Transportadora l){
        this.transp.putIfAbsent(l.getCod(),l.clone());
    }

    /**
     * Método que remove um transportadora da variavel transportadoras
     * @param l     transportadora a remover
     */
    public void rmTransportadora(Transportadora l){
        this.transp.remove(l.getCod());
    }

    /**
     * Método que verifica se o ID e pass do Login são validos para aceder
     * @param s     String com ID
     * @param p     String com a pass
     * @return      boolean
     */
    public boolean verificaLogin(String s, String p){
        if(transp.containsKey(s))
            return transp.get(s).validaPass(p);
        else return false;
    }

    /**
     * Método que verifica se a variavel transportadoras contem um transportadora
     * @param t     String com codigo de transportadora
     * @return      boolean
     */
    public boolean existeTransp(String t){ return this.transp.containsKey(t);}


    /**
     * Get de um transportadora
     * @param cod         String com codigo do transportadora
     * @return            transportadoras com codigo correspondente
     */
    public Transportadora getTransportadora(String cod){
        return transp.get(cod);
    }

}
