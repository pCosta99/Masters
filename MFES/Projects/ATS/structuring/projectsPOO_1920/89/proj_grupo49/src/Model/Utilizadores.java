package Model;

import java.io.Serializable;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

/**
 * Classe que guarda a informação de todos os utilizadores da aplicação
 */
public class Utilizadores implements Serializable {
    private Map<String,Utilizador> utilizadores;

    /**
     * Construtor sem parametros
     */
    public Utilizadores(){
        this.utilizadores = new TreeMap<>();
    }

    /**
     * Construtor parametrizado
     * @param utilizadores      Map a copiar
     */

    public Utilizadores(Map<String,Utilizador> utilizadores){
        this.setUtilizadores(utilizadores);
    }

    /**
     * Construtor por copia
     * @param l     Utilizadores a copiar
     */
    public Utilizadores(Utilizadores l){
        this.setUtilizadores(l.getUtilizadores());
    }

    /**
     * Get da variavel utilizadores do objeto
     * @return      Map com os utilizadores
     */
    public Map<String,Utilizador> getUtilizadores() {
        Map<String, Utilizador> aux = new TreeMap<>();
        for (Map.Entry<String, Utilizador> l : this.utilizadores.entrySet())
            aux.put(l.getKey(), l.getValue());
        return aux;
    }

    /**
     * Get de um utilizador
     * @param cod       String com codigo do utilizador
     * @return          Utilizador com codigo correspondente
     */
    public Utilizador getUtilizador(String cod){
        return this.utilizadores.get(cod);
    }

    /**
     * Set da variavel utilizadores do objeto
     * @param utilizadores      Utilizadores a copiar
     */
    public void setUtilizadores(Map<String,Utilizador> utilizadores){
        this.utilizadores = new TreeMap<>();
        for(Map.Entry<String,Utilizador> l : utilizadores.entrySet())
            this.utilizadores.put(l.getKey(),l.getValue().clone());
    }

    /**
     * Método que adiciona um utilizador a variavel utilizadores
     * @param l     Utilizador a adicionar
     */
    public void addUtilizador(Utilizador l){
        this.utilizadores.putIfAbsent(l.getCod(),l.clone());
    }

    /**
     * Método que remove um utilizador da variavel utilizadores
     * @param l     Utilizador a remover
     */
    public void rmUtilizador(Utilizador l){
        this.utilizadores.remove(l.getCod());
    }

    /**
     * Método que verifica se o ID e pass do Login são validos para aceder
     * @param s     String com ID
     * @param p     String com a pass
     * @return      boolean
     */
    public boolean verificaLogin(String s, String p){
        if(utilizadores.containsKey(s))
            return utilizadores.get(s).validaPass(p);
        else return false;
    }

    /**
     * Método que verifica se a variavel utilizadores contem um Utilizador
     * @param c     String com codigo de utilizador
     * @return      boolean
     */
    public boolean existeUser(String c){ return this.utilizadores.containsKey(c);}


    /**
     * Método que devolve um set com todos os utilizadores, ordenado pelo comparator no parametro
     * @param aux   Set a colocar os Utilizadores
     * @return      Set com Utilizadores
     */
    public Set<Utilizador> maisUsados(Set<Utilizador> aux){
        for(Map.Entry<String,Utilizador> u : utilizadores.entrySet()){
            aux.add(u.getValue());
        }
        return aux;
    }

    /**
     * Método que adiciona uma encomenda e um utilizador pelo parse de um ficheiro
     * @param l     String com cosigo de utilizador
     * @param e     Encomenda a adicionar
     */
    public void addEncomendaParse(String l, Encomenda e){
        this.utilizadores.get(l).addEncomendaParse(e);
    }

}
