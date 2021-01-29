package Model;

import java.io.Serializable;
import java.util.Map;
import java.util.TreeMap;

/**
 * Clase responsavel por guardar todos os voluntarios da aplicação
 */
public class Voluntarios implements Serializable {
    private Map<String,Voluntario> voluntarios;

    /**
     * Construtor sem parametros
     */
    public Voluntarios(){
        this.voluntarios = new TreeMap<>();
    }

    /**
     * Construtor parametrizado
     * @param Voluntarios      Map a copiar
     */

    public Voluntarios(Map<String,Voluntario> Voluntarios){
        this.setVoluntarios(Voluntarios);
    }

    /**
     * Construtor por copia
     * @param l     Voluntarios a copiar
     */
    public Voluntarios(Voluntarios l){
        this.setVoluntarios(l.getVoluntarios());
    }

    /**
     * Get da variavel voluntarios do objeto
     * @return      Map com os voluntarios
     */
    public Map<String,Voluntario> getVoluntarios(){
        Map<String,Voluntario> aux = new TreeMap<>();
        for(Map.Entry<String,Voluntario> l : this.voluntarios.entrySet())
            aux.put(l.getKey(),l.getValue());
        return aux;
    }

    /**
     * Set da variavel voluntarios do objeto
     * @param Voluntarios      voluntarios a copiar
     */
    public void setVoluntarios(Map<String,Voluntario> Voluntarios){
        this.voluntarios = new TreeMap<>();
        for(Map.Entry<String,Voluntario> l : Voluntarios.entrySet())
            this.voluntarios.put(l.getKey(),l.getValue().clone());
    }

    /**
     * Método que adiciona um voluntario a variavel voluntarios
     * @param l     Voluntario a adicionar
     */
    public void addVoluntario(Voluntario l){
        this.voluntarios.putIfAbsent(l.getCod(),l.clone());
    }

    /**
     * Método que remove um voluntario da variavel voluntarios
     * @param l     Voluntario a remover
     */
    public void rmVoluntario(Voluntario l){
        this.voluntarios.remove(l.getCod());
    }

    /**
     * Método que verifica se o ID e pass do Login são validos para aceder
     * @param s     String com ID
     * @param p     String com a pass
     * @return      boolean
     */
    public boolean verificaLogin(String s, String p){
        if(voluntarios.containsKey(s))
            return voluntarios.get(s).validaPass(p);
        else return false;
    }

    /**
     * Método que verifica se a variavel voluntarios contem um Voluntario
     * @param c     String com codigo de voluntario
     * @return      boolean
     */
    public boolean existeVol(String c){ return this.voluntarios.containsKey(c);}

    /**
     * Get de um voluntario
     * @param c         String com codigo do voluntario
     * @return          Voluntarios com codigo correspondente
     */
    public Voluntario getVoluntario(String c){ return this.voluntarios.get(c);}
}
