
/**
 * Escreva a descrição da classe GestaoLojas aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */

import java.util.HashMap;
import java.util.Map;
import java.io.*;
public class GestaoLojas implements Serializable{
    private Map<String,Loja> lojas;  //codVoluntario->Voluntario
    
    public GestaoLojas(){
        this.lojas= new HashMap<>();
    }
    
    public GestaoLojas(String codLoja,Map<String,Loja> lojas){
        setLojas(lojas);
    }
    
    public GestaoLojas (GestaoLojas gl){
        this.lojas= gl.getLojas();
    }
    
    /**
     * 
     * Gets
     */
     
    public Map<String, Loja> getLojas(){
        // retorna cópia do map das lojas
        //Map<String, Loja> res = new HashMap<>();
        //for(Loja l: this.lojas.values()){
        //    res.put(l.getCodLoja(),l.clone());
        //}
        // retorna o pointer das lojas
        return lojas;
    }
    
    /**
     * Sets
     */
   
    
    public void setLojas(Map<String,Loja> lojas){
        this.lojas =new HashMap<>();
        lojas.values().forEach(l->this.lojas.put(l.getCodLoja(),l.clone()));
    }
    
    /**
     * Método clone()
     */
    
    public GestaoLojas clone(){
        return new GestaoLojas(this);
    }
    
    /**
     * Método equals()
     */
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GestaoLojas gl = (GestaoLojas) obj;
        return gl.getLojas().equals(this.lojas);
    }
    
    
    //tostring
    public String toString() {
        StringBuilder sb = new StringBuilder();

        for(Loja l: this.lojas.values())
          sb.append(l.toStringMaisBonito());
        return sb.toString();
    }
    
    public void adicionaLoja(Loja novaLoja){
        lojas.put(novaLoja.getCodLoja(),novaLoja);
    }
    
    public void removeLoja(String codLoja){
        lojas.remove(codLoja);
    }
}
