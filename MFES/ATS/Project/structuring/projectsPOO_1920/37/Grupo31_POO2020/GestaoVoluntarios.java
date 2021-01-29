
/**
 * Escreva a descrição da classe GestaoVoluntarios aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */

import java.util.Map;
import java.util.HashMap;
import java.io.*;
public class GestaoVoluntarios implements Serializable{
    private Map<String,Voluntario> voluntarios;  //codVoluntario->Voluntario
    
    public GestaoVoluntarios(){
        this.voluntarios= new HashMap<>();
    }
    
    public GestaoVoluntarios(Map<String,Voluntario> voluntarios){
        setVoluntario(voluntarios);
    }
    
    public GestaoVoluntarios (GestaoVoluntarios gv){
        this.voluntarios= gv.getVoluntarios();
    }
    
    /**
     * 
     * Gets
     */
    
     
    public Map<String, Voluntario> getVoluntarios(){
        //Map<String, Voluntario> res = new HashMap<>();
        //for(Voluntario v: this.voluntarios.values()){
        //    res.put(v.getCodV(),v.clone());
        //}
        return voluntarios;
    }
    
    /**
     * Sets
     */
    
    public void setVoluntario(Map<String,Voluntario> voluntario){
        this.voluntarios =new HashMap<>();
        voluntarios.values().forEach(v->this.voluntarios.put(v.getCodV(),v.clone()));
    }
    
    /**
     * Método clone()
     */
    
    public GestaoVoluntarios clone(){
        return new GestaoVoluntarios(this);
    }
    
    /**
     * Método equals()
     */
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GestaoVoluntarios gv = (GestaoVoluntarios) obj;
        return gv.getVoluntarios().equals(this.voluntarios);
    }
    
    
    //tostring
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append ("Gestão de Voluntários: ").append (this.voluntarios);
       
        return sb.toString();
    }
    
    public void adicionaVoluntario(Voluntario novoVoluntario){
        voluntarios.put(novoVoluntario.getCodV(),novoVoluntario);
    }
    
    public void removeVoluntarios(String codV){
        voluntarios.remove(codV);
    }

	public Voluntario getVoluntario(String cod) {
        try{
            return this.voluntarios.get(cod);
        }
        catch(NullPointerException e){
            return null;
        }
	}
}
