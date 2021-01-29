
/**
 * Escreva a descrição da classe GestaoUtilizadores aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.HashMap;
import java.util.Map;
import java.io.*;
public class GestaoUtilizadores implements Serializable{
    private Map<String,Utilizador> utilizadores;  
    
    public GestaoUtilizadores(){
        this.utilizadores= new HashMap<>();
    }
    
    public GestaoUtilizadores(Map<String,Utilizador> utilizadores){
        setUtilizadores(utilizadores);
    }
    
    public GestaoUtilizadores (GestaoUtilizadores gu){
        this.utilizadores= gu.getUtilizadores();
    }
    
    /**
     * 
     * Gets
     */
    
     
    public Map<String, Utilizador> getUtilizadores(){
        //Map<String, Utilizador > res =  new HashMap<>();
        //for(Utilizador u: this.utilizadores.values()){
        //    res.put(u.getCodU(),u.clone());
        //}
        return utilizadores;
    }
    
    /**
     * Sets
     */
    
    
    public void setUtilizadores(Map<String,Utilizador> utilizadores){
        this.utilizadores =new HashMap<>();
        utilizadores.values().forEach(u->this.utilizadores.put(u.getCodU(),u.clone()));
    }
    
    /**
     * Método clone()
     */
    
    public GestaoUtilizadores clone(){
        return new GestaoUtilizadores(this);
    }
    
    /**
     * Método equals()
     */
    
    public boolean equals (Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        GestaoUtilizadores gu = (GestaoUtilizadores) obj;
        return (gu.getUtilizadores().equals(this.utilizadores));
    }
    
    
    //tostring
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append ("Gestão de Utilizadores: ").append (this.utilizadores);
       
        return sb.toString();
    }
    
    
    public void adicionaUtilizador(Utilizador novoUtilizador){
        utilizadores.put(novoUtilizador.getCodU(),novoUtilizador);
    }
    
    
    public void removeUtilizador(String codU){
        utilizadores.remove(codU);
    }
    
    public boolean existeUtilizador(String user){
        String nova = user.replace("@email.com", "");
        return utilizadores.containsKey(nova);
    }

	public Utilizador getUtilizador(String k) {
		return utilizadores.get(k).clone();
	}
}
