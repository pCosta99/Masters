/**
 * Escreva a descrição da classe Logins aqui.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992;  
 * @version 20200611
 */
import java.util.*;
import java.io.Serializable;
public class Logins implements Serializable{
    private HashMap<Cliente, String> logins;
    private HashMap<Loja, String> logins_lojas;
    private HashMap<Empresa, String> logins_empresas;
    private HashMap<Voluntario, String> logins_voluntarios;
    
    /**
     * Construtores
     */
    public Logins(){
        this.logins = new HashMap<>();
        this.logins_lojas = new HashMap<>();
        this.logins_empresas = new HashMap<>();
        this.logins_voluntarios = new HashMap<>();
    }
    
    public Logins(HashMap<Cliente, String> logins, HashMap<Loja, String> logins_lojas, HashMap<Empresa, String> logins_empresas, HashMap<Voluntario, String> logins_voluntarios){
        this.logins = logins;
        this.logins_lojas = logins_lojas;
        this.logins_empresas = logins_empresas;
        this.logins_voluntarios = logins_voluntarios;
    }
    
    public Logins(Logins l){
        this.logins = l.getlogins();
        this.logins_lojas = l.getlogins_lojas();
        this.logins_empresas = l.getlogins_empresas();
        this.logins_voluntarios = l.getlogins_voluntarios();
    }
    
    /**
     * Get's
     */
    public HashMap<Cliente, String> getlogins(){
        HashMap<Cliente, String> copia = new HashMap<>();
        for(Map.Entry<Cliente, String> e :this.logins.entrySet()){
           copia.put(e.getKey(), e.getValue()); 
        }
        return copia;
    }
    
    public HashMap<Loja, String> getlogins_lojas(){
        HashMap<Loja, String> copia = new HashMap<>();
        for(Map.Entry<Loja, String> e :this.logins_lojas.entrySet()){
           copia.put(e.getKey(), e.getValue()); 
        }
        return copia;
    }
    
    public HashMap<Empresa, String> getlogins_empresas(){
        HashMap<Empresa, String> copia = new HashMap<>();
        for(Map.Entry<Empresa, String> e :this.logins_empresas.entrySet()){
           copia.put(e.getKey(), e.getValue()); 
        }
        return copia;
    }
    
    public HashMap<Voluntario, String> getlogins_voluntarios(){
        HashMap<Voluntario, String> copia = new HashMap<>();
        for(Map.Entry<Voluntario, String> e :this.logins_voluntarios.entrySet()){
           copia.put(e.getKey(), e.getValue()); 
        }
        return copia;
    }
    
    /**
     * Set's
     */
    public void setlogins(HashMap<Cliente, String> logins){
        this.logins = logins;
    }
    
    public void setlogins_lojas(HashMap<Loja, String> logins_lojas){
        this.logins_lojas = logins_lojas;
    }
    
    public void setlogins_empresas(HashMap<Empresa, String> logins_empresas){
        this.logins_empresas = logins_empresas;
    }
    
    public void setlogins_voluntarios(HashMap<Voluntario, String> logins_voluntarios){
        this.logins_voluntarios = logins_voluntarios;
    }
    
    /**
     * clone
     */
    public Logins clone(){
        return (new Logins(this));
    }
    
    /**
     * Regista cliente
     */
    public void registar(Cliente cliente, String pass){
        if (!(this.logins.containsKey(cliente))){
           this.logins.put(cliente, pass);
        }
    }
    
    /**
     * Efetua o login de um cliente
     */
    public Cliente logar(String cod, String pass){
        Cliente bol = null;
        for(Cliente cliente: this.logins.keySet()){
            if (cliente.getCodC().equals(cod) && pass.equals(this.logins.get(cliente))){
                bol = cliente.clone();
            }
        }
        return bol;
    }
    
    /**
     * Efetua o login de uma loja
     */
    public Loja logar_loja(String cod, String pass){
        Loja bol = null;
        for(Loja loja: this.logins_lojas.keySet()){
            if (loja.getCodL().equals(cod) && pass.equals(this.logins_lojas.get(loja))){
                bol = loja.clone();
            }
        }
        return bol;
    }
    
    /**
     * Regista loja 
     */
    public void registarloja(Loja loja, String pass){
        if (!(this.logins_lojas.containsKey(loja))){
           this.logins_lojas.put(loja, pass);
        }
    }
        
    /**
     * Regista empresa
     */
    public void registarempresa(Empresa empresa, String pass){
        if (!(this.logins_empresas.containsKey(empresa))){
           this.logins_empresas.put(empresa, pass);
        }
    }
    
    /**
     * Efetua login de uma empresa
     */
    public Empresa logarempresa(String codE, String pass){
        Empresa emp = null;
        for(Empresa empresa: this.logins_empresas.keySet()){
            if (empresa.getCodT().equals(codE) && pass.equals(this.logins_empresas.get(empresa))){
                emp = empresa;
            }
        }
        return emp;
    }
    
     /**
      * Efetua login de um voluntario
      */
    public Voluntario logarvoluntarios(String codV, String pass){
        Voluntario v = null;
        for(Voluntario voluntario: this.logins_voluntarios.keySet()){
            if (voluntario.getCodT().equals(codV) && pass.equals(this.logins_voluntarios.get(voluntario))){
                v = voluntario;
            }
        }
        return v;
    }
    
    /**
     * Regista Voluntario
     */
    public void registarvoluntario(Voluntario voluntario, String pass){
        if (!(this.logins_voluntarios.containsKey(voluntario))){
           this.logins_voluntarios.put(voluntario, pass);
        }
    }
    
    public void fazlogin(Cliente cliente, String pass){
        this.getlogins().put(cliente, pass);
    }
}
