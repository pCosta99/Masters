

/**
 * Write a description of class PréCarregarDados here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.util.Set;
import java.util.Map;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.io.Serializable;

public class BaseDeDados implements Serializable
{
    
    private Map<String, Transportador> transportadoras;
    private Map<String , Utilizador> utilizadores;
    private Map<Pair, Pair> login; //(username,senha)-> Pair (tipo de utilizador (cliente(utilizador) ou transportador), codigo)
    
    private Map<String, String> qualLoja;//codEncomenda -> Cod da Loja
    private Map<String, Loja> lojas;     //Cod daLoja -> Loja
    private Map<String, Encomenda> entregues;  //encomendas já entregues
    
    public BaseDeDados()
    {
        this.transportadoras = new HashMap<String, Transportador>();
        this.utilizadores = new HashMap<String , Utilizador>();
        this.login = new HashMap<Pair, Pair>(); 
        
        this.qualLoja = new HashMap <String, String>();
        this.lojas = new HashMap <String, Loja>();
        this.entregues = new HashMap<String, Encomenda>();  
    
    }

    public void addEntregue (Encomenda e){
        this.entregues.put(e.getCod(), e);
    }
    
    public Encomenda getEntregue (String cod){
        return this.entregues.get(cod);
    }
    
    public int addUtilizador (Utilizador utilizador){
        String cod = utilizador.getCod();
        
        if (this.utilizadores.keySet().contains(cod)){
            return -1;
        }
        else{
            this.utilizadores.put(cod, utilizador);
        }
        return 0;
    }
    
    public int addVoluntario (Voluntario voluntario){
        String cod = voluntario.getCod();
        
        if (this.transportadoras.keySet().contains(cod)){
            return -1;
        }
        else{
            this.transportadoras.put(cod, voluntario);
        }
        return 0;
    }
    
    public int addEmpresa (Empresa empresa){
        String cod = empresa.getCod();
        
        if (this.transportadoras.keySet().contains(cod)){
            return -1;
        }
        else{
            this.transportadoras.put(cod, empresa);
        }
        return 0;
    }
    
    public int addLoja (Loja loja){
        String cod = loja.getCod();
        
        if (this.lojas.keySet().contains(cod)){
            return -1;
        }
        else{
            this.lojas.put(cod, loja);
        }
        return 0;
        
    }
    
    public int addLogin (Pair username_senha, Pair tipo_cod){
        if (this.login.keySet().stream().map(key -> key.p1()).collect(Collectors.toSet()).contains(username_senha.p1())){
            return -1;            
        }
        else{
            this.login.put(username_senha, tipo_cod);
        }
        return 0;
    }
    
    public Transportador getTransportador(String cod)
    {
        return this.transportadoras.get(cod);
    }
    
    public Utilizador getUtilizador(String cod){
        return this.utilizadores.get(cod);
    }
    
    public String qualLoja(String cod){
        if(!this.qualLoja.keySet().contains(cod)){
            return null;
        }
        
        return this.qualLoja.get(cod);
    }
    
    public void addQualLoja (String e, String loja){
        this.qualLoja.put(e,loja);
    }
    
    public Loja getLoja(String cod){
        return this.lojas.get(cod);
    }
    
    public Pair getLogin(Pair loginInfo){        
        for(Pair x : this.login.keySet()){
            if (x.equals(loginInfo)){
                return x;
            }
        }
        return null;
    }
    
    public Map<Pair ,Pair> getLogin(){ //Só para consulta
        return new HashMap<Pair, Pair> (this.login);
    }
    
    public Map<String, Utilizador> getUtilizadores()
    {
        return new HashMap (this.utilizadores);
    }
    
    public String getTipo (Pair loginInfo){
        return this.login.get(loginInfo).p1();
    }
    
    public String getCod (Pair loginInfo){
        return this.login.get(loginInfo).p2();
    }
    
    public Map<String, String> getNomeLoja(){
        HashMap nomes = new HashMap <String, String>();
        this.lojas.entrySet().stream().forEach(entry ->nomes.put(entry.getKey(),Loja.class.cast(entry.getValue()).getNome()));
        return nomes; 
    }
    
    public int adicionarProdutoDisponivel(String codLoja, LinhaEncomenda produto){
        return this.getLoja(codLoja).adicionarProdutoDisponivel(produto);
    }
}
