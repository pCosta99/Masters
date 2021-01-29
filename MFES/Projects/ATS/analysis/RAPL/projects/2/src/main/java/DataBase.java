import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Collection;
import java.util.ArrayList;
import java.util.List;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.Serializable;
import java.util.stream.Collectors;
import java.util.Iterator;
import java.util.*;
import java.time.LocalDateTime; 
public class DataBase implements Serializable
{
    private Map<String,Encomenda> encomendas; // ref -> encomenda
    private Map<String,Perfil> perfis; //mail -> Perfil
    private List<String> encomendasAceites; //codigo encomenda 
    
    public DataBase(){
       encomendas =new HashMap<String,Encomenda>();
       perfis = new HashMap<String,Perfil>();
       encomendasAceites = new ArrayList<String>();
    }
     
    
    /**
     * Método que devolve  a encomenda identificada pela sua referencia.
     * 
     * @return Encomenda 
     */public Encomenda getEncomenda(String ref) throws EncomendaNaoExisteException{
        Encomenda a= this.encomendas.get(ref);
        if(a==null)
            throw new EncomendaNaoExisteException("Encomenda nao existe");
        
        return a;
    }
    /**
     * Método que devolve  as encomendas por preparar para uma dada loja.
     * 
     * @return Set<Encomenda> 
     */public Set<Encomenda> getEncomendasLoja(String loja){
         return this.encomendas.values().stream()
                                        .filter(a->a.getLoja().equals(loja))
                                        .filter(a->a.getEstado() < 1).
                                        collect(Collectors.toSet());
    }
    /**
     * Método que devolve  as encomendas para um dado user.
     * 
     * @return Set<Encomenda> 
     */public Set<Encomenda> getEncomendasUser(String user){
         return this.encomendas.values().stream()
                                        .filter(a->a.getUser().equals(user))
                                        .collect(Collectors.toSet());
    }
    /**
     * Método que devolve  as encomendas transportadas por 1 empresa.
     * 
     * @return Set<Encomenda> 
     */public Set<Encomenda> getEncomendasTrans(String trans){
         return this.encomendas.values().stream()
                                        .filter(a->a.getNomTrans().equals(trans))
                                        .collect(Collectors.toSet());
    }
    
    /**
     * Método que devolve  as encomendas todas.
     * 
     * @return Set<Encomenda> 
     */public Set<Encomenda> getEncomendas(){
        return this.encomendas.values().stream()
                        .collect(Collectors.toSet());
    }
    /**
     * Método que devolve o perfil identificado pelo seu email.
     * 
     * @return Perfil
     */public Perfil getPerfil(String mail) {
         return perfis.get(mail);
  
    }
    public Utilizador getUtilizador(String mail){
        Utilizador a = new Utilizador();
        a = (Utilizador) perfis.get(mail);
        return a;
    }
    public Loja getLoja(String mail){
        Loja a = new Loja();
        a = (Loja) perfis.get(mail);
        return a;
    }
    public Empresa getEmpresa(String mail){
        Empresa a = new Empresa();
        a = (Empresa) perfis.get(mail);
        return a;
    }
    public Voluntario getVoluntario(String mail){
        Voluntario a = new Voluntario();
        a = (Voluntario) perfis.get(mail);
        return a;
    }
    /**
     * Método que devolve  todos os perfis.
     * 
     * @return Encomenda 
     */
    public Set<String> getPerfis(){
        return this.perfis.keySet();
    }
    /**
     * .
     * 
     * @return Encomenda 
     */
    public String getEncomendaAceite(int i){
        return encomendasAceites.get(i);
    }
    /**
     * Método que devolve as encomendas prontas para transporte que aceitam 
     * custo
     * 
     * @return Encomenda 
     */
    public Set<Encomenda> getEncomendaAceitaCusto(){
        return this.encomendas.values().stream()
                                        .filter(a->a.getAceitaCusto()==1)
                                        .filter(a->a.getEstado() < 1).
                                        collect(Collectors.toSet());
    }
    public Set<Encomenda> getEncomendaNaoAceitaCusto(){
        return this.encomendas.values().stream()
                                        .filter(a->a.getAceitaCusto()==0)
                                        .filter(a->a.getEstado() < 1).
                                        collect(Collectors.toSet());
    }
    /**
     * Método que adiciona Encomenda
     * 
     * @return  
     */
    public void addEnc(Encomenda a) throws  EncomendaJaExisteException{
        if(!encomendas.containsKey(a.getEnc())){
            encomendas.put(a.getEnc(),a);
        }
        else {
            throw new EncomendaJaExisteException();
        }
        
     }
    
    /**
     * Método que adiciona Perfil
     * 
     * @return  
     */
    public void add(Perfil a){
        perfis.put(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona Utilizador
     * 
     * @return  
     */
    public void add(Utilizador a){
        perfis.put(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona Loja
     * 
     * @return  
     */
    public void add(Loja a){
        perfis.put(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona Empresa
     * 
     * @return  
     */
    public void add(Empresa a){
        perfis.put(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona Voluntario
     * 
     * @return  
     */
    public void add(Voluntario a){
        perfis.putIfAbsent(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona encomenda as aceites
     * 
     * @return  
     */
    public void addEncAceite(String a){
        encomendasAceites.add(a);
    }
    /**
     * Método verifica se existe utilizador
     * 
     * @return  
     */
    public boolean existeUtilizador(String mail){
       return perfis.containsKey(mail);
    }
    /**
     * Método que grava estado em objeto 
     * 
     * @return  
     */
    public void gravaEmObjStream(String fich) throws IOException{
       ObjectOutputStream oout = new ObjectOutputStream(new FileOutputStream(fich));
       oout.writeObject(this);
       oout.flush(); 
       oout.close();
    } 
    /**
     * Método que devolve o total faturado por 1 empresa
     * 
     * @return  
     */
    public double totalFaturado(String emp,LocalDateTime date){
        Set<Encomenda> aux = this.getEncomendasTrans(emp);
        return aux.stream().filter(v->v.getData().isAfter(date)).mapToDouble(Encomenda::getCusto).sum();
        /*double saldo=0;
        for(Encomenda a: aux){
            saldo +=a.getCusto();
        }
        return saldo;
        */
       
    }
    /**
     * Método que devolve a lista de utilizadores que mais utilizaram
     * a aplicaçao por ordem decrescente
     * 
     * 
     * @return  
     */
    public List<Utilizador> ordenarUsers(Comparator<Utilizador> c){
        return this.perfis.values().stream()
                        .filter(a-> a instanceof Utilizador)
                        .map(v-> (Utilizador) v)
                        .map(Utilizador::clone)
                        .sorted(c)
                        .limit(10)
                        .collect(Collectors.toCollection(ArrayList<Utilizador>::new));
    }
}

   
