package Utils;

import Encomenda.Encomenda;
import Exceptions.EncomendaJaExisteException;
import Exceptions.EncomendaNaoExisteException;
import Transporte.Empresa;
import Transporte.Voluntario;
import Perfis.*;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class DataBase implements Serializable
{
    private Map<String,Encomenda> encomendas; // ref -> encomenda
    private Map<String, Perfil> perfis; //mail -> Utilizador.Perfil
    private List<String> encomendasAceites; //codigo encomenda 
    
    public DataBase(){
       encomendas =new HashMap<>();
       perfis = new HashMap<>();
       encomendasAceites = new ArrayList<>();
    }
     
    
    /**
     * Método que devolve  a encomenda identificada pela sua referencia.
     * 
     * @return Encomenda.Encomenda
     */public Encomenda getEncomenda(String ref) throws EncomendaNaoExisteException {
        Encomenda a= this.encomendas.get(ref);
        if(a==null)
            throw new EncomendaNaoExisteException("Encomenda.Encomenda nao existe");
        return a;
    }
    /**
     * Método que devolve  as encomendas por preparar para uma dada loja.
     * 
     * @return Set<Encomenda.Encomenda>
     */public Set<Encomenda> getEncomendasLoja(String loja){
         return this.encomendas.values().stream()
                                        .filter(a->a.getLoja().equals(loja))
                                        .filter(a->a.getEstado() < 1).
                                        collect(Collectors.toSet());
    }
    /**
     * Método que devolve  as encomendas para um dado user.
     * 
     * @return Set<Encomenda.Encomenda>
     */public Set<Encomenda> getEncomendasUser(String user){
         return this.encomendas.values().stream()
                                        .filter(a->a.getUser().equals(user))
                                        .collect(Collectors.toSet());
    }
    /**
     * Método que devolve  as encomendas transportadas por 1 empresa.
     * 
     * @return Set<Encomenda.Encomenda>
     */public Set<Encomenda> getEncomendasTrans(String trans){
         return this.encomendas.values().stream()
                                        .filter(a->a.getNomTrans().equals(trans))
                                        .collect(Collectors.toSet());
    }

    /**
     * Método que devolve o perfil identificado pelo seu email.
     * 
     * @return Utilizador.Perfil
     */public Perfil getPerfil(String mail) {
         return perfis.get(mail);
  
    }
    public Utilizador getUtilizador(String mail){
        Utilizador a;
        a = (Utilizador) perfis.get(mail);
        return a;
    }
    public Loja getLoja(String mail){
        Loja a;
        a = (Loja) perfis.get(mail);
        return a;
    }

    /**
     * Método que devolve as encomendas prontas para transporte que aceitam 
     * custo
     * 
     * @return Encomenda.Encomenda
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
     * Método que adiciona Encomenda.Encomenda
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
     * Método que adiciona Utilizador.Utilizador
     * 
     * @return  
     */
    public void add(Utilizador a){
        perfis.put(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona Utilizador.Loja
     * 
     * @return  
     */
    public void add(Loja a){
        perfis.put(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona Transporte.Empresa
     * 
     * @return  
     */
    public void add(Empresa a){
        perfis.put(a.getEmail(),a.clone());
    }
    /**
     * Método que adiciona Transporte.Voluntario
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
        try (ObjectOutputStream oout = new ObjectOutputStream(new FileOutputStream(fich))) {
            oout.writeObject(this);
            oout.flush();
        }
    }
    /**
     * Método que devolve o total faturado por 1 empresa
     * 
     * @return  
     */
    public double totalFaturado(String emp,LocalDateTime date){
        Set<Encomenda> aux = this.getEncomendasTrans(emp);
        return aux.stream().filter(v->v.getData().isAfter(date)).mapToDouble(Encomenda::getCusto).sum();
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

   
