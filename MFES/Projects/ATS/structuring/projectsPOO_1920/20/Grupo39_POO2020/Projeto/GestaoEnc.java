
/**
 * Write a description of class GestaoEnc here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.util.*;

import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;

import java.util.stream.Collectors;
import java.util.Iterator;
import java.io.Serializable;

//COMPOSIÇÃO


public class GestaoEnc implements Serializable
{
    private String nome;
    private int count;
    private Map<String, Encomenda> encomendas;
    
    //Construtores
    
    public GestaoEnc(){
        this.count = 0;
        this.nome = "n/a";
        this.encomendas = new HashMap<>();
        
    }
 
    //Métodos
    
    public int getCount(){
        return this.count;
    }
    
    public Set<String> todosCodigosEnc(){
        return this.encomendas.keySet().stream().collect(Collectors.toSet());
        
    }
    
    public void addEncomenda (Encomenda enc){
        this.count++;
        this.encomendas.put(enc.getCod(), enc.cloneEncomenda());
    }
    
    public Encomenda getEncomenda(String cod){
        return this.encomendas.get(cod).cloneEncomenda();
    }
    
    public void removeEncomenda (String cod){
        this.encomendas.remove(cod);
        this.count--;
    }
    
    public String encomendaComMaisProdutos()
    {
        
        if (this.encomendas.keySet().isEmpty()){
            return null;
        }

        return this.encomendas.entrySet().stream().max(Map.Entry.comparingByValue(new ComparatorNumProds())).get().getKey();

    }
    
    
    public Set<Encomenda> encomendasValorDecrescente(){
        Set<Encomenda> res = new TreeSet<>(new ComparatorDecrescente());
        
        for(Encomenda a: this.encomendas.values()) {
            res.add(a.cloneEncomenda());
        }
        
        return res;
    }
    
    
    public Map<String,List<String>> encomendasDeProduto(){
       
        
       Map<String,List<String>> res = new HashMap<>();
       
       HashSet <Encomenda> copias = new HashSet ();
           
       for (Encomenda a: this.encomendas.values()){
               
               copias.add(a.cloneEncomenda());
       }
       for(Encomenda a : copias){
           
            for(LinhaEncomenda b: a.getLinhasEncomenda()){
                if(!res.containsKey(b.getReferencia())){
                    res.put(b.getReferencia(), new ArrayList(this.encomendasComProduto(b.getReferencia())));
                }
             }
             
       }
       return res;     
       
       }
       
       
    public Set <Encomenda> encomendasComProduto (String codProd){
           
       Set <Encomenda> res = new HashSet();
           
       HashSet <Encomenda> copias = new HashSet ();
           
       for (Encomenda a: this.encomendas.values()){
               
               copias.add(a.cloneEncomenda());
            }
           
            for(Encomenda a : copias){
           
               for(LinhaEncomenda b: a.getLinhasEncomenda()){
                   if (b.getReferencia().equals(codProd)){
                       res.add(a);
                       break;
                    }
                }
            }
                
    
       return res;
    }
    
   
    public boolean contains (String cod){
       return this.encomendas.containsKey(cod);
    }
}

        

