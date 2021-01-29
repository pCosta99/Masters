import java.util.Map;
import java.util.List;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.*;
import java.security.*;
import java.io.*;
import java.text.*;
import java.io.Serializable;
/**
 * Escreva a descrição da classe DataBase aqui.
 * 
 /**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class DataBase implements Serializable{
    private Map<String,User> users; //email -> user
    private Map<String,Encomenda> encomendas; //codigo da encomenda -> encomenda
    private List<Transporte> transportes; //(ArrayList) integer -> Transporte , transportes das encomendas
  
  private static Map<String,Comparator<Transportadora>> comparadores = new HashMap<>();  
  
    public static void juntaOrdenacao(String nome, Comparator<Transportadora> ordem) {
       comparadores.put(nome, ordem);
  }
  
  public static Comparator<Transportadora> getOrdem(String nome) {
      return comparadores.get(nome);
  } 
    
   //menu
   public DataBase(){
       users = new HashMap<String,User>();
       encomendas = new HashMap<String,Encomenda>();
       transportes = new ArrayList<Transporte>();
   }
   
   //pega numa encomenda
   public Encomenda getEncomenda(String codEncomenda){
       return encomendas.get(codEncomenda);
    }
   //dá um transporte dado um indice
   public Transporte getTransporte(int index){
       return transportes.get(index);
    }
   // pega num mail
   public User getUser(String mail){
       return users.get(mail);
   }
   
   
   public Set<String> getUsers() {
        return this.users.keySet();
    } 

    
    
   public Set<Encomenda> getEncomendas() {
        return this.encomendas.values().stream().collect(Collectors.toSet());
    }   
    
    
   //verifica se existe encomenda(String codigo)
   public boolean existeEncomenda(String codigo){
       return encomendas.containsKey(codigo);
    }
    // verifica se existe esse mail nos utilizadores
   public boolean existeUtilizador(String mail){
       return users.containsKey(mail);
   }
   
   public void add(User u){
       users.put(u.getTag(),u);
    }
   
   public void add(Encomenda e){
       encomendas.put(e.getCodEncomenda(),e);
    } 
    
   //adiciona um transporte
   public int add(Transporte t){
       transportes.add(t);
       return transportes.size()-1;
    }
   
   public TreeSet<Utilizador> maisUtilizamU(){
       TreeSet<Utilizador> c = new TreeSet<Utilizador>(new ComparaEncomendas());
       int num = 0;
       for(User a: users.values()){
           if(a instanceof Utilizador){
               if(num<=10){
                   c.add((Utilizador)a.clone());
                   num+=1;
                }
            }
        }
        return c;
    
    }
   
   public TreeSet<Transportadora> maisUtilizamT(){
       TreeSet<Transportadora> c = new TreeSet<Transportadora>(new ComparaEncomendasT());
       int num = 0;
       for(User a: users.values()){
           if(a instanceof Transportadora){
               if(num<=10){
                   c.add((Transportadora)a.clone());
                   num+=1;
                }
            }
        }
        return c;
    
    }
     
   //parse 
   public void addUser(User u){
       users.put(u.getTag(), u);
       //users.put(u.getNome(), u);
   } 
   
   public void addEncomenda(Encomenda e){
       encomendas.put(e.getCodEncomenda(),e);
    }
   
    public void gravaEmObjStream(String fich) throws IOException{
       ObjectOutputStream oout = new ObjectOutputStream(new FileOutputStream(fich));
       oout.writeObject(this);
       oout.flush(); 
       oout.close();
   }
}
