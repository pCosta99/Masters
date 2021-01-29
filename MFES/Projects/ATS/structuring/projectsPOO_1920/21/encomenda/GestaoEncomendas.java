import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;

public class GestaoEncomendas {
  private Map<String,Encomenda> encomendas;
  
  public GestaoEncomendas() {    
    this.encomendas = new HashMap<>();
  }    
  
  public GestaoEncomendas(Map<String,Encomenda> encs) {
    //this.encomendas = encs.values().stream().collect(Collectors.toMap((e) -> e.getcodenc(),(e) -> e.clone(),(e1,e2) -> e1,HashMap::new));
    this.encomendas = encs.values().stream().collect(Collectors.toMap((e) -> e.getcodenc(),(e) -> e.clone()));
  }
  
  public GestaoEncomendas(GestaoEncomendas ge) {
    this.encomendas = ge.getEncomendas();    
  }
  
  public Map<String,Encomenda> getEncomendas() {
    return this.encomendas.values().stream().collect(Collectors.toMap((e) -> e.getcodenc(),(e) -> e.clone())); 
  }
  
  
       
// a) public Set<String> todosCodigosEnc()
// se fizessemos return this.encomendas.keySet(), seria possível apagar chaves do map
  public Set<String> todosCodigosEnc() {
    return new TreeSet<String>(this.encomendas.keySet());    
  }

// b) public void addEncomenda(Encomenda enc)
  public void addEncomenda(Encomenda enc) {
    this.encomendas.put(enc.getcodenc(), enc.clone());    
  }
  
// c) public Encomenda getEncomenda(String codEnc)

  public Encomenda getEncomenda(String codEnc) {
    return (this.encomendas.get(codEnc)).clone();
  }

// d) public void removeEncomenda(String codEnc)

  public void removeEncomenda(String codEnc) {
    this.encomendas.remove(codEnc);    
  }
// e) public String encomendaComMaisProdutos()
// 
// Nota: criado um método em Encomenda que determina a quantidade de produtos encomendados
  

// f) public Set<String> encomendasComProduto(String codProd)
//
// Nota: criado um método, da classe Encomenda, que determina um produto consta da encomenda
  public Set<String> encomendasComProduto(String codProd) {
     return this.encomendas.values().stream().filter(e -> e.existeNaEncomenda(codProd)).map(Encomenda::getcodenc).collect(Collectors.toCollection(TreeSet::new));       
  }

// 
// h) public Set<Encomenda> encomendasValorDecrescente()
   public Set<Encomenda> encomendasValorDecrescente() { 
     TreeSet<Encomenda> aux = new TreeSet<Encomenda>((e1,e2) -> (int)(e2.calculaValorTotal() - e1.calculaValorTotal()));
     //Nota: valor de e2 - valor de e1, para garantir ordem decrescente
     
     for(Encomenda e: this.encomendas.values())
        aux.add(e.clone());
     
     return aux;        
   }
// i) public Map<String,List<String>> encomendasDeProduto()
// devolve um map de código de produto -> lista de códigos de encomenda
    
   public Map<String,List<String>> encomendasDeProduto() {
     Map<String,List<String>> aux = new HashMap<>();
     
     for (Encomenda e: this.encomendas.values()) {
        List<String> lprods = e.getListaProdutos();
        for (String codProd: lprods) {
           if (!aux.containsKey(codProd))
             aux.put(codProd, new ArrayList<String>());
           aux.get(codProd).add(e.getcodenc());
        }   
     }
     return aux;
   }
   
   
   public String toString() {
     StringBuffer sb = new StringBuffer();
     for (Encomenda e: this.encomendas.values())
       sb.append(e.toString() + "\n");
     return sb.toString(); 
   }
   
   
   public boolean equals(Object o) {
      if (this == o) 
        return true;
      if ((o == null) || (this.getClass() != o.getClass()))
        return false;
      GestaoEncomendas ge = (GestaoEncomendas) o;
      return this.encomendas.equals(ge.getEncomendas());
       
   }    

   public GestaoEncomendas clone() {
     return new GestaoEncomendas(this); 
   }
}
