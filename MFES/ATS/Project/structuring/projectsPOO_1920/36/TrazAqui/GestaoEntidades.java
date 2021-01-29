
/**
 * Write a description of class GestaoUtilizadores here.
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class GestaoEntidades implements Serializable {
    
     /* 
      * 
      * variaveis de instancia
      */
     private Map<String,Entidade> entidades;
     
     /* 
      * 
      * construtores
      */
     public GestaoEntidades() {
         this.entidades = new HashMap<>();
     }
     
     public GestaoEntidades(Map<String, Entidade> entidades) {
         this.entidades = entidades.values()
                                   .stream()
                                   .collect(Collectors.toMap(e -> e.getCodigo(), e -> e.clone()));
     }
     
     public GestaoEntidades(GestaoEntidades ge) {
         this.entidades = ge.getEntidades();
     }
     
     public List<Entidade> getClassificaveis() {
         return this.entidades.values()
                              .stream()
                              .filter(e -> e instanceof Classificavel)
                              .map(e -> e.clone())
                              .collect(Collectors.toList());                
     }
     
     /* 
      * 
      * metodos de instancia
      */
     private Map<String, Entidade> getEntidades() {
         return this.entidades.values()
                              .stream()
                              .collect(Collectors.toMap(e -> e.getCodigo(), e -> e.clone()));
     }
     
     public void addEntidade(Entidade e) {
         this.entidades.put(e.getCodigo(), e.clone());
     }
     
     public void removeEntidade(String codigoEntidade) {
         this.entidades.remove(codigoEntidade);
     }
     
     public Entidade getEntidade(String codigoEntidade) {
         if(existeEntidade(codigoEntidade)) {
            return this.entidades.get(codigoEntidade).clone();
         }
         return null;
     }
     
     public int totalEntidades() {
         return this.entidades.size();
     }
     
     public boolean existeEntidade(String codigoEntidade) {
         return this.entidades.containsKey(codigoEntidade);
     }
     
     public Entidade login(String email, String password) {
         Entidade entidade = this.entidades
                    .values()
                    .stream()
                    .filter(e -> (e.getEmail().equals(email) && e.getPassword().equals(password)))
                    .findAny()
                    .orElse(null);     
                    
         if(entidade != null) {
             return entidade.clone();
         } else {
             return null;
         }
     }
     
     /* 
      * 
      * uso de interface Classificavel para classificar voluntarios e transportadoras
      */
     public void classificarTransportador(String codigoTransportador, Integer classificacao) {
         ((Classificavel)this.entidades.get(codigoTransportador)).classifica(classificacao);
     }
     
     public boolean equals(Object o) {
         if(this == o) return true;
         if((this == null) || this.getClass() != o.getClass()) return false;
         
         GestaoEntidades ge = (GestaoEntidades) o;
         return this.entidades.equals(ge.getEntidades());
     }
     
     public String toString() {
         StringBuffer sb = new StringBuffer();
         for (Entidade e: this.entidades.values()) {
             sb.append(e.toString());
             sb.append("\n\n");
         }
         return sb.toString();
     }
     
     public GestaoEntidades clone() {
         return new GestaoEntidades(this);
     }
}
