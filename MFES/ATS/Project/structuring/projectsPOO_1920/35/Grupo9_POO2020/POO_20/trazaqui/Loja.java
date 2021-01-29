import java.util.List;
import java.util.ArrayList;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.Collections;


public class Loja extends Utilizador {
    
   //Variaveis de instância
  
  
   private List<LinhaEncomenda> produtos;
   private int tamanhoFila; //nº de pessoas em fila de espera

   /**
    * Construtor vazio
    */
   public Loja(){
        super("Loja","",null,"","","");
        this.tamanhoFila = 0;
        this.produtos = new ArrayList<>();
   }

   /**
    * Construtor por parametrizado
    */
   public Loja(String id, String nome, Coordenadas coordenadas, String email,
   String password, String morada, boolean encParaEntrega, int tamanhoFila,List<LinhaEncomenda> linhasEnc){
       super(id, nome, coordenadas, email, password, morada);
       this.tamanhoFila = tamanhoFila;
       setEncomendas(linhasEnc);
   }
   
   /**
    * Construtor por cópia
    */
   public Loja(Loja l) {
        super(l); 
        this.tamanhoFila = l.getTamanhoFila();
        this.produtos = l.getProdutos();
   }

   //Getters

   public int getTamanhoFila(){
        return this.tamanhoFila;
   }
    
   public List<LinhaEncomenda> getProdutos() {
      return this.produtos.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());        
    }
   //Setters

   public void setTamanhoFila(int tf){
        this.tamanhoFila = tf;
   }
   
   public void setEncomendas(List<LinhaEncomenda> linhasEnc) {
        this.produtos = new ArrayList<>();
        for(LinhaEncomenda le : linhasEnc) {
            this.produtos.add(le.clone());
        }
   }
   /**
    * Metodo Equals
    */
   public boolean equals(Object o){
       if (this == o)
           return true;
           
       if (o == null || this.getClass() != o.getClass())
            return false;
            
       Loja l = (Loja) o;
       
       return super.equals(l) && this.tamanhoFila == l.getTamanhoFila() && this.produtos.equals(l.getProdutos());
   }
   
   public String toString() {
       StringBuilder sb = new StringBuilder();
       sb.append("Número de pessoas em espera: " + this.getTamanhoFila() +"\n");
       sb.append("Produtos em stock: " + this.produtos.toString() + "\n");
       return sb.toString();
   }
  
   //Clone
    
   public Loja clone(){
       return new Loja(this);
   }
   
}