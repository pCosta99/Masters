import java.time.LocalDate;
import java.time.LocalDateTime;

public class LinhaEncomenda{
    
   //Variaveis de instância
   private String idProdutos; //codigo do produto
   private String descricao; //descricao do produto
   private int quantidade; //nº de produtos
   private float valor; //valor do produto
   private float peso; //peso do produto
  
   /**
     * Construtor vazio
     */
   public LinhaEncomenda(){
       this.idProdutos = "";
       this.descricao = "";
       this.quantidade = 0;
       this.valor = 0;
       this.peso = 0;
   }
    
   /**
     * Construtor parametrizado 
     */
   public LinhaEncomenda(String idProdutos, String descricao, int quantidade,
   float valor,float peso){
       this.idProdutos = idProdutos;
       this.descricao = descricao;
       this.quantidade = quantidade;
       this.valor = valor;
       this.peso = peso;
   }
    
   /**
     * Construtor de cópia 
     */
   public LinhaEncomenda(LinhaEncomenda le){
        this.idProdutos = le.getIdProdutos();
        this.descricao = le.getDescricao();
        this.quantidade = le.getQuantidade();
        this.valor = le.getValor();
        this.peso = le.getPeso();
   }
    
   // Getters
 
   public String getIdProdutos(){
        return this.idProdutos;
   }
    
   public String getDescricao(){
        return this.descricao;
   }
   
   public int getQuantidade(){
        return this.quantidade;
   }
    
   public float getValor(){
        return this.valor;
   }
   
   public float getPeso(){
       return this.peso;
    }
   // Setters
   
   public void setIdProdutos(String i){
        this.idProdutos = i;
   } 
   
   public void setDescricao(String d){
       this.descricao = d;
   }
   
   public void setQuantidade(int q){
       this.quantidade = q;
   }
    
   public void setValor(float v){
       this.valor = v;
   }
   
   public void setPeso(float p){
       this.peso = p;
    }
   /**
     * Metodo Equals
     */
   public boolean equals(Object o){
        if (this == o)
            return true;
            
        if (o == null || this.getClass() != o.getClass())
            return false;
            
        LinhaEncomenda le = (LinhaEncomenda) o;
        
        return this.idProdutos.equals(le.getIdProdutos()) &&
        this.descricao.equals(le.getDescricao()) &&
        this.quantidade == le.getQuantidade() &&
        this.valor == le.getValor() && this.peso == le.getPeso();
   }
    
   public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Código do produto: " + this.getIdProdutos()+"\n");
        sb.append("Descrição do produto: " + this.getDescricao() + "\n");
        sb.append("Quantidade: " + this.getQuantidade() + "\n");
        sb.append("Valor do produto: " + this.getValor()+ "\n");  
        sb.append("Peso do Produto: " + this.getValor()+ "\n"); 
        return sb.toString(); 
   }  
    
   //Clone
    
   public LinhaEncomenda clone (){
        return new LinhaEncomenda(this);    
   }
   public double pesoTotal(){
       return this.peso *  this.quantidade;
    }
}