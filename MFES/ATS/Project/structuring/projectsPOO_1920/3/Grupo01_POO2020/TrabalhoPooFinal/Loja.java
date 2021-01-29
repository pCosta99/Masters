import java.util.HashMap;
/**
 * Escreva a descrição da classe Loja aqui.
 * 
 /**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Loja extends User{
   //pessoas em espera
   private int pessoasEspera;
   // lista de encomendas prontas para entrega
   private HashMap<String,Encomenda> encomendasProntas;//codigo->encomendas
   /** Coordenadas Gps */
   private Localizacao posicao; 
   
   /**
     * Construtor sem argumentos
   */
   public Loja(){
        super();
        pessoasEspera = 0;
        encomendasProntas = new HashMap<String,Encomenda>(); 
        posicao = new Localizacao();
   }
   
   /**
     * Construtor por paramentros
     * @param email: email da Loja
     * @param nome: nome da Loja
     * @param password: password da Loja
     * @param log: localizaçao da Loja
     */
   public Loja(String email,String nome, String password, Localizacao loc){
        super(email,nome,password);
        this.pessoasEspera = 0;
        this.encomendasProntas=encomendasProntas;
        this.posicao = loc;
   }
   
   /**
     * Construtor de copia do objeto Loja
     */
   public Loja( Loja c){
        super(c);
        this.pessoasEspera = c.getPessoasEspera();
        this.encomendasProntas = c.getEncomendasProntas();
        this.posicao = c.getPosicao();
    }
   
   /**
     * Get das pessoas que estao a espera
   */
   public int getPessoasEspera(){
       return this.pessoasEspera;
   } 
    
   public HashMap<String,Encomenda> getEncomendasProntas(){
       HashMap<String,Encomenda> encomendasP = new HashMap<String,Encomenda>();
       for(String codEncomenda: encomendasProntas.keySet()){
           encomendasP.put(codEncomenda,encomendasProntas.get(codEncomenda));
        }
       return encomendasProntas; 
   }
   
   /**
     * Get da Localizaçao da Loja
      */
   public Localizacao getPosicao(){
        return this.posicao;
    }
    
   public void setPosicao( Localizacao x){
        this.posicao = x;
    } 
   
   public void setPessoasEspera(int pessoas){
       this.pessoasEspera=pessoas;
    } 
   /**
     * Metodo clone faz uma copia do objecto Abast_Hibrido
     * @return uma cópia do carro
   */
   public Loja clone(){
        return new Loja(this);
    }
    
   /**
     * Método equals
     * Compara um objeto para ver se é Loja 
     */
   public boolean equals(Object o){
        return super.equals(o);
   } 
   
   /**
     * Implementação do método toString
     * @return uma string com a informação de um User
   */
   public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append(super.toString());
        
        sb.append("\nLocalização: ");
        sb.append(posicao.toString());
        sb.append("\nPessoas em lista de espera:");
        sb.append(pessoasEspera);
        sb.append("\nEncomendas prontas para entrega:");
        sb.append(encomendasProntas);
        
        return sb.toString();
    }
   
   public boolean encomendaDisponivel(String codEncomenda){
       return encomendasProntas.containsKey(codEncomenda);
    }
    
   public void addEncomenda(Encomenda e){
       encomendasProntas.put(e.getCodEncomenda(),e);
   } 
    
   public void addTransporte(int index, DataBase db){
       super.addTransporte(index);
       //falta coisas
   } 
}
