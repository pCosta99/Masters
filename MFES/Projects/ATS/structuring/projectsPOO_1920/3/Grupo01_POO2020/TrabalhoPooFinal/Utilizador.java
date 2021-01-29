
/**
 * classe que representa os Utilizadores
 */
/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Utilizador extends User{
        
    /** Coordenadas Gps */
    private Localizacao posicao;
        
    /**
     * Construtor sem argumentos
     */    
    public Utilizador(){
        super(); 
        posicao = new Localizacao();
    }
    
    /**
     * Construtor por paramentros
     * @param tag: tag do Utilizador
     * @param nome: nome do Utilizador
     * @param password: password de Utilizador
     * @param log: localizaçao do Utilizador
     */
    public Utilizador(String tag,String nome, String password,Localizacao loc){
        super(tag,nome,password);
        this.posicao = loc;
        
    }
    
    /**
     * Construtor de copia do objeto Utilizador 
     */
    public Utilizador( Utilizador c){
        super(c);
        this.posicao = c.getPosicao();
    }
    
    /**
     * Get da Localizaçao do Utilizador
      */
    public Localizacao getPosicao(){
        return this.posicao;
    }
    
     /**
     * Altera a posiçao que o Utilizador tem
     */
    public void setPosicao( Localizacao x){
        this.posicao = x;
    } 
    
    /**
     * Metodo clone faz uma copia do objecto Utilizador
     * @return uma cópia do User
     */
    public Utilizador clone(){
        return new Utilizador(this);
    }
    
    /**
     * Implementação do método toString
     * @return uma string com a informação de um Utilizador
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append(super.toString());
        
        sb.append("\nLocalização: ");
        sb.append(posicao.toString());
        
        return sb.toString();
    }

    
    public void addTransporte(int index){
        super.addTransporte(index);  
    }
}
