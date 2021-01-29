import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.io.Serializable;

/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 *
/**
 * Classe que guarda o perfil que é criado 
 */
public abstract class User implements Comparable<User>, Serializable{
     // Variaveis de Instância 
    private String tag;
    private String password;
    private String nome;
    
    private List<Integer> nencomendas; 
    
    
    /** 
     * Construtior para Objetos da Classe User
     */
    public User() {
     
        this.tag = "";
        this.nome = "";
        this.password = "*****";
        this.nencomendas = new ArrayList<Integer>();
    }
    
    /**
      * Construtor por Parâmetro
      * @param tag: tag do user
      * @param nome: nome do user
      * @param password: password do user
      * @param nincomendas :numero de encomendas do user
      */
    public User(String tag,String nome, String password){
        this.tag = tag;
        this.nome = nome;
        this.password = password;
        this.nencomendas = nencomendas;
    }
    
    /** 
     * Construtor de cópia
     */
    public User(User a) {
        this.tag = a.getTag();
        this.nome = a.getNome();
        this.password = a.getPassword();
        
        this.nencomendas= a.nencomendas.stream().collect(Collectors.toList());
        
    }
    
    //Gets 
    /**
     * Get do nome para o User
     * @return o nome
     */
    public String getNome(){
        return nome;
    }
    
    /**
     * Get do Tag para o User
     * @return o tag
     */
    public String getTag(){
        return tag;
    }
    
    /**
     * Get da Nencomendas para o User
     * @return o nencomendas
     */
    public List<Integer> getNencomendas(){
        return nencomendas.stream().collect(Collectors.toList());
    } 
    
    /**
      * Get da password para o User
      * @return a password
      */
    private String getPassword(){
        return password;
    }
    

    public void setTag(String tag){
        this.tag=tag;
    }
    public void setNome(String nome){
        this.nome = nome;
    }
    private void setPassword(String pass){
        password = pass;
    }
    
    //password no menu , alterar password
    public void setPassWord(String nova, String antiga) throws PasswordErradaException{
        if(validaLogin(antiga))
            this.password = nova;
        else 
            throw new PasswordErradaException();     
    }
    
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append("\nTag: ");
        sb.append(this.tag);
        sb.append("\nNome: ");
        sb.append(this.nome);
        sb.append("\nNumero de encomedas: ");
        sb.append(this.nencomendas.size());
      
        return sb.toString();
    }
    
    /**
     * Método clone faz uma cópia do objeto actor
     */
    public abstract User clone();
    
     /**
      * Equals
     */  
    public boolean equals( Object o){
        if( o == this) return true;
        if( o == null || o.getClass() != this.getClass()) return false;
        
        User m = (User) o;
        
        return this.tag.equals(m.getTag()) && this.password.equals( m.getPassword()) 
                && this.nome.equals(m.getNome());
    }
    
    //menu
    public boolean validaLogin(String pass){
        return pass.equals(password);
    }
    
    //adiciona mais um transporte de encomendas ao nencomendas totais
    public void addTransporte(int t){
        nencomendas.add(t);
    }
    
    // listar encomendas transportadas
    public String listaEncomendasT(DataBase db){
        StringBuilder sb = new StringBuilder();
        int max = nencomendas.size();
        for(int i =0; i< max; i ++)
            sb.append((i+1) + " - " + db.getTransporte(i).resumo()+"\n");
        
        return sb.toString();
    }
    

    public int compareTo(User h) {
        return h.getNome().compareTo(this.nome);
        
    }
    
    public int getEncomenda(int x){
        return nencomendas.get(x);
    }
}
