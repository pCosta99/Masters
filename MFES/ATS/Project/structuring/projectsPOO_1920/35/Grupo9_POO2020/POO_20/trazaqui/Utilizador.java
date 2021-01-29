import java.util.List;
import java.util.ArrayList;
import java.lang.Object;
import java.io.Serializable;
import java.util.Arrays; 

public class Utilizador implements Serializable{
    
   //Variaveis de instância
   private String id; //id do utilizador
   private String nome; //nome do utilizador
   private Coordenadas coordenadas; //coordenadas do utilizador
   private String email; //email do utilizador
   private String password; //password do utilizador
   private String morada; //morada do utilizador

   /**
    * Construtor vazio
    */
    public Utilizador(){
        this.id = "";
        this.nome = "";
        this.coordenadas = null;
        this.email = "";
        this.password = "";
        this.morada = "";
   }
    
   /**
    * Construtor parametrizado 
    */
    public Utilizador(String id, String nome, Coordenadas coordenadas,
    String email, String password, String morada){
        this.id = id;
        this.nome = nome;
        this.coordenadas = coordenadas;
        this.email = email;
        this.password = password;
        this.morada = morada;
   }
    
   /**
    * Construtor de cópia 
    */
    public Utilizador(Utilizador u){
        this.id = u.getId();
        this.nome = u.getNome();
        this.coordenadas = u.getCoordenadas();
        this.email = u.getEmail();
        this.password = u.getPassword();
        this.morada = u.getMorada();
   }
    
   //Getters
   
   public String getId(){
       return this.id;
   }
    
   public String getNome(){
        return this.nome;
   }

   public Coordenadas getCoordenadas(){
       return this.coordenadas;
   }
    
   public String getEmail(){
        return this.email;
   }
    
   public String getPassword(){
        return this.password;
   }
   
   public String getMorada(){
        return this.morada;
   }
   
   //Setters
    
   public void setId(String i){
       this.id = i;
   }
    
   public void setNome(String n){
        this.nome = n;
   }
    
   public void setCoordenadas(Coordenadas c){
       this.coordenadas = c;
   }
    
   public void setEmail(String e){
        this.email = e;
   }
   
   public void setPassword(String p){
        this.password = p;
   }
   
   public void setMorada(String m){
       this.morada = m;
   }
   
   /**
    * Metodo Equals
    */
   public boolean equals(Object o){
      if (this == o)
            return true;
            
      if (o == null || this.getClass() != o.getClass())
            return false;
            
      Utilizador u = (Utilizador) o;
        
      return this.id.equals(u.getId()) &&
      this.nome.equals(u.getNome()) &&
      this.coordenadas.equals(u.getCoordenadas()) &&
      this.email.equals(u.getEmail()) &&
      this.password.equals(u.getPassword()) &&
      this.morada.equals(u.getMorada());
   }
    
   public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Id: " + this.getId() + "\n");
        sb.append("Nome: " + this.getNome() + "\n");
        sb.append("Coordenadas: " + this.getCoordenadas() + "\n");
        sb.append("Email: " + this.getEmail() + "\n");
        sb.append("Morada: " + this.getMorada() + "\n");
        return sb.toString();
   }
   
   /**
     * método que esconde a password substituindo o nr de caracteres da password pelo mesmo numero de * 
     */
   private  String esconderPassword (int tamanho, char caracter) {
       if (tamanho > 0) {
          char[] array = new char[tamanho];
          Arrays.fill(array, caracter);
          return new String(array);
       }
       return "";
   }

   public Utilizador clone(){
       return new Utilizador(this);
   }
}