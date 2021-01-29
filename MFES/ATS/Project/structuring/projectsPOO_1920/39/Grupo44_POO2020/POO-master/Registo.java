
/**
 * Abstract class Registo - write a description of the class here
 *
 * @author (your name here)
 * @version (version number or date here)
 */
public abstract class Registo
{
    /** Email para Registo **/
    private String email;
    /** Password para Registo **/
    private String password;
    
    /** Construtor nulo */
    public Registo(){
        this.email= "";
        this.password= "";
    }
    
    /** Construtor parametrizado para a classe Registo */
    public Registo(String mail, String pass){
        this.email=mail;
        this.password=pass;
    }
    
    /** Construtor de cópia */
    public Registo(Registo r){
        this.email= r.getEmail();
        this.password= r.getPassword();
    }
    
    /** Retorna o email do Registo**/
    public String getEmail(){
        return this.email;
    }
    
    /** Define o email do Registo **/
    public void setEmail(String novoemail){
        this.email= novoemail;
    }
    
    /** Retorna a password do Registo **/
    public String getPassword(){
        return this.password;
    }
    
    /** Define a password do Registo **/
    public void setPassword(String novapass){
        this.password= novapass;
    }
    
    /** Método que clona um Registo */
    public abstract Registo clone();
    
    /** Método que devolve um boolean true caso os Registos sejam iguais e false caso não sejam */
    public boolean equals(Object o){
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        Registo r = (Registo) o;
        return this.email.equals(r.getEmail()) &&  
               this.password.equals(r.getPassword());
    }
    /** Método que cria uma string com a informação do Registo */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Email: ").append(this.email+"\n");        
        sb.append("Password: ").append(this.password+"\n");        
        return sb.toString();
    }  
}

