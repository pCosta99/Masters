import java.lang.Object;
import java.time.LocalDate;
import java.lang.String;
import java.util.Locale;

public class Usuário {
    private String codigoUsuario;
    private String nome;
    private Coordenadas gps; 
    private String password;
    
    //construtores
    public Usuário(){
    this.codigoUsuario= new String();
    this.nome=new String(); 
    this.gps = new Coordenadas();
    this.password = new String(); 
    }
        
    public Usuário(String nro,String n, Coordenadas gps,String e){
    this.codigoUsuario=new String (nro);
    this.nome= new String (n);
    this.gps=new Coordenadas(gps);
    this.password = e;
    }
    
    public Usuário(Usuário e){
    this.codigoUsuario=e.getcodigoUsuario();
    this.nome=e.getnome();
    this.gps=e.getgps();
    }
    
    //getters e setters
    public String getnome(){
    return this.nome;   
    }
    
    public String getpassword(){
    return this.password;   
    }
   
    public String getcodigoUsuario(){
    return this.codigoUsuario;    
    }
    
    public Coordenadas getgps(){
    return this.gps;    
    }
    
    public void setpassword(String e){
    this.password=e;   
    }
    
    public double getx(){
    return gps.getX();    
    }
    
    public double gety(){
    return gps.getY();           
    }
    
    public void setnome(String e){
    this.nome=e;   
    }
 
    public void setcodigoUsuario(String e){
    this.codigoUsuario=e; 
    }
    
    public void setgps(Coordenadas e){
    this.gps=e;    
    }
    
    public Usuário clone(){
    return new Usuário(this);   
    }
    
    public boolean existeUsuario(String e){ //verifica se exista o usuario
    if(e.equals(getcodigoUsuario())) return true;
    return false;
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nUtilizador: ");
        sb.append("\nNome: " + nome);
        sb.append("\nCodigo Utilizador: " + codigoUsuario);
        sb.append("\nCoordenadas: " + gps);
        return sb.toString();
    }
    
    public boolean login(String nome,String password){ // login
    if(nome.equals(this.nome) && password.equals(this.password)) return true; 
    return false; 
    }

}