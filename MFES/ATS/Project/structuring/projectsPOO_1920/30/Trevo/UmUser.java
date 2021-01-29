import java.lang.Object;
import java.time.LocalDate;
import java.lang.String;
import java.util.Locale;
import java.io.IOException;
public class UmUser {
    private String codutilizador;
    private String nome;
    private Coordenadas gps; 
    private String pass;

    public UmUser(){
    this.codutilizador= new String();
    this.nome=new String(); 
    this.gps = new Coordenadas();
    this.pass = new String(); 
    }
        
    public UmUser(String nro,String n, Coordenadas gps,String e){
    this.codutilizador=new String (nro);
    this.nome= new String (n);
    this.gps=new Coordenadas(gps);
    this.pass = e;
    }
    
    public UmUser(UmUser e){
    this.codutilizador=e.getcodutilizador();
    this.nome=e.getnome();
    this.gps=e.getgps();
    }
     
    public String getnome(){
    return this.nome;   
    }
    
    public String getpass(){
    return this.pass;   
    }
   
    public String getcodutilizador(){
    return this.codutilizador;    
    }
    public Coordenadas getgps(){
    return this.gps;    
    }
    
    public void setpass(String e){
    this.pass=e;   
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
 
    public void setcodutilizador(String e){
    this.codutilizador=e; 
    }
    public void setgps(Coordenadas e){
    this.gps=e;    
    }
    public UmUser clone(){
    return new UmUser(this);   
    }
    
    public boolean existeuser(String e){
    if(e.equals(getcodutilizador())) return true;
    return false;
    }
    
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("\nUtilizador: ");
        sb.append("\nNome: " + nome);
        sb.append("\nCodigo Utilizador: " + codutilizador);
        sb.append("\n" + gps);
        return sb.toString();
    }
    
    public boolean login(String nome,String pass){
    if(nome.equals(this.nome) && pass.equals(this.pass)) return true; 
    return false; 
    }
    
    public int nropedidos() throws IOException{
    Parse p = new Parse();    
    p.parse();
    return p.nropedidosuser(this.codutilizador);
    }

}