
/**
 * Abstract class Entidade
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.io.Serializable;
public abstract class Entidade implements Serializable {
    
   private String codigo;
   private String nome;
   private GPS gps;
   private String email;
   private String password;
   
   public Entidade() {
       this.codigo = "";
       this.nome = "";
       this.gps = new GPS();
       this.email = "";
       this.password = "";
   }
   
   public Entidade(String codigo, String nome, GPS gps, String email, String password) {
       this.codigo = codigo;
       this.nome = nome;
       this.gps = gps.clone();
       this.email = email;
       this.password = password;
   }
   
   public Entidade(Entidade entidade) {
       this.codigo = entidade.getCodigo();
       this.nome = entidade.getNome();
       this.gps = entidade.getGPS();
       this.email = entidade.getEmail();
       this.password = entidade.getPassword();
   }
   
   public String getCodigo() {
       return this.codigo;
   }
   
   public String getNome() {
       return this.nome;
   }
   
   public GPS getGPS() {
       return this.gps;
   }
   
   public String getEmail() {
       return this.email;
   }
   
   public String getPassword() {
       return password;
   }
   
   public void getCodigo(String codigo) {
       this.codigo = codigo;
   }
   
   public void getNome(String nome) {
       this.nome = nome;
   }
   
   public void getGPS(GPS gps) {
       this.gps = gps.clone();
   }
   
   public void setEmail (String email) {
       this.email = email;
   }
   
   public double calculaDistancia(GPS destino) {
         double x1,x2,y1,y2;
	 double distancia;
	 
	 x1 = this.gps.getX();
	 y1 = this.gps.getY();
	 x2 = destino.getX();
	 y2 = destino.getY();
	 
	 distancia = Math.sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));	 	    
         
         return distancia;
   }
   
   public boolean equals(Object obj) {
       if(this == obj) return true;
       if((obj == null) || (this.getClass() != obj.getClass())) return false;
       Entidade e = (Entidade) obj;
       return this.codigo.equals(e.getCodigo()) && this.nome.equals(e.getNome()) 
                                                && this.gps.equals(e.gps);
   }
   
   public abstract String toString();
   
   public abstract Entidade clone();
   
}
