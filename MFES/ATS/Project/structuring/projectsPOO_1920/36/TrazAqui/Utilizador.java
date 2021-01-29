
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
public class Utilizador extends Entidade {
    public Utilizador() {
        super();
    }
    
    public Utilizador(String codigo, String nome, GPS gps, String email, String password) {
        super(codigo, nome, gps, email, password);
    }
    
    public Utilizador(Utilizador u) {
        super(u);
    }
    
    public boolean equals(Object obj) {
       if(this == obj) return true;
       if((obj == null) || (this.getClass() != obj.getClass())) return false;
       Utilizador u = (Utilizador) obj;
       return super.equals(u);
   }
    
    public String toString() {
        return "Utilizador: " + "\n" +
               " - Código: " + this.getCodigo() + "\n" +
               " - Nome: " + this.getNome() + "\n" +
               " - GPS: " + this.getGPS().toString();
    }
    
    public Utilizador clone() {
        return new Utilizador(this);
    }
}
