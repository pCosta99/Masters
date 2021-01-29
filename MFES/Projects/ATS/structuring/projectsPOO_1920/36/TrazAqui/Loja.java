
/**
 * 
 *
 * @authorArtur Drohobytskyy
 * @version 1.0
 */
public class Loja extends Entidade {
    
    public Loja() {
        super();
    }
    
    public Loja(String codigo, String nome, GPS gps, String email, String password) {
        super(codigo, nome, gps, email, password);
    }
    
    public Loja(Loja l) {
        super(l);
    }
    
    public boolean equals(Object obj) {
       if(this == obj) return true;
       if((obj == null) || (this.getClass() != obj.getClass())) return false;
       Loja l = (Loja) obj;
       return super.equals(l);
   }
    
    public String toString() {
        return "Loja: " + "\n" +
               " - Código: " + this.getCodigo() + "\n" +
               " - Nome: " + this.getNome() + "\n" +
               " - GPS: " + this.getGPS().toString();
    }
    
    public Loja clone() {
        return new Loja(this);
    }
}
