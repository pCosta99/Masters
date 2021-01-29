import java.io.Serializable;

/**
 * Classe que implementa um Cliente para efeitos de log in
 *
 * @author monteiro06
 * @version 20200421
 */
public class Cliente extends Entidade implements Serializable {

    public Cliente(){
       super();
    }

    public Cliente(String umCodigo, String umNome, GPS umaLocalizacao){
        super(umCodigo, umNome, umaLocalizacao);
    }

    public Cliente(Cliente umCliente){
        super(umCliente);
    }

    public Cliente clone(){
        return new Cliente(this);
    }

    public boolean equals(Object o){
        if(this == o)
            return true;
        if(o == null || this.getClass() != o.getClass())
            return false;

        Cliente c = (Cliente) o;
        return super.equals(c);
    }

    public String toString(){
        return super.toString();
    }

    public String toCSV(){
        return super.toCSV();
    }

    public String toLog(){
        return super.toLog();
    }
}
