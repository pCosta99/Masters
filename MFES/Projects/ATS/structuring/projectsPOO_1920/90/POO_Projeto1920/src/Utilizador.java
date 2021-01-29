import java.io.Serializable;
/**
 * Classe que representa os utilizadores
 */
public class Utilizador extends Entidade implements Serializable {
    public Utilizador() {
    }

    /**
     * Construtor parametrizado
     */
    public Utilizador(String codigo, String nome, GPS gps) {
        super(codigo, nome, gps);
    }


    /**
     * Construtor por cópia
     */
    public Utilizador(Entidade e) {
        super(e);
    }


    public Utilizador clone() {
        return new Utilizador(this);
    }


}
