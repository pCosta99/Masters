import java.io.Serializable;

/**
 * Classe que representa os voluntários
 */
public class Voluntario extends MeioTransporte implements Serializable {

    /**
     * Construtor por omissão
     */
    public Voluntario() {
        super();
    }

    /**
     * Construtor parametrizado
     */
    public Voluntario(String codigo, String nome, GPS gps, double raio, boolean certificado, double velocidade) {
        super(codigo, nome, gps, raio, certificado, velocidade);
    }

    /**
     * Construtor de cópia
     */
    public Voluntario(MeioTransporte t) {
        super(t);
    }


    public Voluntario clone() {
        return new Voluntario(this);
    }
}
