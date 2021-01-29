import java.io.Serializable;

public class TransportesIndisponíveis extends Exception implements Serializable {

    public TransportesIndisponíveis(){
        super();
    }

    public TransportesIndisponíveis(String mensagem){
        super(mensagem);
    }
}
