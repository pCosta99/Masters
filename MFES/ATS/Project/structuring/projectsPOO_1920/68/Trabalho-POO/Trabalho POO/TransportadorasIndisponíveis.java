import java.io.Serializable;

public class TransportadorasIndisponíveis extends Exception implements Serializable {

    public TransportadorasIndisponíveis(){
        super();
    }

    public TransportadorasIndisponíveis(String mensagem){
        super(mensagem);
    }

}
