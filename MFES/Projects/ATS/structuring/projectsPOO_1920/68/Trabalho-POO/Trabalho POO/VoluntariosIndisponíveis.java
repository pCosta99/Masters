import java.io.Serializable;

public class VoluntariosIndisponíveis extends Exception implements Serializable {

    public VoluntariosIndisponíveis(){
        super();
    }

    public VoluntariosIndisponíveis(String mensagem){
        super(mensagem);
    }
}
