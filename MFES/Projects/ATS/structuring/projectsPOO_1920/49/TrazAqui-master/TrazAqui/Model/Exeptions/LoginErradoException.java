package Model.Exeptions;
import java.io.Serializable;


public class LoginErradoException extends Exception implements Serializable
{

    public LoginErradoException(){
        super();
    }
    public LoginErradoException(String message){
        super(message);
    }


}
