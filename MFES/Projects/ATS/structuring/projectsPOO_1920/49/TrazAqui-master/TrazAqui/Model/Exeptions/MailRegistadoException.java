package Model.Exeptions;

import java.io.Serializable;
public class MailRegistadoException extends Exception implements Serializable{
    public MailRegistadoException(){
        super();
    }
    public MailRegistadoException(String message){
        super(message);
    }
}
