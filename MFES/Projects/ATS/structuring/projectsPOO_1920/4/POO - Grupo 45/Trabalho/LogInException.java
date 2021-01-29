import java.util.*;
public class LogInException extends Exception
{
    public LogInException (){
        super("Credenciais erradas");
    }
    public LogInException(String tex){
        super(tex);
    }
}
