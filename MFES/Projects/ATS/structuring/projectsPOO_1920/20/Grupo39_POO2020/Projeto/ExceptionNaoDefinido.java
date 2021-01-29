
/**
 * Write a description of class ExceptionNaoDefinido here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.io.Serializable;
public class ExceptionNaoDefinido extends Exception implements Serializable
{
    public ExceptionNaoDefinido(){
        super();
    }
    
    public ExceptionNaoDefinido (String m){
        super("Nao definido: " + m);
    }
}
