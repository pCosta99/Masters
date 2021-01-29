
/**
 * Write a description of class ExceptionEncomendaNaoEncontrada here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
import java.io.Serializable;
public class ExceptionEncomendaNaoEncontrada extends Exception implements Serializable
{
    public ExceptionEncomendaNaoEncontrada(){
        super();
    }
    
    public ExceptionEncomendaNaoEncontrada (String mensagem){
        super(mensagem);
    }
}
