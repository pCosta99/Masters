
/**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class UserNaoExistenteException extends Exception
{
    public UserNaoExistenteException(){
        super();
    }
    public UserNaoExistenteException(String message){
        super(message);
    }
}
