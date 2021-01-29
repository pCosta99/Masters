

/**
 * Classe MensagemException que devolve uma mensagem caso alguma coisa seja invalida
 */
 
public class MensagemException extends Exception 
{
     public MensagemException()
    {
        super();
    }
    public MensagemException(String msg)
    {
        super(msg);
    }
}
