package trazaqui.Exceptions;

public class CatalogoNaoExisteException extends Exception {
    public CatalogoNaoExisteException()
    {
        super();
    }

    public CatalogoNaoExisteException(String msg)
    {
        super(msg);
    }
}