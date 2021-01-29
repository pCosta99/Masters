package TrazAqui;

/**
 * Exception de quando já existe um código no sistema, seja de utilizador, estafeta ou loja.
 */
public class ExistingCodeException extends Exception {
    public ExistingCodeException(String s) {
        super(s);
    }
}
