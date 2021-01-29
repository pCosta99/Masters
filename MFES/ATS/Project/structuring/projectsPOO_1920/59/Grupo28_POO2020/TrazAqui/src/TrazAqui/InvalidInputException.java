package TrazAqui;

public class InvalidInputException extends Exception {
    /**
     * Erro quando o input dado e invalido
     * @param s String
     */
    public InvalidInputException(String s) {
        super(s);
    }
}
