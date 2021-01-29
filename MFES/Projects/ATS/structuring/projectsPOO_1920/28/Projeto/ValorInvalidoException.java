/**
 * Acontece quando o Usuário insere valores negativos onde 
 * é impossivel isso acontecer (velocidade, peso, preço, etc..)
*/

public class ValorInvalidoException extends Exception {
    public ValorInvalidoException (String msg) {
        super(msg);
    }
}