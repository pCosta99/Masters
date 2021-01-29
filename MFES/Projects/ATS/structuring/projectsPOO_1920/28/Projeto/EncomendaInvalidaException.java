/**
 * Acontece quando algum Usuário tenta escolher uma encomenda para transportar, 
 * preparar ou aceitar inválida (que não existe ou que não é relevante a ele mesmo)
*/

public class EncomendaInvalidaException extends Exception {
    public EncomendaInvalidaException (String msg) {
        super(msg);
    }
}