package TrazAqui;

public class LojaInexistenteException extends Exception {
    /**
     * Erro quando nao existe uma loja na base de dados
     * @param s String
     */
    public LojaInexistenteException(String s){
        super(s);
    }
}
