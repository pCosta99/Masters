public class ExisteEncomendaException extends Exception{
    /** Mensagem no caso de existir uma certa Encomenda*/
    public ExisteEncomendaException(String msg){
        super(msg);
    }
    /** Mensagem vazia no caso de existir uma certa Encomenda*/
    public ExisteEncomendaException(){
        super();
    }
}