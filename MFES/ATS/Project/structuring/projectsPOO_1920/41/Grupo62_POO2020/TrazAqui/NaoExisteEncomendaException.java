public class NaoExisteEncomendaException extends Exception{
    /** Mensagem vazia no caso de nao existir uma encomenda*/
    public NaoExisteEncomendaException(){
        super();
    }
    /** Mensagem no caso de nao existir uma encomenda*/
    public NaoExisteEncomendaException(String str){
        super(str);
    }
}