public class NaoExisteEntidadeException extends Exception{
    /** Mensagem no caso de nao existir uma Entidade*/
    public NaoExisteEntidadeException(String msg){
        super(msg);
    }
    /** Mensagem vazia no caso de nao existir uma certa entidade*/
    public NaoExisteEntidadeException(){
        super();
    }
}