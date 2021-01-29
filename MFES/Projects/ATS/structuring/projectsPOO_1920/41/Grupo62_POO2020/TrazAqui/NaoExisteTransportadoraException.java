public class NaoExisteTransportadoraException extends Exception{
    /** Mensagem no caso de nao existir uma transportadora*/
    public NaoExisteTransportadoraException(String msg){
        super(msg);
    }
    /** Mensagem vazia no caso de nao existir uma certa transportadora*/
    public NaoExisteTransportadoraException(){
        super();
    }
}