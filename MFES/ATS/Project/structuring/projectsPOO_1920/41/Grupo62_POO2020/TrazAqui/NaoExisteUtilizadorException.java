public class NaoExisteUtilizadorException extends Exception{
    /** Mensagem no caso de nao existir um Utilizador*/
    public NaoExisteUtilizadorException(String msg){
        super(msg);
    }
    /** Mensagem vazia no caso de nao existir um certo utilizador*/
    public NaoExisteUtilizadorException(){
        super();
    }
}