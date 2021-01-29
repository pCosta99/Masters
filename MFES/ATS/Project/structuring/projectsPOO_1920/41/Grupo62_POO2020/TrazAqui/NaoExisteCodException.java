public class NaoExisteCodException extends Exception{
    /** Mensagem no caso de nao existir um Código*/
    public NaoExisteCodException(String msg){
        super(msg);
    }
    /** Mensagem vazia no caso de nao existir um certo código*/
    public NaoExisteCodException(){
        super();
    }
}