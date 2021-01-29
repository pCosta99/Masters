public class ExisteCodSistemaException extends Exception{
    /** Mensagem no caso de existir um certo codigo*/
    public ExisteCodSistemaException(String msg){
        super(msg);
    }
    /** Mensagem vazia no caso de existir um certo codigo*/
    public ExisteCodSistemaException(){
        super();
    }
}