public class PasseErradaException extends Exception{
    /**Mensagem no caso de a passe estiver errada*/ 
    public PasseErradaException(String msg){
        super(msg);
    }
    /** Mensagem vazia no caso de a palavra passe estiver incorreta*/
    public PasseErradaException(){
        super();
    }
}