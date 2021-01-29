/**
 *  Acontece quando vamos tentar escolher uma encomenda, 
 *  quer seja para aceitar, preparar ou transportar de uma lista vazia.
 */


public class ListaVaziaException extends Exception {

    public  ListaVaziaException (){
        super();
    }

    public  ListaVaziaException (String msg){
        super(msg);
    }
    
}