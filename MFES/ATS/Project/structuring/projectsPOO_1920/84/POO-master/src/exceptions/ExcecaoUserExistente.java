package exceptions;

public class ExcecaoUserExistente extends Exception {

    public ExcecaoUserExistente(){
        super();
    }

    public ExcecaoUserExistente(String s){
        super(s);
    }
}