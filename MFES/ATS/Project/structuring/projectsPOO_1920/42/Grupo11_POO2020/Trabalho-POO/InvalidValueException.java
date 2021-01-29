//classe de excecção para o caso de serem lidos valores inválidos

public class InvalidValueException extends Exception {
    public InvalidValueException(){
        super();
    }

    public InvalidValueException(String msg){
        super(msg);
    }
}
