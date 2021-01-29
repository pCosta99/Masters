package exceptions;

public class ExcecaoCoordenadasInvalidas extends Exception {

    public ExcecaoCoordenadasInvalidas(){ super(); }

    public ExcecaoCoordenadasInvalidas(String s){
        super(s);
    }
}