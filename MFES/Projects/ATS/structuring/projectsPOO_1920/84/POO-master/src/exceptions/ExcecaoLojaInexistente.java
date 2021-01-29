package exceptions;

public class ExcecaoLojaInexistente extends Exception {

    public ExcecaoLojaInexistente(){ super(); }

    public ExcecaoLojaInexistente(String s){
        super(s);
    }
}