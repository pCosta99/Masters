package exceptions;

public class ExcecaoSemFacturacao extends Exception {

    public ExcecaoSemFacturacao(){ super(); }

    public ExcecaoSemFacturacao(String s){
        super(s);
    }
}