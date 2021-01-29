public class ExisteLojaException extends Exception{
    public ExisteLojaException(String cod){
        super(String.valueOf(cod));
    }
}