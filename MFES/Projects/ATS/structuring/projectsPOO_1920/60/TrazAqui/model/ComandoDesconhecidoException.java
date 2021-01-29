package model;

public class ComandoDesconhecidoException extends Exception
{
    public ComandoDesconhecidoException(String s){
        super(s);
    }
}