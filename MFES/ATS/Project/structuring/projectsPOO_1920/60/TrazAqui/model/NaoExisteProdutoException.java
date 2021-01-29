package model;

public class NaoExisteProdutoException extends Exception
{
    public NaoExisteProdutoException(String s){
        super(s);
    }
}