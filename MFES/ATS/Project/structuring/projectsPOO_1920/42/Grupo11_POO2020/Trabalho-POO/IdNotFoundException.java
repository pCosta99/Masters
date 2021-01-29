//classe de excepção para o caso de um ID não ser encontrado no sistema

public class IdNotFoundException extends Exception{
    public IdNotFoundException(){
        super();
    }

    public IdNotFoundException(String msg){
        super(msg);
    }
}

