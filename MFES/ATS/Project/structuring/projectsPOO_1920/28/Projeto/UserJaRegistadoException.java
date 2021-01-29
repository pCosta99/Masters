/**
 Acontece quando um Usuário tenta se registarum nome já antes escolhido por outro.
*/

public class UserJaRegistadoException  extends Exception
{
    public  UserJaRegistadoException (String msg){
        super(msg);
    }
    public  UserJaRegistadoException (){
        super();
    }
}