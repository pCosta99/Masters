/**
 * Acontece quando uma Transportadora ou Voluntário tenta mudar 
 * a sua disponibilidade para false enquanto estiver a transporta uma encomenda.
 */

public class MudancaDisponibilidadeException  extends Exception
{
    public  MudancaDisponibilidadeException (String msg){
        super(msg);
    }
}