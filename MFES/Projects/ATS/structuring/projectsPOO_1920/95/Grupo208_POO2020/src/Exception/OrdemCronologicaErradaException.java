package Exception;

import java.time.temporal.Temporal;

/**
 * Exception que é invocada quando ocorrem erros com a ordem cronológica.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class OrdemCronologicaErradaException extends Exception{

    /**
     * Utilizada quando a ordem cronológica está incorreta.
     */
    public OrdemCronologicaErradaException(){
        super("Ordem Cronológica errada!");
    }

    /**
     * Utilizada quando a order cronológica está incorreta.
     * @param supposedInf, Limite inferior.
     * @param supposedSup, Limite superior.
     */
    public OrdemCronologicaErradaException(Temporal supposedInf, Temporal supposedSup){
        super("" + supposedInf + " vem depois de " + supposedSup);
    }
}
