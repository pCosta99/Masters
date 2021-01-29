package Exception;

/**
 * Exception que é invocada quando ocorrem erros com Empresas.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class EmpresaNotFoundException extends Exception {
    /**
     * Utilizada quando uma empresa é inválida.
     */
    public EmpresaNotFoundException(){
        super("Empresa Inválida!");
    }

    /**
     * Utilizada quando o código de uma empresa não é encontrado.
     */
    public EmpresaNotFoundException(String s){
        super("Empresa " + s + " não encontrada");
    }
}