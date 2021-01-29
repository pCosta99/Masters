package Exception;

/**
 * Escreva a descrição da classe VoluntarioNotFoundException aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class VoluntarioNotFoundException extends Exception{

    /**
     * Utilizada quando o voluntário é inválido.
     * @param s, Código do voluntário é inválido.
     */
    public VoluntarioNotFoundException(){
        super("Voluntario Inválido!");
    }

    /**
     * Utilizada quando o voluntário é inválido.
     * @param s, Código do voluntário.
     */
    public VoluntarioNotFoundException(String s){
        super("Voluntário " + s + " não encontrado!");
    }
}