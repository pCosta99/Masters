/**
 * Interface de utilizadores
 * Usada para assegurar validaçao
 */
public interface IUtilizador {
    public static boolean isValid(String username){
        return (username.matches("[u][0-9]+"));
    }

    /**
     * Metodo get para aceder ao id do Utilizador
     * @return userID
     */
    String getUserID();

    /**
     * Metodo clone
     * @return copia da interface do utilizador
     */
    IUtilizador clone();
}
