package View;

import java.io.Serializable;
import java.security.InvalidParameterException;

/**
 * Classe que processa exceções de uma forma simples.
 */
public interface ViewError{

    /**
     * Imprime uma dada exceção.
     *
     * @param e Exceçao a ser imprimida.
     */
    static void show(Exception e) {
        System.out.println(e.getMessage()+'\n');
    }

    /**
     * Imprime uma dada na sua totalidade.
     *
     * @param e Exceçao a ser imprimida.
     */
    static void fullShow(Exception e) {
        e.printStackTrace(System.out);
    }
}
