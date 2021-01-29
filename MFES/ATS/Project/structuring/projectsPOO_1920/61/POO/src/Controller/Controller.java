package Controller;

import Exceptions.ComandoInvalidoException;
import Model.Sistema;

import java.io.IOException;
import java.util.List;

/**
 * Interface do tipo Controller
 */
public interface Controller {
    /**
     * Atualiza o Sistema de um Controller
     * @param sys novo sistema para inserir
     */
    void setSistema(Sistema sys);

    /**
     * Devolve o Sistema de um Controller
     * @return Sistema do Controller
     */
    Sistema getSistema();

    /**
     * Executa um comando no sistema
     * @param s informação relativa ao que se pretende executar
     * @return Lista de strings com atualização de estado ou outras informações relevantes
     * @throws IOException Erro de email ou password
     * @throws ComandoInvalidoException Comando Inválido
     */
    List<String> execute(List<String> s) throws IOException, ComandoInvalidoException;
}
