package View;

import Controller.*;
public interface Viewable {

    /**
     * Funcao que imprime a lista de comandos disponiveis para o utilizador
     */
    void printInfo();

    /**
     * Funcao responsavel pela execução dos comandos
     */
    void run();

    /**
     * Define o controlador da classe
     */
    void setController(Controller controller);

    /**
     * Retorna o controlador da classe
     */
    Controller getController();
}
