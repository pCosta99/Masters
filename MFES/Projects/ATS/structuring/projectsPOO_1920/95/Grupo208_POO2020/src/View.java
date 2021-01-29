import Enums.Tipo;

import java.io.Serializable;

import static java.lang.System.out;

/**
 * Classe que auxilia a interação com o utilizador.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class View implements Serializable {
    private String codigo;

    /**
     * Construtor do View.
     */
    public View(){
        this.codigo = "";
    }

    /**
     * Função que modifica o código.
     * @param codigo, Novo código.
     */
    public void setCodigo(String codigo){
        this.codigo = codigo;
    }

    /**
     * Função que retorna o código.
     * @return Código.
     */
    public String getCodigo(){
        return this.codigo;
    }

    /**
     * Função que imprime o tipo fornecido e o código.
     * @param tipo, Tipo a imprimir.
     */
    public void printAtual(Tipo tipo){
        out.printf("Código de %s atual: %s\n",tipo,this.codigo);
    }

    /**
     * Função que imprime uma mensagem de sucesso de registo de um tipo fornecido e apresenta ainda o seu código.
     * @param tipo, Tipo a imprimir.
     */
    public void sucessoRegisto(Tipo tipo){
        out.printf("Foi criado um(a) novo %s com o código %s!\n",tipo,this.codigo);
    }

    /**
     * Função que imprime uma mensagem fornecida.
     * @param msg, Mensagem fornecida.
     */
    public static void printSpacedMessage(String msg){
        out.println("\n" + msg + "\n");
    }

    /**
     * Função que imprime uma mensagem de regresso ao menu anterior.
     */
    public static void printMenuAnterior(){
        View.printSpacedMessage("Regressando ao menu anterior!");
    }

    /**
     * Função que imprime uma mensagem de LogOut.
     */
    public static void printLogOut(){
        View.printSpacedMessage("Logging out...");
    }

    /**
     * Função que imprime uma mensagem de opção inválida.
     */
    public static void printOpcaoInvalida(){
        View.printSpacedMessage("Opção Inválida!");
    }

    /**
     * Função que imprime uma linha em branco.
     */
    public static void printBlankLine(){
        out.println();
    }
}