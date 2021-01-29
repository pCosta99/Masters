/**
 * Classe responsavel pelas apresentação dos menus
 */
package View;

import java.io.Serializable;

public class ApresentacaoMain implements Serializable {
    private final Output out;

    public ApresentacaoMain() {
        out = new Output();
    }

    /**
     * Apresenta menu welcome
     */
    public void welcome() {
        System.out.println(" _____                 _               _");
        System.out.println("|_   _| __ __ _ ____  / \\   __ _ _   _(_)");
        System.out.println("  | || '__/ _` |_  / / _ \\ / _` | | | | |");
        System.out.println("  | || | | (_| |/ / / ___ \\ (_| | |_| | |");
        System.out.println("  |_||_|  \\__,_/___/_/   \\_\\__, |\\__,_|_|");
        System.out.println("                              |_|");
        System.out.println("Bem vindo à aplicação traz aqui.");
        System.out.println("Pressione qualquer tecla para continuar.");
    }

    /**
     * Apresenta menu login
     */
    public void printMainMenuLogIn() {
        out.printMenus((new String[]{"Login/Registar", "Gravar para um Ficheiro", "Carregar de um ficheiro"}),"MENU PRINCIPAL",0);
    }

    /**
     * Apresenta menu logout
     * @param type type
     * @param numN numero notificações
     */
    public void printMainMenuLogOut(String type, int numN) {
        out.printMenus((new String[]{"Logout","Menu " + type, "Consultas", "Notificações (" + numN + ")"}),"MENU PRINCIPAL",0);
    }

    /**
     * Apresenta menu consultas
     */
    public void printMenuConsultas() {
        out.printMenus((new String[]{"Top Utilizadores do Sistema", "Top Transportadoras do Sistema"}),"MENU CONSULTAS",1);
    }

    /**
     * Apresenta mensagem comando inválido
     */
    public void printErroComandoInvalido(){
        System.out.println("Comando Inválido");
    }

    /**
     * Apresenta mensagem ficheiro carregado
     * @param file ficheiro
     */
    public void printFicheiroCarregado(String file){
        System.out.println("Ficheiro " + file + " carregado");
    }

    /**
     * Apresenta mensagem ficheiro guardado
     * @param file ficheiro
     */
    public void printFicheiroGuardado(String file){
        System.out.println("Ficheiro " + file + " guardado");
    }

    /**
     * Apresenta sair
     */
    public void printSair() {
        System.out.println("A Sair do Programa");
    }

}
