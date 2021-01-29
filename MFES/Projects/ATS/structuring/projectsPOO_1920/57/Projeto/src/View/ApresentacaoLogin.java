package View;

import java.io.Serializable;

public class ApresentacaoLogin implements Serializable {
    private final Output out;

    public ApresentacaoLogin() {
        out = new Output();
    }

    /**
     * Apresenta menu login
     */
    public void printMenuLogin() {
        out.printMenus((new String[]{"Login", "Registar"}),"MENU LOGIN",1);
    }

    /**
     * Apresenta codigo de acesso
     * @param code code
     */
    public void printCodigoAcesso(String code) {
        System.out.println("Códido de Acesso: " + code);
    }

    /**
     * Apresenta login com sucesso
     */
    public void printLoginSucesso() {
        System.out.println("Login efetuado com sucesso");
    }

    /**
     * Apresenta logout com sucesso
     */
    public void printLogoutSucesso() {
        System.out.println("Logout efetuado com sucesso");
    }

    /**
     * Apresenta registo com sucesso
     */
    public void printRegistoSucesso() {
        System.out.println("Registo efetuado com sucesso");
    }

    /**
     * Apresenta dados inválidos
     */
    public void printErroDadosInvalidos() {
        System.out.println("Dados inválidos");
    }

    /**
     * Apresenta pedir username
     */
    public void printPedirUsername() {
        System.out.println("Introduza o username: ");
    }

    /**
     * Apresenta pedir pass
     */
    public void printPedirPassword() {
        System.out.println("Introduza a password: ");
    }

    /**
     * Apresenta pedir encomendas médicas
     */
    public void printPedirEncomendasMedicas() {
        System.out.println("Pode transportar encomendas médicas? (S/N): ");
    }

    /**
     * Apresenta pedir fila de espera
     */
    public void printPedirFilaEspera() {
        System.out.println("A loja tem informação sobre a fila de espera? (S/N): ");
    }

    /**
     * Apresenta pedir nome
     */
    public void printPedirNomeCompleto() {
        System.out.println("Introduza o nome completo: ");
    }

    /**
     * Apresenta pedir tipo conta
     */
    public void printPedirTipoConta() {
        System.out.println("Introduza o tipo de conta (Voluntario / Transportadora / Utilizador / Loja): ");
    }
}
