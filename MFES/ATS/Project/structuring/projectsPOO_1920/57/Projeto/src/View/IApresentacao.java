package View;

import Model.Encomenda;

import java.util.List;

public interface IApresentacao {
    public void welcome();
    public void printMainMenuLogIn();
    public void printMainMenuLogOut(String type, int numN);
    public void printMenuConsultas();
    public void printErroComandoInvalido();
    public void printFicheiroCarregado(String file);
    public void printFicheiroGuardado(String file);
    public void printSair();
    public void printMenuLogin();
    public void printCodigoAcesso(String code);
    public void printLoginSucesso();
    public void printLogoutSucesso();
    public void printRegistoSucesso();
    public void printErroDadosInvalidos();
    public void printPedirUsername();
    public void printPedirPassword();
    public void printPedirEncomendasMedicas();
    public void printPedirFilaEspera();
    public void printPedirNomeCompleto();
    public void printPedirTipoConta();
    public void printMenuUtilizador();
    public void printEncomendas(String message, List<Encomenda> arr);
    public void printFatura(Encomenda enc);
    public void printErroEntrega();
    public void printEncomendaEntregueVol(String code, String nome, double tempo);
    public void printEncomendaEntregue(String code, String tipo, String nome, double preco, double tempo);
    public void printEncomendaAceite();
    public void printCompraCancelada();
    public void printPedirClassificar();
    public void printEncomendaStandBy(String code);
    public String pedirEncomenda();
    public void printMenuVoluntario();
    public void printMenuTransportadora();
    public void printEstafetaDisponivel();
    public void printEstafetaIndisponivel();
    public void printEstafetaPreco(double preco);
    public void printEstafetaFaturacao(double faturacao);
    public void printEstafetaClassicacao(double classificacao);
    public void printSemEncomendas();
    public void printEncRecusada();
    public void printMenuLoja();
    public void printMenuLojaIndisponivel();
    public void notifTable(String not, int type, int page, int max);
    public void printEmptyNot();
    public void printArray(String message, List<String> arr);
    public void printTable(String message, List<String> arr);
    public void printMessage(String message);
}
