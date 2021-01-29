/**
 * classe que reponsavel pela apresentação
 */
package View;

import Model.Encomenda;
import Model.LinhaEncomenda;

import java.io.Serializable;
import java.util.List;

public class Apresentacao implements IApresentacao, Serializable {
    private final ApresentacaoMain am;
    private final ApresentacaoLogin al;
    private final ApresentacaoUtilizador au;
    private final ApresentacaoVoluntarioTransportadora avt;
    private final ApresentacaoNotificacoes an;
    private final ApresentacaoLoja alj;
    private final Output out;

    // ------------------------ Construtor ------------------------- \\

    public Apresentacao() {
        am = new ApresentacaoMain();
        al = new ApresentacaoLogin();
        au = new ApresentacaoUtilizador();
        avt = new ApresentacaoVoluntarioTransportadora();
        an = new ApresentacaoNotificacoes();
        alj = new ApresentacaoLoja();
        out = new Output();
    }

    // ------------------------ Apresentação Main ------------------------- \\

    /**
     * Apresenta menu de boas-vindas
     */
    public void welcome() {
        am.welcome();
    }

    /**
     * Apresenta menu login/registar
     */
    public void printMainMenuLogIn() {
        am.printMainMenuLogIn();
    }

    /**
     * Apresenta menu logOut
     */
    public void printMainMenuLogOut(String type, int numN) {
        am.printMainMenuLogOut(type, numN);
    }

    /**
     * Apresenta menu consultas
     */
    public void printMenuConsultas() {
        am.printMenuConsultas();
    }

    /**
     * Apresenta erro comando inválido
     */
    public void printErroComandoInvalido(){
        am.printErroComandoInvalido();
    }

    /**
     * Apresenta messagem ficheiro carregado
     */
    public void printFicheiroCarregado(String file){
        am.printFicheiroCarregado(file);
    }

    /**
     * Apresenta mensagem ficheiro guardado
     */
    public void printFicheiroGuardado(String file){
        am.printFicheiroGuardado(file);
    }

    /**
     * Apresenta mensagem de saida
     */
    public void printSair() {
        System.out.println("A Sair do Programa");
    }

    // ------------------------ Apresentação Login ------------------------- \\

    /**
     * Apresenta menu login
     */
    public void printMenuLogin() {
        al.printMenuLogin();
    }

    /**
     * Apresenta messagem código de acesso
     */
    public void printCodigoAcesso(String code) {
        al.printCodigoAcesso(code);
    }

    /**
     * Apresenta messagem login realizado com sucesso
     */
    public void printLoginSucesso() {
        al.printLoginSucesso();
    }

    /**
     * Apresenta messagem logOut realizado com sucesso
     */
    public void printLogoutSucesso() {
        al.printLogoutSucesso();
    }

    /**
     * Apresenta messagem registo realizado com sucesso
     */
    public void printRegistoSucesso() {
        al.printRegistoSucesso();
    }

    /**
     * Apresenta messagem dados inválidos
     */
    public void printErroDadosInvalidos() {
        al.printErroDadosInvalidos();
    }

    /**
     * Apresenta messagem pedir username
     */
    public void printPedirUsername() {
        al.printPedirUsername();
    }

    /**
     * Apresenta messagem pedir password
     */
    public void printPedirPassword() {
        al.printPedirPassword();
    }

    /**
     * Apresenta messagem pedir encomendas médicas
     */
    public void printPedirEncomendasMedicas() {
        al.printPedirEncomendasMedicas();
    }

    /**
     * Apresenta messagem pedir fila de espera
     */
    public void printPedirFilaEspera() {
        al.printPedirFilaEspera();
    }

    /**
     * Apresenta messagem pedir nome
     */
    public void printPedirNomeCompleto() {
        al.printPedirNomeCompleto();
    }

    /**
     * Apresenta messagem pedir tipo de conta
     */
    public void printPedirTipoConta() {
        al.printPedirTipoConta();
    }

    // ------------------------ Apresentação Utilizador ------------------------- \\

    /**
     * Apresenta menu do utilizador
     */
    public void printMenuUtilizador() {
        au.printMenuUtilizador();
    }

    /**
     * Apresenta list de encomendas
     */
    public void printEncomendas(String message, List<Encomenda> arr)  {
        au.printEncomendas(message, arr);
    }

    /**
     * Apresenta fatura
     */
    public void printFatura(Encomenda enc)  {
        au.printFatura(enc);
    }

    /**
     * Apresenta messagem erro de entrega
     */
    public void printErroEntrega() {
        au.printErroEntrega();
    }

    /**
     * Apresenta messagem sem encomendas
     */
    public void printErroSemEncomenda(){
        au.printErroSemEncomenda();
    }

    /**
     * Apresenta messagem encomenda entregue pelo voluntario
     */
    public void printEncomendaEntregueVol(String code, String nome, double tempo) {
        au.printEncomendaEntregueVol(code, nome, tempo);
    }

    /**
     * Apresenta messagem encomenda entregue
     */
    public void printEncomendaEntregue(String code, String tipo, String nome, double preco, double tempo) {
        au.printEncomendaEntregue(code, tipo, nome, preco, tempo);
    }

    /**
     * Apresenta messagem encomenda aceite
     */
    public void printEncomendaAceite() {
        au.printEncomendaAceite();
    }

    /**
     * Apresenta messagem encomenda cancelada
     */
    public void printCompraCancelada() {
        au.printCompraCancelada();
    }

    /**
     * Apresenta messagem compra em espera
     */
    public void printCompraEspera() {
        au.printCompraEspera();
    }

    /**
     * Apresenta messagem pedir para classificar
     */
    public void printPedirClassificar() {
        au.printPedirClassificar();
    }

    /**
     * Apresenta messagem encomenda em stand-by
     */
    public void printEncomendaStandBy(String code){
        au.printEncomendaStandBy(code);
    }

    /**
     * Apresenta messagem pedir encomendas
     */
    public String pedirEncomenda() {
        return au.pedirEncomenda();
    }

    /**
     * Apresenta messagem encomenda inválida
     */
    public void printEncomendaInvalida() {
        au.printEncomendaInvalida();
    }

    // ------------------------ Apresentação Voluntario Transportadora ------------------------- \\

    /**
     * Apresenta menu voluntario
     */
    public void printMenuVoluntario() {
        avt.printMenuVoluntario();
    }

    /**
     * Apresenta menu transportadora
     */
    public void printMenuTransportadora() {
        avt.printMenuTransportadora();
    }

    /**
     * Apresenta mensagem estafeta disponivel
     */
    public void printEstafetaDisponivel() {
        avt.printEstafetaDisponivel();
    }

    /**
     * Apresenta mensagem estafeta indisponivel
     */
    public void printEstafetaIndisponivel() {
        avt.printEstafetaIndisponivel();
    }

    /**
     * Apresenta preço
     * @param preco preço
     */
    public void printEstafetaPreco(double preco) {
        avt.printEstafetaPreco(preco);
    }

    /**
     * Apresenta faturação
     * @param faturacao faturação
     */
    public void printEstafetaFaturacao(double faturacao) {
        avt.printEstafetaFaturacao(faturacao);
    }

    /**
     * Apresenta classificação do estafeta
     * @param classificacao classificação
     */
    public void printEstafetaClassicacao(double classificacao) {
        avt.printEstafetaClassicacao(classificacao);
    }

    /**
     * Apresenta mensagem sem encomendas
     */
    public void printSemEncomendas(){
        avt.printSemEncomendas();
    }

    /**
     * Apresenta mensagem encomenda recusada
     */
    public void printEncRecusada(){
        avt.printEncRecusada();
    }

    // ------------------------ Apresentacao Loja ------------------------- \\

    /**
     * Apresenta menu loja
     */
    public void printMenuLoja() {
        alj.printMenuLoja();
    }

    /**
     * Apresenta menu loja indisponivel
     */
    public void printMenuLojaIndisponivel() {
        alj.printMenuLojaIndisponivel();
    }

    /**
     * Apresenta mensagem compra aceite
     * @param encCode encCode
     */
    public void printCompraAceite(String encCode) {
        alj.printCompraAceite(encCode);
    }

    /**
     * Apresenta mensagem compra recusada
     * @param encCode encCode
     */
    public void printCompraRecusada(String encCode) {
        alj.printCompraRecusada(encCode);
    }

    // ------------------------ Apresentacao Notificacoes ------------------------- \\

    /**
     * Apresenta notificações
     * @param not   notificação
     * @param type  tipo
     * @param page  pagina
     * @param max   pagina max
     */
    public void notifTable(String not, int type, int page, int max) {
        an.notifTable(not, type, page, max);
    }

    /**
     * Apresenta mensagem sem notificações
     */
    public void printEmptyNot() {
        an.printEmptyNot();
    }

    /**
     * Retorna notificação ao utilizador de loja aceite
     * @param storeCode storeCode
     * @return          notificação
     */
    public String notificacaoUtilizadorLojaAceite(String storeCode) {
        return an.notificacaoUtilizadorLojaAceite(storeCode);
    }

    /**
     * Retorna notificação ao utilizador de loja recusada
     * @param storeCode storeCode
     * @return          notificação
     */
    public String notificacaoUtilizadorLojaRecusado(String storeCode) {
        return an.notificacaoUtilizadorLojaRecusado(storeCode);
    }

    /**
     * Retorna notificação ao utilizador de voluntário recusado
     * @param code voluntário Code
     * @return     notificação
     */
    public String notificacaoUtilizadorVoluntarioRecusado(String code) {
        return an.notificacaoUtilizadorVoluntarioRecusado(code);
    }

    /**
     * Retorna notificação ao utilizador de entrega da transportadora
     * @param transCode transpCode
     * @param encCode   encCode
     * @return          notificação
     */
    public String notificacaoUtilizadorEntregaTransportadora(String transCode, String encCode) {
        return an.notificacaoUtilizadorEntregaTransportadora(transCode, encCode);
    }

    /**
     * Retorna notificação ao utilizador de entrega do voluntário
     * @param transCode transpCode
     * @param encCode   encCode
     * @return          notificação
     */
    public String notificacaoUtilizadorEntregaVoluntario(String transCode, String encCode) {
        return an.notificacaoUtilizadorEntregaVoluntario(transCode, encCode);
    }

    /**
     * Retorna notificação ao voluntário de nova entrega
     * @param encCode   encCode
     * @param userCode  userCode
     * @return          notificação
     */
    public String notificacaoVoluntarioNovaEntrega(String encCode, String userCode) {
        return an.notificacaoVoluntarioNovaEntrega(encCode, userCode);
    }

    /**
     * Retorna notificação à loja de nova compra
     * @param encCode   encCode
     * @param userCode  userCode
     * @return          notificação
     */
    public String notificacaoLojaNovaCompra(String encCode, String userCode) {
        return an.notificacaoLojaNovaCompra(encCode, userCode);
    }

    /**
     * Retorna notificação ao utilizador de  aceitar transportadora
     * @param encCode       encCode
     * @param transpCode    transpCode
     * @return              notificação
     */
    public String notificacaoUtilizadorAceitarTransportadora(String encCode, String transpCode) {
        return an.notificacaoUserNovaTransportadora(encCode, transpCode);
    }

    /**
     * Retorna notificação para a transportadora
     * @param encCode  encCode
     * @param userCode userCode
     * @param aceite   aceite
     * @return         notificação
     */
    public String notificacaoTransportadora(String encCode, String userCode,boolean aceite) {
        if(aceite)
            return an.notificacaoTransportadoraAceite(encCode, userCode);
        else
            return an.notificacaoTransportadoraRecusada(encCode, userCode);
    }

    // ------------------------ Outros métodos ------------------------- \\

    /**
     * Apresenta mensagem e arr
     * @param message mensagem
     * @param arr     List de string
     */
    public void printArray(String message, List<String> arr) {
        out.printArray(message, arr);
    }

    /**
     * Apresenta tabela com a mensagem e o arr
     * @param message mensagem
     * @param arr     list de string
     */
    public void printTable(String message, List<String> arr) {
        out.printTable(message, arr);
    }

    /**
     * Apresenta mensagem
     * @param message mensagem
     */
    public void printMessage(String message) {
        out.printMessage(message);
    }

    public String pedirPrimeiraData() {
        return "Introduza a 1º data de tipo(02-12-2018)";
    }

    public String pedirSegundaData() {
        return "Introduza a 2º data de tipo(02-12-2018)";
    }
}
