package TrazAqui;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

public class UI {
    /**
     * Imprime a string recebida
     * @param s String
     */
    public static void print(String s) {
        System.out.println(s);
    }

    /**
     * Imprime o menu inicial
     */
    public static void printMenuInicial(){
        System.out.println("----------------MENU-PRINCIPAL----------------");
        System.out.println("0 - Sair");
        System.out.println("1 - Login");
        System.out.println("2 - Criar uma nova conta");
        System.out.print("Opcao: ");
    }

    /**
     * Imprime opcao para inserir email
     */
    public static void printInsiraEmail(){
        System.out.print("Email: ");
    }

    /**
     * Imprime erro de input
     */
    public static void printIncorrectInput() {
        System.out.print("Opcão inválida, tente novamente: ");
    }

    /**
     * Imprime opcao insira password
     */
    public static void printInsiraPassword(){
        System.out.print("Password: ");
    }

    /**
     * Imprime o menu de utilizador
     */
    public static void printMenuUtilizador() {
        System.out.println("----------------MENU-UTILIZADOR----------------");
        System.out.println("1 - Efetuar uma encomenda ");
        System.out.println("2 - Ver histórico de encomendas ");
        System.out.println("3 - Aceitar pedidos ");
        System.out.println("4 - Classificar voluntário");
        System.out.println("5 - Top 10 utilizadores");
        System.out.println("6 - Top 10 transportadoras");
        System.out.println("7 - Terminar sessao");
        System.out.println("0 - Sair");
    }

    /**
     * Imprime opcao fazer descricao
     */
    public static void printFazerDescricao() {
        System.out.print("Faça a descrição do produto: ");
    }

    /**
     * Imprime opcao indicar o preco
     */
    public static void printIndicarPreco() {
        System.out.print("Indique o preço: ");
    }

    /**
     * Imprime opcao indicar quantidade
     */
    public static void printIndicarQuant() {
        System.out.print("Indique a quantidade: ");
    }

    /**
     * Imprime opcao indicar fagil
     */
    public static void printIndicarFragil() {
        System.out.print("Indique se o produto é frágil ou não (escreva true se sim, false se não): ");
    }

    /**
     * Imprime opcao indicar codigo do produto
     */
    public static void printIndiqueCodProd() {
        System.out.print("Indique o código do produto: ");
    }

    /**
     * Imprime a opcao certificado
     */
    public static void printCertificado() {
        System.out.println("Tem certificado: ");
    }

    /**
     * Imprime a opcao nif
     */
    public static void printNif() {
        System.out.println("Nif: ");
    }

    /**
     * Imprime a opcao deseja mais produtos
     */
    public static void printDesejaMaisProd() {
        System.out.print("Deseja encomendar mais produtos? (escreva true se sim, false se não)");
    }

    /**
     * Imprime o historico de encomendas
     * @param enc Map<String,Encomenda>
     * @param opcao int
     * @param u Utilizador
     * @param inicio LocalDateTime
     * @param fim LocalDateTime
     */
    public static void printHistoricoEncomendas(Map<String,Encomenda> enc, int opcao,Utilizador u, LocalDateTime inicio,LocalDateTime fim) {
        if(opcao==1 || opcao ==2 || opcao == 3 || opcao ==4) {
            if(enc.size()>0) {
                int i=0;
                if (opcao == 1) {
                    for (Map.Entry<String, Encomenda> map : enc.entrySet()) {
                        if (map.getValue().getEstafeta().contains("t")) {
                            System.out.println(map.getKey() + ":" + map.getValue());
                            i++;
                        }
                    }
                    if(i==0) UI.print("Não existe nenhuma encomenda para apresentar");
                }
                if (opcao == 2) {
                    for (Map.Entry<String, Encomenda> map : enc.entrySet()) {
                        if (map.getValue().getEstafeta().contains("v")) {
                            System.out.println(map.getKey() + ":" + map.getValue());
                            i++;
                        }
                    }
                    if(i==0) UI.print("Não existe nenhuma encomenda para apresentar");
                }
                if (opcao == 3) {
                    UI.printEncomendas(u.procuraPor(inicio, fim));
                }
                if(opcao == 4){
                    for (Map.Entry<String, Encomenda> map : enc.entrySet()) {
                            System.out.println(map.getKey() + ":" + map.getValue());
                            i++;
                        }
                    if(i==0) UI.print("Não existe nenhuma encomenda para apresentar");
                }
            }
            else UI.print("Não existe nenhuma encomenda para apresentar");
        }
        else UI.print("Opcão inválida");
    }

    /**
     * Imprime o menu de voluntario
     */
    public static void printMenuVoluntario() {
        System.out.println("------------------MENU-VOLUNTARIO------------------");
        System.out.println("1 - Mudar disponibilidade");
        System.out.println("2 - Escolher encomenda para ir buscar");
        System.out.println("3 - Alterar o raio de ação");
        System.out.println("4 - Ver classificação do estafeta");
        System.out.println("5 - Top 10 utilizadores");
        System.out.println("6 - Top 10 Transportadoras");
        System.out.println("7 - Terminar sessao");
        System.out.println("0 - Sair");
        System.out.print("Opcao: ");
    }

    /**
     * Imprime o menu da transportadora
     */
    public static void printMenuTransportadora() {
        System.out.println("----------------MENU-TRANSPORTADORA----------------");
        System.out.println("1  - Mudar disponibilidade");
        System.out.println("2  - Determinar preço da encomenda");
        System.out.println("3  - Transportar encomenda");
        System.out.println("4  - Top 10 utilizadores");
        System.out.println("5  - Top 10 Transportadoras");
        System.out.println("6  - Indicar o total faturado");
        System.out.println("7  - Alterar raio de ação");
        System.out.println("8  - Alterar preço por kilometro");
        System.out.println("9  - Ver classificação do estafeta");
        System.out.println("10 - Terminar sessao");
        System.out.println("0  - Sair");
        System.out.print("Opcao: ");
    }

    /**
     * Imprime o menu da loja
     */
    public static void printMenuLoja() {
        System.out.println("---------------------MENU-LOJA---------------------");
        System.out.println("0) Sair");
        System.out.println("1) Encomendas disponiveis para serem entregues");
        System.out.println("2) Indicar tamanho da fila");
        System.out.println("3) Top 10 utilizadores");
        System.out.println("4) Top 10 Transportadoras");
        System.out.println("5) Terminar sessão");
    }

    /**
     * Imprime a lista de strings
     * @param r List<String>
     */
    public static void printTop10(List<String> r) {
        System.out.println(" -> Top 10:");
        for(String nome: r)
            System.out.println(nome);
    }

    /**
     * Imprime a lista a encomendas, exceto as medicas
     * @param enc List<Encomenda>
     */
    public static void printEncomendasNormais(List<Encomenda> enc) {
        StringBuilder sb;
        if(enc.size() != 0) {
            for (Encomenda e : enc) {
                if (!e.getMedicamentos()) {
                    sb = new StringBuilder();
                    sb.append("Código: ").append(e.getCod()).append(" Conteúdo: ").append(e.getProdutos());
                    System.out.println(sb.toString());
                }
            }
        }
        else System.out.println(" -> Sem encomendas a apresentar!");
    }

    /**
     * Imprime a lista de encomendas
     * @param enc List<Encomenda>
     */
    public static void printEncomendas(List<Encomenda> enc) {
        StringBuilder sb;
        if(enc.size() != 0) {
            for (Encomenda e : enc) {
                sb = new StringBuilder();
                sb.append("Código: ").append(e.getCod()).append(" Conteúdo: ").append(e.getProdutos());
                System.out.println(sb.toString());
            }
        }
        else System.out.println(" -> Sem encomendas a apresentar!");
    }

    /**
     * Imprime o total faturado
     * @param f double
     */
    public static void printTotFat(double f) {
        StringBuilder sc = new StringBuilder();
        sc.append(" -> Total faturado: ").append(f);
        System.out.println(sc.toString());
    }

    /**
     * Imprime o preco
     * @param p double
     */
    public static void printPreco(double p) {
        System.out.println(p);
    }

    /**
     * Imprime a mensagem de saida
     */
    public static void goodbye() {
        System.out.println(" -> A sair...\nObrigado por usar a nossa aplicação! :)");
    }

    /**
     * Imprime as lojas
     * @param lojas Map<String,Loja>
     */
    public static void printLojas(Map<String,Loja> lojas){
        for(Map.Entry<String,Loja> map: lojas.entrySet()){
            System.out.println("Código da loja: " + map.getKey());
            System.out.println("Nome da loja: " + map.getValue().getNome() + "  Localização: " + map.getValue().getLocalizacao());
        }
    }

    /**
     * Imprime a lista de
     * @param e List<Encomenda>
     * @param m Menu
     * @param trans String
     */
    public static void printPedidosEncomenda(List<Encomenda> e,Menu m, String trans){
        int i=0;
        for(Encomenda enc: e){
            if(enc.getEstafeta().contains("t") ) {
                System.out.println("Preço: " + String.format("%.2f", m.getPreco(enc, trans)) + ", Código: " + enc.getCod() + " -> " + enc);
                i++;
            }
        }
    }

    /**
     * Imprime tipo incorreto
     */
    public static void printTipoIncorreto(){
        System.out.println("Input incorreto");
    }

    /**
     * Imprime sub menu da transportadora
     */
    public static void printDesejaTransVolTempo(){
        System.out.println("1 - Ver encomendas transportadas por transportadoras");
        System.out.println("2 - Ver encomendas transportadas por voluntários");
        System.out.println("3 - Ver encomendas transportadas num intervalo de tempo");
        System.out.println("4 - Ver todas as encomendas transportadas");
    }

    /**
     * Imprime insira codigo
     */
    public static void printInsiraCod() {
        System.out.println("Código: ");
    }

    /**
     * Imprime o tipo de registo
     */
    public static void printTipoRegisto() {
        System.out.println("Regista-se como Utilizador, Loja, LojaFilaEspera, Transportadora ou Voluntário?: ");
    }

    /**
     * Imprime opcao insira nome
     */
    public static void printInsiraNome() {
        System.out.println("Nome: ");
    }

    /**
     * Imprima a opcao insira latitude
     */
    public static void printInsiraLatitude() {
        System.out.println("Latitude: ");
    }

    /**
     * Imprima a opcao insira latitudo
     */
    public static void printInsiraLongitude() {
        System.out.println("Longitude: ");
    }

    /**
     * Imprime a formato invalido
     */
    public static void printFormatoInvalido() {
        System.out.println("Formato inválido!");
    }

    /**
     * Imprime a insira data incial
     */
    public static void printDataInicial() {
        System.out.println("Insira a data inicial da procura ( formato yyyy-mm-dd HH:mm)");
    }

    /**
     * Imprime insira data final 
     */
    public static void printDataFinal() {
        System.out.println("Insira a data final da procura ( formato yyyy-mm-dd HH:mm)");
    }

    /**
     * Imprime tamanho da fila de espera 
     * @param tam int
     */
    public static void printTamanhoFilaEspera(int tam) {
        System.out.println("Tamanho da lista de espera: " + tam);
    }

    /**
     * Imprime loja sem fila de espera
     */
    public static void printNtemFilaEspera(){
        System.out.println("Esta loja não tem fila de espera.");
    }

    /**
     * Imprime encomenda inexistente
     */
    public static void printEncomendaInex(){
        System.out.println(" -> Encomenda inexistente.");
    }

    /**
     * Imprime opcao selecionar raio
     */
    public static void printSelectRaio() {
        System.out.print("Selecione o raio: ");
    }

    /**
     * Imprime opcao preco por kilometro
     */
    public static void printSelectPrecoKM() {
        System.out.print("Selecione o preço por km: ");
    }

    /**
     * Imprime encomenda em transporte
     */
    public static void printEncomendaEmTrans(){
        System.out.println(" -> Encomenda em transporte.");
    }

    /**
     * Imprime insira codigo de encomenda
     */
    public static void printInsiraCodEnc(){
        System.out.println("Código da encomenda: ");
    }

    /**
     * Imprime quando nao existem encomendas
     */
    public static void print0NEncomendas(){
        System.out.println("Pressione enter caso não existam encomendas.");
    }

    /**
     * Imprime alterar disponibilidade
     */
    public static void printMudeDisp(){
        System.out.println(" -> Altere a sua disponibilidade.");
    }

    /**
     * Imprime disponivel
     */
    public static void printDisponivel(){
        System.out.println(" -> Disponível");
    }

    /**
     * Imprime indisponivel
     */
    public static void printIndisponivel(){
        System.out.println(" -> Indisponível");
    }

    /**
     * Imprime nao existem encomendas medicas
     */
    public static void printNoMedica(){
        System.out.println("Não pode transportar medicamentos!");
    }

    /**
     * Imprime nao existem encomendas a aceitar
     */
    public static void print0encParaAceitar(){
        System.out.println("Não existem encomendas para serem aceites.");
    }

    /**
     * Imprime insira classificacao
     */
    public static void printInsiraClass(){
        System.out.println("Indique a classificação que deseja dar, de 1 a 5: ");
    }

    /**
     * Imprime insira codigo de encomenda a aceitar
     */
    public static void printCodEncAceitar() {
        System.out.println("Indique o código da encomenda que deseja tratar:");
    }

    /**
     * Imprime insira codigo da loja
     */
    public static void printInsiraCodLoja(){
        System.out.println("Insira o código da loja: ");
    }

    /**
     * Imprime insira peso 
     */
    public static void printInsiraPeso(){
        System.out.println("Indique o peso: ");
    }

    /**
     * Imprime insira tipo de encomenda
     */
    public static void printTransMedica(){
        System.out.println("Transporta medicamentos(escreva true se sim, false se não)");
    }

    /**
     * Imprime codigo de encomenda ja existe
     */
    public static void printCodEncJaExiste(){
        System.out.println("Código da encomenda já existente");
    }

    /**
     * Imprime opcoes de escolha
     */
    public static void printAceitaOuNao() {
        System.out.println("1 - Aceitar");
        System.out.println("2 - Rejeitar");
        System.out.println("0 - Sair");
        System.out.print("Opcao: ");
    }

    /**
     * Imprime classificacao media 
     * @param clas Double
     */
    public static void printClassMedia(Double clas){
        System.out.println("O estafeta tem a classificação de " +clas);
    }

    /**
     * Imprime a mensagem que não há voluntários para classificar
     */
    public static void print0ClassVol() {
        System.out.println("Sem voluntários para classificar!");
    }

    /**
     * Imprime opção para selecionar o código de encomenda para classificar
     */
    public static void printCodEncomendaClass() {
        System.out.print("Indique o código de encomenda a classificar: ");
    }
}


