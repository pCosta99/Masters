package View;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import Model.IEncomenda;
import Model.ITransportadora;
import Model.IUtilizador;

/** Funções relacionadas com a apresentação ao utilizador. */
public class View {

    /** Limpa a consola. */
    public static void clear() {
        System.out.print("\033[H\033[2J");  
        System.out.flush();  
    }


    /** Apresenta o nome do programa. */
    public static void intro() {
        System.out.print("\n  ╒═══════════════════════════════════╕\n" 
                         + "  │             TrazAqui!             │\n"
                         + "  ╘═══════════════════════════════════╛\n\n");
    }


    /** Imprime as opções ao iniciar o programa. */
    public static void login() {
        System.out.print(" Opções:\n"
                       + "  1. Iniciar sessão.\n"
                       + "  2. Registar uma nova conta.\n");
    }

    /** Apresenta o resultado do login. */
    public static void loginResultado(int res) {
        switch (res) {
            case 0: System.out.print("\n Sucesso!\n");                  break;
            case 1: System.out.print("\n Erro: Email inexistente.\n");  break;
            case 2: System.out.print("\n Erro: Senha incorreta.\n");    break;
            case 3: System.out.print("\n Erro: Email já existente.\n"); break;
        }
    }




    
    /**
     * Imprime a mensagem de boas-vindas.
     * @param id Identificador do utilizador.
     */
    public static void greeting(String id) {
        System.out.print(" Bem-vind@ " + id + "!\n\n");
    }

    /** Imprime as opções do menu de administrador. */
    public static void opcoesAdmin() {
        System.out.print(" Opções:\n"
                       + "  1. Registar uma entidade.\n"
                       + "  2. Ver as encomendas de um dado distribuidor.\n"
                       + "  3. Ver total faturado por uma dada transportadora num dado período.\n"
                       + "  4. Ver os 10 utilizadores que mais utilizam o sistema.\n"
                       + "  5. Ver as 10 transportadoras que mais utilizam o sistema.\n"
                       + "  6. Ver encomendas associadas a uma entidade\n");
    }

    /** Imprime as opções do menu de loja. */
    public static void opcoesLoja() {
        System.out.print(" Opções:\n"
                       + "  1. Inserir informação de encomenda pronta a ser entregue.\n"
                       + "  2. Ver lista de encomendas pendentes.\n");
    }

    /** Imprime as opções do menu de transportadora. */
    public static void opcoesTransportadora() {
        System.out.print(" Opções:\n"
                       + "  1. Ver encomendas pendentes.\n"
                       + "  2. Indicar que encomenda foi entregue.\n");
    }

    /** Imprime as opções do menu de utilizador. */
    public static void opcoesUtilizador() {
        System.out.print(" Opções:\n"
                       + "  1. Inserir um pedido de encomenda.\n"
                       + "  2. Classificar um serviço de entrega.\n");
    }

    /** Imprime as opções do menu de voluntário. */
    public static void opcoesVoluntario() {
        System.out.print(" Opções:\n"
                       + "  1. Ver encomendas pendentes.\n"
                       + "  2. Indicar que encomenda foi entregue.\n"
                       + "  3. Indicar se transporta medicamentos.\n");
    }

    /** Imprime as opções do menu geral. */
    public static void opcoesGeral() {
        System.out.print("\n Definições:\n"
                         + "  c: Criar um novo estado.\n"
                         + "  g: Gravar o estado atual.\n"
                         + "  l: Carregar um estado. \n"
                         + "  m: Mudar de sessão.\n"
                         + "  n: Ver as notificações.\n"
                         + "  p: Ver o perfil.\n"
                         + "  s: Sair.\n");
    }

    /**
     * Imprime o prompt de inserção de dados.
     * @param s O que será pedido ao utilizador que insira.
     */
    public static void insira(String s) {
        System.out.print("\n > Insira " + s + ": ");
    }
    

    /** Imprime o prompt. */
    public static void prompt() {
        System.out.print("\n  > Escolha uma opção: ");
    }


    /** Apresenta a mensagem de encerramento do programa. */
    public static void outro() {
        System.out.print("\n *** A encerrar a execução... ***\n\n");
    }
    /** Apresenta as opções de registo a um Admin. */
    public static void opcoesRegistoEntidade(){
        System.out.println("\n Opções de registo:\n"
                           + " l: Registar uma loja. \n"
                           + " t: Registar uma transportadora. \n"
                           + " u: Registar um utilizador. \n"
                           + " v: Registar um voluntário. \n");
    }

    public static void pressEnterToContinue() { 
        System.out.print("\n Carregue no Enter para continuar. ");
    }

    public static void error(String e) {
        System.out.println("\nErro: " + e);
    }

    public static void cancelar(){
        System.out.println("\nAção cancelada...\n");
        View.pressEnterToContinue();
    }

    public static void paraCancelar(){
        System.out.println("\nInsira '0' para cancelar. \n");
    }

    public static void out(String s){
        System.out.println(s);
    }

    public static void navegador(boolean searchMode){
            StringBuilder sb = new StringBuilder();
    
            sb.append("\nControls: (n) next   |  (p) previous    |  (c) close ");
            sb.append("\n          (f) first  |  (l) last        |  (g) goto  ");
            sb.append("\n          (s) search");
            if (searchMode) 
                sb.append(" |  (i) exit search |");
            sb.append("\n > ");
    
            System.out.print(sb);
    }

    public static void pretende(String s){
        System.out.print("\nPretende " + s + "?");
        System.out.print("\n\n\t(s) - sim    (n) - não");
        System.out.print("\n\n > ");
    }

    public static void gravarEstado(){
        System.out.print("\nGravar no ficheiro padrão?");
        System.out.print("\n\n(s) - sim      (n) - não     (c) - cancelar");
        System.out.print("\n\n > ");
    }

    public static void carregarEstado(){
        System.out.print("\nCarregar de um ficheiro log ou programa total?");
        System.out.print("\n\n(l) - log      (t) - total     (c) - cancelar");
        System.out.print("\n > ");
    }

    public static void query8(List<Map.Entry<IEncomenda, String>> q8, Map<String, LocalDateTime> datas){
        StringBuilder sb = new StringBuilder();
        LocalDateTime ldt;
        int s;

        if(q8 != null && (s = q8.size()) > 0){
            int cD = 38, cC = 11, cE = 35, cP = 35;
          sb.append("\n┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓\n");
            sb.append("┃              Data/Hora              ┃  Código  ┃              Estado              ┃     Data Prevista de Entrega     ┃\n");
            for(int i = 0; i < s; i++){
            sb.append("┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫\n");
                ldt = q8.get(i).getKey().getDataAceitacao();
                if(ldt == null)
                    ldt = q8.get(i).getKey().getData();
                String aux = "┃  " + PrintFormat.dateFormat(ldt);
                sb.append(aux);
                for(int j = aux.length(); j < cD; j++)
                    sb.append(" ");
                aux =  "┃  " + q8.get(i).getKey().getCodigo();
                sb.append(aux);
                for(int j = aux.length(); j < cC; j++)
                    sb.append(" ");
                aux = "┃  " + q8.get(i).getValue();
                sb.append(aux);
                for(int j = aux.length(); j < cE; j++)
                    sb.append(" ");
                if((ldt = datas.get(q8.get(i).getKey().getCodigo())) != null)
                    aux = "┃  " + PrintFormat.dateFormat(ldt);
                else
                    aux = "┃  N/A";
                sb.append(aux);
                for(int j = aux.length(); j < cP; j++)
                    sb.append(" ");
                sb.append("┃\n");
            }
            sb.append("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛\n");

            System.out.println(sb);
        }
        else
            System.out.println("\nSem encomendas.");
    }

    public static void query10(List<IUtilizador> q10){
        StringBuilder sb = new StringBuilder();

        int s = q10.size();
        if(s > 0){
            int cN = 6, cC = 11, cE = 16;
            sb.append("\n┏━━━━━┯━━━━━━━━━━┯━━━━━━━━━━━━━━━┓\n");
            sb.append("┃  N  ┃  Código  ┃ Nº Encomendas ┃\n");
            for(int i = 0; i < s; i++){
            sb.append("┣━━━━━╋━━━━━━━━━━╋━━━━━━━━━━━━━━━┫\n");
                String aux = "┃  " + (i + 1);
                sb.append(aux);
                for(int j = aux.length(); j < cN; j++)
                    sb.append(" ");
                aux =  "┃  " + q10.get(i).getId();
                sb.append(aux);
                for(int j = aux.length(); j < cC; j++)
                    sb.append(" ");
                aux = "┃  " + q10.get(i).getnEnc();
                sb.append(aux);
                for(int j = aux.length(); j < cE; j++)
                    sb.append(" ");
                sb.append("┃\n");
            }
            sb.append("┗━━━━━┻━━━━━━━━━━┻━━━━━━━━━━━━━━━┛\n");

            System.out.println(sb);
        }
        else
            System.out.println("\nSem referência a quaisquer utilizadores.");
    }



    public static void query11(List<ITransportadora> q11){
        StringBuilder sb = new StringBuilder();
        int s = q11.size();
        if(s > 0){
            int cN = 6, cC = 11, cK = 13;
            sb.append("┏━━━━━┯━━━━━━━━━━┯━━━━━━━━━━━━┓\n");
            sb.append("┃  N  ┃  Código  ┃ Kms Totais ┃\n");
            for(int i = 0; i < s; i++){
                sb.append("┣━━━━━╋━━━━━━━━━━╋━━━━━━━━━━━━┫\n");
                String aux = "┃  " + (i + 1);
                sb.append(aux);
                for(int j = aux.length(); j < cN; j++)
                    sb.append(" ");
                aux =  "┃  " + q11.get(i).getId();
                sb.append(aux);
                for(int j = aux.length(); j < cC; j++)
                    sb.append(" ");
                aux = "┃  " + q11.get(i).getKmTotais();
                sb.append(aux);
                for(int j = aux.length(); j < cK; j++)
                    sb.append(" ");
                sb.append("┃\n");
            }
            sb.append("┗━━━━━┻━━━━━━━━━━┻━━━━━━━━━━━━┛\n");

            System.out.println(sb);
        }
        else
            System.out.println("\nSem referência a quaisquer transportadoras.");
    }

    public static void aviso(String warning){
        System.out.println("Aviso: " + warning + "!");
    }

    public static void emFila(List<IEncomenda> l){
        StringBuilder sb = new StringBuilder();
        int s;

        if(l != null && (s = l.size()) > 0){
            int cD = 38, cC = 11, cE = 15;
          sb.append("\n┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━━━┯━━━━━━━━━━━━━━┓\n");
            sb.append("┃              Data/Hora              ┃  Código  ┃  Utilizador  ┃\n");
            for(int i = 0; i < s; i++){
            sb.append("┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━╋━━━━━━━━━━━━━━┫\n");
                String aux = "┃  " + PrintFormat.dateFormat(l.get(i).getData());
                sb.append(aux);
                for(int j = aux.length(); j < cD; j++)
                    sb.append(" ");
                aux =  "┃  " + l.get(i).getCodigo();
                sb.append(aux);
                for(int j = aux.length(); j < cC; j++)
                    sb.append(" ");
                aux = "┃  " + l.get(i).getCodUtil();
                sb.append(aux);
                for(int j = aux.length(); j < cE; j++)
                    sb.append(" ");
                sb.append("┃\n");
            }
            sb.append("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━┻━━━━━━━━━━━━━━┛\n");

            System.out.println(sb);
        }
        else
            System.out.println("\nSem encomendas.");
    }

    public static void transpPorDecidir(List<Map.Entry<String, IEncomenda>> l_ts){
        StringBuilder sb = new StringBuilder();
        LocalDateTime ldt;
        int s;

        if(l_ts != null && (s = l_ts.size()) > 0){
            int cD = 38, cE = 14, cT = 19, cP = 17 ;
          sb.append("\n┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┯━━━━━━━━━━━━━┯━━━━━━━━━━━━━━━━━━┯━━━━━━━━━━━━━━━━┓\n");
            sb.append("┃              Data/Hora              ┃  Encomenda  ┃  Transportadora  ┃     Portes     ┃\n");
            for(int i = 0; i < s; i++){
            sb.append("┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━━━╋━━━━━━━━━━━━━━━━┫\n");
                ldt = l_ts.get(i).getValue().getDataAceitacao();
                if(ldt == null)
                    ldt = l_ts.get(i).getValue().getData();
                String aux = "┃  " + PrintFormat.dateFormat(ldt);
                sb.append(aux);
                for(int j = aux.length(); j < cD; j++)
                    sb.append(" ");
                aux =  "┃  " + l_ts.get(i).getValue().getCodigo();
                sb.append(aux);
                for(int j = aux.length(); j < cE; j++)
                    sb.append(" ");
                aux = "┃  " + l_ts.get(i).getKey();
                sb.append(aux);
                for(int j = aux.length(); j < cT; j++)
                    sb.append(" ");
                aux = "┃  " + PrintFormat.currencyFormat(l_ts.get(i).getValue().getPortes());
                sb.append(aux);
                for(int j = aux.length(); j < cP; j++)
                        sb.append(" ");    
                sb.append("┃\n");
            }
            sb.append("┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━━━┻━━━━━━━━━━━━━━━━┛\n");

            System.out.println(sb);
        }
    }


}