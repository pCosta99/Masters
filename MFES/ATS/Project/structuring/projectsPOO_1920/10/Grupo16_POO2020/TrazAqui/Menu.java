package TrazAqui;

import TrazAqui.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.time.LocalDateTime;
import java.util.*;

/**
 * Classe que representa a View do nosso sistema e garante a interação com Qualquer tipo de User TrazAqui!. Tem acesso à BaseDados e évoca os métodos definidos nesse objeto
 */
public class Menu {
    private BaseDados bd;
    private Parser parser;

    public Menu(BaseDados bd) {
        this.bd = bd;
        parser = new Parser(bd);
    }

    /**
     * PrintEcra mainMenu
     */
    public void mainMenu() {
        System.out.println();
        System.out.println("#######################################");
        System.out.println("#@@@@@ M_E_N_U____I_N_I_C_I_A_L @@@@@@#");
        System.out.println("#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#");
        System.out.println("#*-*x*-*-*-*-*-*-*-*-*-*-*-*-*-*-*x*-*#");
        System.out.println("#---x*--------!TRAZ AQUI!--------*x---#");
        System.out.println("#---x*---------------------------*x---#");
        System.out.println("#---x*-----Escolha uma opção:----*x---#");
        System.out.println("#---x*---------------------------*x---#");
        System.out.println("#---x*----1-Registar um User-----*x---#");
        System.out.println("#---x*----2-Realizar Login-------*x---#");
        System.out.println("#---x*----3-Administrar o site---*x---#");
        System.out.println("#---x*----4-Leaderboard----------*x---#");
        System.out.println("#-*-x-*-*-*-*-*-*-*-*-*-*-*-*-*-*-x-*-#");
        System.out.println("#*-*x*-*-*-*-*-*-*-*-*-*-*-*-*-*-*x*-*#");
        System.out.println("#######################################");
    }

    /**
     * PrintEcra leaderboard
     */
    public void leaderboard() {
        System.out.println();
        System.out.println("#######################################");
        System.out.println("#@@@@@   L E A D E R B O A R D  @@@@@@#");
        System.out.println("#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@#");
        System.out.println("#*-*x*-*-*-*-*-*-*-*-*-*-*-*-*-*-*x*-*#");
        System.out.println("#---x*--------!TRAZ AQUI!--------*x---#");
        System.out.println("#---x*---------------------------*x---#");
        System.out.println("#---x*-----Escolha uma opção:----*x---#");
        System.out.println("#---x*---------------------------*x---#");
        System.out.println("#---x*-1-Top 10 Utilizadores-----*x---#");
        System.out.println("#---x*-2-Top 10 Empresas---------*x---#");
        System.out.println("#---x*-3-Voltar ao Menu Principal*x---#");
        System.out.println("#---x*---------------------------*x---#");
        System.out.println("#-*-x-*-*-*-*-*-*-*-*-*-*-*-*-*-*-x-*-#");
        System.out.println("#*-*x*-*-*-*-*-*-*-*-*-*-*-*-*-*-*x*-*#");
        System.out.println("#######################################");
    }
    /**
     * PrintEcra menuAdmin
     */
    public void printMenuAdmin() {
        System.out.println();
        System.out.println("########################################################################");
        System.out.println("#**********************************************************************#");
        System.out.println("#-----------------------------!TRAZ AQUI!------------------------------#");
        System.out.println("#----------------------------------------------------------------------#");
        System.out.println("#----------------------------MENU DE ADMIN-----------------------------#");
        System.out.println("#----------------------------------------------------------------------#");
        System.out.println("#----------____________________________________________________--------#");
        System.out.println("#---------|                                                    |-------#");
        System.out.println("#---------|1- Carregar os dados do ficheiro logs.txt           |-------#");
        System.out.println("#---------|                                                    |-------#");
        System.out.println("#---------|2- Carregar o último estado guardado da aplicação   |-------#");
        System.out.println("#---------|                                                    |-------#");
        System.out.println("#-------- |3- Guardar em disco o atual estado da Base de dados |-------#");
        System.out.println("#---------|                                                    |-------#");
        System.out.println("#---------|4- Mostrar Todos os Users                           |-------#");
        System.out.println("#---------|                                                    |-------#");
        System.out.println("#---------|5- Mostrar todos os Pedidos de Encomendas           |-------#");
        System.out.println("#---------|                                                    |-------#");
        System.out.println("#---------|6- Voltar ao menu Principal                         |-------#");
        System.out.println("#---------|____________________________________________________|-------#");
        System.out.println("#----------------------------------------------------------------------#");
        System.out.println("#----------------------------------------------------------------------#");
        System.out.println("########################################################################");
    }

    /**
     * PrintEcra invalidOption
     */
    public void invalidOption() {
        System.out.println("##########################");
        System.out.println("#------------------------#");
        System.out.println("#     Opção inválida     #");
        System.out.println("#------------------------#");
        System.out.println("##########################");
    }

    /**
     * PrintEcra sucessoOption
     */
    public void successOption() {
        System.out.println("######################################");
        System.out.println("#------------------------------------#");
        System.out.println("#     Opção realizada com sucesso    #");
        System.out.println("#------------------------------------#");
        System.out.println("######################################");
    }

    /**
     * PrintEcra menuRegist
     */
    public void printMenuRegist() {
        System.out.println("\n##########################");
        System.out.println("#************************#");
        System.out.println("#------!TRAZ AQUI!-------#");
        System.out.println("#------------------------#");
        System.out.println("#----MENU DE REGISTO-----#");
        System.out.println("#------------------------#");
        System.out.println("#---Escolha uma opção----#");
        System.out.println("#------------------------#");
        System.out.println("#------1-LOJA------------#");
        System.out.println("#------2-UTILIZADOR------#");
        System.out.println("#------3-VOLUNTÁRIO------#");
        System.out.println("#------4-EMPRESA---------#");
        System.out.println("#------------------------#");
        System.out.println("#------------------------#");
        System.out.println("##########################");
    }

    /**
     * PrintEcra registaUserName
     */
    public void printRegistaUserName() {
        System.out.println("\n##########################");
        System.out.println("#************************#");
        System.out.println("#------!TRAZ AQUI!-------#");
        System.out.println("#------------------------#");
        System.out.println("#----MENU DE REGISTO-----#");
        System.out.println("#------------------------#");
        System.out.println("#---Insira o seu nome----#");
        System.out.println("#------------------------#");
        System.out.println("##########################");
    }

    /**
     * PrintEcra inserePw
     */
    public void printInserePw() {
        System.out.println("\n###########################");
        System.out.println("#*************************#");
        System.out.println("#------!TRAZ AQUI!--------#");
        System.out.println("#-------------------------#");
        System.out.println("#-------------------------#");
        System.out.println("#---Insira uma Password:--#");
        System.out.println("#-------------------------#");
        System.out.println("###########################");
    }

    /**
     * Print da informação de que um registo foi realizado com sucesso.
     * @param email Email gerado a mostrar
     */
    public void sucessoRegisto(String email) {
        System.out.println("\n#############################");
        System.out.println("#---------------------------#");
        System.out.println("#---Registado com Sucesso---#");
        System.out.println("#---------------------------#");
        System.out.println("#--O seu email do TrazAqui:-#");
        System.out.println("#---------------------------#");
        System.out.println(email);
        System.out.println("#---------------------------#");
        System.out.println("#---------------------------#");
        System.out.println("#-Pretende iniciar Sessão?--#");
        System.out.println("#---------------------------#");
        System.out.println("#1-SIM .....................#");
        System.out.println("#2-Voltar ao menu Principal-#");
        System.out.println("#############################");
    }

    /**
     * PrintEcra menuPrintGoodBye
     */
    public void printGoodBye() {
        System.out.println("##########################");
        System.out.println("#------------------------#");
        System.out.println("#-----Até à próxima!-----#");
        System.out.println("#------------------------#");
        System.out.println("##########################");
    }


    public void printGps(){
        System.out.println("\n##################################");
        System.out.println("#********************************#");
        System.out.println("#-----------!TRAZ AQUI!----------#");
        System.out.println("#--------------------------------#");
        System.out.println("#--------------------------------#");
        System.out.println("#--Insiras as suas coordenadas --#");
        System.out.println("#--------(Uma por linha)---------#");
        System.out.println("#--------------------------------#");
        System.out.println("##################################");
    }

    public void printRaio(){
        System.out.println("\n##################################");
        System.out.println("#********************************#");
        System.out.println("#-----------!TRAZ AQUI!----------#");
        System.out.println("#--------------------------------#");
        System.out.println("#--------------------------------#");
        System.out.println("#--Insiras o seu raio de ação ---#");
        System.out.println("#--------------------------------#");
        System.out.println("##################################");
    }


    /**
     * Menu que potencializa o registo de um User.
     * @throws IOException Exceção de IO
     * @throws ClassNotFoundException Exceção de classe inexistente
     */
    public void menuRegisto() throws IOException, ClassNotFoundException {
        Login log;
        BufferedReader bra = new BufferedReader(new InputStreamReader(System.in));
        String rd;
        printMenuRegist();

        if ((rd = bra.readLine()) != null) {
            //Verifica se a linha lida é alguma das opções
            if (!(rd.equals("1") || rd.equals("2") || rd.equals("3") || rd.equals("4"))) {
                invalidOption();
                menuRegisto();
                return;
            }
            //Cria um objeto da opção escolhida
            int opt = Integer.parseInt(rd);
            switch (opt) {
                case 1:
                    log = new Loja();
                    break;
                case 2:
                    log = new Utilizador();
                    break;
                case 3:
                    log = new Voluntario();
                    break;
                case 4:
                    log = new Empresa();
                    break;
                default: {
                    invalidOption();
                    menuRegisto();
                    return;
                }
            }
            printRegistaUserName();
            //Nome completo do User
            String user = bra.readLine();

            //Valida a String nome e atualiza o objeto Login criado
            if (log != null && user.length() > 0) {
                log.setCodUser(bd.generateCode(opt));
                log.setUsername(user);
            } else {
                menuRegisto();
                invalidOption();
                return;
            }
            printInserePw();
            //Password do User
            String pw = bra.readLine();

            //Valida a String password e atualiza o objeto Login criado
            if (pw != null && pw.length() > 0) {
                log.setPassword(pw);
            } else {
                menuRegisto();
                throw new IOException("Erro no registo");
            }

            String email = "#----".concat(log.getCodUser().concat("@trazaqui.com.pt").concat("----#"));
            printGps();
            double lat = Double.parseDouble(bra.readLine());
            double longi = Double.parseDouble(bra.readLine());
            Ponto p = new Ponto(lat,longi);
            log.setPonto(p);

            if(log instanceof Entregas){
                printRaio();
                float range = Float.parseFloat(bra.readLine());
                ((Entregas) log).setRange(range);
            }
            bd.regista(log);
            sucessoRegisto(email);
            opt = Integer.parseInt(bra.readLine());


            switch (opt){
                case 1:
                    if(log instanceof Utilizador){
                        userSession(log.getCodUser());
                        return;
                    }else if(log instanceof Empresa){
                        empresaSession(log.getCodUser());
                        return;
                    }else if(log instanceof Voluntario){
                        voluntarioSession(log.getCodUser());
                        return;
                    }else if (log instanceof Loja){
                        lojaSession(log.getCodUser());
                        return;
                    }

                case 2:
                    menuInicial();
                    return;
                default:
                    printGoodBye();
                    return;

            }

        } else {
            menuRegisto();
            return;
        }
    }

    /**
     * Menu que permite que um User aceda à sua página do sistema.
     * @throws IOException Exceção de IO
     * @throws ClassNotFoundException Exceção de Classe não existente
     */
    public void menuLogin() throws IOException, ClassNotFoundException {
        System.out.println("###################################");
        System.out.println("#*********************************#");
        System.out.println("#------------!TRAZ AQUI!----------#");
        System.out.println("#---------------------------------#");
        System.out.println("#-----------MENU DE SESSÃO--------#");
        System.out.println("#---------------------------------#");
        System.out.println("#--Insira o seu email ou código:--#");
        System.out.println("#---------------------------------#");
        System.out.println("###################################");
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String[] campos = br.readLine().split("@");
        String user = campos[0];
        if (bd.validaAcesso(user)) {
            printInserePw();
            String pw = br.readLine();
            if (bd.validaAcesso(user, pw)) {
                switch (user.charAt(0)){
                    case 'u':
                        userSession(user);
                        return;
                    case 't':
                        empresaSession(user);
                        return;
                    case 'v':
                        voluntarioSession(user);
                        return;
                    case 'l':
                        lojaSession(user);
                        return;
                    default:
                        invalidOption();
                        menuLogin();
                        return;
                }
            }
        } else {
            System.out.println("#########################################");
            System.out.println("#***************************************#");
            System.out.println("#--------------!TRAZ AQUI!--------------#");
            System.out.println("#---------------------------------------#");
            System.out.println("# (︶︿︶) USUÁRIO NÃO EXISTENTE (︶︿︶) #");
            System.out.println("#---------------------------------------#");
            System.out.println("#---------------------------------------#");
            System.out.println("#-----------1-Registar-me --------------#");
            System.out.println("#---------------------------------------#");
            System.out.println("#---Qualquer Tecla- Inserir novamente---#");
            System.out.println("#---------------------------------------#");
            System.out.println("#########################################");
            String opt = br.readLine();
            if (opt.equals("1")) {
                menuRegisto();
                return;
            } else {
                menuLogin();
                return;
            }
        }
    }

    /**
     * Menu de LeaderBoard
     * @throws IOException Exceção de IO
     * @throws ClassNotFoundException Exceção de Classe não existente
     */
    public void menuLeaderboard() throws IOException, ClassNotFoundException {
        leaderboard();
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String opt = br.readLine();
        switch (opt) {
            case "1":
                int place = 1;
                System.out.println("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
                System.out.println("################ TOP 10 UTILIZADORES MAIS ATIVOS ##############");
                System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
                for (Utilizador u : bd.top10User()) {
                    System.out.println("     >>> " + place + "º: " + u.getCodUser() + " - " + u.getUsername());
                    place++;
                }
                System.out.println("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
                System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
                menuLeaderboard();
                return;

            case "2":
                int placeEmp = 1;
                System.out.println("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
                System.out.println("################ TOP 10 EMPRESAS MAIS ATIVAS ##############");
                System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$\n");
                for (Empresa e : bd.top10Empresa()) {
                    System.out.println("     >>> " + placeEmp + "º: " + e.getCodUser() + " - " + e.getUsername());
                    placeEmp++;
                }
                System.out.println("\n$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
                System.out.println("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
                menuLeaderboard();
                return;
            case "3":
                menuInicial();
                return;
            default:
                invalidOption();
                menuLeaderboard();
                return;
        }
    }

    /**
     * Menu de administrador do sistema. Permite ver o estado da Base de Dados.
     * @throws IOException Exceção de IO
     * @throws ClassNotFoundException Exceção de CLasse não existente
     */
    public void menuAdmin() throws IOException, ClassNotFoundException {
        printMenuAdmin();
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String opt = br.readLine();
        switch (opt) {
            case "1":
                if (this.parser.parse("/home/luisaraujo/Desktop/TrazAqui/TrazAqui/logs.txt")) {
                    successOption();
                    menuAdmin();
                } else {
                    invalidOption();
                    menuAdmin();
                }
                return;
            case "2":
                BaseDados aux = bd.binaryRead();
                if (aux != null) {
                    successOption();
                    this.bd = aux;
                    menuAdmin();
                } else {
                    invalidOption();
                    menuAdmin();
                }
                return;
            case "3":
                this.bd.binaryWrite();
                successOption();
                menuAdmin();
                return;
            case "4":
                System.out.println("\n##########################################################################");
                System.out.println("################# Lista de Users registados no TrazAqui! #################\n");
                for (Login l : this.bd.printUsers()) {
                    System.out.println(l.toString());
                }
                menuAdmin();
                return;
            case "5":
                System.out.println("##########################################################################");
                System.out.println("####### Lista de pedidos de encomenda pendentes no TrazAqui! #############\n");
                for (Set<Encomenda> st : this.bd.getRequests().values()) {
                    for (Encomenda enc : st) {
                        System.out.println(enc.toString());
                    }
                }
                menuAdmin();
                return;
            case "6":
                menuInicial();
                return;
        }
    }

    /**
     * Menu Inicial do sistema.
     * @throws IOException Erro de IO
     * @throws ClassNotFoundException Erro de Classe não existente
     */
    public void menuInicial() throws IOException, ClassNotFoundException {
        mainMenu();
        Scanner sc = new Scanner(System.in);
        String opt = sc.nextLine();
        switch (opt) {
            case "1":
                menuRegisto();
                break;
            case "2":
                menuLogin();
                break;
            case "3":
                menuAdmin();
                break;
            case "4":
                menuLeaderboard();
            default:
                invalidOption();
                menuInicial();
                break;
        }
    }


    public void userSessionPrt() {
        System.out.println("###################################");
        System.out.println("#*********************************#");
        System.out.println("#-----------!TRAZ AQUI!-----------#");
        System.out.println("#---------------------------------#");
        System.out.println("#-------MENU DE UTILIZADOR--------#");
        System.out.println("#---------------------------------#");
        System.out.println("#----1-Realizar Encomenda---------#");
        System.out.println("#----2-Aceitar Serviço------------#");
        System.out.println("#----3-Histórico de encomendas----#");
        System.out.println("#----4-Classificar um serviço-----#");
        System.out.println("#----5-Menu Principal-------------#");
        System.out.println("#---------------------------------#");
        System.out.println("###################################");
    }

    public void semEntregasPendentes(){
        System.out.println("###################################");
        System.out.println("#---------------------------------#");
        System.out.println("#--------MENU DE UTILIZADOR-------#");
        System.out.println("#---------------------------------#");
        System.out.println("#------Sem Entregas pendentes-----#");
        System.out.println("#---------------------------------#");
        System.out.println("###################################");
    }

    public void semEncomendas(){
        System.out.println("###################################");
        System.out.println("#---------------------------------#");
        System.out.println("#--------MENU DE UTILIZADOR-------#");
        System.out.println("#---------------------------------#");
        System.out.println("#--Ainda não recebeu encomendas---#");
        System.out.println("#---------------------------------#");
        System.out.println("###################################");
    }

    public void semLojasPrint(){
        System.out.println("###################################");
        System.out.println("#---------------------------------#");
        System.out.println("#--------MENU DE UTILIZADOR-------#");
        System.out.println("#---------------------------------#");
        System.out.println("#--Não existem lojas disponíveis--#");
        System.out.println("#---------------------------------#");
        System.out.println("###################################");
    }

    public void classificarPrint(){
        System.out.println("#########################################");
        System.out.println("#---------------------------------------#");
        System.out.println("#----CLASSIFICAR SERVIÇO DE ENTREGA-----#");
        System.out.println("#---------------------------------------#");
        System.out.println("#--------Para classificar digite:-------#");
        System.out.println("#---------------------------------------#");
        System.out.println("# <Código Enomenda>   <Classificação>   #");
        System.out.println("#---------------------------------------#");
        System.out.println("#---Digite \"sair\" para voltar ao Menu---#");
        System.out.println("#---------------------------------------#");
        System.out.println("#########################################");
    }

    public void userSession(String userCode) throws IOException, ClassNotFoundException {
        userSessionPrt();
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String opt = br.readLine();
        Utilizador util = (Utilizador) this.bd.getUsers().get(userCode);
        switch (opt) {
            case "1":
                realizarEncomenda(userCode);
                return;
            case "2":
                if(((Utilizador) this.bd.getUsers().get(userCode)).getEntregas().isEmpty()){
                    semEntregasPendentes();
                    userSession(userCode);
                    return;
                }else{
                    HashMap<String,Encomenda> hm = util.getEntregas();
                    Set<Map.Entry<String,Encomenda>>st =  hm.entrySet();
                    for(Map.Entry<String,Encomenda> en : st){
                        System.out.println(en.getKey()+" "+en.getValue().toString());
                        double valorEnc = en.getValue().calculaValorTotalEncomenda();
                        double taxa = this.bd.getPrecoKMbycode(en.getKey());
                        long dur = this.bd.calculaDuracao(en.getValue(),en.getKey()).toMinutes();
                        double taxaFinal = taxa+ taxa*(en.getValue().getPeso()/1000);
                        taxaFinal*=dur;
                        System.out.println("Duração da entrega aproximadamente: "+dur+" Minutos");
                        System.out.println("Preço da encomenda (só produtos): "+valorEnc+"€" );
                        System.out.println("Preço da taxa de entrega: "+  +taxaFinal+"€");
                        System.out.println("Preço total da encomenda: "+ Double.sum(taxaFinal,valorEnc)+"€");
                    }
                    System.out.println("Digite o código das encomendas que pretende aceitar");
                    System.out.println("Digite sair para rejeitar as encomendas não aceites e voltar ao menu anterior.");
                    String o;
                    while(!(o = br.readLine()).equals("sair")){
                        for(Map.Entry<String,Encomenda> en : st){
                            if(en.getValue().getCod().equals(o)){
                                this.bd.userAcceptEntrega(this.bd.getEncomenda(o),en.getKey());
                                break;
                            }
                        }
                    }
                    userSession(userCode);
                    return;
                }

            case "3":
                System.out.println("#################################################");
                System.out.println("#-----------------------------------------------#");
                System.out.println("#--------------HISTÓRICO DE ENCOMENDAS:---------#");
                System.out.println("#-----------------------------------------------#");
                System.out.println("#-----------------------------------------------#");
                System.out.println("#-------1: Ver todo o registo de Encomendas-----#");
                System.out.println("#-----------------------------------------------#");
                System.out.println("#-------Ou Insira uma linha com: ---------------#");
                System.out.println("#-----------------------------------------------#");
                System.out.println("#--------<LocalDateTime>(Inicial)---------------#");
                System.out.println("#--------<LocalDateTime>(Final)-----------------#");
                System.out.println("#-----------------------------------------------#");
                System.out.println("#-----_____________________________________-----#");
                System.out.println("#----|*LocalDateTime: <yyyy-mm-ddThh:mm:ss>|----#");
                System.out.println("#----|_____________________________________|----#");
                System.out.println("#-----------------------------------------------#");
                System.out.println("#################################################");

                String hisOpt = br.readLine();
                if(hisOpt.equals("1")){
                    for(Encomenda enc: ((Utilizador) this.bd.getUsers().get(userCode)).getHistorico()){
                    System.out.println(enc.toString());
                    }
                }else{
                    LocalDateTime init = LocalDateTime.parse(hisOpt);
                    LocalDateTime max = LocalDateTime.parse(br.readLine());
                    for(Encomenda enc: ((Utilizador) this.bd.getUsers().get(userCode)).getHistorico(init,max)) {
                        System.out.println(enc.toString());
                    }
                }
                userSession(userCode);
                return;
            case "4":
                ArrayList <Encomenda> his = util.getHistorico();
                if(his.isEmpty()){
                    semEncomendas();
                    userSession(userCode);
                    return;
                }else{
                    classificarPrint();
                    for(Encomenda enc: his){
                        System.out.println("Encomenda: "+enc.getCod()+" Realizada por: "+this.bd.getNameEntregadorByEncomenda(enc));
                    }
                    String [] spl = br.readLine().split(" ");
                    while(spl.length == 2 || !spl[0].equals("sair")){
                        String codig = spl[0];
                        Encomenda enc = his.stream().filter(e -> e.getCod().equals(codig)).findAny().orElse(null);
                        this.bd.classificaEntrega(enc,Float.parseFloat(spl[1]));
                        spl = br.readLine().split(" ");
                    }
                    userSession(userCode);
                    return;
                }

            case "5":
                menuInicial();
                return;
            default:
                invalidOption();
                userSession(userCode);
                return;
        }
    }

    public void realizarEncomenda(String userCode) throws IOException, ClassNotFoundException {
        ArrayList <Loja> lj = new ArrayList<>(bd.printLojas());
        //Verifica se já existem lojas registadas
        if(lj.size() == 0){
            semLojasPrint();
            userSession(userCode);
            return;
        }
        System.out.println("###################################");
        System.out.println("#*********************************#");
        System.out.println("#-----------!TRAZ AQUI!-----------#");
        System.out.println("#---------------------------------#");
        System.out.println("#--------Por favor escolha--------#");
        System.out.println("#------------uma loja-------------#");
        System.out.println("#---------------------------------#");
        System.out.println("###################################");
        int place = 1;
        for(Loja l : lj){
            System.out.println(place + "ª: " + l.toString() );
            place++;
        }

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        int lojaIdex = Integer.parseInt(br.readLine());
        String lojaCode = lj.get(lojaIdex-1).getCodUser();
        Encomenda enc = new Encomenda(this.bd.generateCode(5),lojaCode,userCode,0, new ArrayList<>(),LocalDateTime.now());
        carrinho(lojaCode,enc);
    }

    public void carrinho(String codLoja,Encomenda enc) throws IOException, ClassNotFoundException {
        System.out.println("##################################################################");
        System.out.println("#****************************************************************#");
        System.out.println("#-----------------------------!TRAZ AQUI!------------------------#");
        System.out.println("#----------------------------------------------------------------#");
        System.out.println("#---------Adicione itens ao carrinho digitando o número ---------#");
        System.out.println("#----------------------------------------------------------------#");
        System.out.println("#---------Digite 0 para Concluir---------------------------------#");
        System.out.println("#----------------------------------------------------------------#");
        int place =1;
        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        ArrayList<LinhaEncomenda> inventario = new ArrayList<>(parser.parseInventario("/home/luisaraujo/Desktop/TrazAqui/TrazAqui/Invetario.txt"));
        for(LinhaEncomenda le : inventario){
            System.out.println(place +": " + le.toString());
            place++;
        }
        String opt = br.readLine();
        while( Integer.parseInt(opt) > 0 && Integer.parseInt(opt) < 10 ){
            LinhaEncomenda le = inventario.get(Integer.parseInt(opt)-1);
            le.setPreco(le.calculaValorLinhaEnc());
            enc.addLinhaEncomenda(le);
            opt = br.readLine();
        }
        enc.setPeso(new Random().nextFloat());
        enc.setTempo(LocalDateTime.now());

        System.out.println("####################################");
        System.out.println("#**********************************#");
        System.out.println("#-----------!TRAZ AQUI!------------#");
        System.out.println("#----------------------------------#");
        System.out.println("#--Pretende Realizar a encomenda:--#");
        System.out.println("#----------------------------------#");
        System.out.println(enc.toString());
        System.out.println(">> Preço Total: "+enc.calculaValorTotalEncomenda());
        System.out.println("#----------------------------------#");
        System.out.println("#--Digite \"sim\" para confirmar---#");
        System.out.println("#----------------------------------#");
        System.out.println("####################################");

        if(br.readLine().equals("sim")){
            System.out.println("#############################################");
            System.out.println("#*******************************************#");
            System.out.println("#-----------------!TRAZ AQUI!---------------#");
            System.out.println("#-------------------------------------------#");
            System.out.println("#-----Encomenda Realizada com sucesso!------#");
            System.out.println("#---_____________________________________---#");
            System.out.println("#--|No Menu de Utilizador poderá ver se  |--#");
            System.out.println("#--|já algum Voluntário a aceitou, ou    |--#");
            System.out.println("#--|aceite os custos de envio, caso tenha|--#");
            System.out.println("#--|sido uma empresa a aceitar a entrega |--#");
            System.out.println("#--|_____________________________________|--#");
            System.out.println("#-------------------------------------------#");
            System.out.println("#############################################");
            userSession(enc.getUser());
            return;
        }else{
            carrinho(codLoja,enc);
            return;
        }
    }

    public void voluntarioSession(String volCode) throws IOException, ClassNotFoundException {
        System.out.println("#############################################");
        System.out.println("#*******************************************#");
        System.out.println("#------------------!TRAZ AQUI!--------------#");
        System.out.println("#-------------------------------------------#");
        System.out.println("#--1 Selecionar uma Encomenda para entregar-#");
        System.out.println("#--2 Ver as minhas Encomendas Entregues-----#");
        System.out.println("#--3 Ver a minha Classificação -------------#");
        System.out.println("#--4 Voltar ao Menu Principal --------------#");
        System.out.println("#-------------------------------------------#");
        System.out.println("#############################################");

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String opt = br.readLine();
        switch (opt){
            case "1":
                ArrayList<Encomenda> lst= new ArrayList<Encomenda>(this.bd.getEcomendasRaio(volCode));
                for(Encomenda enc : lst){
                    System.out.println(enc.toString());
                }
                System.out.println("Insira o Código da encomenda que pretende entregar:");
                opt = br.readLine();
                Encomenda enc;
                if((enc= this.bd.getEncomenda(opt))!=null){
                    this.bd.volAcceptEncomenda(enc,volCode);
                    System.out.println("Encomenda Entregue com sucesso!");
                    voluntarioSession(volCode);
                    return;
                }else{
                    invalidOption();
                    voluntarioSession(volCode);
                    return;
                }
            case "2":
                for(Encomenda en : ((Entregas)this.bd.getUsers().get(volCode)).getHistorico()){
                    System.out.println("Duração da entrega: "+this.bd.getDuracaoEntrega(en.getCod()).toMinutes()+" Minutos");
                    System.out.println(en.toString());
                }
                voluntarioSession(volCode);
                return;
            case "3":
                System.out.println("A minha Classificação: "+((Entregas)this.bd.getUsers().get(volCode)).getClassificacao());
                voluntarioSession(volCode);
                return;
            case "4":
                menuInicial();
                return;
        }


    }

    public void empresaSession(String empCode) throws IOException, ClassNotFoundException {
        System.out.println("#############################################");
        System.out.println("#*******************************************#");
        System.out.println("#------------------!TRAZ AQUI!--------------#");
        System.out.println("#-------------------------------------------#");
        System.out.println("#--1 Selecionar uma Encomenda para entregar-#");
        System.out.println("#--2 Ver as minhas Encomendas Entregues-----#");
        System.out.println("#--3 Ver a minha Classificação -------------#");
        System.out.println("#--4 Ver total faturado --------------------#");
        System.out.println("#--5 Alterar preço por km ------------------#");
        System.out.println("#--6 Voltar ao Menu Principal --------------#");
        System.out.println("#-------------------------------------------#");
        System.out.println("#-------------------------------------------#");
        System.out.println("#############################################");

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String opt = br.readLine();
        switch (opt){
            case "1":
                ArrayList<Encomenda> lst= new ArrayList<Encomenda>(this.bd.getEcomendasRaio(empCode));
                for(Encomenda enc : lst){
                    System.out.println(enc.toString());
                }
                System.out.println("Insira o Código da encomenda que pretende entregar:");
                opt = br.readLine();
                Encomenda enc;
                if((enc= this.bd.getEncomenda(opt))!=null){
                    this.bd.empAcceptEncomenda(enc,empCode);
                    System.out.println("Pedido de confirmação enviado com secesso!");
                    empresaSession(empCode);
                    return;
                }else{
                    invalidOption();
                    empresaSession(empCode);
                    return;
                }
            case "2":
                for(Encomenda en : ((Entregas)this.bd.getUsers().get(empCode)).getHistorico()){
                    System.out.println("Duração da entrega: "+this.bd.getDuracaoEntrega(en.getCod()).toMinutes()+" Minutos");
                    System.out.println("Taxa de Entrega cobrada: "+this.bd.getPrecoEntrega(en.getCod())+"€");
                    System.out.println(en.toString());
                }
                empresaSession(empCode);
                return;
            case "3":
                System.out.println("A minha Classificação: "+((Entregas)this.bd.getUsers().get(empCode)).getClassificacao());
                empresaSession(empCode);
                return;
            case "4":
                System.out.println("Insira a Data inicial:");
                LocalDateTime dataI = LocalDateTime.parse(br.readLine());
                System.out.println("Insira a Data final:");
                LocalDateTime dataF = LocalDateTime.parse(br.readLine());
                System.out.println("Total Faturado: " + this.bd.totalFaturado(empCode,dataI,dataF)+"€");
                empresaSession(empCode);
                return;
            case "5":
                System.out.println("Insira novo preço:");
                float preco = Float.parseFloat(br.readLine());
                ((Empresa) this.bd.getUsers().get(empCode)).setPrecoKm(preco);
                successOption();
                empresaSession(empCode);
                return;
            case "6":
                menuInicial();
                return;
        }

    }

    public void lojaSession(String lojaCode) throws IOException, ClassNotFoundException {
        System.out.println("####################################################");
        System.out.println("#**************************************************#");
        System.out.println("#----------------------!TRAZ AQUI!-----------------#");
        System.out.println("#--------------------------------------------------#");
        System.out.println("#--1 Sinalizar encomenda pronta a ser entregue ----#");
        System.out.println("#--2 Ver pessoas em fila de espera ----------------#");
        System.out.println("#--3 Voltar ao Menu Principal ---------------------#");
        System.out.println("#--------------------------------------------------#");
        System.out.println("####################################################");

        BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
        String opt = br.readLine();
        Loja l = (Loja) this.bd.getUsers().get(lojaCode);
        PriorityQueue<Encomenda> fila = l.getPedidos();

        switch (opt){
            case "1":
                System.out.println("Fila de espera:");
                int i=1;
                for(Encomenda enc : fila){
                    System.out.println(i+"º: "+ enc.toString());
                }
                System.out.println("Quantas Encomendas estão prontas?");
                opt = br.readLine();
                if(Integer.parseInt(opt) <= fila.size()){
                    int optI = Integer.parseInt(opt);
                    while(optI > 0){
                        this.bd.lojaRegistaEncomenda(lojaCode);
                        optI--;
                    }
                    this.bd.atualizaFilaLoja(fila.size()-Integer.parseInt(opt),lojaCode);
                    successOption();
                    lojaSession(lojaCode);
                    return;
                }else{
                    invalidOption();
                    lojaSession(lojaCode);
                    return;
                }
            case "2":
                System.out.println("###########################################################");
                System.out.println("Atualmente estão "+l.getQueue()+" Pessoas em fila de espera");
                lojaSession(lojaCode);
                return;
            case "3":
                menuInicial();
                return;
            default:
                invalidOption();
                lojaSession(lojaCode);
                return;
        }

    }

    public static void main(String[] args) throws IOException, ClassNotFoundException {

        Menu menu = new Menu(new BaseDados());
        menu.menuInicial();
    }

}

