import org.json.simple.parser.ParseException;
import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class Controller {

    //Variáveis Globais
    private trazAqui dados;    //Variavel de Instancia trazAqui -> armazena em memoria todos os dados.
    private final Menu menu =new Menu();      //Variavel de Instancia Menu. -> View do programa.
    private saveState saver;
    private final loadState loader = new loadState();


    //Metodo construtor
    public Controller(int tesmodeSelected) throws IOException, ParseException {
        saver=new saveState(tesmodeSelected);
        dados = new trazAqui(loader.parser());
        saver.run(dados.clone());
        setInicioApp();        //o programa começa com a função Inicio
    }

    //Metodo MAIN.
    public static void main(String args[]) throws IOException, ParseException {
        try {
            new Controller(1);
        }catch (Exception e){
            System.out.println("Ocorreu um erro a iniciar o programa. Tente de novo!");
        }
    }


    /*Este metodo é o primeiro a ser executado após a criação do Controller e carregamento de dados
    chama os metodos da view que imprimem o menu principal, que permite escolher autenticar ou registar.*/
    public void setInicioApp (){
      try {
          int start = menu.InicioApp();
          switch (start) {
              case 1:
                  setMenuLogin();
                  break;
              case 2:
                  setMenuRegisto();
                  break;
              case 0:
                  System.exit(0);
              default:
                  System.out.print("Escolha Inválida");
          }
      }catch (Exception e){System.out.println("Ocorreu um problema a carregar a interface.");}
    }

    //Se o utilizador decidir autenticar, será chamado este método que imprime, atravês a view, as opções de contas.
    public void setMenuLogin(){
        int menuLogin = menu.menuInicial();
        try {
            switch (menuLogin) {
            case 1:
                LoginLoja();
                break;
            case 2:
                LoginTransportadora();
                break;
            case 3:
                LoginVoluntario();
                break;
            case 4:
                LoginCliente();
                break;
            case 0:
                setInicioApp();
                break;       //Volta ao Ecrão Inicial da Aplicação (função InicioApp)
            default:
                System.out.print("Escolha Inválida");
                setMenuLogin();
                break;
        }
        }catch (Exception e){System.out.println("Ocorreu um erro"); setInicioApp();}
    }


    //Os 4 Metodos seguintes permitem o login nas contas. Se errarem alguma credencial dará erro de conta nao existente.
    public void LoginLoja(){
       try {
           String L = menu.LoginL();
           if (L.equals("<b")) {
               setMenuLogin();
           } else {
               MenuLoja(dados.validaLogin(1, L));
           }
       }catch (Exception e){
           if(e instanceof ContaNãoExistente) System.out.println(e.getMessage());
           else System.out.println("Erro a efetuar login.");
           LoginLoja();}
    }

    public void LoginTransportadora(){
        try {
            String L = menu.LoginT();
            if (L.equals("<b")) {
                setMenuLogin();
            } else {
                MenuEmpresa(dados.validaLogin(2, L));
            }
        }catch (Exception e){
            if(e instanceof ContaNãoExistente) System.out.println(e.getMessage());
            else System.out.println("Erro a efetuar login.");
            LoginTransportadora();}
    }

    public void LoginVoluntario(){
        try {
            String L = menu.LoginV();
            System.out.println(L);
            if (L.equals("<b")) {
                setMenuLogin();
            } else {
                MenuVoluntario(dados.validaLogin(3, L));
            }
        }catch (Exception e){
            if(e instanceof ContaNãoExistente) System.out.println(e.getMessage());
            else System.out.println("Erro a efetuar login.");
            LoginVoluntario();}
    }

    public void LoginCliente(){
        try {
            String L = menu.LoginC();
            if (L.equals("<b")) {
                setMenuLogin();
            } else {
                MenuCliente(dados.validaLogin(4, L));
            }
        }catch (Exception e){
            if(e instanceof ContaNãoExistente) System.out.println(e.getMessage());
            else System.out.println("Erro a efetuar login.");
            LoginCliente();}
    }



    //Métodos para registo
    public void setMenuRegisto(){
        try{
        int menuReg = menu.menuRegistoGeral();
        switch (menuReg){
            case 1: RegistoLoja(); break;
            case 2: RegistoTransportadora(); break;
            case 3: RegistoVoluntario(); break;
            case 4: RegistoCliente(); break;
            case 0:setInicioApp(); break;
            default: System.out.print("Escolha Inválida");
        }
        }catch (Exception e){System.out.println("Erro a registar."); setMenuRegisto();}
    }

    public void RegistoLoja(){
        String L = menu.RegL();
        System.out.println(L);
        try {
            if(L.equals("<b")) {setMenuRegisto();}
            else {
            String code = dados.createStore(L.split(","));
            saver.run(dados.clone());
            System.out.println("Recorde o seguinte código, servirá como credencial de login: " + code);
            setInicioApp();}
        }catch (Exception e){
            if (e instanceof ContaExistente) {
                System.out.print(e.getMessage());
                setMenuLogin();
            } else {
                System.out.print("Erro a efetuar registo, tente novamente.");
                setMenuRegisto();
            }
        }
    }

    public void RegistoTransportadora(){
        String T = menu.RegT();
        if(T.equals("<b")) setMenuRegisto();
        else try {
            String code = dados.createTransporter(T.split(","));
            saver.run(dados.clone());
            System.out.println("Recorde o seguinte código, servirá como credencial de login: " + code);
            setInicioApp();
        } catch (Exception e) {
            if (e instanceof ContaExistente) {
                System.out.print(e.getMessage());
                setMenuLogin();
            } else {
                System.out.print("Erro a efetuar registo, tente novamente.");
                setMenuRegisto();
            }
        }
    }

    public void RegistoVoluntario(){
        String V = menu.RegV();
        System.out.println(V);
        if(V.equals("<b")) setMenuRegisto();
        else try {
            String code = dados.createVolunteer(V.split(","));
            saver.run(dados.clone());
            System.out.println("Recorde o seguinte código, servirá como credencial de login: " + code);
            setInicioApp();
        }catch (Exception e) {
            if (e instanceof ContaExistente) {
                System.out.print(e.getMessage());
                setMenuLogin();
            } else {
                System.out.print("Erro a efetuar registo, tente novamente.");
                setMenuRegisto();
            }
        }
    }

    public void RegistoCliente(){
        String C = menu.RegC();
        if(C.equals("<b")) setMenuRegisto();
        else try {
            String code = dados.createUser(C.split(","));
            saver.run(dados.clone());
            System.out.println("Recorde o seguinte código, servirá como credencial de login: " + code);
            setInicioApp();
        }catch (Exception e){
            if (e instanceof ContaExistente) {
                System.out.print(e.getMessage());
                setMenuLogin();
            } else {
                System.out.print("Erro a efetuar registo, tente novamente.");
                setMenuRegisto();
            }
        }
    }



    //Metodos para cada tipo de utilizador.

    //Loja
    public void MenuLoja(String code){
       try {
           int choise = menu.menuGeralLoja(dados.getStores().get(code).getName());
           switch (choise){
               default: setInicioApp(); break;
               case 1: dados.setQueueStore(code,menu.lerInt("Tamanho da fila de espera (0->Nenhum; 1->Pouco; 2-> Médio; 3->Elevado):")); break;
               case 2: dados.setAvgServiceTimeStore(code,menu.lerInt("Tempo de Espera Médio (em minutos):")); break;
               case 3: processOrders(code); break;
               case 4: dados.addProductToStore(code,dados.createProductforStore(menu.lerString("Formato - papel higiénico-peso-preço,desinfetante-peso-preço):").split(","), 0));break;
               case 5: dados.removeProductFromStore(code,menu.showProducts(dados.getStores().get(code).getAvailableProducts())); break;
               case 6: showPastOrders(code,1); break;
               case 7: menu.showTop10Transporters(dados.listaEmpresasMaisUsam());
                   menu.lerString("Para sair digite \"<b\"");break;
               case 8: menu.showTop10Users(dados.listaUtilizadoresMaisUsam());
                   menu.lerString("Para sair digite \"<b\"");break;
           }
       }catch (Exception e){System.out.println(e.getMessage());}
       saver.run(dados.clone());
       MenuLoja(code);
    }

    public void processOrders(String code){
        Set<Order> ordersToSetReady = dados.getOrdersToSetFromStore(code);
        List<String> ordersReady = menu.showOrders(ordersToSetReady,"Entregas a ser concluidas.");
        if(!ordersReady.contains("<b")){
        ordersReady.forEach(dados::createOrderReady);}
    }




    //Voluntário
    public void MenuVoluntario(String code){
        try {
            int choise = menu.menuGeralVoluntario(dados.getVolunteers().get(code).getName());
            switch (choise){
            default: setInicioApp(); break;
            case 1: System.out.println("Disponilibidade Alterada para: " + dados.alterarDisponibilidade(code,2)); break;
            case 2: deliveryDone(code,2); break;
            case 3: System.out.println("A sua classificação é: "+dados.getVolunteers().get(code).getClassification());
                    menu.lerString("Para sair digite \"<b\"");
                    break;
            case 4:showPastOrders(code,3); break;
            case 5: menu.showTop10Transporters(dados.listaEmpresasMaisUsam());
                menu.lerString("Para sair digite \"<b\"");break;
            case 6: menu.showTop10Users(dados.listaUtilizadoresMaisUsam());
                menu.lerString("Para sair digite \"<b\"");break;
        }}catch (Exception e){MenuVoluntario(code);}
        saver.run(dados.clone());
        MenuVoluntario(code);
    }


    //Empresa
    public void MenuEmpresa(String code) {
        try {
            int choise = menu.menuGeralEmpresa(dados.getTransporters().get(code).getName());
            switch (choise){
            default: setInicioApp(); break;
            case 1: System.out.println("Disponilibidade Alterada para: " + dados.alterarDisponibilidade(code,1)); break;
            case 2: dados.atualizarEmpregados(code,menu.lerInt("Quantos estafetas estão atualmente a trabalhar?")); break;
            case 3: deliveryDone(code,1); break;
            case 4: System.out.println("A sua classificação é: "+dados.getTransporters().get(code).getClassification());
                menu.lerString("Para sair digite \"<b\"");
                break;
            case 5:showPastOrders(code,2); break;
            case 6: menu.showTop10Transporters(dados.listaEmpresasMaisUsam());
                menu.lerString("Para sair digite \"<b\"");break;
            case 7: menu.showTop10Users(dados.listaUtilizadoresMaisUsam());
                menu.lerString("Para sair digite \"<b\"");break;
            case 8: totalFaturado(code); break;
        }}catch (Exception e){MenuEmpresa(code);}
        saver.run(dados.clone());
        MenuEmpresa(code);
    }

    public void deliveryDone(String code,int type){
        Set<Order> orders = dados.getOrdersToDelivery(code);
        String codeOrder = menu.showOrdersString(orders,"Encomendas Prontas a ser Entregues");
        if(!codeOrder.equals("<b")){
        double time = menu.lerDouble("Tempo de entrega? em minutos.");
        dados.setDeliveryTimeAndFinished(codeOrder,time,code,type);
        }
    }

    public void totalFaturado(String code){
        String before = menu.lerString("De: (DD-MM-AAAA)");
        if(!before.equals("<b")){
        String after = menu.lerString("Ate: (DD-MM-AAAA)");
        if(!after.equals("<b")){
            LocalDate beforeDate = LocalDate.parse(before);
            LocalDate afterDate = LocalDate.parse(after);
            menu.lerString("Total faturado é no perido selecionado é: " +dados.totalFaturadoEmpresa(code,afterDate,beforeDate) +"\n " +
                    "Para retroceder prima qualquer tecla seguido de enter");
        } }
    }

    //Cliente
    public void MenuCliente(String code) {
        try {
            int choise = menu.menuGeralUser(dados.getUsers().get(code).getName());
            switch (choise){
            default: setInicioApp(); break;
            case 1: criarEncomenda(code); break;
            case 2: showPastOrders(code,4); break;
            case 3: menu.showTop10Transporters(dados.listaEmpresasMaisUsam());
                    menu.lerString("Para sair digite \"<b\"");
                    break;
            case 4:menu.showTop10Users(dados.listaUtilizadoresMaisUsam());
                   menu.lerString("Para sair digite \"<b\"");break;
            case 5: classificarEntrega(code); break;
        }
        }catch (Exception e){MenuCliente(code);}
        saver.run(dados.clone());
        MenuCliente(code);
    }

    public void classificarEntrega(String codeUser){
        String codeOrder = menu.showOrdersString(dados.ordersToClassificate(codeUser),"Entregas a Classificar");
        if(!codeOrder.equals("<b")){
        double classification = menu.lerDouble("Insira a classificação de 1 a 10");
        dados.classificateOrder(codeOrder,classification);}
    }

    public void showPastOrders(String code,int type){
       try {
        menu.showPastOrders(dados.getPastOrders(code,type),dados.getStores());
        menu.lerString("Para sair digite \"<b\"");}
       catch (Exception e){System.out.println("Ocorreu um erro.");MenuCliente(code);}

    }

    public void criarEncomenda(String codeUser) {
        try {
            String codeVolunteerTransporter;
            String codeStore = menu.showStores(new ArrayList<>(dados.getStores().values()));
            if(!codeStore.equals("<b")){
            List<String> codeProducts = menu.showProducts(dados.getStores().get(codeStore).getAvailableProducts());
            boolean str=menu.lerBoolean("Deseja Transportadora? Sim/Não");
            if (str) codeVolunteerTransporter = selectTransporter(codeStore,codeProducts);
            else codeVolunteerTransporter = dados.getCloserVolunteerToStore(codeStore);
            if(codeVolunteerTransporter.startsWith("v") || codeVolunteerTransporter.startsWith("RV")) str=false;
            if(menu.lerBoolean("Deseja confirmar? Sim/Não")){
             String codigoEncomenda = dados.createEncomenda(codeUser,codeStore,str,codeVolunteerTransporter,codeProducts);
             LocalDate entregaPrevista = dados.createDeliveryExpectedDate(codeVolunteerTransporter,str);
             System.out.println("Codigo de Encomenda: " + codigoEncomenda + " Data Prevista de Entrega:" +entregaPrevista);}
            }
        }catch (Exception e){System.out.println(e.getMessage());System.out.println("Encomenda nao realizada, tente novamente");}
        MenuCliente(codeUser);
    }

    private String selectTransporter(String codeStore,List<String> products) throws ContaNãoExistente {
        String codeTransporter = "";
        try {
            ArrayList<String> codigosNaoDesejados = new ArrayList<>();
            codeTransporter = dados.getCloserTransporterToStore(codeStore, codigosNaoDesejados);
            double price = dados.getPriceT(codeTransporter, codeStore, products);
            boolean accepted = menu.lerBoolean("O preço será " + price + ", aceita? Sim ou Não.");
            while (!accepted) {
                codigosNaoDesejados.add(codeTransporter);
                codeTransporter = dados.getCloserTransporterToStore(codeStore, codigosNaoDesejados);
                price = dados.getPriceT(codeTransporter, codeStore, products);
                accepted = menu.lerBoolean("O preço será " + price + ", aceita? Sim ou Não.");

            }
        }catch (ContaNãoExistente e){System.out.println(e.getMessage());
            codeTransporter = dados.getCloserVolunteerToStore(codeStore);
            System.out.println(codeTransporter);
        }
        return codeTransporter;
    }


}
