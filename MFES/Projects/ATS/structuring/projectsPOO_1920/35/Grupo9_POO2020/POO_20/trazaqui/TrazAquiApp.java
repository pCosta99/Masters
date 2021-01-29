import java.io.IOException;
import java.util.Scanner;
import java.util.List;
import java.util.ArrayList;
import java.util.Deque;
import java.util.ArrayDeque;
import java.util.InputMismatchException;
import java.util.Map;
import java.util.TreeMap;
import java.time.LocalDate;

public class TrazAquiApp {
    private TrazAquiApp () {}
    private static TrazAqui trazaqui;
    private static Menu mainMenu, menuRegistar, menuCliente, menuLoja, menuVoluntario, menuEmpresaTransp;
    public static String file_name = "trazaqui.data";
    public static void main(String [] args){
         loadMenus();
         loadDados();
         imprimeMainMenu();
         
        
         try{
             trazaqui.gravar(file_name);
         }
         catch(IOException e){
             System.out.println("Não foi possível gravar os dados!");
         }
    }
    
    private static void loadDados(){
        try{
            trazaqui = TrazAqui.initApp(file_name);
        }
        catch (IOException e){
            trazaqui = new TrazAqui();
            System.out.println("Não foi possível ler os dados!\nErro de leitura!");
        }
        catch (ClassNotFoundException e){
            trazaqui = new TrazAqui();
            System.out.println("Não foi possível ler os dados!\nFicheiro com formato desconhecido!");
        }
        catch (ClassCastException e){
            trazaqui = new TrazAqui();
            System.out.println("Não foi possível ler os dados!\nErro de formato!");
        }
    }
    
    private static void loadMenus(){       
        String [] main = { "Iniciar Sessão",
                           "Registar Utilizador",
                           "Top 10 Clientes com mais encomendas associadas",
                           "Top 10 Empresas Transportadoras com mais kms percorridos"
        };
        
        String [] registar = { "Registar Cliente",
                               "Registar Loja",
                               "Registar Voluntário",
                               "Registar Empresa Transportadora"
        };
        
        String [] cliente = { "Requisitar Entrega de Encomenda",
                              "Classificar Serviço de Entrega",
                              "Consultar Histórico de Encomendas"
        };
        
        String [] loja = { "Sinalizar Encomenda a ser entregue",
                           "Nº de Pessoas em Fila de Espera"
        };
         
        String [] voluntario = { "Consultar Histórico de Encomendas Entregues",
                                 "Consultar Classificação"
        };
        
        String [] empresatransp = { "Consultar Histórico de Encomendas Entregues",
                                    "Consultar Classificação",
                                    "Consultar Total Faturado"
        };
        
        mainMenu = new Menu(main);
        menuRegistar = new Menu(registar);
        menuCliente = new Menu(cliente);
        menuLoja = new Menu(loja);
        menuVoluntario = new Menu(voluntario);
        menuEmpresaTransp = new Menu(empresatransp);
    }
    
    private static void imprimeMainMenu(){
        int op;
        do{
            mainMenu.executa();
            op = mainMenu.getOpcao();
            switch(op){
                case 1: iniciaSessao(); break;
                case 2: registaUtilizador(); break;
                case 3: top10Clientes(); break;
                case 4: top10EmpresasTransportadoras(); break;
            }
            
        }while (op != 0);
        
        if(op == 0){
            trazaqui.fechaSessao();
        }
    }
    
    private static void iniciaSessao(){
        Scanner input = new Scanner(System.in);
        String email,password;
        
        System.out.println("Email: ");
        email = input.nextLine();
        
        System.out.println("Password: ");
        password = input.nextLine();
        
        try{
            trazaqui.iniciarSessao(email,password);
        }
        catch (SemAutorizacaoException e){
            System.out.println(e.getMessage());
        }
        finally{
            input.close();
        }
        
        switch(trazaqui.getTipoUtilizador()){ 
            case 1:imprimeMenuCliente(); break;
            case 2:imprimeMenuLoja(); break;
            case 3:imprimeMenuVoluntario(); break;
            case 4:imprimeMenuEmpresaTransp(); break;
        }
    }
    
    private static void imprimeMenuCliente(){
        int op;
        
        do{
            menuCliente.executa();
            op = menuCliente.getOpcao();
            switch(op){
                case 1: requisitaEntrega(); break;
                case 2: avaliaTransportador(); break;
                case 3: consultaHistoricoEnc(); 
            }
        }while (op != 0);
        
        if(op == 0) trazaqui.fechaSessao();
    }
    
   private static void imprimeMenuLoja(){
       int op;
        
       do{
           menuLoja.executa();
           op = menuLoja.getOpcao();
           switch(op){
               case 1: sinalizaEncomenda(); break;
               case 2: atualFilaAtendimento(); break;
           }
       }while (op != 0);
        
       if(op == 0) trazaqui.fechaSessao();
    }
    
    private static void imprimeMenuVoluntario(){
        int op;
        
        do{
            menuVoluntario.executa();
            op = menuVoluntario.getOpcao();
            
            switch(op){
                case 1: consultaHistoricoEncEntregueV(); break;
                case 2: consultaClassificacao(); break;
            }
          
        }while (op != 0);
        
        if(op == 0) trazaqui.fechaSessao();
    }
    
    private static void imprimeMenuEmpresaTransp(){
        int op;
        
        do{
            menuEmpresaTransp.executa();
            op = menuEmpresaTransp.getOpcao();
            switch(op){
                case 1: consultaHistoricoEncEntregueET(); break;
                case 2: consultaClassificacao(); break;
                case 3: totalFaturadoET(); 
            }
        }while (op != 0);
        
        if(op == 0) trazaqui.fechaSessao();
    }
    
    private static void registaUtilizador(){
        int op;
        String email,nome,password,morada;
        Scanner input = new Scanner(System.in);
        Utilizador u = null;
        menuRegistar.executa();
        op = menuRegistar.getOpcao();
        
        if(op == 0){
            System.out.println("Registo Cancelado");
            return;
        }
        
        System.out.println("Insira o seu email: ");
        email = input.nextLine();

        System.out.println("Insira o seu nome: ");
        nome = input.nextLine();

        System.out.println("Insira a password: ");
        password = input.nextLine();

        System.out.println("Insira a sua morada: ");
        morada = input.nextLine();
        
        switch(op){
            case 0: input.close(); return;
            case 1: u = new Cliente(); break;
            case 2: u = new Loja(); break;
            case 3: u = new Voluntario(); break;
            case 4: u = new EmpresaTransp();         
        }
        
        u.setEmail(email);
        u.setNome(nome);
        u.setPassword(password);
        u.setMorada(morada);
   
        try{
            trazaqui.registaUtilizador(u);
        }
        catch (UtilizadorExistenteException e){
            System.out.println(e.getMessage());
        }
        finally{
            input.close();
        }
    }
   
    private static void top10Clientes(){
        List<Cliente> top10;
        top10 = trazaqui.top10C();
        
        for(Cliente c: top10){
            System.out.println(c.getNome());
        }   
    }
    
    private static void top10EmpresasTransportadoras(){
        List<EmpresaTransp> top10t;
        top10t = trazaqui.top10ET();
        
        for(EmpresaTransp t: top10t){
            System.out.println(t.getNome());
        }
    }
    
    private static void avaliaTransportador(){
        Scanner input = new Scanner(System.in);
        int avaliacao;
        String email;
        
        System.out.println("Insira o email do transportador que pretende avaliar: ");
        email = input.nextLine();
        
        avaliacao = lerInt("Insira a avaliação que pretende atribuir: ");
        Avaliacao a = new Avaliacao(avaliacao);
        
        try{
            trazaqui.classificarTransportador(a,email);
        }catch(SemAutorizacaoException|TransportadorInexistenteException e){
            System.out.println(e.getMessage());
        }
        finally{
            input.close();
        }
    }
    
    //consulta de encomendas por parte de um cliente
    private static void consultaHistoricoEnc(){
        Cliente c = (Cliente) trazaqui.getUtilizador();
        List<Encomenda> hist = (List<Encomenda>) c.getHist();
         
        for(Encomenda e : hist){
            System.out.println(e.toString());
        }
    }
    
    //historico de encomendas transportadas por um volunatario
    private static void consultaHistoricoEncEntregueV(){
        Voluntario v = (Voluntario) trazaqui.getUtilizador();
        List<Encomenda> historico = (List<Encomenda>) v.getHistorico();
            
        for(Encomenda e : historico)
            System.out.println(e.toString());
    }
    
    //consultar a classificacao de um transportador
    private static void consultaClassificacao(){
        Transportador t = (Transportador) trazaqui.getUtilizador();
        double avaliacao = t.getClassificacao();
            
        System.out.println("A sua classificação é: " + avaliacao);
    }
    
    //historico de encomendas transportadas por uma transportadora
    private static void consultaHistoricoEncEntregueET(){
        EmpresaTransp et = (EmpresaTransp) trazaqui.getUtilizador();
        List<Encomenda> historico = (List<Encomenda>) et.getHistorico();
            
        for(Encomenda e : historico)
            System.out.println(e.toString());
    }
    
    //total faturado por empresa entre datas
    private static void totalFaturadoET(){
        Scanner input = new Scanner(System.in);
        String email;
        int dI,mI,aI,dF,mF,aF;
        String inicio,fim;
       
        email = lerString("Insira o email da Empresa que quer consultar: ");
        
        System.out.println("Insira a data inicial");
        aI = lerInt("Ano: ");
        mI = lerInt("Mês: ");
        dI = lerInt("Dia: ");
        
        LocalDate i = LocalDate.of(aI,mI,dI);
        
        System.out.println("Insira a data final");
        aF = lerInt("Ano: ");
        mF = lerInt("Mês: ");
        dF = lerInt("Dia: ");
        
        LocalDate f = LocalDate.of(aF,mF,dF);
        
        try{
            double total = trazaqui.totalFaturado(email,i,f);
            System.out.println("O total faturado pela Empresa Transportadora entre o período indicado é " + total);
        }
        catch(SemAutorizacaoException e){
            System.out.println(e.getMessage());
        }
        finally{
            input.close();
        }
    }
    
    //caso o utilizador coloque um id num formato diferente de string
    private static String lerString(String msg){
        Scanner input = new Scanner(System.in);
        String s = "";

        System.out.println(msg);
        
        try{
            s = input.nextLine();
        }
        catch (InputMismatchException e){
            System.out.println("Formato errado!");
            s = lerString(msg);
        }
        finally{
            input.close();
        }
        return s;
    }
    
    //caso o cliente insira uma classificação sem ser com um int
    private static int lerInt(String msg){
        Scanner input = new Scanner(System.in);
        int i = 0;
        
        System.out.println(msg);
        
        try{
            i = input.nextInt();
        }
        catch (InputMismatchException e){
            System.out.println("Formato errado!");
            i = lerInt(msg);
        }
        finally{
            input.close();
        }
        return i;
    }
   
    //quando a loja quer sinalizar uma encomenda pra entrega
    private static void sinalizaEncomenda(){
        Loja l= (Loja) trazaqui.getUtilizador();
        l.setEncParaEntrega(true);
       
    }
    
    //quando a loja quer atualizar o tamanho da fila de espera
    private static void atualFilaAtendimento(){
        Scanner input = new Scanner(System.in);
        Loja l= (Loja) trazaqui.getUtilizador();
        int tfa;
        System.out.println("O tamanho da fila de espera registado no seu estabelecimento é:"+ l.getTamanhoFila());
        System.out.println("Indique o tamanho da fila de espera no seu estabelecimento:");
        tfa=input.nextInt();
        l.setTamanhoFila(tfa);
        System.out.println("O tamanho da fila de espera do seu estabelecimento foi atualizado para:"+l.getTamanhoFila());
    }
}