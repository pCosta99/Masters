import java.io.*;
import java.util.*;
import java.time.LocalDate;
import java.lang.*;
import java.text.*;
import java.time.*;

/**
 * Escreva a descrição da classe App aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class App
{
    // Menus da aplicação
    private Menu menuInicial;
    private Menu menuRegisto;
    private Menu menuUtilizador;
    private Menu menuVoluntario;
    private Menu menuTransportadora;
    private Menu menuLoja;
    private Menu menuEncomenda;
    private Dados d = new Dados();
    
    /**
     * Construtor.
     * 
     * Cria os menus e a camada de negócio.
     */
    
    public App(){
        String[] opcoes_Inicial = {"Login", "Registo", "Guardar Estado", "Carregar logs"};
        String[] opcoes_Registo = {"Registar como Utilizador",  "Registar como Loja", "Registar como Empresa Transportadora","Registar como Voluntário"};
        String[] opcoes_Utilizador = {"Ver perfil", "Solicitar Encomenda","Todos as encomendas efetuadas", "Classificar entrega","Custo total por uma encomenda","Lista dos 10 utilizadores que mais utilizam o sistema (em número de encomendas transportadas)"};
        String[] opcoes_Voluntario = {"Ver perfil", "Efetuar Entrega", "Todas as entregas efetuadas","Alterar Disponibilidade de um voluntário",
                                        "Verificar Certificado de transporte de medicamentos"}; 
        String[] opcoes_Transportadora = {"Ver perfil","Recolher Encomenda + Total Faturado + Tempo demorado", "Todas as entregas efetuadas",
                                           "Verificar Certificado de transporte de medicamentos",
                                           "Lista das 10 empresas transportadoras que mais utilizam o sistema (em número de kms percorridos)",
                                           "Lista dos 10 utilizadores que mais utilizam o sistema (em número de encomendas transportadas)"};
        String[] opcoes_Loja = {"Ver perfil", "Novo pedido + informação de encomenda","Altera Preço de um Produto","Alterar Disponibilidade de um Produto",
                                "Todos os pedidos efetuados","Total Faturado"}; //"Quantidade de pessoas em fila para serem atendidas" nº6
        String[] opcoes_Encomenda = {"Voluntário mais próximo", "Solicitar Entrega Transportadora"};
                                       
        this.menuInicial = new Menu(opcoes_Inicial);
        this.menuRegisto = new Menu(opcoes_Registo);
        this.menuUtilizador= new Menu(opcoes_Utilizador);
        this.menuVoluntario = new Menu(opcoes_Voluntario);
        this.menuTransportadora = new Menu(opcoes_Transportadora);
        this.menuLoja = new Menu(opcoes_Loja);
        this.menuEncomenda = new Menu(opcoes_Encomenda);
    }
    
    /**
     * Carrega os dados
     */
    
    public void carrega(){
        try {
            Dados da = new Dados();
                this.d = da.abrirFicheiro("/home/patricia/Desktop/POO-Project-master/backup.txt"); //  /home/patricia/Desktop/POO-Project-master/logs.txt
         //  System.out.println(d.getUtilizadores().get("u48").getEmail() + "ola");
        }
        catch (FileNotFoundException e){
            e.printStackTrace();
            System.out.println("Dados nao lidos!\nFicheiro nao encontrado.");
            d = new Dados();
        }
        catch (ClassNotFoundException e) {
            e.printStackTrace();
            System.out.println("Dados nao lidos!\nFicheiro com formato desconhecido.");
            d = new Dados();
        }
        catch (IOException e) {
            e.printStackTrace();
            System.out.println("Dados nao lidos!\nErro de leitura.");
            d = new Dados();
        }
        catch (ClassCastException e) {
            e.printStackTrace();
            System.out.println("Dados nao lidos!\nErro de formato.");
            d = new Dados();
        }
    }
    
    /*
     * Guarda os dados
     */
    public void guarda() {
        try {
            this.d.guardaFicheiro("/home/patricia/Desktop/POO-Project-master/backup.txt "); //  /home/patricia/Desktop/POO-Project-master/logs.txt
        }                  
        catch (FileNotFoundException e) {
            e.printStackTrace();
            System.out.println("Dados nao guardados!\nFicheiro nao encontrado.");
            d = new Dados();
        }
        catch (IOException e) {
            e.printStackTrace();
            System.out.println("Dados nao guardados!\nErro de escrita.");
            d = new Dados();
        }
    }
    
    /*
                ******* MENUS *******
     */   
    
    /*
     * Executa o menu principal e invoca o método correspondente à opção seleccionada.
     */    
    public void menu_Inicial() {
        carrega();
        int r = 1;
        while(r == 1){
            int op = menuInicial.executa();
            System.out.println(op);
            switch(op){
                case 1:loginUVTL();
                            break;
                case 2: menu_Registo();
                            break;
                case 3: guarda();
                            break;
                case 4: try {
                            this.d.carregaLogs("/home/patricia/Desktop/POO-Project-master/logs.bak");
                            System.out.println("DONE!");
                        }                  
                        catch (FileNotFoundException e) {
                            e.printStackTrace();
                            System.out.println("Dados nao guardados!\nFicheiro nao encontrado.");
                        }
                        catch (IOException e) {
                            e.printStackTrace();
                            System.out.println("Dados nao guardados!\nErro de escrita.");
                        }
                            break;
                case 0: System.out.println("Obrigado, volte sempre!");
                        r = 0;
                            break;
            }
        }
        guarda();
    }  
    
    
    
    /**
     * Metodo responsavel por distinguir o registo que seguidamente sera feito (Cliente ou Loja)
     */
     public void menu_Registo() {
         int r = 1;
         while (r==1) {
             int op = menuRegisto.executa();
           switch(op){
            case 1: registoUVTL(1);
                        break; 
            case 2: registoUVTL(2);
                        break;
            case 3: registoUVTL(3);
                        break;
            case 4: registoUVTL(4);
                        break;
               case 0: r = 0;
                        break;
            }
        if(op == -1)
            menu_Registo();    
        }
     }
    
    /**
     * Main cria a aplicação e invoca o método exec
     */
    public static void main(String[] args) {
       new App().menu_Inicial();
    }

    /**
     * Metodo que disponibiliza um menu de opções ao Utilizador
     * @param u O utilizador
     * @param l a Loja
     * @param aux2 as Entregas recebidas
     * @param p os produtos solicitados
     */
    
    public void menu_Utilizador (Utilizador u, Loja l, Encomenda e,Entregas aux2, Produto p,  Transportadora t){
        int r = 1;
        if (u.verificaEncomendaPendente()){
            Encomenda en = u.encomendaPendente();
            System.out.println("O teu pedido de encomenda foi aceite. \n");
            efetuarEncomenda(u, p, aux2, en);
        }
        if (u.verificaEncomendaRejeitada()) {
            System.out.println("O teu pedido de encomenda nao foi aceite. \n");
            Encomenda aux = u.encomendaPendente3();
            d.removeEncomenda(aux2.getCodeUser(), aux2.getCodeLoja(), p.getCodProduto(), aux);
        }
        while (r == 1){
            int op = menuUtilizador.executa();
            switch(op){
                case 0 : r = 0;
                            break;
                case 1 : perfilUser(u);
                            break;
                case 2 : ArrayList<Loja> loj = new ArrayList();
                         ArrayList<Produto> lista = new ArrayList();
                         ArrayList<Voluntarios> vol = new ArrayList();
                         menu_Encomenda(e, aux2, u, p,t, l, loj, vol,lista);
                         break;
                case 3 : todas_EncomendasUser(u);
                            break;
                case 4 : top10(1);
                            break;
                case 5 : top10(2);
                            break;
                case 6 : u.alteraClassificacao(u.getClassificacao());
                            break;             
                //totalFaturado 
            }
            u = d.codeToUser(u.getCod());
        }
    }
    
    /**
     * Metodo que disponibiliza um menu de opçoes ao Voluntario apos login
     * 
     * @param  v o Voluntario que acabou de dar login
     */
    public void menu_Voluntarios (Voluntarios v, Entregas e){
        int r = 1;

        while(r == 1){
            int op = menuVoluntario.executa();
            switch(op){
                case 0: r = 0;
                        break;
                case 1: perfilVol(v);
                        break;
                case 2: if(v.dispostoFazerEntrega()){
                        Encomenda en = v.escolhaEncomenda();
                        System.out.println("Pode fazer a entrega da encomenda pretendida. \n");
                        efetuarEntregaVol(v, en, e, e.getKm(), e.getGpsX(), e.getGpsY());
                        }
                        break;
                case 3: todas_EntregasVol(v);
                        break;  
                case 5: if(v.aceitoTransporteMedicamentos()){
                        System.out.println("Aceitou o pedido de encomenda de produtos medicais. \n");
                        Encomenda en = v.escolhaEncomenda3();
                        efetuarEntregaVol(v, en,e, e.getKm(), e.getGpsX(), e.getGpsY());
                        }      
                        break;

                case 6 : top10(1);
                            break;
                case 7 : top10(2);
                            break;
                        
                        //alterar dispo
            }
                    v = d.codeToVoluntario(v.getEmail());
        }
    }
     
    /**
     * Metodo que disponibiliza um menu de opçoes a Empresa apos login
     * 
     * @param  t a Empresa que acabou de dar login
     */
    public void menu_Transportadora(Transportadora t, Entregas e){
        int r = 1;
        while(r == 1){
            int op = menuTransportadora.executa();
            switch(op){
                case 0: r = 0;
                        break;
                case 1: perfilEmpresa(t);
                        break;
                case 2: if(t.dispostoFazerEntrega()){
                        Encomenda en = t.escolhaEncomenda();
                        System.out.println("Pode fazer a entrega da encomenda pretendida. \n");
                        efetuarEntregaTransp(t, en,e, e.getKm(), e.getGpsX(), e.getGpsY());
                       }    
                       break;
                case 3: todas_EntregasEmpresa(t);
                        break;
                case 4: if(t.aceitoTransporteMedicamentos()){
                         System.out.println("Aceitou o pedido de encomenda de produtos medicais. \n");
                         Encomenda en = t.escolhaEncomenda3();
                         efetuarEntregaTransp(t, en,e, e.getKm(), e.getGpsX(), e.getGpsY());
                        }
                case 6 : top10(1);
                    break;
                case 7 : top10(2);
                    break;
            }
                    t = d.codeToEmpresa(t.getEmail());
        }
    }
        
        
    /**
     * Metodo que disponibiliza um menu de opçoes a Loja apos login
     * 
     * @param  l a Loja que acabou de dar login
     */
    public void menu_Loja (Loja l,Entregas e, Encomenda en1) {
        int r = 1;
        Scanner in = new Scanner(System.in);
        if(l.verificaEncomendaPendente()){
            Encomenda en= l.encomendaPendente();
            System.out.println("Tem um pedido pendente do utilizador " + e.getCodeUser() + ". Quer aceitar?\nSim(1)\nNao(0)\n");
            int opcao = in.nextInt();
            if(opcao == 1){
                d.removeEncomenda(e.getCodeUser(), e.getCodeLoja(), e.getCodEncomenda(), en);
                en.setEstado(2);
                d.alteraAntesEntrega(e,e.getCodeUser(), e.getCodeLoja(), e.getCodEncomenda(), en);
            }
            else if(opcao == 0){
                d.removeEncomenda(e.getCodeUser(), e.getCodeLoja(), e.getCodEncomenda(), en);
                en.setEstado(3);
                d.alteraAntesEntrega(e,e.getCodeUser(), e.getCodeLoja(), e.getCodEncomenda(), en);
            }
            else{
                System.out.println("Opçao invalida!\n");
                menu_Loja(l,e,en1);
            }
        }
        while(r == 1){
            int op = menuLoja.executa();
            switch(op){
                case 0: r = 0;     
                            break;
                case 1: perfilLoja(l);
                            break;
                case 2: d.insereEncomendaLoj(l.getCod(),en1.getCodEncomenda(),en1);
                        break;
                case 4: top10(1);
                        break;
                case 5: top10(2);
                        break;
                case 6: todas_EntregasLoja(l);
                        break;
            }
            l = d.codeToLoja(l.getEmail());
        } 
    }              
    /**
     * Metodo que disponibiliza um menu de opcoes de encomenda para o utilizador
     * 
     * @param  u o Utilizador que efetuara a encomenda
     */
    
    public void menu_Encomenda(Encomenda e,Entregas en,Utilizador u,Produto p, Transportadora t, Loja l,List<Loja> loj, List<Voluntarios> vol, List<Produto> lista){
        int r = 1;
        while(r == 1){
            int op = menuEncomenda.executa();
            if(op == 0)
                 r = 0;
            else{
                System.out.println("Preencha as coordenadas correspondentes à sua localização.");
                double x = validaCoord("x");
                double y = validaCoord("y");
                System.out.println("Preencha as coordenadas correspondentes a loja pretendida.");
                double gpsX = validaCoord("x");
                double gpsY = validaCoord("y");
                double dist = d.dist(x,y,gpsX,gpsY);
                if(u == null) {
                    switch (op) {
                        case 0:
                            r = 0;
                        case 1:
                            tipoEncomenda(u, t, en, e, x, y, dist, vol, 1, gpsX, gpsY);
                        case 2:
                            tipoEncomenda(u, t, en, e, x, y, dist, vol, 2, gpsX, gpsY);
                    }
                }
            }
            r = 0;
            e = d.codigoToEncomenda(e.getCodEncomenda());
        }
    }

    
    /*
                *******METODOS DE REGISTO*******
     */
    /**
     * Registra um novo Utilizador ou Loja ou Empresa ou Voluntário
     * 
     * @param  opcao Utilizador(1) ou Loja(2) ou Empresa(3) ou Voluntário(4)
     *
    **/
    public void registoUVTL (int opcao) {
        Scanner in = new Scanner (System.in);
        boolean validade = false;
        String userCode = null;
        String nome = null;
        String email = null;
        String passx2 = null;
        String password = null;
        double gpsx, gpsy, raio, precoKm;
        
        System.out.println("\n*** REGISTO *** ");
        
        while (!validade) {
            System.out.print("User Code: ");
            userCode = in.nextLine();
            try{
                validade = d.codigoValido(userCode);
            } catch(CodigoInvalidoException c){
                System.out.println("Codigo invalido. Introduza um codigo diferente.");
                validade = false;
            }
         }
        validade = false;

         while (!validade) {
            System.out.print("Email: ");
            email = in.nextLine();
            try{
                validade = d.emailValido(email);
            } catch(EmailInvalidoException e){
                System.out.println("Email invalido. Introduza um email diferente.");
                validade = false;
            }
         }
        
        validade = false;
        
        while (!validade) {
            System.out.print("Nome: ");
            nome = in.nextLine();
            try{
                validade = d.nomeValido(nome);
            } catch(NomeInvalidoException e){
                System.out.println("Nome invalido. Tente novamente.");
                validade = false;
            }
        }        

        validade = false;
        
        while (!validade) {
            System.out.print("Password: ");
            password = in.nextLine();
            System.out.print("Confirmação da Password: ");
            passx2 = in.nextLine();
            
            try {
                validade = d.passwordValida(password, passx2);
            }
            catch (PasswordInvalidaException e) {
                System.out.println("Erro na password. Insira novamente.");
                validade = false;
            }
        }

        System.out.print("Morada: ");
        String morada = in.nextLine();
                
        String data = null;
        LocalDate date = null;
        while(date == null){
            System.out.println("Data de Nascimento\nExemplo: 17/09/1999");
            data = in.nextLine();
            date = convertToDate(data);
        }
        
        System.out.println("NIF: ");
        String nif = in.nextLine(); 
        
        System.out.println("Coordenada X onde se encontra: ");
        gpsx = in.nextDouble();
        
        System.out.println("Coordenada Y onde se encontra: ");
        gpsy = in.nextDouble();
        

        
        if (opcao == 1){
            registoUtilizador(userCode, email, nome, password, morada, date, Integer.parseInt(nif), gpsx,gpsy);
        }
        else {
            if (opcao == 2) {
                registoLoja(userCode, email, nome, password, Integer.parseInt(nif), gpsx, gpsy);
            } else {
                if (opcao == 3) {
                    System.out.println("Raio: ");
                    raio = in.nextDouble();
                    System.out.println("Preço Por Quilometros: ");
                    precoKm = in.nextDouble();
                    registoEmpresa(userCode, email, nome, password, Integer.parseInt(nif), gpsx, gpsy, raio, precoKm);
                } else {
                    if (opcao == 4) {
                        System.out.println("Raio: ");
                        raio = in.nextDouble();
                        registoVoluntario(userCode, email, nome, password, Integer.parseInt(nif), gpsx, gpsy, raio);
                    }
                }
            }
        }
        in.close();
     }
     
     /*
     * Metodo que regista um utilizador na aplicaçao
     * 
     * @param  codUtilizador o codigo do utilizador a registar 
     * @param  email o email a registar
     * @param  nome o nome a registar
     * @param  password a pass a registar
     * @param  morada a morada a registar
     * @param  data a data a registar
     * @param  nif  o nif a registar
     * @param  gpsX  a registar
     * @param  gpsY  a registar
     * 
     */
     public void registoUtilizador (String codUtilizador, String email, String nome, String password, String morada, LocalDate data, int nif,double x,double y) {
        Utilizador u = new Utilizador();

        u.setCodUtilizador(codUtilizador);
        u.setEmail(email);
        u.setNome(nome);
        u.setPassword(password);
        u.setMorada(morada);
        u.setDDN(data);
        u.setNif(nif);
        u.setGPSx(x);
        u.setGPSy(y);
        
        d.registarUtilizador(u);
        
    }  
    
    /*
     * Metodo que regista uma loja na aplicaçao
     * 
     * @param  codLoja o codigo da loja a registar
     * @param  email o email a registar
     * @param  nome o nome a registar
     * @param  password a pass a registar
     * @param  gpsX  a registar
     * @param  gpsY  a registar
     * @param  nif   a registar
     */
    public void registoLoja (String codLoja, String email, String nome, String password, int nif, double x,double y) {
        Loja l = new Loja();
        
        l.setCod(codLoja);
        l.setEmail(email);
        l.setNome(nome);
        l.setPassword(password);
        l.setNif(nif);
        l.setGPSx(x);
        l.setGPSy(y);
        
        d.registarLoja(l);
    }
    
    /**
     * Metodo que regista uma empresa transportadora na aplicaçao
     * 
     * @param  codEmpresa o codigo a registar
     * @param  email o email a registar
     * @param  nome o nome a registar
     * @param  password a pass a registar
     * @param  nif  a registar
     * @param  x  o gpsX
     * @param  y  o gpsY
     * @param  r  raio da empresa
     * @param  pk preco por quilometros
     */
    public void registoEmpresa (String codEmpresa, String email, String nome, String password, int nif,double x, double y, double r, double pk) {
        Transportadora t = new Transportadora();
        
        t.setCod(codEmpresa);
        t.setEmail(email);
        t.setNome(nome);
        t.setPassword(password);
        t.setNif(nif);
        t.setGPSx(x);
        t.setGPSy(y);
        t.setRaio(r);
        t.setPrecoKm(pk);
        
        d.registarTransportadora(t);
    }
    
    /**
     * Metodo que regista um voluntario na aplicaçao
     * 
     * @param  email o email a registar
     * @param  nome o nome a registar
     * @param  password a pass a registar
     * @param  nif  a registar
     */
    public void registoVoluntario (String codVoluntario,String email, String nome, String password, int nif, double x,double y, double r ) {
        Voluntarios v = new Voluntarios();
        
        v.setCod(codVoluntario);
        v.setEmail(email);
        v.setNome(nome);
        v.setPassword(password);
        v.setNif(nif);
        v.setGPSx(x);
        v.setGPSy(y);
        v.setRaio(r);
        
        d.registarVoluntario(v);
    }
    
    
    /*
                ******* METODOS DE LOGIN *******
    */ 
    
    /**
     * Login do utilizador(Utilizador ou Loja)
    */
    public void loginUVTL(){
        Scanner input = new Scanner (System.in);
        String password = null;
        String cod = null;
        String email = null;
        Utilizador u = null;
        Loja l = null;
        Transportadora t = null;
        Voluntarios v = null;
        Entregas en = null;
        Encomenda e = null;
        Produto p = null;
        
        System.out.println("\n*** LOGIN");
        while (cod == null){
            System.out.println("UserCode (começando com a letra correspondente ao seu estatuto seguida por dois digitos):");
            String c = input.nextLine();
            try{
                d.existeCodigo(c);
                u = d.codeToUser(c);
                l = d.codeToLoja(c);
                t = d.codeToEmpresa(c);
                v = d.codeToVoluntario(c);
                break;

            } catch (CodigoInexistenteException a) {
                System.out.println("Codigo inexistente.");
                break;
            }
        }
        
        while (email == null){
            System.out.print("Email: ");
            String em = input.nextLine();
            
            try{
                d.existeEmail(em);
                u = d.mailToUser(em);
                l = d.mailToLoja(em);
                t = d.mailToEmpresa(em);
                v = d.mailToVoluntario(em);
                break;
            }
            catch(EmailInexistenteException ei){
                System.out.println("Email inexistente.");
                break;
            }
        }
       
        int r = 1;
        if( u != null){
            System.out.print("Password: ");
            password = input.nextLine();
            
            if(!(u.getPassword().equals(password))){
                System.out.println("Password incorreta.");
                r = 0;
            }
            
            if(r == 1)
                menu_Utilizador(u,l,e,en,p,t);
        }
        else if( l !=null){
            System.out.print("Password: ");
            password = input.nextLine();
            
            if (!(l.getPassword().equals(password))){
                System.out.println("Password incorreta.");
                r = 0;
            }
            
            if(r == 1)
                menu_Loja(l,en,e);
        }
        else if ( t != null){
            System.out.print("Password: ");
            password = input.nextLine();
            
            if (!(t.getPassword().equals(password))){
                System.out.println("Password incorreta.");
                r = 0;
            }
            
            if(r == 1)
                menu_Transportadora(t,en);
        }
        else if ( v != null){
            System.out.print("Password: ");
            password = input.nextLine();
            
            if (!(t.getPassword().equals(password))){
                System.out.println("Password incorreta.");
                r = 0;
            }
            
            if(r == 1)
                menu_Voluntarios(v,en);
        }
            else {
            System.out.println("Usuário não encontrado.");
        }
        input.close();
    }
    /**
     * Metodo que procura o produto conforme a opçao que o Utilizador escolheu
     * 
     * @param  u utilizador que se encontra no sistema
     * @param opcao que o Utilizador tomou
     * @param  x a coordenada x atual
     * @param  y a coordenada y atual
     * @param  xDestino a coordenada x destino
     * @param  yDestino a coordenada y destino
     */        
    public void tipoEncomenda(Utilizador u,Transportadora t,Entregas en,Encomenda e, double x, double y, double d, List <Voluntarios> vol, int opcao,double xDestino, double yDestino){
        switch (opcao) {
            case 1 : volMaisProximo(u,en,e,vol,d, x , y ,xDestino,yDestino);
            case 2 : efetuarEntregaTransp(t,e,en,d,x,y);
        }
    }

    /**
     * Metodo que encontra a loja mais proxima e inicia o processo de efetuar encomenda
     * 
     * @param  u Utilizador que se encontra no sistema
     * @param  x a coordenada x atual
     * @param  y a coordenada y atual
     * @param  xDestino a coordenada x destino
     * @param  yDestino a coordenada y destino
     */
    public void volMaisProximo(Utilizador u,Entregas en, Encomenda e, List<Voluntarios> disponiveis,double dist, double x, double y, double xDestino, double yDestino){
        Voluntarios maisproximo = d.proximo(disponiveis, u, x, y);
        efetuarEntregaVol(maisproximo,e,en,dist, xDestino, yDestino);
    }

    
    public void efetuarEntregaVol(Voluntarios v, Encomenda e, Entregas en, double distancia, double xDestino, double yDestino){
        Scanner input = new Scanner (System.in);
         double distanciaEntrega = d.dist (en.getGpsX(), en.getGpsY(), xDestino, yDestino);
         double tempoViagem = d.tempoVol(en.getGpsX(), en.getGpsY(), xDestino, yDestino);
         String emailUser = en.getCodeUser();
         String emailVoluntario = en.getCodeVoluntario();
         String emailLoja = en.getCodeLoja();
         String emailEmpresa = en.getCodeEmpresa();
         String codEncomenda = e.getCodEncomenda();
         if(!v.dispostoFazerEntrega()){
             Entregas ent = new Entregas(emailUser, emailLoja,emailVoluntario, emailEmpresa, codEncomenda, 1, distanciaEntrega, xDestino, yDestino);     
             d.alteraAntesEntrega(ent ,emailUser, emailLoja,codEncomenda, e);
             System.out.println("Já pode anular a entrega do pedido" + codEncomenda + ".\n");
        }
        else {
               System.out.println("A viagem terá uma duração estimada de " + tempoViagem + " segundos.");
            }
    }
      
    public void efetuarEntregaTransp(Transportadora t, Encomenda en, Entregas e, double distancia, double xDestino,double yDestino){
         Scanner input = new Scanner (System.in);
         double distanciaEntrega = d.dist (e.getGpsX(), e.getGpsY(), xDestino, yDestino);
         double tempoViagem = d.tempoTrans(e.getGpsX(), e.getGpsY(), xDestino, yDestino);
         double transito = Math.random()*(1.5-1.0)+1.0;
         double tempoRealViagemEmpresa = tempoViagem * transito;
         
         String emailEmpresa = t.getCod();
         String emailUser = e.getCodeUser();
         String emailVoluntario = e.getCodeVoluntario();
         String emailLoja = e.getCodeLoja();
         String codEncomenda = e.getCodEncomenda();
         
         
         double custo = t.getPrecoKm() * distanciaEntrega;
         if(!t.dispostoFazerEntrega()){
             Entregas ent = new Entregas(emailUser, emailLoja,emailVoluntario, emailEmpresa, codEncomenda, 1, distanciaEntrega, xDestino, yDestino);     
             d.alteraAntesEntrega(ent, emailUser, emailLoja,codEncomenda, en);
            System.out.println("Já pode anular a entrega do pedido" + codEncomenda + ".\n");
        }
        else {
               System.out.println("A viagem terá uma duração estimada de "+ tempoViagem +" segundos.");
                System.out.println("\nA Entrega foi efetuada e teve uma duração de " + tempoRealViagemEmpresa + " segundos.\n");
                double diferencaTempo = tempoRealViagemEmpresa - tempoViagem;
                if (diferencaTempo != 0) {
                    System.out.println("A Entrega demorou mais " + diferencaTempo + " segundos que o suposto.\n");
                    double custoAdicional = t.getPrecoAdicional();
                    System.out.println("A entrega tera um custo adicional de " + custoAdicional + "€.\n");
                    double custoTotal = custo + custoAdicional;
                    System.out.println("E um custo total de " + custoTotal + "€.\n");
                }
                else{
                    System.out.println("A entrega tera um custo total de " + custo + "€.\n");
                }
            }
    }
    
    public void efetuarEncomenda(Utilizador u,Produto p,Entregas en,Encomenda e){
        Scanner input = new Scanner (System.in);
        
        Loja l = d.produtoToloja(p.getCodProduto());
        String codLoja = l.getCod();
        String codUser = u.getCod();
        String codEmpresa = en.getCodeEmpresa();
        String codEncomenda = e.getCodEncomenda();
        
        double custo = (p.getPreco()) * (p.getQuantidade()) ;
        if(u.verificaEncomendaPendente()){
            System.out.println("A encomenda tera um custo de " + custo + "euros.\n");
            d.alteraAntesEntrega(en,codUser, codLoja ,codEncomenda,e);
            System.out.println("O pedido foi efetuado. Agora a Transportadora " + codEmpresa + " precisa de aceitar ou nao o pedido.\n");
        }
        else{
            System.out.println("\n A encomenda foi efetuada.\n");
        }
        input.close();
    }
        
        
    /**
     * Disponibiliza o perfil de um Utilizador
     * 
     * @param  u o Utilizador cujo perfil sera disponibilizado
     */
    public void perfilUser (Utilizador u) {
        System.out.println("\n**** PERFIL ");
        System.out.print(u.toString()+"\n");
    }
    
    /**
     * Disponibiliza o perfil de uma Loja
     * 
     * @param  l a Loja cujo perfil sera disponibilizado
     */
    public void perfilLoja (Loja l) {
        System.out.println("\n**** PERFIL");
        System.out.print(l.toString()+"\n");
    }
    
    /**
     * Disponibiliza o perfil de uma Transportadora
     * 
     * @param  t a Transportadora cujo perfil sera disponibilizado
     */
    public void perfilEmpresa (Transportadora t) {
        System.out.println("\n**** PERFIL");
        System.out.print(t.toString()+"\n");
    }
    
    /**
     * Disponibiliza o perfil de um Voluntario
     * 
     * @param  v o Voluntario cujo perfil sera disponibilizado
     */
    public void perfilVol (Voluntarios v) {
        System.out.println("\n**** PERFIL");
        System.out.print(v.toString()+"\n");
    }
    
    
    public void top10(int op) {
        List<Utilizador> top10 = new ArrayList<Utilizador>();
        List<Transportadora> top_10 = new ArrayList<Transportadora>(); 
        if(op == 1){
             top10 = d.top10_Utilizacao();
        
             System.out.println("Os 10 Utilizadores que utilizaram mais o sistema: \n");
             for (Utilizador u : top10)
                System.out.println(u.toString_top10());
        }
        else{
            top_10 = d.top10_Distancia();
        
             System.out.println("As 10 Empresas que percorram mais kms: \n");
             for (Transportadora t : top_10)
                System.out.println(t.toString_top10_distancia());
        }
    }
    /*
                *******METODOS DE VALIDAÇAO*******
     */
    
/*
     * Metodo que valida um double
     * 
     * @param  x string que contem o double ou uma frase
     */
    public double validaCoord(String xy) {
        Scanner in = new Scanner (System.in);
        boolean validade = false;
        double x = 0.0;
        
        System.out.println("Coordenada " + x + ": ");
        while(!validade){
            try{
                validade = true;
                x = Double.parseDouble(in.nextLine());
            }
            catch (InputMismatchException e){
                validade = false;
                System.out.println("Coordenada inválida!");
                x = validaCoord(xy);
            }
        }
        in.close();
        return x;
    }
    
    /*
     * Metodo que valida um double positivo
     * 
     * @param xy string que contem um double ou uma fras
     */
    public double validaCoordPositiva(String y) {
        Scanner in = new Scanner (System.in);
        boolean validade = false;
        double x = 0.0;
        
        while(!validade){
            try{
                System.out.println(y);
                validade = true;
                x = Double.parseDouble(in.nextLine());
                
                if(x <= 0)
                    validade = false;
            }
            catch (InputMismatchException e){
                validade = false;
                System.out.println("Valor inválido!");
                x = validaCoordPositiva(y);
            }
        }
        in.close();
        return x;
    }
    
    /*
     * Metodo que valida um codigo de produto
     *
     * @param  xy string que contem o codigo
     */
    public String validaCodProduto(String c){
        Scanner in = new Scanner(System.in);
        boolean validade = false;
        String cod = null;
        while(!validade){
            try{
                System.out.println(c);
                validade = true;
                cod = in.nextLine();
                try{
                    if(cod.length() != 6 || d.existeProduto(cod)){
                        validade = false;
                        System.out.println("Codigo inválido!");
                        cod = validaCodProduto(c);

                    }
                }
                catch(ProdutoInexistenteException e){
                    validade = true;
                }
            }
            catch(InputMismatchException e){
                validade = false;
                System.out.println("Codigo inválido!");
                cod = validaCodProduto(c);
            }
        }
        in.close();
        return cod; 
    }
    
    
    /**
     * Metodo que valida uma classificacao
     * 
     * @param  classificacao string que contem a classificacao
     */
    public Double validaClassificacao(String classificacao){
        Scanner in = new Scanner(System.in);
        boolean validade = false;
        double x = 0.0;
        while(!validade){
            try{
                System.out.println(classificacao);
                validade = true;
                x = Double.parseDouble(in.nextLine());
                
                if(x < 0 || x > 100){
                    validade = false;
                }
            }
            catch(InputMismatchException e){
                validade = false;
                System.out.println("Classificacao inválida!");
                x = validaClassificacao(classificacao);
            }
        }
        in.close();
        return x; 
    }
    
     /*
                *******METODOS AUXILIARES*******
     */
    public void faturaProdutos(Loja l){
        Scanner in =new Scanner (System.in);
        System.out.println("Que produto pretende saber o total faturado?\n");
        int i = 0;
        if(l.getProdutos().isEmpty())
             System.out.println("Nao tem qualquer produto.\n");
        else{     
            for(Produto p : l.getProdutos()){
                System.out.println("Produto " + i + ":\n" + p.getCodProduto());
                i++;
            }
            int op = in.nextInt();
            if(op >= 0 && op < i){
                Produto p = l.getProdutos().get(op);
                System.out.println("Total faturado: " + p.getTotalFaturado());
            }
            else
                System.out.println("Opçao invalida. \n");
        }
    }
            
    public void alteraPrecoProd (Loja l){
    Scanner in = new Scanner (System.in);
        System.out.println("Que produto pretende alterar preco?\n");
        int i = 0;    
    if (l.getProdutos().isEmpty())
        System.out.println("Nao tem qualquer produto.\n");
    else {
        for( Produto p : l.getProdutos()){
            System.out.println("Produto " + i + ":\n" + p.getCodProduto());
                i++;
            }
        
         int op = in.nextInt();
            System.out.println("Novo preco: \n");
            double preco = in.nextDouble();
         Produto pro = null;
         if (op >= 0 && op < i){
             pro = l.getProdutos().get(op);
             pro.setPreco(preco);
             d.insereProduto(pro,l);
         }
         else
            System.out.println("Opçao invalida. \n");
        }
    }
    
    public void alterarDisponibilidade( Loja l){
        Scanner in = new Scanner (System.in);
        System.out.println("Que produto pretende alterar a disponibilidade?\n");
        int i = 0;
       
        if(l.getProdutos().isEmpty())
             System.out.println("Nao tem qualquer produto.\n");
        else{     
            for(Produto p : l.getProdutos()){
                System.out.println("Produto " + i + ":\n" + p.getCodProduto());
                i++;
            }     
         int op = in.nextInt();
            Produto pro = null;
            if(op >= 0 && op < i){
                pro = l.getProdutos().get(op);
                pro.setDisponibilidadeLoja(!pro.getDisponibilidadeLoja());
                d.insereProduto(pro,l);
            }
            else
                System.out.println("Opçao invalida. \n");
        }
    }
        
    public void todas_EntregasLoja(Loja l){
        System.out.println("Entregas efetuadas:\n");
        int i = 0;
        for(Encomenda e :l.getEncomendas()){
            for(Entregas en : e.getEntregas()){
            if(en.getPendente() == 0){
                System.out.println("Entrega" + i + ":\n" + en.toString());
                i++;
                }
            }
        }
    }
    
    public void todas_EncomendasUser(Utilizador u){
     System.out.println("Encomendas efetuadas:\n");
        int i = 0;
        for(Encomenda e :u.getEncomendas()){
            for(Entregas en : e.getEntregas()){
            if(en.getPendente() == 0){
                System.out.println("Encomenda" + i + ":\n" + e.toString());
                i++;
                }
            }
        }
    }
    public void todas_EntregasVol(Voluntarios v){
        System.out.println("Entregas efetuadas:\n");
        int i = 0;

        for(Entregas e :v.getEntregas()){
            if(e.getPendente() == 0){
                System.out.println("Entrega" + i + ":\n" + e.toString());
                i++;
                }
            }
    }
    
    public void todas_EntregasEmpresa(Transportadora t){
        System.out.println("Entregas efetuadas:\n");
        int i = 0;

        for(Entregas e :t.getEntregas()){
            if(e.getPendente() == 0){
                System.out.println("Entrega" + i + ":\n" + e.toString());
                i++;
                }
            }
    }

    
    
    /*

     * Metodo que converte uma string numa variavel do tipo Date
     * 
     * @param  input a string a converter
     * 
     * @return Date caso o input corresponda a uma data valida no formato valido, null caso o contrario
     */
    public LocalDate convertToDate(String in) {
        Date date = null;
        LocalDate ldate = null;
        if(null == in) {
            return null;
        }
        SimpleDateFormat format = new SimpleDateFormat("dd/MM/yyyy");
        try {
            format.setLenient(false);
            date = format.parse(in);
            ldate = date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        }catch (ParseException e) {
            System.out.println("A data inserida esta invalida");
        }
        return ldate;
    }  
}    