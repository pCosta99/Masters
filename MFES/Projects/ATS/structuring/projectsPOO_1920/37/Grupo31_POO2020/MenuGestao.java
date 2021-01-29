import java.util.Scanner;
import java.util.Random;
import java.io.*;
public class MenuGestao extends Menu  implements Serializable{
    // apenas um scanner para todas as operações
    static final Scanner in = new Scanner(System.in);
    // variável privada para o código da loja (quando faz login)
    private String codLoja;
    // variável privada para o código da empresa (quando faz login)
    private String codEmp;
    // variável privada para o código do voluntário (quando faz login)
    private String codVol;
    
    // construtor vazio
    public MenuGestao(){}
    
    // menu inicial
    public void menuInicial(GestaoTotal gt){
        String s;
        System.out.println("--------------------------------------");
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Secção de Gestão de Lojas");
        System.out.println("\n[2] Secção de Gestão de Empresas Transportadoras");
        System.out.println("\n[3] Secção de Gestão de Voluntários");
        System.out.println("\n[4] Secção de Gestão de Encomendas");
        System.out.println("\n[5] Voltar ao menu Inicial");
        s = in.nextLine();
        
        switch(s){
            case "0": System.exit(0);
            case "1": zoneLojas(gt);break;
            case "2": zoneEmpresas(gt);break;
            case "3": zoneVoluntarios(gt);break;
            case "4": zoneEncomendas(gt);break;
            case "5": super.menuInicial(gt); break;
            default: System.out.printf("\nInput inválido. Tente novamente.\n"); break;
        }

        this.menuInicial(gt);
    }

    // secção de lojas
    private void zoneLojas(GestaoTotal gt) {
        String s;
        System.out.println("--------------------------------------");
        System.out.println("\nBem-vindo à Gestão de Lojas: ");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Retornar ao menu anterior");
        System.out.println("\n[2] Registar uma loja");
        System.out.println("\n[3] Login");
        
        s = in.nextLine();
        
        switch(s){ 
            case "0": 
                    System.exit(0); 
            case "1": 
                    System.out.println("Reencaminhando para a secção anterior"); this.menuInicial(gt); break;
            case "2":
                    registoLojas(gt); zoneLojas(gt);break;
            case "3": 
                    int i = loginLojas(gt); 
                    // switch para o login
                    switch(i){
                        case 0: System.out.println("Inválido."); zoneLojas(gt); break;
                        case 1: acoesLojas(gt); break;
                        default: System.out.println("!!!! Input inválido. Tente novamente.\n"); break;
                    }
                    break;
                    //
            default:
                    System.out.println("Opção errada"); zoneLojas(gt); break;
        }
    }
    
    private void acoesLojas(GestaoTotal gt) {
        String s;
        System.out.println("-------------Bem Vindo----------------");
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Terminar Sessão");
        System.out.println("\n[2] Info da loja");
        System.out.println("\n[3] Histórico de Encomendas");
        System.out.println("\n[4] Faturação total");
        s = in.nextLine();
        switch(s){
            case "0": System.exit(0);
            case "1": zoneLojas(gt); 
                    // limpar a string do codigo da loja
                    codLoja = null;
                    break;
            case "2": 
                    System.out.println (gt.getGL().getLojas().get(codLoja));
                    acoesLojas(gt);
                    break;
            case "3": 
                    System.out.println (gt.getGE().printHistoricoLojas(gt.getGE().historicoLojas(codLoja), codLoja));
                    acoesLojas(gt);
                    break;
            case "4":
                    System.out.println("--------------------------------------");
                    System.out.println("O Total faturado pela loja " + codLoja + " é " + gt.getGE().faturacaoLojas(codLoja));
                    acoesLojas(gt); 
                    break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); acoesLojas(gt); break;
        }
    }

    public int loginLojas(GestaoTotal gt) {
        int i[] = {0};
        // String email
        String e;
        System.out.println("\n >Insira aqui o seu email: ");
        e = in.nextLine();

        // string password
        String p;
        System.out.println("\n >Insira aqui a sua password :");
        p = in.nextLine();

        gt.getGL().getLojas().values().stream()
            .forEach(l -> {
                            if ((l.getUser()).equals(e) && l.getPass().equals(p)) {
                                    i[0] = 1; 
                                    codLoja = l.getCodLoja();
                                }
                            }
              );
        return i[0];
    }
    
    /**
         * Registo para Lojas
     */
    public void registoLojas(GestaoTotal gt) {
        // nome da empresa
        String nomeLoja;
        System.out.println(" Insira aqui o nome da Loja: ");
        nomeLoja = in.nextLine();
        while (gt.existeNomeLoja(nomeLoja)){
            System.out.println("\n !!!! O nome da loja que inseriu já existe, coloque outro:");
            nomeLoja = in.nextLine();
            while(nomeLoja.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            nomeLoja = in.nextLine();
            }
        }
        
        // gerar um código voluntario 
        String codLoja;
        System.out.println("\n >O código da sua loja é :");
        Random g = new Random();
        codLoja = "l"+g.nextInt(200);
        while (gt.existeCodigoLoja(codLoja)){
            codLoja = "v"+g.nextInt(200);
        }
        System.out.println(codLoja);

        // user para login
        String user;
        System.out.println("\n >Insira um email para login :");
        user = in.nextLine();
        while (gt.existeEmailLoja(user)){
            System.out.println("\n !!!! O email que inseriu já existe, coloque outro:");
            user = in.nextLine();
            while(user.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            user = in.nextLine();
            }
        }

        // password para login
        String pass;
        System.out.println("\n >Insira uma password para login:");
        pass = in.nextLine();
        while(pass.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            pass = in.nextLine();
        }

        // localização da loja
        String gpsx;
        String gpsy;
        System.out.println("\n Insira aqui a coordenada x (latitude) da loja: ");
        gpsx = in.nextLine();
        while(gpsx.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            gpsx = in.nextLine();
        }
        //
        System.out.println(" Insira aqui a coordenada y (longitude) da loja: ");
        gpsy = in.nextLine();
        while(gpsy.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            gpsy = in.nextLine();
        }
        do{
            try{
                double latitude = Double.parseDouble(gpsx);
                double longitude = Double.parseDouble(gpsy);

                // cria uma localização se tudo estiver ok
                Localizacao local = new Localizacao(latitude, longitude);
          
                // criar nova loja e inserir na estrutura de dados
                gt.adicionaLoja(new Loja(local, codLoja, nomeLoja, user, pass));
                System.out.println("\n  O seu registo foi efetuado.\n");
            }
            catch(NumberFormatException e){
                System.out.println("!!!! Input inválido. Tente novamente."); registoLojas(gt);
                break;
            }
        }
        while(false);
    }
    
    // secção de empresas
    private void zoneEmpresas(GestaoTotal gt) {
        String s;
        System.out.println("--------------------------------------");
        System.out.println("\nBem-vindo à Gestão de Empresas: ");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Retornar ao menu anterior");
        System.out.println("\n[2] Registar uma Empresa Transportadora");
        System.out.println("\n[3] Login");

        s = in.nextLine();

        switch(s){
            case "0": System.exit(0);
            
            case "1": System.out.println("Reencaminhando para a secção anterior"); this.menuInicial(gt); break;
            
            case "2": 
                registoEmpresa(gt); zoneEmpresas(gt); break;
                  
            case "3": int i = loginEmp(gt); 
                    // switch para o login
                    switch(i){
                        case 0: System.out.println("Inválido."); break;
                        case 1: acoesEmpresas(gt); break;
                        default: System.out.println("!!!! Input inválido. Tente novamente.\n"); break;
                    }
                    //
                    break;
            default: 
                System.out.println("Opção errada"); zoneEmpresas(gt); break;
        }
    }
    
    private void acoesEmpresas(GestaoTotal gt) {
        String s;
        System.out.println("-------------Bem Vindo----------------");
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Terminar Sessão");
        System.out.println("\n[2] Info da Empresa");
        System.out.println("\n[3] Histórico de Encomendas");
        System.out.println("\n[4] Definir uma encomenda como entregue");
        System.out.println("\n[5] Total de faturação");
        System.out.println("\n[6] Verificar a classificação");

        s = in.nextLine();
        switch(s){
            case "0": System.exit(0);
            case "1": zoneEmpresas(gt); 
                    // limpar a string do codigo da loja
                    codEmp = null;
                    break;
            case "2": System.out.println(gt.getGEMP().getEmpresasTransp().get(codEmp));acoesEmpresas(gt);break;
            case "3": System.out.println (gt.getGE().historico(codEmp));acoesEmpresas(gt); break;
            case "4": mudarEstadoEncomendaEmpresa(gt); acoesEmpresas(gt); break;
            case "5": 
                    System.out.println("--------------------------------------");
                    System.out.println("O Total faturado pelos custos de transporte da empresa " + codEmp+ " é " + gt.faturacaoVOLeEMP(codEmp));
                    acoesEmpresas(gt); 
                    break;
            case "6": 
                    System.out.println("--------------------------------------");
                    System.out.println("A classificação da empresa "+codEmp+" é " +gt.getGEMP().getEmpresasTransp().get(codEmp).getClassificacao());
                    acoesEmpresas(gt);
                    break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); acoesEmpresas(gt); break;
        }
    }
    
    private void mudarEstadoEncomendaEmpresa(GestaoTotal gt) {
        String s;
        System.out.println("-----------------------------");
        System.out.printf("Encomendas pendentes: "); gt.printEncomendasPendentes(gt,codEmp); 
        System.out.println("[0] Voltar atrás");
        System.out.println("[1] Escolher a encomenda");
        s = in.nextLine();
        switch(s){
            case "0": acoesEmpresas(gt); break;
            case "1": 
                System.out.println("Introduza o número da encomenda");
                String nr;
                nr = in.nextLine();
                while(nr.equals("")){
                    System.out.println("!!!! Input inválido. Tente novamente.\n");
                    nr = in.nextLine();
                } 
                gt.alteraEstadoEncomenda(nr, codEmp);
                gt.getGEMP().getEmpresasTransp().get(codEmp).adicionarKMS(gt.custoTransporteEmp(gt.getGE().getEncomenda(nr).getCodU(), codEmp));
                break;
            default: System.out.println("\n !!!! Input inválido. Tente novamente.\n");
        }
    }

    public int loginEmp(GestaoTotal gt) {
        int i[] = {0};
        // String email
        String e;
        System.out.println("\n >Insira aqui o seu email: ");
        e = in.nextLine();

        // string password
        String p;
        System.out.println("\n >Insira aqui a sua password :");
        p = in.nextLine();

        gt.getGEMP().getEmpresasTransp().values().stream()
            .forEach(em -> {
                            if ((em.getUser()).equals(e) && em.getPass().equals(p)) {
                                    i[0] = 1; 
                                    codEmp = em.getCodEmp();
                                }
                            }
              );
        return i[0];
    }

    /**
        * Registo para Empresas
    */
    public void registoEmpresa(GestaoTotal gt) {
        //nome da empresa
        String nome;
        System.out.println(" Insira aqui o nome da Empresa: ");
        nome = in.nextLine();
        while(nome.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            nome = in.nextLine();
            }
        while (gt.existeNomeEmpresa(nome)){
            System.out.println("\n !!!! O nome da Empresa que inseriu já existe, coloque outro:");
            nome = in.nextLine();
            while(nome.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            nome = in.nextLine();
            }
        }
        
        //codigo da empresa
        String codEmp;
        System.out.println("\n >O código da sua Empresa Transportadora é :");
        Random g = new Random();
        codEmp = "t"+g.nextInt(100);
        while (gt.existeEmpresa(codEmp)){
            codEmp = "t"+g.nextInt(100);
        }
        System.out.println(codEmp);

        // user para login
        String user;
        System.out.println("\n >Insira um email para login :");
        user = in.nextLine();
        while(user.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            user = in.nextLine();
            }
        while (gt.existeEmailEmpresa(user)){
            System.out.println("\n !!!! O email que inseriu já existe, coloque outro:");
            user = in.nextLine();
            while(user.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            user = in.nextLine();
            }
        }

        // password para login
        String pass;
        System.out.println("\n >Insira uma password para login:");
        pass = in.nextLine();
        while(pass.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            pass = in.nextLine();
        }
        

        //nif da empresa
        String nif;
        System.out.println(" Insira aqui o nif da Empresa :");
        nif = in.nextLine();
        while(nif.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            nif = in.nextLine();
        }

        // tempo  de entrega
        double tempo = 0;
        
        //Classificação no momento do registo
        double classificacao = 0;

        //custo transporte
        double custo = 0;

        //número de viagens efetuadas no momento do registo
        int viagens = 0;
        
        //número de Kms efetuadas no momento do registo
        int kms = 0;
        
        //taxa de transporte
        String t;
        double taxa;
        System.out.println(" Insira aqui a Taxa Fixa (preço/km) ");
        t = in.nextLine();
        while(t.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            t = in.nextLine();
        }
        
        //capacidade
        String c;
        int capacidade;
        System.out.println(" Quantas encomendas consegue transportar?  ");
        c = in.nextLine();
        while(c.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            c = in.nextLine();
        }
        
        
        //Velocidade  no momento do registo
        String v;
        double velocidade;
        System.out.println(" Média de velocidade de entrega ");
        v = in.nextLine();
        while(v.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            v = in.nextLine();
        }
        
        //Raio de distância
        String r;
        double raio;
        System.out.println(" Raio de entrega da empresa ");
        r = in.nextLine();
        while(r.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            r = in.nextLine();
        }
        
        // certificado estado da empresa
        String cer;
        boolean certificado;
        System.out.println(" Insira aqui o se está disponível para entregar encomendas médicas.");
        System.out.println("\n[true] Disponível");
        System.out.println("\n[false] Indiponível");
        cer = in.nextLine();
        while(cer.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            cer = in.nextLine();
        }
       
        //localização da empresa
        String x,y;
        double gpsx,gpsy;
        System.out.println(" Insira aqui a coordenada x (latitude) da empresa :");
        x = in.nextLine();
        while(x.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            x = in.nextLine();
        }
        System.out.println(" Insira aqui a coordenada y (longitude) da empresa :");
        y = in.nextLine();
        while(y.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            y = in.nextLine();
        }
        
        //estado da empresa
        String b;
        boolean estado;
        System.out.println(" Insira aqui o se está disponível para entregar encomendas.");
        System.out.println("\n[true] Disponível");
        System.out.println("\n[false] Indiponível");
        b = in.nextLine();
        while(b.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            b = in.nextLine();
        }
        do{
            try{
                estado = Boolean.parseBoolean(b);
                taxa = Double.parseDouble(t);
                capacidade = Integer.parseInt(c);
                velocidade = Double.parseDouble(v);
                raio = Double.parseDouble(r);
                certificado = Boolean.parseBoolean(cer);
                gpsx = Double.parseDouble(x);
                gpsy = Double.parseDouble(y);
                Localizacao local = new Localizacao(gpsx, gpsy);
                gt.adicionaEmpresa(new EmpresaTransportadora(nome,local,estado,custo,taxa,capacidade,tempo,classificacao,velocidade,raio,codEmp,nif,certificado,viagens,kms,user,pass));
                System.out.printf("\n O seu registo foi efetuado.\n");
            }
            catch(Exception e){
                System.out.println("Input inválido. Tente novamente");
                registoEmpresa(gt);
                break;
            }
        }
        while(false);
    }
      
    // secção de voluntários
    private void zoneVoluntarios(GestaoTotal gt) {
        String s;
        System.out.println("--------------------------------------");
        System.out.println("\nBem-vindo à Gestão de Empresas: ");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Retornar ao menu anterior");
        System.out.println("\n[2] Registar um Voluntário");
        System.out.println("\n[3] Login");

        s = in.nextLine();

        switch(s){
            case "0": System.exit(0);
            
            case "1": System.out.println("Reencaminhando para a secção anterior"); this.menuInicial(gt); break;
            
            case "2": 
                registoVoluntario(gt); zoneVoluntarios(gt); break;
                  
            case "3": int i = loginVol(gt); 
                    // switch para o login
                    switch(i){
                        case 0: System.out.println("Inválido."); break;
                        case 1: acoesVoluntarios(gt); break;
                        default: System.out.println("!!!! Input inválido. Tente novamente.\n"); break;
                    }
                    //
                    break;
            default: 
                System.out.println("Opção errada"); zoneVoluntarios(gt); break;
        }
    }


    private void acoesVoluntarios(GestaoTotal gt) {
        String s;
        System.out.println("-------------Bem Vindo----------------");
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Terminar Sessão");
        System.out.println("\n[2] Info do Voluntário");
        System.out.println("\n[3] Histórico de Encomendas");
        System.out.println("\n[4] Mudar estado de disponiblidade para entregar encomendas");
        System.out.println("\n[5] Definir uma encomenda como entregue");
        System.out.println("\n[6] Mudar o estado de certificação de encomendas médicas");
        System.out.println("\n[7] Verificar a classificação do voluntário");

        s = in.nextLine();
        switch(s){
            case "0": System.exit(0);
            case "1": zoneVoluntarios(gt); 
                    // limpar a string do codigo do voluntário
                    codVol = null;
                    break;
            case "2": System.out.println(gt.getGV().getVoluntarios().get(codVol)); acoesVoluntarios(gt); break;
            case "3": System.out.println(gt.getGE().historico(codVol)); acoesVoluntarios(gt); break;
            case "4": mudarEstadoVoluntario(gt); acoesVoluntarios(gt); break;
            case "5": mudarEstadoEncomenda(gt); acoesVoluntarios(gt); break;
            case "6": mudarEstadoCertificado(gt); acoesVoluntarios(gt); break;
            case "7": 
                    System.out.println("--------------------------------------");
                    System.out.println("A classificação do voluntário "+codVol+" é " +gt.getGV().getVoluntarios().get(codVol).getClassificacao());
                    acoesVoluntarios(gt);
                    break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); acoesVoluntarios(gt); break;
        }
    }
     
    private void mudarEstadoCertificado(GestaoTotal gt) {
        String s;
        System.out.println("\n >Escolha uma opção: ");
        System.out.println("\n >[1] Mudar o estado para DISPONÍVEL para entregar encomendas medicas");
        System.out.println("\n >[2] Mudar o estado para INDISPONÍVEL para entregar encomendas medicas");
        s = in.nextLine();
        
        switch(s){
            case "1": gt.getGV().getVoluntarios().get(codVol).setCertificado(true);
                      System.out.println("\n --Disponível para entregar encomendas medicas!--.\n");
                      acoesVoluntarios(gt);
                      break;
            case "2": gt.getGV().getVoluntarios().get(codVol).setCertificado(false);
                      System.out.println("\n --Indisponível para entregar encomendas medicas!--.\n");
                      acoesVoluntarios(gt); 
                      break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); mudarEstadoCertificado(gt); break;
        }
    }

    private void mudarEstadoEncomenda(GestaoTotal gt) {
        String s;
        System.out.println("-----------------------------");
        System.out.printf("Encomendas pendentes: "); gt.printEncomendasPendentes(gt,codVol); 
        System.out.println("[0] Voltar atrás");
        System.out.println("[1] Escolher a encomenda");
        s = in.nextLine();
        switch(s){
            case "0": acoesVoluntarios(gt); break;
            case "1": 
                System.out.println("Introduza o número da encomenda");
                String nr;
                nr = in.nextLine();
                while(nr.equals("")){
                    System.out.println("!!!! Input inválido. Tente novamente.\n");
                    nr = in.nextLine();
                } 
                gt.alteraEstadoEncomenda(nr, codVol);
                break;
            default: System.out.println("\n !!!! Input inválido. Tente novamente.\n");
        }
    }

    public void mudarEstadoVoluntario(GestaoTotal gt) {
        String s;
        System.out.println("\n >Escolha uma opção: ");
        System.out.println("\n >[1] Mudar o estado para DISPONÍVEL para entregar encomendas");
        System.out.println("\n >[2] Mudar o estado para INDISPONÍVEL para entregar encomendas");
        s = in.nextLine();
        
        switch(s){
            case "1": gt.getGV().getVoluntarios().get(codVol).setEstado(true);
                      System.out.println("\n --Disponível para entregar entregas!--.\n");
                      acoesVoluntarios(gt);
                      break;
            case "2": gt.getGV().getVoluntarios().get(codVol).setEstado(false);
                      System.out.println("\n --Indisponível para entregar entregas!--.\n");
                      acoesVoluntarios(gt); 
                      break;
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); mudarEstadoVoluntario(gt); break;
        }
    }
    
      public int loginVol(GestaoTotal gt) {
        int i[] = {0};
        // String email
        String e;
        System.out.println("\n >Insira aqui o seu email: ");
        e = in.nextLine();

        // string password
        String p;
        System.out.println("\n >Insira aqui a sua password :");
        p = in.nextLine();

        gt.getGV().getVoluntarios().values().stream()
            .forEach(v -> {
                            if ((v.getUser()).equals(e) && v.getPass().equals(p)) {
                                    i[0] = 1; 
                                    codVol = v.getCodV();
                                }
                            }
              );
        return i[0];
    }
      
      /**
       * Registo para Voluntário
       */
    public void registoVoluntario(GestaoTotal gt) {
        //nome do Voluntário
        String nome;
        System.out.println(" Insira aqui o nome do Voluntário: ");
        nome = in.nextLine();
        while(nome.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            nome = in.nextLine();
            }
        while (gt.existeNomeVoluntario(nome)){
            System.out.println("\n !!!! O nome do voluntário que inseriu já existe, coloque outro:");
            nome = in.nextLine();
            while(nome.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            nome = in.nextLine();
            }
        }
        
        // gerar um código voluntario 
        String codV;
        System.out.println("\n >O código do seu voluntário é :");
        Random g = new Random();
        codV = "v"+g.nextInt(100);
        while (gt.existeVoluntario(codV)){
            codV = "v"+g.nextInt(100);
        }
        System.out.println(codV);

        // user para login
        String user;
        System.out.println("\n >Insira um email para login :");
        user = in.nextLine();
        while(user.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            user = in.nextLine();
            }
        while (gt.existeEmailVoluntario(user)){
            System.out.println("\n !!!! O email que inseriu já existe, coloque outro:");
            user = in.nextLine();
            while(user.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            user = in.nextLine();
            }
        }

        // password para login
        String pass;
        System.out.println("\n >Insira uma password para login:");
        pass = in.nextLine();
        while(pass.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            pass = in.nextLine();
        }

        //Classificação no momento do registo
        double classificacao = 0;
        
        //número de viagens efetuadas no momento do registo
        int viagens = 0;
  
        //estado do voluntário
        String e;
        boolean estado;
        System.out.println("\n Insira aqui o se está disponível para entregar encomendas.");
        System.out.println("\n[true] Disponível");
        System.out.println("\n[false] Indiponível");
        e = in.nextLine();
        while(e.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            e = in.nextLine();
        }
        
        //Velocidade  no momento do registo
        String v;
        double velocidade;
        System.out.println(" Média de velocidade de entrega ");
        v = in.nextLine();
        while(v.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            v = in.nextLine();
        }
        
        //Raio de distância
        String r;
        double raio;
        System.out.println(" Raio de entrega do voluntário ");
        r = in.nextLine();
        while(r.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            r = in.nextLine();
        }
        
        //certificado do voluntario
        String cer;
        boolean certificado;
        System.out.println(" Insira aqui o se está disponível para entregar encomendas médicas.");
        System.out.println("\n[true] Disponível");
        System.out.println("\n[false] Indiponível");
        cer = in.nextLine();
        while(cer.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            cer = in.nextLine();
        }
        
        //localização do voluntário
        String x,y;
        double gpsx,gpsy;
        System.out.println(" Insira aqui a coordenada X (Latitude) do voluntário :");
        x = in.nextLine();
        while(x.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            x = in.nextLine();
        }
        System.out.println(" Insira aqui a coordenada Y (Longitude) do voluntário :");
        y = in.nextLine();
        while(y.equals("")){
            System.out.println("!!!! Input inválido. Tente novamente.");
            y = in.nextLine();
        }
        
        do{
            try{
                estado = Boolean.parseBoolean(e);
                velocidade = Double.parseDouble(v);
                raio = Double.parseDouble(r);
                certificado = Boolean.parseBoolean(cer);
                gpsx = Double.parseDouble(x);
                gpsy = Double.parseDouble(y);
                Localizacao local = new Localizacao(gpsx, gpsy);
                
                gt.adicionaVoluntario(new Voluntario(nome,estado,velocidade,local,raio,codV,certificado,viagens,classificacao,user,pass));
                System.out.printf("\n O seu registo foi efetuado.\n");
            }
            catch(Exception ee){
                System.out.println("!!!! Input inválido. Tente novamente.");
                registoVoluntario(gt);
                break;
            }
        }
        while(false);
    }
      
    // secção de encomendas
    private void zoneEncomendas(GestaoTotal gt) {
        String s;
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Voltar atrás");
        System.out.println("\n[2] Login");
        s = in.nextLine();
        switch(s){
            case "0": System.exit(0);
            case "1": System.out.println("Reencaminhando para a secção anterior"); this.menuInicial(gt); break;
            case "2": 
                    String a;
                    String b;
                    System.out.println("Insira o user");
                    a = in.nextLine();
                    while(a.equals("")){
                        System.out.println("!!!! Input inválido. Tente novamente.");
                        a = in.nextLine();
                    }
                    System.out.println("\nInsira a password");
                    b = in.nextLine();
                    while(b.equals("")){
                        System.out.println("!!!! Input inválido. Tente novamente.");
                        b = in.nextLine();
                    }
                    // credenciais são admin/admin
                    if(a.equals("admin") && b.equals("admin")){
                        menuEncomendasTotais(gt);
                    }
                    else{
                        System.out.println("Credenciais erradas!! Tente novamente!! (pista: admin) 🐻");
                        zoneEncomendas(gt);
                    }
                    break;
            default:  System.out.println("Opção errada"); zoneEncomendas(gt); break;
        }
    }

	private void menuEncomendasTotais(GestaoTotal gt) {
        // total medicas e total normais
        String s;
        System.out.println("\nEscolha uma das seguintes opções:");
        System.out.println("\n[0] Sair da aplicação.");
        System.out.println("\n[1] Voltar atrás");
        System.out.println("\n[2] Número total de encomendas");
        s = in.nextLine();
        switch(s){
            case "0": System.exit(0);
            case "1": System.out.println("Reencaminhando para a secção anterior"); this.zoneEncomendas(gt); break;
            case "2": 
                    gt.getGE().totalEncomendas();
                    menuEncomendasTotais(gt);
                    break;
            default:  System.out.println("Opção errada"); menuEncomendasTotais(gt); break;
        }
	}
}