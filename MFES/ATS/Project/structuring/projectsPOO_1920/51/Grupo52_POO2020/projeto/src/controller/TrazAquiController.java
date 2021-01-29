package controller;

import exceptions.EmailJaExisteException;
import model.*;
import view.Menu;
import view.MenuLogin;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class TrazAquiController {
    /* camada lógica */
    private TrazAquiModel model;

    /* Menus da aplicaçãoo - View */
    private Menu mainMenu, signUpMenu, menuAdmin, menuUtilizador, menuVoluntario, menuLoja, menuEmpresa, auxiliar;
    private MenuLogin menuLogin;

    /* condições climatéricas */
    private Randomizer estado = new Randomizer();


    /**
     * Construtor prametrizado da classe Controller
     * @param model Modelo que guarda todos os dados necessários ao funcionamento do programa
     */
    public TrazAquiController(TrazAquiModel model){
        this.model = model;

        /* criação dos menus */
        String[] mainMenu = {"Sign-in",
                "Sign-up",
                "Top 10 utilizadores",
                "Top 10 empresas"};
        String[] signup = {"Novo Utilizador",
                "Novo Voluntário",
                "Nova Loja",
                "Nova Empresa"};
        String[] signin = {"Administrador",
                "Utilizador",
                "Voluntário",
                "Loja",
                "Empresa"};
        String[] admin = {"Carregar dados atráves de ficheiro",
                "Gravar Estado"};
        String[] utilizador = {"Fazer Encomenda",
                "Ver encomendas disponíveis para entrega",
                "Classificar encomendas recebidas",
                "Consultar as minhas encomendas",
                "Ver minha informação pessoal"};
        String[] voluntario = {"Sinalizar disponibilidade",
                "Registar tempo de entrega de uma encomenda",
                "Licença para entregar encomendas médicas",
                "Consultar encomendas que transportei",
                "Ver minha informação pessoal"};
        String[] loja = {"Registar encomenda pronta para entrega",
                "Consultar histórico de encomendas",
                "Ver minha informação pessoal"};
        String[] empresa = {"Sinalizar disponiblidade",
                "Registar tempo de entrega de uma encomenda e custo associado",
                "Licença para entregar encomendas médicas",
                "Consultar encomendas transportadas",
                "Total Faturado num determinado período",
                "Ver minha informação pessoal"};
        String[] aux = {};
        this.mainMenu = new Menu(mainMenu);
        this.signUpMenu = new Menu(signup);
        this.menuAdmin = new Menu(admin);
        this.menuUtilizador = new Menu(utilizador);
        this.menuVoluntario = new Menu(voluntario);
        this.menuLoja = new Menu(loja);
        this.menuEmpresa = new Menu(empresa);
        this.auxiliar = new Menu(aux);
        this.menuLogin = new MenuLogin(signin);

        //carregar obj
        try {
            this.model = TrazAquiModel.carregaEstado();
        } catch(FileNotFoundException e) {
            System.out.println("Parece que é a primeira utilização... A carregar ficheiro 'logs.txt' ...");
        try {
                carregaFicheiro(null);
            } catch(IOException | EmailJaExisteException ex) {
                System.out.println("Erro a carregar ficheiro 'logs.txt'. ");
            }
        } catch(IOException e) {
            System.out.println("Ops! Erro de leitura!");
        } catch(ClassNotFoundException e) {
            System.out.println("Ops! Formato de ficheiro de dados errado!");
        }

    }

    /**
     * Executa o menu principal e invoca o método correspondente à opção seleccionada.
     */
    public void run(){
        System.out.println("----------------------TrazAquiApp------------------------");
        System.out.println("Clima de hoje: " + this.estado.getClima());

        do {
            /* executing main menu while user don't want to leave */
            this.mainMenu.executa();
            switch(this.mainMenu.getOp()) {
                case 1:
                    clearScreen();
                    do{
                        /* entidade to login */
                        this.menuLogin.executaReader();
                        if (this.menuLogin.getOp() == 0) {
                            break;
                        }
                        do{/* do login */
                            this.menuLogin.executaParametros();
                        }while(! this.model.checkLogin(this.menuLogin.getOp(), this.menuLogin.getEmail(), this.menuLogin.getPassword()) && this.menuLogin.getOp()!=0);
                        /* check what entidade logged */
                        switch(menuLogin.getOp()){
                            /* Admin is logged */
                            case 1:
                                do{
                                    this.menuAdmin.executa();
                                    switch(this.menuAdmin.getOp()){
                                        case 1: /* admin wants to load file */
                                            admin_load_file();
                                            break;
                                        case 2: /* admin wants to save obj */
                                            try {
                                                this.model.guardaEstado();
                                            } catch(IOException e) {
                                                this.menuAdmin.sendMessage("Erro a guardar estado");
                                            }
                                            break;

                                    }
                                }while(this.menuAdmin.getOp()!=0);
                                break;
                            /* utilizador is logged */
                            case 2:
                                do{
                                    String uEmail = this.menuLogin.getEmail();
                                    this.menuUtilizador.executa();
                                    switch(this.menuUtilizador.getOp()){
                                        case 1: /* Fazer Encomenda */
                                            fazerEncomenda(uEmail);
                                            break;
                                        case 2: /* Ver as minhas encomendas à espera de voluntário*/
                                            encomendasStandby(uEmail);
                                            break;
                                        case 3: /* Classificar encomendas recebidas */
                                            classificarEncomenda(uEmail);
                                            break;
                                        case 4: /* As minhas encomendas */
                                            minhasEncomendas(uEmail, this.menuLogin.getOp());
                                            break;
                                        case 5: /*Ver a minha informação pessoal*/
                                            this.menuUtilizador.sendMessage(this.model.getUtilizador(uEmail).toString());
                                            break;
                                    }
                                }while(this.menuUtilizador.getOp()!=0);
                                break;
                            /* voluntario is logged */
                            case 3:
                                do{
                                    String vEmail = this.menuLogin.getEmail();
                                    this.menuVoluntario.executa();
                                    switch(this.menuVoluntario.getOp()){
                                        case 1: /* Sinalizar disponibilidade */
                                            /*Sempre que entra aqui,adiciono uma encomenda em standby dentro do seu raio
                                             */
                                            disponibilidade(this.menuLogin.getOp(), vEmail);
                                            break;
                                        case 2: /* Registar tempo de entrega de uma encomenda */
                                            /*Sempre que entra aqui, mudo a flag para entregue e adiciono nas encomendas para classificar do utilizador
                                             */
                                            tempo_entrega(this.menuLogin.getOp(), vEmail);
                                            break;
                                        case 3: /*Licença para entregar encomendas médicas */
                                            licenca(this.menuLogin.getOp(), vEmail);
                                            break;
                                        case 4: /*Consultar encomendas que transportei*/
                                            minhasEncomendas(vEmail, this.menuLogin.getOp());
                                            break;
                                        case 5: /*Ver a minha informação pessoal*/
                                            this.menuVoluntario.sendMessage(this.model.getVoluntario(vEmail).toString());
                                            break;

                                    }
                                }while(this.menuVoluntario.getOp()!=0);
                                break;
                            /* loja is logged */
                            case 4:
                                do{
                                    String lEmail=this.menuLogin.getEmail();
                                    this.menuLoja.executa();
                                    switch(this.menuLoja.getOp()){
                                        case 1: /* Registar encomenda pronta para entrega */
                                            readyToentrega(lEmail);
                                            break;
                                        case 2: /*Consultar histórico de encomendas */
                                            minhasEncomendas(lEmail, this.menuLogin.getOp());
                                            break;
                                        case 3: /*Ver minha informação pessoal*/
                                            this.menuLoja.sendMessage(this.model.getLoja(lEmail).toString());
                                            break;

                                    }
                                }while(this.menuLoja.getOp()!=0);
                                break;
                            /* empresa is logged */
                            case 5:
                                do{
                                    String eEmail = this.menuLogin.getEmail();
                                    this.menuEmpresa.executa();
                                    switch(this.menuEmpresa.getOp()) {
                                        case 1: /* Sinalizar disponibilidade */
                                            disponibilidade(this.menuLogin.getOp(), eEmail);
                                            break;
                                        case 2: /* Registar tempo de entrega de uma encomenda e custo */
                                            tempo_entrega(this.menuLogin.getOp(), eEmail);
                                            break;
                                        case 3: /*Licença para entregar encomendas médicas */
                                            licenca(this.menuLogin.getOp(), eEmail);
                                            break;
                                        case 4: /*Consultar encomendas que transportei*/
                                            minhasEncomendas(eEmail, this.menuLogin.getOp());
                                            break;
                                        case 5: /* Total Faturado num determinado período */
                                            totalFaturado(eEmail);
                                            break;
                                        case 6: /*Ver a minha informação pessoal*/
                                            this.menuEmpresa.sendMessage(this.model.getEmpresa(eEmail).toString());
                                            break;
                                    }
                                }while(this.menuEmpresa.getOp()!=0);
                                break;
                            default:
                                break;
                        }
                    } while(this.menuLogin.getOp()!=0);
                    break;
                case 2:
                    /* sign up*/
                    clearScreen();
                    this.signUpMenu.executa();
                    if(this.signUpMenu.getOp() == 0) break;
                    clearScreen();
                    try {
                        registo(this.signUpMenu.getOp());
                    } catch(EmailJaExisteException e) {
                        this.signUpMenu.sendMessage("O Email que introduziu já existe");
                    }

                    break;
                case 3:
                    /* top 10 users em numero de encomendas*/
                    Set<Utilizador> res = this.model.top10Users();
                    this.mainMenu.sendMessage("Top 10 utilizadores por número de encomendas efetuadas ");
                    res.forEach(user -> this.mainMenu.sendMessage(user.getNome() + " --> " ,  user.nrEncomendas(), ""));
                    break;
                case 4:
                    /* top 10 transportadoras por kms percorridos*/
                    Set<Empresa> top10 = this.model.top10Empresas();
                    this.mainMenu.sendMessage("Top 10 empresas transportadoras por kms percorridos");
                    top10.forEach(user -> this.mainMenu.sendMessage(user.getNome() + " --> ", user.totalKms(), "kms"));
                case 0:
                    mainMenu.sendMessage("\n                                   ＧＯＯＤＢＹＥ\n");
                    break;
                default:
                    mainMenu.sendMessage("Opção inválida.");
                    break;
            }
        } while(this.mainMenu.getOp() != 0);
        clearScreen();
        try {
            this.model.guardaEstado();
        } catch(IOException e) {
            System.out.println("Erro ao salvar dados.");
        }
        System.out.println("Saindo do programa...");

    }


    private void clearScreen() {
        System.out.print('\u000C');
    }

    public void carregaFicheiro(String filename) throws IOException, EmailJaExisteException {
        Parser parser = new Parser(this.model);
        parser.loadData(filename);
    }

    public void admin_load_file(){
        menuAdmin.sendMessage("Indique o nome do ficheiro a ler: ");
        try {
            carregaFicheiro(menuAdmin.leString());
        } catch(IOException | EmailJaExisteException e) {
            System.out.println("Erro na leitura do ficheiro...");
        }
    }

    /** TIPOS
     *  1 Utilizador
     *  2 Voluntário
     *  3 Loja
     *  4 Empresa
     */
    private void registo(int tipo) throws EmailJaExisteException {
        this.signUpMenu.sendMessage("Email: ");
        String email = this.signUpMenu.leString();
        this.signUpMenu.sendMessage("Password:  ");
        String pwd = this.signUpMenu.leString();

        this.signUpMenu.sendMessage("Nome: ");
        String nome = this.signUpMenu.leString();
        String nif = null;
        Double pkm = null;
        if(tipo==4) {
            do {
                this.signUpMenu.sendMessage("NIF: ");
                nif = this.signUpMenu.leString();
                if (nif.length() != 9) this.signUpMenu.sendMessage("O NIF tem que ter 9 números!");
            } while (nif.length() != 9);
            this.signUpMenu.sendMessage("Preço por Km: ");
            pkm = this.signUpMenu.lerDouble();
        }
        Double raio = null;
        Boolean licenca= false;
        if(tipo==2 || tipo==4){
            this.signUpMenu.sendMessage("Tem licença para entregas de encomendas médicas? (y/n)");
            if (this.signUpMenu.leYesNo().equals("y")) licenca=true;
            this.signUpMenu.sendMessage("Raio de ação: ");
            raio = this.signUpMenu.lerDouble();
        }

        GPS gps = this.signUpMenu.lerLocalizacao();

        switch(tipo){
            case 1:
                int nu = this.model.getnUtilizadores()+1;
                this.model.addUtilizador(new Utilizador(email,pwd,nome,gps,nu));
                this.model.setnUtilizadores(nu);
                this.signUpMenu.sendMessage("\nUtilizador "+nome+" criado com sucesso!");
                break;
            case 2:
                int nv=this.model.getnVoluntarios()+1;
                Voluntario v = new Voluntario(email,pwd,nome,gps,raio,nv);
                v.aceitaMedicamentos(licenca);
                this.model.addVoluntario(v);
                this.model.setnVoluntarios(nv);
                this.signUpMenu.sendMessage("\nVoluntário "+nome+" criado com sucesso!");
                break;
            case 3:
                int nl=this.model.getnLojas()+1;
                this.model.addLoja( new Loja(email,pwd,nome,gps,nl) );
                this.model.setnLojas(nl);
                this.signUpMenu.sendMessage("\nLoja "+nome+" criada com sucesso!");
                break;
            case 4:
                int ne=this.model.getnEmpresas()+1;
                Empresa e = new Empresa(email,pwd,nome,gps,nif,raio,pkm,ne);
                e.setLicenca(licenca);
                this.model.addEmpresa(e);
                this.model.setnEmpresas(ne);
                this.signUpMenu.sendMessage("\nEmpresa "+nome+" criada com sucesso!");
                break;

        }
    }


    public List<Linha_Encomenda> makeEncomenda(){
        String input = this.menuUtilizador.leString();
        String[] campos = input.split(",");
        List<Linha_Encomenda> lle = new ArrayList<>();
        for(int i=0 ;i<(campos.length);i=i+4){
            Linha_Encomenda le = new Linha_Encomenda(campos[i], campos[i+1], Double.parseDouble(campos[i+2]), Double.parseDouble(campos[i+3]));
            lle.add(le);
        }

        return lle;
    }

    public void setEncomendaEntregue(Encomenda encomenda, Empresa empresa){
        encomenda.setCodEntidade_transportadora(empresa.getCodEmpresa());
        encomenda.setData(LocalDateTime.now());

        this.model.getUtilizadorC(encomenda.getCodUtilizador()).addEncomendaEntregue(encomenda);
        this.model.getLojaC(encomenda.getCodLoja()).addToAceites(encomenda);
        empresa.addToEncomendasPorSinalizar(encomenda);

    }

    public void setEncomendaEntregue(Encomenda encomenda, Voluntario voluntario){
        encomenda.setCodEntidade_transportadora(voluntario.getCodVoluntario());
        encomenda.setData(LocalDateTime.now());

        this.model.getUtilizadorC(encomenda.getCodUtilizador()).addEncomendaEntregue(encomenda);
        this.model.getLojaC(encomenda.getCodLoja()).addToAceites(encomenda);
        voluntario.addEncomendaPorSinalizar(encomenda);

    }

    public boolean getTransportadoras(Encomenda encomenda){
        Map<Empresa, Double> empresasDisponiveis = this.model.empresasDisponiveis(encomenda);
        if(empresasDisponiveis.size() == 0) return false;
        List<Empresa> empresasDS = empresasDisponiveis.entrySet().stream().map(Map.Entry::getKey).collect(Collectors.toList());
        List<String> res = new ArrayList<>();
        empresasDisponiveis.entrySet().stream().forEach(entry -> res.add(entry.getKey().getNome() + " --> " + String.format("%.2f", entry.getValue()) + "€"));

        Menu aux = new Menu(res);
        aux.sendMessage("Escolha a empresa pelo código, com o correspondente custo associado: \nCaso pretenda esperar por um voluntário disponível, escolha 0. ");
        do {
            aux.executa();
            if (aux.getOp() == 0) break;
            Empresa empresa = empresasDS.get(aux.getOp() -1);
            aux.sendMessage("\nConfirma a seleção da empresa? (y/n)");
            if (aux.leYesNo().equals("y")) {
                setEncomendaEntregue(encomenda, empresa);
                this.menuUtilizador.sendMessage("O tempo estimado da entrega é " , this.estado.tempoEntrega(empresa.getVkm(),encomenda.getDist_total()) , "minutos");
                return true;
            }
        }while(aux.getOp()!=0);
        return false;
    }

    public boolean checkTransporte(Encomenda encomenda){
        Voluntario v= this.model.temVoluntario(encomenda);
        if (v!=null) {
            this.menuUtilizador.sendMessage("\nTemos um voluntário para fazer a sua entrega: ");
            this.menuUtilizador.sendMessage(v.toString(0));
            this.menuUtilizador.sendMessage("O tempo estimado da entrega é ", this.estado.tempoEntrega(v.getVkm(), encomenda.getDist_total()), "minutos");
            setEncomendaEntregue(encomenda, v);
            return true;
        }
        else return getTransportadoras(encomenda);
    }



    private void fazerEncomenda(String email){
        Boolean medica=false;
        this.menuUtilizador.sendMessage("Trata-se de uma encomenda médica? (y/n) ");
        if (auxiliar.leYesNo().equals("y")) medica=true;

        this.menuUtilizador.sendMessage("Indique a loja onde quer efetuar a sua encomenda: ");
        Loja loja = null;
        while(loja == null){
            String res= menuUtilizador.leString();
            loja = this.model.getLojaNome(res);
            if(loja == null) this.menuUtilizador.sendMessage("A loja que introduziu não existe!");
        }

        espera_loja(loja);

        this.menuUtilizador.sendMessage("Faça a sua encomenda: ");
        List<Linha_Encomenda> lle = makeEncomenda();
        int nEncomenda = this.model.getnEncomendas() +1;
        this.model.setnEncomendas(nEncomenda);

        Encomenda e;
        if(medica) e = new EncomendaMedica("e"+nEncomenda, this.model.getMUtilizador().get(email).getCodUtilizador(),loja.getCodLoja(),0.0, lle);
        else e=new Encomenda("e"+nEncomenda, this.model.getMUtilizador().get(email).getCodUtilizador(),loja.getCodLoja(),0.0, lle);

        this.model.getLoja(loja.getEmail()).addToQueue(e);
        this.model.getUtilizador(email).addToStandby(e);
        this.menuUtilizador.sendMessage(e.toString());

        if(!checkTransporte(e)) this.menuUtilizador.sendMessage("A sua encomenda não será entregue para já... ");

    }

    /**
     * Indica o tempo de espera aproximado para uma encomenda ser entregue
     * Consideramos que demoram 5 minutos por pessoa, no máximo
     */
    private void espera_loja (Loja l){
        int queuesize = l.getQueue().size();
       int tempo_espera = (queuesize * 5);
       if (queuesize>5) {
           this.menuUtilizador.sendMessage("A sua encomenda pode demorar até " + tempo_espera + " minutos a sair da loja.");
       }else this.menuUtilizador.sendMessage("A sua encomenda vai demorar menos de " + tempo_espera + " minutos a sair da loja.");
    }

    private void encomendasStandby(String email){
        List<String> standBy =  this.model.getUtilizador(email).getEncomendas_Standy().stream().map(Encomenda::getCodEncomenda).collect(Collectors.toList());
        Menu aux = new Menu(standBy);
        do{
            aux.executa();
            int op = aux.getOp();
            if(op == 0) break;
            String codEncomenda = aux.getOpcion(op-1);
            Encomenda encomenda = this.model.getEncomendaC(codEncomenda);

            if(checkTransporte(encomenda)) break;
            else aux.sendMessage("A encomenda " + codEncomenda + " não será entregue para já...");

        }while(aux.getOp()!=0);

    }

    private void minhasEncomendas(String email, int op){
        for (Encomenda e: this.model.getAllEncomendas(email, op)){
            if(e.getData() == null) this.menuUtilizador.sendMessage("Esta encomenda ainda não foi entregue!\nEncomenda: " + e.toString());
            else this.menuUtilizador.sendMessage("Data/Hora " + e.getData() + "\nEncomenda: " + e.toString());
        }
    }

    private void disponibilidade(int op, String email){
        System.out.println("Está disponível para fazer entregas (y/n)");
        if (this.auxiliar.leYesNo().equals("y")) this.model.available(op, email, true);
        else this.model.available(op, email, false);
    }


    public void tempo_entrega(int op, String email){
        Menu aux = new Menu(this.model.encomendas_por_sinalizar(op, email));
        do{
            aux.executa();
            if(aux.getOp()== 0) break;

            int encomenda_index = aux.getOp();
            String codEnc_escolhida = aux.getOpcion(encomenda_index -1);
            Encomenda encomenda =this.model.getEncomendaC(codEnc_escolhida);

            aux.sendMessage("Indique o tempo que demorou a entregar a encomenda: ");
            double time = aux.lerDouble();
            encomenda.setTempo(time);
            if(op == 5){
                aux.sendMessage("Indique o custo associado: ");
                double price = aux.lerDouble();
                encomenda.setPreco(price);
                this.model.getEmpresa(email).sinalizarEncomenda(encomenda);
                break;
            }else{
                this.model.getVoluntario(email).sinalizarEncomenda(encomenda);
                encomenda.setCodEntidade_transportadora(this.model.getVoluntario(email).getCodVoluntario());
                break;
            }
        }while(aux.getOp()!= 0);
    }

    public void readyToentrega(String lEmail){
        Menu aux = new Menu(this.model.toEntrega(lEmail));
        aux.sendMessage("Escolha a encomenda: ");
            aux.executa();
            if(aux.getOp()!= 0) {
                int encomenda_index= aux.getOp();
                String codEnc_escolhida =this.model.toEntrega(lEmail).get(encomenda_index-1);
                Encomenda encomenda = this.model.getEncomendaC(codEnc_escolhida);
                Voluntario v = this.model.temVoluntario(encomenda);

                if(v != null){
                    setEncomendaEntregue(encomenda, v);
                    aux.sendMessage("A encomenda vai agora sair da loja...");

                }else  aux.sendMessage("De momento a encomenda não pode ser entregue! ");
            }
    }


    public void classificarEncomenda(String uEmail){
        Utilizador u= this.model.getUtilizador(uEmail);
        List<String> codigos_enc_entregues = u.getEncomendas_entregues().stream().filter(e -> ! e.isClassificada()).map(Encomenda::getCodEncomenda).collect(Collectors.toList());
        Menu aux = new Menu(codigos_enc_entregues);
        aux.executa();
        if(aux.getOp()!= 0) {
            int encomenda_index= aux.getOp();
            String codEnc_escolhida = codigos_enc_entregues.get(encomenda_index -1);
            Encomenda e_escolhida = this.model.getEncomendaC(codEnc_escolhida);
            aux.sendMessage("Classifique a encomenda "+codEnc_escolhida+" de 0 a 5: ");
            double cl= aux.lerDouble();
            e_escolhida.setClassificada(true);
            String cod_entidade_transportadora=e_escolhida.getCodEntidade_transportadora();
            if(cod_entidade_transportadora.charAt(0)=='v') this.model.getVoluntarioC(cod_entidade_transportadora).addClassificacao(cl);
            else this.model.getEmpresaC(cod_entidade_transportadora).addClassificacao(cl);

        }

    }

    public void licenca(int op, String email){
        System.out.println("Tem licença para entregas de encomendas médicas? (y/n)");
        if (this.auxiliar.leYesNo().equals("y")) this.model.availableToMed(op, email, true);
        else this.model.availableToMed(op, email, false);
    }


    void totalFaturado(String emailEmpresa){
        this.auxiliar.sendMessage("Data Inicial: ");
        LocalDateTime di = auxiliar.lerData();
        this.auxiliar.sendMessage("Data Final: ");
        LocalDateTime df = auxiliar.lerData();

        System.out.println("\nTotal Faturado pela empresa transportadora no intervalo indicado: " + this.model.getEmpresa(emailEmpresa).totalFaturado(di, df) + "€");
    }
}
