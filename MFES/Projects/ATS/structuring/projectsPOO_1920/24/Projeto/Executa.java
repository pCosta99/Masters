import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.io.IOException;
import java.io.FileNotFoundException;


public class Executa implements Serializable
{
    private Menu menuSignUp, menuUtilizador, menuEncomenda, menuYesNo, menuTiposLogin, menualterar,menuVoluntario, menuLoja, menuTransportadora;
    private MenuLogin menuLogIn;
    private TrazAqui t;
    private Parse p;
    private static final String regex = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$";
    private TransitoRandom transito = new TransitoRandom();

    Pattern pattern = Pattern.compile(regex);

    public static void main(String[] args){
        new Executa().run();
    }

    private Executa (){

        String[] sign = {"Login",
                        "Sign-up",
                        "Top 10"};

        String[] login = {"Tentar Novamente"};

        String[] tiposLogin = {"Utilizador",
                            "Voluntario",
                            "Transportadora",
                            "Loja"};

        String[] utilizador = {"Solicitar entrega de uma encomenda",
                            "Ver as minhas encomendas recebidas",
                            "Ver entregas ocurridas entre duas datas ",
                            "Classificar a entrega",
                            "Alterar informações"};

        String [] voluntario = {"Alterar serviços",
                                "Ver historico de entregas",
                                "Alterar informações"};

        String[] transportadora = {"Alterar serviços",
                                "Ver historico de entregas",
                                "Ver total faturado",
                                "Alterar informações"};

        String [] loja = {"Ver encomendas prontas a entregar",
                        "Ver historico de encomendas entregues",
                        "Alterar informações"};

        String[] codencomenda = {"Insira o numero da encomenda"};

        String[] alterarLogin = {"Email", "Password"};

        String[] yesno = {"Sim", "Não"};

        this.menuSignUp = new Menu(sign);
        this.menuLogIn = new MenuLogin(login);
        this.menuUtilizador = new Menu(utilizador);
        this.menuEncomenda = new Menu(codencomenda);
        this.menuYesNo = new Menu(yesno);
        this.menuTiposLogin = new Menu (tiposLogin);
        this.menualterar = new Menu(alterarLogin);
        this.menuVoluntario = new Menu(voluntario);
        this.menuLoja = new Menu(loja);
        this.menuTransportadora = new Menu(transportadora);

        //this.t = new TrazAqui();
        System.out.println("Bem Vindo");
        //Carregar dados (Parse(agrs))
        //...
        this.p = new Parse();
        this.t = new TrazAqui();
        try{
            this.t = t.leFicheiro("./TrazAqui.object");
            System.out.println("Dados carregados com sucesso\n");
        }
        catch (FileNotFoundException e){
            try{
                System.out.println("A iniciar...\n");
                this.p.parse();
                this.t = new TrazAqui(this.p.getInfoAll(), this.p.getAllEnc(), this.p.getEncAceites(), this.p.getAllLogins());
            }
            catch (FileNotFoundException f){
                System.out.println("Ficheiro não encontrado.");
            }
        }
        catch (ClassNotFoundException | IOException e) {
            System.out.println("Erro.\n");
        }
    }

	//...
    //Guardar dados

    private void run() {
        System.out.println("Estado do Transito: " + this.transito.getTransito());
        do{
            clearScreen();
            String email="";
            this.menuSignUp.executa();
            outer:
            switch(this.menuSignUp.getOpcao()) {
                case 1: //Sign In
                clearScreen();
                do{
                    this.menuTiposLogin.executa();
                    switch(this.menuTiposLogin.getOpcao()){
                    case 1: //Login Utilizador
                    do {
                        clearScreen();
                        System.out.println("Menu de Login");
                        this.menuLogIn.executaParametros();
                        if(!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaUtilizador(this.menuLogIn.getEmail()) == null){
                            System.out.println("Palavra passe ou email errados");
                            this.menuLogIn.executaReader();
                            if (this.menuLogIn.getOpcao() == 0) {
                                clearScreen();
                                break outer;
                            }
                        }
                    } while (!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaUtilizador(this.menuLogIn.getEmail()) == null);
                        email = this.menuLogIn.getEmail();
                        Utilizadores u = retornaUtilizador(email);
                        do{
                            this.menuUtilizador.executa();
                            switch(this.menuUtilizador.getOpcao()){
                                case 1:
                                clearScreen();
                                String input;
                                int flag=0;
                                String encmed;
                                    do {
                                        System.out.println("A sua encomenda contem produtos médicos?");
                                        encmed = this.menuEncomenda.leSimNao();
                                        System.out.println("\nInsira o numero da encomenda:");
                                        input = this.menuEncomenda.leString();
                                        if(!(this.t.getAllEnc().containsKey(input))){
                                            System.out.println("Numero de encomenda invalido");
                                            this.menuLogIn.executaReader();
                                            if (this.menuLogIn.getOpcao() == 0) {
                                                break;
                                            }   
                                        }
                                        else {
                                            flag =1;
                                        }
                                    }while (!(this.t.getAllEnc().containsKey(input)));
                                        
                                    if(flag==1 && (this.t.getAllEnc().get(input).getUserCode()).equals(encontraUtilizador(email))){
                                        //mostrar ao utilizador entregas possiveis (switch para ele escolher caso nao haja voluntarios)
                                        Voluntarios v = haVoluntario(u.getGps(),t.getInfoAll(),encmed);
                                        Transportadoras tr = haTransportadora(u.getGps(),t.getInfoAll(),encmed);
                                        Encomenda enc = retornaEncomenda(input);
                                        if (v!=null){
                                            Double distv = Coordenadas.distance(retornaLoja(enc.getStoreCode()).getGps(),u.getGps()) + Coordenadas.distance(v.getGps(),retornaLoja(enc.getStoreCode()).getGps());
                                            System.out.println("Encontramos um Voluntario");
                                            System.out.println("A sua encomenda sera entregue em:");
                                            calculaTempoEntrega(distv,this.transito.getTransito());
                                            t.addEncomendaAceite(input, this.t.getAllEnc().get(input));
                                            addData(enc);
                                            enc.setReady(true);
                                            v.addHistorico(enc);
                                            u.adicionaPorClassificar(input);
                                            u.addHistorico(enc);
                                            System.out.println("0 - Sair");
                                            int k = -1;
                                            do{
                                                k = this.menuUtilizador.leInt();
                                            }while (k!=0);
                                        }
                                        else {
                                            if (tr!=null){
                                                Double distt = Coordenadas.distance(tr.getGps(),u.getGps()) + Coordenadas.distance(tr.getGps(),retornaLoja(enc.getStoreCode()).getGps());
                                                System.out.println("Encontramos uma transportadora");
                                                System.out.println("Aceita pagar "+Math.round(tr.getPricekm()*distt*100.0)/100.0+"€?");
                                                this.menuYesNo.executa();
                                                switch(this.menuYesNo.getOpcao()){
                                                    case 1:
                                                    //Entrega encomenda
                                                    System.out.println("A sua encomenda sera entregue em:");
                                                    calculaTempoEntrega(distt, this.transito.getTransito());
                                                    t.addEncomendaAceite(input, this.t.getAllEnc().get(input));
                                                    addData(enc);
                                                    enc.setReady(true);
                                                    tr.addHistorico(enc);
                                                    u.adicionaPorClassificar(input);
                                                    u.addHistorico(enc);
                                                    System.out.println("0 - Sair");
                                                    int j = -1;
                                                    do{
                                                        j=this.menuUtilizador.leInt();
                                                    }while(j!=0);
                                                    clearScreen();
                                                    break;
                                                    case 2:
                                                    break;
                                                }
                                            }
                                            else{
                                                System.out.println("Não existe ninguem para transportar a sua encomenda");
                                            }
                                        }
                                    }
                                    else {
                                        System.out.println("Numero de encomenda invalido");
                                        //break;
                                    }
                                    break;

                                case 2:
                                clearScreen();
                                //Mostrar historico e voltar atras apenas
                                int k=-1;
                                System.out.println(mostraHistoricoU(u));
                                System.out.println("0 - Sair");
                                do{//alteracao..............
                                    k=this.menuLogIn.leInt();
                                }while (k!=0);
                                break;

                                case 3://aceder a entregas de voluntarios/transportadoras~
                                clearScreen();
                                historico();
                                k = -1;
                                System.out.println("0 - Sair");
                                do{
                                    k = this.menuUtilizador.leInt();
                                }while (k!=0);
                                break;

                                case 4:
                                //Classificar
                                clearScreen();
                                adicionaClassificacao(email);
                                break;

                                case 5: //Alterar informaçoes (email/password)
                                clearScreen();
                                alteralogin(u);
                                System.out.println("A voltar ao menu de Login");
                                break outer;
                                }
                        }while (this.menuUtilizador.getOpcao()!=0);
                        break;
                    case 2://Login Voluntario
                    do {
                        clearScreen();
                        System.out.println("Menu de Login");
                        this.menuLogIn.executaParametros();
                        if(!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaVoluntario(this.menuLogIn.getEmail()) == null){
                            System.out.println("Palavra passe ou email errados");
                            this.menuLogIn.executaReader();
                            if (this.menuLogIn.getOpcao() == 0) {
                                clearScreen();
                                break outer;
                            }
                        }
                    } while (!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaVoluntario(this.menuLogIn.getEmail()) == null);
                    email = this.menuLogIn.getEmail();
                    Voluntarios v = retornaVoluntario(email);
                    do{
                        clearScreen();
                        showRating(v);
                        this.menuVoluntario.executa();
                        switch(this.menuVoluntario.getOpcao()){
                            case 1:
                            clearScreen();
                            mudaEstadoV(v);
                            break;

                            case 2:
                            clearScreen();
                            int j=-1;
                                System.out.println(mostraHistoricoV(v));
                                System.out.println("0 - Sair");
                                do{//alteracao..............
                                    j=this.menuVoluntario.leInt();
                                }while (j!=0);
                            break;

                            case 3:
                            clearScreen();
                            alteralogin(v);
                            System.out.println("A voltar ao menu de Login");
                            break outer;
                        }
                    }while (this.menuVoluntario.getOpcao()!=0);
                    break;

                    case 3://Login Transportadora
                    do {
                        clearScreen();
                        System.out.println("Menu de Login");
                        this.menuLogIn.executaParametros();
                        if(!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaTransportadora(this.menuLogIn.getEmail()) == null){
                            System.out.println("Palavra passe ou email errados");
                            this.menuLogIn.executaReader();
                            if (this.menuLogIn.getOpcao() == 0) {
                                clearScreen();
                                break outer;
                            }
                        }
                    } while (!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaTransportadora(this.menuLogIn.getEmail()) == null);
                    email = this.menuLogIn.getEmail();
                    Transportadoras tr = retornaTransportadora(email);
                    do{
                        clearScreen();
                        showRating(tr);
                        this.menuTransportadora.executa();
                        switch(this.menuTransportadora.getOpcao()){
                            case 1:
                            clearScreen();
                            mudaEstadoT(tr);
                            break;

                            case 2:
                            clearScreen();
                            int j=-1;
                                System.out.println(mostraHistoricoT(tr));
                                System.out.println("0 - Sair");
                                do{
                                    j=this.menuTransportadora.leInt();
                                }while (j!=0);
                            break;

                            case 3: //total faturado entre 2 datas
                            clearScreen();
                            totalFaturado(tr);
                            do{
                                j=this.menuTransportadora.leInt();
                            }while (j!=0);
                            clearScreen();
                            break;
                            case 4:
                            clearScreen();
                            alteralogin(tr);
                            System.out.println("A voltar ao menu de Login");
                            break outer;
                        }
                    }while (this.menuTransportadora.getOpcao()!=0);
                    break;

                    case 4://Login Loja
                    do {
                        clearScreen();
                        System.out.println("Menu de Login");
                        this.menuLogIn.executaParametros();
                        if(!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaLojas(this.menuLogIn.getEmail()) == null){
                            System.out.println("Palavra passe ou email errados");
                            this.menuLogIn.executaReader();
                            if (this.menuLogIn.getOpcao() == 0) {
                                clearScreen();
                                break outer;
                            }
                        }
                    } while (!this.t.verificaLogin(this.menuLogIn.getEmail(), this.menuLogIn.getPassword()) || retornaLojas(this.menuLogIn.getEmail()) == null);
                    email = this.menuLogIn.getEmail();
                    Lojas l = retornaLojas(email);
                    do{
                        this.menuLoja.executa();
                        switch(this.menuLoja.getOpcao()){
                            case 1:
                            clearScreen();
                            showEncProntas(l);
                            System.out.println("0 - Sair");
                            int j = -1;
                            do{
                                j=this.menuUtilizador.leInt();
                            }while(j!=0);
                            clearScreen();
                            break;

                            case 2:
                            clearScreen();
                            j=-1;
                                System.out.println(mostraHistoricoL(l));
                                System.out.println("0 - Sair");
                                do{//alteracao..............
                                    j=this.menuLoja.leInt();
                                }while (j!=0);
                            break;

                            case 3:
                            clearScreen();
                            alteralogin(l);
                            System.out.println("A voltar ao menu de Login");
                            break outer;
                        }
                    }while (this.menuLoja.getOpcao()!=0);
                    break;
                    }
                }while(this.menuTiposLogin.getOpcao()!=0);
                break;
                case 2: //Sign in
                do{
                    clearScreen();
                    this.menuTiposLogin.executa();
                    switch(this.menuTiposLogin.getOpcao()){
                        case 1://registar utilizador
                        clearScreen();
                        registaUtilizador();
                        break;
                        case 2://registar voluntario
                        clearScreen();
                        registaVoluntario();
                        break;
                        case 3://registar transportadora
                        clearScreen();
                        registaTransportadora();
                        break;
                        case 4://registar loja
                        clearScreen();
                        registaLoja();
                        break;
                    }
                }while(this.menuLogIn.getOpcao()!=0);
                break;
                case 3: //Top10
                clearScreen();
                executaTopT();
                System.out.println("----------------------");
                executaTopU();
                break;
            }     
        }while(this.menuSignUp.getOpcao()!=0);
        System.out.println("Fim!");
        //Guarda em ficheiro
	    try{
            this.t.gravaCSV("./infoGravada.csv");
            System.out.println("Escreveu.\n");

        }
        catch (FileNotFoundException e){
	        System.out.println("Ficheiro nao encontrado.\n");
        }

        try{
            this.t.gravaEmFicheiro("./TrazAqui.object");
        }
        catch (FileNotFoundException e){
            System.out.println("Ficheiro nao encontrado.\n");
        }

        catch (IOException e){
            e.printStackTrace();
            System.out.println("Erro ao gravar.\n");
        }   
    }

        
    

    public void clearScreen() {  
        System.out.print("\u001B[2J");
        System.out.flush();
      }

    private String encontraUtilizador(String email){
        String cod = "";
        for (AllUsers t : this.t.getInfoAll().values()){
            if (t.tipoUtilizador().equals("Utilizador")){
                Utilizadores u = (Utilizadores) t;
                if (u.getEmail().equals(email)){
                    cod=u.getCode();
                }
            }
        }
        return cod;
    }

    private Utilizadores retornaUtilizador(String email){
        Utilizadores u = null;
        for (AllUsers a : this.t.getInfoAll().values()){
            if (a.tipoUtilizador().equals("Utilizador") && a.getEmail().equals(email)){
                u = (Utilizadores) a;
                return u;
            }
        }
        return u;
    }

    private Transportadoras retornaTransportadora(String email){
        Transportadoras t = null;
        for (AllUsers a : this.t.getInfoAll().values()){
            if (a.tipoUtilizador().equals("Transportadora") && a.getEmail().equals(email)){
                t = (Transportadoras) a;
                return t;
            }
        }
        return t;
    }

    private Voluntarios retornaVoluntario(String email){
        Voluntarios v = null;
        for (AllUsers a : this.t.getInfoAll().values()){
            if (a.tipoUtilizador().equals("Voluntario") && a.getEmail().equals(email)){
                v = (Voluntarios) a;
                return v;
            }
        }
        return v;
    }

    private Lojas retornaLojas(String email){
        Lojas l = null;
        for (AllUsers a : this.t.getInfoAll().values()){
            if (a.tipoUtilizador().equals("Loja") && a.getEmail().equals(email)){
                l = (Lojas) a;
                return l;
            }
        }
        return l;
    }

    private Lojas retornaLoja(String codloja){
        Lojas l = null;
        for (AllUsers a : this.t.getInfoAll().values()){
            if (a.tipoUtilizador().equals("Loja") && a.getCode().equals(codloja)){
                l = (Lojas) a;
                return l;
            }
        }
        return l;
    }

    private Voluntarios haVoluntario(Coordenadas gps, Map<String, AllUsers> allusers, String encmed){
        Voluntarios v = null;
        Voluntarios vf = null;
        for (AllUsers t : this.t.getInfoAll().values()){
            if (t.tipoUtilizador().equals("Voluntario") && encmed.equals("Nao")){
                v = (Voluntarios) t;
                if (v.zonaEntrega(v.getGps(), gps) && v.getPronto()){
                    vf = (Voluntarios) v;
                    return vf;
                }
            }

            else{
                if (t.tipoUtilizador().equals("Voluntario")){
                    v = (Voluntarios) t;
                    if (v.zonaEntrega(v.getGps(), gps) && v.getAceitamed() && v.aceitoTransporteMedicamentos() && v.getPronto()){
                        vf = (Voluntarios) v;
                        return vf;
                    }
                }
            }
        }
        return vf;
    }

    private Transportadoras haTransportadora (Coordenadas gps, Map<String, AllUsers> allusers, String encmed){
        double price = 0;
        double dist = 0;
        double min = Double.POSITIVE_INFINITY;
        Transportadoras t = null;
        Transportadoras tf = null;
        for (AllUsers a : this.t.getInfoAll().values()){
            if (a.tipoUtilizador().equals("Transportadora") && encmed.equals("Nao")){
                t = (Transportadoras) a;
                if (t.zonaEntrega(t.getGps(), gps) && t.getPronto()){
                    dist = Coordenadas.distance(t.getGps(),gps);
                    price = t.getPricekm()*dist;
                    if (price<min){
                        min=price;
                        tf=t;
                    }
                }
            }

            else{
                if (a.tipoUtilizador().equals("Transportadora")){
                    t = (Transportadoras) a;
                    if (t.zonaEntrega(t.getGps(), gps) && t.aceitoTransporteMedicamentos() && t.getAceitamed() && t.getPronto()){
                        dist = Coordenadas.distance(t.getGps(),gps);
                        price = t.getPricekm()*dist;
                        if (price<min){
                            min=price;
                            tf=t;
                        }
                    }
                }
            }
        }
        return tf;
    }

    private void adicionaClassificacao(String email){
        int rating;
        String codenc = "";
        boolean valid = false;
        Encomenda encomenda = null;
        System.out.println("Estes são as suas entregas por classificar:");
        List<String> pent = this.retornaUtilizador(email).getPorClassificar();
        if(pent.isEmpty()) {
            System.out.println("Não há entregas por classificar.");
            return;
        }
        for (String e:pent){
            System.out.println("\nCodigo da encomenda: " + e);
        }
        do{
            System.out.print("\nInsira o codigo da encomenda para o qual deseja classificar a entrega:");
            codenc = this.menuLogIn.leString();
        }while(codenc=="");

        for(String e:pent) {
            if(codenc.equals(e)) {
                encomenda = retornaEncomenda(e);
                valid = true;
                pent.remove(codenc);
                this.retornaUtilizador(email).setPorClassificar(pent);
                break;
            }
        }

        if (!valid){
            System.out.println("Codigo de encomenda invalido");
            return;
        }

        do{
            System.out.print("Insira uma classificação (0-10): ");
            rating=this.menuLogIn.leInt();
        }while(rating==-1 || rating<0 || 10<rating);

        Transportadoras tra = null;
        Voluntarios vol = null;
        for (AllUsers a : this.t.getInfoAll().values()){
            if (a.tipoUtilizador().equals("Transportadora")){
                tra = (Transportadoras) a;
                if (tra.getHistorico().contains(encomenda)){
                    tra.addclassificacao(rating);
                }
            }
            else{
                if (a.tipoUtilizador().equals("Voluntario")){
                    vol = (Voluntarios) a;
                    if (vol.getHistorico().contains(encomenda)){
                        vol.addclassificacao(rating);
                    }
                }
            }
        }
    }
    private void alteralogin(AllUsers u){
        String novoemail= "";
        String novapassword= "";
        do{
        this.menualterar.executa();
        switch(this.menualterar.getOpcao()){
            case 1:
            System.out.println("Insira um novo email");//adicionar uma exception para ver se o mail ja existe
            novoemail=this.menualterar.leString();
            try{
                alteraEmail(novoemail,u);
            }catch (EmailRepetidoException e){
                System.out.println("Erro  :" + e);
            }
            break;
            case 2:
            System.out.println("Insira uma nova password");
            novapassword=this.menualterar.leString();
            
            if (novapassword != u.getPassword()){
                u.setPassword(novapassword);
                this.t.getAllLogins().remove(u.getEmail());
                this.t.getAllLogins().put(u.getEmail(), novapassword);
            }
            break;
        }
        }while(this.menualterar.getOpcao()!=0);
        
    }

    public void alteraEmail(String email, AllUsers u) throws EmailRepetidoException{
        if (this.t.getAllLogins().containsKey(email)){
            throw new EmailRepetidoException("Email já existente");
        }
        else{
            this.t.getAllLogins().remove(u.getEmail());
            u.setEmail(email);
            this.t.getAllLogins().put(u.getEmail(), u.getPassword());
        }
    }

    private void addData(Encomenda e){
        e.setDate(LocalDateTime.now());
    }
    
    private void historico(){
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        LocalDateTime inicio,fim;
        System.out.println("Data de início: ");
        inicio = this.menuLogIn.lerData();
        System.out.println("Data de fim: ");
        fim = this.menuLogIn.lerData();
        List<Encomenda> historico = t.getAllEnc().values().stream()
                .filter(e -> e.getDate()!= null && e.getDate().isAfter(inicio) && e.getDate().isBefore(fim) && this.t.getEncAceites().containsKey(e.getOrderCode()))
                .collect(Collectors.toList());
        if(historico.isEmpty()) {
            System.out.println("Não há entregas dentro desse período.");
            return;
        }
        System.out.println("Entregas efetuadas entre " +inicio.format(formatter).toString()+ " e " +fim.format(formatter).toString()+":");
        for (Encomenda enc : historico){
            System.out.println(enc);
        }
    }

    private void mudaEstadoV(Voluntarios v){
        System.out.println("Encontro-me pronto para fazer entregas? "+v.getPronto());
        System.out.println("Deseja alterar? Sim/Nao");
        String s = "";
        String n = "";
        do{
            s = this.menuVoluntario.leSimNao();
            if (s != null){
                if (s.equals("Sim")){
                    v.setPronto(!v.getPronto());
                    System.out.println("Estado alterado para "+v.getPronto());
                }
                else{
                    System.out.println("O seu estado continuara igual");
                }
            }
            else{
                System.out.println("Insira uma resposta valida!");
            }
        }while (s==null);

        System.out.println("Encontro-me pronto para entregar encomendas médicas? "+v.aceitoTransporteMedicamentos());
        System.out.println("Deseja alterar? Sim/Nao");
        do{
            n = this.menuVoluntario.leSimNao();
            if (n != null){
                if (n.equals("Sim")){
                    v.aceitaMedicamentos(!v.aceitoTransporteMedicamentos());
                    System.out.println("Estado alterado para "+v.aceitoTransporteMedicamentos());
                }
                else{
                    System.out.println("O seu estado continuara igual");
                }
            }
            else{
                System.out.println("Insira uma resposta valida!");
            }
        }while (n==null);

    }

    private void mudaEstadoT(Transportadoras v){
        System.out.println("Encontro-me pronto para fazer entregas? "+v.getPronto());
        System.out.println("Deseja alterar? Sim/Nao");
        String s = "";
        String n = "";
        do{
            s = this.menuVoluntario.leSimNao();
            if (s != null){
                if (s.equals("Sim")){
                    v.setPronto(!v.getPronto());
                    System.out.println("Estado alterado para "+v.getPronto());
                }
                else{
                    System.out.println("O seu estado continuara igual");
                }
            }
            else{
                System.out.println("Insira uma resposta valida!");
            }
        }while (s==null);

        System.out.println("Encontro-me pronto para entregar encomendas médicas? "+v.aceitoTransporteMedicamentos());
        System.out.println("Deseja alterar? Sim/Nao");
        do{
            n = this.menuVoluntario.leSimNao();
            if (n != null){
                if (n.equals("Sim")){
                    v.aceitaMedicamentos(!v.aceitoTransporteMedicamentos());
                    System.out.println("Estado alterado para "+v.aceitoTransporteMedicamentos());
                }
                else{
                    System.out.println("O seu estado continuara igual");
                }
            }
            else{
                System.out.println("Insira uma resposta valida!");
            }
        }while (n==null);

    }

    private void calculaTempoEntrega(Double dist, String estadotransito){
        double vmedia = 50;
        double hora = 0;
        double minutos = 0;

        if(estadotransito.equals("moderado")){
            vmedia = 35;
        }

        if(estadotransito.equals("congestionado")){
            vmedia = 20;
        }

        double tempo = Math.round(dist/vmedia);

        if (tempo>60){
            hora = Math.floor(tempo/60);
            System.out.println("\n"+hora+" horas");
        }
        minutos = tempo%60;
        System.out.println("\n"+minutos+" minutos");
    }

    private void showEncProntas(Lojas l){
        List<Encomenda> nova = new ArrayList<>();
        System.out.println("Lista de encomendas prontas a entregar:");
        for (Encomenda e : this.t.getAllEnc().values()){
            if (e.getReady() && e.getStoreCode().equals(l.getCode())){
                nova.add(e);
            }
        }
        if (nova.size()!=0){
            for (Encomenda e : nova){
                System.out.println("\nCódigo da encomenda: "+e.getOrderCode());
            }
        }
        else{
            System.out.println("Não existem encomendas prontas a entregar");
        }
    }

    private void executaTopT(){
        System.out.println("Top 10 Transportadoras:");
        List<Transportadoras> top10t = new ArrayList<>();
        Transportadoras a = null;
        int i = 0;
        for (AllUsers t : this.t.getInfoAll().values()){
            if (t.tipoUtilizador().equals("Transportadora")){
                a=(Transportadoras)t;
                top10t.add(a);
            }
        }
        if(a==null){
            System.out.println("Nao existem Transportadoras para classificar!");
        }
        else{
            top10t.sort(new ComparatorNumKmT());
            if (top10t.size()>=10){
                top10t=top10t.subList(0,10);
            }
            for (Transportadoras t : top10t){
                System.out.println(i+1+" - "+t.getUser()+" - "+t.getHistorico().size());
                i++;
            }
        }
    }

    private void executaTopU(){
        System.out.println("Top 10 Utilizadores:");
        List<Utilizadores> top10u = new ArrayList<>();
        Utilizadores u = null;
        int i = 0;
        int k = -1;
        for (AllUsers t : this.t.getInfoAll().values()){
            if (t.tipoUtilizador().equals("Utilizador")){
                u=(Utilizadores) t;
                top10u.add(u);
            }
        }
        if(u==null){
            System.out.println("Nao existem Utilizadores para classificar!");
        }
        else{
            top10u.sort(new ComparatorNumEncU());
            if (top10u.size()>=10){
                top10u=top10u.subList(0,10);
            }
            for (Utilizadores y : top10u){
                System.out.println(i+1+" - "+y.getUser()+" - "+y.getHistorico().size());
                i++;
            }
        }
        do{
            System.out.println("0 - Sair");
            k = this.menuLogIn.leInt();
        }while (k!=0);
    }

    private Encomenda retornaEncomenda (String cod){
        Encomenda en = null;
        en = this.t.getAllEnc().get(cod);
        return en;
    }

    private void totalFaturado(Transportadoras t){
        LocalDateTime inicio,fim;
        System.out.println("Data de início: ");
        inicio = this.menuLogIn.lerData();
        System.out.println("Data de fim: ");
        fim = this.menuLogIn.lerData();
        double total=0;
        double distancia = 0;
        for (Encomenda e : t.getHistorico()){
            if (e != null && e.getDate()!= null && e.getDate().isAfter(inicio) && e.getDate().isBefore(fim)){
                distancia = Coordenadas.distance(t.getGps(),retornaUtilizador(e.getUserCode()).getGps());
                total+=Math.round(t.getPricekm()*distancia*100.0)/100.0;
            }
        }
        if (total!=0){
            System.out.print("Faturou "+total+"€ entre "+inicio+" e "+fim+".");
        }
        else{
            System.out.println("Nao faturou entre "+inicio+" e "+fim+".");
        }

    }

    private String mostraHistoricoL(AllUsers a){
        StringBuilder sb = new StringBuilder();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        sb.append("\nHistórico de Encomendas:");
        for (Encomenda e : this.t.getAllEnc().values()){
            if (a.tipoUtilizador().equals("Loja") && a.getCode().equals(e.getStoreCode())){
                if(e.getDate()!=null){
                    sb.append("\nData: "+e.getDate().format(formatter));
                }
                else{
                    break;
                }
                sb.append("\nCódigo: "+e.getOrderCode());
                sb.append("\nDestinatario: "+e.getUserCode());
                sb.append("\nLoja: "+e.getStoreCode());
                sb.append("\n-----------------------------");
            }
        }
        return sb.toString();
    }

    private String mostraHistoricoU(Utilizadores u){
        StringBuilder sb = new StringBuilder();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        sb.append("\nHistórico de Encomendas:");
        for (Encomenda e : u.getHistorico()){
            sb.append("\nData: "+ e.getDate().format(formatter));
            sb.append("\nCódigo: "+e.getOrderCode());
            sb.append("\nDestinatario: "+e.getUserCode());
            sb.append("\nLoja: "+e.getStoreCode());
            sb.append("\n-----------------------------");
        }
        return sb.toString();
    }

    private String mostraHistoricoT(Transportadoras u){
        StringBuilder sb = new StringBuilder();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        sb.append("\nHistórico de Encomendas:");
        for (Encomenda e : u.getHistorico()){
            sb.append("\nData: "+ e.getDate().format(formatter));
            sb.append("\nCódigo: "+e.getOrderCode());
            sb.append("\nDestinatario: "+e.getUserCode());
            sb.append("\nLoja: "+e.getStoreCode());
            sb.append("\n-----------------------------");
        }
        return sb.toString();
    }

    private String mostraHistoricoV(Voluntarios u){
        StringBuilder sb = new StringBuilder();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        sb.append("\nHistórico de Encomendas:");
        for (Encomenda e : u.getHistorico()){
            sb.append("\nData: "+ e.getDate().format(formatter));
            sb.append("\nCódigo: "+e.getOrderCode());
            sb.append("\nDestinatario: "+e.getUserCode());
            sb.append("\nLoja: "+e.getStoreCode());
            sb.append("\n-----------------------------");
        }
        return sb.toString();
    }

    private void registaUtilizador(){
        Utilizadores newuser = new Utilizadores();
        String nome, codigo, email, password = "";
        double x = 0;
        double y = 0;
        Matcher matcher = null;
        System.out.println("A registar um utilizador...");
        System.out.println("Insira o seu nome");
        nome = this.menualterar.leString();
        newuser.setUser(nome);
        do{
            System.out.println("Insira o seu codigo de utilizador (u**)");
            codigo = this.menualterar.leString();
            if (codigo.charAt(0)!='u'){
                System.out.println("Código de utilizador invalido!!!");
            }
            if (this.t.getInfoAll().containsKey(codigo)){
                System.out.println("Código de utilizador já existe!!!");
            }
        }while(this.t.getInfoAll().containsKey(codigo) || codigo.charAt(0)!='u');
        newuser.setCode(codigo);
        System.out.println("Insira a sua morada (x)");
        x = this.menualterar.leDouble();
        System.out.println("Insira a sua morada (y)");
        y = this.menualterar.leDouble();
        Coordenadas cords = new Coordenadas(x,y);
        newuser.setGps(cords);
        do{
            System.out.println("Insira o seu email");
            email=this.menualterar.leString();
            if (this.t.getAllLogins().containsKey(email)){
                System.out.println("Email já em utilização");
            }
            matcher = pattern.matcher(email);
            if (matcher.matches()==false){
                System.out.println("Formato inválido");
            }
        }while(this.t.getAllLogins().containsKey(email) || !matcher.matches());
        newuser.setEmail(email);
        System.out.println("Insira a sua password");
        password=this.menualterar.leString();
        newuser.setPassword(password);
        this.t.getAllLogins().put(email, password);
        this.t.getInfoAll().put(codigo, newuser);
    }

    private void registaVoluntario(){
        Voluntarios newuser = new Voluntarios();
        String nome, codigo, email, password, cert = "";
        double x, y, raio = 0;
        Matcher matcher = null;
        System.out.println("A registar um voluntario...");
        System.out.println("Insira o seu nome");
        nome = this.menualterar.leString();
        newuser.setUser(nome);
        do{
            System.out.println("Insira o seu codigo de voluntario (v**)");
            codigo = this.menualterar.leString();
            if (codigo.charAt(0)!='v'){
                System.out.println("Código de voluntario invalido!!!");
            }
            if (this.t.getInfoAll().containsKey(codigo)){
                System.out.println("Código de voluntario já existe!!!");
            }
        }while(this.t.getInfoAll().containsKey(codigo) || codigo.charAt(0)!='v');
        newuser.setCode(codigo);
        System.out.println("Insira a sua morada (x)");
        x = this.menualterar.leDouble();
        System.out.println("Insira a sua morada (y)");
        y = this.menualterar.leDouble();
        Coordenadas cords = new Coordenadas(x,y);
        newuser.setGps(cords);
        do{
            System.out.println("Insira o seu email");
            email=this.menualterar.leString();
            if (this.t.getAllLogins().containsKey(email)){
                System.out.println("Email já em utilização");
            }
            matcher = pattern.matcher(email);
            if (matcher.matches()==false){
                System.out.println("Formato inválido");
            }
        }while(this.t.getAllLogins().containsKey(email) || !matcher.matches());
        newuser.setEmail(email);
        System.out.println("Insira a sua password");
        password=this.menualterar.leString();
        System.out.println("Possui certificado para entrega de encomendas médicas? (Sim/Nao)");
        do{
            cert=this.menualterar.leString();
            if (cert.equals("Sim")){
                newuser.setCertificado(true);
            }
            if (cert.equals("Nao")){
                newuser.setCertificado(false);
            }
        }while (cert!="Sim" || cert!="Nao");
        System.out.println("Qual o seu raio de distrubuição");
        raio = this.menualterar.leDouble();
        newuser.setVraio(raio);
        newuser.setPassword(password);
        this.t.getAllLogins().put(email, password);
        this.t.getInfoAll().put(codigo, newuser);
    }

    private void registaTransportadora(){
        Transportadoras newuser = new Transportadoras();
        String nome, codigo, email, password, cert = "";
        double x,y, preco, raio = 0;
        int numeroencomendas=1;
        Matcher matcher = null;
        System.out.println("A registar um transportadora...");
        System.out.println("Insira o nome da sua empresa");
        nome = this.menualterar.leString();
        newuser.setUser(nome);
        do{
            System.out.println("Insira o seu codigo de transportadora (t**)");
            codigo = this.menualterar.leString();
            if (codigo.charAt(0)!='t'){
                System.out.println("Código de transportadora invalido!!!");
            }
            if (this.t.getInfoAll().containsKey(codigo)){
                System.out.println("Código de transportadora já existe!!!");
            }
        }while(this.t.getInfoAll().containsKey(codigo) || codigo.charAt(0)!='t');
        newuser.setCode(codigo);
        System.out.println("Insira a localização da sua empresa (x)");
        x = this.menualterar.leDouble();
        System.out.println("Insira a localização da sua empresa (y)");
        y = this.menualterar.leDouble();
        Coordenadas cords = new Coordenadas(x,y);
        newuser.setGps(cords);
        do{
            System.out.println("Insira o seu email");
            email=this.menualterar.leString();
            if (this.t.getAllLogins().containsKey(email)){
                System.out.println("Email já em utilização");
            }
            matcher = pattern.matcher(email);
            if (matcher.matches()==false){
                System.out.println("Formato inválido");
            }
        }while(this.t.getAllLogins().containsKey(email) || !matcher.matches());
        newuser.setEmail(email);
        System.out.println("Insira a sua password");
        password=this.menualterar.leString();
        System.out.println("Possui certificado para entrega de encomendas médicas? (Sim/Nao)");
        do{
            cert=this.menualterar.leString();
            if (cert.equals("Sim")){
                newuser.setCert(true);
            }
            if (cert.equals("Nao")){
                newuser.setCert(false);
            }
        }while (cert!="Sim" || cert!="Nao");
        System.out.println("Quanto cobra por kilometro?");
        preco = this.menualterar.leDouble();
        System.out.println("Qual o raio de distrubuição da empresa?");
        raio = this.menualterar.leDouble();
        System.out.println("Quantas encomendas consegue a empresa transportar em simultaneo?");
        numeroencomendas = this.menualterar.leInt();
        newuser.setNumEnc(numeroencomendas);
        newuser.setRaio(raio);
        newuser.setPricekm(preco);
        newuser.setPassword(password);
        this.t.getAllLogins().put(email, password);
        this.t.getInfoAll().put(codigo, newuser);
    }

    private void registaLoja(){
        Lojas newuser = new Lojas();
        String nome, codigo, email, password = "";
        double x = 0;
        double y = 0;
        Matcher matcher = null;
        System.out.println("A registar uma loja...");
        System.out.println("Insira o nome da sua loja");
        nome = this.menualterar.leString();
        newuser.setUser(nome);
        do{
            System.out.println("Insira o código da sua loja (l**)");
            codigo = this.menualterar.leString();
            if (codigo.charAt(0)!='u'){
                System.out.println("Código invalido!!!");
            }
            if (this.t.getInfoAll().containsKey(codigo)){
                System.out.println("O código inserido já existe!!!");
            }
        }while(this.t.getInfoAll().containsKey(codigo) || codigo.charAt(0)!='u');
        newuser.setCode(codigo);
        System.out.println("Insira a morada da loja (x)");
        x = this.menualterar.leDouble();
        System.out.println("Insira a morada da loja (y)");
        y = this.menualterar.leDouble();
        Coordenadas cords = new Coordenadas(x,y);
        newuser.setGps(cords);
        do{
            System.out.println("Insira o seu email");
            email=this.menualterar.leString();
            if (this.t.getAllLogins().containsKey(email)){
                System.out.println("Email já em utilização");
            }
            matcher = pattern.matcher(email);
            if (matcher.matches()==false){
                System.out.println("Formato inválido");
            }
        }while(this.t.getAllLogins().containsKey(email) || !matcher.matches());
        newuser.setEmail(email);
        System.out.println("Insira a sua password");
        password=this.menualterar.leString();
        newuser.setPassword(password);
        this.t.getAllLogins().put(email, password);
        this.t.getInfoAll().put(codigo, newuser);
    }

    private void showRating(AllUsers a){
        double media = 0;
        if (a.getClassificacao()!= null && a.getClassificacao().size()>0){
            for (Integer i : a.getClassificacao()){
                media+=i;
            }
            System.out.println("Média das avaliações: "+media);

        }
        else{
            System.out.println("Ainda nao obteve avaliações");
        }
    }

}
