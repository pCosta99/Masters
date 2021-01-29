import java.text.DecimalFormat;
import java.time.DateTimeException;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.io.IOException;

public class Menus {
    private Scanner ler;

    public Menus(){
        ler = new Scanner(System.in);
    }


    public Logger menuLogin(TrazAqui db, ProgramController aux)
    {

        Logger a1 = new Logger();
        int op;
        Scanner ler2 = new Scanner(System.in).useDelimiter("\n");
        try {
            do {
                op = mostraOpcoes("Menu Principal",
                        new String[] {"Login",
                                "Registar","Show db","Top 10 Utilizadores","Top 10 Empresas","Criar contas automaticamente "});
                switch (op) {
                    case 1: {       //LOGIN!!!!!
                        System.out.print("E-mail:");
                        String user = ler2.next().toLowerCase();
                        System.out.print("Password:");
                        String pass = ler.next();
                        if (db.getLogIn().verificaLogin(user, pass)) {
                            System.out.println("Login valido");
                            aux.setExistente(true);
                            a1 = db.getLogIn().getLogger(user);
                            return a1;
                        } else {
                            System.out.println("Login invalido");
                            aux.setExistente(false);
                        }

                    }
                    break;
                    case 2: {       //REGISTAR
                        op = mostraOpcoes("Menu Registar",
                                new String[] {"Utilizador",
                                        "Transportadora",
                                        "Voluntario",
                                        "Loja"});
                        /**
                         * Criar conta.
                         */
                        //Utilizador
                        if (op == 1) {
                            LogUtilizador u = addUtilizador(db);
                             a1 = new LogUtilizador(u);
                            Utilizador u1 = new Utilizador(((LogUtilizador) a1).getDadosUtilizador());
                            if (a1!=null){
                                db.addLogIn(a1.getEmail(),a1);
                            }
                            if(u1!=null){
                                db.addUtilizador(u1.getCodUtilizador(),u1);
                            }
                            //Transportadora
                        } else if (op == 2) {
                            LogEmpresa e = addTransportadora(db);
                            a1 = new LogEmpresa(e);
                            Transportadora t1 = new Transportadora(((LogEmpresa) a1).getDadosEmpresa());
                            if (a1!=null){
                                db.getLogIn().addLogIn(a1.getEmail(),a1);
                            }
                            if(t1!=null){
                                db.addTransportadora(t1.getCodEmpresa(),t1);
                            }
                            //Voluntario
                        } else if (op == 3) {
                            LogVoluntario v = addVoluntario(db);
                            a1 = new LogVoluntario(v);
                            Voluntario t1 = new Voluntario(((LogVoluntario) a1).getDadosVoluntario());
                            if (a1!=null){
                                db.addLogIn(a1.getEmail(),a1);
                            }
                            if(t1!=null){
                                db.addVoluntario(t1.getCodVoluntario(),t1);
                            }
                            //Loja
                        }else if (op == 4) {
                            LogLoja logLojaL = addLoja(db);
                            a1 = new LogLoja(logLojaL);
                            Loja l1 = new Loja(((LogLoja) a1).getDadosLoja());
                            if (a1!=null){
                                db.addLogIn(a1.getEmail(),a1);
                            }
                            if(l1!=null){
                                db.addLoja(l1.getCodLoja(),l1);
                            }
                        }
                    }break;
                    case 3: {

                        System.out.println(db);
                        break;
                    }

                    case 4:{
                        System.out.println("Utilizadores com mais Encomendas feitas:\n(Codigo Utilizadro,Encomendas feitas)\n"+db.utilizadoresMaisEncomendas());
                        clickEnter();
                        break;
                    }

                    case 5:{
                        System.out.println("Empresas com mais Kms feitos:\n(Codigo Empresa,Kms feitos)\n"+db.empresasMaisKms());
                        clickEnter();
                        break;
                    }

                    case 6:{
                        System.out.println("Alto, isto é uma funcao restrita, precisa de password:");
                        String pass = ler2.next();
                        if(pass.equals("devkit123")){
                            System.out.println("A criar contas para todos o dados guardados:");
                            TrazAqui contas = this.automaticAccountCreation(db);
                            db = new TrazAqui(contas);
                        }
                        else System.out.println("Pass errada");
                        break;
                    }

                    
                    case 0: {
                        System.out.println("A sair da App");
                        op = mostraOpcoes("Deseja gravar estado da App?",
                                new String[] {"Sim",
                                        "Nao"});
                        if(op==1) {
                            System.out.println("Obrigado pela preferência.");
                            /**db.gravaFicheiro(); //  GRAVA O ESTADO DA APP */
                            try {
                                db.gravaTrazAqui(aux.getFile_path());
                            } catch (IOException e) {
                                System.out.println("Erro ao guardar o ficheiro.\n");
                            }
                            System.exit(0);
                        }
                        else if (op==2){
                            System.exit(0);
                        }
                    
                    }

                }

            } while (op != 0);
        }catch (InputMismatchException | DateTimeException  e){
            System.out.println(e.getMessage());
            ler= new Scanner(System.in);
        }
        return a1;
    }


    public void menuUtilizador(TrazAqui db, Logger a1) {
        TrazAqui clone =  db.clone();
        LogUtilizador utilizador = (LogUtilizador) a1.clone();
        int op;
        DecimalFormat fmt = new DecimalFormat("0.00");
        try{
            do {
                op = mostraOpcoes("Menu Cliente",
                        new String[] {"Solicitar Encomenda",
                                "Ver propostas",
                                "Lista de entregas por tempo",
                                "Lista de entregas por Voluntario ou transportadora",
                                "Classificar Servicos",
                                "Historico de Transportador entre Datas"});
                switch (op) {
                    case 1: {
                        int op1 = mostraOpcoes("Escolha Tipo:",
                                new String[]{ "Encomenda", 
                                        "Encomenda Medica"});


                        if(op1==1 || op1 ==2) {
                            Encomenda encomenda = addEncomenda(db,a1);
                            if(encomenda!=null) {
                                db.addEncomenda(encomenda.getCodigoEncomenda(), encomenda);
                                System.out.println("Encomenda feita");
                            }else System.out.println("Ocorreu um erro.");
                        }
                        break;

                    }
                    case 2: {
                        List<Servico> propostas = db.getHistorico().getServicos().stream().filter(e->e.getCodUtilizador().equals(utilizador.getDadosUtilizador().getCodUtilizador()) && !e.isConcluido()).collect(Collectors.toList());
                        if (propostas.size()>0) db.menuAceitacaoProposta(propostas);
                        else System.out.println("Nao ha propostas");
                        break;
                    }
                    case 3: {
                        db.menuHistoricoSEntidade();

                        break;
                    }
                    case 4: {
                        System.out.println("Codigo do transportador(empresa ou voluntario):");
                        String codV = ler.next();
                        List<Servico> servicos = db.getHistorico().getServicos().stream().filter(e->e.isConcluido() && e.getCodTranportador().equals(codV)).collect(Collectors.toList());
                        if(servicos.size()>0){
                            System.out.println(servicos);
                        }else System.out.println("Ainda nao ha historico deste transportador");
                        break;
                    }
                    case 5: {
                        List<Servico> servicosSC = db.getHistorico().getServicosSClassificacao(utilizador.getDadosUtilizador().getCodUtilizador());
                        if(servicosSC.size()== 0) System.out.println("Nao ha servicos por classificar");
                        else{
                            for(int i=0;i<servicosSC.size();i++){
                                int aux5=0;
                                    System.out.println(servicosSC.get(i));
                                    System.out.println("1)Classificar Servico               2)Proximo Servico                    3)Sair");
                                    aux5 = ler.nextInt();
                                    if (aux5 == 1) {
                                        System.out.println("Classificacao:");
                                        int calssificacao = ler.nextInt();
                                        if(calssificacao < 0 || calssificacao>10) System.out.println("Classificacao inválida");
                                        else {
                                            db.changeClassificacao(servicosSC.get(i), calssificacao);
                                            break;
                                        }
                                    }else if(aux5==2) break;
                                    else if(aux5==3) break;break;

                            }
                        }
                        break;
                    }
                    case 6 :{
                        db.menuHistorico();
                        clickEnter();
                    }break;
                }
            } while (op != 0);
        }catch (InputMismatchException | DateTimeException  e){
            if(e instanceof InputMismatchException)
                System.out.println("Input inválido");
            else
                System.out.println(e.getMessage());
            ler= new Scanner(System.in);
            menuUtilizador(db,a1);
        }
    }


    public void menuVoluntario(TrazAqui db, Logger a1) {
        TrazAqui clone = db.clone();
        LogVoluntario voluntario = (LogVoluntario) a1;
        int op;
        try {
            do {
                op = mostraOpcoes("Menu Voluntario",
                        new String[]{"Mudar Disponibilidade",
                                "Fazer Entrega"});
                switch (op) {
                    case 1: {
                        System.out.println("Disponibilidade:" + voluntario.isDisponibilidade() + " Deseja mudar?\nSim(s)  Nao(n) ");
                        String r = ler.next();
                        if (r .equals("s")) {
                            voluntario.changeDisponibilidade();
                            System.out.println("Disponibilidade mudada.");
                        } else if (r .equals("n")) System.out.println("Disponibilidade mantida");
                        else throw new InputMismatchException("Input Invalido");
                        break;
                    }
                    case 2:{
                        List<Encomenda> lista = db.encomendasDisponiveisVoluntario(voluntario.getDadosVoluntario());
                        db.menuServicos(lista,voluntario.getDadosVoluntario());
                        break;
                    }

                }
            }while(op!=0);
        }catch (InputMismatchException |  DateTimeException  e){
            if(e instanceof InputMismatchException)
                System.out.println("Input inválido");
            else
                System.out.println(e.getMessage());
            ler= new Scanner(System.in);
            menuVoluntario(db,a1);
        }
    }

    public void menuTransportadora(TrazAqui db, Logger a1) {
        TrazAqui clone = db.clone();
        LogEmpresa empresa = (LogEmpresa) a1;
        int op;
        try {
            do {
                op = mostraOpcoes("Menu Transportadora",
                        new String[]{"Mudar Disponibilidade",
                                "Ver total Faturado em determinado tempo",
                                "Fazer Entrega"});
                switch (op) {
                    case 1: {
                        System.out.println("Disponibilidade:" + empresa.isDisponibilidade() + " Deseja mudar?\nSim(1)  Nao(2) ");
                        int r = ler.nextInt();
                        if (r == 1) {
                            empresa.changeDisponibilidade();
                            System.out.println("Disponibilidade mudada.");
                        } else if (r == 2) System.out.println("Disponibilidade mantida");
                        else throw new InputMismatchException("Input Invalido");
                        break;
                    }
                    case 2: {
                        double faturado = db.totalFaturado(((LogEmpresa) a1).getDadosEmpresa().getCodEmpresa());
                        System.out.println("Total faturado = " + faturado);
                        break;
                    }

                    case 3:{
                        List<Encomenda> lista = db.encomendasDisponiveisTransportadora(((LogEmpresa) a1).getDadosEmpresa());
                        db.menuPropostas(lista,empresa.getDadosEmpresa());
                        break;
                    }

                }
            }while(op!=0);
        }catch (InputMismatchException |  DateTimeException  e){
            if(e instanceof InputMismatchException)
                System.out.println("Input inválido");
            else
                System.out.println(e.getMessage());
            ler= new Scanner(System.in);
            menuVoluntario(db,a1);
        }
    }



    public void menuLoja(TrazAqui db, Logger a1) {
        TrazAqui clone = db.clone();
        LogLoja loja = (LogLoja) a1;
        int op;
        try {
            do {
                op = mostraOpcoes("Menu Loja",
                        new String[]{"Lista de Encomendas Pedidas"});
                switch (op) {
                    case 1: {
                        List<Encomenda> lista1 = db.encomendasPLoja(loja.getDadosLoja().getCodLoja());
                        db.menuAceitacaoLoja(lista1);
                        break;
                    }


                }
            }while(op!=0);
        }catch (InputMismatchException |  DateTimeException  e){
            if(e instanceof InputMismatchException)
                System.out.println("Input inválido");
            else
                System.out.println(e.getMessage());
            ler= new Scanner(System.in);
            menuVoluntario(db,a1);
        }
    }



    public int mostraOpcoes(String titulo, String[] opcoes) throws InputMismatchException{
        Scanner ler = new Scanner(System.in);
        System.out.println("<=====>" + titulo + "<=====>");
        for (int i = 0; i < opcoes.length; i++) {
            System.out.println((1 + i) + "- " + opcoes[i]);
        }
        System.out.println("0 - Sair");
        int op = ler.nextInt();
        return op;
    }


    public LogUtilizador addUtilizador(TrazAqui db){
        String regex = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$";
        Pattern pattern = Pattern.compile(regex);
        Scanner ler2= new Scanner(System.in).useDelimiter("\n");
        String email="n/a", nome="n/a",codUtilizador="n/a",pass="n/a";
        int n=0,x=0,y=0;
            while(n==0) {
                System.out.print("E-mail:");
                email = ler.next();
                if (!pattern.matcher(email).matches()) {
                    System.out.println("Email nao valido");
                    n = 0;
                } else if (db.getLogIn().logContainsMail(email.toLowerCase())) {
                    System.out.println("Já existe conta com esse e-mail");
                } else n++;
            }

            while(n== 1) {
                System.out.print("Nome:");
                nome = ler2.next();
                n++;
            }

            while(n==2) {
                System.out.print("CodUtilizador(numero a sua escolha):");
                codUtilizador = ("u" + ler.next());
                if (db.getUtilizadores().containsKey(codUtilizador)) {
                    System.out.println("Já existe utilizador com esse codigo");
                }
                else n++;
            }

            while (n==3) {
                System.out.print("Password:");
                pass = ler.next();
                n++;
            }

            while (n==4) {
                System.out.print("Gps(x y):\n");
                x = ler.nextInt();
                y = ler.nextInt();
                n++;
            }

            while(n==5) {
                break;
            }
        Utilizador utilizador = new Utilizador(codUtilizador,nome,new GPS(x,y));
        return new LogUtilizador(email,pass,utilizador);
    }


    public LogEmpresa addTransportadora(TrazAqui db){
        String regex = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$";
        Pattern pattern = Pattern.compile(regex);
        Scanner ler2= new Scanner(System.in).useDelimiter("\n");
        try {
            System.out.print("E-mail:");
            String email = ler.next();
            if(!pattern.matcher(email).matches())
                throw new InputMismatchException("Email não é válido");
            if(db.getLogIn().logContainsMail(email.toLowerCase()))
                throw new ExistingAtorException("Já existe conta com esse e-mail");
            System.out.print("Nome Empresa:");
            String nomeEmpresa = ler2.next();
            System.out.print("CodEmpresa(numero a sua escolha):");
            String codEmpresa = ("t"+ler.next());
            if(db.getTransportadoras().containsKey(codEmpresa)){
                throw new ExistingAtorException("Já existe empresa com esse codigo");
            }
            System.out.print("Password:");
            String pass = ler.next();
            System.out.print("Gps(x y):\n");
            int x = ler.nextInt();
            int y = ler.nextInt();
            System.out.print("Nif:");
            String nif = ler.next();
            System.out.print("Raio de alcance:");
            double raio = ler.nextDouble();
            System.out.print("Preco por Km:");
            double preco = ler.nextDouble();


            Transportadora empresa = new Transportadora(codEmpresa,nomeEmpresa,new GPS(x,y),nif,raio,preco);
            return new LogEmpresa(email,pass,empresa,false);
        } catch (InputMismatchException | ExistingAtorException e){
            System.out.println(e.getMessage());
            return null;
        }
    }


    public LogVoluntario addVoluntario(TrazAqui db){
        String regex = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$";
        Pattern pattern = Pattern.compile(regex);
        Scanner ler2= new Scanner(System.in).useDelimiter("\n");
        try {
            System.out.print("E-mail:");
            String email = ler.next();
            if(!pattern.matcher(email).matches())
                throw new InputMismatchException("Email não é válido");
            if(db.getLogIn().logContainsMail(email.toLowerCase()))
                throw new ExistingAtorException("Já existe conta com esse e-mail");
            System.out.print("Nome:");
            String nome = ler2.next();
            System.out.print("CodVoluntario(numero a sua escolha):");
            String cod = ("v"+ler.next());
            if(db.getVoluntarios().containsKey(cod)){
                throw new ExistingAtorException("Já existe voluntario com esse codigo");
            }
            System.out.print("Password:");
            String pass = ler.next();
            System.out.print("Gps(x y):\n");
            int x = ler.nextInt();
            int y = ler.nextInt();
            System.out.print("Raio de alcance:");
            double raio = ler.nextDouble();

            Voluntario voluntario = new Voluntario(cod,nome,new GPS(x,y),raio);
            return new LogVoluntario(email,pass,false,voluntario);
        } catch (InputMismatchException | ExistingAtorException e){
            System.out.println(e.getMessage());
            return null;
        }
    }

    public LogLoja addLoja(TrazAqui db){
        String regex = "^[\\w!#$%&'*+/=?`{|}~^-]+(?:\\.[\\w!#$%&'*+/=?`{|}~^-]+)*@(?:[a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,6}$";
        Pattern pattern = Pattern.compile(regex);
        Scanner ler2= new Scanner(System.in).useDelimiter("\n");
        try {
            System.out.print("E-mail:");
            String email = ler.next();
            if(!pattern.matcher(email).matches())
                throw new InputMismatchException("Email não é válido");
            if(db.getLogIn().logContainsMail(email.toLowerCase()))
                throw new ExistingAtorException("Já existe conta com esse e-mail");
            System.out.print("Nome:");
            String nome = ler2.next();
            System.out.print("CodLoja(numero a sua escolha):");
            String cod = ("l"+ler.next());
            if(db.getLojas().containsKey(cod)){
                throw new ExistingAtorException("Já existe loja com esse codigo");
            }
            System.out.print("Password:");
            String pass = ler.next();
            System.out.print("Gps(x y):\n");
            int x = ler.nextInt();
            int y = ler.nextInt();

            Loja loja = new Loja(cod,nome,new GPS(x,y));
            return new LogLoja(email,pass,loja);
        } catch (InputMismatchException | ExistingAtorException e){
            System.out.println(e.getMessage());
            return null;
        }
    }


    public Encomenda addEncomenda(TrazAqui db, Logger a1) {
        LogUtilizador utilizador = (LogUtilizador) a1.clone();
        List<LinhaEncomenda> linhas = new ArrayList<>();
        Scanner ler = new Scanner(System.in).useDelimiter("\n");
        String codiU = utilizador.getDadosUtilizador().getCodUtilizador();
        int n = 0;
        String codigoE = null;
        String codL = null;
        double peso = 0;

            while(n== 0) {
                System.out.println("Codigo encomenda(numero a sua escolha):");
                codigoE = ("e" + ler.next());
                if (db.getEncomendas().get(codigoE) != null) {
                    System.out.println("Ja existe encomenda com esse codigo.");
                } else n++;
            }
            while(n==1) {
                System.out.println("Codigo da Loja:");
                String nomeL = ler.next();
                Loja lojaAux = db.getLoja(nomeL);
                if (lojaAux == null) {
                    System.out.println("Loja nao encontrada");
                } else {
                    codL = lojaAux.getCodLoja();
                    n++;
                }
            }
            while(n== 2) {
                System.out.println("Peso:");
                peso = ler.nextDouble();
                n++;
            }
            while(n== 3) {
                int l = 1;
                System.out.println("Produto:");
                while (l == 1) {
                    linhas.add(addLinhaEncomenda(db));
                    System.out.println("Adicionar novo Produto?\n1(sim) 2(nao)");
                    l = ler.nextInt();
                }
                n++;
            }

        return new Encomenda(codigoE, codiU, codL, peso, (ArrayList<LinhaEncomenda>) linhas);
    }

    public LinhaEncomenda addLinhaEncomenda(TrazAqui db){
        Scanner ler = new Scanner(System.in).useDelimiter("\n");

                System.out.println("Codigo produto(numero a sua escolha):");
                String codigoP=("p"+ler.next());

                System.out.println("Nome:");
                String descricao = ler.next();

                System.out.println("Quantidade:");
                double quantidade = ler.nextDouble();

                System.out.println("Preco:");
                double preco = ler.nextDouble();

                LinhaEncomenda linha = new LinhaEncomenda(codigoP,descricao,quantidade,preco);
                return linha;
    }

    public void clickEnter(){
        System.out.println("\nClique ENTER para prosseguir.");
        Scanner scanner = new Scanner(System.in);
        scanner.nextLine();
    }


    public TrazAqui automaticAccountCreation(TrazAqui db){
        Map<String,Utilizador> listaUtilizadores = db.getUtilizadores();
        Map<String,Loja> listaLojas = db.getLojas();
        Map<String,Voluntario> listaVoluntarios = db.getVoluntarios();
        Map<String,Transportadora> listaTransportadoras = db.getTransportadoras();
        for(Utilizador e:listaUtilizadores.values()){
            LogUtilizador logUtilizador = new LogUtilizador((e.getCodUtilizador()+"@gmail.com"),e.getCodUtilizador(),e);
            if (!db.getLogIn().getLogs().containsValue(logUtilizador)) db.addLogIn(logUtilizador.getEmail(),logUtilizador);
        }
        System.out.println("Utilizadores feito!");
        for(Loja e:listaLojas.values()){
            LogLoja logLoja = new LogLoja((e.getCodLoja()+"@gmail.com"),e.getCodLoja(),e);
            if (!db.getLogIn().getLogs().containsValue(logLoja)) db.addLogIn(logLoja.getEmail(),logLoja);
        }
        System.out.println("Lojas feito!");
        for(Voluntario e:listaVoluntarios.values()){
            LogVoluntario logVoluntario = new LogVoluntario((e.getCodVoluntario()+"@gmail.com"),e.getCodVoluntario(),false,e);
            if (!db.getLogIn().getLogs().containsValue(logVoluntario)) db.addLogIn(logVoluntario.getEmail(),logVoluntario);
        }
        System.out.println("Voluntarios feito!");
        for(Transportadora e:listaTransportadoras.values()){
            LogEmpresa logEmpresa = new LogEmpresa((e.getCodEmpresa()+"@gmail.com"),e.getCodEmpresa(),e,false);
            if (!db.getLogIn().getLogs().containsValue(logEmpresa)) db.addLogIn(logEmpresa.getEmail(),logEmpresa);
        }
        System.out.println("feito!");
        return db;
    }
}
