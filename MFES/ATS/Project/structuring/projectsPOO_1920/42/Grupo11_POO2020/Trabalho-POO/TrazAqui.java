import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.*;

public class TrazAqui implements Serializable {

    private final static String passwordAdmin = "admin";

    private static void imprimirmenu(int i){
        if (i == 1)
            menu1();
        if (i == 2)
            menu2();
        if (i == 3)
            menu3();
        if (i == 4)
            menu4();
        if( i == 5)
            menu5();
        if( i == 6)
            menu6();
    }


    //menu inicial
    private static void menu1(){
        System.out.println("------------------TrazAqui!------------------");
        System.out.println("Opçoes");
        System.out.println("1-Registar Utilizador"); //pode ser comprador, voluntário ou empresa
        System.out.println("2-Iniciar Sessão");
        System.out.println("3-Guardar Ficheiro");
        System.out.println("4-Importar Ficheiro");
        System.out.println("5-Sair");
    }

    //menu para logins
    private static void menu2(){
        System.out.println("------------------TrazAqui!------------------");
        System.out.println("Opçoes");
        System.out.println("1-Login do Comprador");
        System.out.println("2-Login da Empresa");
        System.out.println("3-Login do Voluntário");
        System.out.println("4-Login da Loja");
        System.out.println("5-Login do Admin");
        System.out.println("6-Voltar Menu anterior");
    }

    //menu do comprador
    private  static void menu3(){
        System.out.println("------------Comprador-----------");
        System.out.println("Opçoes");
        System.out.println("1-Realizar encomenda");
        System.out.println("2-Aceitar encomendas pendentes");
        System.out.println("3-Classificar entrega de encomenda");
        System.out.println("4-Verificar estado da encomenda");
        System.out.println("5-Logout");
    }

    //menu da empresa e voluntario
    private  static void menu4(){
        System.out.println("------------Transportador-----------");
        System.out.println("Opçoes");
        System.out.println("1- Sinalizar disponibilidade");
        System.out.println("2-Realizar entrega");
        System.out.println("3-Consultar o total faturado");
        System.out.println("4-Consultar encomendas transportadas");
        System.out.println("5-Logout");
    }

    private static void menu5(){
        System.out.println("------------Loja-----------");
        System.out.println("Opçoes");
        System.out.println("1-Sinalizar encomenda");
        System.out.println("2-Logout");
    }

    private static void menu6(){
        System.out.println("------------Admin-----------");
        System.out.println("Opçoes");
        System.out.println("1-Top 10 utilizadores mais ativos");
        System.out.println("2-Top 10 empresas mais ativas");
        System.out.println("3-Logout");
    }



//metodo que dá login de um utilizador e devolve o utilizador o qual fez o login

    public static Utilizador loginComprador(Estrutura es, String id, String password) throws IdNotFoundException, InvalidValueException{
        Utilizador ret;

        try{
            ret = es.getUtilizadores().getUtilizador(id);
        }
        catch(IdNotFoundException e){
            throw new IdNotFoundException((id));
        }
        String p = ret.getPassword();
        if(!p.equals((password))){
            throw new InvalidValueException("Password incorreta");
        }
        else return ret;
    }


//metodo que dá login de uma empresa e devolve a empresa a qual fez o login
    
    public static Empresa loginEmpresa(Estrutura es, String id, String password) throws IdNotFoundException, InvalidValueException{
        Empresa empresa;

        try{
            empresa = es.getEmpresas().getEmpresa(id);
        }
        catch(IdNotFoundException e){
            throw new IdNotFoundException((id));
        }
        String p = empresa.getPassword();
        if(!p.equals((password))){
            throw new InvalidValueException("Password incorreta");
        }
        else return empresa;
    }


//metodo que dá login de um voluntario e devolve o voluntario o qual fez o login
    
    public static Voluntario loginVoluntario(Estrutura es, String id, String password) throws IdNotFoundException, InvalidValueException{
        Voluntario v;

        try{
            v = es.getVoluntarios().getVoluntario(id);
        }
        catch(IdNotFoundException e){
            throw new IdNotFoundException((id));
        }
        String p = v.getPassword();
        if(!p.equals((password))){
            throw new InvalidValueException("Password incorreta");
        }
        else return v;
    }


    public static Loja loginLoja(Estrutura es, String id, String password) throws IdNotFoundException, InvalidValueException{
        Loja l;

        try{
            l = es.getLojas().getLoja(id);
        }
        catch(IdNotFoundException e){
            throw new IdNotFoundException((id));
        }
        String p = l.getPassword();
        if(!p.equals((password))){
            throw new InvalidValueException("Password incorreta");
        }
        else return l;
    }



    //metodo que regista utilizadores, voluntários, lojas ou empresas no sistema
    
    public static void registarUtilizadores(Estrutura es)
    {
        int tipo;
        Scanner scan = new Scanner(System.in);

        System.out.println("Insira o tipo de Utilizador (1-Comprador, 2-Voluntário, 3-Loja, 4-Empresa):");
        tipo = scan.nextInt();

        if(tipo == 1)
        {
            System.out.println("Insira o nome:");
            String nome = scan.next();

            System.out.println("Insira o email:");
            String email = scan.next();

            System.out.println("Insira o id:");
            String id = scan.next();

            System.out.println("Insira a password:");
            String password = scan.next();

            System.out.println("Insira o nif:");
            int nif = scan.nextInt();

            System.out.println("Insira as suas coordenadas\n");
            System.out.println("Latitude:");
            float latitude = scan.nextFloat();
            System.out.println("Longitude:");
            float longitude = scan.nextFloat();

            List<Encomenda> enc = new ArrayList<>();
            Utilizador u = new Utilizador(id, nome, email, password, nif, latitude, longitude, (ArrayList<Encomenda>) enc);
            try{
                es.getUtilizadores().addUtilizador(u);
            }
            catch(InvalidValueException ex){
                System.out.println(ex.getMessage());
            }
        }

        else if (tipo == 2)
        {
            System.out.println("Insira o nome:");
            String nome = scan.next();

            System.out.println("Insira o email:");
            String email = scan.next();

            System.out.println("Insira o id:");
            String id = scan.next();

            System.out.println("Insira a password:");
            String password = scan.next();

            System.out.println("Insira o nif:");
            int nif = scan.nextInt();

            System.out.println("Insira as suas coordenadas\n");
            System.out.println("Latitude:");
            float latitude = scan.nextFloat();
            System.out.println("Longitude:");
            float longitude = scan.nextFloat();

            System.out.println("Insira o seu raio de recolha de encomendas");
            float raio = scan.nextFloat();

            System.out.println("Pretende transportar encomendas médicas? (true-sim, false-não)");
            boolean encMedicas = scan.nextBoolean();



            Voluntario v = new Voluntario(id, nome, email, password, longitude, latitude,raio, encMedicas, true, 0);
            try{
                es.getVoluntarios().addVoluntario(v);
            }
            catch(InvalidValueException ex){
                System.out.println(ex.getMessage());
            }

        }
        else if (tipo == 3)
        {
            System.out.println("Insira o nome:");
            String nome = scan.next();

            System.out.println("Insira o id:");
            String id = scan.next();

            System.out.println("Insira a password:");
            String password = scan.next();

            System.out.println("Insira as coordenadas da loja\n");
            System.out.println("Latitude:");
            float latitude = scan.nextFloat();
            System.out.println("Longitude:");
            float longitude = scan.nextFloat();

            Map<String,Encomenda> enc = new HashMap<String, Encomenda>();

            Loja l = new Loja(nome, id, password, latitude, longitude, enc, 0);
            try{
                es.getLojas().addLoja(l);
            }
            catch(InvalidValueException ex){
                System.out.println(ex.getMessage());
            }
        }
        else if(tipo == 4)
        {
            System.out.println("Insira o nome:");
            String nome = scan.next();

            System.out.println("Insira o id:");
            String id = scan.next();

            System.out.println("Insira a password:");
            String password = scan.next();

            System.out.println("Insira o nif:");
            int nif = scan.nextInt();

            System.out.println("Insira as coordenadas da empresa\n");
            System.out.println("Latitude:");
            float latitude = scan.nextFloat();
            System.out.println("Longitude:");
            float longitude = scan.nextFloat();

            System.out.println("Insira o seu raio de recolha de encomendas");
            float raio = scan.nextFloat();

            System.out.println("Insira a taxa a cobrar por km");
            float taxa = scan.nextFloat();

            System.out.println("Indique o número máximo de encomendas que pode transportar");
            int n = scan.nextInt();

            System.out.println("Pretende transportar encomendas médicas? (true-sim, false-não)");
            boolean encMedicas = scan.nextBoolean();

            Empresa e = new Empresa(id, nome, password, nif, longitude, latitude, raio, taxa, 0, n, encMedicas, 0, true);

            try{
                es.getEmpresas().addEmpresa(e);
            }
            catch(InvalidValueException ex){
                System.out.println(ex.getMessage());
            }

        }

    }

    //método que imprime cada menu de acordo com as escolhas do utilizador
    public static void menu() throws IdNotFoundException, InvalidValueException {
        Scanner ac = new Scanner(System.in);

        Estrutura es = new Estrutura();
        Utilizador u = null;
        Empresa emp = null;
        Voluntario v = null;
        Loja lo = null;
        String id, p, string_1;

        int flag = 1;
        int choice= -1;
        String produto, desc, loja;
        float preco=0, peso=0;
        double quantidade = 0;



        while(flag != 0){
            imprimirmenu(flag);

            try{
                choice=ac.nextInt();
            }
            catch(InputMismatchException e){
                System.out.println("Insere digitos apenas");
                ac.next();

                continue;
            }

            if (flag == 1){

                switch(choice){
                    case 1:
                        registarUtilizadores(es);

                    case 2:
                        flag = 2;
                        break;

                    case 3:
                        try{
                            System.out.println("Nome Ficheiro : ");
                            string_1=ac.next();
                            es.guardaEstado(string_1);
                        }
                        catch(InputMismatchException e){
                            System.out.println(e.getMessage());
                            break;
                        }
                        catch(FileNotFoundException e){
                            System.out.println(e.getMessage());
                            break;
                        }
                        catch(IOException e){
                            System.out.println(e.getMessage());
                            break;
                        }
                        System.out.println("Gravado com sucesso");
                        break;
                    case 4:
                        try{
                            System.out.println("Nome Ficheiro : ");
                            string_1=ac.next();
                            es.carregaLogs(string_1);
                        }
                        catch(InputMismatchException e){
                            System.out.println(e.getMessage());
                            break;
                        }
                        catch(FileNotFoundException e){
                            System.out.println(e.getMessage());
                            break;
                        }
                        catch(IOException e){
                            System.out.println(e.getMessage());
                            break;
                        }

                        System.out.println("Lido com sucesso");
                        break;


                    case 5:
                        System.out.println("A sair");
                        flag = 0;
                        break;

                    default:
                        System.out.println("Opçao Invalida");
                }

            }

            else if (flag == 2){

                switch(choice){
                    case 1:

                        System.out.println("Id: ");
                        try{
                            id  = ac.next();
                            System.out.println("Password: ");
                            p = ac.next();

                        }
                        catch(InputMismatchException e){
                            System.out.println("Erro no input passado");
                            break;
                        }
                        try{
                            u = loginComprador(es,id,p);
                            es.setIdLogged(id);
                        }
                        catch (InvalidValueException e) {
                            e.printStackTrace();
                        } catch (IdNotFoundException e) {
                            e.printStackTrace();
                        }
                        flag = 3;
                        break;



                    case 2:
                        System.out.println("Id: ");
                        try{
                            id  = ac.next();
                            System.out.println("Password: ");
                            p = ac.next();

                        }
                        catch(InputMismatchException e){
                            System.out.println("Erro no input passado");
                            break;
                        }
                        try{
                            emp = loginEmpresa(es,id,p);
                            es.setIdLogged(id);
                        }
                        catch (InvalidValueException e) {
                            e.printStackTrace();
                        } catch (IdNotFoundException e) {
                            e.printStackTrace();
                        }
                        flag = 4;
                        break;

                    case 3:
                        System.out.println("Id: ");
                        try{
                            id  = ac.next();
                            System.out.println("Password: ");
                            p = ac.next();

                        }
                        catch(InputMismatchException e){
                            System.out.println("Erro no input passado");
                            break;
                        }
                        try{
                            v = loginVoluntario(es,id,p);
                            es.setIdLogged(id);
                        }
                        catch (InvalidValueException e) {
                            e.printStackTrace();
                        } catch (IdNotFoundException e) {
                            e.printStackTrace();
                        }
                        flag = 4;
                        break;

                    case 4:
                        System.out.println("Id: ");
                        try{
                            id  = ac.next();
                            System.out.println("Password: ");
                            p = ac.next();

                        }
                        catch(InputMismatchException e){
                            System.out.println("Erro no input passado");
                            break;
                        }
                        try{
                            lo = loginLoja(es,id,p);
                            es.setIdLogged(id);
                        }
                        catch (InvalidValueException e) {
                            e.printStackTrace();
                        } catch (IdNotFoundException e) {
                            e.printStackTrace();
                        }
                        flag = 5;
                        break;

                    case 5:
                        try{
                            System.out.println("Password do admin: ");
                            string_1 = ac.next();

                        }
                        catch(InputMismatchException e){
                            System.out.println("Erro no input passado");
                            break;
                        }
                        if (string_1.equals(passwordAdmin)){
                            flag = 6;
                        }
                        else{
                            System.out.println("Password errada");
                        }

                        break;

                    case 6:
                        flag = 1;
                        break;
                    default:
                        System.out.println("Opçao Invalida");
                }
            }

            else if (flag == 3){
                switch(choice) {
                    case 1:

                        for(Loja j: es.getLojas().getLojas().values()){
                            System.out.println("Nome: "+ j.getNome() + " Id: "+j.getId() + " Latitude: " + j.getLat() + " Lonngitude "+ j.getLong());
                        }

                        System.out.println("Id da Loja:");
                        loja = ac.next();
                        System.out.println("Id da encomenda:");
                        id = ac.next();
                        int l =0;
                        List<LinhaEncomenda> linhas = new ArrayList<LinhaEncomenda>();
                        while(l == 0) {

                            System.out.println("Nome do produto:");
                            produto = ac.next();
                            System.out.println("Descrição do produto:");
                            desc = ac.next();
                            System.out.println("Quantidade que deseja:");
                            quantidade = ac.nextDouble();
                            System.out.println("Insira o peso");
                            peso = ac.nextFloat();

                            System.out.println("Preco do produto");
                            preco = ac.nextFloat();

                            LinhaEncomenda linha = new LinhaEncomenda(produto, desc, quantidade, preco);
                            linhas.add(linha);
                            System.out.println("Concluir encomenda? (1-Sim, 0-não)");
                            l = ac.nextInt();
                        }
                        Encomenda e = new Encomenda(id, es.getIdLogged(), loja, peso);
                        e.setLinhas((ArrayList<LinhaEncomenda>) linhas);
                        es.inserePedido(e);


                        System.out.println("Encomenda inserida");
                        flag = 3;
                        
                        break;

                    case 2:
                        List<Encomenda> encomendas = new ArrayList<Encomenda>();
                        for(Encomenda en : es.getEncomendas().getEncomendas().values()){
                            if(!en.getaceC() && en.getAce() && en.getEncPronta() && !es.getVoluntarios().getVoluntarios().containsKey(en.getIdTransportador())){
                                encomendas.add(en);
                            }
                        }
                        if(encomendas.size()==0){
                            System.out.println("Não há encomendas disponíveis");
                        } else {
                            for(Encomenda en : encomendas) System.out.println(en.toString());

                            System.out.println("Indique o id da encomenda que pretende aceitar:");
                            id = ac.next();
                            es.aceitaComp(id);
                            System.out.println("Encomenda aceite!");}
                        break;

                    case 3:
                        System.out.println("Insira o id da encomenda pretendida");
                        id = ac.next();
                        Encomenda enc = es.getEncomendas().getEncomenda(id);
                        System.out.println("Insira a classificação do transportador da encomenda:");
                        int cla = ac.nextInt();
                        es.classificaEntrega(enc, cla);
                        System.out.println("Tranportador classificado");
                        break;

                    case 4:
                        System.out.println("Insira o id da encomenda pretendida");
                        id = ac.next();
                        Encomenda la = es.getEncomendas().getEncomenda(id);
                        if(la.getEncPronta() && !la.getaceC() && !la.getAce()){
                            System.out.println("Encomenda pronta na loja para ser entregue");
                        } else if(la.getEncPronta() && la.getAce() && !la.getaceC()){
                            System.out.println("Encomenda à espera de aceitação do cliente para transporte");
                        } else if(la.getEncPronta() && la.getaceC() && la.getAce()){
                            System.out.println("Encomenda a ser transportada");
                        } else System.out.println("Encomenda em processamento na loja");
                        break;

                    case 5:
                        flag = 1;
                        es.setIdLogged(null);
                        break;
                    default:
                        System.out.println("Opçao Invalida");

                }
            }
            else if(flag == 4) {
                switch (choice) {
                    case 1:
                        System.out.println("Insira o tipo de transportador (1-empresa,  2-voluntário)");
                        int t = ac.nextInt();
                        if (t == 1) {
                            boolean d = emp.getDisponivel();
                            System.out.println("A sua disponibilidade é:" + d);
                            System.out.println("Alterar disponibilidade? (0-não, 1-sim)");
                            int k = ac.nextInt();
                            if (k == 0) {
                                break;
                            } else if (k == 1) {
                                if (d == true) d = false;
                                else if (!d) d = true;
                            } else System.out.println("Opção incorreta");

                        } else if (t == 2) {
                            boolean d = v.getDisp();
                            System.out.println("A sua disponibilidade é:" + d);
                            System.out.println("Alterar disponibilidade? (0-não, 1-sim");
                            int k = ac.nextInt();
                            if (k == 0) {
                                break;
                            } else if (k == 1) {
                                if (d == true) d = false;
                                else if (d == false) d = true;
                            } else System.out.println("Opção incorreta");
                        }
                        break;

                    case 2:

                        List<Encomenda> encomendas = new ArrayList<Encomenda>();
                        System.out.println("1-Empresa, 2-Voluntário");
                        int r = ac.nextInt();
                        if (r == 1) {
                            for (Encomenda en : es.getEncomendas().getEncomendas().values()) {
                                if (!en.getaceC() && !en.getAce() && en.getEncPronta() && distanciaRaio(en, es)) {
                                    encomendas.add(en);
                                }
                            }
                            if (encomendas.size() == 0) {
                                System.out.println("Não há encomendas disponíveis");
                            } else {
                                for (Encomenda i : encomendas) System.out.println(i.toString());
                                System.out.println("Indique o id da encomenda que pretende aceitar:");
                                id = ac.next();
                                es.aceitaTransporte(emp.getId(), id);
                                System.out.println("Encomenda aceite!");
                            }
                        } else if(r == 2) {
                            for (Encomenda en : es.getEncomendas().getEncomendas().values()) {
                                if (!en.getaceC() && !en.getAce() && en.getEncPronta() && distanciaRaioVol(en, es)) {
                                    encomendas.add(en);
                                }
                            }
                            if (encomendas.size() == 0) {
                                System.out.println("Não há encomendas disponíveis");
                            } else {
                                for (Encomenda i : encomendas) System.out.println(i.toString());
                                System.out.println("Indique o id da encomenda que pretende aceitar:");
                                id = ac.next();
                                es.aceitaTransporte(v.getId(), id);
                                System.out.println("Encomenda aceite!");
                            }
                        } else System.out.println("Opção inválida");

                        break;


                    case 3:
                        System.out.println("Data inicial");
                        System.out.println("Dia");
                        int dia = ac.nextInt();
                        System.out.println("Mês");
                        int mes = ac.nextInt();
                        System.out.println("Ano");
                        int ano = ac.nextInt();
                        LocalDate i = LocalDate.of(ano, mes, dia);
                        System.out.println("Data final");
                        System.out.println("Dia");
                        int diaf = ac.nextInt();
                        System.out.println("Mês");
                        int mesf = ac.nextInt();
                        System.out.println("Ano");
                        int anof = ac.nextInt();
                        LocalDate f = LocalDate.of(anof, mesf, diaf);
                        System.out.println("Insira o tipo de transportador (1-empresa,  2-voluntário)");
                        int tr = ac.nextInt();
                        if(tr == 1) {
                            float total = es.totalFaturado(emp, i, f);
                            System.out.println("O total faturado é " + total);
                        } else System.out.println("Não possível efetuar esta ação");
                        break;

                    case 4:
                        List<Encomenda> encomendasTransE = new ArrayList<Encomenda>();
                        List<Encomenda> encomendasTransV = new ArrayList<Encomenda>();

                        System.out.println("Insira o tipo de transportador (1-empresa,  2-voluntário)");
                        int trans = ac.nextInt();
                        if(trans == 1) {
                            for (Encomenda x : es.getEncomendas().getEncomendas().values()) {
                                if (x.getIdTransportador().equals(emp.getId())){
                                    encomendasTransE.add(x);
                                    for(Encomenda z : encomendasTransE){
                                    System.out.println(z.toString());}
                                }
                            }
                        }else if(trans == 2){
                            for (Encomenda y : es.getEncomendas().getEncomendas().values()) {
                                if (y.getIdTransportador().equals(v.getId())){
                                    encomendasTransV.add(y);
                                    for(Encomenda encs : encomendasTransV){
                                        System.out.println(encs.toString());}
                                }
                            }
                        } else System.out.println("Opção incorreta");
                        break;

                    case 5:
                        flag = 1;
                        es.setIdLogged(null);
                        break;

                    default:
                        System.out.println("Opçao Invalida");


                }
            }
            else if(flag == 5){
                switch (choice){
                    case 1:

                        List<Encomenda> encomendas = new ArrayList<Encomenda>();
                        for(Encomenda en : es.getLojas().getLoja(es.getIdLogged()).getEnc().values()){
                            if(!en.getaceC() && !en.getAce() && !en.getEncPronta()){
                                encomendas.add(en);
                            }
                        }
                        if(encomendas.size()==0){
                            System.out.println("Não há encomendas disponíveis");
                        } else {
                            for(Encomenda en : encomendas) System.out.println(en.toString());

                            System.out.println("Indique o id da encomenda que pretende aceitar:");
                            id = ac.next();
                            es.encomendaPronta(id);
                            System.out.println("Encomenda aceite!");}
                        break;

                    case 2:
                        flag = 1;
                        es.setIdLogged(null);
                        break;

                    default:
                        System.out.println("Opçao Invalida");

                }
            }
            else if(flag == 6){
                switch (choice){
                    case 1:
                        List<Utilizador> ret;
                        ret = es.top10UtilizadoresAtivos();
                        for(Utilizador ut : ret){
                            System.out.println(ut.getId());
                        }
                        break;

                    case 2 :
                        List<Empresa> a;
                        a = es.top10Empresas();
                        for(Empresa o : a){
                            System.out.println(o.getId());
                        }
                        break;

                    case 3 :
                        flag = 1;
                        break;

                    default:
                        System.out.println("Opçao Invalida");
                }
            }
        }

    }


    public static boolean distanciaRaio(Encomenda e, Estrutura es) throws IdNotFoundException {
        boolean ret = false;
        float latU, latL, latE, longU, longL, longE;
        latU = es.getUtilizadores().getUtilizador(e.getIdComprador()).getLatitude();
        longU = es.getUtilizadores().getUtilizador(e.getIdComprador()).getLongitude();
        latL = es.getLojas().getLoja(e.getIdLoja()).getLat();
        longL = es.getLojas().getLoja(e.getIdLoja()).getLong();
        latE = es.getEmpresas().getEmpresa(es.getIdLogged()).getLatitude();
        longE = es.getEmpresas().getEmpresa(es.getIdLogged()).getLongitude();

        String t = es.getIdLogged();
        float raio = es.getEmpresas().getEmpresa(t).getRaio();


        float lojaTrans = (float) Math.sqrt(Math.pow(latL-latE, 2) + Math.pow(longL-longE,2));
        float transUti = (float) Math.sqrt(Math.pow(latE-latU, 2) + Math.pow(longE-longU,2));
        if(lojaTrans <= raio && transUti <= raio){
            ret = true;
        }

        return  ret;

    }

    public static boolean distanciaRaioVol(Encomenda e, Estrutura es) throws IdNotFoundException {
        boolean ret = false;
        float latU, latL, latE, longU, longL, longE;
        latU = es.getUtilizadores().getUtilizador(e.getIdComprador()).getLatitude();
        longU = es.getUtilizadores().getUtilizador(e.getIdComprador()).getLongitude();
        latL = es.getLojas().getLoja(e.getIdLoja()).getLat();
        longL = es.getLojas().getLoja(e.getIdLoja()).getLong();
        latE = es.getVoluntarios().getVoluntario(es.getIdLogged()).getLatitude();
        longE = es.getVoluntarios().getVoluntario(es.getIdLogged()).getLongitude();

        String t = es.getIdLogged();
        float raio = es.getVoluntarios().getVoluntario(t).getRaio();


        float lojaTrans = (float) Math.sqrt(Math.pow(latL-latE, 2) + Math.pow(longL-longE,2));
        float transUti = (float) Math.sqrt(Math.pow(latE-latU, 2) + Math.pow(longE-longU,2));
        if(lojaTrans <= raio && transUti <= raio){
            ret = true;
        }

        return  ret;

    }

}
