package main;

import Auxiliares.GPS;
import Encomendas.Encomenda;
import Encomendas.EncomendaMedica;
import Encomendas.EncomendaNormal;
import Encomendas.LinhaEncomenda;
import Perfis.*;
import Registos.Registos;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

import static java.lang.System.exit;
import static java.lang.System.in;


public class Main {


    public static void main(String[] args) {


        Registos registo = new Registos();
        parse(registo);
        try {
            if (checkBinVazio()) {
                System.out.println("O ficheiro logs.bin está vazio!");
            } else {
                System.out.println("O ficheiro logs.bin tem informações que foram carregadas!");
                parseBin(registo);
            }
        } catch (IOException | ClassNotFoundException e) {
            System.out.println(e);
            e.printStackTrace();
        }
        final Scanner input = new Scanner(System.in);

        try {
            menuInicial(registo, input);
        } catch (InputMismatchException e) {
            System.out.println("Erro crítico!");
            System.out.println(e.toString());
            main(args);
        }

    }


// -----------------------------------------------     MENU PRINCIPAL    -----------------------------------------------

    private static void menuInicial(Registos r, Scanner input) throws InputMismatchException {

        System.out.println("#############################################################################");
        System.out.println("#                          Bem vindo ao TrazAqui!                           #");
        System.out.println("#                          Selecione a sua opção:                           #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (1) Fazer login na sua conta                                             #");
        System.out.println("#  (2) Registar conta                                                       #");
        System.out.println("#  (3) Informações                                                          #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Sair                                                                 #");
        System.out.println("#############################################################################");


        int escolha = lerInteiro(input);
        input.nextLine();

        while (escolha >= 0 && escolha <= 3) {
            switch (escolha) {
                case 0: {
                    try {
                        escreverBin(r);
                        guardarFicheiro(r);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    System.out.println("Obrigado por utilizar o TrazAqui!");
                    exit(0);
                }

                case 1: {
                    login(r, input);
                    break;
                }
                case 2: {
                    menuCriarConta(r, input);
                    break;
                }
                case 3: {
                    menuInformacoes(r, input);
                    break;
                }

                default: {
                    System.out.println("Opção inexistente\n");
                    menuInicial(r, input);
                    break;
                }
            }
        }
    }


    // ------------------------------------------------------------------------------PARSE-------------------------------------------------------------------------------

    private static boolean checkBinVazio() {
        File bin = new File("logs.bin");
        return bin.length() == 0;
    }


    public static void parse(Registos registos) {
        List<String> linhas = lerFicheiro("LogsGerados.csv");
        String[] linhaPartida;
        ArrayList<Transportadores> aceitesAux = new ArrayList<>();
        int n = 0;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    int max = 0;
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    String semU = u.getCodigoPerfil().replace("u", "");
                    if (max < Integer.parseInt(semU)) {
                        max = Integer.parseInt(semU);
                    }
                    System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                    registos.addPerfil(u);
                    registos.setUtilizador(max);
                    break;

                case "Loja":
                    int maxL = 0;
                    Loja l = parseLoja(linhaPartida[1]);

                    String semL = l.getCodigoPerfil().replace("l", "");
                    if (maxL < Integer.parseInt(semL)) {
                        maxL = Integer.parseInt(semL);
                    }


                    System.out.println(l.toString());
                    registos.addPerfil(l);
                    registos.setLoja(maxL);
                    break;


                case "Voluntario":
                    int maxV = 0;
                    Voluntario v = parseVoluntario(linhaPartida[1]);

                    String semV = v.getCodigoPerfil().replace("v", "");
                    if (maxV < Integer.parseInt(semV)) {
                        maxV = Integer.parseInt(semV);
                    }

                    System.out.println(v.toString());
                    registos.addPerfil(v);
                    registos.setVoluntario(maxV);
                    aceitesAux.add(v);
                    break;


                case "Transportadora":

                    int maxT = 0;
                    Empresa t = parseEmpresa(linhaPartida[1]);

                    String semT = t.getCodigoPerfil().replace("t", "");
                    if (maxT < Integer.parseInt(semT)) {
                        maxT = Integer.parseInt(semT);
                    }

                    System.out.println(t.toString());
                    registos.addPerfil(t);
                    registos.setEmpresa(maxT);
                    aceitesAux.add(t);
                    break;


                case "Encomenda":


                    int maxE = 0;
                    Encomenda e = parseEncomenda(linhaPartida[1], registos);

                    String semE = e.getCodEncomenda().replace("e", "");
                    if (maxE < Integer.parseInt(semE)) {
                        maxE = Integer.parseInt(semE);
                    }

                    System.out.println(e.toString());

                    registos.getPerfil(e.getCodUtilizador()).adicionaAoHistorico(e);
                    registos.getPerfil(e.getCodLoja()).adicionaAoHistorico(e);
                    registos.addEncomenda(e);
                    registos.setEncomenda(maxE);
                    break;

                case "Aceite":
                    if (n < aceitesAux.size()) {
                        String codEncomenda = linhaPartida[1];//Vai buscar código de encomenda
                        Encomenda encAux = registos.buscaEncomenda(codEncomenda);//Vai buscar a encomenda a partir do código
                        Transportadores transAux = aceitesAux.get(n);//Vai buscar um transportador a partir da lista de transportadores lidos no parse
                        transAux.addAceite(encAux);//Adiciona a encomenda ao historico de aceites do transportador
                        transAux.adicionaAoHistorico(encAux);//Adiciona a encomenda ao historico total do transportador
                        transAux.entregaRealizada(registos);//Cria uma viagem
                        transAux.atualizaViagens(codEncomenda);
                        encAux.setCodTransportadora(transAux.getCodigoPerfil());//Atualiza a encomenda com o transportador
                        encAux.setEntregue(true);//Atualiza a encomenda com o estado Entregue
                        n++;
                    } else {
                        n = 0;
                        String codEncomenda = linhaPartida[1];//Vai buscar código de encomenda
                        Encomenda encAux = registos.buscaEncomenda(codEncomenda);//Vai buscar a encomenda a partir do código
                        Transportadores transAux = aceitesAux.get(n);//Vai buscar um transportador a partir da lista de transportadores lidos no parse
                        transAux.addAceite(encAux);//Adiciona a encomenda ao historico de aceites do transportador
                        transAux.adicionaAoHistorico(encAux);//Adiciona a encomenda ao historico total do transportador
                        transAux.entregaRealizada(registos);//Cria uma viagem
                        transAux.atualizaViagens(codEncomenda);
                        encAux.setCodTransportadora(transAux.getCodigoPerfil());//Atualiza a encomenda com o transportador
                        encAux.setEntregue(true);//Atualiza a encomenda com o estado Entregue
                        n++;
                    }

                    break;
                default:
                    System.out.println(linha);
                    System.out.println("Linha inválida.");
                    break;
            }
        }
        System.out.println("Foram lidos os registos guardados no ficheiro LogsGerados.csv");
    }

    private static void parseBin(Registos registo) throws IOException, ClassNotFoundException, EOFException {
        File bin = new File("logs.bin");
        bin.createNewFile();

        FileInputStream fis = new FileInputStream("logs.bin");
        ObjectInputStream ois = new ObjectInputStream(fis);


        HashMap<String, Perfil> perfisRead = (HashMap<String, Perfil>) ois.readObject();
        HashMap<String, String> emailsRead = (HashMap<String, String>) ois.readObject();
        HashMap<String, Encomenda> encomendasRead = (HashMap<String, Encomenda>) ois.readObject();
        HashMap<String, ArrayList<Transportadores>> vaiRecolherRead = (HashMap<String, ArrayList<Transportadores>>) ois.readObject();

        ArrayList<Integer> paraGuardar = (ArrayList<Integer>) ois.readObject();

        int lojaRead = paraGuardar.get(0);
        int encomendaRead = paraGuardar.get(1);
        int utilizadorRead = paraGuardar.get(2);
        int voluntarioRead = paraGuardar.get(3);
        int empresaRead = paraGuardar.get(4);
        int produtosRead = paraGuardar.get(5);


        registo.atualizaRegistos(perfisRead, emailsRead, encomendasRead, vaiRecolherRead, lojaRead, encomendaRead, utilizadorRead, voluntarioRead, empresaRead, produtosRead);

        ois.close();
        fis.close();

    }


    private static void escreverBin(Registos r) throws IOException {
        File bin = new File("logs.bin");

        try {
            bin.createNewFile();

            FileOutputStream fos = new FileOutputStream("logs.bin", false);
            ObjectOutputStream oos = new ObjectOutputStream(fos);


            //Guarda os HashMaps
            oos.writeObject(r.getPerfis());
            oos.writeObject(r.getEmails());
            oos.writeObject(r.getEncomendas());
            oos.writeObject(r.getVaiRecolher());

            //Guarda os ints

            ArrayList<Integer> paraGuardar = new ArrayList<>();

            paraGuardar.add(r.getLoja());
            paraGuardar.add(r.getEncomenda());
            paraGuardar.add(r.getUtilizador());
            paraGuardar.add(r.getVoluntario());
            paraGuardar.add(r.getEmpresa());
            paraGuardar.add(r.getProdutos());


            oos.writeObject(paraGuardar);


            oos.close();
            fos.close();
            System.out.println("Os registos foram guardados no ficheiro logs.bin!");


        } catch (IOException e) {
            System.out.println("Erro, não consigo guardar ficheiro!");
        }
    }


    private static Encomenda parseEncomenda(String input, Registos r) {


        ArrayList<LinhaEncomenda> linhaEncomenda = new ArrayList<>();


        // campos[e25,u52,l123,pe,llllll
        String[] campos = input.split(",");
        int tam = (campos.length);
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        HashMap<String, LinhaEncomenda> produtos = new HashMap<>();
        int maxP = 0;

        int n = 4;
        int i = 0;
        while (n < tam) {
            while (i <= 3) {

                LinhaEncomenda nova = new LinhaEncomenda();
                String codProduto = campos[n + i];
                nova.setCodProduto(codProduto);

                String semP = codProduto.replace("p", "");
                if (maxP < Integer.parseInt(semP)) {
                    maxP = Integer.parseInt(semP);
                }

                i++;
                nova.setNome(campos[n + i]);
                i++;
                nova.setQuantidade(Double.parseDouble(campos[n + i]));
                i++;
                nova.setPrecoUnit(Double.parseDouble(campos[n + i]));
                i++;
                linhaEncomenda.add(nova);
                produtos.put(nova.getCodProduto(), nova);
            }

            i = 0;
            n = n + 4;

            //System.out.println("LINHA ENCOMENDA: " + linhaEncomenda);


        }
        //public Encomenda(String codTransportadora, String codLoja, String codEncomenda, String codUtilizador, double peso, ArrayList<LinhaEncomenda> listaEnc, LocalDateTime horaPedido, boolean entregue) {

        Loja l = r.getPerfilLoja(codLoja);
        l.setProdutos(produtos);

        r.setProdutos(maxP);
        r.atualizaPerfil(l);
        return new EncomendaNormal("", codLoja, codEncomenda, codUtilizador, peso, linhaEncomenda, LocalDateTime.now(), false);


    }

    private static Empresa parseEmpresa(String input) {

        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[5]);
        String nif = campos[4];
        double precoKM = Double.parseDouble(campos[6]);
        String email = codEmpresa + "@trazaqui.pt";

        GPS novo = new GPS(gpsx, gpsy);


        //(String codigoPerfil,GPS coordenadas, String nome, String email,
        // String pass,Boolean prontoARecolher,Boolean certificado,ArrayList<Viagem> trips,
        // Double kmsPercorridos, int nif, double raio, double taxaKm, int capacidade, int classificacoes,
        // double classificacaoFinal,ArrayList<Encomenda> porEntregar, ArrayList<Encomenda> encomendasAceites) {

        return new Empresa(codEmpresa, novo, nome, email, codEmpresa, false, false, new ArrayList<>(), 0.0, nif, raio, precoKM, 1, 0, 0.0, new ArrayList<>(), new ArrayList<>());
    }


    private static Voluntario parseVoluntario(String input) {

        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Double raio = Double.parseDouble(campos[4]);
        String email = codVoluntario + "@trazaqui.pt";

        GPS novo = new GPS(gpsx, gpsy);


        //String codigoPerfil,GPS coordenadas, String nome, String email, String pass,
        // Double raio, Integer classificacoes, Double classificacaoFinal, Boolean prontoARecolher,
        // Boolean certificado, ArrayList<Viagem> trips,Double kmsPercorridos,Integer capacidade,
        // ArrayList<Encomenda> porEntregar,ArrayList<Encomenda> encomendasAceites

        return new Voluntario(codVoluntario, novo, nome, email, codVoluntario, raio, 0, 0.0, false, false, new ArrayList<>(), 0.0, 1, new ArrayList<>(), new ArrayList<>());

    }


    public static Utilizador parseUtilizador(String input) {
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        String email = codUtilizador + "@trazaqui.pt";
        GPS novo = new GPS(gpsx, gpsy);

        //String codigoPerfil,GPS coordenadas,String nome,String email,String pass
        return new Utilizador(codUtilizador, novo, nome, email, codUtilizador);

    }

    public static Loja parseLoja(String input) {
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        String email = codLoja + "@trazaqui.pt";
        GPS novo = new GPS(gpsx, gpsy);

        //( ArrayList<Encomenda> encomendasEmFila, HashMap<String, LinhaEncomenda> produtos


        return new Loja(codLoja, novo, nomeLoja, email, codLoja, new ArrayList<>(), new HashMap<>());
    }

    public static List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }

    public static void guardarFicheiro(Registos r) throws IOException, FileNotFoundException {

        File csv = new File("LogsGerados.csv");
        csv.createNewFile();


        PrintWriter pw = new PrintWriter("LogsGerados.csv");

        HashMap<String, Perfil> perfis = r.getPerfis();
        HashMap<String, Encomenda> encomendas = r.getEncomendas();
        HashMap<String, ArrayList<Transportadores>> vaiRecolher = r.getVaiRecolher();


        for (Perfil p : perfis.values()) {

            if (p instanceof Utilizador) {
                pw.println("Utilizador:" + ((Utilizador) p).toStringGuardar());
            } else if (p instanceof Voluntario) {
                pw.println("Voluntario:" + ((Voluntario) p).toStringGrava());
            } else if (p instanceof Empresa) {
                pw.println("Transportadora:" + ((Empresa) p).toStringGrava());
            } else if (p instanceof Loja) {
                pw.println("Loja:" + ((Loja) p).toStringGrava());
            }
        }

        for (Encomenda e : encomendas.values()) {
            if (e.getEntregue()) {
                pw.println("Encomenda:" + e.toStringGrava());
                pw.println("Aceite:" + e.getCodEncomenda());
            } else {
                pw.println("Encomenda:" + e.toStringGrava());
            }
        }
        pw.flush();
        pw.close();
    }


    //---------------------------------------------------------------------    MENUS --------------------------------------------------------------------------------------


    private static void login(Registos r, Scanner input) throws InputMismatchException {

        String mail;
        String pass;

        System.out.println("Indique o seu Email: \n");
        mail = input.nextLine();

        System.out.println("Indique a sua password: \n");
        pass = input.nextLine();

        if (r.loginCorreto(mail, pass)) {

            System.out.println(" \n | Login efetuado com sucesso! | \n ");

            Perfil p = r.getPerfil(r.codigoPerfil(mail));

            if (p instanceof Utilizador) menuUtilizador((Utilizador) p, r, input);
            if (p instanceof Empresa) menuEmpresa((Empresa) p, r, input);
            if (p instanceof Voluntario) menuVoluntario((Voluntario) p, r, input);
            if (p instanceof Loja) menuLoja((Loja) p, r, input);
        } else {
            System.out.println("Email e/ou password incorretos. Tente novamente! \n");
            menuInicial(r, input);
        }
    }


// -----------------------------------------------     MENU UTILIZADOR     -----------------------------------------------

    public static void menuUtilizador(Utilizador u, Registos r, Scanner input) throws InputMismatchException {

        int opcao;

        System.out.println("#############################################################################");
        System.out.println("#                          Bem vindo ao TrazAqui!                           #");
        System.out.println("#                          Selecione a sua opção:                           #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (1) O Meu Perfil                                                         #");
        System.out.println("#  (2) Fazer Encomenda                                                      #");
        System.out.println("#  (3) Ver Histórico                                                        #");
        System.out.println("#  (4) Classificar Transportador                                            #");
        System.out.println("#  (5) Estado da Encomenda                                                  #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Sair da conta                                                        #");
        System.out.println("#############################################################################");

        opcao = lerInteiro(input);
        input.nextLine();

        while (opcao >= 0 && opcao <= 5) {
            switch (opcao) {

                // OPCAO: O MEU PERFIL - Informacoes sobre o Perfil Utilizador
                case 1: {
                    System.out.println(u.toString());
                    menuUtilizador(u, r, input);
                    break;
                }

                // OPCAO: FAZER ENCOMENDA
                case 2: {
                    if( u.getHistorico().isEmpty() ||(u.getHistorico().get((u.getHistorico().size()) - 1).getEntregue().equals(true))){
                        ArrayList<LinhaEncomenda> le = new ArrayList<>();
                        String codeL = "";
                        menuUtilizadorAuxiliar(u, r, le, codeL, input,false);
                    }
                    else {
                        System.out.println("Só poderá fazer uma nova encomenda quando a sua última for entregue. Por favor volte mais tarde.\n");
                        menuUtilizador(u, r, input);
                    }
                    break;
                }


                // OPCAO VER HISTORICO
                case 3: {

                    System.out.println("#############################################################################");
                    System.out.println("#  (1) Ver histórico todo                                                   #");
                    System.out.println("#  (2) Ver parte do histórico                                               #");
                    System.out.println("#   ---------------------------------------------------------------------   #");
                    System.out.println("#  (0) Voltar ao Menu                                                       #");
                    System.out.println("#############################################################################");

                    int op = lerInteiro(input);
                    input.nextLine();

                    while (op >= 0 && op <= 2) {
                        switch (op) {

                            // OPCAO: VOLTA AO MENU - Volta para o MenuUtilizador
                            case 0: {
                                menuUtilizador(u, r, input);
                                break;
                            }

                            // OPCAO: VER HISTORICO TOODO
                            case 1: {
                                if (u.getHistorico().isEmpty()) {
                                    System.out.println("O seu histórico está vazio.\n");
                                    menuUtilizador(u, r, input);
                                    break;
                                } else {
                                    u.printHistorico();
                                    menuUtilizador(u, r, input);
                                    break;
                                }
                            }

                            // OPCAO: VER PARTE DO HISTORICO
                            case 2: {
                                if (u.getHistorico().isEmpty()) {
                                    System.out.println("O seu histórico está vazio.\n");
                                    menuUtilizador(u, r, input);
                                    break;
                                } else {
                                    LocalDate dataDesde = lerDataDesde(input);
                                    LocalDate dataAte = lerDataAte(input);
                                    u.historicoPeriodo(dataDesde, dataAte);
                                    menuUtilizador(u, r, input);
                                    break;
                                }
                            }

                            // caso nao e um numero entre as opcoes
                            default: {
                                System.out.println("Opção inexistente\n");
                                menuInicial(r, input);
                                break;
                            }
                        }
                    }
                    break;
                }


                //OPCAO: CLASSIFICAR TRANSPORTADORA
                case 4: {
                    if (u.buscarTransportadora(r).equals(true)) {
                        System.out.println("Ainda não tem transportadora para classificar.\n");
                        menuUtilizador(u, r, input);
                        break;
                    } else {
                        System.out.println("Indique o valor de 0-10 com o qual deseja classificar o último transportador:");
                        int c = lerInteiro(input);
                        input.nextLine();
                        while (!Integer.toString(c).equals("")) {
                            if ((0 > c) || (c > 10)) {
                                System.out.println("Input inválido, tente novamente\n");
                                c = lerInteiro(input);
                                input.nextLine();
                            } else {
                                String codigoTransportadora = u.getHistorico().get((u.getHistorico().size()) - 1).getCodTransportadora();
                                r.classificarTransportadores(c, codigoTransportadora, u.getHistorico().get(u.getHistorico().size() - 1));
                                ((Transportadores) (r.getPerfil(codigoTransportadora))).atualizaViagens(u.getHistorico().get((u.getHistorico().size()) - 1).getCodEncomenda());
                                menuUtilizador(u, r, input);
                                break;
                            }
                        }
                    }
                }

                //OPCAO: ESTADO DA ENCOMENDA - Ve o estado da encomenda
                case 5: {
                    if (u.getHistorico().isEmpty()) {
                        System.out.println("Não tem encomendas em espera. Por favor tente mais tarde.");
                        menuUtilizador(u, r, input);

                    } else{
                        Encomenda enc = u.getHistorico().get(u.getHistorico().size() - 1); // ultima encomenda realizada

                        menuUtilizadorEncomenda(u, r, enc);
                        ArrayList<Transportadores> transParaEstaEnc = r.getVaiRecolher().get(enc.getCodEncomenda());// ArrayList de todas as transportadoras que querem entregar esta encomenda

                        int op = lerInteiro(input);
                        input.nextLine();

                        while (op >= 0 && op <= 2) {
                            switch (op) {

                                // OPCAO: VOLTAO AO MENU - Volta para o menu de utilizador
                                case 0: {
                                    menuUtilizador(u, r, input);
                                    break;
                                }

                                // OPCAO: VER ESTADO DA ULTIMA ENCOMENDA - Verifica o estado da encomenda (Se ja tem transportadora ou nao)
                                case 1: {
                                    if (transParaEstaEnc.isEmpty()) {
                                        System.out.println("Ainda não temos um transportador para entregar a sua encomenda.\n");
                                        menuUtilizador(u, r, input);
                                        break;
                                    }
                                    else {
                                        if (!transParaEstaEnc.isEmpty() && enc.getCodTransportadora().equals("")) {
                                            System.out.println("A sua encomenda já tem transportadores para a entregar, escolha um.\n");
                                        } else {
                                            if (enc.getEntregue()) {
                                                System.out.println("A sua última encomenda já foi entregue.\n"); // o transportado
                                            } else {
                                                System.out.println("A sua encomenda vai a caminho.\n");
                                            }
                                        }
                                    }
                                    menuUtilizador(u, r, input);
                                    break;
                                }

                                // OPCAO: ESCOLHER TRANSPORTADORA - O utilizador escolhe a transportadora que quer
                                case 2: {
                                    if (transParaEstaEnc.isEmpty()) { // para o caso de ainda nao ter feito uma encomenda ou de nao ter quem a quer entregar
                                        System.out.println("Infelizmente ainda não é possível escolher uma transportadora.\n");
                                        menuUtilizador(u, r,input);
                                        break;
                                    }
                                    else {
                                        ArrayList<Voluntario> voluntarios = r.verificarVoluntarioEncomenda(enc);
                                        ArrayList<Empresa> empresas = r.verificarEmpresaEncomenda(enc);
                                        if (!voluntarios.isEmpty()) { // caso haja voluntarios para realizar a entrega da
                                            String codigoVoluntario = voluntarios.get(0).getCodigoPerfil();

                                            u.atualizaHistorico(enc, codigoVoluntario, false);// atualiza historico e a encomenda ao mesmo tempo
                                            for (Voluntario v : voluntarios) { // remove a encomenda no porEntregar de todas as empresas candidatas a esta encomenda
                                                String codI = v.getCodigoPerfil();
                                                ArrayList<Encomenda> old = new ArrayList<Encomenda>(((Voluntario) r.getPerfil(codI)).getPorEntregar());
                                                old.removeIf(x -> x.getCodEncomenda().equals(enc.getCodEncomenda()));
                                                ((Voluntario) r.getPerfil(codI)).setPorEntregar(old);

                                            }
                                            if (!empresas.isEmpty()) {
                                                for (Empresa e : empresas) { // remove a encomenda no porEntregar de todas as empresas candidatas a esta encomenda
                                                    String codI = e.getCodigoPerfil();
                                                    ArrayList<Encomenda> old = new ArrayList<Encomenda>(((Empresa) r.getPerfil(codI)).getPorEntregar());
                                                    old.removeIf(x -> x.getCodEncomenda().equals(enc.getCodEncomenda()));
                                                    ((Empresa) r.getPerfil(codI)).setPorEntregar(old);

                                                }
                                            }
                                            Voluntario v = (Voluntario) r.getPerfil(codigoVoluntario);
                                            v.adicionaAoHistorico(enc);
                                            r.getVaiRecolher().remove(enc.getCodEncomenda());// // Remove (enc, arrayLista<Transportadores>) pois a encomenda ja tem quem a vai transportar
                                            ArrayList<Encomenda> aceites = ((Voluntario) r.getPerfil(codigoVoluntario)).getEncomendasAceites(); // encomendas aceites que o voluntario pode fazer
                                            aceites.add(enc); // adiciona a nova encomenda
                                            ((Voluntario) r.getPerfil(codigoVoluntario)).setEncomendasAceites(aceites); // atualiza as encomendas aceites
                                            ((Loja)r.getPerfil(enc.getCodLoja())).removeEncomendasEmFila(enc); // removeu a encomenda do encomendasEmFila da loja

                                            System.out.println("Já tem um voluntário a buscar o seu pedido, não terá custos.\n");
                                        }
                                        else {
                                            r.toStringEmpresasVaiRevolher(empresas, enc); // display de todas as empresas que querem efetuar a entrega da encomenda especifica com o seu preco

                                            System.out.println("Indique a empresa que quer que faça a sua entrega:\n");
                                            String nomeEmpresa = input.nextLine();
                                            String codigoEmpresa = r.codigoEmpresa(nomeEmpresa);
                                            while (codigoEmpresa.isEmpty()) { // caso escreva mal o nome da Empresa
                                                System.out.println("Oops, erro no input, por favor tente novamente.\n");
                                                nomeEmpresa = input.nextLine();
                                                codigoEmpresa = r.codigoEmpresa(nomeEmpresa);
                                            }
                                            u.atualizaHistorico(enc, codigoEmpresa, false);// atualiza historico e a encomenda ao mesmo tempo
                                            Empresa e = (Empresa) r.getPerfil(codigoEmpresa);
                                            e.adicionaAoHistorico(enc); // poe a encomenda no historico de encomendas da empresa

                                            for (Empresa i : empresas) { // remove  a encomenda no porEntregar de todas as empresas candidatas a esta encomenda
                                                String codI = i.getCodigoPerfil();
                                                ArrayList<Encomenda> old = new ArrayList<Encomenda>(((Empresa) r.getPerfil(codI)).getPorEntregar());
                                                old.removeIf(x -> x.getCodEncomenda().equals(enc.getCodEncomenda()));
                                                ((Empresa) r.getPerfil(codI)).setPorEntregar(old);
                                            }

                                            r.getVaiRecolher().remove(enc.getCodEncomenda()); // Remove (enc, arrayLista<Transportadores>) pois a encomenda ja tem quem a vai transportar
                                            ArrayList<Encomenda> aceites = ((Empresa) r.getPerfil(codigoEmpresa)).getEncomendasAceites();
                                            aceites.add(enc);
                                            ((Empresa) r.getPerfil(codigoEmpresa)).setEncomendasAceites(aceites); // atualiza as encomendas aceites
                                            System.out.println("A empresa " + r.getPerfil(codigoEmpresa).getNome() + " efetuará a entrega com um custo de " + ((Empresa) r.getPerfil(codigoEmpresa)).calculaPrecoTransporte(enc, r));
                                            ((Loja)r.getPerfil(enc.getCodLoja())).removeEncomendasEmFila(enc); // removeu a encomenda do encomendasEmFila da loja
                                        }
                                        menuUtilizador(u, r, input);
                                        break;
                                    }
                                }

                                // caso nao e um numero entre as opcoes
                                default: {
                                    System.out.println("Opção inexistente\n");
                                    menuInicial(r, input);
                                    break;
                                }
                            }
                        }
                    }
                }
                // OPCAO: SAIR DA CONTA - Volta para o Menu Principal
                case 0: {
                    menuInicial(r, input);
                    break;
                }

                // quando nao e um numero que esteja dentro das opcoes
                default: {
                    System.out.println("Opção inexistente\n");
                    menuInicial(r, input);
                    break;
                }
            }
        }
    }

// MENU UTILIZADOR ACABA AQUI


    public static void menuUtilizadorEncomenda(Utilizador u, Registos r,Encomenda e) throws InputMismatchException{
        System.out.println("#############################################################################");
        System.out.println("#  (1) Ver estado da ultima encomenda                                       #");
        System.out.println("#  (2) Escolher Transportadora                                              #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Voltar ao Menu                                                       #");
        System.out.println("#############################################################################");
    }

    public static void menuUtilizadorAuxiliar(Utilizador u, Registos r,ArrayList<LinhaEncomenda> le, String codeL,Scanner input, boolean especial) throws InputMismatchException{
        System.out.println("#############################################################################");
        System.out.println("#  (1) Escolher Loja                                                        #");
        System.out.println("#  (2) Ver produtos da Loja                                                 #");
        System.out.println("#  (3) Adicionar produto à encomenda                                        #");
        System.out.println("#  (4) Remover produto da encomenda                                         #");
        System.out.println("#  (5) Tipo de Encomenda                                                    #");
        System.out.println("#  (6) Completar encomenda                                                  #");
        System.out.println("#  (7) Ver Encomenda                                                        #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Voltar ao Menu                                                       #");
        System.out.println("#############################################################################");


        String nomeProd = "";
        double quant = 0.0;

        String nomeLoja = "";

        int op = lerInteiro(input);
        input.nextLine();


        while (op >= 0 && op <= 7) {
            switch (op) {

                // Volta ao inicio do menuUtilizador
                case 0: {
                    menuUtilizador(u, r,input);
                    break;
                }

                // OPCAO: ESCOLHER LOJA - Ver as lojas abrangidas pela aplicacao
                case 1: {
                    System.out.println("Estas são as lojas abrangidas pelos nossos serviços: \n");
                    System.out.println(r.toStringPerfisLojas());
                    if(r.toStringPerfisLojas().isEmpty()){
                        System.out.println("Não existem lojas disponíveis na aplicação, por favor tente mais tarde!");
                        menuUtilizador(u,r,input);
                        break;
                    }
                    System.out.println("Escolha a loja a qual quer fazer a sua encomenda: \n");
                    nomeLoja = input.nextLine();
                    codeL = r.codigoLoja(nomeLoja);
                    while(codeL.isEmpty()) { // para o caso de escrever mal o nome
                        System.out.println("Oops, nome errado.\n");
                        nomeLoja = input.nextLine();
                        codeL = r.codigoLoja(nomeLoja);
                    }
                    System.out.println("A loja que escolheu foi " + nomeLoja + ".\n");
                    menuUtilizadorAuxiliar(u, r, le,codeL,input,especial);// NOVO MENU PARA FAZER A ENCOMENDA
                    break;
                }


                // OPCAO: VER PRODUTOS DA LOJA - Fornece o nome de todos os produtos
                case 2: {
                    if (codeL.equals("")) {
                        System.out.println("Ainda não escolheu a loja. Por favor escolha a loja a qual quer fazer a sua encomeda.\n");
                    } else {
                        System.out.println("Estes são os produtos disponiveis na loja: \n");
                        System.out.println(((Loja) (r.getPerfil(codeL))).toStringProdutos()); // nome dos produtos
                    }
                    menuUtilizadorAuxiliar(u, r, le,codeL,input,especial);
                    break;

                }

                // OPCAO: ADICIONAR PRODUTO A ENCOMENDA
                case 3: {
                    if(codeL.equals("")){
                        System.out.println("Ainda não escolheu a loja a qual quer fazer a encomenda!\n");
                    }
                    else {
                        System.out.println("Indique o produto que quer adicionar à sua encomenda:\n");
                        nomeProd = input.nextLine();
                        String codigoProduto = ((Loja) (r.getPerfil(codeL))).getCodFromNome(nomeProd);
                        while (codigoProduto.isEmpty()) { // caso escreva mal o nome do produto ou o nao esteja na lista de produtos da loja
                            System.out.println("Oops, erro no input, por favor tente novamente\n");
                            nomeProd = input.nextLine();
                            codigoProduto = ((Loja) (r.getPerfil(codeL))).getCodFromNome(nomeProd);
                        }

                        System.out.println("Indique quantas embalagens que quer deste produto:\n");
                        quant = lerInteiro(input);
                        input.nextLine();
                        // Nos nao queremos saber da quantidade de cada produto na loja
                        ((Loja) (r.getPerfil(codeL))).atualizaProdutos(codigoProduto, quant);// atualiza-se na loja a quantidade que o utilizador quer e assim e so por no arrayList
                        le.add(((Loja) (r.getPerfil(codeL))).getProdutos().get(codigoProduto)); // adiciona o produto ao ArrayList le

                    }
                    menuUtilizadorAuxiliar(u, r, le,codeL,input,especial);
                    break;
                }

                // OPCAO: REMOVER O PRODUTO DA ENCOMENDA
                case 4: {
                    if (le.isEmpty()) {
                        System.out.println("Não existe produtos nesta encomenda para remover!");
                    } else {
                        System.out.println("Indique o produto que quer remover:");
                        nomeProd = input.nextLine();
                        String codigoProduto = ((Loja) (r.getPerfil(codeL))).getCodFromNome(nomeProd);
                        while (codigoProduto.isEmpty()) { // caso escreva mal o nome do produto ou o nao esteja na lista de produtos da loja
                            System.out.println("Oops, erro no input, por favor tente novamente\n");
                            nomeProd = input.nextLine();
                        }
                        le.remove(((Loja) (r.getPerfil(codeL))).getProdutos().get(codigoProduto));
                    }
                    menuUtilizadorAuxiliar(u, r, le,codeL,input,especial);
                    break;
                }

                // OPCAO: TIPO DE ENCOMENDA
                case 5:{
                    System.out.println("Escolha o tipo de encomenda:      (1)Normal      (2)Médica\n");
                    int tipo = lerInteiro(input);
                    while(tipo < 1 || tipo > 2){
                        System.out.println("Opção inválida. Tente novamente.\n");
                        tipo = lerInteiro(input);
                    }

                    especial = tipo != 1;

                    menuUtilizadorAuxiliar(u, r, le,codeL,input,especial);
                    break;
                }


                // OPCAO: COMPLETAR - Acabar a encomenda
                case 6: {
                    Encomenda encomenda;

                    if (le.isEmpty()) {
                        System.out.println("Não pode acabar uma encomenda vazia!\n");
                        menuUtilizadorAuxiliar(u, r, le,codeL,input,especial);
                    } else {
                        if(especial == true){
                            encomenda = new EncomendaMedica();
                        }
                        else{
                            encomenda = new EncomendaNormal();
                        }

                        String codEncomenda = "e" + (r.getEncomenda() + 1); // Precisa de estar aqui
                        r.setEncomenda(r.getEncomenda() + 1);
                        encomenda.setCodEncomenda(codEncomenda); // poe o codigo da encomenda
                        encomenda.setCodUtilizador(u.getCodigoPerfil()); // poe o codigo do utilizador que fez o pedido de encomenda
                        encomenda.setCodLoja(codeL); // poe o codigo da loja escolhida pelo utilizador
                        encomenda.setLinhaEncomenda(le); // coloca os produtos escolhidos na encomenda
                        encomenda.pesoTotal(); // poe o peso total da encomenda depois de ter os produtos
                        encomenda.setHoraPedido(LocalDateTime.now()); // regista o tempo em que se efetuou o pedido de encomenda

                        u.atualizaHistorico(encomenda,"",false);// atualiza historico e a encomenda ao mesmo tempo

                        r.adicionaEncomenda(encomenda);// adiciona a encomenda aos registos de encomendas

                        r.atualizaVaiRecolher(codEncomenda,""); // Adiciona a encomenda sem transportador ao vaiRevolher (HashMap de encomendas para transportar)

                        ((Loja) (r.getPerfil(codeL))).adicionaEncomendasEmFila(encomenda); // adiciona a encomenda em fila para os transportadores

                        System.out.println("Pedido de encomenda realizado!\n Não se esqueça de ver o seu estado de encomenda para verificar se já tem transportadores disponíveis para sí.\n");
                        menuUtilizador(u, r,input);// volta para o menu principal do utilizador
                    }
                    break;

                }

                // OPCAO: VER ENCOMENDA - ve os produtos que ela tem ate agora
                case 7:{
                    if(le.isEmpty()){
                        System.out.println("Ainda não tem uma encomenda criada.\n");
                    }
                    else{
                        System.out.println("Esta é a sua encomenda até agora: \n");
                        System.out.println(le.toString());
                    }
                    menuUtilizadorAuxiliar(u,r,le,codeL,input,especial);
                    break;
                }

                // caso nao e um numero entre as opcoes
                default:{
                    System.out.println("Opção inexistente\n");
                    menuUtilizadorAuxiliar(u,r,le,codeL,input,especial);
                    break;
                }
            }break;
        }
    }



// -----------------------------------------------     MENU EMPRESA     -----------------------------------------------

    public static void menuEmpresa(Empresa e, Registos r, Scanner input) throws InputMismatchException{

        int opcao;

        System.out.println("#############################################################################");
        System.out.println("#                          Bem vindo ao TrazAqui!                           #");
        System.out.println("#                          Selecione a sua opção:                           #");
        if(e.getProntoARecolher().equals(true))
        {
            System.out.println("#                      Neste momento está de serviço                        #");
        }
        else{
            System.out.println("#                      Neste momento está fora de serviço                   #");
        }
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (1) O Meu Perfil                                                         #");
        System.out.println("#  (2) Procurar serviço                                                     #");
        System.out.println("#  (3) Verificar  serviço                                                   #");
        System.out.println("#  (4) Ver Faturação                                                        #");
        System.out.println("#  (5) Ver Histórico                                                        #");
        System.out.println("#  (6) Ativar/Desativar Serviço                                             #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Sair da conta                                                        #");
        System.out.println("#############################################################################");

        opcao = lerInteiro(input);
        input.nextLine();

        while (opcao >= 0 && opcao <= 6) {
            switch (opcao) {

                // OPCAO: SAIR DA CONTA - Volta para o Menu Principal
                case 0: {
                    menuInicial(r,input);
                    break;
                }


                // OPCAO: O MEU PERFIL - Informacoes sobre o Perfil Empresa
                case 1: {
                    System.out.println(e.toString());
                    menuEmpresa(e, r, input);
                    break;
                }

                // OPCAO: PROCURAR SERVICO -  Fornecer as encomendas que existem para entregar no seu raio de acao
                case 2: {
                    if (e.getProntoARecolher().equals(false)) { // O servico esta desativado, nao pode efetuar nenhuma encomenda
                        System.out.println("De momento não se encontra com o serviço ativo.\n");
                        menuEmpresa(e, r, input);
                    } else {
                        ArrayList<Encomenda> paraEntrega = new ArrayList<>();
                        // a Transportadora faz as encomendas aceites de uma so vez, mesmo que o arrayList encomendasAceites nao esteja completo
                        menuTransportadorAuxiliar(e, r, paraEntrega,input); // NOVO MENU PARA PROCURAR ENCOMENDA PARA ENTREGAR
                    }
                    break;
                }


                // OPCAO: VERIFICAR SERVICO - (se pode efetuar a entrega da encomenda que tinha feito o pedido de entrega)
                case 3: {
                    if (e.getEncomendasAceites().isEmpty() || e.tudoEntregue().equals(true)) {
                        System.out.println("Não existe encomendas para entregar\n");
                    } else {
                        System.out.println("Estas são as encomendas que pode entregar: \n");
                        e.toStringEncomendasAceites(); // Display de todas as encomendas que a empresa pode entregar
                        e.entregaRealizada(r); //Dá todas as encomendas como entregues e cria a sua "viagem"
                        e.atualizaAceites(r); // coloca todas as encomendas aceites em entregues
                    }
                    menuEmpresa(e,r, input);
                    break;
                }


                // OPCAO: VER FATURACAO - Ver valor faturado
                case 4: {
                    System.out.println("#############################################################################");
                    System.out.println("#  (1) Ver total faturado                                                   #");
                    System.out.println("#  (2) Ver parte do faturado                                                #");
                    System.out.println("#   ---------------------------------------------------------------------   #");
                    System.out.println("#  (0) Voltar ao Menu                                                       #");
                    System.out.println("#############################################################################");


                    int op = lerInteiro(input);
                    input.nextLine();

                    while(op >= 0 &&  op <= 2) {
                        switch (op) {

                            // OPCAO: VOLTAR AO MENU - volta ao menuEmpresa
                            case 0: {
                                menuEmpresa(e, r, input);
                                break;
                            }

                            // OPCAO : VER TOTAL FATURADO
                            case 1: {
                                e.faturacaoTotal(); // Ver
                                menuEmpresa(e, r, input);
                                break;
                            }

                            // OPCAO : VER PARTE DO FATURADO
                            case 2: {
                                LocalDate dataDesde = lerDataDesde(input);
                                LocalDate dataAte = lerDataAte(input);
                                e.faturacaoPeriodo(dataDesde, dataAte);
                                menuEmpresa(e, r, input);
                                break;

                            }

                            // caso nao e um numero entre as opcoes
                            default:{
                                System.out.println("Opção inexistente\n");
                                menuInicial(r,input);
                                break;
                            }
                        }
                    }
                }

                // OPCAO: VER HISTORICO
                case 5: {
                    System.out.println("#############################################################################");
                    System.out.println("#  (1) Ver todas as viagens realizadas                                      #");
                    System.out.println("#  (2) Ver parte das viagens realizadas                                     #");
                    System.out.println("#  (3) Ver viagens realizadas para um utilizador                            #");
                    System.out.println("#   ---------------------------------------------------------------------   #");
                    System.out.println("#  (0) Voltar ao Menu                                                       #");
                    System.out.println("#############################################################################");


                    int op = lerInteiro(input);
                    input.nextLine();

                    while(op >= 0 && op <= 3) {
                        switch (op) {

                            // OPCAO: VOLTAR AO MENU - volta ao menuEmpresa
                            case 0: {
                                menuEmpresa(e, r, input);
                                break;
                            }

                            // OPCAO - VER TODAS AS VIAGENS REALIZADAS
                            case 1: {
                                e.viagensTotal();
                                menuEmpresa(e, r, input);
                                break;
                            }

                            // OPCAO: VER PARTE DAS VIAGENS REALIZADAS
                            case 2 : {
                                LocalDate dataDesde = lerDataDesde(input);
                                LocalDate dataAte = lerDataAte(input);
                                e.viagensPeriodo(dataDesde, dataAte);
                                menuEmpresa(e, r, input);
                                break;
                            }

                            // OPCAO: VER VIAGENS REALIZADAS PARA UM UTILIZADOR
                            case 3:{
                                e.utilizadoresServidos(r);
                                System.out.println("Indique o utilizador: \n");
                                String nomeUtil = input.nextLine();
                                String codigoUtil = r.codigoUtilizador(nomeUtil);
                                while (codigoUtil.isEmpty()) { // caso escreva mal o nome da Empresa
                                    System.out.println("Oops, erro no input, por favor tente novamente\n");
                                    nomeUtil = input.nextLine();
                                    codigoUtil = r.codigoEmpresa(nomeUtil);
                                }
                                System.out.println("Estas são as viagens para "+ nomeUtil + "." );
                                e.viagensUtilizador(nomeUtil,r);
                                menuEmpresa(e,r,input);
                                break;
                            }

                            // caso nao e um numero entre as opcoes
                            default:{
                                System.out.println("Opção inexistente\n");
                                menuInicial(r,input);
                                break;
                            }
                        }
                    }break;
                }

                // OPCAO: ATIVAR/DESATIVAR SERVIÇO
                case 6: {
                    if (e.getProntoARecolher().equals((false))) {
                        System.out.println("AVISO : Ativou o seu serviço, agora pode entregar encomendas!\n");
                        e.setProntoARecolher(true);
                    } else {
                        System.out.println("AVISO : Desativou o seu serviço, agora não pode entregar encomendas!\n");
                        if (!(e.getPorEntregar().isEmpty())) {
                            System.out.println("Todas as encomendas que sinalizou para entregar e que ainda não tinham sido aceites foram retiradas.");
                            e.getPorEntregar().clear();
                        }
                        e.setProntoARecolher(false);
                    }
                    menuEmpresa(e, r, input);
                    break;
                }


                // caso nao e um numero entre as opcoes
                default:{
                    System.out.println("Opção inexistente\n");
                    menuInicial(r,input);
                    break;
                }
            }
        }
    }



// -----------------------------------------------     MENU VOLUNTARIO     -----------------------------------------------

    public static void menuVoluntario(Voluntario v, Registos r,Scanner input) throws InputMismatchException {

        int opcao;

        System.out.println("#############################################################################");
        System.out.println("#                          Bem vindo ao TrazAqui!                           #");
        System.out.println("#                          Selecione a sua opção:                           #");
        if (v.getProntoARecolher().equals(true)) {
            System.out.println("#                      Neste momento está de serviço                        #");
        } else {
            System.out.println("#                      Neste momento está fora de serviço                   #");
        }
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (1) O Meu Perfil                                                         #");
        System.out.println("#  (2) Encomendas por entregar                                              #");
        System.out.println("#  (3) Verificar serviço                                                    #");
        System.out.println("#  (4) Ver Histórico                                                        #");
        System.out.println("#  (5) Ativar/Desativar Serviço                                             #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Voltar ao Menu Principal                                             #");
        System.out.println("#############################################################################");

        opcao = lerInteiro(input);
        input.nextLine();

        while (opcao >= 0 && opcao <= 5) {
            switch (opcao) {

                // OPCAO: VOLTAR AO MENU PRINCIPAL
                case 0: {
                    menuInicial(r, input);
                    break;
                }

                // OPCAO: O MEU PERFIL - Informacoes sobre o Perfil Voluntario
                case 1: {
                    System.out.println(v.toString());
                    menuVoluntario(v,r,input);
                    break;
                }

                // OPCAO: ENCOMENDAS POR ENTREGAR - Fornecer as encomendas que existem para entregar no seu raio de acao
                case 2: {
                    if (v.getProntoARecolher().equals(false)) { // O servico esta desativado, nao pode efetuar nenhuma encomenda
                        System.out.println("De momento não se encontra com o serviço ativo.\n");
                        menuVoluntario(v, r, input);
                        break;
                    } else {
                        ArrayList<Encomenda> paraEntrega = new ArrayList<>();
                        menuTransportadorAuxiliar(v, r, paraEntrega, input); // NOVO MENU PARA PROCURAR ENCOMENDA PARA ENTREGAR
                    }
                }

                // OPCAO: VERIFICAR SERVICO - (quais das encomendas escolhidas a empresa pode realizar)
                case 3: {
                    if (v.getEncomendasAceites().isEmpty() || v.tudoEntregue().equals(true)) {
                        System.out.println("Não existe encomendas para entregar! \n");
                    } else {
                        System.out.println("Pode realizar a seguinte encomenda: \n");
                        v.toStringEncomendasAceites(); // Display de todas as encomendas que a voluntario pode entregar
                        v.entregaRealizada(r); //Dá todas as encomendas como entregues e cria a sua "viagem"
                        v.atualizaAceites(r); // coloca todas as encomendas aceites em entregues
                    }
                    menuVoluntario(v, r, input);
                    break;
                }

                // OPCAO: VER HISTORICO - Ver Historico de Viagens
                case 4: {
                    System.out.println("#############################################################################");
                    System.out.println("#  (1) Ver todas as viagens realizadas                                      #");
                    System.out.println("#  (2) Ver parte das viagens realizadas                                     #");
                    System.out.println("#  (3) Ver viagens realizadas para um utilizador                            #");
                    System.out.println("#   ---------------------------------------------------------------------   #");
                    System.out.println("#  (0) Voltar ao Menu                                                       #");
                    System.out.println("#############################################################################");


                    int op = lerInteiro(input);
                    input.nextLine();

                    while (op >= 0 && op <= 3) {
                        switch (op) {

                            // OPCAO: VOLTAR AO MENU - Volta ao menuVoluntario
                            case 0: {
                                menuVoluntario(v, r, input);
                                break;
                            }

                            // OPCAO: VER TOAS AS VIAGENS REALIZADAS
                            case 1: {
                                v.viagensTotal();
                                menuVoluntario(v, r, input);
                                break;
                            }

                            // OPCAO VER PARTE DAS VIAGENS REALIZADAS
                            case 2: {
                                LocalDate dataDesde = lerDataDesde(input);
                                LocalDate dataAte = lerDataAte(input);
                                v.viagensPeriodo(dataDesde, dataAte);
                                menuVoluntario(v, r, input);
                                break;
                            }

                            // OPCAO: VER VIAGENS REALIZADAS PARA UM UTILIZADOR
                            case 3:{
                                v.utilizadoresServidos(r);
                                System.out.println("Indique o utilizador: \n");
                                String nomeUtil = input.nextLine();
                                String codigoUtil = r.codigoUtilizador(nomeUtil);
                                while (codigoUtil.isEmpty()) { // caso escreva mal o nome da Empresa
                                    System.out.println("Oops, erro no input, por favor tente novamente.\n");
                                    nomeUtil = input.nextLine();
                                    codigoUtil = r.codigoEmpresa(nomeUtil);
                                }
                                System.out.println("Estas são as viagens para "+ nomeUtil + ".");
                                v.viagensUtilizador(nomeUtil,r);
                                menuVoluntario(v,r,input);
                                break;
                            }
                            // caso nao e um numero entre as opcoes
                            default: {
                                System.out.println("Opção inexistente\n");
                                menuVoluntario(v, r, input);
                                break;
                            }
                        }
                    }
                    break;
                }


                    //OPCAO: ATIVAR/DESATIVAR SERVICO
                case 5: {
                        if (v.getProntoARecolher().equals((false))) {
                            System.out.println("AVISO : Ativou o seu serviço, agora pode entregar encomendas!\n");
                            v.setProntoARecolher(true);
                        } else {
                            System.out.println("AVISO : Desativou o seu serviço, agora não pode entregar encomendas!\n");
                            if (!(v.getPorEntregar().isEmpty())) {
                                System.out.println("Todas as encomendas que sinalizou para entregar e que ainda não tinham sido aceites foram retiradas. \n");
                                v.getPorEntregar().clear();
                            }
                            v.setProntoARecolher(false);
                        }
                    menuVoluntario(v, r, input);
                    break;
                }

                    // caso nao e um numero entre as opcoes
                    default: {
                        System.out.println("Opção inexistente\n");
                        menuVoluntario(v, r, input);
                        break;
                    }
                }
            }
        }


    public static void menuTransportadorAuxiliar(Transportadores t, Registos r,ArrayList<Encomenda> paraEntrega, Scanner input) throws InputMismatchException{
        System.out.println("#############################################################################");
        System.out.println("#  (1) Ver Encomendas disponíveis por Entregar                              #");
        System.out.println("#  (2) Adicionar Encomenda                                                  #");
        System.out.println("#  (3) Completar lista de Encomendas por Entregar                           #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Voltar ao Menu                                                       #");
        System.out.println("#############################################################################");


            String codigoEncomenda = "";
            int op = lerInteiro(input);
            input.nextLine();

            while (op >= 0 && op <= 3) {
                switch (op) {

                    // OPCAO: VOLTAR AO MENU - Volta ao menuVoluntario
                    case 0: {
                        if (t instanceof Empresa){
                            menuEmpresa((Empresa)t, r,input);
                            break;
                        }
                        if( t instanceof Voluntario){
                            menuVoluntario((Voluntario)t, r,input);
                            break;
                        }
                    }

                    // OPCAO: VER ENCOMENDAS DISPONIVEIS - Display das encomedas para entregar
                    case 1: {
                        if(r.encomendasNoRaio(t).isEmpty()){
                            System.out.println("Não existem encomendas para entregar no seu raio de ação!");
                        }
                        else {
                            System.out.println("Estas são as encomendas para entrega:\n");
                            System.out.println(r.encomendasNoRaio(t));// todas as encomendas (normais e medicas)
                        }
                        menuTransportadorAuxiliar(t,r,paraEntrega,input);
                        break;
                    }

                    // OPCAO: ADICIONAR ENCOMENDA - Escolher uma encomenda
                    case 2: {
                        if (r.encomendasNoRaio(t).isEmpty()) {
                            System.out.println("Não existem encomendas para entregar no seu raio de ação! \n");
                        } else {
                            System.out.println("Indique a encomenda que quer entregar.\n");
                            codigoEncomenda = input.nextLine();
                            if(t instanceof Empresa) {
                                if (t.jaTemEncomenda(codigoEncomenda)) {
                                    System.out.println("Já escolheu esta encomenda, por favor escolha outra.\n");
                                    menuTransportadorAuxiliar(t, r, paraEntrega,input);
                                    break;
                                }
                            }

                            if (r.encomendaExiste(codigoEncomenda).equals(false)) {
                                System.out.println("Oops, erro no input, por favor tente novamente.\n");
                                menuTransportadorAuxiliar(t, r, paraEntrega,input);
                                break;

                            }
                            Encomenda enc = r.getEncomendas().get(codigoEncomenda);
                            if(t.getCertificado().equals(true) && (enc instanceof EncomendaMedica || enc instanceof EncomendaNormal)){
                                if (paraEntrega.size() < t.getCapacidade()) {
                                    paraEntrega.add(enc);
                                } else {
                                    System.out.println("Já está na sua capacidade limite");
                                }
                            }
                            else{
                                if(t.getCertificado().equals(false) && (enc instanceof EncomendaNormal)){
                                    if (paraEntrega.size() < t.getCapacidade()) {
                                        paraEntrega.add(enc);
                                    } else {
                                        System.out.println("Já está na sua capacidade limite");
                                    }
                                }
                                if(t.getCertificado().equals(false) && (enc instanceof  EncomendaMedica)){
                                    System.out.println("Não tem certificado para transportar esta encomenda.\n");
                                }
                            }

                        }
                        menuTransportadorAuxiliar(t, r,paraEntrega,input);
                        break;
                    }

                    // OPCAO: COMPLETAR LISTA DE ENCOMENDAS POR ENTREGAR - Dar como completa a lista de encomendas por entregar
                    case 3: {
                        if (paraEntrega.isEmpty()) {
                            System.out.println("Não escolheste nenhuma encomenda para entregar\n");
                            menuTransportadorAuxiliar(t, r, paraEntrega,input);
                            break;
                        } else {
                            t.setPorEntregar(paraEntrega); // atualiza o arrayList porEntregar com as encomendas que agora quer entregar

                            for (Encomenda enc : paraEntrega) {
                                r.atualizaVaiRecolher(enc.getCodEncomenda(),t.getCodigoPerfil()); // atualiza o HashMap vaiRecolher com a nova empresa que quer entregar a encomenda
                            }

                            System.out.println("O seu pedido para cobrir o serviço de entrega já foi efetuado!\n");
                            System.out.println("Verifique o estado das possíveis encomendas a realizar para saber quais foram aceites! ");
                        }
                        if(t instanceof Empresa){
                            menuEmpresa((Empresa) t,r,input);
                        }
                        else{
                            menuVoluntario((Voluntario) t, r, input);
                        }
                        break;
                    }

                    // caso nao e um numero entre as opcoes
                    default: {
                        System.out.println("Opção inexistente\n");
                        menuInicial(r,input);
                        break;
                    }
                }
            }
        }





// -----------------------------------------------     MENU LOJA     -----------------------------------------------

    public static void menuLoja(Loja l, Registos r, Scanner input) throws InputMismatchException {

        int opcao;

        System.out.println("#############################################################################");
        System.out.println("#                          Bem vindo ao TrazAqui!                           #");
        System.out.println("#                          Selecione a sua opção:                           #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (1) O Meu Perfil                                                         #");
        System.out.println("#  (2) Ver produtos existentes                                              #");
        System.out.println("#  (3) Adicionar produtos                                                   #");
        System.out.println("#  (4) Remover produtos                                                     #");
        System.out.println("#  (5) Ver Histórico                                                        #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Voltar ao Menu Principal                                             #");
        System.out.println("#############################################################################");

        opcao = lerInteiro(input);
        input.nextLine();

        while((opcao >= 0) && (opcao <= 5)) {
            switch (opcao) {

                // OPCAO: VOLTAR AO MENU - Volta ao menuVoluntario
                case 0:{
                    menuInicial(r, input);
                    break;
                }

                // OPCAO: O MEU PERFIL - Ver o perfil da loja
                case 1: {
                    System.out.println(l.toString());
                    menuLoja(l,r,input);
                    break;
                }

                // OPCAO: VER PRODUTOS EXISTENTES
                case 2:{
                    System.out.println("Estes são os produtos que vende:");
                    System.out.println(l.toStringProdutos());
                    menuLoja(l,r,input);
                    break;
                }

                // OPCAO: ADICIONAR PRODUTOS - caso a loja queira adicionar novos produtos ao seu hashMap de produtos
                case 3: {
                    System.out.println("Indique o produto que quer adicionar à sua loja: \n"); // os produtos terao sempre um peso qualquer
                    String nomeProduto = input.nextLine();

                    LinhaEncomenda produto = new LinhaEncomenda();
                    produto.setNome(nomeProduto);
                    produto.setCodProduto("p"+(r.getProdutos()+1)); // cria o codigo do produto
                    r.setProdutos(r.getProdutos()+1);
                    l.addProduto(produto); // adiciona ao HashMap de produtos
                    menuLoja(l,r, input);
                    break;

                }

                // OPCAO: REMOVER PRODUTOS - caso a loja queira remover produtos do seu hashMap de produtos
                case 4: {
                    System.out.println("Indique o produto que quer remover da sua loja: \n"); // remover por completo um produto
                    String nomeProduto = input.nextLine();
                    String codeProd = l.verificaProduto(nomeProduto);
                    while(codeProd.isEmpty()) { // para o caso de escrever mal o nome
                        System.out.println("Oops, nome errado.\n");
                        nomeProduto = input.nextLine();
                        codeProd = r.codigoLoja(nomeProduto);
                    }
                    l.removeProduto(codeProd);
                    menuLoja(l,r,input);
                    break;
                }

                // OPCAO: VER HISTORICO -
                case 5: {
                    System.out.println("#############################################################################");
                    System.out.println("#  (1) Ver histórico todo                                                   #");
                    System.out.println("#  (2) Ver parte do histórico                                               #");
                    System.out.println("#   ---------------------------------------------------------------------   #");
                    System.out.println("#  (0) Voltar ao Menu                                                       #");
                    System.out.println("#############################################################################");

                    int op = lerInteiro(input);
                    input.nextLine();

                    while(op >= 0 && op <=2) {
                        switch (op) {

                            // OPCAO: VER HISTORICO TOODO
                            case 1: {
                                l.printHistorico();
                                menuLoja(l, r, input);
                                break;
                            }

                            // OPCAO: VER PARTE DO HISTORICO
                            case 2: {

                                LocalDate dataDesde = lerDataDesde(input);
                                LocalDate dataAte = lerDataAte(input);

                                l.historicoPeriodo(dataDesde, dataAte);
                                menuLoja(l, r, input);
                                break;
                            }

                            // OPCAO: VOLTA AO MENU - Volta para o MenuLoja
                            case 0: {
                                menuLoja(l, r, input);
                                break;
                            }

                            // caso nao e um numero entre as opcoes
                            default:{
                                System.out.println("Opção inexistente\n");
                                menuLoja(l,r, input);
                                break;
                            }
                        }
                    }
                }

                // caso nao e um numero entre as opcoes
                default:{
                    System.out.println("Opção inexistente\n");
                    menuLoja(l,r, input);
                    break;
                }
            }
        }
    }


// -----------------------------------------------     MENU INFORMACOES     -----------------------------------------------


    public static void menuInformacoes(Registos r, Scanner input) throws InputMismatchException{

        int opcao;

        System.out.println("#############################################################################");
        System.out.println("#           Aqui tem algumas informações sobre a nossa aplicação!           #");
        System.out.println("#                          Selecione a sua opção:                           #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (1) TrazAqui!                                                            #");
        System.out.println("#  (2) Top 10 Utilizadores                                                  #");
        System.out.println("#  (3) Top 10 Empresa                                                       #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Voltar ao Menu Principal                                             #");
        System.out.println("#############################################################################");

        opcao = lerInteiro(input);
        input.nextLine();

        while(opcao >=0 && opcao <= 3) {
            switch (opcao) {

                // OPCAO: VOLTAR AO MENU PRINCIPAL
                case 0: {
                    menuInicial(r,input);
                    break;
                }

                // OPCAO: TRAZAQUI - Informacoes sobre a aplicacao
                case 1: {
                    System.out.println("  Devido à crise sanitária instalada neste momento no nosso querido mundo, decidimos criar o TrazAqui!\n");
                    System.out.println("Queremos ajudar as pessoas afetadas diretamente (ou não) por este vírus, através de um serviço ao domicílio.\n");
                    System.out.println("Foi com a ajuda dos bravos voluntários e funcionários das empresas que conseguimos tornar isto tudo possível.\n");
                    System.out.println("                        Obrigado por utilizar a nossa aplicação e lembre-se:                                 \n");
                    System.out.println("\n");
                    System.out.println("                                FIQUE EM CASA , FIQUE SEGURO                                                 \n");
                    menuInformacoes(r, input);
                    break;
                }

                // OPCAO: TOP 10 UTILIZADORES - faz display do Top10 Utilizadores
                case 2: {
                    r.top10utilizadores();
                    menuInformacoes(r, input);
                    break;
                }

                // OPCAO: TOP 10 EMPRESAS - faz display do Top10 empresas
                case 3: {
                    r.top10transportadoras();
                    menuInformacoes(r, input);
                    break;
                }

                // caso nao e um numero entre as opcoes
                default:{
                    System.out.println("Opção inexistente\n");
                    menuInicial(r,input);
                    break;
                }
            }
        }
    }







    // Registar Contas/Perfis
    private static void menuCriarConta(Registos r, Scanner input)throws InputMismatchException {

        System.out.println("#############################################################################");
        System.out.println("#                    Olá! Vamos proceder com a criação                      #");
        System.out.println("#                                da sua conta!                              #");
        System.out.println("#                         Selecione o tipo de conta:                        #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#   Sou um(a):                                                              #");
        System.out.println("#  (1) Cliente                                                              #");
        System.out.println("#  (2) Empresa                                                              #");
        System.out.println("#  (3) Voluntario                                                           #");
        System.out.println("#  (4) Loja                                                                 #");
        System.out.println("#   ---------------------------------------------------------------------   #");
        System.out.println("#  (0) Voltar ao Menu Principal                                             #");
        System.out.println("#############################################################################");

        int opcao = lerInteiro(input);
        input.nextLine();

        while(opcao >= 0 && opcao <= 4){
            switch(opcao) {
                case 1: {
                    registaCliente(r,input);
                    break;
                }

                case 2: {
                    registaEmpresa(r,input);
                    break;
                }

                case 3: {
                    registaVoluntario(r,input);
                    break;
                }

                case 4: {
                    registaLoja(r,input);
                    break;
                }

                case 0: {
                    menuInicial(r,input);
                    break;
                }

                // caso nao e um numero entre as opcoes
                default: {
                    System.out.println("Opção inexistente\n");
                    menuInicial(r,input);
                    break;
                }
            }
        }
    }

    // Registo de um cliente
    public static void registaCliente(Registos r, Scanner input)throws InputMismatchException{

        String emailRegex = "^[a-zA-Z0-9_+&*-]+(?:\\."+
                "[a-zA-Z0-9_+&*-]+)*@" +
                "(?:[a-zA-Z0-9-]+\\.)+[a-z" +
                "A-Z]{2,7}$";

        Integer numUtil = r.getUtilizador();
        String codigoUtilizador = ("u"+(numUtil+1));
        r.setUtilizador(r.getUtilizador()+1);
        System.out.println("Indique o seu Email: \n");

        String email = input.nextLine();

        if(r.jaExisteMail(email)){
            System.out.println("Esse email já está registado. Por favor tente novamente!");
            registaCliente(r, input);

        }

        if (email.matches(emailRegex)){

            System.out.println("Indique o seu nome: \n");
            String nome = input.nextLine();

            System.out.println("Indique a sua password: \n");
            String pass = input.nextLine();

            System.out.println("Estamos quase lá! Vamos proceder com a sua localização: \n");
            System.out.println("Indique a sua longitude: \n");
            Double longitude = lerDouble(input);
            input.nextLine();
            System.out.println("Indique a sua latitude: \n");
            Double latitude = lerDouble(input);
            input.nextLine();
            GPS coordenadas = new GPS(latitude,longitude);

            Utilizador novo = new Utilizador(codigoUtilizador,coordenadas,nome,email,pass);

            System.out.println("Já está! Encontra-se agora registado no TrazAqui! Vamos proceder para o seu menu! \n ");
            r.addPerfil(novo);
            r.atualizaEmails(codigoUtilizador,email);
            menuUtilizador(novo,r,input);


        }
        else{
            System.out.println("Email inválido! Por favor insira outro email");
            registaCliente(r, input);
        }
    }

    // Registo de uma Empresa
    public static void registaEmpresa(Registos r,Scanner input)throws InputMismatchException{

        String emailRegex = "^[a-zA-Z0-9_+&*-]+(?:\\."+
                "[a-zA-Z0-9_+&*-]+)*@" +
                "(?:[a-zA-Z0-9-]+\\.)+[a-z" +
                "A-Z]{2,7}$";
        Empresa novo = new Empresa();

        Integer numEmpres = r.getEmpresa();
        String codigoEmpresa = "t"+(numEmpres+1);
        r.setEmpresa(r.getEmpresa()+1);
        novo.setCodigoPerfil(codigoEmpresa);

        System.out.println("Indique o seu Email: \n");
        String email = input.nextLine();
        novo.setEmail(email);

        if(r.jaExisteMail(email)){
            System.out.println("Esse email já está registado. Por favor tente novamente!");
            registaEmpresa(r, input);
        }

        if (email.matches(emailRegex)){

            System.out.println("Indique o nome da sua empresa: \n");
            String nome = input.nextLine();
            novo.setNome(nome);

            System.out.println("Indique a sua password: \n");
            String pass = input.nextLine();
            novo.setPass(pass);

            System.out.println("Vamos proceder com a sua localização: \n");
            System.out.println("Indique a sua longitude: \n");
            Double longitude = lerDouble(input);
            input.nextLine();
            System.out.println("Indique a sua latitude: \n");
            Double latitude = lerDouble(input);
            input.nextLine();
            GPS coordenadas = new GPS(latitude,longitude);
            novo.setCoordenadas(coordenadas);

            System.out.println("Vamos agora preencher detalhes específicos para a sua transportadora! \n");

            System.out.println("Indique o seu raio de ação: \n");
            Double raio = lerDouble(input);
            input.nextLine();
            novo.setRaio(raio);
            System.out.println("Indique se é certificado para transporte de encomendas médicas ou outras (Sim/Nao): \n");
            String certiResposta = input.nextLine();
            while(!certiResposta.equals("Sim") && !certiResposta.equals("Nao")){
                System.out.println("Input inválido, tente novamente.\n");
                certiResposta = input.nextLine();
            }
            boolean certificado;
            certificado = certiResposta.equals("Sim");
            novo.setCertificado(certificado);


            System.out.println("Indique o seu NIF: \n");
            String nif = input.nextLine();

            novo.setNif((nif));
            System.out.println("Indique a sua taxa, em Euros, que cobra por cada Km efetuado: \n");
            Double taxaKm = lerDouble(input);
            input.nextLine();
            novo.setTaxaKm(taxaKm);
            System.out.println("Indique quantas encomendas consegue transportar: \n ");
            Integer capacidade = lerInteiro(input);
            input.nextLine();

            novo.setCapacidade(capacidade);

            System.out.println("Já está! Encontra-se agora registado no TrazAqui! Vamos proceder para o seu menu! \n ");
            r.addPerfil(novo);
            r.atualizaEmails(codigoEmpresa,email);
            menuEmpresa(novo,r, input);

        }
        else{
            System.out.println("Email inválido! Por favor insira outro email");
            registaEmpresa(r,input);
        }
    }

    // Registo de um voluntario
    public static void registaVoluntario(Registos r,Scanner input)throws InputMismatchException{

        String emailRegex = "^[a-zA-Z0-9_+&*-]+(?:\\."+
                "[a-zA-Z0-9_+&*-]+)*@" +
                "(?:[a-zA-Z0-9-]+\\.)+[a-z" +
                "A-Z]{2,7}$";
        Voluntario novo = new Voluntario();

        Integer numVol = r.getVoluntario();
        String codigoVoluntario = "v"+(numVol+1);
        r.setVoluntario(r.getVoluntario()+1);
        novo.setCodigoPerfil(codigoVoluntario);

        System.out.println("Indique o seu Email: \n");
        String email = input.nextLine();
        novo.setEmail(email);

        if(r.jaExisteMail(email)){
            System.out.println("Esse email já está registado. Por favor tente novamente!");
            registaVoluntario(r, input);
        }

        if (email.matches(emailRegex)){

            System.out.println("Indique o seu nome: \n");
            String nome = input.nextLine();
            novo.setNome(nome);

            System.out.println("Indique a sua password: \n");
            String pass = input.nextLine();
            novo.setPass(pass);

            System.out.println("Vamos proceder com a sua localização: \n");
            System.out.println("Indique a sua longitude: \n");
            Double longitude = lerDouble(input);
            input.nextLine();
            System.out.println("Indique a sua latitude: \n");
            Double latitude = lerDouble(input);
            input.nextLine();
            GPS coordenadas = new GPS(latitude,longitude);
            novo.setCoordenadas(coordenadas);

            System.out.println("Vamos agora preencher os seus detalhes específicos! \n");

            System.out.println("Indique o seu raio de ação: \n");
            Double raio = lerDouble(input);
            input.nextLine();
            novo.setRaio(raio);
            System.out.println("Indique se é certificado para transporte de encomendas médicas ou outras (Sim/Não): \n");
            String certiResposta = input.nextLine();
            if(certiResposta.equals("Sim")){
                Boolean certificado = true;
                novo.setCertificado(certificado);
            }
            else if(certiResposta.equals("Nao")){
                Boolean certificado = false;
                novo.setCertificado(certificado);
            }
            else {System.out.println("Input inválido, tente novamente.\n");}

            System.out.println("Já está! Encontra-se agora registado no TrazAqui! Vamos proceder para o seu menu! \n ");
            r.addPerfil(novo);
            r.atualizaEmails(codigoVoluntario,email);
            menuVoluntario(novo,r,input);


        }
        else{
            System.out.println("Email inválido! Por favor insira outro email");
            registaVoluntario(r,input);
        }
    }


    // Registo de uma loja
    public static void registaLoja(Registos r, Scanner input)throws InputMismatchException{

        String emailRegex = "^[a-zA-Z0-9_+&*-]+(?:\\."+
                "[a-zA-Z0-9_+&*-]+)*@" +
                "(?:[a-zA-Z0-9-]+\\.)+[a-z" +
                "A-Z]{2,7}$";
        Loja novo = new Loja();

        Integer numLoja = r.getLoja();
        String codigoLoja = "l"+(numLoja+1);
        r.setLoja(r.getLoja()+1);
        novo.setCodigoPerfil(codigoLoja);

        System.out.println("Indique o seu Email: \n");
        String email = input.nextLine();
        novo.setEmail(email);

        if(r.jaExisteMail(email)){
            System.out.println("Esse email já está registado. Por favor tente novamente!");
            registaLoja(r, input);
        }

        if (email.matches(emailRegex)){

            System.out.println("Indique o seu nome: \n");
            String nome = input.nextLine();
            novo.setNome(nome);

            System.out.println("Indique a sua password: \n");
            String pass = input.nextLine();
            novo.setPass(pass);

            System.out.println("Vamos proceder com a sua localização: \n");
            System.out.println("Indique a sua longitude: \n");
            Double longitude = lerDouble(input);
            input.nextLine();
            System.out.println("Indique a sua latitude: \n");
            Double latitude = lerDouble(input);
            input.nextLine();
            GPS coordenadas = new GPS(latitude,longitude);
            novo.setCoordenadas(coordenadas);

            System.out.println("Já está! Encontra-se agora registado no TrazAqui! Vamos proceder para o seu menu! \n ");
            r.addPerfil(novo);
            r.atualizaEmails(codigoLoja,email);
            menuLoja(novo,r, input);


        }
        else{
            System.out.println("Email inválido! Por favor insira outro email");
            registaLoja(r,input);
        }
    }

    // ------------------------------------ Trata de Exceptions -------------

    // Verifica se o input e um inteiro
    private static int lerInteiro(Scanner input) {
        Integer opcao = null;
        do {
            try {
                opcao = input.nextInt();
            } catch (InputMismatchException e) {
                System.out.println("Input inválido. Tente novamente.");
                input.nextLine();
            }
        }
        while(opcao == null);
        return opcao;
    }


    // Verifica se o input e uma data
    private static LocalDate lerDataDesde(Scanner input) {
        LocalDate data = null;
        do {
            try {

                System.out.println("Desde ano: \n");
                int ano = lerInteiro(input);
                input.nextLine();

                System.out.println("Desde mês: \n");
                int mes = lerInteiro(input);
                input.nextLine();

                System.out.println("Desde dia: \n");
                int dia = lerInteiro(input);
                input.nextLine();

                data = LocalDate.of(ano,mes,dia);
            }
            catch (DateTimeException e) {
                System.out.println("Input inválido. Tente novamente. " + e.getMessage());
            }
        }
        while (data == null);
        return data;
    }
    private static LocalDate lerDataAte(Scanner input) {
        LocalDate data = null;
        do {
            try {

                System.out.println("Ate ano: \n");
                int ano = lerInteiro(input);
                input.nextLine();

                System.out.println("Ate mês: \n");
                int mes = lerInteiro(input);
                input.nextLine();

                System.out.println("Ate dia: \n");
                int dia = lerInteiro(input);
                input.nextLine();

                data = LocalDate.of(ano,mes,dia);
            }
            catch (DateTimeException e) {
                System.out.println("Input inválido. Tente novamente. " + e.getMessage());
            }
        }
        while (data == null);
        return data;
    }

    private static double lerDouble(Scanner input) {
        double opcao = -1.0;
        do {
            try {
                opcao = input.nextDouble();
            } catch (InputMismatchException e) {
                System.out.println("Input inválido. Tente novamente.");
                input.nextLine();
            }
        } while (opcao == -1.0);
        return opcao;
    }




}