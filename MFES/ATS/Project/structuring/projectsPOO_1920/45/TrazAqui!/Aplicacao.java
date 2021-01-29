import java.io.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.ResolverStyle;
import java.util.*;
import java.lang.*;

/**
 * Aplicacao - Class de interface
 *
 * @author (Eduardo Araujo, Ricardo Machado, Guilherme Araujo)
 * @version 11/06/2020
 */
public class Aplicacao {
    private UInterface baseUI;
    private UInterface registoUI;
    private UInterface utilizadorUI;
    private UInterface voluntarioUI;
    private UInterface transportadoraUI;
    private UInterface lojaUI;
    private UInterface queriesUI;
    
    private Dados data = new Dados();

    /**
     * Construtor para objetos da class Aplicacao
     */
    private Aplicacao(){
        String[] opt_baseUI = {"Login", "Registo", "Queries", "Save Estado", "Load Estado", "Load logs"};
        String[] opt_registoUI = {"Registar (Utilizador)", "Registar (Voluntário)", "Registar (Transportadora)", "Registar (Loja)"};
        String[] opt_utilizadorUI = {"Ver Perfil", "Criar nova encomenda", "Avaliar encomenda passada","Histórico de encomendas"};
        String[] opt_voluntarioUI = {"Ver Perfil", "Escolher encomenda para entrega", "Ve avaliacoes",  "Historico encomendas"};
        String[] opt_transportadoraUI = {"Ver Perfil", "Escolher encomenda para entrega","Ve avaliacoes",  "Historico encomendas"};
        String[] opt_lojaUI = {"Ver Perfil", "Sinaliza encomenda pronta para entrega",  "Historico encomendas"};
        String[] opt_queriesUI = {"Total faturado por uma transportadora",
                "10 utilizadores que mais utilizaram o sistema",
                "10 transportadoras que mais utilizaram o sistema"};

        this.baseUI = new UInterface(opt_baseUI);
        this.registoUI = new UInterface(opt_registoUI);
        this.utilizadorUI = new UInterface(opt_utilizadorUI);
        this.voluntarioUI = new UInterface(opt_voluntarioUI);
        this.transportadoraUI = new UInterface(opt_transportadoraUI);
        this.lojaUI = new UInterface(opt_lojaUI);
        this.queriesUI = new UInterface(opt_queriesUI);
    }
    private void menuLoja(String codLoja){
        int r = 1;
        while(r == 1){
            int op = lojaUI.exec();
            Loja l = data.codLojaToL(codLoja);
            switch(op){
                case 1: System.out.println(l.toString());
                    break;
                case 2: data.reqEntrega(l);
                    break;
                case 3: data.histEncomendas(l);
                    break;
                case 0: r = 0;
                    break;
            }
        }
    }

    /*Interfaces*/
    private void menuInicial(int s){
        if(s == 0) load();
        int r = 1;
        while(r == 1){
            int op = baseUI.exec();
            switch(op){
                case 1: menuLogin();
                    r = 0;
                    break;
                case 2: menuRegisto();
                    break;
                case 3: menuQueries();
                    break;
                case 4: save();
                    break;
                case 5: load();
                    break;
                case 6: data.parse("logsGerados.csv");
                    break;
                case 0: System.out.println("See ya");
                    r = 0;
                    break;
            }
        }
    }

    private void menuRegisto(){
        Scanner input = new Scanner (System.in);
        String password = null;
        String pw2;
        String email = null;
        String nome = null;
        double gpsX;
        double gpsY;
        int nif = 0;
        double raio = 0.0;
        double cx = 0.0;
        double cy = 0.0;
        double precokm = 0.0;
        Utilizador u;
        Transportadora t;
        Voluntario v;
        Loja l;
        List <Encomenda> encomendas = new ArrayList<>();
        int op;

        while(true){
            op = registoUI.exec();

            if(op == 0){break;}
            System.out.println("\n");


            int val = 1;

            while(val != 0){
                System.out.print("\nEmail: ");
                email = input.nextLine();

                try{
                    data.isEmail(email);
                    val = 0;
                }
                catch(EmailInvalidoException e) {
                    System.out.println("Email inválido!");
                    val = 1;
                }
            }

            val = 1;

            while(val != 0){
                System.out.print("\nPassword: ");
                password = input.nextLine();
                System.out.print("\nPassword (Confirmacao): ");
                pw2 = input.nextLine();
                try {
                    val = data.passValida(password, pw2);
                }
                catch (PassInvalidaException e){
                    System.out.println("Passwords nao coincidem. Insira novamente.");
                    val = 1;
                }
            }

            val = 1;

            while(val != 0){
                System.out.print("\nNome: ");
                nome = input.nextLine();
                try{
                    val = data.nomeValido(nome);
                } catch(NomeInvalidoException e){
                    System.out.println("Nome invalido. Tente novamente.");
                    val = 1;
                }
            }

            val = 1;

            while(val != 0) {
                try {
                    System.out.print("\nCoordenada x: ");
                    cx = input.nextDouble();
                    val = 0;
                } catch (InputMismatchException e) {
                    System.out.println("Coordenadas invalidas. Tente novamente.");
                    input.nextLine();
                    val = 1;
                }
                if(val == 0) {
                    try {
                        System.out.print("\nCoordenada y: ");
                        cy = input.nextDouble();
                        val = 0;
                    } catch (InputMismatchException e) {
                        System.out.println("Coordenada invalida. Tente novamente.");
                        input.nextLine();
                        val = 1;
                    }
                }
            }

            val = 1;

            if(op == 2 || op == 3){
                while(val != 0){
                    try {
                        System.out.print("\nRaio: ");
                        raio = input.nextDouble();
                        val = 0;
                    } catch (InputMismatchException e) {
                        System.out.println("Raio invalido. Tente novamente.");
                        val = 1;
                        input.nextLine();
                    }
                }
            }

            val = 1;

            if(op == 3){
                while(val != 0){
                    try{
                        System.out.print("\nNIF: ");
                        nif = input.nextInt();
                        val = data.nifValido(nif);
                    }catch(NifInvalidoException e){
                        System.out.println("NIF invalido. Tente novamente.");
                        val = 1;
                        input.nextLine();
                    }
                }

                val = 1;

                while(val != 0){
                    try{
                        System.out.print("\nPreco/KM: ");
                        precokm = input.nextDouble();
                        val = 0;
                    }catch(InputMismatchException e){
                        System.out.println("Preco/KM invalido. Tente novamente.");
                        val = 1;
                        input.nextLine();
                    }
                }
            }

            if(op == 1){
                String codUser = data.newCodUser();
                System.out.println(codUser);
                gpsX = cx;
                gpsY = cy;
                u = new Utilizador(codUser, nome, gpsX, gpsY, email, password, encomendas);
                System.out.println(u.toString());
                data.addUser(u);
            }
            if(op == 2){
                String codVolu = data.newCodVolu();
                System.out.println(codVolu);
                gpsX = cx;
                gpsY = cy;
                v = new Voluntario(codVolu, nome, gpsX, gpsY, raio, email, password, 0, encomendas);
                System.out.println(v.toString());
                data.addVolu(v);
            }
            if(op == 3){
                String codTran = data.newCodTran();
                System.out.println(codTran);
                gpsX = cx;
                gpsY = cy;
                t = new Transportadora(codTran, nome, gpsX, gpsY, nif, raio, precokm, email, password, 0, encomendas);
                System.out.println(t.toString());
                data.addTran(t);
            }
            if(op == 4){
                String codLoja = data.newCodLoja();
                System.out.println(codLoja);
                gpsX = cx;
                gpsY = cy;
                l = new Loja(codLoja, nome, gpsX, gpsY, email, password, encomendas);
                System.out.println(l.toString());
                data.addLoja(l);
            }
        }
    }

    private void menuLogin(){
        Scanner input = new Scanner (System.in);
        String password;
        String email;
        Utilizador u = null;
        Transportadora t = null;
        Voluntario v = null;
        Loja l = null;

        System.out.println("\n");
        int val = 1;
        while (val != 0) {
            System.out.print("Email: ");
            email = input.nextLine();

            try{
                data.existeEmail(email);
                u = data.emailToUser(email);
                t = data.emailToTran(email);
                v = data.emailToVolu(email);
                l = data.emailToLoja(email);
                val = 0;
            }
            catch (NoEmailException e) {
                System.out.println("Email inexistente.");
                val = 1;
            }
        }


        val = 1;

        if(u != null){
            while(val != 0) {
                System.out.print("Password: ");
                password = input.nextLine();
                val = 0;

                if (!(u.getPassword().equals(password))) {
                    System.out.println("Password incorreta.");
                    val = 1;
                }
                if(val == 0){
                    menuUtilizador(u.getCodUser());

                    menuInicial(1);
                }
            }
        }
        else if(t != null){
            while(val != 0) {
                System.out.print("Password: ");
                password = input.nextLine();
                val = 0;
                if (!(t.getPassword().equals(password))) {
                    System.out.println("Password incorreta.");
                    val = 1;
                }

                if(val == 0){
                    menuTransportadora(t.getCodTran());
                    menuInicial(1);
                }
            }
        }
        else if(v != null){
            while(val != 0) {
                System.out.print("Password: ");
                password = input.nextLine();
                val = 0;
                if (!(v.getPassword().equals(password))) {
                    System.out.println("Password incorreta.");
                    val = 1;
                }

                if(val == 0){
                    menuVoluntario(v.getCodVol());
                    menuInicial(1);
                }
            }
        }
        else if(l != null){
            while(val != 0) {
                System.out.print("Password: ");
                password = input.nextLine();
                val = 0;
                if (!(l.getPassword().equals(password))) {
                    System.out.println("Password incorreta.");
                    val = 1;
                }

                if (val == 0){
                    menuLoja(l.getCodLoja());
                    menuInicial(1);
                }
            }
        }

        else{
            System.out.println("Usuário não encontrado.");
        }
        input.close();
    }

    private void menuUtilizador(String codUser){
        int r = 1;
        while(r == 1){
            Utilizador u = data.codUserToU(codUser);
            while(data.existeRespPend(u)){
                data.gereRespPend(u);
                u = data.codUserToU(codUser);
            }
            int op = utilizadorUI.exec();
            switch(op){
                case 1: System.out.println(u.toString());
                    break;
                case 2: data.criarEncomenda(codUser);
                    break;
                case 3: data.avaliaEncomenda(codUser);
                    break;
                case 4: data.histEncomendas(u);
                    break;
                case 0: r = 0;
                    break;
            }
        }
    }

    private void menuVoluntario(String codVol){
        int r = 1;
        while(r == 1){
            Voluntario v = data.codVolToV(codVol);
            int op = voluntarioUI.exec();
            switch(op){
                case 1: data.verPerfilVol(v);
                    break;
                case 2: data.escolheEncVol(v);
                    break;
                case 3: data.veAvaliacoes(codVol);
                    break;
                case 4: data.histEncomendas(v);
                    break;
                case 0: r = 0;
                    break;
            }
        }
    }

    private void menuTransportadora(String codTran){
        int r = 1;
        while(r == 1){
            Transportadora  t = data.codTranToT(codTran);
            int op = transportadoraUI.exec();
            switch(op){
                case 1: data.verPerfilTran(t);
                    break;
                case 2: data.escolheEncTran(t);
                    break;
                case 3: data.veAvaliacoes(codTran);
                    break;
                case 4: data.histEncomendas(t);
                    break;
                case 0: r = 0;
                    break;
            }
        }
    }

    private void menuQueries(){
        int r = 1;
        while(r == 1){
            int op = queriesUI.exec();
            switch(op){
                case 1: data.totFatTrans();
                    break;
                case 2: data.dezUsrMaisUsaram();
                    break;
                case 3: data.dezTranMaisUsaram();
                    break;
                case 0: r = 0;
                    break;
            }
        }
    }
    /* Load Save */

    /**
     * Carrega os dados
     */
    private void load() {
        try {
            this.data = Dados.abrirFicheiro("save.txt");
            System.out.println("\nDados carregados");
        }
        catch (FileNotFoundException e){
            System.out.println("Dados nao lidos!\nFicheiro nao encontrado!");
            data = new Dados();
        }
        catch (ClassNotFoundException e) {
            System.out.println("Dados nao lidos!\nFormato desconhecido!");
            data = new Dados();
        }
        catch (IOException e) {
            System.out.println("Dados nao lidos!\nErro de leitura!");
            data = new Dados();
        }
        catch (ClassCastException e){
            System.out.println("Dados nao lidos!\nErro de formato.");
            data = new Dados();
        }
    }
    
    /**
     * Guarda os dados
     */
    private void save() {
        try {
            this.data.guardaFicheiro("save.txt");
            System.out.println("\nDados guardados");
        }                  
        catch (FileNotFoundException e) {
            e.printStackTrace();
            System.out.println("Dados nao guardados!\nFicheiro nao encontrado.");
            data = new Dados();
        }
        catch (IOException e) {
            e.printStackTrace();
            System.out.println("Dados nao guardados!\nErro de escrita.");
            data = new Dados();
        }
    }

    /* Main*/
    public static void main(String[] args) {
       new Aplicacao().menuInicial(0);
    }
}
