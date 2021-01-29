package Controller;

import Model.*;
import Model.Leitura.IRWEstado;
import Model.Leitura.IReadFile;
import Model.Leitura.RWEstado;
import Model.Leitura.ReadFile;
import Model.Logins.ILogin;
import Model.Logins.Login;
import Model.Tipos.*;
import View.*;

import java.io.IOException;
import java.util.*;

/**
 * Controller geral que faz as opções comuns a todos os models.
 */
public class AppController implements IAppController {
    private ISistema sistema;  //sistema para fazer set na main
    private IAppView view;     //view para fazer set main
    private INavegador nav;    //navegador
    private IReadFile lerFiles; //readfile
    private String file; //ficheiro
    private int opcao;   //opção que insere
    private IRWEstado rw;  //rwestado


    public AppController(){
        this.file = " ";
        this.nav = new Navegador();
        this.lerFiles = new ReadFile();
        this.opcao = -1;
        this.rw = new RWEstado();
    }

    public void setSistema(ISistema sistema){
        this.sistema = sistema;
    }

    public void setAppView(IAppView view) {
        this.view = view;
    }

    /**
     * switch case para as opções que surgem no menu da view preInicio
     */
    public ISistema runController() {
        Scanner ler = new Scanner(System.in);
        do{
            view.preInicio();
            opcao = ler.nextInt();
            switch (opcao){
                case 1:{
                    view.printMensagem("Insira um ficheiro de leitura: ");
                    ler = new Scanner(System.in);
                    file = ler.nextLine();
                    lerFiles.leitura(file,sistema);
                    opcao = 0;
                    break;
                }
                case 2: {
                    view.printMensagem("Insira o nome do ficheiro:");
                    ler = new Scanner(System.in);
                    String f = ler.nextLine();
                    rw.setFileIn(f);
                    try {
                        view.printMensagem("Loading ...");
                        setSistema(rw.loadData());
                    } catch (ClassNotFoundException | IOException e) {
                        e.printStackTrace();
                    }
                    System.out.println(sistema.toString());
                    opcao = 0;
                    break;
                }
            }
        }while(opcao!=0);
        return sistema;
    }

    /*Esta função faz login ou regista na aplicação.
     * Faz return do char correspondente ao modo.
     * Caso seja 'l' -> Loja;
     * Caso seja 'u' -> user;
     * Caso seja 't' -> empresa;
     * Caso seja 'v' -> voluntário.
     */

    public char signUp (){
        Scanner ler = new Scanner(System.in);
        String email,pass;
        char mode = '0';
        do {
            view.inicio();
            opcao = ler.nextInt();

            switch (opcao) {
                case 1: { //registo

                    view.registo();
                    ler = new Scanner(System.in);
                    String tipo = ler.nextLine();
                    if (!tipo.equals("0")){
                        if (tipo.equals("1")) {
                            regista(1);
                        }
                        if (tipo.equals("2")){

                            regista(2);
                        }
                        if (tipo.equals("3")){

                            regista(3);
                        }
                        if (tipo.equals("4")) {
                            regista(4);
                        }
                    }
                    break;
                }
                case 2:{ //login
                    view.login(0);
                    email = ler.nextLine(); //ver o que se passa aqui
                    view.login(1);
                    email = ler.nextLine();
                    view.login(2);
                    pass = ler.nextLine();
                    ILogin login = new Login();
                    login.setEmail(email);
                    login.setPassword(pass);
                    while(!sistema.getLogins().existsLogin(login)){
                        view.login(3);
                        view.login(1);
                        email = ler.nextLine();
                        view.login(2);
                        pass = ler.nextLine();
                        login.setEmail(email);
                        login.setPassword(pass);
                    }
                    mode = email.charAt(0);
                    sistema.setQuem(login);
                    view.printMensagem("Login concluído!!\n");
                    //System.out.println("Login : " + login.toString());
                    opcao = 0;
                    break;
                }
                case 3:{
                    view.printMensagem("Indique o seu email");
                    ler = new Scanner(System.in);
                    String email2 = ler.nextLine();
                    view.printMensagem("Indique a palavra passe antiga");
                    ler = new Scanner(System.in);
                    String pass2 = ler.nextLine();
                    ILogin login = new Login();
                    login.setEmail(email2);
                    login.setPassword(pass2);
                    if (sistema.getLogins().existsLogin(login)) {
                        view.printMensagem("Insira a password nova");
                        ler = new Scanner(System.in);
                        String pass3 = ler.nextLine();
                        String id = sistema.getLogins().getCodigoID(email2);
                        ILogin log = sistema.getLogins().getLog(id);
                        log.setPassword(pass3);
                    }
                    else view.printMensagem("Login inexistente");
                    System.out.println(sistema.toString());

                    break;
                }
                case 4:{
                    view.printMensagem("Insira um nome para o ficheiro .dat");
                    ler = new Scanner(System.in);
                    String f = ler.nextLine();
                    rw.setFileOut(f);
                    try {
                        view.printMensagem("Saving ...");
                        rw.saveData(this.sistema);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                    break;
                }
                case 5:{
                    ITipo[] res = sistema.top10Users();
                    Integer[] n = sistema.getNComprasUser(res);
                    view.top(res,n);
                    break;
                }
                case 6:
                    String[] res = sistema.getFilaEntregues().top10Empresas();
                    Float[] dist = sistema.getFilaEntregues().distEmpresa(res);
                    view.top2(res,dist);
                    break;
            }
        } while(opcao!=0);
        //view.printMensagem("Adeus!!");
        return mode;
    }

    public void regista (int tipo) {
        /**
         Cria o id
         */
        String id = "";
        Random rn = new Random();
        int id1 = rn.nextInt(100);
        id = String.valueOf(id1);

        /**
         * Lê as variáveis comuns a todos
         */
        view.reg(1);
        Scanner ler = new Scanner (System.in);
        String nome = ler.nextLine();
        view.reg(2);
        ler = new Scanner(System.in);
        float x = ler.nextFloat();
        view.reg(3);
        ler = new Scanner(System.in);
        float y = ler.nextFloat();

        /**
         Tipo do user
         */

        if(tipo==1) {
            id = "u" + id;
            String pass = id + "_0000";
            String email = id+ "@trazAqui.com";
            ITipo user = new User();
            ILogin login = new Login();
            login.setPassword(pass);
            login.setEmail(email);
            while (sistema.getLogins().existsLogin(login)){
                rn = new Random();
                id1 = rn.nextInt(100);
                id = "u" + String.valueOf(id1);
            }
            pass = id + "_0000";
            email = id+ "@trazAqui.com";
            login.setPassword(pass);
            login.setEmail(email);
            sistema.getLogins().addLogin(login,id);
            user.setNome(nome);
            user.setX(x);
            user.setY(y);
            user.setId(id);
            sistema.getUsers().addTipo(user);
        }

        /**
         Tipo do voluntário
         */
        if(tipo==2) {
            id = "v" + id;
            String pass = id + "_0000";
            String email = id+ "@trazAqui.com";
            ILogin login = new Login();
            login.setPassword(pass);
            login.setEmail(email);
            ITipo vol = new Voluntario();
            while (sistema.getLogins().existsLogin(login)){
                rn = new Random();
                id1 = rn.nextInt(100);
                id = "v"+ String.valueOf(id1);
            }
            pass = id + "_0000";
            email = id+ "@trazAqui.com";
            login.setPassword(pass);
            login.setEmail(email);

            vol.setNome(nome);;
            vol.setX(x);
            vol.setY(y);
            vol.setId(id);
            view.reg(4);
            ler= new Scanner(System.in);
            float raio = ler.nextFloat();
            ((Voluntario)vol).setRadius_volunteer(raio);
            sistema.getVoluntarios().addTipo(vol);
            sistema.getLogins().addLogin(login,id);

        }

        /**
         * Tipo de loja
         */
        if (tipo==3){
            id = "l" + id;
            String pass = id + "_0000";
            String email = id+ "@trazAqui.com";
            ILogin login = new Login();
            login.setPassword(pass);
            login.setEmail(email);
            ITipo loja = new Loja();

            while (sistema.getLogins().existsLogin(login)){
                rn = new Random();
                id1 = rn.nextInt(100);
                id = "l" + String.valueOf(id1);
            }
            pass = id + "_0000";
            email = id+ "@trazAqui.com";

            loja.setY(y);
            loja.setX(x);
            loja.setId(id);
            loja.setNome(nome);
            login.setPassword(pass);
            login.setEmail(email);

            sistema.getLojas().addTipo(loja);
            sistema.getLogins().addLogin(login,id);
        }

        /**
         * Tipo de empresa
         */
        if (tipo==4) {
            id = "t" + id;
            String pass = id + "_0000";
            String email = id+ "@trazAqui.com";
            ILogin login = new Login();
            login.setPassword(pass);
            login.setEmail(email);
            ITipo emp = new Empresa();
            while (sistema.getLogins().existsLogin(login)){
                rn = new Random(100);
                id1 = rn.nextInt(100);
                id = "t" + String.valueOf(id1);
            }
            pass = id + "_0000";
            email = id+ "@trazAqui.com";
            login.setPassword(pass);
            login.setEmail(email);
            view.reg(4);
            float raio = ler.nextFloat();

            view.reg(5);
            ler= new Scanner(System.in);
            int nif = ler.nextInt();

            view.reg(6);
            ler= new Scanner(System.in);
            float preco = ler.nextFloat();

            ((Empresa)emp).setNif(nif);
            emp.setNome(nome);
            emp.setId(id);
            emp.setX(x);
            emp.setY(y);
            ((Empresa)emp).setPreco(preco);
            ((Empresa)emp).setRaio(raio);
            sistema.getEmpresas().addTipo(emp);
            System.out.println(emp.toString());
            sistema.getLogins().addLogin(login,id);
        }
        view.printMensagem("Registo concluído com sucesso");

        String pass = id + "_0000";
        String email = id+ "@trazAqui.com";
        view.printMensagem("O seu email: " +email);
        view.printMensagem("A sua password é: " +pass);
        System.out.println(sistema.toString());

    }
}
