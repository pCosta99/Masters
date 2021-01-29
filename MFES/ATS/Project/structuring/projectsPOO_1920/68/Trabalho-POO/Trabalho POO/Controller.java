import java.io.IOException;
import java.io.Serializable;

/**
 * Controller do programa. O programa segue o modelo MCV.
 */

public class Controller implements Serializable {

    private Sistema sistema;
    private ViewGeral view;
    private ControllerUtilizador controladorUtilizador;
    private  ControllerVoluntario controladorVoluntario;
    private ControllerTransportadora controladorTransportadora;


    /**
     * Construtor parametrizado da classe Controller.
     * @param sistema uma instãncia da classe Sistema
     */
    public Controller (Sistema sistema){
        this.sistema = sistema;
        this.view = new ViewGeral();
    }

    /**
     * Define o Sistema do controller.
     * @param sistema um Sistema
     */
    public void setSistema(Sistema sistema) {
        this.sistema = sistema;
    }

    /**
     * Define a View do controller.
     * @param view uma View
     */
    public void setView(ViewGeral view){
        this.view=view;
    }


    public boolean aceitaEntrega(Encomenda e, Transportadora t) {
        return true;
    }


    public void login() throws InterruptedException {
        String username, password;
        username = Input.lerString(); if(username.equals("q")) return;
        password = Input.lerString(); if(password.equals("q")) return;

        while(!this.sistema.userExiste(username)) {
            ViewGeral.PrintMensagem("Username não existe no sistema. Tente novamente.");
            ViewGeral.PrintMensagem("Insira o seu Username");
            username=Input.lerString(); if(username.equals("q")) return;
            ViewGeral.PrintMensagem("Insira a sua Password");
            password=Input.lerString(); if(password.equals("q")) return;
        }

        while (!this.sistema.validaLogin(username, password)){
            ViewGeral.PrintMensagem("Password incorreta. Tente novamente.");
            ViewGeral.PrintMensagem("Insira a sua Password");
            password=Input.lerString(); if(password.equals("q")) return;
        }

        AppUser user = this.sistema.getUser(username);
        if(user instanceof Utilizador) {
            Utilizador utilizador = (Utilizador) user;
            //System.out.println("UTILIZADOR: " + utilizador.toString());
            this.controladorUtilizador = new ControllerUtilizador(utilizador, this.sistema);
            this.controladorUtilizador.inicio();
        }
        else if (user instanceof Voluntario){
            Voluntario voluntario = (Voluntario) user;
            this.controladorVoluntario = new ControllerVoluntario(voluntario);
            this.controladorVoluntario.inicio();
        }
        else if (user instanceof Transporta) {
            assert user instanceof Transportadora;
            Transportadora transportadora = (Transportadora) user;
           this.controladorTransportadora = new ControllerTransportadora(transportadora);
           this.controladorTransportadora.inicio();
        }


    }


    public String registaUsername(){
        String username;
        username = Input.lerString();

        while (this.sistema.userExiste(username) && !username.equals("q")) { //VER CONDICOES, JA ESTOU BURRA COM O SONO
            ViewGeral.PrintMensagem("Username já existente. Tente novamente");
            username = Input.lerString();
        }

        if(username.equals("q")) return "q";

        return username;
    }


    public void registar(){
        String username, password, nome, nif;
        double raio, precoKm;

        int opcao = Input.lerInt();
        switch (opcao){
            case 0:
                break;
            case 1:
                this.view.RegistoUtilizador();
                nome = Input.lerString();
                username=registaUsername();
                if(username.equals("q")) break;
                password = Input.lerString();
                this.sistema.registarUtilizador(nome, username, password);
                ViewGeral.PrintMensagem("Registo realizado com sucesso.");
                break;

            case 2:
                this.view.RegistoVoluntario();
                username=registaUsername();
                if(username.equals("q")) break;
                password= Input.lerString();
                nome = Input.lerString();
                if(nome.equals("q")) break;
                raio = Input.lerDouble();
                this.sistema.registarVoluntario(username,password,nome,raio);
                ViewGeral.PrintMensagem("Registo realizado com sucesso.");
                break;

            case 3:
                this.view.RegistoTransportadora();
                username = registaUsername();
                if(username.equals("q")) break;
                password = Input.lerString();
                nome = Input.lerString();
                if(nome.equals("q")) break;
                nif = Input.lerString();
                raio = Input.lerDouble();
                precoKm = Input.lerDouble();
                this.sistema.registarTransportadora(username, password, nome, nif,raio,precoKm);
                ViewGeral.PrintMensagem("Registo realizado com sucesso.");
                break;

            default:
                ViewGeral.PrintMensagem("Opção inválida. Tente novamente.");
                opcao = Input.lerInt();
        }
}

    /**
     * Fazer Login ou registar
     */
    public void inicio() throws InterruptedException, IOException {


        int opcao = 1;

        while (opcao!=0) {
            view.bemVindo();
            opcao = Input.lerInt();
            switch (opcao) {
                case 1:
                    view.login();
                    login();
                    break;

                case 2:
                    view.RegistoEscolher();
                    registar();
                    break;

                case 3:
                    try {
                        this.sistema.guardaEstado();
                    }catch (Exception e){
                        ViewGeral.PrintMensagem("Ocorreu um erro ao tentar guardar o estado. Tente novamente.");
                    }
                    break;

                case 4:
                    view.TopUtilizadores(this.sistema.top_utilizadores(10));
                    String quit = Input.lerString();
                    while (!quit.equals("q")) {
                        ViewGeral.PrintMensagem("Opção inválida");
                        quit = Input.lerString();
                    }
                    break;
                case 5:
                    view.TopEmpresas(this.sistema.top_empresas(10));
                    String q = Input.lerString();
                    while (!q.equals("q")) {
                        ViewGeral.PrintMensagem("Opção inválida");
                        q = Input.lerString();
                    }
                    break;

                case 0:
                    ViewGeral.saida();
                    break;
                default:
                    ViewGeral.PrintMensagem("Opção Inválida. Volte a tentar");

            }
        }
    }

}