package Projeto;


import java.sql.SQLOutput;
import java.util.ArrayList;
import java.util.Arrays;

public class Controller {
    private TrazAqui model;
    private Menu menu;
    private Utilizador utlizador;
    private Loja loja;
    private Encomenda encomenda;
    private Pedido pedido;

    public Controller(TrazAqui model) {
        this.menu = new Menu();
        this.model = model;
        this.loja = null;
        this.encomenda = null;
        this.pedido = null;
    }

    public void run() {
        //Scanner scanner = new Scanner(System.in);
        //String input;
        //while(this.menu.getIsUP()){
        do {
            this.menu.options();
            switch (this.menu.scanOption()) {
                case "Load":
                    new Parser("Projeto/data/log.txt", this.model);
                    break;
                case "Login":
                    try {
                        NovoLogin loginData = menu.novoLogin();
                        this.utlizador = model.logIn(loginData.getUser(), loginData.getPassword());
                        menu.print("Login efetuado, press Enter");

                        if(loginData.getUser().startsWith("u")) {
                            this.menu.secondaryMenuAfterUserLogin();
                            switch (this.menu.scanOption()) {
                                case "Inserir":
                                    try {
                                        System.out.println(model.getLojas());
                                        System.out.println("-------------");
                                        System.out.println(model.getEncomendas());

                                        System.out.println("Escolher Loja");
                                        String loja = this.menu.scanOption();
                                        this.loja = model.getLoja(loja);

                                        System.out.println("Escolher encomenda");
                                        String encomenda = this.menu.scanOption();
                                        this.encomenda = model.getEncomenda(this.model.getEncomendas(),encomenda);

                                        this.pedido = new Pedido(this.loja,this.encomenda);
                                        System.out.println("a");
                                        System.out.println(this.pedido);
                                        System.out.println("b");
                                        Empresa empresaMaisPerto = model.calculateClosestBuilding(this.pedido);
                                        System.out.println("c");
                                        System.out.println("##################################################");
                                        System.out.println("##################################################");
                                        System.out.println("Empresa mais perto para o Pedido: " + this.pedido);
                                        System.out.println(empresaMaisPerto);
                                        System.out.println("##################################################");
                                        System.out.println("##################################################");


                                    }catch (Exception e) {
                                        menu.print("Nao reconheco essa opcao");
                                    }
                            }
                        }
                        switch(this.menu.scanOption()){
                            case "banana":
                                System.out.println("entrei ");
                                break;
                        }

                    } catch (Exception e) {
                        menu.print("User não é válido");
                    }
                    break;
                case "RegistoUtilizador":
                    int bin = 0;
                    try {
                        RegistoUtilizador regutil = menu.novoRegistoUtilizador();
                        Utilizador user = new Utilizador(regutil.getEmail(),
                                regutil.getEmail(),
                                regutil.getPassword(),
                                regutil.getPos());
                        this.model.adicionaUtilizador(user);
                        menu.print("User criado, press Enter");
                    } catch (Exception e) {
                        menu.print("O registo falhou, try again!");
                        bin = 1;
                        break;
                    }
                    break;
                case "RegistoVoluntario":
                    try {
                        RegistoVoluntario regVol = menu.novoRegistoVoluntario();
                        Voluntario voluntario = new Voluntario(regVol.getEmail(),
                                regVol.getEmail(),
                                regVol.getPassword(),
                                regVol.getPos(),
                                regVol.getRaio());
                        this.model.adicionaUtilizador(voluntario);
                        menu.print("Voluntário registado, press Enter");
                    } catch (Exception e) {
                        menu.print("O registo falhou, tente novamente");
                    }
                    break;
                case "RegistoLoja":
                    try {
                        Loja regLoja = menu.novoRegistoLoja();
                        /*Loja loja = new Loja(
                                regLoja.getEmail(),
                                regLoja.getUser(),
                                regLoja.getPassword(),
                                regLoja.getPos());*/
                        this.model.adicionaUtilizador(regLoja);
                        this.model.addLojas(regLoja);
                        menu.print("Loja criada, press Enter");
                    } catch (Exception e) {
                        menu.print("O registo falhou, try again!");
                    }
                    break;
                case "RegistoEmpresa":
                    try {
                        RegistoEmpresa regEmpresa = menu.novoRegistoEmpresa();
                        Empresa empresa = new Empresa(
                                regEmpresa.getPos(),
                                regEmpresa.getNif(),
                                regEmpresa.getRaio(),
                                regEmpresa.getTaxa());
                        this.model.adicionaUtilizador(empresa);
                        menu.print("Empresa registada com sucesso, pressione Enter");
                    } catch (Exception e) {
                        menu.print("O registo falhou, tente novamente");
                    }
                    break;
                case "Encomenda":
                    System.out.println(Arrays.toString(model.getLojas().toArray()));
                    try {
                        Encomenda newEnc = menu.novoPedidoEncomenda(this.utlizador.getEmail());
                        model.addEncomenda(newEnc);

                        model.respondEnc(newEnc);

                    } catch (Exception e) {
                        menu.print("A encomenda falhou, pressione Enter");
                    }
                    break;
            }
        }
        while (this.menu.scanOption() != "terminar");
    }

}


