package MVC.Controller;

import MVC.*;
import MVC.Models.*;
import MVC.Models.BaseModels.Encomenda;
import MVC.Models.BaseModels.LinhaEncomenda;
import MVC.Models.BaseModels.Transportadora;
import MVC.Models.BaseModels.Utilizador;
import MVC.Exceptions.InvalidInputException;
import MVC.Exceptions.NaoExisteException;
import MVC.Input;
import MVC.Views.ListView;
import MVC.Views.MenuView;
import MVC.Views.MessageView;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class Controller {
    private Model model;
    private IView view;
    private String user;

    private List<String> mainMenuOpcoes(){
        List<String> r = new ArrayList<>();
        r.add("Login");
        r.add("Registar novo Utilizador/Loja/Voluntario/Transportadora");
        r.add("Carregar/Gravar estado do sistema");
        r.add("top 10 utilizadores");
        r.add("top 10 transportadoras");
        return r;
    }

    private List<String> menuUtilizadorOpcoes(){
        List<String> r = new ArrayList<>();
        Utilizador u = this.model.getUtilizador(this.user);
        r.add("Criar Encomenda");
        r.add("Aceder à entregas de um Voluntario/Transportadora");
        r.add("Encomendas por aceitar("+u.getPorAceitar().size()+")");
        r.add("Encomendas por classificar("+u.getPorClassificar().size()+")");
        r.add("Encomendas clasificadas("+u.getCodencomendas().size()+")");
        return r;
    }

    private List<String> menuEntregadorOpcoes(){
        List<String> r = new ArrayList<>();
        r.add("Sinalizar a disponibilizacao para a recolha e encomendas");
        if(this.user.charAt(0)=='t')
            r.add("Total faturado pela transportadora");
        return r;
    }

    private List<String> menuLojaOpcoes(){
        List<String> r = new ArrayList<>();
        r.add("Adicionar novo produto");
        return r;
    }

    private List<String> menuRegisto(){
        List<String> r = new ArrayList<>();
        r.add("Utilizador");
        r.add("Voluntario");
        r.add("Transportadora");
        r.add("Loja");
        return r;
    }

    public void start(){
        try{
            this.model.carregaLog();
            this.view = new MenuView(this.mainMenuOpcoes());
            String op;
            do{
                this.view.show();
                op = Input.lerString();
                op = op.toLowerCase();
                switch(op){
                    case "1" :
                        this.login();
                        break;
                    case "2":
                        this.registo();
                        break;
                    case "3":
                        this.gravaCarregaEstado();
                        break;
                    case "4":
                        this.top10Utilizadores();
                        break;
                    case "5":
                        this.top10Transportadoras();
                        break;
                    case "0":
                        break;
                    default:
                        new MessageView().show("Opcao Invalida!!!");
                        break;
                }
            }while(!op.equals("0"));
        } catch (NaoExisteException e){
            new MessageView().show("Erro: " + e.getMessage());
        }
    }

    private void login(){
        this.view = new MessageView();
        String user;
        String senha;
        String op = "";
        boolean val;
        do{
            this.view.show("Username: ");
            user = Input.lerString().toLowerCase();
            this.view.show("Senha: ");
            senha = Input.lerString().toLowerCase();
            val = model.loginValido(user,senha);
            if(!val) {
                this.view.show("Username/Senha Errado");
                this.view.show("Deseja voltar para o menu principal?");
                this.view.show("1 - Sim");
                this.view.show("0 - Nao");
                op = Input.lerString();
                if(!op.equals("0") && !op.equals("1"))
                    this.view.show("Opcao Invalida!!!");
            }
        }while(!val && !op.equals("1"));
        if(val) {
            this.user = user;
            switch (this.user.charAt(0)) {
                case 'u':
                    this.menuUtilizador();
                    break;
                case 'v':
                    this.menuEntregador();
                    break;
                case 't':
                    this.menuEntregador();
                    break;
                case 'l':
                    this.menuLoja();
                    break;
            }
        }
        else
            this.view = new MenuView(mainMenuOpcoes());
    }

    private void menuUtilizador(){
        this.view = new MenuView(this.menuUtilizadorOpcoes());
        String op;
        do{
            this.view.show();
            op = Input.lerString();
            op = op.toLowerCase();
            switch(op){
                case "1" :
                    this.criaEncomenda();
                    break;
                case "2" :
                    this.encomendasEntregador();
                    break;
                case "3":
                    this.aceitaEncomenda();
                    break;
                case "4":
                    this.classificaEncomenda();
                    break;
                case "5":
                    this.encomendasClassificadas();
                    break;
                case "0":
                    break;
                default:
                    new MessageView().show("Opcao Invalida!!!");
                    break;
            }
        }while(!op.equals("0"));
        this.user = "";
        this.view = new MenuView(this.mainMenuOpcoes());
    }

    private void encomendasEntregador(){
        this.view = new ListView<Encomenda>();
        String op;
        List<Encomenda> l;
        boolean valido = false;
        String ent;
        do {
            this.view.show("Insira o código do voluntário/transportadora:");
            ent = Input.lerString();
            if(this.model.existeEntregador(ent))
                valido = true;
            else
                this.view.show("Código inexistente");
        }while(!valido);
        l = this.model.getEncomendasEntregador(ent);
        if(l.size()==0)
            this.view.show("O Entregador ainda nao fez nenhuma entrega!");
        else {
            ((ListView) this.view).setList(l);
            this.view.show();
        }
        this.view.show("0-Sair");
        op = Input.lerString();
        while(!op.equals("0")){
            this.view.show("Opcao Invalida!!!");
            op = Input.lerString();
        }
        this.view = new MenuView(this.menuUtilizadorOpcoes());
    }

    private void criaEncomenda(){
        this.view = new ListView<>(this.model.getListaLojas());
        List<LinhaEncomenda> le = new ArrayList<>();
        String op;
        boolean valido;
        String loj;
        this.view.show();
        do {
            valido = true;
            this.view.show("Insira o codigo da loja: ");
            loj = Input.lerString();
            try {
                ((ListView) this.view).setList(this.model.getProdutosLoja(loj));
            } catch (NaoExisteException e){
                new MessageView().show("Erro! Nao existe a loja: " + e.getMessage());
                valido = false;
            }
        } while (!valido);
        do{
            this.view.show();
            do {
                valido = true;
                this.view.show("Insira o codigo do produto: ");
                String prod = Input.lerString();
                this.view.show("Insira a quantidade desejada: ");
                double quant = Input.lerFloat();
                try {
                    le.add(this.model.criaLinha(loj, prod, quant));
                } catch (NaoExisteException e){
                    new MessageView().show("Erro, o produto: " + e.getMessage() + " nao existe.");
                    valido = false;
                } catch (InvalidInputException e){
                    new MessageView().show("Erro, quantidade inválida: " + e.getMessage());
                    valido = false;
                }
            } while (!valido);
            do {
                this.view.show("Deseja comprar mais produtos?");
                this.view.show("1 - Sim");
                this.view.show("0 - Nao");
                op = Input.lerString();
                if (!op.equals("0") && !op.equals("1"))
                    this.view.show("Opcao Invalida!!!");
            }while(!op.equals("0") && !op.equals("1"));
        }while(!op.equals("0"));
        try {
            String codE = this.model.criaEncomenda(this.user,loj,le);
            if(codE.equals(""))
                this.view.show("Nao foi possivel criar a encomenda, nao existe nenhum voluntario/transportadora no seu alcance!");
            else{
                this.view.show("Encomenda criada com sucesso!");
                if (codE.charAt(0)=='v') {
                    this.view.show("Encomenda Recebida!");
                }
            }
            this.view = new MenuView(this.menuUtilizadorOpcoes());
        } catch (NaoExisteException e){
            new MessageView().show("Erro! Nao existe a encomenda: " + e.getMessage());
        }
    }

    private void aceitaEncomenda(){
        this.view = new ListView<Encomenda>();
        String enc;
        String op1;
        String op2;
        boolean valido;
        do {
            List<Encomenda> l = this.model.getListaEncomendasPorAceitar(this.user);
            if(l.size()==0) {
                this.view.show("Nao existem encomendas por aceitar!");
                op1 = "0";
            }
            else {
                ((ListView) this.view).setList(l);
                this.view.show();
                this.view.show("Deseja aceitar ou recusar alguma encomenda?");
                this.view.show("1 - Sim");
                this.view.show("0 - Nao");
                op1 = Input.lerString();
                switch (op1) {
                    case "1":
                        this.view.show("Insira o codigo da encomenda que deseja aceitar/recusar:");
                        enc = Input.lerString();
                        this.view.show("Deseja aceitar ou recusar a encomenda?");
                        this.view.show("1 - Aceitar");
                        this.view.show("0 - Recusar");
                        op2 = Input.lerString();
                        do {
                            switch (op2) {
                                case "1":
                                    try {
                                        valido = this.model.utilizadorAceitaEncomenda(this.user, enc, true);
                                        if (valido) {
                                            this.view.show("A encomenda foi aceite!");
                                            this.view.show("Encomenda Recebida!");
                                        } else
                                            this.view.show("Codigo de encomenda invalido!");
                                        break;
                                    } catch(NaoExisteException e){
                                        new MessageView().show("Erro! Nao existe encomenda: " + e.getMessage());
                                    }

                                case "0":
                                    try {
                                        valido = this.model.utilizadorAceitaEncomenda(this.user, enc, false);
                                        if (valido)
                                            this.view.show("A encomenda foi recusada!");
                                        else
                                            this.view.show("Codigo de encomenda invalido!!");
                                        break;
                                    } catch (NaoExisteException e){
                                        new MessageView().show("Erro! Nao existe encomenda: " + e.getMessage());
                                    }

                                default:
                                    this.view.show("Opcao Invalida!!!");
                            }
                        } while (!op2.equals("0") && !op2.equals("1"));
                        break;
                    case "0":
                        break;
                    default:
                        this.view.show("Opcao Invalida!!!");
                }
            }
        }while (!op1.equals("0"));
        this.view = new MenuView(menuUtilizadorOpcoes());
    }

    private void classificaEncomenda(){
        this.view = new ListView<Encomenda>();
        String enc = "";
        String op1 = "";
        int nota = 0;
        boolean valido;
        do {
            List<Encomenda> l = this.model.getListaEncomendasPorClassificar(this.user);
            if(l.size()==0) {
                this.view.show("Nao existem encomendas por classificar!");
                op1 = "0";
            }
            else {
                ((ListView) this.view).setList(l);
                this.view.show();
                this.view.show("Deseja classificar alguma encomenda?");
                this.view.show("1 - Sim");
                this.view.show("0 - Nao");
                op1 = Input.lerString();
                switch (op1) {
                    case "1":
                        try {
                            this.view.show("Insira o codigo da encomenda que deseja classificar:");
                            enc = Input.lerString();
                            do {
                                this.view.show("Insira a classificacao da encomenda (1-5):");
                                nota = Input.lerInt();
                                if (nota > 0 && nota < 6) {
                                    valido = this.model.utilizadorClassificaEncomenda(this.user, enc, nota);
                                    if (valido)
                                        this.view.show("A encomenda foi classificada!");
                                    else {
                                        this.view.show("Codigo de encomenda invalido!!");
                                    }
                                } else
                                    this.view.show("Classificacao invalida! A classificacao tem de ser entre 1 e 5!");
                            } while (nota < 1 || nota > 5);
                            break;
                        } catch (NaoExisteException e){
                            new MessageView().show("Erro: Não existe Encomenda com o código " + e.getMessage());
                        }

                    case "0":
                        break;
                    default:
                        this.view.show("Opcao Invalida!!!");
                }
            }
        }while (!op1.equals("0"));
        this.view = new MenuView(menuUtilizadorOpcoes());
    }

    private void encomendasClassificadas(){
        this.view = new ListView<Encomenda>();
        String op;
        do {
            List<Encomenda> l = this.model.getListaEncomendasClassificadas(this.user);
            if(l.size()==0) {
                this.view.show("Nao existem encomendas classificadas!");
                op = "0";
            }
            else {
                ((ListView) this.view).setList(l);
                this.view.show();
                this.view.show("0 - Sair");
                op = Input.lerString();
            }
        }while (!op.equals("0"));
        this.view = new MenuView(menuUtilizadorOpcoes());
    }

    private void menuEntregador(){
        this.view = new MenuView(this.menuEntregadorOpcoes());
        String op;
        do{
            this.view.show();
            op = Input.lerString();
            op = op.toLowerCase();
            if(this.user.charAt(0)=='t'){
                switch(op){
                    case "1" :
                        setDisponibilidadeEntregador();
                        break;
                    case "2":
                        this.factTotal();
                    case "0":
                        break;
                    default:
                        new MessageView().show("Opcao Invalida!!!");
                        break;
                }
            }
            else {
                switch (op) {
                    case "1":
                        setDisponibilidadeEntregador();
                        break;
                    case "0":
                        break;
                    default:
                        new MessageView().show("Opcao Invalida!!!");
                        break;
                }
            }
        }while(!op.equals("0"));
        this.user = "";
        this.view = new MenuView(this.mainMenuOpcoes());
    }

    private void menuLoja(){
        this.view = new MenuView(this.menuLojaOpcoes());
        String op = "";
        do{
            this.view.show();
            op = Input.lerString();
            op = op.toLowerCase();
            switch(op){
                case "1" :
                    this.adicionaNovoProduto();
                    break;
                case "0":
                    break;
                default:
                    new MessageView().show("Opcao Invalida!!!");
                    break;
            }
        }while(!op.equals("0"));
        this.user = "";
        this.view = new MenuView(this.mainMenuOpcoes());
    }

    private void setDisponibilidadeEntregador(){
        this.view = new MessageView();
        String op;
        do{
            this.view.show("Esta disponivel para recolher encomendas? ");
            this.view.show("1 - Sim");
            this.view.show("0 - Nao");
            op = Input.lerString();
            switch(op){
                case "1" :
                    this.model.setEstaLivreEntregador(this.user,true);
                    break;
                case "0":
                    this.model.setEstaLivreEntregador(this.user,false);
                    break;
                default:
                    new MessageView().show("Opcao Invalida!!!");
                    break;
            }
        }while(!op.equals("0") && !op.equals("1"));
        this.view = new MenuView(this.menuEntregadorOpcoes());
    }

    private void factTotal(){
        this.view = new MessageView();
        String op;
        do {
            this.view.show("Total Faturado: " + String.format("%.2f",this.model.getFaturacao(this.user)));
            this.view.show("0 - Sair");
            op = Input.lerString();
            if(!op.equals("0"))
                this.view.show("Opcao Invalida!!!");
        }while (!op.equals("0"));
        this.view = new MenuView(this.menuEntregadorOpcoes());
    }

    private void adicionaNovoProduto(){
        this.view = new MessageView();
        String op;
        boolean valid;
        double p;
        this.view.show("Insira o nome do produto: ");
        String name = Input.lerString();
        do {
            this.view.show("Insira o preco do produto: ");
            p = Input.lerDouble();
            if(p<0)
                this.view.show("Preco tem de ser >0 !");
        }while(p<0);
        do {
            this.view.show("O produto e um medicamento? ");
            this.view.show("1 - Sim");
            this.view.show("0 - Nao");
            op = Input.lerString();
            if(!op.equals("1")&&!op.equals("0"))
                this.view.show("Opcao Invalida!!!");
        }while (!op.equals("1")&&!op.equals("0"));
        switch(op){
            case "1" :
                valid = this.model.addProduto(this.user,name,p,true);
                if(valid)
                    this.view.show("Produto adicionado com sucesso!");
                else
                    this.view.show("Nao foi posivel adicionar o produto!");
                break;
            case "0":
                valid = this.model.addProduto(this.user,name,p,false);
                if(valid)
                    this.view.show("Produto adicionado com sucesso!");
                else
                    this.view.show("Nao foi posivel adicionar o produto!");
                break;
            default:
                new MessageView().show("Opcao Invalida!!!");
                break;
            }
        this.view = new MenuView(this.menuLojaOpcoes());
    }

    private void registo(){
        this.view = new MenuView(this.menuRegisto());
        String name;
        double lat;
        double lon;
        double raio;
        double preco;
        int capacidade;
        String cod;
        String op;
        String isMedic;
        do{
            this.view.show();
            op = Input.lerString();
            switch(op){
                case "1" :
                    this.view.show("Insira o nome do utilizador: ");
                    name = Input.lerString();
                    this.view.show("Insira a latitude da localizacao do utilizador: ");
                    lat = Input.lerDouble();
                    this.view.show("Insira a longitude da localizacao do utilizador: ");
                    lon = Input.lerDouble();
                    cod = this.model.addUtilizador(name,lat,lon);
                    if(cod.equals(""))
                        this.view.show("Nao foi possivel criar um novo utilizador");
                    else {
                        this.view.show("Utilizador criado com sucesso!");
                        this.view.show("username: " + cod);
                        this.view.show("password: " + cod);
                    }
                    break;
                case "2" :
                    do {
                        this.view.show("O voluntario pode transportar encomendas medicas? ");
                        this.view.show("1 - Sim");
                        this.view.show("0 - Nao");
                        isMedic = Input.lerString();
                        if(!isMedic.equals("1") && !isMedic.equals("0"))
                            this.view.show("Opcao Invalida!!!");
                    }while(!isMedic.equals("1") && !isMedic.equals("0"));
                    this.view.show("Insira nome do voluntario: ");
                    name = Input.lerString();
                    this.view.show("Insira a latitude da localizacao do voluntario: ");
                    lat = Input.lerDouble();
                    this.view.show("Insira a longitude da localizacao do voluntario: ");
                    lon = Input.lerDouble();
                    do{
                        this.view.show("Insira o raio que o voluntario esta disposto a entregar: ");
                        raio = Input.lerDouble();
                        if(raio<0)
                            this.view.show("O raio tem de ser >0 !");
                    }while (raio<0);
                    if(isMedic.equals("1"))
                        cod = this.model.addVoluntario(name,lat,lon,raio,true);
                    else
                        cod = this.model.addVoluntario(name,lat,lon,raio,false);
                    if(cod.equals(""))
                        this.view.show("Nao foi possivel criar um novo voluntario");
                    else {
                        this.view.show("Voluntario criado com sucesso!");
                        this.view.show("username: " + cod);
                        this.view.show("password: " + cod);
                    }
                    break;
                case "3":
                    do {
                        this.view.show("A transportadora pode transportar encomendas medicas? ");
                        this.view.show("1 - Sim");
                        this.view.show("0 - Nao");
                        isMedic = Input.lerString();
                        if(!isMedic.equals("1") && !isMedic.equals("0"))
                            this.view.show("Opcao Invalida!!!");
                    }while(!isMedic.equals("1") && !isMedic.equals("0"));
                    this.view.show("Insira o nome da transportadora: ");
                    name = Input.lerString();
                    this.view.show("Insira a latitude da localizacao da transportadora: ");
                    lat = Input.lerDouble();
                    this.view.show("Insira a longitude da localizacao da transportadora: ");
                    lon = Input.lerDouble();
                    this.view.show("Insira o nif da transportadora: ");
                    String nif = Input.lerString();
                    do{
                    this.view.show("Insira o raio que a transportadora está disposta a entregar: ");
                    raio = Input.lerDouble();
                        if(raio<0)
                            this.view.show("O Raio tem de ser >0 !");
                    }while (raio<0);
                    do {
                        this.view.show("Insira o preco por km da transportadora: ");
                        preco = Input.lerDouble();
                        if(preco<0)
                            this.view.show("O preco tem de ser >0 !");
                    }while (preco<0);
                    do {
                        this.view.show("Insira a capacidade de encomendas que a transportadora pode fazer: ");
                        capacidade= Input.lerInt();
                        if(capacidade<0)
                            this.view.show("A capacidade tem de ser >0 !");
                    }while (capacidade<0);
                    if(isMedic.equals("1"))
                        cod = this.model.addTransportadora(name,lat,lon,nif,raio,preco,capacidade,true);
                    else
                        cod = this.model.addTransportadora(name,lat,lon,nif,raio,preco,capacidade,false);
                    if(cod.equals(""))
                        this.view.show("Nao foi possivel criar uma nova transportadora");
                    else {
                        this.view.show("Transportadora criada com sucesso!");
                        this.view.show("username: " + cod);
                        this.view.show("password: " + cod);
                    }
                    break;
                case "4" :
                    this.view.show("Insira o nome da loja: ");
                    name = Input.lerString();
                    this.view.show("Insira a latitude da localizacao da loja: ");
                    lat = Input.lerDouble();
                    this.view.show("Insira a longitude da localizacao da loja: ");
                    lon = Input.lerDouble();
                    cod = this.model.addLoja(name,lat,lon);
                    if(cod.equals(""))
                        this.view.show("Nao foi possivel criar uma nova loja");
                    else {
                        this.view.show("Loja criada com sucesso!");
                        this.view.show("username: " + cod);
                        this.view.show("password: " + cod);
                    }
                    break;
                case "0":
                    break;
                default:
                    new MessageView().show("Opcao Invalida!!!");
                    break;
            }
        }while(!op.equals("0"));
        this.view = new MenuView(this.mainMenuOpcoes());
    }

    private void gravaCarregaEstado(){
        this.view = new MessageView();
        String op;
        boolean success = true;
        do{
            this.view.show("1 - Gravar");
            this.view.show("0 - Carregar");
            op = Input.lerString();
            switch(op){
                case "1" :
                    try {
                        this.model.gravaEstado();
                    }catch (IOException e){
                        success = false;
                        this.view.show("Erro: " + e.getMessage());
                    }
                    if(success)
                        this.view.show("Estado gravado com sucesso!");
                    break;
                case "0":
                    try {
                    model.carregaEstado();
                    }catch (IOException | ClassNotFoundException e){
                        success = false;
                        this.view.show("Erro: " + e.getMessage());
                    }
                    if(success)
                        this.view.show("Estado carregado com sucesso!");
                    break;
                default:
                    new MessageView().show("Opcao Invalida!!!");
                    break;
            }
        }while(!op.equals("0") && !op.equals("1"));
        this.view = new MenuView(this.mainMenuOpcoes());
    }

    private void top10Utilizadores(){
        this.view = new ListView<Utilizador>();
        ((ListView) this.view).setList(this.model.getTop10Utilizadores());
        String op;
        do {
            this.view.show();
            this.view.show("0 -Sair");
            op = Input.lerString();
        }while (!op.equals("0"));
        this.view = new MenuView(mainMenuOpcoes());
    }

    private void top10Transportadoras(){
        this.view = new ListView<Transportadora>();
        ((ListView) this.view).setList(this.model.getTop10Transportadoras());
        String op;
        do {
            this.view.show();
            this.view.show("0 -Sair");
            op = Input.lerString();
        }while (!op.equals("0"));
        this.view = new MenuView(mainMenuOpcoes());
    }

    public void setModel(Model model) {
        this.model = model;
    }

    public void setView(IView view) {
        this.view = view;
    }

    public void setUser(String user) {

        this.user = user;

    }
}
