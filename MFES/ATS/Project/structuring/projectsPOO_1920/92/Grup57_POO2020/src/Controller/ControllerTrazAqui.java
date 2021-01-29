package Controller;

import View.View;
import Model.Model;
import Model.Parse;
import Model.Auxiliar;
import Model.TrazAqui;
import Model.ITrazAqui;
import java.awt.font.NumericShaper;
import java.io.IOException;
import java.util.Random;
import java.util.Scanner;
import java.lang.Character;

public class ControllerTrazAqui {

    private final View view;
    private final Model model;

    public ControllerTrazAqui() {
        this.view = new View();
        this.model = new Model();
    }

    public void startController() throws IOException {
        ITrazAqui trazAqui = new TrazAqui();
        Parse.parse(trazAqui);
        trazAqui.preencheRegistos();
        int quemFala = 1000;
        int loginOuRegisto = 1000;
        int option = 1000;
        int continuar = 1;
        int inserirMaisProdutos = 1;
        int trocarTransportadora = 2;
        int fatorAleatoriedade = 0;
        int classificacao = 0;
        double xNovo = 1000;
        double xAntigo = 1000;
        double yNovo = 1000;
        double yAntigo = 1000;
        double peso = 1000;
        double quantidade = 1000;
        double valorUnitario = 1000;
        boolean aceita = false;
        String username = new String();
        String pw = new String();
        String pwAntiga = new String();
        String pwNova = new String();
        String nomeNovo = new String();
        String nomeAntigo = new String();
        String emailNovo = new String();
        String emailAntigo = new String();
        String codEncomenda = new String();
        String codLoja = new String();
        String codProduto = new String();
        String descricao = new String();
        String registoVoluntarioTransportadora = new String();
        String quemEntrega = new String();
        Character first;
        this.view.welcomeMessage();
        this.view.printTopUsers(this.model.getTopUsers(trazAqui));
        this.view.printTopCarriers(this.model.getTopCarriers(trazAqui));
        Scanner sc = new Scanner(System.in);
        while (continuar == 1) {
            try {
                loginOuRegisto = Integer.parseInt(sc.next());
            }
            catch (NumberFormatException exception) {}
            switch (loginOuRegisto) {
                case 1 -> {
                    this.view.printSpace();
                    this.view.insertUserName();
                    username = sc.next();
                    this.view.printSpace();
                    while ((!Auxiliar.validUserName(username) || (!trazAqui.checkUserName(username)))) {
                        if (!Auxiliar.validUserName(username)) {
                            this.view.invalidUserName();
                            username = sc.next();
                            this.view.printSpace();
                        } else {
                            this.view.userNameNotFound();
                            username = sc.next();
                            this.view.printSpace();
                        }
                    }
                    this.view.insertPassWord();
                    pw = sc.next();
                    this.view.printSpace();
                    while (!trazAqui.checkCredentials(username, pw)) {
                        this.view.wrongPassWord();
                        pw = sc.next();
                        this.view.printSpace();
                    }
                    first = username.charAt(0);
                    switch (first) {
                        case 'u' -> {
                            this.view.welcomeWithUserNameAndName(username, trazAqui.extractNameByUserNameUtilizadores(username));
                            this.view.printSpace();
                            while (continuar == 1) {
                                this.view.queriesUser();
                                try {
                                    option = Integer.parseInt(sc.next());
                                }
                                catch (NumberFormatException exception) {}
                                this.view.printSpace();
                                while (option != 1 && option != 2) {
                                    this.view.deveSerDe1a2();
                                    option = Integer.parseInt(sc.next());
                                    this.view.printSpace();
                                }
                                switch (option) {
                                    case 1 -> {
                                        this.view.criarEncomenda();
                                        this.view.insertCodigoEncomenda();
                                        codEncomenda = sc.next();
                                        this.view.printSpace();
                                        this.view.insertCodigoLoja();
                                        codLoja = sc.next();
                                        while (!trazAqui.checkExisteLoja(codLoja)) {
                                            this.view.printSpace();
                                            this.view.lojaNaoExiste();
                                            codLoja = sc.next();
                                        }
                                        this.view.printSpace();
                                        this.view.insertPesoEncomenda();
                                        try {
                                            peso = Integer.parseInt(sc.next());
                                        }
                                        catch (NumberFormatException exception) {}
                                        this.view.printSpace();
                                        trazAqui.criaEncomenda(codEncomenda, username, codLoja, peso);
                                        this.view.mensagemEncomenda();
                                        while (inserirMaisProdutos == 1) {
                                            this.view.insertCodigoProduto();
                                            codProduto = sc.next();
                                            this.view.printSpace();
                                            this.view.insertDescricaoProduto();
                                            descricao = sc.next();
                                            this.view.printSpace();
                                            this.view.insertQuantidadeProduto();
                                            try {
                                                quantidade = Integer.parseInt(sc.next());
                                            }
                                            catch (NumberFormatException exception) {}
                                            this.view.printSpace();
                                            this.view.insertPrecoProduto();
                                            try {
                                                valorUnitario = Integer.parseInt(sc.next());
                                            }
                                            catch (NumberFormatException exception) {}
                                            this.view.printSpace();
                                            trazAqui.adicionaProdutos(codEncomenda, codProduto, descricao, quantidade, valorUnitario);
                                            this.view.produtoAdicionado();
                                            try {
                                                inserirMaisProdutos = Integer.parseInt(sc.next());
                                            }
                                            catch (NumberFormatException exception) {}
                                            this.view.printSpace();
                                            while (inserirMaisProdutos != 1 && inserirMaisProdutos != 2) {
                                                this.view.deveSerDe1a2();
                                                inserirMaisProdutos = Integer.parseInt(sc.next());
                                                this.view.printSpace();
                                            }
                                        }
                                        trazAqui.insereEncomendaFilaDeEspera(codLoja, trazAqui.extraiEncomenda(codLoja));
                                        this.view.printEncomendaPronta(codLoja, trazAqui.encomendaPronta(codLoja));
                                        this.view.relativamenteAoTransporte();
                                        try {
                                            option = Integer.parseInt(sc.next());
                                        }
                                        catch (NumberFormatException exception) {}
                                        while (option != 1 && option != 2) {
                                            this.view.deveSerDe1a2();
                                            option = Integer.parseInt(sc.next());
                                            this.view.printSpace();
                                        }
                                        switch (option) {
                                            case 1 -> {
                                                this.view.printSpace();
                                                this.view.quemVaiTransportar();
                                                try {
                                                    option = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                while (option != 1 && option != 2) {
                                                    this.view.deveSerDe1a2();
                                                    option = Integer.parseInt(sc.next());
                                                    this.view.printSpace();
                                                }
                                                if (option == 1) {
                                                    this.view.printSpace();
                                                    this.view.printQuemTransporta((quemEntrega = trazAqui.atribuiTransportadoraVoluntario(username, option)), option);
                                                    this.view.printSpace();
                                                    fatorAleatoriedade = trazAqui.tempoTransito();
                                                    this.view.aleatoriedadeViagem(fatorAleatoriedade);
                                                    this.view.printTempoViagem(trazAqui.tempoViagemVoluntario(username, quemEntrega, fatorAleatoriedade));
                                                    this.view.printSpace();
                                                    this.view.printClassificar();
                                                    try {
                                                        classificacao = Integer.parseInt(sc.next());
                                                    }
                                                    catch (NumberFormatException exception) {}
                                                    while ((classificacao < 0) || (classificacao > 20)) {
                                                        this.view.deveSerDe1a20();
                                                        classificacao = Integer.parseInt(sc.next());
                                                        this.view.printSpace();
                                                    }
                                                    this.view.printSpace();
                                                    trazAqui.addClassificacaoVoluntario(quemEntrega, classificacao);
                                                    this.view.printClassificacaoFinalVoluntario(quemEntrega, trazAqui.exportClassMediaVoluntario(quemEntrega));
                                                    this.view.printSpace();
                                                    this.view.desejaContinuar();
                                                    try {
                                                        continuar = Integer.parseInt(sc.next());
                                                    }
                                                    catch (NumberFormatException exception) {}
                                                    this.view.printSpace();
                                                    while (continuar != 1 && continuar != 2) {
                                                        this.view.deveSerDe1a2();
                                                        continuar = Integer.parseInt(sc.next());
                                                        this.view.printSpace();
                                                    }
                                                }
                                                if (option == 2) {
                                                    while (trocarTransportadora == 2) {
                                                        this.view.printSpace();
                                                        this.view.printQuemTransporta((quemEntrega = trazAqui.atribuiTransportadoraVoluntario(username, option)), option);
                                                        this.view.printSpace();
                                                        this.view.printCustoViagem(trazAqui.exportDistancia(username, quemEntrega),
                                                                trazAqui.custoTotalViagem(username, quemEntrega), quemEntrega);
                                                        trocarTransportadora = Integer.parseInt(sc.next());
                                                        while (trocarTransportadora != 1 && trocarTransportadora != 2) {
                                                            this.view.deveSerDe1a2();
                                                            trocarTransportadora = Integer.parseInt(sc.next());
                                                            this.view.printSpace();
                                                        }
                                                    }
                                                    fatorAleatoriedade = trazAqui.tempoTransito();
                                                    this.view.printSpace();
                                                    this.view.aleatoriedadeViagem(fatorAleatoriedade);
                                                    this.view.printTempoViagem(trazAqui.tempoViagemTransportadora(username, quemEntrega, fatorAleatoriedade));
                                                    this.view.printSpace();
                                                    this.view.printClassificar();
                                                    try {
                                                        classificacao = Integer.parseInt(sc.next());
                                                    }
                                                    catch (NumberFormatException exception) {}
                                                    while ((classificacao < 0) || (classificacao > 20)) {
                                                        this.view.deveSerDe1a20();
                                                        classificacao = Integer.parseInt(sc.next());
                                                        this.view.printSpace();
                                                    }
                                                    this.view.printSpace();
                                                    trazAqui.addClassificacaoTransportadora(quemEntrega, classificacao);
                                                    this.view.printClassificacaoFinalTransportadora(quemEntrega, trazAqui.exportClassMediaTransportadora(quemEntrega));
                                                    this.view.printSpace();
                                                    this.view.desejaContinuar();
                                                    try {
                                                        continuar = Integer.parseInt(sc.next());
                                                    }
                                                    catch (NumberFormatException exception) {}
                                                    this.view.printSpace();
                                                    while (continuar != 1 && continuar != 2) {
                                                        this.view.deveSerDe1a2();
                                                        continuar = Integer.parseInt(sc.next());
                                                        this.view.printSpace();
                                                    }
                                                }
                                            }
                                            case 2 -> {
                                                this.view.printSpace();
                                                this.view.insertTransportadoraVoluntario();
                                                registoVoluntarioTransportadora = sc.next();
                                                while ((!trazAqui.checkExisteVoluntario(registoVoluntarioTransportadora)) &&
                                                        (!trazAqui.checkExisteTransportadora(registoVoluntarioTransportadora))) {
                                                    this.view.printSpace();
                                                    this.view.voluntarioTransportadoraNaoExiste();
                                                    registoVoluntarioTransportadora = sc.next();
                                                    this.view.printSpace();
                                                }
                                                this.view.printSpace();
                                                this.view.printRegistosDeAlguem(trazAqui.registosDeAlguem(registoVoluntarioTransportadora));
                                                this.view.desejaContinuar();
                                                try {
                                                    continuar = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                this.view.printSpace();
                                                while (continuar != 1 && continuar != 2) {
                                                    this.view.deveSerDe1a2();
                                                    continuar = Integer.parseInt(sc.next());
                                                    this.view.printSpace();
                                                }
                                            }
                                        }
                                    }
                                    case 2 -> {
                                        this.view.changeUserPersonalData();
                                        try {
                                            option = Integer.parseInt(sc.next());
                                        }
                                        catch (NumberFormatException exception) {}
                                        this.view.printSpace();
                                        while (option != 1 && option != 2 && option != 3 && option != 4) {
                                            this.view.deveSerDe1a4();
                                            option = Integer.parseInt(sc.next());
                                            this.view.printSpace();
                                        }
                                        switch (option) {
                                            case 1 -> {
                                                this.view.insertNewPassWord();
                                                pw = sc.next();
                                                this.view.printSpace();
                                                pwAntiga = trazAqui.extractPassWordByUserName(username);
                                                trazAqui.changePassWord(username, pw);
                                                pwNova = trazAqui.extractPassWordByUserName(username);
                                                this.view.trocaComSucessoPw(pwAntiga, pwNova);
                                                this.view.desejaContinuar();
                                                try {
                                                    continuar = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                this.view.printSpace();
                                                while (continuar != 1 && continuar != 2) {
                                                    this.view.deveSerDe1a2();
                                                    continuar = Integer.parseInt(sc.next());
                                                    this.view.printSpace();
                                                }
                                            }
                                            case 2 -> {
                                                this.view.insertNewName();
                                                nomeNovo = sc.next();
                                                this.view.printSpace();
                                                nomeAntigo = trazAqui.extractNameByUserNameUtilizadores(username);
                                                trazAqui.changeName(username, nomeNovo);
                                                nomeNovo = trazAqui.extractNameByUserNameUtilizadores(username);
                                                this.view.trocaComSucessoNome(nomeAntigo, nomeNovo);
                                                this.view.desejaContinuar();
                                                try {
                                                    continuar = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                this.view.printSpace();
                                                while (continuar != 1 && continuar != 2) {
                                                    this.view.deveSerDe1a2();
                                                    continuar = Integer.parseInt(sc.next());
                                                    this.view.printSpace();
                                                }
                                            }
                                            case 3 -> {
                                                this.view.insertNewX();
                                                try {
                                                    xNovo = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                this.view.printSpace();
                                                xAntigo = trazAqui.extractXByUserName(username);
                                                this.view.insertNewY();
                                                try {
                                                    yNovo = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                this.view.printSpace();
                                                yAntigo = trazAqui.extractYByUserName(username);
                                                trazAqui.changeGPS(username, xNovo, yNovo);
                                                xNovo = trazAqui.extractXByUserName(username);
                                                yNovo = trazAqui.extractYByUserName(username);
                                                this.view.trocaComSucessoGPS(xAntigo, xNovo, yAntigo, yNovo);
                                                this.view.desejaContinuar();
                                                try {
                                                    continuar = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                this.view.printSpace();
                                                while (continuar != 1 && continuar != 2) {
                                                    this.view.deveSerDe1a2();
                                                    continuar = Integer.parseInt(sc.next());
                                                    this.view.printSpace();
                                                }
                                            }
                                            case 4 -> {
                                                this.view.insertNewEmail();
                                                emailNovo = sc.next();
                                                this.view.printSpace();
                                                emailAntigo = trazAqui.extractEmailByUserName(username);
                                                trazAqui.changeEmail(username, emailNovo);
                                                emailNovo = trazAqui.extractEmailByUserName(username);
                                                this.view.trocaComSucessoEmail(emailAntigo, emailNovo);
                                                this.view.desejaContinuar();
                                                try {
                                                    continuar = Integer.parseInt(sc.next());
                                                }
                                                catch (NumberFormatException exception) {}
                                                this.view.printSpace();
                                                while (continuar != 1 && continuar != 2) {
                                                    this.view.deveSerDe1a2();
                                                    continuar = Integer.parseInt(sc.next());
                                                    this.view.printSpace();
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        case 'v' -> {
                            this.view.welcomeWithUserNameAndName(username, trazAqui.extractNameByUserNameVoluntarios(username));
                            this.view.welcomeWithUserNameAndName(username, trazAqui.extractNameByUserNameVoluntarios(username));
                            this.view.printSpace();
                            this.view.voluntariosTransportadorasQueries();
                            try {
                                option = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (option != 1) {
                                this.view.deveSerDe1a1();
                                option = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                            this.view.changeAceita();
                            try {
                                option = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (option != 1 && option != 2) {
                                this.view.deveSerDe1a2();
                                option = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                            if (option == 1) {
                                aceita = true;
                            } else {
                                aceita = false;
                            }
                            trazAqui.changeAceitaVoluntario(username, aceita);
                            this.view.changeAceitaComSucesso();
                            this.view.desejaContinuar();
                            try {
                                continuar = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (continuar != 1 && continuar != 2) {
                                this.view.deveSerDe1a2();
                                continuar = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                        }
                        case 't' -> {
                            this.view.welcomeWithUserNameAndName(username, trazAqui.extractNameByUserNameTransportadoras(username));
                            this.view.welcomeWithUserNameAndName(username, trazAqui.extractNameByUserNameTransportadoras(username));
                            this.view.printSpace();
                            this.view.voluntariosTransportadorasQueries();
                            try {
                                option = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (option != 1) {
                                this.view.deveSerDe1a1();
                                option = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                            this.view.changeAceita();
                            try {
                                option = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (option != 1 && option != 2) {
                                this.view.deveSerDe1a2();
                                option = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                            if (option == 1) {
                                aceita = true;
                            } else {
                                aceita = false;
                            }
                            trazAqui.changeAceitaTransportadora(username, aceita);
                            this.view.changeAceitaComSucesso();
                            this.view.desejaContinuar();
                            try {
                                continuar = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (continuar != 1 && continuar != 2) {
                                this.view.deveSerDe1a2();
                                continuar = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                        }
                        case 'l' -> {
                            this.view.welcomeWithUserNameAndName(username, trazAqui.extractNameByUserNameLojas(username));
                            this.view.printSpace();
                            this.view.lojasQueries();
                            try {
                                option = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (option != 1) {
                                this.view.deveSerDe1a1();
                                option = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                            this.view.changeAceitaFila();
                            try {
                                option = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (option != 1 && option != 2) {
                                this.view.deveSerDe1a2();
                                option = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                            if (option == 1) {
                                aceita = true;
                            } else {
                                aceita = false;
                            }
                            trazAqui.changeAceitaLojas(username, aceita);
                            this.view.changeAceitaComSucesso();
                            this.view.desejaContinuar();
                            try {
                                continuar = Integer.parseInt(sc.next());
                            }
                            catch (NumberFormatException exception) {}
                            this.view.printSpace();
                            while (continuar != 1 && continuar != 2) {
                                this.view.deveSerDe1a2();
                                continuar = Integer.parseInt(sc.next());
                                this.view.printSpace();
                            }
                        }
                    }
                }
                case 2 -> {
                    this.view.printSpace();
                    this.model.signIn(trazAqui);
                    this.view.desejaContinuar();
                    try {
                        continuar = Integer.parseInt(sc.next());
                    }
                    catch (NumberFormatException exception) {}
                    this.view.printSpace();
                    while (continuar != 1 && continuar != 2) {
                        this.view.deveSerDe1a2();
                        continuar = Integer.parseInt(sc.next());
                        this.view.printSpace();
                    }
                }
                case 3 -> {
                    try {
                        trazAqui = trazAqui.loadData("./estado.dat");
                    }
                    catch(IOException | ClassNotFoundException e) {
                    }
                    this.view.desejaContinuar();
                    try {
                        continuar = Integer.parseInt(sc.next());
                    }
                    catch (NumberFormatException exception) {}
                }
                case 4 -> {
                    try {
                        trazAqui.saveData("./estado.dat");
                    }
                    catch(IOException e) {}
                    this.view.desejaContinuar();
                    try {
                        continuar = Integer.parseInt(sc.next());
                    }
                    catch (NumberFormatException exception) {}
                }
                default -> {
                    this.view.printSpace();
                    this.view.wrongOption();
                }
            }
        }
    }
}
