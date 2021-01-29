package com.javamarlon;

import java.io.Serializable;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Scanner;
import java.util.stream.Collectors;

public class UserInterface implements Serializable {

    private static Menu Inicio;
    private static Menu Registo;
    private static Menu Utilizador;
    private static Menu Voluntario;
    private static Menu Transportadora;
    private static Menu Loja;
    private static Input input;
    private static TrazAqui trazAqui;
    private static Scanner scanner;

    public static void main(String[] args) {
        trazAqui = new TrazAqui();
        input = new Input();
        scanner = new Scanner(System.in);
        povoar();
        menu();
        paginaInicial();
    }

    private static void povoar() {

        Utilizador francisco = new Utilizador("Francisco Coutinho Martins Correia", new GPS(14, 6), "francisco@mail.com", "francisco");
        Utilizador pedro = new Utilizador("Pedro Filipe Carvalho Barbosa", new GPS(8, 6), "pedro@mail.com", "pedro");
        Utilizador hugo = new Utilizador("Hugo Coelho Cardoso", new GPS(-14.681885, 72.73595), "hugo@mail.com", "hugo");
        Utilizador guilherme = new Utilizador("Guilherme Cerqueira da Mota Miranda", new GPS(-18.299461, -35.02321), "guilherme@mail.com", "guilherme");
        trazAqui.addUtilizador(francisco);
        trazAqui.addUtilizador(pedro);
        trazAqui.addUtilizador(hugo);
        trazAqui.addUtilizador(guilherme);

        Loja fnac = new Loja("Fnac", new GPS(11, 10), "fnac@mail.com", "fnac");
        Loja lefties = new Loja("Lefties", new GPS(14, 8), "lefties@mail.com", "lefties");
        trazAqui.addLoja(fnac);
        trazAqui.addLoja(lefties);

        LinhaEncomenda tv = new LinhaEncomenda("TV0002", "TV", 1, 150.90);
        LinhaEncomenda huawei = new LinhaEncomenda("HUAWEIG4935", "Huawei P20", 4, 230.90);

        ArrayList<LinhaEncomenda> linhasEncomenda1 = new ArrayList<>();
        linhasEncomenda1.add(tv);
        linhasEncomenda1.add(huawei);

        Encomenda franciscoFnacV1 = new Encomenda("franciscoFnacV1", francisco, fnac, 5, linhasEncomenda1);
        Encomenda pedroFnacV1 = new Encomenda("pedroFnacV1", pedro, fnac, 5, linhasEncomenda1);
        fnac.addEncomenda(franciscoFnacV1);
        francisco.fazerPedido(franciscoFnacV1);
        fnac.addEncomenda(pedroFnacV1);
        pedro.fazerPedido(pedroFnacV1);

        Transportadora fedEx = new Transportadora("FedEx", new GPS(14, 10), 6, "123456789", 1, 2, "fedex@mail.com", "fedex");
        trazAqui.addTransportadora(fedEx);

        Voluntario v1 = new Voluntario("V1", new GPS(7, 9), 5, "voluntario1@mail.com", "v1");
        trazAqui.addVoluntario(v1);

        try (Scanner scanner = new Scanner(Paths.get("logs.txt"))) {

            while (scanner.hasNextLine()) {
                String line = scanner.nextLine();
                String[] linhaPartida = line.split(":");
                switch (linhaPartida[0]) {
                    case "Utilizador":
                        String[] campos = linhaPartida[1].split(",");
                        String cod = campos[0];
                        String nome = campos[1];
                        double gpsx = Double.parseDouble(campos[2]);
                        double gpsy = Double.parseDouble(campos[3]);
                        String email = cod + "@mail.com";
                        String password = cod;
                        trazAqui.addUtilizador(new Utilizador(nome, new GPS(gpsx, gpsy), email, password));
                        break;
                    case "Voluntario":
                        campos = linhaPartida[1].split(",");
                        String codVol = campos[0];
                        String nomeVol = campos[1];
                        gpsx = Double.parseDouble(campos[2]);
                        gpsy = Double.parseDouble(campos[3]);
                        double raio = Double.parseDouble(campos[4]);
                        String emailVol = codVol + "@mail.com";
                        password = codVol;
                        trazAqui.addVoluntario(new Voluntario(nomeVol, new GPS(gpsx, gpsy), raio, emailVol, password));
                        break;
                    case "Transportadora":
                        campos = linhaPartida[1].split(",");
                        String codEmp = campos[0];
                        String nomeEmp = campos[1];
                        gpsx = Double.parseDouble(campos[2]);
                        gpsy = Double.parseDouble(campos[3]);
                        String NIF = campos[4];
                        raio = Double.parseDouble(campos[5]);
                        double taxa = Double.parseDouble(campos[6]);
                        int capacidade = 1;
                        email = codEmp + "@mail.com";
                        password = codEmp;
                        trazAqui.addTransportadora(new Transportadora(nomeEmp, new GPS(gpsx, gpsy), raio, NIF, taxa, capacidade, email, password));
                        break;
                    case "Loja":
                        campos = linhaPartida[1].split(",");
                        String codLoja = campos[0];
                        String nomeLoja = campos[1];
                        gpsx = Double.parseDouble(campos[2]);
                        gpsy = Double.parseDouble(campos[3]);
                        email = codLoja + "@mail.com";
                        password = codLoja;
                        trazAqui.addLoja(new Loja(nomeLoja, new GPS(gpsx, gpsy), email, password));
                        break;
                    case "Encomenda":
                        campos = linhaPartida[1].split(",");
                        String codEncomenda = campos[0];
                        String codUtilizador = campos[1];
                        Utilizador utilizador = trazAqui.getUtilizador(codUtilizador + "@mail.com");
                        codLoja = campos[2];
                        Loja loja = trazAqui.getLoja(codLoja + "@mail.com");
                        double peso = Double.parseDouble(campos[3]);
                        ArrayList<LinhaEncomenda> linhaEncomendas = new ArrayList<>();
                        String codProduto, descricao;
                        double quantidade, valorUnitario;
                        for (int i = 4; i < campos.length; i += 4) {
                            codProduto = campos[i];
                            descricao = campos[i + 1];
                            quantidade = Double.parseDouble(campos[i + 2]);
                            valorUnitario = Double.parseDouble(campos[i + 3]);
                            linhaEncomendas.add(new LinhaEncomenda(codProduto, descricao, quantidade, valorUnitario));
                        }
                        Encomenda encomenda = new Encomenda(codEncomenda, utilizador, loja, peso, linhaEncomendas);
                        trazAqui.getLoja(codLoja + "@mail.com").addEncomenda(encomenda);
                        trazAqui.getUtilizador(codUtilizador + "@mail.com").fazerPedido(encomenda);
                        break;
                    case "Aceite":
                        codEncomenda = linhaPartida[1];
                        Encomenda encomendaAProcessar = null;
                        loja = null;
                        ArrayList<Loja> lojas = trazAqui.getLojas().keySet().stream().map(key -> trazAqui.getLojas().get(key)).collect(Collectors.toCollection(ArrayList::new));
                        for (Loja loja1 : lojas) {
                            for (Encomenda encomenda1 : loja1.getNovas()) {
                                if (encomenda1.getCodEncomenda().equals(codEncomenda)) {
                                    encomendaAProcessar = encomenda1;
                                    loja = loja1;
                                }
                            }
                        }
                        if (loja != null) {
                            loja.removeFromNovas(encomendaAProcessar);
                            loja.addToProcessadas(encomendaAProcessar);
                        }
                        break;
                }
            }
        } catch (Exception e) {
            System.out.println("Error: " + e.getMessage());
        }
    }

    private static int paginaInicial() {
        int opcao;
        int ret = 1;
        while (ret == 1) {
            Inicio.showMenu();
            System.out.print("Insira uma opção: ");
            opcao = input.lerInt();
            switch (opcao) {
                case 0:
                    ret = 0;
                    System.out.println("\nTenha um bom dia!");
                    break;
                case 1:
                    ret = login();
                    break;
                case 2:
                    ret = Registo();
                    break;
            }
        }
        return 0;
    }

    private static int login() {
        System.out.print("\nInsira o seu Email: ");
        String email = input.lerString();
        System.out.print("Insira a sua password: ");
        String password = input.lerString();
        if (trazAqui.checkLoginUtilizador(email, password)) {
            return Utilizador(email);
        } else if (trazAqui.checkLoginVoluntario(email, password)) {
            return Voluntario(email);
        } else if (trazAqui.checkLoginTransportadora(email, password)) {
            return Transportadora(email);
        } else if (trazAqui.checkLoginLoja(email, password)) {
            return Loja(email);
        } else {
            System.out.println("\nEmail ou password inválidos. Tente novamnete.");
            return paginaInicial();
        }
    }

    private static int Registo() {
        int ret = 1;
        while (ret == 1) {
            Registo.showMenu();
            System.out.print("Insira uma opção: ");
            int opcao = input.lerInt();
            switch (opcao) {
                case 0:
                    ret = paginaInicial();
                    break;
                case 1:
                    ret = RegistoUtilizador();
                    break;
                case 2:
                    ret = RegistoVoluntario();
                    break;
                case 3:
                    ret = RegistoTransportadora();
                    break;
                case 4:
                    ret = RegistoLoja();
                    break;
                default:
                    System.out.println("Opção inválida. Tente novamente.");
            }
        }
        return ret;
    }

    private static int RegistoUtilizador() {
        String email, nome, password;
        Double latitude, longitude;
        GPS gps;
        System.out.print("\nInsira o seu Email: ");
        email = input.lerString();
        if (isInvalidEmailAddress(email)) {
            System.out.println("\nO email não está num formato válido.");
            return Registo();
        }
        if (trazAqui.emailExiste(email)) {
            System.out.println("\nEste email já está em uso.");
            return Registo();
        }
        System.out.print("Crie uma password: ");
        password = input.lerString();
        System.out.print("Introduza o seu nome: ");
        nome = scanner.nextLine();
        System.out.println("Indique-nos a sua localização: ");
        System.out.print("Latitude: ");
        latitude = input.lerDouble();
        System.out.print("Longitude: ");
        longitude = input.lerDouble();
        gps = new GPS(latitude, longitude);
        Utilizador novoUtilizador = new Utilizador(nome, gps, email, password);
        trazAqui.addUtilizador(novoUtilizador);
        System.out.println("\nUtilizador registado com sucesso!");
        return Utilizador(email);
    }

    private static int RegistoVoluntario() {
        String email, nome, password, cod;
        Double latitude, longitude, raio;
        GPS gps;
        System.out.print("\nInsira o seu Email: ");
        email = input.lerString();
        if (isInvalidEmailAddress(email)) {
            System.out.println("\nO email não está num formato válido.");
            return Registo();
        }
        if (trazAqui.emailExiste(email)) {
            System.out.println("\nEste email já está em uso.");
            return Registo();
        }
        System.out.print("Crie uma password: ");
        password = input.lerString();
        System.out.print("Introduza o seu nome: ");
        nome = scanner.nextLine();
        System.out.println("Indique-nos a sua localização: ");
        System.out.print("Latitude: ");
        latitude = input.lerDouble();
        System.out.print("Longitude: ");
        longitude = input.lerDouble();
        gps = new GPS(latitude, longitude);
        System.out.print("Indique-nos o raio máximo: ");
        raio = input.lerDouble();
        Voluntario novoVoluntario = new Voluntario(nome, gps, raio, email, password);
        trazAqui.addVoluntario(novoVoluntario);
        System.out.println("\nVoluntário registado com sucesso!");
        return Voluntario(email);
    }

    private static int RegistoTransportadora() {
        String email, nome, password, NIF;
        Double latitude, longitude, raio, taxa;
        GPS gps;
        int capacidade;
        System.out.print("\nInsira o seu Email: ");
        email = input.lerString();
        if (isInvalidEmailAddress(email)) {
            System.out.println("\nO email não está num formato válido.");
            return Registo();
        }
        if (trazAqui.emailExiste(email)) {
            System.out.println("\nEste email já está em uso.");
            return Registo();
        }
        System.out.print("Crie uma password: ");
        password = input.lerString();
        System.out.print("Introduza o nome da transportadora: ");
        nome = scanner.nextLine();
        System.out.print("Insira o Número de Identificação Fiscal: ");
        NIF = input.lerString();
        System.out.println("Indique-nos a sua localização: ");
        System.out.print("Latitude: ");
        latitude = input.lerDouble();
        System.out.print("Longitude: ");
        longitude = input.lerDouble();
        gps = new GPS(latitude, longitude);
        System.out.print("Indique-nos o raio máximo: ");
        raio = input.lerDouble();
        System.out.print("Indique-nos a capacidade de entregas: ");
        capacidade = input.lerInt();
        System.out.print("Indique-nos a taxa a aplicar(€/km): ");
        taxa = input.lerDouble();
        Transportadora novaTransportadora = new Transportadora(nome, gps, raio, NIF, taxa, capacidade, email, password);
        trazAqui.addTransportadora(novaTransportadora);
        System.out.println("\nTransportadora registada com sucesso!");
        return Transportadora(email);
    }

    private static int RegistoLoja() {
        String email, nome, password;
        Double latitude, longitude;
        GPS gps;
        System.out.print("\nInsira o seu Email: ");
        email = input.lerString();
        if (isInvalidEmailAddress(email)) {
            System.out.println("\nO email não está num formato válido.");
            return Registo();
        }
        if (trazAqui.emailExiste(email)) {
            System.out.println("\nEste email já está em uso.");
            return Registo();
        }
        System.out.print("Crie uma password: ");
        password = input.lerString();
        System.out.print("Introduza o nome da loja: ");
        nome = scanner.nextLine();
        System.out.println("Indique-nos a sua localização: ");
        System.out.print("Latitude: ");
        latitude = input.lerDouble();
        System.out.print("Longitude: ");
        longitude = input.lerDouble();
        gps = new GPS(latitude, longitude);
        Loja novaLoja = new Loja(nome, gps, email, password);
        trazAqui.addLoja(novaLoja);
        System.out.println("\nLoja registada com sucesso!");
        return Loja(email);
    }

    private static int Utilizador(String email) {
        int opcao;
        int ret = 1;
        Utilizador utilizador = trazAqui.getUtilizador(email);
        Utilizador.showMenu();
        System.out.print("\nEscolha uma opção: ");
        opcao = input.lerInt();
        while (ret == 1) {
            switch (opcao) {
                case 0:
                    ret = paginaInicial();
                    break;
                case 1: // 1 - Consulte as suas informações
                    System.out.println(utilizador);
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    int numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Utilizador(email);
                    break;
                case 2: // 2 - Consultar os seus pedidos
                    utilizador.printPedidos();
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Utilizador(email);
                    break;
                case 3: // 3 - Serviços de entrega propostos
                    HashMap<String, Encomenda> pendentes = utilizador.getPendentes();
                    if (pendentes.isEmpty()) {
                        System.out.println("Não existem pedidos");
                        System.out.println("\n0 - Voltar");
                        System.out.print("\nEscolha uma opção: ");
                        numero = input.lerInt();
                        while (numero != 0) {
                            System.out.println("\nOpção inválida");
                            System.out.print("Escolha uma opção: ");
                            numero = input.lerInt();
                        }
                        ret = Utilizador(email);
                    } else {
                        utilizador.getPropostas().keySet().stream().map(key -> utilizador.getPropostas().get(key)).forEach(System.out::println);
                        System.out.print("\nInsira o código da encomenda a aceitar ou rejeitar: ");
                        String cod = input.lerString();
                        System.out.println("1 - Aceitar\n2 - Rejeitar");
                        System.out.println("\n0 - Voltar");
                        System.out.print("\nEscolha uma opção: ");
                        numero = input.lerInt();
                        while (numero != 0 && numero != 1 && numero != 2) {
                            System.out.println("\nOpção inválida");
                            System.out.print("Escolha uma opção: ");
                            numero = input.lerInt();
                        }
                        Transportadora transportadora = null;
                        ArrayList<Transportadora> transportadoras = trazAqui.getTransportadoras().keySet().stream().map(key -> trazAqui.getTransportadoras().get(key)).collect(Collectors.toCollection(ArrayList::new));
                        for (Transportadora elem : transportadoras) {
                            if (elem.getPendentesAceitacao().containsKey(cod)) {
                                transportadora = elem;
                                break;
                            }
                        }
                        if (numero == 1) {
                            if (transportadora != null) {
                                transportadora.utilizadorAceita(cod);
                                utilizador.aceitarPendente(cod);
                                transportadora.propostaAceite(utilizador.getPropostas().get(cod));
                                utilizador.aceitarProposta(utilizador.getPropostas().get(cod));
                            }
                        } else if (numero == 2) {
                            if (transportadora != null) {
                                transportadora.utilizadorRejeita(cod);
                                utilizador.rejeitarPendente(cod);
                                utilizador.rejeitarProposta(cod);
                            }
                        }

                        ret = Utilizador(email);
                    }
                    break;
                case 4: // 4 - Consultar o histórico de pedidos
                    utilizador.printEntregues();
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Utilizador(email);
                    break;

                case 5: // 5 - Solicitar entrega
                    return SolicitarEntrega(utilizador);
                case 6:

                    ArrayList<Encomenda> entregues = utilizador.getEntregues();
                    if (!entregues.isEmpty()) {
                        System.out.println("\nAs suas encomendas entregues:");
                        entregues.forEach(System.out::println);
                        System.out.print("\nInsira o código da encomenda a avaliar: ");
                        String cod = input.lerString();
                        Encomenda encomendaAAvaliar = null;
                        for (Encomenda encomenda : entregues) {
                            if (encomenda.getCodEncomenda().equals(cod)) {
                                encomendaAAvaliar = encomenda;
                            }
                        }
                        if (encomendaAAvaliar == null) {
                            System.out.println("\nO código inserido não existe");
                        } else {
                            System.out.println("\nDe 0 a 5, qual é o sau grau de satisfação com o serviço prestado?");
                            System.out.print("Insira a sua classificação: ");
                            int classificacao = input.lerInt();
                            if (classificacao > 0 && classificacao < 6) {
                                encomendaAAvaliar.setClassificacao(classificacao);
                            } else {
                                System.out.println("O valor inserido não é válido");
                            }
                        }
                    } else {
                        System.out.println("\nAinda não existem encomendas");
                    }
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Utilizador(email);
                    break;
            }
        }
        return ret;
    }

    private static int Voluntario(String email) {
        int opcao;
        int ret = 1;
        Voluntario voluntario = trazAqui.getVoluntario(email);
        Voluntario.showMenu();
        System.out.print("\nEscolha uma opção: ");
        opcao = input.lerInt();
        while (ret == 1) {
            switch (opcao) {
                case 0: // 0 - Voltar
                    ret = paginaInicial();
                    break;
                case 1: // 1 - Consulte as suas informações
                    System.out.println(voluntario);
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    int numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Voluntario(email);
                    break;
                case 2: // 2 - Iniciar uma entrega
                    if (!voluntario.disponivel()) {
                        System.out.println("\nÉ necessário conluir a entrega atual antes de iniciar uma nova");
                        System.out.println("\n0 - Voltar");
                        System.out.print("\nEscolha uma opção: ");
                        numero = input.lerInt();
                        while (numero != 0) {
                            System.out.println("\nOpção inválida");
                            System.out.print("Escolha uma opção: ");
                            numero = input.lerInt();
                        }
                    } else {
                        System.out.println("\nEncomendas ao alcance:");
                        ArrayList<Encomenda> encomendas = trazAqui.encomendasNoRaio(voluntario);
                        if (encomendas.isEmpty()) {
                            System.out.println("\nNão existem encomendas disponiveis dentro do raio definido");
                            System.out.println("\n0 - Voltar");
                            System.out.print("\nEscolha uma opção: ");
                            numero = input.lerInt();
                            while (numero != 0) {
                                System.out.println("\nOpção inválida");
                                System.out.print("Escolha uma opção: ");
                                numero = input.lerInt();
                            }
                        } else {
                            encomendas.forEach(System.out::println);
                            System.out.print("\nPara iniciar uma entrega insira o código da encomenda: ");
                            String cod = input.lerString();
                            Encomenda encomenda = null;
                            for (Encomenda elem : encomendas) {
                                if (elem.getCodEncomenda().equals(cod)) {
                                    encomenda = elem;
                                }
                            }
                            if (encomenda == null) {
                                System.out.println("\nNão existe nenhuma encomenda com o código inserido");
                                System.out.println("\n0 - Voltar");
                                System.out.print("\nEscolha uma opção: ");
                                numero = input.lerInt();
                                while (numero != 0) {
                                    System.out.println("\nOpção inválida");
                                    System.out.print("Escolha uma opção: ");
                                    numero = input.lerInt();
                                }
                            } else {
                                Loja loja = encomenda.getLoja();
                                loja.entregar(encomenda.getCodEncomenda());
                                voluntario.receber(encomenda);
                                System.out.println("\nEncomenda recolhida com sucesso");
                                System.out.println("\n0 - Voltar");
                                System.out.print("\nEscolha uma opção: ");
                                numero = input.lerInt();
                                while (numero != 0) {
                                    System.out.println("\nOpção inválida");
                                    System.out.print("Escolha uma opção: ");
                                    numero = input.lerInt();
                                }
                            }
                        }
                    }
                    ret = Voluntario(email);
                    break;
                case 3: // 3 - Concluir a entrega
                    if (voluntario.disponivel()) {
                        System.out.println("\nNão existem entregas para concluir");
                    } else {
                        ArrayList<String> keys = new ArrayList<>(voluntario.expedidas.keySet());
                        Encomenda encomenda = voluntario.expedidas.get(keys.get(0));
                        voluntario.concluirEntrega(keys.get(0));
                        Utilizador utilizador = encomenda.getUtilizador();
                        utilizador.encomendaEntregue(encomenda);
                        System.out.println("\nEncomenda concluida com sucesso");
                    }
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Voluntario(email);
                    break;
            }
        }
        return ret;
    }

    private static int Transportadora(String email) {
        int opcao;
        int ret = 1;
        Transportadora transportadora = trazAqui.getTransportadora(email);
        Transportadora.showMenu();
        System.out.print("\nEscolha uma opção: ");
        opcao = input.lerInt();
        while (ret == 1) {
            switch (opcao) {
                case 0:
                    ret = paginaInicial();
                    break;
                case 1: // 1 - Consulte as suas informações
                    System.out.println(transportadora);
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    int numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Transportadora(email);
                    break;
                case 2: // 2 - Iniciar uma entrega
                    if (!transportadora.disponivel()) {
                        System.out.println("Capacidade máxima atingida. Conclua uma entrega antes de iniciar uma nova");
                        System.out.println("\n0 - Voltar");
                        System.out.print("\nEscolha uma opção: ");
                        numero = input.lerInt();
                        while (numero != 0) {
                            System.out.println("\nOpção inválida");
                            System.out.print("Escolha uma opção: ");
                            numero = input.lerInt();
                        }
                    } else {
                        ArrayList<Encomenda> encomendas = trazAqui.encomendasNoRaio(transportadora);
                        // Se existir uma encomenda dentro do raio que esteja pendente de aceitação, não dar print
                        encomendas.removeIf(elem -> transportadora.getPendentesAceitacao().containsKey(elem.getCodEncomenda()));
                        encomendas.removeIf(elem -> transportadora.getExpedidas().containsKey(elem.getCodEncomenda()));
                        System.out.println("\nEncomendas ao alcance:");
                        if (encomendas.isEmpty()) {
                            System.out.println("\nNão existem encomendas disponiveis dentro do raio definido");
                            System.out.println("\n0 - Voltar");
                            System.out.print("\nEscolha uma opção: ");
                            numero = input.lerInt();
                            while (numero != 0) {
                                System.out.println("\nOpção inválida");
                                System.out.print("Escolha uma opção: ");
                                numero = input.lerInt();
                            }
                        } else {
                            encomendas.forEach(System.out::println);
                            System.out.print("\nPara iniciar uma entrega insira o código da encomenda: ");
                            String cod = input.lerString();
                            Encomenda encomenda = null;
                            for (Encomenda elem : encomendas) {
                                if (elem.getCodEncomenda().equals(cod)) {
                                    encomenda = elem;
                                }
                            }
                            if (encomenda == null) {
                                System.out.println("\nNão existe nenhuma encomenda com o código inserido");
                                System.out.println("\n0 - Voltar");
                                System.out.print("\nEscolha uma opção: ");
                                numero = input.lerInt();
                                while (numero != 0) {
                                    System.out.println("\nOpção inválida");
                                    System.out.print("Escolha uma opção: ");
                                    numero = input.lerInt();
                                }
                            } else {
                                Utilizador utilizador = encomenda.getUtilizador();
                                transportadora.aceitarEncomenda(encomenda);
                                utilizador.setPendente(encomenda);
                                Loja loja = encomenda.getLoja();
                                double distEmpLoj = transportadora.distancia(loja);
                                double distLojClt = loja.distancia(utilizador);
                                double distTotal = distEmpLoj + distLojClt;
                                double custo = transportadora.getTaxa() * distTotal;
                                utilizador.addProposta(new PropostaTransportadora(encomenda, custo, transportadora));
                                System.out.println("\nA aguardar que cliente confirme a recolha");
                                System.out.println("\n0 - Voltar");
                                System.out.print("\nEscolha uma opção: ");
                                numero = input.lerInt();
                                while (numero != 0) {
                                    System.out.println("\nOpção inválida");
                                    System.out.print("Escolha uma opção: ");
                                    numero = input.lerInt();
                                }
                            }
                        }
                    }
                    ret = Transportadora(email);
                    break;
                case 3: // 3 - Consultar pendentes
                    System.out.println("\nA aguardar aceitação por parte dos clientes:");
                    transportadora.getPendentesAceitacao().keySet().stream()
                            .map(key -> transportadora.getPendentesAceitacao().get(key))
                            .forEach(System.out::println);
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Transportadora(email);
                    break;
                case 4: // 3 - Concluir uma entrega
                    if (transportadora.getExpedidas().isEmpty()) {
                        System.out.println("\nNão existem entregas para concluir");
                    } else {
                        System.out.println("\nEstas são as encomendas expedidas:");
                        transportadora.getExpedidas().forEach((s, encomenda) -> System.out.println(encomenda));
                        System.out.print("\nInsira o código da encomenda a concluir: ");
                        String codigo = input.lerString();
                        Encomenda encomenda = transportadora.getExpedidas().get(codigo);
                        Loja loja = encomenda.getLoja();
                        Utilizador utilizador = encomenda.getUtilizador();
                        if (transportadora.getExpedidas().containsKey(codigo)) {
                            transportadora.concluirEntrega(codigo);
                            loja.entregar(codigo);
                            utilizador.encomendaEntregue(encomenda);
                            System.out.println("\nEncomenda concluida com sucesso");
                        } else {
                            System.out.println("\nNão existe nenhuma encomenda com código indicado");
                        }
                    }
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Transportadora(email);
                    break;
                case 5: // 4 - Consultar o histórico de entrgas
                    break;
            }
        }
        return ret;
    }

    private static int Loja(String email) {
        int opcao;
        int ret = 1;
        Loja loja = trazAqui.getLoja(email);
        Loja.showMenu();
        System.out.print("\nEscolha uma opção: ");
        opcao = input.lerInt();
        while (ret == 1) {
            switch (opcao) {
                case 0: // 0 - Terminar sessão
                    ret = paginaInicial();
                    break;
                case 1: // 1 - Consulte as suas informações
                    System.out.println(loja);
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    int numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Loja(email);
                    break;
                case 2: // 2 - Consultar os pedidos não processados
                    loja.printNovas();
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Loja(email);
                    break;
                case 3: // 3 - Consultar a proximo pedido na fila de espera
                    System.out.println("\nEsta é a próxima encomenda a ser processada:\n" + loja.proximaEncomenda());
                    System.out.println("\nProcessar?\n1 - Sim");
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    if (numero == 1) {
                        loja.processar();
                    }
                    while (numero != 0 && numero != 1) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Loja(email);
                    break;
                case 4: // 4 - Consultar as encomendas prontas a serem entregues
                    loja.printProcessadas();
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Loja(email);
                    break;
                case 5: // 5 - Consultar historico de entregas
                    loja.printEntregues();
                    System.out.println("\n0 - Voltar");
                    System.out.print("\nEscolha uma opção: ");
                    numero = input.lerInt();
                    while (numero != 0) {
                        System.out.println("\nOpção inválida");
                        System.out.print("Escolha uma opção: ");
                        numero = input.lerInt();
                    }
                    ret = Loja(email);
                    break;
            }
        }
        return ret;
    }

    private static int SolicitarEntrega(Utilizador utilizador) {
        String codProduto, descricao, codEncomenda, nomeLoja;
        double quantidade, valorUnitario, peso;
        int continuar;
        LinhaEncomenda linhaEncomenda;
        ArrayList<LinhaEncomenda> linhaEncomendas = new ArrayList<>();
        Encomenda encomenda;
        System.out.println("\nLojas disponíveis:");
        trazAqui.getLojas().forEach((key, loja) -> System.out.println("- " + loja.getNome()));
        System.out.print("\nInsira o nome da loja pretendida: ");
        nomeLoja = scanner.nextLine();
        if (!trazAqui.lojaExiste(nomeLoja)) {
            System.out.println("\nA loja indicada não existe.");
            System.out.println("\n0 - Voltar");
            System.out.print("\nEscolha uma opção: ");
            int numero = input.lerInt();
            while (numero != 0) {
                System.out.println("\nOpção inválida");
                System.out.print("Escolha uma opção: ");
                numero = input.lerInt();
            }
            return Utilizador(utilizador.getEmail());
        }
        do {
            //TODO: Questionar se é encomenda médica. A encomenda poderá ter uma flag que a identifique dessa forma.
            // Encomendas Médicas que só poderão ser transportadas por voluntários ou empresas certiﬁcados para o efeito.
            System.out.print("Insira o nome do produto: ");
            descricao = scanner.nextLine();
            //TODO: Se colocar em branco e for o primeiro produto da encomenda, solicitar que insira pelo menos um.
            System.out.print("Insira o código do produto: ");
            codProduto = input.lerString();
            System.out.print("Insira a quantidade que pretende comprar: ");
            quantidade = input.lerDouble();
            System.out.print("Insira custo por produto: ");
            valorUnitario = input.lerDouble();
            System.out.println("\nDeseja inserir outro produto na sua encomenda?\n1 - Sim\n\n0 - Voltar");
            System.out.print("\nEscolha uma opção: ");
            continuar = input.lerInt();
            linhaEncomenda = new LinhaEncomenda(codProduto, descricao, quantidade, valorUnitario);
            linhaEncomendas.add(linhaEncomenda);
        } while (continuar == 1);

        codEncomenda = Long.toHexString(Double.doubleToLongBits(Math.random())); //Gera codigo de encomenda aleatorio
        //TODO: Decidir como definir o peso da encomenda.
        peso = linhaEncomendas.size();
        Loja loja = trazAqui.getLojaComNome(nomeLoja);
        encomenda = new Encomenda(codEncomenda, utilizador, loja, peso, linhaEncomendas);
        utilizador.fazerPedido(encomenda);
        loja.addEncomenda(encomenda);
        return Utilizador(utilizador.getEmail());
    }

    private static void menu() {
        String[] inicial = {
                "\n------------------------------------------------",
                "---------- PÁGINA INICIAL - TrazAqui! ----------",
                "------------------------------------------------",
                "1 - Login",
                "2 - Registo\n",
                "0 - Sair\n"
        };

        String[] registo = {
                "\n------------------------------------------------",
                "----------------- REGISTO ----------------------",
                "------------------------------------------------",
                "1 - Registar-me como Cliente",
                "2 - Registar-me como Voluntário",
                "3 - Registar-me como Empresa de Entrega",
                "4 - Registar-me como Loja\n",
                "0 - Voltar\n"
        };

        String[] utilizador = {
                "\n------------------------------------------------",
                "--------------- CLIENTE ------------------------",
                "------------------------------------------------",
                "1 - Consulte as suas informações",
                "2 - Consultar os seus pedidos",
                "3 - Serviços de entrega propostos",
                "4 - Consultar o histórico de pedidos",
                "5 - Solicitar entrega",
                "6 - Classificar entrega\n",
                "0 - Terminar sessão",
        };

        String[] voluntario = {
                "\n------------------------------------------------",
                "--------------- VOLUNTÁRIO ---------------------",
                "------------------------------------------------",
                "1 - Consulte as suas informações",
                "2 - Iniciar uma entrega",
                "3 - Concluir a entrega",
                "4 - Consultar o histórico de entregas\n", //TODO: Implementar a funcionalidade de consulta de historico
                "0 - Terminar sessão"
        };

        String[] transportadora = {
                "\n------------------------------------------------",
                "------------- TRANSPORTADORA -------------------",
                "------------------------------------------------",
                "1 - Consulte as suas informações",
                "2 - Iniciar uma entrega",
                "3 - Consultar pendentes",
                "4 - Concluir uma entrega",
                "5 - Consultar o histórico de entregas\n",
                "0 - Terminar sessão"
        };

        String[] loja = {
                "\n------------------------------------------------",
                "------------------ LOJA ------------------------",
                "------------------------------------------------",
                "1 - Consulte as suas informações",
                "2 - Consultar os pedidos não processados",
                "3 - Consultar a proximo pedido na fila de espera",
                "4 - Consultar as encomendas prontas a serem entregues",
                "5 - Consultar historico de entregas\n",
                "0 - Terminar sessão"
        };

        String[] encomendas1 = {
                "\n------------------------------------------------",
                "----------------- ENCOMENDAS -------------------",
                "------------------------------------------------",
                "\n"
        };

        String[] processarEncomendasLoja = {
                "1 - Processar a próxima encomenda na fila de espera\n",
                "0 - Voltar\n"
        };

        Inicio = new Menu(inicial);
        Registo = new Menu(registo);
        Utilizador = new Menu(utilizador);
        Voluntario = new Menu(voluntario);
        Transportadora = new Menu(transportadora);
        Loja = new Menu(loja);
    }

    public static boolean isInvalidEmailAddress(String email) {
        String ePattern = "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$";
        java.util.regex.Pattern p = java.util.regex.Pattern.compile(ePattern);
        java.util.regex.Matcher m = p.matcher(email);
        return !m.matches();
    }

}