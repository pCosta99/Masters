package View;

import Model.*;
import Model.Atores.Transportadores.EmpresaTransportadora;
import Model.Atores.Loja;
import Model.Atores.Transportadores.Transporte;
import Model.Atores.Transportadores.Voluntario;
import Model.Atores.User;
import Model.Exeptions.ProdutosException;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.text.DecimalFormat;
import java.time.LocalDateTime;
import java.awt.geom.Point2D;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import java.util.stream.Collectors;
import static  java.lang.Double.parseDouble;
import  static java.lang.Float.parseFloat;
import  static java.lang.System.exit;



public class View implements Serializable {



        private  static TrazAqui dados = new TrazAqui();
        private static Menu principal, cliente, transportador, loja, escolhaC, showPreco, voluntario;

        private void carregaMenus () {
            String[] menuPrincipal = {"Registar",
                    "Login",
                    "Top 10 utilizadores que mais usam o sistema",
                    "Top 10 Empresas Transportadoras com mais kms"};

            String[] menuCliente = {"Dados pessoais",
                    "Requesitar entrega",
                    "Pedidos aceites",
                    "Minhas encomendas",
            };

            String[] menuTransportador = {"Dados pessoais",
                    "Minhas Encomendas",
                    "Faturação",
                    "Pedidos",
                    "Estado de trabalho"};

            String[] menuVoluntario = {"Dados pessoais",
                    "Minhas Encomendas",
                    "Estado de trabalho"
            };

            String[] menuLoja = {"Dados da sua loja",
                    "Listagem de encomendas",
                    "Meus Produtos",
                    "Adicionar Produto",
                    "Pedidos"};


            String[] mshowPreco = {"Total faturado ",
                    "Total faturado num periodo"};

            String[] mescolhaC = {"Sou cliente", "Sou transportador de encomendas", "Sou voluntario", "Sou lojista"};

            principal = new Menu(menuPrincipal);
            cliente = new Menu(menuCliente);
            transportador = new Menu(menuTransportador);
            loja = new Menu(menuLoja);
            escolhaC = new Menu(mescolhaC);
            showPreco = new Menu(mshowPreco);
            voluntario = new Menu(menuVoluntario);
        }

        public void start() {

            System.out.println("***********************************************************************************************" +
                    "\n\nÉ necessária fazer uma leitura de um dos ficheiros de logs." +
                    "\n\nNOTA: Caso tenha lido um dos ficheiros, a leitura de outro levará à perda dos dados originais." +
                    "\n\nCaso ja tenha lido um dos ficheiros  por favor prossiga\n1-Ler ficheiro default de logs?\n2-Ler ficheiro de logs personalizado" +
                    "\n3-Prosseguir" +
                    "\n\n***********************************************************************************************\n");

            int a = Input.lerInt();
            if (a == 1) {
                carregaDados();

                try {
                    dados.gravar();
                } catch (IOException e) {
                    System.out.println("Falha gravar estado");
                }

                carregaMenus();
                lerDadosGravados();
                do {

                    principal.executa();
                    try {

                        dados.gravar();
                    } catch (IOException e) {
                        System.out.println("Falha gravar estado");
                    }

                    switch (principal.getOp()) {
                        case 0:
                            exit(0);
                        case 1:
                            registar();
                            break;
                        case 2:
                            iniciaSessao();
                            break;
                        case 3:
                            showTop();
                            break;
                        case 4:
                            showTopKm();
                            break;
                        default:
                            System.out.println("Opção inválida.");
                    }
                } while (principal.getOp() != 0);
            }
            if (a == 2) {
                carregaDadosPersonalizados();

                try {
                    dados.gravar();
                } catch (IOException e) {
                    System.out.println("Falha gravar estado");
                }

                carregaMenus();
                lerDadosGravados();
                do {

                    principal.executa();
                    try {

                        dados.gravar();
                    } catch (IOException e) {
                        System.out.println("Falha gravar estado");
                    }

                    switch (principal.getOp()) {
                        case 0:
                            exit(0);
                        case 1:
                            registar();
                            break;
                        case 2:
                            iniciaSessao();
                            break;
                        case 3:
                            showTop();
                            break;
                        case 4:
                            showTopKm();
                            break;
                        default:
                            System.out.println("Opção inválida.");
                    }
                } while (principal.getOp() != 0);
            } else {
                carregaMenus();
                lerDadosGravados();
                do {

                    principal.executa();
                    try {

                        dados.gravar();
                    } catch (IOException e) {
                        System.out.println("Falha gravar estado");
                    }

                    switch (principal.getOp()) {
                        case 0:
                            exit(0);
                        case 1:
                            registar();
                            break;
                        case 2:
                            iniciaSessao();
                            break;
                        case 3:
                            showTop();
                            break;
                        case 4:
                            showTopKm();
                            break;
                        default:
                            System.out.println("Opção inválida.");
                    }
                } while (principal.getOp() != 0);

            }

        }

        public void perfilCliente () throws ProdutosException {

            do {
                cliente.executa();


                switch (cliente.getOp()) {
                    case 0:
                        break;
                    case 1:
                        showdadosC();
                        break;
                    case 2:
                        encomendar();
                        break;
                    case 3:
                        pedidos();
                        break;
                    case 4:
                        showencguer(2);
                        break;

                    default:
                        System.out.println("Opção inválida.");
                }
            } while (cliente.getOp() != 0);

        }
        public void perfilEmpresa () {


            do {
                transportador.executa();


                switch (transportador.getOp()) {
                    case 0:
                        break;
                    case 1:
                        showdadosE();
                        break;
                    case 2:
                        showencguer(1);
                        break;
                    case 3:
                        showPreco();
                        break;
                    case 4:
                        pedidosEmpresa();
                        break;
                    case 5:
                        estadoE();
                        break;
                    default:
                        System.out.println("Opção inválida.");
                }
            } while (transportador.getOp() != 0);

        }

        public void perfilVoluntario () {


            do {
                voluntario.executa();


                switch (voluntario.getOp()) {
                    case 0:
                        break;
                    case 1:
                        showdadosV();
                        break;
                    case 2:
                        showencguer(4);
                        break;
                    case 3:
                        estadoV();
                        break;

                    default:
                        System.out.println("Opção inválida.");
                }
            } while (voluntario.getOp() != 0);

        }


        public void perfilLoja () {
            do {
                loja.executa();


                switch (loja.getOp()) {
                    case 0:
                        break;
                    case 1:
                        showdadosL();
                        break;
                    case 2:
                        showencguer(3);
                        break;
                    case 3:
                        produtosDisponiveis();
                        break;
                    case 4:
                        insereProdutos();
                        break;
                    case 5:
                        status();
                        break;

                    default:
                        System.out.println("Opção inválida.");
                }
            } while (transportador.getOp() != 0);

        }

        public void status () {
            List<String> list = dados.getPedidosLoja().stream().map(Encomenda::toStringNav).collect(Collectors.toList());
            Navegador a = new Navegador(list, 10, 1);
            a.run();
        }

        public void encomendar () throws ProdutosException {


            Encomenda encomenda = new Encomenda();

            String x;
            List<String> a = dados.getLojas().values().stream().map(Loja::navString).collect(Collectors.toList());
            Navegador n1 = new Navegador(a, 1, 5);
            n1.run();
            System.out.println("1. Indique a referencia da loja");
            x = Input.lerString();
            Loja loja = dados.getLojas().get(x);

            int produtos;
            List<String> b = dados.getLojas().get(x).getProdutos().values().stream().map(Produto::navString).collect(Collectors.toList());

            Navegador n2 = new Navegador(b, 1, 10);
            n2.run();


            int i = 0;
            List<Produto> prod = new ArrayList<>();

            System.out.println("1. Indique o numero de produtos que pretende comprar");
            produtos = Input.lerInt();
            while (i < produtos) {
                int j = i + 1;

                Produto p;
                int quantidade;
                System.out.println("_______________________________");
                System.out.println("Produto " + j);
                System.out.println("Indique a referencia do produto");
                String nome = Input.lerString();
                p = dados.getLojas().get(x).getProdutos().get(nome);
                if (p == null) throw new ProdutosException("Produto Inexistente");
                System.out.println("Quantidade:");
                quantidade = Input.lerInt();
                p.setQuantidade(quantidade);
                prod.add(p);
                i++;
            }
            encomenda.setProdutos(prod);
            encomenda.setCusto();
            DecimalFormat df = new DecimalFormat("####0.00");
            System.out.println("Valor total dos seus produtos: " + df.format(encomenda.getCustoProdutos()) + "$");
            encomenda.setLoja(dados.getLojas().get(x));
            encomenda.setComprador(dados.getClienteIn());
            encomenda.setPesoEncomenda();
            encomenda.setData(LocalDateTime.now());
            System.out.println("Peso total " + df.format(encomenda.getPeso()) + "kg");
            System.out.println("Produtos adicionados com sucesso");
            encomenda = dados.geraReferenciaEncomenda(encomenda);
            System.out.println("Referencia da encomenda: " + encomenda.getReferencia());
            dados.setEnc(encomenda);
            dados.addRegistoC();
            dados.addRegistoL();
            Transporte transportador;
            transportador = dados.sortEncomendaTransporte(encomenda);
            System.out.println("Foi enviado um pedido para o transportador:" + transportador.getNome() + " " + transportador.getReferencia());
            dados.adicionaEncomenda(encomenda);
            dados.addRegistoPedidoTransportador(transportador);


            if (transportador instanceof Voluntario) {

                dados.setEnc(encomenda);

                String refv = transportador.getReferencia();
                Voluntario e = dados.getVoluntariosTransporte().get(refv);
                encomenda.setAceiteCliente(true);
                encomenda.setDistribuidor(e);
                dados.setEnc(encomenda);
                e.setDisponibilidade(false);
                e.setMorada(encomenda.getDistribuidor().getMorada());

                dados.addEncomendaVoluntario();
                System.out.println("A sua encomenda encontra-se a caminho....\n\n\n\n\n");

                System.out.println("Viagem concluida. O seu pedido foi realizado com sucesso" + "\nDuração da viagem: " + df.format(encomenda.getTempo()) + " mins"
                        + "\nClassificação a atribuir ao seu voluntario (0-5): ");
                int y = Input.lerInt();
                while (y > 5) {
                    System.out.println("Classificação a atribuir ao seu voluntario (0-5)");
                    y = Input.lerInt();
                }

                e.setClassificacao(y);
                System.out.println(e.getClassificacao());
                e.setDisponibilidade(true);
                dados.adicionaEncomenda(encomenda);
                dados.addRegistoL();
                dados.addRegistoC();
                dados.addRegistoT();
                dados.adicionaTransportador(e);
                dados.adicionaEncomenda(encomenda);
            }


        }

        public void pedidosEmpresa () {
            String referencia = dados.getEmpresaIn().getReferencia();
            List<String> a = dados.getEmpresaIn().getEncomendasPedidas().values().stream().map(Encomenda::toStringNav).collect(Collectors.toList());
            Navegador n1 = new Navegador(a, 1, 5);
            n1.run();
            int op;
            String ref;
            if (dados.getEmpresaIn().getEncomendasPedidas().size() > 0) {
                System.out.println("Pretende aceitar algum pedido?\n(1)->Aceitar (2)->Recusar (3)->Cancelar");
                op = Input.lerInt();
                if (op == 1) {
                    System.out.println("Indique a referencia da encomenda a aceitar");
                    ref = Input.lerString();
                    Encomenda e = dados.getEmpresaIn().getEncomendas().get(ref);
                    System.out.println(e.toStringNav());
                    e.setDistribuidor(dados.getEmpresaIn());
                    e.setCustoTransporte(dados.getEmpresaIn().defineCusto(e));
                    e.setAceiteTransportador(true);
                    dados.setEnc(e);
                    dados.addRegistoC();
                    dados.adicionaEncomenda(e);
                    System.out.println("Encomenda aceite, aguarde confirmação do cliente");
                }
                if (op == 2) {
                    System.out.println("Indique a referencia da encomenda a recusar");
                    ref = Input.lerString();
                    Encomenda enc = dados.getEmpresaIn().getEncomendas().get(ref);
                    EmpresaTransportadora in = (EmpresaTransportadora) dados.removeEncomendaTransportador(dados.getEmpresaIn().getReferencia(), enc);
                    dados.sortEncomendaTransporteExp(enc, dados.getEmpresaIn().getReferencia());
                    System.out.println("O pedido recusado foi reencaminhado para o transportador: " + dados.sortEncomendaTransporteExp(enc, dados.getEmpresaIn().getReferencia()).getNome());
                    EmpresaTransportadora t = (EmpresaTransportadora) dados.sortEncomendaTransporteExp(enc, dados.getEmpresaIn().getReferencia());
                    dados.adicionaEncomendaTransportador(t, enc);
                    System.out.println(dados.sortEncomendaTransporteExp(enc, dados.getEmpresaIn().getReferencia()));
                    dados.setEmpresaIn(in);

                }
                if (op == 3) {
                    System.out.println("Ok!");
                }
            } else System.out.println("Não tem pedidos");

        }


        public void pedidos () {
            List<String> aceites = dados.getClienteIn().getEncomendas().values().stream().filter(e -> e.isAceiteTransportador() == true && !e.isAceiteCliente()).map(Encomenda::toStringNavpreco).collect(Collectors.toList());
            if (aceites.size() > 0) {
                System.out.println("Existem produtos aceites pelo transportador");
                Navegador n1 = new Navegador(aceites, 1, 5);
                n1.run();
                System.out.println("(1)->Aceitar (2)->Recusar (3) -> Cancelar");
                int op = Input.lerInt();

                if (op == 1) {
                    System.out.println("Confirme a encomenda em que aceita o transporte pago");
                    String ref = Input.lerString();
                    Encomenda encomenda = dados.getEncomendas().get(ref);
                    encomenda.setAceiteCliente(true);
                    String referencia = encomenda.getDistribuidor().getReferencia();
                    if (dados.getTransportador().get(referencia) instanceof EmpresaTransportadora) {
                        dados.setEnc(encomenda);
                        EmpresaTransportadora e = (EmpresaTransportadora) encomenda.getDistribuidor();
                        encomenda.setAceiteCliente(true);
                        encomenda.setDistribuidor(e);
                        e.setDisponibilidade(false);
                        dados.setEnc(encomenda);
                        e = dados.addEncomendaEmpresa();
                        Double custo = encomenda.getCustoProdutos() + encomenda.getCustoTransporte();
                        DecimalFormat df = new DecimalFormat("####0.00");
                        System.out.println("A sua encomenda encontra-se a caminho...\n\n\n\n\n");
                        System.out.println("Transportador ficou com um total de " + df.format(e.getNumeroKms()) + " kms percorridos");
                        System.out.println("Viagem concluida. O seu pedido foi realizado com sucesso" + "\nDuração da viagem: " + df.format(encomenda.getTempo()) + "mins\nCusto total do seu pedido: " + df.format(custo)
                                + "\nClassificação a atribuir ao seu transportador (0-5): ");
                        double y = Input.lerInt();
                        while (y > 5) {
                            System.out.println("Classificação a atribuir ao seu voluntario (0-5)");
                            y = Input.lerInt();
                        }

                        e.setClassificacao(y);
                        System.out.println("Voluntario ficou com classificação de: " + e.getClassificacao());
                        e.setDisponibilidade(true);
                        dados.addRegistoL();
                        dados.addRegistoC();
                        dados.addRegistoT();
                        dados.adicionaEncomenda(encomenda);
                        dados.adicionaTransportador(e);

                    } else System.out.println("Algo correu mal!");
                }


                if (op == 2) {
                    System.out.println("Confirme a encomenda em que rejeita o transporte pago");
                    String ref = Input.lerString();
                    dados.setEnc(dados.getEncomendas().get(ref));
                    Encomenda enc = dados.getEncomenda();
                    System.out.println("Pedido eliminado");
                    dados.removeEncomendaGeral(enc);
                }

                if (op == 3) {
                    System.out.println("Ok!");
                }
            } else {
                System.out.println("Sem pedidos");
            }
        }

        public void showencguer ( int x){
            LocalDate data;
            String date;
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
            System.out.println("Insira a data limite:(dd/mm/yyyy) ");
            date = Input.lerString();
            data = LocalDate.parse(date, formatter);
            if (x == 1) {
                List<String> a = dados.showEncomendasEmpresa(data).stream().map(Encomenda::toStringNav).collect(Collectors.toList());

                Navegador n1 = new Navegador(a, 10, 1);
                n1.run();
            }
            if (x == 2) {
                List<String> a = dados.showEncomendaUser(data).stream().map(Encomenda::toStringNav).collect(Collectors.toList());
                Navegador n1 = new Navegador(a, 10, 1);
                n1.run();

            }

            if (x == 3) {
                List<String> a = dados.showEncomendaLoja(data).stream().map(Encomenda::toStringNav).collect(Collectors.toList());
                Navegador n1 = new Navegador(a, 10, 1);
                n1.run();

            }
            if (x == 4) {
                List<String> a = dados.showEncomendasVoluntario(data).stream().map(Encomenda::toStringNav).collect(Collectors.toList());
                Navegador n1 = new Navegador(a, 10, 1);
                n1.run();
            }
        }

        public void insereProdutos () {
            Loja l = dados.ShowDadosL();
            Produto p = new Produto();
            System.out.println("Nome do produto que deseja adicionar");
            String nome = Input.lerString();
            System.out.println("Preco do produto");
            double preco = Input.lerDouble();
            System.out.println("Peso do produto");
            float peso = Input.lerFloat();
            p.setNome(nome);
            p.setPreco(preco);
            p.setPeso(peso);
            p = dados.geraReferenciaProduto(p, dados.getLojaIn());
            System.out.println("Produto adicionado com sucesso com a referencia " + p.getReferencia());
            Loja j = dados.getLojaIn();
            j.adicionaProdutoLoja(p);
            dados.setLojaIn(j);
            dados.adicionaLoja(j);
        }

        public void produtosDisponiveis () {
            System.out.println("Lista de produtos disponiveis para venda:\n");
            List<String> a = dados.getLojaIn().getProdutos().values().stream().map(Produto::navString).collect(Collectors.toList());
            Navegador n1 = new Navegador(a, 1, 5);
            n1.run();
        }


        public void showPreco () {

            do {
                showPreco.executa();

                switch (showPreco.getOp()) {
                    case 0:
                        break;
                    case 1:
                        totalFaturadoEmpresa();
                        break;
                    case 2:
                        totalFperiodo();
                        break;

                    default:
                        System.out.println("Opção inválida.");
                }
            } while (showPreco.getOp() != 0);

        }

        public void totalFaturadoEmpresa () {
            System.out.println("Total faturado pela empresa :" + dados.getEmpresaIn().getTotalFaturado());

        }

        public void totalFperiodo () {

            String email;
            LocalDate data;
            String date;

            email = dados.getEmpresaIn().getReferencia();
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
            System.out.println("Insira a limite faturação: (dd/mm/yyyy) ");
            date = Input.lerString();
            data = LocalDate.parse(date, formatter);
            System.out.println("Total faturado nesse periodo:" + dados.totalFaturadoPeriodo(email, data));

        }


        public void showdadosC () {

            User e = dados.ShowDadosU();
            System.out.println("Cliente:" + e.getNome());
            System.out.println("Referencia:" + e.getReferencia());
            System.out.println("Numero de encomendas em que esteve envolvido:" + e.getEncomendas().size());
            System.out.println("________________________________________________");
        }

        public void showdadosE () {
            EmpresaTransportadora e = dados.ShowDadosE();
            System.out.println("Empresa:" + e.getNome());
            System.out.println("Referencia:" + e.getReferencia());
            System.out.println("Numero de encomendas em que esteve envolvido:" + e.getEncomendas().size());
            System.out.println("Numero de kms percorridos: " + e.getNumeroKms());
            System.out.println("Classificação média: " + e.getClassificacao());
            System.out.println("Total faturado: " + e.getTotalFaturado());
            System.out.println("_______________________________________________");
        }

        public void showdadosV () {

            Voluntario v = dados.ShowDadosV();
            System.out.println("Empresa:" + v.getNome());
            System.out.println("Referencia:" + v.getReferencia());
            System.out.println("Numero de encomendas em que esteve envolvido:" + v.getEncomendas().size());
            System.out.println("Numero de kms percorridos: " + v.getNumeroKms());
            System.out.println("_______________________________________________");
        }

        public void showdadosL () {

            Loja l = dados.ShowDadosL();
            System.out.println("Loja:" + l.getNome());
            System.out.println("Referencia:" + l.getReferencia());
            System.out.println("Numero de encomendas em que esteve envolvido:" + l.getEncomendas().size());
            System.out.println("_______________________________________________");
        }

        public void estadoV () {
            if (dados.getVoluntarioIn().isDisponivel()) {
                System.out.println("Deseja não estar ativo para entregas?\n (1)->Sim (2)-> Não");
                int op = Input.lerInt();
                if (op == 1) {
                    Voluntario v = dados.getVoluntarioIn();
                    v.setDisponibilidade(false);
                    dados.adicionaTransportador(v);
                }
                if (op == 2) {

                }
            }
            if (!dados.getVoluntarioIn().isDisponivel()) {
                System.out.println("Deseja estar ativo para entregas?\n (1)->Sim (2)-> Não");
                int op = Input.lerInt();
                if (op == 1) {
                    Voluntario v = dados.getVoluntarioIn();
                    v.setDisponibilidade(true);
                    dados.adicionaTransportador(v);
                }
                if (op == 2) {

                }

            }
        }

        public void estadoE () {
            if (dados.getEmpresaIn().isDisponivel()) {
                System.out.println("Deseja não estar ativo para entregas?\n (1)->Sim (2)-> Não");
                int op = Input.lerInt();
                if (op == 1) {
                    EmpresaTransportadora v = dados.getEmpresaIn();
                    v.setDisponibilidade(false);
                    dados.adicionaTransportador(v);
                }
                if (op == 2) {

                }
            } else {
                System.out.println("Deseja estar ativo para entregas?\n (1)->Sim (2)-> Não");
                int op = Input.lerInt();
                if (op == 1) {
                    EmpresaTransportadora v = dados.getEmpresaIn();
                    v.setDisponibilidade(true);
                    dados.adicionaTransportador(v);
                }
                if (op == 2) {

                }

            }
        }


        public void registar () {
            String email, nome, password;
            int op, nif;
            double morada1, morada2;


            System.out.println("1. Sou Cliente");
            System.out.println("2. Sou Transportador afiliado a uma empresa");
            System.out.println("3. Sou Transportador voluntario");
            System.out.println("4. Sou Lojista");
            op = Input.lerInt();
            if (op > 4 || op < 1) {
                System.out.println("Opção invalida");

            } else {


                System.out.println("Insira o seu email: ");
                email = Input.lerString();

                System.out.println("Insira o seu nome: ");
                nome = Input.lerString();

                System.out.println("Insira a password: ");
                password = Input.lerString();

                System.out.println("Insira a coordenada x da sua morada ou localização atual:");
                morada1 = Input.lerDouble();
                System.out.println("Insira a coordenada y da sua morada ou localização atual:");
                morada2 = Input.lerDouble();

                Point2D.Double morada = new Point2D.Double(morada1, morada2);

                System.out.println("Insira o seu Nif");
                nif = Input.lerInt();

                if (op == 1) {
                    User c = new User();

                    c.setEmail(email);
                    c.setNome(nome);
                    c.setPassword(password);
                    c.setMorada(morada);
                    c.setNif(nif);
                    c = dados.geraReferenciaUser(c);
                    System.out.println(dados.getUsers());

                    try {
                        dados.registarUtilizadorEmail(c);
                        System.out.println("A sua referencia de cliente é: " + c.getReferencia());
                        System.out.println("Registado com sucesso");
                    } catch (Exception e) {
                        System.out.println(e);
                    }
                }
                if (op == 2) {
                    double taxa, raio;
                    System.out.println("Insira o seu raio de acao");
                    raio = Input.lerDouble();

                    System.out.println("Insira a taxa de transporte que pretende (0-15) (%)");
                    taxa = Input.lerDouble();
                    if (taxa > 15) {
                        while (taxa > 15) {
                            System.out.println("Insira a taxa de transporte que pretende (0-15) (%)");
                            taxa = Input.lerDouble();
                        }
                    }

                    taxa = taxa / 100;
                    EmpresaTransportadora p = new EmpresaTransportadora();
                    p.setRaio(raio);
                    p.setEmail(email);
                    p.setNome(nome);
                    p.setPassword(password);
                    p.setMorada(morada);
                    p.setNif(nif);
                    p.setTaxa(taxa);
                    p = dados.geraReferenciaTransportadorEmpresa(p);

                    try {
                        dados.registarEmpresaEmail(p);
                        System.out.println("A sua referencia de transportador é: " + p.getReferencia());

                        System.out.println("Empresa transportadora Registada com sucesso");
                    } catch (Exception e) {
                        System.out.println(e);
                    }

                }
                if (op == 3) {
                    double raio;
                    System.out.println("Insira o seu raio de acao");
                    raio = Input.lerDouble();

                    Voluntario p = new Voluntario();

                    p.setEmail(email);
                    p.setNome(nome);
                    p.setPassword(password);
                    p.setMorada(morada);
                    p.setNif(nif);
                    p.setRaio(raio);
                    p = dados.geraReferenciaTransportadorVoluntario(p);


                    try {
                        dados.registarVoluntarioEmail(p);
                        System.out.println("A sua referencia de voluntario é: " + p.getReferencia());
                        System.out.println("Voluntario Registado com sucesso");
                    } catch (Exception e) {
                        System.out.println(e);
                    }
                }
                if (op == 4) {

                    Loja l = new Loja();

                    l.setEmail(email);
                    l.setNome(nome);
                    l.setPassword(password);
                    l.setMorada(morada);
                    l.setNif(nif);
                    l = dados.geraReferenciaLoja(l);


                    try {

                        dados.RegistaLojaEmail(l);
                        System.out.println(dados.getLojas());
                        System.out.println(dados.getLojasEmail());

                        System.out.println("A sua referencia de loja é: " + l.getReferencia());
                        System.out.println("Registado com sucesso");
                    } catch (Exception e) {
                        System.out.println(e);
                    }

                }
            }


        }

        private void iniciaSessaoAux ( int op){
            String email, pass;

            System.out.println("Email: ");
            email = Input.lerString();
            System.out.println("Password: ");
            pass = Input.lerString();
            if (op == 1) {
                try {
                    dados.iniciaSessaoC(email, pass);
                    System.out.println("Sessão iniciada com sucesso");
                    perfilCliente();
                } catch (Exception e) {
                    System.out.println(e);
                }
            }
            if (op == 2) {
                try {
                    dados.iniciaSessaoE(email, pass);
                    System.out.println("Sessão iniciada com sucesso");
                    perfilEmpresa();
                } catch (Exception e) {
                    System.out.println(e);
                }
            }
            if (op == 3) {
                try {
                    dados.iniciaSessaoV(email, pass);
                    System.out.println("Sessão iniciada com sucesso");
                    perfilVoluntario();
                } catch (Exception e) {
                    System.out.println(e);
                }
            }
            if (op == 4) {
                try {
                    dados.iniciaSessaoL(email, pass);
                    System.out.println("Sessão iniciada com sucesso");
                    perfilLoja();
                } catch (Exception e) {
                    System.out.println(e);
                }
            }
        }


        private void iniciaSessao () {


            do {
                escolhaC.executa();

                switch (escolhaC.getOp()) {
                    case 0:
                        break;
                    case 1:
                        iniciaSessaoAux(1);
                        break;
                    case 2:
                        iniciaSessaoAux(2);
                        break;
                    case 3:
                        iniciaSessaoAux(3);
                        break;
                    case 4:
                        iniciaSessaoAux(4);
                        break;
                    default:
                        System.out.println("Opção inválida.");
                }
            } while (escolhaC.getOp() != 0);
        }

        public void showTop () {

            List<User> top;
            top = dados.topUsers();
            int i = 1;
            Iterator<User> it = top.iterator();
            while (it.hasNext()) {
                User c = it.next();
                System.out.println("Pos " + i + " " + c.getNome() + " " + c.getReferencia() + "| Total encomendas: " + c.getEncomendas().size());
                i++;
            }


        }

        public void showTopKm () {

            List<EmpresaTransportadora> top;
            top = dados.top10Kms();
            int i = 1;
            Iterator<EmpresaTransportadora> it = top.iterator();
            while (it.hasNext()) {
                EmpresaTransportadora c = it.next();
                System.out.println("Pos:" + " " + i + " " + c.getNome() + " " + c.getNumeroKms() + "kms");
                i++;
            }
            System.out.println("\n");
        }


        public void lerDadosGravados () {
            try {
                dados = TrazAqui.lerDados();
            } catch (IOException e) {
                dados = new TrazAqui();
                System.out.println("Não conseguiu ler os dados 1 !.");
            } catch (ClassNotFoundException e) {
                dados = new TrazAqui();
                System.out.println("Não conseguiu ler os dados 2 !");
            } catch (ClassCastException e) {
                dados = new TrazAqui();
                System.out.println("Não conseguiu ler os dados 3 !");
            }
        }


        public void carregaDados () {
            try {

                BufferedReader br = new BufferedReader(new FileReader("TrazAqui/logs.txt"));

                while (br.ready()) {
                    String linha = br.readLine();
                    tratalinhas(linha);
                }
                br.close();
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }

        }

        public void carregaDadosPersonalizados () {
            try {

                BufferedReader br = new BufferedReader(new FileReader("TrazAqui/logs2.txt"));

                while (br.ready()) {
                    String linha = br.readLine();
                    trataLinhasPersonalizado(linha);
                }
                br.close();
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }

        }


        public void tratalinhas (String linha){

            EmpresaTransportadora p = new EmpresaTransportadora();
            Voluntario v = new Voluntario();
            Encomenda enc = new Encomenda();
            User u = new User();
            Loja l = new Loja();
            StringTokenizer Tok = new StringTokenizer(linha, ":,");

            List<String> tokens = new ArrayList<>();
            while (Tok.hasMoreElements()) {
                tokens.add(Tok.nextToken());
            }


            if (tokens.get(0).equals("Utilizador")) {

                Point2D.Double coordenadascliente = new Point2D.Double();

                u.setReferencia(tokens.get(1));
                u.setNome(tokens.get(2));
                double x = parseDouble(tokens.get(3));
                double y = parseDouble(tokens.get(4));
                coordenadascliente.setLocation(x, y);
                u.setMorada(coordenadascliente);

                try {
                    dados.registarUtilizador(u);


                    System.out.println("Utilizador Registado com sucesso");
                } catch (Exception e) {
                    System.out.println(e);
                }
            } else if (tokens.get(0).equals("Loja")) {

                l.setReferencia(tokens.get(1));
                l.setNome(tokens.get(2));
                double x = Double.parseDouble(tokens.get(3));
                double y = Double.parseDouble(tokens.get(4));
                Point2D.Double coordenadas = new Point2D.Double();
                coordenadas.setLocation(x, y);
                l.setMorada(coordenadas);


                try {
                    dados.RegistaLoja(l);
                    System.out.println("Loja Registada com sucesso");

                } catch (Exception e) {
                    System.out.println(e);
                }
            } else if (tokens.get(0).equals("Transportadora")) {

                p.setReferencia(tokens.get(1));
                p.setNome(tokens.get(2));
                double x = Double.parseDouble(tokens.get(3));
                double y = Double.parseDouble(tokens.get(4));
                p.setNif(Long.parseLong(tokens.get(5)));
                Point2D.Double coordenadastransporte = new Point2D.Double();
                coordenadastransporte.setLocation(x, y);
                p.setMorada(coordenadastransporte);
                p.setRaio(Float.parseFloat(tokens.get(6)));
                p.setTaxa(Double.parseDouble(tokens.get(7)));


                try {
                    dados.registarEmpresa(p);
                    System.out.println("Transportadora Registado com sucesso");

                } catch (Exception e) {
                    System.out.println(e);
                }
            } else if (tokens.get(0).equals("Voluntario")) {
                v.setReferencia(tokens.get(1));
                v.setNome(tokens.get(2));
                v.setRaio(parseFloat(tokens.get(5)));
                double x = Double.parseDouble(tokens.get(3));
                double y = Double.parseDouble(tokens.get(4));
                Point2D.Double coordenadasvoluntario = new Point2D.Double();
                coordenadasvoluntario.setLocation(x, y);
                v.setMorada(coordenadasvoluntario);

                try {
                    dados.registarVoluntario(v);

                    System.out.println("Voluntario Registado com sucesso");
                } catch (Exception e) {
                    System.out.println(e);
                }


            } else if (tokens.get(0).equals("Encomenda")) {

                enc.setReferencia(tokens.get(1));
                enc.setComprador(dados.getUsers().get(tokens.get(2)));
                enc.setLoja(dados.getLojas().get(tokens.get(3)));
                enc.setPeso(parseFloat(tokens.get(4)));


                List<Produto> aux = new ArrayList<>();
                int i = 5;
                while (i < tokens.size()) {
                    Produto x = new Produto();
                    x.setReferencia(tokens.get(i++));
                    x.setNome(tokens.get(i++));
                    x.setPreco(Double.parseDouble(tokens.get(i++)));
                    dados.adicionaProdutoLoja(x, tokens.get(3));
                    x.setQuantidade(Float.parseFloat(tokens.get(i++)));

                    aux.add(x);


                }
                enc.setProdutos(aux);


                dados.adicionaEncomenda(enc);


                User e = dados.getUsers().get(tokens.get(2));
                e.adicionaEncomendaUser(enc);
                dados.adicionaUser(e);


                Loja j = dados.getLojas().get(tokens.get(3));
                j.adicionaEncomendaLoja(enc);
                dados.adicionaLoja(j);


                System.out.println("produtos adicionados");

            } else if (tokens.get(0).equals("Aceite")) {

                Encomenda e = dados.getEncomendas().get(tokens.get(1));
                e.setEfetuada(true);
                dados.adicionaEncomenda(e);
                System.out.println("Encomendas efetuadas lidas");

                try {
                } catch (NumberFormatException nfe) {

                }


            }

        }

        public void trataLinhasPersonalizado (String linha){

            EmpresaTransportadora p = new EmpresaTransportadora();
            Voluntario v = new Voluntario();
            Encomenda enc = new Encomenda();
            User u = new User();
            Loja l = new Loja();
            StringTokenizer Tok = new StringTokenizer(linha, ":,");

            List<String> tokens = new ArrayList<>();
            while (Tok.hasMoreElements()) {
                tokens.add(Tok.nextToken());
            }


            if (tokens.get(0).equals("Utilizador")) {

                Point2D.Double coordenadascliente = new Point2D.Double();

                u.setReferencia(tokens.get(1));
                u.setNome(tokens.get(2));
                double x = parseDouble(tokens.get(3));
                double y = parseDouble(tokens.get(4));
                coordenadascliente.setLocation(x, y);
                u.setMorada(coordenadascliente);
                u.setEmail(tokens.get(5));
                u.setPassword(tokens.get(6));

                try {
                    dados.registarUtilizador(u);


                    System.out.println("Utilizador Registado com sucesso");
                } catch (Exception e) {
                    System.out.println(e);
                }
            } else if (tokens.get(0).equals("Loja")) {

                l.setReferencia(tokens.get(1));
                l.setNome(tokens.get(2));
                double x = Double.parseDouble(tokens.get(3));
                double y = Double.parseDouble(tokens.get(4));
                Point2D.Double coordenadas = new Point2D.Double();
                coordenadas.setLocation(x, y);
                l.setMorada(coordenadas);
                l.setEmail(tokens.get(5));
                l.setPassword(tokens.get(6));
                l.setEspera(parseFloat(tokens.get(7)));
                l.setFila(Integer.parseInt(tokens.get(8)));


                try {
                    dados.RegistaLoja(l);
                    System.out.println("Loja Registada com sucesso");

                } catch (Exception e) {
                    System.out.println(e);
                }
            } else if (tokens.get(0).equals("Transportadora")) {

                p.setReferencia(tokens.get(1));
                p.setNome(tokens.get(2));
                double x = Double.parseDouble(tokens.get(3));
                double y = Double.parseDouble(tokens.get(4));
                p.setNif(Long.parseLong(tokens.get(5)));
                Point2D.Double coordenadastransporte = new Point2D.Double();
                coordenadastransporte.setLocation(x, y);
                p.setMorada(coordenadastransporte);
                p.setRaio(Float.parseFloat(tokens.get(6)));
                p.setTaxa(Double.parseDouble(tokens.get(7)));
                p.setEmail(tokens.get(8));
                p.setPassword(tokens.get(9));
                p.setNumeroKms(parseDouble(tokens.get(10)));
                p.setVelocidadeMedia(parseDouble(tokens.get(11)));
                p.setClassificacao(parseDouble(tokens.get(12)));
                p.setTotalFaturado(parseDouble(tokens.get(13)));


                try {
                    dados.registarEmpresa(p);
                    System.out.println("Transportadora Registado com sucesso");

                } catch (Exception e) {
                    System.out.println(e);
                }
            } else if (tokens.get(0).equals("Voluntario")) {
                v.setReferencia(tokens.get(1));
                v.setNome(tokens.get(2));
                v.setRaio(parseFloat(tokens.get(5)));
                double x = Double.parseDouble(tokens.get(3));
                double y = Double.parseDouble(tokens.get(4));
                Point2D.Double coordenadasvoluntario = new Point2D.Double();
                coordenadasvoluntario.setLocation(x, y);
                v.setMorada(coordenadasvoluntario);
                v.setEmail(tokens.get(6));
                v.setPassword(tokens.get(7));
                v.setNumeroKms(parseDouble(tokens.get(8)));
                v.setVelocidadeMedia(parseDouble(tokens.get(9)));
                v.setClassificacao(parseDouble(tokens.get(10)));

                try {
                    dados.registarVoluntario(v);

                    System.out.println("Voluntario Registado com sucesso");
                } catch (Exception e) {
                    System.out.println(e);
                }


            } else if (tokens.get(0).equals("Encomenda")) {

                enc.setReferencia(tokens.get(1));
                enc.setComprador(dados.getUsers().get(tokens.get(2)));
                enc.setLoja(dados.getLojas().get(tokens.get(3)));
                enc.setPeso(parseFloat(tokens.get(4)));
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd-MM-yyyy HH-mm");
                enc.setData(LocalDateTime.parse(tokens.get(5), formatter));


                List<Produto> aux = new ArrayList<>();
                int i = 6;
                while (i < tokens.size()) {
                    Produto x = new Produto();
                    x.setReferencia(tokens.get(i++));
                    x.setNome(tokens.get(i++));
                    x.setPreco(Double.parseDouble(tokens.get(i++)));
                    Produto y = new Produto(x);
                    x.setQuantidade(Double.parseDouble(tokens.get(i++)));
                    x.setPeso(Double.parseDouble(tokens.get(i++)));
                    y.setPeso(x.getPeso());
                    dados.adicionaProdutoLoja(y, tokens.get(3));


                    aux.add(x);


                }
                enc.setProdutos(aux);


                dados.adicionaEncomenda(enc);


                //atualizar encomendas nos users
                User e = dados.getUsers().get(tokens.get(2));
                e.adicionaEncomendaUser(enc);
                dados.adicionaUser(e);


                //atualizar encomenda nas loja
                Loja j = dados.getLojas().get(tokens.get(3));
                j.adicionaEncomendaLoja(enc);
                dados.adicionaLoja(j);


                System.out.println("produtos adicionados");

            } else if (tokens.get(0).equals("Aceite")) {

                Encomenda e = dados.getEncomendas().get(tokens.get(1));
                e.setDistribuidor(dados.getTransportador().get(tokens.get(2)));
                e.setCustoProdutos(Double.parseDouble(tokens.get(3)));
                e.setCustoTransporte(Double.parseDouble(tokens.get(4)));
                e.setEfetuada(true);
                dados.adicionaEncomenda(e);
                if (dados.getTransportador().get(tokens.get(2)) instanceof EmpresaTransportadora) {
                    EmpresaTransportadora a = (EmpresaTransportadora) dados.getTransportador().get(tokens.get(2));
                    dados.adicionaEncomendaTransportador(a, e);
                }
                if (dados.getTransportador().get(tokens.get(2)) instanceof Voluntario) {
                    Voluntario volun = (Voluntario) dados.getTransportador().get(tokens.get(2));
                    dados.adicionaEncomendaVoluntario(volun, e);
                }

                System.out.println("Encomendas efetuadas lidas");

                try {
                } catch (NumberFormatException nfe) {

                }


            }

        }

    }







