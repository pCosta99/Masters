package app.views;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Scanner;

import app.controllers.GestorRegistos;
import app.enums.TiposUtilizadoresEnum;
import app.exceptions.CodigoProdutoNaoExistenteException;
import app.exceptions.EmpresaTransportadoraNaoExistenteException;
import app.exceptions.EncomendaJaCanceladaException;
import app.exceptions.EncomendaNaoExistenteException;
import app.exceptions.EstadoRegressivoException;
import app.exceptions.LojaNaoExistenteException;
import app.exceptions.SemPermissaoAprovarTransporteException;
import app.exceptions.SemPermissaoClassificarEntregaException;
import app.exceptions.SemPermissaoEfectuarEncomendaException;
import app.exceptions.UtilizadorNaoExistenteException;
import app.interfaces.IInfo;
import app.models.Encomenda;
import app.models.LinhaEncomenda;
import app.models.Loja;
import app.models.Produto;
import app.models.RegistoEncomenda;
import app.models.Transportador;
import static java.lang.System.out;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;

public class MenuUtilizador {
    private GestorRegistos gr;
    private Scanner scanner;
    private static String strOptInvalida = "Opção Inválida!!\n";
    private static String strProdutos = "*************** Produtos ***************";
    private static String strEncomendas = "*************** Encomendas ***************";
    private static String strLojas = "**************** Lojas *****************";
    private static String strTransportadores = "************* Transportadores ***************";
    private static String strSemEnc = "Sem encomendas disponiveis";
    private static String strSemLojas = "Sem Lojas disponiveis";
    private static String strSemProdutos = "Sem Produtos disponiveis";
    private static String strSemTransportadores = "Sem Transportadores disponiveis";


    /**
     * Método construtor parametrizado
     */
    public MenuUtilizador(GestorRegistos g) {
        scanner = new Scanner(System.in);
        this.gr = g;
    }

    /**
     * Método que cria o menu loja
     */
    public void print() {
        int op;

        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("************* Menu Utilizador **************");
            out.println("*                                          *");
            out.println("*  [1] Solicitar encomenda                 *");
            out.println("*  [2] Aceitar entrega de encomenda        *");
            out.println("*  [3] Listagem de encomendas efectuadas   *");
            out.println("*  [4] Avaliar entrega de encomenda        *");
            out.println("*  [5] Listagem de encomendas por estado   *");
            out.println("*  [0] Sair                                *");
            out.println("*                                          *");
            out.println("********************************************");
            out.println("Opção: ");

            op = scanner.nextInt();


            switch (op) {
                case 1:
                    menuSolicitarEncomenda();
                    break;
                case 2:
                    menuAceitaEntregaEncomenda();
                    break;
                case 3:
                    menuListagemEfetuadas();
                    break;
                case 4:
                    menuEntregasParaAvaliacao();
                    break;
                case 5:
                    menuListagemEstado();
                    break;
                case 0:
                    gr.logout();
                    flagContinue = false;
                    break;

                default:
                    out.println(strOptInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
            }
        }
    }

    private void menuListagemEstado() {
        String op;

        List<RegistoEncomenda> encomendas = new ArrayList<>(gr.listagemPorUtilizador());
        MenuPaginatedEncomendas menupag = new MenuPaginatedEncomendas(encomendas, scanner, true);

        if (!encomendas.isEmpty()) {
            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strEncomendas);
                op = menupag.print();


                switch (op) {
                    case "-":
                        break;
                    case "+":
                        break;
                    case "0":
                        flagContinue = false;
                        break;

                    default:
                        out.println(strOptInvalida);
                        scanner.nextLine();
                        scanner.nextLine();
                }

            }
        } else {
            out.println(strSemEnc);
            scanner.nextLine();
            scanner.nextLine();
        }
    }

    private void menuEntregasParaAvaliacao() {
        String op;

        List<RegistoEncomenda> encomendas = new ArrayList<>();
        try {
            encomendas = new ArrayList<>(gr.listagemEncomendasAguardarClassificacao());
        } catch (SemPermissaoClassificarEntregaException e1) {
            out.println(e1.getMessage());
        }
        MenuPaginatedEncomendas menupag = null;

        boolean flagContinue = true;
        boolean refresh = true;
        while (flagContinue) {
            clearConsole();
            out.println(strEncomendas);
            if (refresh) {
                try {
                    encomendas = new ArrayList<>(gr.listagemEncomendasAguardarClassificacao());
                } catch (SemPermissaoClassificarEntregaException e1) {
                    out.println(e1.getMessage());
                }
                if (!encomendas.isEmpty()) {
                    menupag = new MenuPaginatedEncomendas(encomendas, scanner);

                } else {
                    out.println(strSemEnc);
                    scanner.nextLine();
                    scanner.nextLine();
                    break;
                }
                refresh = false;
            }
            op = menupag.print();


            switch (op) {
                case "-":
                    break;
                case "+":
                    break;
                case "0":
                    flagContinue = false;
                    break;

                default:
                    try {
                        RegistoEncomenda re = gr.getRegistoEncomenda(op);
                        if (encomendas.contains(re)) {
                            refresh = menuClassificacao(op);
                        }
                    } catch (EncomendaNaoExistenteException | EncomendaJaCanceladaException
                            | EstadoRegressivoException
                            | SemPermissaoClassificarEntregaException e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }

            }

        }
    }

    private boolean menuClassificacao(String codEnc)
            throws EncomendaNaoExistenteException, EncomendaJaCanceladaException,
            EstadoRegressivoException, SemPermissaoClassificarEntregaException {
        boolean refresh = false;
        int classif = -1;
        boolean classOk = false;
        while (!classOk) {
            clearConsole();
            out.println("Classificação(0-10): ");
            boolean opValida = false;
            while (!opValida) {
                try {
                    classif = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(strOptInvalida);
                    scanner.nextLine();
                }
            }
            if (classif >= 0 && classif <= 10) {
                out.println(" 1 - Confirmar");
                out.println(" 0 - Cancelar");
                opValida = false;
                int valConf = -1;
                while (!opValida) {
                    try {
                        valConf = scanner.nextInt();
                        opValida = true;

                    } catch (Exception e) {
                        out.println(strOptInvalida);
                        scanner.nextLine();
                    }
                }
                if (valConf == 1) {
                    gr.classificaEntrega(codEnc, classif);
                    refresh = true;
                }
                classOk = true;
            } else {
                out.println("Valor invalido de classificação!");
                scanner.nextLine();
            }
        }
        return refresh;
    }

    private void menuListagemEfetuadas() {
        String op;
        Date dtInicio = null;
        Date dtFim = null;
        boolean flagContinue = true;
        boolean dataOk = false;
        while (flagContinue) {
            clearConsole();
            out.println("************* Listagem Encomendas *****************");
            out.println("Introduza a data inicial de pesquisa (dd-MM-yyyy): (0 - Sair)");
            SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
            boolean cancel = false;
            while (!dataOk) {
                try {
                    op = scanner.next();
                    if (op.equals("0")) {
                        cancel = true;
                        break;
                    }
                    dtInicio = formatter.parse(op);
                    dataOk = true;
                } catch (ParseException e) {
                    out.println("Data em formato errado, tente de novo (dd-MM-yyyy) (0 - Sair)");
                }
            }
            if (cancel) {
                break;
            }
            dataOk = false;
            out.println("Introduza a data final de pesquisa (dd-MM-yyyy): (0 - Sair)");
            while (!dataOk) {
                try {
                    op = scanner.next();
                    if (op.equals("0")) {
                        cancel = true;
                        break;
                    }
                    dtFim = formatter.parse(op);
                    dataOk = true;
                } catch (ParseException e) {
                    out.println("Data em formato errado, tente de novo (dd-MM-yyyy) (0 - Sair)");
                }
            }
            if (cancel) {
                break;
            }
            dataOk = false;

            out.println("Opção: ");
            out.println("* [1] - Continuar                                 *");
            out.println("* [0] - Sair                                      *");
            op = scanner.next();


            switch (op) {
                case "1":
                    menuListagemEfectIntervalo(dtInicio, dtFim);
                    flagContinue = false;
                    break;
                case "0":
                    flagContinue = false;
                    break;

                default:
                    out.println(strOptInvalida);
                    scanner.nextLine();
                    scanner.nextLine();

            }

        }


    }

    private void menuListagemEfectIntervalo(Date dtInicio, Date dtFim) {
        String op;
        LocalDateTime dataInicio = Instant.ofEpochMilli(dtInicio.getTime())
                .atZone(ZoneId.systemDefault()).toLocalDateTime();
        LocalDateTime dataFim = Instant.ofEpochMilli(dtFim.getTime()).atZone(ZoneId.systemDefault())
                .toLocalDateTime();
        List<IInfo> transportadores =
                new ArrayList<>(gr.listagemTransportadoresIntervaloTempo(dataInicio, dataFim));
        if (!transportadores.isEmpty()) {
            MenuPaginatedUtilizadores menupag =
                    new MenuPaginatedUtilizadores(transportadores, scanner);

            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strTransportadores);
                op = menupag.print();


                switch (op) {
                    case "-":
                        break;
                    case "+":
                        break;
                    case "0":
                        flagContinue = false;
                        break;

                    default:
                        try {
                            if (gr.getUtilizador(op) instanceof Transportador) {
                                String codTransp = op;
                                if (transportadores.stream()
                                        .anyMatch(t -> t.getEmail().equals(codTransp))) {
                                    menuListaEncTransIntTempo(codTransp, dataInicio, dataFim);
                                } else {
                                    out.println(strOptInvalida);
                                    scanner.nextLine();
                                    scanner.nextLine();
                                }
                            }
                        } catch (UtilizadorNaoExistenteException e) {
                            out.println(e.getMessage());
                            scanner.nextLine();
                            scanner.nextLine();
                        }
                }

            }
        } else {
            out.println(strSemTransportadores);
            scanner.nextLine();
            scanner.nextLine();
        }
    }

    private void menuListaEncTransIntTempo(String codTransp, LocalDateTime dataInicio,
            LocalDateTime dataFim) {
        String op;

        List<RegistoEncomenda> encomendas = new ArrayList<>(
                gr.listagemEncTranspPorEmIntervaloTempo(codTransp, dataInicio, dataFim));

        if (!encomendas.isEmpty()) {

            MenuPaginatedEncomendas menupag = new MenuPaginatedEncomendas(encomendas, scanner);
            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strEncomendas);
                op = menupag.print();


                switch (op) {
                    case "-":
                        break;
                    case "+":
                        break;
                    case "0":
                        flagContinue = false;
                        break;
                    default:
                        try {
                            RegistoEncomenda re = gr.getRegistoEncomenda(op);
                            if (encomendas.contains(re)) {
                                out.println(re.toString());
                            } else {
                                out.println(strOptInvalida);
                                scanner.nextLine();
                                scanner.nextLine();
                            }
                        } catch (EncomendaNaoExistenteException e) {
                            out.println(e.getMessage());
                            scanner.nextLine();
                            scanner.nextLine();
                        }
                }

            }
        } else {
            out.println(strSemEnc);
            scanner.nextLine();
            scanner.nextLine();
        }
    }

    /**
     * Método que cria o menu que aceita a entrega encomenda
     */
    private void menuAceitaEntregaEncomenda() {
        String op;

        List<RegistoEncomenda> encomendas = null;
        try {
            encomendas = new ArrayList<>(gr.listagemEncomendasAguardarAprovacao());
        } catch (SemPermissaoAprovarTransporteException e1) {
            out.println(e1.getMessage());
        }
        MenuPaginatedEncomendas menupag = null;

        boolean flagContinue = true;
        boolean refresh = true;
        while (flagContinue) {
            clearConsole();
            out.println(strEncomendas);
            if (refresh) {
                try {
                    encomendas = new ArrayList<>(gr.listagemEncomendasAguardarAprovacao());
                } catch (SemPermissaoAprovarTransporteException e1) {
                    out.println(e1.getMessage());
                }
                if (encomendas != null && !encomendas.isEmpty()) {
                    menupag = new MenuPaginatedEncomendas(encomendas, scanner);
                    refresh = false;

                } else {
                    out.println(strSemEnc);
                    scanner.nextLine();
                    scanner.nextLine();
                    return;
                }
            }
            op = menupag.print();


            switch (op) {
                case "-":
                    break;
                case "+":
                    break;
                case "0":
                    flagContinue = false;
                    break;

                default:
                    String codEnc = op;
                    if (!gr.existeEncomenda(codEnc)) {
                        out.println("Código de Encomenda não existe!");
                        scanner.nextLine();
                        scanner.nextLine();
                    } else if (encomendas.stream()
                            .anyMatch(e -> e.getEncomenda().getCodEnc().equals(codEnc))) {

                        refresh = menuListaTransportadoresEntrega(op);
                    } else {
                        out.println(strOptInvalida);
                        scanner.nextLine();
                        scanner.nextLine();
                    }

            }

        }
    }

    private boolean menuListaTransportadoresEntrega(String codEncomenda) {
        List<String> listaTransportadores = null;
        boolean flagContinue = true;
        boolean refresh = false;
        try {
            listaTransportadores = gr.listaTransportadoresAguardaAprovacaoEnc(codEncomenda);

        } catch (EncomendaNaoExistenteException e) {
            out.print(e.getMessage());
            flagContinue = false;
        }
        String op;
        while (flagContinue) {
            clearConsole();
            out.println("************* Transportador *************");
            if (refresh) {
                try {
                    listaTransportadores = gr.listaTransportadoresAguardaAprovacaoEnc(codEncomenda);

                } catch (EncomendaNaoExistenteException e) {
                    out.print(e.getMessage());
                    flagContinue = false;
                }
            }
            for (String codTrans : listaTransportadores) {
                try {
                    out.println("* [" + codTrans + "] "
                            + gr.getPrecoTransporte(codEncomenda, codTrans) + "€ *");
                } catch (UtilizadorNaoExistenteException | EncomendaNaoExistenteException
                        | LojaNaoExistenteException
                        | EmpresaTransportadoraNaoExistenteException e) {
                    out.println(e.getMessage());
                    scanner.nextLine();
                    scanner.nextLine();
                }
            }
            out.println("0 - Cancelar");
            op = scanner.next();


            switch (op) {
                case "0":
                    flagContinue = false;
                    break;

                default:
                    try {
                        if (listaTransportadores.contains(op)) {
                            gr.aprovaPreco(codEncomenda, op);
                            refresh = true;
                        } else {
                            out.println(strOptInvalida);
                            scanner.nextLine();
                            scanner.nextLine();
                        }
                    } catch (EncomendaNaoExistenteException | EncomendaJaCanceladaException
                            | EstadoRegressivoException | SemPermissaoAprovarTransporteException
                            | UtilizadorNaoExistenteException | LojaNaoExistenteException
                            | EmpresaTransportadoraNaoExistenteException e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }

            }

        }

        return refresh;
    }

    /**
     * Método que cria o menu de listagem de Lojas para depois selecionar produtos
     */
    private void menuSolicitarEncomenda() {
        String op;

        List<IInfo> lojas =
                new ArrayList<>(gr.listagemUtilizadoresTipo(TiposUtilizadoresEnum.LOJA));

        if (!lojas.isEmpty()) {
            MenuPaginatedUtilizadores menupag = new MenuPaginatedUtilizadores(lojas, scanner);

            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strLojas);
                op = menupag.print();


                switch (op) {
                    case "-":
                        break;
                    case "+":
                        break;
                    case "0":
                        flagContinue = false;
                        break;

                    default:
                        try {
                            IInfo user = gr.getUtilizador(op);
                            if (lojas.contains(user) && user instanceof Loja) {
                                menuEfetuarEncomenda(op);
                            } else {
                                out.println(strOptInvalida);
                                scanner.nextLine();
                                scanner.nextLine();
                            }
                        } catch (UtilizadorNaoExistenteException e) {
                            out.println(e.getMessage());
                            scanner.nextLine();
                            scanner.nextLine();
                        }
                }

            }

        } else {
            out.println(strSemLojas);
            scanner.nextLine();
            scanner.nextLine();
        }
    }

    /**
     * Método que cria o menu de listagem de produtos
     */
    private void menuEfetuarEncomenda(String codLoja) {
        String op;

        List<Produto> produtos = new ArrayList<>(gr.listaProdutos(codLoja));

        if (!produtos.isEmpty()) {
            MenuPaginatedProdutos menupag = new MenuPaginatedProdutos(produtos, scanner, true);
            List<LinhaEncomenda> linhasEnc = new ArrayList<>();


            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strProdutos);
                op = menupag.print();
                out.println();

                switch (op) {
                    case "-":
                        break;
                    case "+":
                        break;
                    case "CONFIRMAR":
                        if (!linhasEnc.isEmpty()) {
                            Encomenda novaEncomenda = new Encomenda();
                            novaEncomenda.setLinhasEncomenda(linhasEnc);
                            try {
                                gr.inserirPedidoLoja(codLoja, novaEncomenda);
                            } catch (SemPermissaoEfectuarEncomendaException e) {
                                out.println(e.getMessage());
                                scanner.nextLine();
                                scanner.nextLine();
                            }
                        }
                        flagContinue = false;
                        break;
                    case "0":
                        flagContinue = false;
                        break;

                    default:
                        if (gr.existeProduto(codLoja, op)) {

                            LinhaEncomenda novaLinha = null;
                            try {
                                novaLinha = menuSelecaoProduto(codLoja, op);
                            } catch (CodigoProdutoNaoExistenteException e) {
                                out.println(e.getMessage());
                                scanner.nextLine();
                                scanner.nextLine();
                            }
                            if (novaLinha != null) {
                                linhasEnc.add(novaLinha);
                            }
                        } else {
                            out.println(strOptInvalida);
                            scanner.nextLine();
                            scanner.nextLine();
                        }
                }

            }

        } else {
            out.println(strSemProdutos);
            scanner.nextLine();
            scanner.nextLine();
        }
    }

    private LinhaEncomenda menuSelecaoProduto(String codLoja, String codProduto)
            throws CodigoProdutoNaoExistenteException {
        LinhaEncomenda le = null;
        Produto produto = gr.devolveProduto(codLoja, codProduto);
        int op = -1;
        clearConsole();
        out.println("Quantidade(Cancelar < 1): ");
        boolean opValida = false;
        while (!opValida) {
            try {
                op = scanner.nextInt();
                opValida = true;

            } catch (Exception e) {
                out.println(strOptInvalida);
                scanner.nextLine();
                scanner.nextLine();
            }
        }
        if (op > 0) {
            le = new LinhaEncomenda(produto, op);
        }
        return le;
    }


    private void clearConsole() {
        // try {
        // String os = System.getProperty("os.name");

        // if (os.contains("Windows")) {
        // Runtime.getRuntime().exec("cls");
        // } else {
        // Runtime.getRuntime().exec("clear");
        // }
        // } catch (Exception e) {
        // // Handle any exceptions.
        // }
        out.print("\033[H\033[2J");
        out.flush();
    }

}
