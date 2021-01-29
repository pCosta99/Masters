package app.views;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import app.controllers.GestorRegistos;
import app.enums.EstadosEncomendaEnum;
import app.exceptions.CodigoProdutoJaExistenteException;
import app.exceptions.CodigoProdutoNaoExistenteException;
import app.exceptions.LojaNaoExistenteException;
import app.exceptions.UtilizadorNaoExistenteException;
import app.models.Loja;
import app.models.Produto;
import app.models.RegistoEncomenda;

import static java.lang.System.out;

public class MenuLoja {
    private GestorRegistos gr;
    private Scanner scanner;
    private static String strOptInvalida = "Opção Inválida!!\n";
    private static String strProdutos = "*************** Produtos ***************";
    private static String strSemEnc = "Sem encomendas disponiveis";
    private static String strSemProdutos = "Sem produtos disponiveis";

    /**
     * Método construtor parametrizado scanner.next();
     */
    public MenuLoja(GestorRegistos g) {
        scanner = new Scanner(System.in);
        this.gr = g;
    }

    /**
     * Método que cria o menu loja
     */
    public void print() {
        int op = -1;

        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("*************** Menu Loja ***************");
            out.println("*                                          *");
            out.println("*  [1] Sinalizar encomenda entrega         *");
            out.println("*  [2] Gestão de Produtos                  *");
            if (gr.getUtilizadorActual() instanceof Loja) {
                Loja l = (Loja) gr.getUtilizadorActual();
                if (l.temFilaEspera()) {
                    out.println("*  [3] Pessoas em fila de espera           *");
                }
            }
            out.println("*  [0] Sair                                *");
            out.println("*                                          *");
            out.println("********************************************\n");
            out.println("Opção: ");

            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(strOptInvalida);
                    scanner.nextLine();
                }
            }

            switch (op) {
                case 1:
                    menuSinalizaEncomenda();
                    break;
                case 2:
                    menuGestaoProdutos();
                    break;
                case 3:
                    if (gr.getUtilizadorActual() instanceof Loja) {
                        Loja l = (Loja) gr.getUtilizadorActual();
                        if (l.temFilaEspera()) {
                            menuMostraFilaEspera();
                        }
                    } else {
                        out.println(strOptInvalida);
                    }
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

    /**
     * Método que cria o menu que sinaliza encomenda
     */
    private void menuSinalizaEncomenda() {
        String op;

        List<RegistoEncomenda> encomendas =
                new ArrayList<>(gr.listagemPorUtilizadorPorData(EstadosEncomendaEnum.ABERTA));
        MenuPaginatedEncomendas menupag = new MenuPaginatedEncomendas(encomendas, scanner);


        boolean flagContinue = true;
        boolean refresh = false;
        while (flagContinue) {
            clearConsole();
            out.println("*************** Encomendas ***************");
            if (refresh) {
                encomendas = new ArrayList<>(
                        gr.listagemPorUtilizadorPorData(EstadosEncomendaEnum.ABERTA));
                menupag = new MenuPaginatedEncomendas(encomendas, scanner);
                refresh = false;
            }
            if (!encomendas.isEmpty()) {
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
                            String codEnc = op;
                            if (encomendas.stream()
                                    .anyMatch(re -> re.getEncomenda().getCodEnc().equals(codEnc))) {

                                gr.encomendaProntaEntrega(codEnc);
                                refresh = true;

                            } else {
                                out.println(strOptInvalida);
                                scanner.nextLine();
                                scanner.nextLine();
                            }
                        } catch (Exception e) {
                            out.println(strOptInvalida);
                            out.println(e.getMessage());
                            scanner.nextLine();
                            scanner.nextLine();
                        }
                }

            } else {
                out.println(strSemEnc);
                flagContinue = false;
                scanner.nextLine();
                scanner.nextLine();
            }
        }
    }

    private void menuMostraFilaEspera() {
        int op = -1;
        clearConsole();
        out.println("*************** FILA DE ESPERA ***************");
        out.println("*Indique quantidade                         **");

        boolean opValida = false;
        while (!opValida) {
            try {
                op = scanner.nextInt();
                opValida = true;

            } catch (Exception e) {
                out.println(strOptInvalida);
                scanner.nextLine();
            }
        }
        try {
            gr.acrescentaFilaEspera(op);
        } catch (LojaNaoExistenteException | UtilizadorNaoExistenteException e) {
            out.println(e.getMessage());
            scanner.nextLine();
            scanner.nextLine();
        }

    }



    /**
     * Método que cria o menu de gestao de produtos
     */
    private void menuGestaoProdutos() {
        int op = -1;

        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("*********** Gestão de Produtos *************");
            out.println("*  [1] Listagem                            *");
            out.println("*  [2] Adicionar                           *");
            out.println("*  [3] Atualizar                           *");
            out.println("*  [4] Remover                             *");
            out.println("*  [0] Retroceder                          *");
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(strOptInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    menuListaProdutos();
                    break;
                case 2:
                    menuAdicionaProduto();
                    break;
                case 3:
                    menuAtualizacaoProduto();
                    break;
                case 4:
                    menuRemoverProduto();
                    break;
                case 0:
                    flagContinue = false;
                    break;

                default:
                    out.println(strOptInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
            }

        }
    }

    private void menuRemoverProduto() {
        String op;

        List<Produto> produtos =
                new ArrayList<>(gr.listaProdutos(gr.getUtilizadorActual().getEmail()));
        MenuPaginatedProdutos menupag = new MenuPaginatedProdutos(produtos, scanner);

        if (!produtos.isEmpty()) {

            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strProdutos);
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
                            if (gr.existeProduto(gr.getUtilizadorActual().getEmail(), op)) {
                                gr.removeProduto(gr.getUtilizadorActual().getEmail(), op);
                                produtos = new ArrayList<>(
                                        gr.listaProdutos(gr.getUtilizadorActual().getEmail()));
                                if (!produtos.isEmpty()) {

                                    menupag = new MenuPaginatedProdutos(produtos, scanner);
                                } else {
                                    out.println(strSemProdutos);
                                    scanner.nextLine();
                                    scanner.nextLine();
                                }

                            } else {
                                out.println(strOptInvalida);
                                scanner.nextLine();
                                scanner.nextLine();
                            }

                        } catch (CodigoProdutoNaoExistenteException e) {
                            out.println(e.getMessage());
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

    private void menuAtualizacaoProduto() {
        String op;

        List<Produto> produtos =
                new ArrayList<>(gr.listaProdutos(gr.getUtilizadorActual().getEmail()));
        MenuPaginatedProdutos menupag = new MenuPaginatedProdutos(produtos, scanner);

        if (!produtos.isEmpty()) {

            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strProdutos);
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
                            if (gr.existeProduto(gr.getUtilizadorActual().getEmail(), op)) {
                                Produto produto =
                                        gr.devolveProduto(gr.getUtilizadorActual().getEmail(), op);
                                boolean refresh = menuAtualizaProduto(produto);

                                if (refresh) {
                                    produtos = new ArrayList<>(
                                            gr.listaProdutos(gr.getUtilizadorActual().getEmail()));
                                    if (!produtos.isEmpty()) {
                                        menupag = new MenuPaginatedProdutos(produtos, scanner);

                                    } else {
                                        out.println(strSemProdutos);
                                        scanner.nextLine();
                                        scanner.nextLine();
                                    }
                                }
                            } else {
                                out.println(strOptInvalida);
                                scanner.nextLine();
                                scanner.nextLine();
                            }

                        } catch (CodigoProdutoNaoExistenteException e) {
                            out.println(e.getMessage());
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

    private boolean menuAtualizaProduto(Produto produto) {
        boolean refresh = false;
        int op = -1;
        String nome;
        double precouni = 0;
        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("*********** Atualizar Produto *************");
            out.println("*  [1] Descrição do Produto               *");
            out.println("*  [2] Preço Unitário                     *");
            out.println("*  [3] Guardar                            *");
            out.println("*  [0] Retroceder                         *");
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(strOptInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:

                    out.println("Descrição do Produto");
                    nome = scanner.next();
                    produto.setDescricao(nome);
                    break;
                case 2:
                    out.println("Preço Unitário");
                    opValida = false;
                    while (!opValida) {
                        try {
                            precouni = scanner.nextDouble();
                            opValida = true;

                        } catch (Exception e) {
                            out.println(strOptInvalida);
                            scanner.nextLine();
                        }
                    }
                    produto.setPrecoUni(precouni);
                    break;
                case 3:
                    gr.atualizaProduto(gr.getUtilizadorActual().getEmail(), produto);
                    refresh = true;
                    break;
                case 0:
                    flagContinue = false;
                    break;

                default:
                    out.println(strOptInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
            }

        }
        return refresh;
    }

    private void menuAdicionaProduto() {
        int op = -1;
        String cod;
        String nome;
        double precouni = 0;
        clearConsole();

        out.println("Código do Produto");
        cod = scanner.next();

        out.println("Descrição do Produto");
        nome = scanner.next();

        out.println("Preço Unitário");
        boolean opValida = false;
        while (!opValida) {
            try {
                precouni = scanner.nextDouble();
                opValida = true;

            } catch (Exception e) {
                out.println(strOptInvalida);
                scanner.nextLine();
            }
        }


        boolean flagContinue = true;
        while (flagContinue) {
            out.println("1- Confirma");
            out.println("2- Cancelar");
            out.println("Opção: ");
            opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(strOptInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    try {
                        gr.adicionaProduto(gr.getUtilizadorActual().getEmail(),
                                new Produto(cod, nome, precouni));
                        flagContinue = false;
                    } catch (CodigoProdutoJaExistenteException e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 2:
                    flagContinue = false;
                    break;

                default:
                    out.println(strOptInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
                    break;
            }
        }
    }

    /**
     * Método que cria o menu de listagem de produtos
     */
    private void menuListaProdutos() {
        String op;

        List<Produto> produtos =
                new ArrayList<>(gr.listaProdutos(gr.getUtilizadorActual().getEmail()));
        MenuPaginatedProdutos menupag = new MenuPaginatedProdutos(produtos, scanner);

        if (!produtos.isEmpty()) {
            boolean flagContinue = true;
            while (flagContinue) {
                clearConsole();
                out.println(strProdutos);
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
            out.println(strSemProdutos);
            scanner.nextLine();
            scanner.nextLine();
        }
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
