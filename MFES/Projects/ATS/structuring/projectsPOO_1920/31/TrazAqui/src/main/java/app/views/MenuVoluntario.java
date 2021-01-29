package app.views;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import app.controllers.GestorRegistos;
import app.enums.EstadosEncomendaEnum;
import app.enums.EstadosTransportadorEnum;
import app.exceptions.UtilizadorNaoExistenteException;
import app.models.Voluntario;
import app.models.RegistoEncomenda;
import app.models.Transportador;

import static java.lang.System.out;

public class MenuVoluntario {

    private GestorRegistos gr;
    private Scanner scanner;

    private static String prtLin = "*                                             *";
    private static String strOpt = "Opção: ";
    private static String strOptInvalida = "Opção Inválida!!\n";
    private static String strLinEnc = "*************** Encomendas ***************";
    private static String strSemEnc = "Sem encomendas disponiveis";
    private static String strEncomendas = "*************** Encomendas ***************";

    /**
     * Método construtor parametrizado
     */
    public MenuVoluntario(GestorRegistos g) {
        scanner = new Scanner(System.in);
        this.gr = g;
    }

    /**
     * Método que cria o menu voluntario
     */
    public void print() {
        boolean flagContinue = true;
        while (flagContinue) {
            if (gr.getUtilizadorActual() instanceof Voluntario) {
                Transportador transportador = (Transportador) gr.getUtilizadorActual();
                if (transportador.getEstado() == EstadosTransportadorEnum.LIVRE) {
                    menuEstadoLivre();
                } else {
                    menuEstadoOcupado();
                }
            } else {
                flagContinue = false;
            }
        }
    }

    /**
     * Método que cria o menu em que o estado do transportador está livre
     */
    private void menuEstadoLivre() {
        int op = -1;

        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("*************** Menu Voluntario ***************");
            out.println(prtLin);
            out.println("*  [1] Sinalizar disponibilidade entrega      *");
            out.println("*  [2] Lista de encomendas disponíveis        *");
            out.println("*  [3] Lista de encomendas para entregar      *");
            out.println("*  [4] Lista de encomendas por estado         *");
            out.println("*  [0] Sair                                   *");
            out.println(prtLin);
            out.println("***********************************************\n");
            out.println(strOpt);

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
                    menuSinalizaDisponibilidadeEntrega();
                    flagContinue = false;
                    break;
                case 2:
                    try {
                        menuDisponibilidadeParaEntregarEncomenda();

                    } catch (Exception e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 3:
                    try {
                        // Se ocupado sai deste modo de menu
                        flagContinue = !menuListaVaiEntregarEncomenda();

                    } catch (Exception e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 4:
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

    /**
     * Método que cria o menu em que o estado do transportador está livre
     */
    private void menuEstadoOcupado() {
        int op = -1;

        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("*************** Menu Voluntario ***************");
            out.println(prtLin);
            out.println("*  [1] Sinalizar disponibilidade entrega      *");
            out.println("*  [2] Lista de encomendas entregues          *");
            out.println("*  [3] Lista de encomendas por estado         *");
            out.println("*  [0] Sair                                   *");
            out.println(prtLin);
            out.println("***********************************************\n");
            out.println(strOpt);

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
                    menuSinalizaDisponibilidadeEntrega();
                    flagContinue = false;
                    break;
                case 2:
                    try {
                        flagContinue = menuEncomendaEntregue();

                    } catch (Exception e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 3:
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

    /**
     * Método que cria o menu que sinaliza estado transportador
     */
    private void menuSinalizaDisponibilidadeEntrega() {
        int op = -1;

        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("*************** Estados ***************");
            out.println("*  [1] OCUPADO                        *");
            out.println("*  [2] LIVRE                          *");
            out.println("*  [0] Sair                           *");
            out.println("*                                     *");
            out.println("*************************************\n");
            out.println(strOpt);

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
                    try {
                        gr.alteraEstadoTransp(EstadosTransportadorEnum.OCUPADO);
                        flagContinue = false;

                    } catch (Exception e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 2:
                    try {
                        gr.alteraEstadoTransp(EstadosTransportadorEnum.LIVRE);
                        flagContinue = false;

                    } catch (Exception e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
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

    /**
     * Método que cria o menu que sinaliza encomenda
     * 
     * @throws UtilizadorNaoExistenteException
     */
    private boolean menuListaVaiEntregarEncomenda() throws UtilizadorNaoExistenteException {
        String op;

        List<RegistoEncomenda> encomendas = new ArrayList<>(
                gr.listagemPorUtilizadorPorDataDentroRaioAccao(EstadosEncomendaEnum.APROVADA));
        MenuPaginatedEncomendas menupag = new MenuPaginatedEncomendas(encomendas, scanner);

        boolean ocupado = false;
        boolean flagContinue = true;
        boolean refresh = false;
        while (flagContinue) {
            clearConsole();
            out.println(strLinEnc);
            if (refresh) {
                encomendas = new ArrayList<>(gr.listagemPorUtilizadorPorDataDentroRaioAccao(
                        EstadosEncomendaEnum.APROVADA));
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
                                ocupado = gr.transportaEncomenda(codEnc);
                                flagContinue = !ocupado;
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
                scanner.nextLine();
                scanner.nextLine();
            }
        }
        return ocupado;
    }

    /**
     * Método que cria o menu que sinaliza encomenda
     * 
     * @throws UtilizadorNaoExistenteException
     */
    private void menuDisponibilidadeParaEntregarEncomenda() throws UtilizadorNaoExistenteException {
        String op;

        List<RegistoEncomenda> encomendas = new ArrayList<>(
                gr.listagemPorUtilizadorPorDataDentroRaioAccao(EstadosEncomendaEnum.AGUARDAENVIO));
        MenuPaginatedEncomendas menupag = new MenuPaginatedEncomendas(encomendas, scanner);

        boolean flagContinue = true;
        boolean refresh = false;
        while (flagContinue) {
            clearConsole();
            out.println(strLinEnc);
            if (refresh) {
                encomendas = new ArrayList<>(gr.listagemPorUtilizadorPorDataDentroRaioAccao(
                        EstadosEncomendaEnum.AGUARDAENVIO));
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

                                gr.indicaEncomendaParaTransporte(codEnc);
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
                scanner.nextLine();
                scanner.nextLine();
                flagContinue = false;
            }
        }
    }

    /**
     * Método que cria o menu que sinaliza encomenda
     * 
     * @throws UtilizadorNaoExistenteException
     */
    private boolean menuEncomendaEntregue() throws UtilizadorNaoExistenteException {
        String op;

        List<RegistoEncomenda> encomendas = new ArrayList<>(
                gr.listagemPorUtilizadorPorDataDentroRaioAccao(EstadosEncomendaEnum.ENVIADA));
        MenuPaginatedEncomendas menupag = new MenuPaginatedEncomendas(encomendas, scanner);

        boolean ocupado = true;
        boolean flagContinue = true;
        boolean refresh = false;
        while (flagContinue) {
            clearConsole();
            out.println(strLinEnc);
            if (refresh) {
                encomendas = new ArrayList<>(gr
                        .listagemPorUtilizadorPorDataDentroRaioAccao(EstadosEncomendaEnum.ENVIADA));
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
                                ocupado = gr.encomendaEntregue(codEnc);
                                flagContinue = ocupado;
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
                scanner.nextLine();
                scanner.nextLine();
                flagContinue = false;
            }
        }
        return ocupado;
    }

    private void menuListagemEstado() {
        String op;

        List<RegistoEncomenda> encomendas = new ArrayList<>(gr.listagemPorUtilizador());
        MenuPaginatedEncomendas menupag = new MenuPaginatedEncomendas(encomendas, scanner, true);

        if (!encomendas.isEmpty()) {
            boolean flagContinue = true;
            while (flagContinue) {
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
