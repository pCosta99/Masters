package app.views;

import java.util.Scanner;
import app.Parser;
import app.controllers.GestorRegistos;
import app.enums.EstadosTransportadorEnum;
import app.exceptions.UtilizadorJaExistenteException;
import app.interfaces.IInfo;
import app.models.EmpresaTransportadora;
import app.models.Localizacao;
import app.models.Loja;
import app.models.Utilizador;
import app.models.Voluntario;

import static java.lang.System.out;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

public class MenuInicial {
    private GestorRegistos gr;
    private Scanner scanner;

    private static String msgOpccao = "Opção: ";
    private static String msgEmail = "Email: ";
    private static String msgPassword = "Password: ";
    private static String msgNome = "Nome: ";
    private static String msgLatitude = "Latitude: ";
    private static String msgLongitude = "Longitude: ";
    private static String msgConfirmar = "1- Confirmar";
    private static String msgCancelar = "2- Cancelar";
    private static String msgOpcaoInvalida = "Opção Inválida!!";
    private static String msgLinhaEstrela = "********************************************";
    private static String msgLinhaEstrelaGr =
            "******************************************************************";
    private static String msgLinhaBranca = "*                                          *";

    /**
     * Método construtor parametrizado
     */
    public MenuInicial(GestorRegistos g) {
        scanner = new Scanner(System.in);
        this.gr = g;
    }

    /**
     * Método que cria o menu inicial
     */
    public void menuInicial() {
        int op = -1;
        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println("*************** Menu Inicial ***************");
            out.println(msgLinhaBranca);
            out.println("*  [1] Login                               *");
            out.println("*  [2] Registo                             *");
            out.println("*  [3] Menu Admin                          *");
            out.println("*  [0] Fechar                              *");
            out.println(msgLinhaBranca);
            out.println(msgLinhaEstrela);
            out.print(msgOpccao);

            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }

            switch (op) {
                case 1:
                    menuLogin();
                    break;
                case 2:
                    menuRegisto();
                    break;
                case 3:
                    menuAdmin();
                    break;
                case 0:
                    flagContinue = false;
                    break;

                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
            }
        }
    }

    private void menuAdmin() {
        int op = -1;
        boolean flagContinue = true;

        while (flagContinue) {
            clearConsole();
            out.println("*************************** Menu Admin ***************************");
            out.println("*  [1] Listagens dos 10 utilizadores que mais utilizam o sistema *");
            out.println("*  [2] Listagens das 10 empresas que mais utilizam o sistema     *");
            out.println("*  [3] Carregar atraves do ficheiro csv                          *");
            out.println("*  [4] Guardar para ficheiro objectos                            *");
            out.println("*  [5] Carregar de ficheiro  objectos                            *");
            out.println("*  [0] Sair                                                      *");
            out.println(msgLinhaEstrelaGr);
            out.print(msgOpccao);
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }
            out.println();

            switch (op) {
                case 1:
                    clearConsole();
                    out.println(
                            "*************************** Lista ********************************");
                    for (String ut : gr.listagemDezUtilizadoresMaisEnc()) {
                        out.println(ut);
                    }
                    out.println(msgLinhaEstrelaGr);
                    scanner.nextLine();
                    scanner.nextLine();
                    break;
                case 2:
                    clearConsole();
                    out.println(
                            "*************************** Lista ********************************");
                    for (String ut : gr.listagemDezEmpresasMaisKm()) {
                        out.println(ut);
                    }
                    out.println(msgLinhaEstrelaGr);
                    scanner.nextLine();
                    scanner.nextLine();
                    break;
                case 3:
                    carregarFicheirosParse();
                    break;
                case 4:
                    guardarFicheiroObjecto();
                    break;
                case 5:
                    carregarFicheirosObjecto();
                    break;
                case 0:
                    flagContinue = false;
                    break;
                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
            }
        }
    }

    private void carregarFicheirosObjecto() {
        try (FileInputStream f = new FileInputStream(new File("TrazAqui.save"))) {

            ObjectInputStream o = new ObjectInputStream(f);

            this.gr = (GestorRegistos) o.readObject();

            o.close();

        } catch (FileNotFoundException e) {
            out.println("Ficheiro não existe");
            scanner.nextLine();
        } catch (IOException e) {
            out.println("Erro ao iniciar stream");
            scanner.nextLine();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            scanner.nextLine();
        }

    }

    private void guardarFicheiroObjecto() {
        try (FileOutputStream f = new FileOutputStream(new File("TrazAqui.save"))) {

            ObjectOutputStream o = new ObjectOutputStream(f);

            o.writeObject(this.gr);

            o.close();

        } catch (FileNotFoundException e) {
            out.println("Ficheiro não existe");
            scanner.nextLine();
        } catch (IOException e) {
            out.println("Erro ao iniciar stream");
            scanner.nextLine();
        }
    }

    private void carregarFicheirosParse() {
        Parser parser = new Parser(gr);
        parser.parse();
    }

    /**
     * Método que cria o menu login
     */
    private void menuLogin() {
        IInfo utilizadorActual;
        String email;
        String password;
        int op = -1;

        boolean flagContinue = true;
        while (flagContinue) {
            clearConsole();
            out.println(msgEmail);
            email = scanner.next();

            out.println(msgPassword);
            password = scanner.next();

            try {
                gr.login(email, password);

            } catch (Exception e) {
                out.println(e.getMessage());
                out.println("1- Tentar outra vez");
                out.println("0- Menu Inicial");
                out.println(msgOpccao);
                boolean opValida = false;
                while (!opValida) {
                    try {
                        op = scanner.nextInt();
                        opValida = true;

                    } catch (Exception e1) {
                        out.println(msgOpcaoInvalida);
                        scanner.nextLine();
                    }
                }

                if (op == 0) {
                    flagContinue = false;
                }
            }
            utilizadorActual = gr.getUtilizadorActual();

            if (utilizadorActual != null) {
                if (utilizadorActual instanceof Utilizador) {
                    new MenuUtilizador(gr).print();
                    flagContinue = false;
                } else if (utilizadorActual instanceof Loja) {
                    new MenuLoja(gr).print();
                    flagContinue = false;

                } else if (utilizadorActual instanceof EmpresaTransportadora) {
                    new MenuEmpresa(gr).print();
                    flagContinue = false;
                } else if (utilizadorActual instanceof Voluntario) {
                    new MenuVoluntario(gr).print();
                    flagContinue = false;
                }
            }

        }
    }

    /**
     * Método que cria o menu de registo
     */
    private void menuRegisto() {
        int op = -1;
        boolean flagContinue = true;

        while (flagContinue) {
            clearConsole();
            out.println("************ Tipo de utilizador ************");
            out.println(msgLinhaBranca);
            out.println("* [1] Utilizador                           *");
            out.println("* [2] Loja                                 *");
            out.println("* [3] Empresa Transportador                *");
            out.println("* [4] Voluntário                           *");
            out.println("* [0] Menu Inicial                         *");
            out.println(msgLinhaBranca);
            out.println(msgLinhaEstrela);
            out.print(msgOpccao);

            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }

            switch (op) {
                case 1:
                    try {
                        menuRegistaUtilizador();
                        flagContinue = false;

                    } catch (UtilizadorJaExistenteException e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 2:
                    try {
                        menuRegistaLoja();
                        flagContinue = false;

                    } catch (UtilizadorJaExistenteException e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 3:
                    try {
                        menuRegistaEmpresaTransportadora();
                        flagContinue = false;

                    } catch (UtilizadorJaExistenteException e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    break;
                case 4:
                    try {
                        menuRegistaVoluntario();
                        flagContinue = false;

                    } catch (UtilizadorJaExistenteException e) {
                        out.println(e.getMessage());
                        scanner.nextLine();
                        scanner.nextLine();
                    }
                    scanner.nextLine();
                    break;
                case 0:
                    flagContinue = false;
                    break;
                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
            }
        }
    }

    /**
     * Método que cria o menu de registo Utilizador
     */
    private void menuRegistaUtilizador() throws UtilizadorJaExistenteException {
        int op = -1;
        String email;
        String password;
        String nome;
        double latitude = -1;
        double longitude = -1;
        clearConsole();
        out.println(msgEmail);
        email = scanner.next();

        out.println(msgPassword);
        password = scanner.next();

        out.println(msgNome);
        nome = scanner.next();

        out.println(msgLatitude);
        boolean opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                latitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println(msgLongitude);
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                longitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        boolean flagContinue = true;
        while (flagContinue) {
            out.println(msgConfirmar);
            out.println(msgCancelar);
            out.println(msgOpccao);
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    gr.registaNovoTipoUtilizador(new Utilizador(email, password, nome,
                            new Localizacao(latitude, longitude)));
                    flagContinue = false;
                    break;
                case 2:
                    flagContinue = false;
                    break;

                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    break;
            }
        }
    }

    /**
     * Método que cria o menu de registo Loja
     */
    private void menuRegistaLoja() throws UtilizadorJaExistenteException {
        int op = -1;
        String email;
        String password;
        String nome;
        double latitude = -1;
        double longitude = -1;
        boolean filaEspera = false;
        clearConsole();
        out.println(msgEmail);
        email = scanner.next();

        out.println(msgPassword);
        password = scanner.next();

        out.println(msgNome);
        nome = scanner.next();

        out.println(msgLatitude);
        boolean opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                latitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println(msgLongitude);
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                longitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println("Tem informação de fila de espera: ");
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                filaEspera = scanner.nextBoolean();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }


        boolean flagContinue = true;
        while (flagContinue) {
            out.println(msgConfirmar);
            out.println(msgCancelar);
            out.println(msgOpccao);
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    gr.registaNovoTipoUtilizador(new Loja(email, password, nome,
                            new Localizacao(latitude, longitude), filaEspera ? 0 : -1));
                    flagContinue = false;
                    break;
                case 2:
                    flagContinue = false;
                    break;

                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    scanner.nextLine();
                    break;
            }
        }
    }

    /**
     * Método que cria o menu de registo Empresa Transportadora
     */
    private void menuRegistaEmpresaTransportadora() throws UtilizadorJaExistenteException {
        int op = -1;
        String email;
        String password;
        String nome;
        double latitude = -1;
        double longitude = -1;
        EstadosTransportadorEnum estado = EstadosTransportadorEnum.OCUPADO;
        boolean aceitaMed;
        double precoPorKM = 0;
        double raioAccao = 0;
        String nif;
        double velocidadeMedia = 0;
        clearConsole();
        out.println(msgEmail);
        email = scanner.next();

        out.println(msgPassword);
        password = scanner.next();

        out.println(msgNome);
        nome = scanner.next();

        out.println(msgLatitude);
        boolean opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                latitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println(msgLongitude);
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                longitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        boolean flagContinue = true;
        while (flagContinue) {
            out.println("Estado:");
            out.println("1- Ocupado");
            out.println("2- Livre");
            out.println(msgOpccao);
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    estado = EstadosTransportadorEnum.OCUPADO;
                    flagContinue = false;
                    break;
                case 2:
                    estado = EstadosTransportadorEnum.LIVRE;
                    flagContinue = false;
                    break;

                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    break;
            }
        }

        out.println("Aceita transporte de encomendas médicas: ");

        aceitaMed = scanner.nextBoolean();

        out.println("Preço pr Km: ");
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                precoPorKM = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println("Raio de Acção: ");
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                raioAccao = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println("NIF: ");
        nif = scanner.next();

        out.println("Velocidade Média: ");
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                velocidadeMedia = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        flagContinue = true;

        while (flagContinue) {
            out.println(msgConfirmar);
            out.println(msgCancelar);
            out.println(msgOpccao);
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    gr.registaNovoTipoUtilizador(new EmpresaTransportadora(email, password, nome,
                            new Localizacao(latitude, longitude), estado, aceitaMed, precoPorKM,
                            raioAccao, nif, velocidadeMedia));
                    flagContinue = false;
                    break;
                case 2:
                    flagContinue = false;
                    break;

                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    break;
            }
        }
    }

    /**
     * Método que cria o menu de registo Voluntário
     */
    private void menuRegistaVoluntario() throws UtilizadorJaExistenteException {
        int op = -1;
        String email;
        String password;
        String nome;
        double latitude = -1;
        double longitude = -1;
        EstadosTransportadorEnum estado = EstadosTransportadorEnum.OCUPADO;
        boolean aceitaMed;
        double raioAccao = 0;
        double velocidadeMedia = 0;
        clearConsole();

        out.println(msgEmail);
        email = scanner.next();

        out.println(msgPassword);
        password = scanner.next();

        out.println(msgNome);
        nome = scanner.next();

        out.println(msgLatitude);
        boolean opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                latitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println(msgLongitude);
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                longitude = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        boolean flagContinue = true;
        while (flagContinue) {
            out.println("Estado:");
            out.println("1- Ocupado");
            out.println("2- Livre");
            out.println(msgOpccao);
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    estado = EstadosTransportadorEnum.OCUPADO;
                    flagContinue = false;
                    break;
                case 2:
                    estado = EstadosTransportadorEnum.LIVRE;
                    flagContinue = false;
                    break;

                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    break;
            }
        }

        out.println("Aceita transporte de encomendas médicas: ");

        aceitaMed = scanner.nextBoolean();

        out.println("Raio de Acção: ");
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                raioAccao = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        out.println("Velocidade Média: ");
        opDoubleValida = false;
        while (!opDoubleValida) {
            try {
                velocidadeMedia = scanner.nextDouble();
                opDoubleValida = true;

            } catch (Exception e) {
                out.println(msgOpcaoInvalida);
                scanner.nextLine();
            }
        }

        flagContinue = true;

        while (flagContinue) {
            out.println(msgConfirmar);
            out.println(msgCancelar);
            out.println(msgOpccao);
            boolean opValida = false;
            while (!opValida) {
                try {
                    op = scanner.nextInt();
                    opValida = true;

                } catch (Exception e) {
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                }
            }


            switch (op) {
                case 1:
                    gr.registaNovoTipoUtilizador(new Voluntario(email, password, nome,
                            new Localizacao(latitude, longitude), estado, aceitaMed, raioAccao,
                            velocidadeMedia));
                    flagContinue = false;
                    break;
                case 2:
                    flagContinue = false;
                    break;

                default:
                    out.println(msgOpcaoInvalida);
                    scanner.nextLine();
                    break;
            }
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
