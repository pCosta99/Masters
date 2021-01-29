import java.io.*;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Implementação do controlador principal do programa
 */
public class Controlador implements IControlador {
    private final IVista v;
    private IModelo s;


    /**
     * Construtor
     */
    public Controlador(IVista v, IModelo s) {
        this.v = v;
        this.s = s;
    }

    /**
     * Carrega de um ficheiro um estado
     */
    private IModelo readFile(String filename) throws IOException, ClassNotFoundException {
        ObjectInputStream o = new ObjectInputStream(new FileInputStream(filename));
        IModelo m = (SistemaTrazAqui) o.readObject();
        o.close();
        return m;
    }

    /**
     * Guarda o estado atual em ficheiro
     */
    private void writeInFile(IModelo s) throws IOException {
        ObjectOutputStream o = new ObjectOutputStream(new FileOutputStream("./sistemaTrazAqui.dat"));
        o.writeObject(s);
        o.flush();
        o.close();
    }

    /**
     * Função principal que arranca o controlador
     */
    public void run() {
        Input i = new Input();
        v.clear();
        s.loadFromLogs();
        v.showMessage("Logs carregados!\n");

        int input = -1;

        while (input != 0) {
            v.menuInicial();
            v.menuOpcoes();
            input = i.lerInt();
            switch (input) {
                case 1:
                    novaConta();
                    break;
                case 2:
                    login();
                    break;
                case 3:
                    input = -1;
                    v.top10();
                    v.showMessage("10 Utilizadores mais ativos (min -> 1 Encomenda Finalizada):\n");
                    v.showMessage(s.utilizadoresMaisAtivos());
                    while (input != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        input = i.lerInt();
                    }
                    input = -1;
                    break;
                case 4:
                    v.top10();
                    input = -1;
                    v.showMessage("10 Empresas transportadoras mais ativas (min -> 1 Km percorrido):\n");
                    v.showMessage(s.empresasTransportadorasMaisAtivas());
                    while (input != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        input = i.lerInt();
                    }
                    input = -1;
                    break;
                case 5:
                    input = -1;
                    try {
                        writeInFile(s);
                        v.showMessage("\nEstado da aplicação gravado.");
                        while (input != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            input = i.lerInt();
                        }
                    } catch (IOException e) {
                        v.showMessage("\nFalha ao gravar.");
                        while (input != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            input = i.lerInt();
                        }
                        e.printStackTrace();
                    }
                    input = -1;
                    break;
                case 6:
                    s = carregarSistemaDoFicheiro();
                    v.showMessage("Ficheiro lido!\n");
                    while (input != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        input = i.lerInt();
                    }
                    input = -1;
                    break;
            }
            v.clear();
        }
    }


    /**
     * Função responsável por criar uma conta
     */
    private void novaConta() {
        Input i = new Input();
        boolean criada = false;
        int qualConta = -1;
        while (!criada) {
            v.clear();
            v.criarConta();
            v.tipoDeConta();
            qualConta = i.lerInt();
            switch (qualConta) {
                case 1:
                    novoUtilizador();
                    criada = true;
                    break;
                case 2:
                    novoVoluntario();
                    criada = true;
                    break;
                case 3:
                    novaLoja();
                    criada = true;
                    break;
                case 4:
                    novaTransportadora();
                    criada = true;
                    break;
                case 0:
                    return;
                default:
                    v.showMessage("Opção inválida\n");
            }
        }
    }

    /**
     * Função responsável por criar um novo Utilizador
     */
    private void novoUtilizador() {
        boolean confirmado = false;
        int opcao = -1;
        Input i = new Input();
        double x;
        double y;
        while (!confirmado) {
            String nome;
            String codigo;
            String email;
            String password;
            opcao = -1;
            v.criarConta();
            v.showMessage("\n1. Nome completo: ");
            nome = i.lerString();
            v.criarConta();
            v.showMessage("\n2. Escolha um nickname [ u(número), exemplo -> u89 ]: ");
            codigo = i.lerString();
            while (s.existeConta(codigo) || codigo.charAt(0) != 'u') {
                v.criarConta();
                v.showMessage("\nCódigo inválido ou já existe, escolha outro: ");
                codigo = i.lerString();
            }
            v.criarConta();
            v.showMessage("\n3. Email: ");
            email = i.lerString();
            v.criarConta();
            v.showMessage("\n4. Password: ");
            password = i.lerString();
            v.criarConta();
            v.showMessage("\n5. Localização (x): ");
            x = i.lerDouble();
            v.showMessage("   Localização (y): ");
            y = i.lerDouble();
            v.criarConta();
            v.showMessage("\nNome: ");
            v.showMessage(nome);
            v.showMessage("\n\nNickname: ");
            v.showMessage(codigo);
            v.showMessage("\n\nEmail: ");
            v.showMessage(email);
            v.showMessage("\n\nPassword: ");
            v.showMessage(password);
            v.showMessage("\n\nLocalização: (");
            v.showMessage(x);
            v.showMessage(", ");
            v.showMessage(y);
            v.showMessage(")");
            while (opcao != 1 && opcao != 2) {
                v.showMessage("\n\nQuer criar a conta com estes dados? (1) Sim (2) Criar outra > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) {
                confirmado = true;
                GPS gps = new GPS(x, y);
                s.novoUtilizador(codigo, nome, gps, email, password);
            }
        }
        v.showMessage("\nConta criada, clique em (0) para voltar > ");
        while (opcao != 0) {
            opcao = i.lerInt();
        }
    }


    /**
     * Função responsável por criar um novo Voluntário
     */
    private void novoVoluntario() {
        boolean confirmado = false;
        int opcao = -1;
        Input i = new Input();
        double x;
        double y;
        double raio;
        double velocidade;
        while (!confirmado) {
            String nome;
            String codigo;
            String email;
            String password;
            boolean certificado = false;
            v.criarConta();
            v.showMessage("\n1. Nome completo: ");
            nome = i.lerString();
            v.criarConta();
            v.showMessage("\n2. Escolha um nickname [ v(número), exemplo -> v56 ]: ");
            codigo = i.lerString();
            while (s.existeConta(codigo) || codigo.charAt(0) != 'v') {
                v.criarConta();
                v.showMessage("\nCódigo inválido ou já existe, escolha outro: ");
                codigo = i.lerString();
            }
            v.criarConta();
            v.showMessage("\n3. Email: ");
            email = i.lerString();
            v.criarConta();
            v.showMessage("\n4. Password: ");
            password = i.lerString();
            v.criarConta();
            v.showMessage("\n5. É certificado para o transporte de medicamentos? (1) Sim (2) Não > ");
            opcao = i.lerInt();
            while (opcao != 1 && opcao != 2) {
                v.showMessage("\nOpção inválida, introduza outra > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) certificado = true;
            v.criarConta();
            v.showMessage("\n6. Localização (x): ");
            x = i.lerDouble();
            v.showMessage("   Localização (y): ");
            y = i.lerDouble();
            v.criarConta();
            v.showMessage("\n7. O seu raio de ação (km): ");
            raio = i.lerDouble();
            v.criarConta();
            v.showMessage("\n8. A sua velocidade de deslocação (km/h): ");
            velocidade = i.lerDouble();
            v.criarConta();
            v.showMessage("\nNome: ");
            v.showMessage(nome);
            v.showMessage("\n\nNickname: ");
            v.showMessage(codigo);
            v.showMessage("\n\nEmail: ");
            v.showMessage(email);
            v.showMessage("\n\nPassword: ");
            v.showMessage(password);
            v.showMessage("\n\nCertificado: ");
            if (certificado) v.showMessage("Sim. ");
            else v.showMessage("Não. ");
            v.showMessage("\n\nLocalização: (");
            v.showMessage(x);
            v.showMessage(", ");
            v.showMessage(y);
            v.showMessage(")");
            v.showMessage("\n\nRaio de ação (km): ");
            v.showMessage(raio);
            v.showMessage("\n\nVelocidade de deslocação (km/h): ");
            v.showMessage(velocidade);
            opcao = -1;
            while (opcao != 1 && opcao != 2) {
                v.showMessage("\n\nQuer criar a conta com estes dados? (1) Sim (2) Criar outra > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) {
                confirmado = true;
                GPS gps = new GPS(x, y);
                s.novoVoluntario(codigo, nome, gps, email, password, raio, certificado, velocidade);
            }
        }
        v.showMessage("\nConta criada, clique em (0) para voltar > ");
        while (opcao != 0) {
            opcao = i.lerInt();
        }
    }


    /**
     * Função responsável por criar uma nova transportadora
     */
    private void novaTransportadora() {
        boolean confirmado = false;
        boolean variasEncomendas = false;
        int opcao = -1;
        Input i = new Input();
        while (!confirmado) {
            double x;
            double y;
            double preco;
            double precoPeso;
            double raio;
            double velocidade;
            String nome;
            String codigo;
            String email;
            String password;
            boolean certificado = false;
            String nif;
            v.criarConta();
            v.showMessage("\n1. Nome da Empresa: ");
            nome = i.lerString();
            v.criarConta();
            v.showMessage("\n2. Escolha um código de acesso [ t(número), exemplo -> t111 ]: ");
            codigo = i.lerString();
            while (s.existeConta(codigo) || codigo.charAt(0) != 't') {
                v.criarConta();
                v.showMessage("\nCódigo inválido ou já existe, escolha outro: ");
                codigo = i.lerString();
            }
            v.criarConta();
            v.showMessage("\n3. Email: ");
            email = i.lerString();
            v.criarConta();
            v.showMessage("\n4. Password: ");
            password = i.lerString();
            v.criarConta();
            v.showMessage("\n5. É certificado para o transporte de medicamentos? (1) Sim (2) Não > ");
            opcao = i.lerInt();
            while (opcao != 1 && opcao != 2) {
                v.showMessage("\nOpção inválida, introduza outra > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) certificado = true;
            v.criarConta();
            v.showMessage("\n6. Localização (x): ");
            x = i.lerDouble();
            v.showMessage("   Localização (y): ");
            y = i.lerDouble();
            v.criarConta();
            v.showMessage("\n7. NIF da Empresa: ");
            nif = i.lerString();
            v.criarConta();
            v.showMessage("\n8. Taxa de transporte (por km): ");
            preco = i.lerDouble();
            v.criarConta();
            v.showMessage("\n8. Taxa de transporte (por kg): ");
            precoPeso = i.lerDouble();
            v.criarConta();
            v.showMessage("\n10. Pode transportar mais do que uma encomenda de cada vez? (1) Sim (2) Não ");
            opcao = i.lerInt();
            while (opcao != 1 && opcao != 2) {
                v.showMessage("Opção inválida > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) variasEncomendas = true;
            opcao = -1;
            v.criarConta();
            v.showMessage("\n\n11. Raio limite para transportações (km): ");
            raio = i.lerDouble();
            v.criarConta();
            v.showMessage("\n12. A sua velocidade de deslocação (km/h): ");
            velocidade = i.lerDouble();
            v.criarConta();
            v.showMessage("\nEmpresa: ");
            v.showMessage(nome);
            v.showMessage("\n\nCódigo de acesso: ");
            v.showMessage(codigo);
            v.showMessage("\n\nEmail: ");
            v.showMessage(email);
            v.showMessage("\n\nPassword: ");
            v.showMessage(password);
            v.showMessage("\n\nCertificado: ");
            if (certificado) v.showMessage("Sim. ");
            else v.showMessage("Não. ");
            v.showMessage("\n\nLocalização: (");
            v.showMessage(x);
            v.showMessage(", ");
            v.showMessage(y);
            v.showMessage(")");
            v.showMessage("\n\nNIF: ");
            v.showMessage(nif);
            v.showMessage("\n\nTaxa de transporte (por km): ");
            v.showMessage(preco);
            v.showMessage("\n\nTaxa de transporte (por kg): ");
            v.showMessage(precoPeso);
            v.showMessage("\n\nRaio de transporte: ");
            v.showMessage(raio);
            v.showMessage("\n\nVelocidade de deslocação (km/h): ");
            v.showMessage(velocidade);
            v.showMessage("\n\nTransporte de várias encomendas: ");
            if (variasEncomendas) v.showMessage("Sim.");
            else v.showMessage("Não.");
            while (opcao != 1 && opcao != 2) {
                v.showMessage("\n\nQuer criar a conta com estes dados? (1) Sim (2) Criar outra > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) {
                confirmado = true;
                GPS gps = new GPS(x, y);
                s.novaTransportadora(codigo, nome, gps, email, password, nif, raio, preco, precoPeso, variasEncomendas, certificado, velocidade);
            }
        }
        v.showMessage("\nConta criada, clique em (0) para voltar > ");
        while (opcao != 0) {
            opcao = i.lerInt();
        }
    }


    /**
     * Função responsável por criar uma nova loja
     */
    private void novaLoja() {
        boolean confirmado = false;
        int opcao = -1;
        Input i = new Input();
        while (!confirmado) {
            boolean infoFilas = false;
            String nome, codigo, email, password;
            double x, y;
            v.criarConta();
            v.showMessage("\n1. Nome da Loja: ");
            nome = i.lerString();
            v.criarConta();
            v.showMessage("\n2. Escolha um código de acesso [ l(número), exemplo -> l120 ]: ");
            codigo = i.lerString();
            while (s.existeConta(codigo) || codigo.charAt(0) != 'l') {
                v.criarConta();
                v.showMessage("\nCódigo inválido ou já existe, escolha outro: ");
                codigo = i.lerString();
            }
            v.criarConta();
            v.showMessage("\n3. Email: ");
            email = i.lerString();
            v.criarConta();
            v.showMessage("\n4. Password: ");
            password = i.lerString();
            v.criarConta();
            v.showMessage("\n5. Localização (x): ");
            x = i.lerDouble();
            v.showMessage("   Localização (y): ");
            y = i.lerDouble();
            v.criarConta();
            v.showMessage("\n6. Pretende dar informação sobre filas de espera na loja? (1) Sim (2) Não > ");
            opcao = i.lerInt();
            while (opcao != 1 && opcao != 2) {
                v.showMessage("\nOpção inválida, introduza outra > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) infoFilas = true;
            opcao = -1;

            v.criarConta();
            v.showMessage("\nLoja: ");
            v.showMessage(nome);
            v.showMessage("\n\nCódigo de acesso: ");
            v.showMessage(codigo);
            v.showMessage("\n\nEmail: ");
            v.showMessage(email);
            v.showMessage("\n\nPassword: ");
            v.showMessage(password);
            v.showMessage("\n\nLocalização: (");
            v.showMessage(x);
            v.showMessage(", ");
            v.showMessage(y);
            v.showMessage(")");
            v.showMessage("\n\nInformação de filas de espera: ");
            if (infoFilas) v.showMessage("Sim.");
            else v.showMessage("Não.");

            while (opcao != 1 && opcao != 2) {
                v.showMessage("\n\nQuer criar a conta com estes dados? (1) Sim (2) Criar outra > ");
                opcao = i.lerInt();
            }
            if (opcao == 1) {
                confirmado = true;
                GPS gps = new GPS(x, y);
                s.novaLoja(codigo, nome, gps, email, password, infoFilas);
                s.adicionaCatalogoALoja(codigo);
            }
        }
        v.showMessage("\nConta criada, clique em (0) para voltar > ");
        while (opcao != 0) {
            opcao = i.lerInt();
        }
    }


    /**
     * Função que efetua o login das diversas entidades
     */
    private void login() {
        Input i = new Input();
        int opcao = -1;
        while (opcao != 2 && opcao != 0) {
            v.login();
            opcao = -1;
            v.showMessage("\nCódigo de acesso: ");
            String codigo = i.lerString();
            if (!s.existeConta(codigo)) {
                while (opcao != 1 && opcao != 2) {
                    v.login();
                    v.showMessage("\nCódigo inválido\n");
                    v.showMessage("\n(1) Tentar novamente");
                    v.showMessage("\n(2) Voltar para o menu\n > ");
                    opcao = i.lerInt();
                }
            }
            if (opcao != 2 && opcao != 1) {
                v.showMessage("\nPassword: ");
                String password = i.lerString();
                if (!s.passCorreta(codigo, password)) {
                    while (opcao != 1 && opcao != 2) {
                        v.login();
                        v.showMessage("\nPassword incorreta\n");
                        v.showMessage("\n(1) Tentar novamente");
                        v.showMessage("\n(2) Voltar para o menu\n > ");
                        opcao = i.lerInt();
                    }
                } else {
                    v.login();
                    opcao = -1;
                    while (opcao != 0) {
                        v.showMessage("Login efetuado!\nPressione (0) para aceder às funcionalidades ");
                        opcao = i.lerInt();
                    }
                    if (codigo.charAt(0) == 'u') funcUser(codigo);
                    else if (codigo.charAt(0) == 'v') funcVoluntario(codigo);
                    else if (codigo.charAt(0) == 't') funcTransportadora(codigo);
                    else funcL(codigo);
                }
            }
        }
    }


    /**
     * Função que permite a cada utilizador ter acesso ás várias funcionalidades
     * @param codigo codigo do utilizador
     */
    private void funcUser(String codigo) {
        Input i = new Input();
        int opcao = -1;
        while (opcao != 0) {
            v.funcionalidadesUtilizador();
            opcao = i.lerInt();
            LocalDate inicio, fim;
            String cod, dataInicio = "", dataFim ="";
            Collection<LinhaEncomenda> carrinho = new ArrayList<>();
            boolean eEncomendaMedica;
            switch (opcao) {
                case 1:
                    cod = "";
                    v.utilizadorOpcao1SelecionarLoja();
                    v.showMessage(s.lojasDisponiveis());
                    while (!s.existeLoja(cod)) {
                        v.showMessage("\nIntroduza o código da loja > ");
                        cod = i.lerString();
                    }
                    opcao = -1;
                    v.utilizadorOpcao1EncomendaMedica();
                    while (opcao != 1 && opcao != 2) {
                        v.showMessage("\n(1) Sim (2) Não > ");
                        opcao = i.lerInt();
                    }
                    eEncomendaMedica = opcao == 1;

                    escolherProduto(cod, carrinho);

                    v.utilizadorOpcao1ConfirmarEnc();

                    v.showMessage(s.estadoEncomenda(carrinho));
                    opcao = -1;
                    while (opcao != 1 && opcao != 2) {
                        v.showMessage("\n(1) Sim (2) Não > ");
                        opcao = i.lerInt();
                    }
                    if (opcao == 1) {
                        double peso = s.calculaPesoCarrinho(carrinho);
                        String codEnc = s.gerarCodigoEnc();
                        Encomenda nova = new Encomenda(codEnc, codigo, cod, peso, eEncomendaMedica, "", carrinho);
                        s.novaEncomenda(nova);
                    } else carrinho.clear();
                    break;
                case 2:
                    v.user();
                    cod = "";
                    v.showMessage(s.verEncomendasPorAceitar(codigo));
                    opcao = -1;

                    if (!s.existeEncomendaPorAceitarCodUser(codigo)) {
                        while (opcao != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                        }
                        opcao = -1;
                        break;
                    }

                    while (opcao != 1 && opcao != 2) {
                        v.showMessage("\nPressione (1) Aceitar (2) Rejeitar > ");
                        opcao = i.lerInt();
                    }
                    if (opcao == 1) {
                        while (opcao != 0 && !s.existeEncomendaPorAceitarCodEnc(cod)) {
                            v.showMessage("\nIntroduza o código da encomenda a aceitar ou pressione (0) voltar > ");
                            cod = i.lerString();
                            if (s.tryParseInt(cod)) {
                                if (Integer.parseInt(cod) == 0)
                                    opcao = 0;
                            }
                        }
                        if (opcao != 0) {
                            v.user();
                            s.sinalizaEncomendaAceite(cod);
                            v.showMessage("\nServiço de transporte da encomenda " + cod + " aceite.");
                            while (opcao != 0) {
                                v.showMessage("\nPressione (0) voltar > ");
                                opcao = i.lerInt();
                            }
                        }
                    } else {
                        while (opcao != 0 && !s.existeEncomendaPorAceitarCodEnc(cod)) {
                            v.showMessage("\nIntroduza o código da encomenda a rejeitar ou pressione (0) voltar > ");
                            cod = i.lerString();
                            if (s.tryParseInt(cod)) {
                                if (Integer.parseInt(cod) == 0)
                                    opcao = 0;
                            }
                        }
                        if (opcao != 0) {
                            v.user();
                            s.sinalizaEncomendaRejeitada(cod);
                            v.showMessage("\nServiço de trasnporte da encomenda " + cod + " rejeitado.");
                            while (opcao != 0) {
                                v.showMessage("\nPressione (0) voltar > ");
                                opcao = i.lerInt();
                            }
                        }
                    }
                    opcao = -1;
                    break;
                case 3:
                    v.user();
                    v.showMessage(s.historicoEncUtilizador(codigo));
                    opcao = -1;
                    if(s.historicoEncUtilizador(codigo).equals("\nNão existe histórico de encomendas!\n")){
                        while (opcao != 0 ) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                        }
                        opcao = -1;
                        break;
                    }
                    while (opcao != 0 && opcao != 1 && opcao != 2 ) {
                        v.showMessage("\nPressione (1) filtrar por data");
                        v.showMessage("\nPressione (2) filtrar por data e voluntário ou empresa transportadora ");
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                    }
                    switch (opcao){
                        case 1:
                            opcao = -1;
                            v.user();
                            v.showMessage("Escolha a data inicial (p.ex 10/3/2020): ");
                            while (dataInicio.length() < 8 || dataInicio.length() > 10) {
                                v.showMessage("\nOpção > ");
                                dataInicio = i.lerString();
                            }
                            v.showMessage("\nEscolha a data final (p.ex 28/2/2020): ");
                            while (dataFim.length() < 8 || dataFim.length() > 10) {
                                v.showMessage("\nOpção > ");
                                dataFim = i.lerString();
                            }
                            inicio = s.dateInput(dataInicio);
                            fim = s.dateInput(dataFim);
                            v.user();
                            v.showMessage(s.historicoEncUtilizadorFiltrado(inicio, fim, codigo, ""));
                            while (opcao != 0) {
                                v.showMessage("\nPressione (0) voltar > ");
                                opcao = i.lerInt();
                            }
                            break;
                        case 2:
                            opcao = -1;
                            v.user();
                            String st = "";
                            v.showMessage("\nInsira código de voluntário ou empresa transportadora > ");
                            while (!s.existeCodMeioTransporte(st)) {
                                v.showMessage("\nOpção > ");
                                st = i.lerString();
                            }
                            v.showMessage("Escolha a data inicial (p.ex 10/3/2020): ");
                            while (dataInicio.length() < 8 || dataInicio.length() > 10) {
                                v.showMessage("\nOpção > ");
                                dataInicio = i.lerString();
                            }
                            v.showMessage("\nEscolha a data final (p.ex 28/2/2020): ");
                            while (dataFim.length() < 8 || dataFim.length() > 10) {
                                v.showMessage("\nOpção > ");
                                dataFim = i.lerString();
                            }
                            inicio = s.dateInput(dataInicio);
                            fim = s.dateInput(dataFim);
                            v.user();
                            v.showMessage(s.historicoEncUtilizadorFiltrado(inicio, fim, codigo, st));
                            while (opcao != 0) {
                                v.showMessage("\nPressione (0) voltar > ");
                                opcao = i.lerInt();
                            }
                            break;
                    }
                    opcao = -1;
                    break;
                case 4:
                    v.user();
                    cod = "";
                    int classificacao = -1;
                    v.showMessage(s.verEncomendasPorClassificar(codigo));
                    opcao = -1;
                    while (opcao != 0 && !s.existeEncomendaPorClassificar(cod)) {
                        v.showMessage("\nIntroduza o código da encomenda a classificar ou pressione (0) voltar > ");
                        cod = i.lerString();
                        if (s.tryParseInt(cod)) {
                            if (Integer.parseInt(cod) == 0)
                                opcao = 0;
                        }
                    }
                    if (opcao != 0) {
                        v.user();
                        while (classificacao < 0 || classificacao > 5) {
                            v.showMessage("\nClassifique o serviço de transporte [0 a 5] > ");
                            classificacao = i.lerInt();
                        }
                        s.classificaEncomenda(cod, classificacao);
                        v.showMessage("\nEncomenda " + cod + " classificada com sucesso.");
                        while (opcao != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                        }
                    }
                    opcao = -1;
                    break;
            }
        }
    }


    /**
     * Função que permite ao utilizador efetuar uma encomenda
     * @param codLoja codigo da loja
     * @param carrinho Carrinho atual de compras que vai possuir a lista de compras efetuadas
     */
    private void escolherProduto(String codLoja, Collection<LinhaEncomenda> carrinho) {
        Input i = new Input();
        String codProd;
        v.utilizadorOpcao1EscolhaProduto();
        String prods = "";
        int pagina, opcao = -1;
        while (opcao != 0) {
            pagina = 1;
            codProd = "";
            prods = s.buscarProdsAoCat(codLoja, pagina++);
            do {
                opcao = -1;
                v.showMessage("\n");
                v.showMessage(prods);
                while (opcao != 0) {
                    v.showMessage("\n\nPressione (0) para continuar > ");
                    opcao = i.lerInt();
                }
                prods = s.buscarProdsAoCat(codLoja, pagina++);
            } while (!prods.equals("Não existe catálogo para esta loja!\n"));
            while (!s.existeProdutoNaLoja(codLoja, codProd) && !codProd.equals("0")) {
                v.showMessage("\nIntroduza o código do produto que quer encomendar [p(número)] ou (0) para sair > ");
                codProd = i.lerString();
            }
            if(codProd.equals("0")){
                break;
            }
            /* Neste ponto pergunta-se quantas unidades se quer comprar */
            double qtd = 0.0;
            while (qtd <= 0.0) {
                v.utilizadorOpcao1EscolhaDeUnidades();
                qtd = i.lerDouble();
            }
            LinhaEncomenda le = s.criarLinha(qtd, codProd, codLoja);
            carrinho.add(le.clone());
            opcao = -1;
            while (opcao != 1 && opcao != 0) {
                v.showMessage("\nProduto adicionado ao carrinho > (1) Adicionar mais (0) Voltar\nOpção > ");
                opcao = i.lerInt();
            }
        }
    }



    /**
     * Função que permite ter acesso ás várias funcionalidades da loja
     * @param codigo Codigo da Loja
     */
    private void funcL(String codigo) {
        Input i = new Input();
        int opcao = -1;
        String opcaoEncomenda = "";
        while (opcao != 0) {
            if (s.lojaTemInfoFilaEspera(codigo)) {
                v.funcionalidadesLojaComInfoEspera();
                opcao = i.lerInt();
                switch (opcao) {
                    case 1:
                        v.loja();
                        v.showMessage(s.imprimeEncNovasLojas(codigo));
                        opcao = -1;
                        while (opcao != 0 && !s.existeCodEnc(opcaoEncomenda)) {
                            v.showMessage("\nIntroduza o código da encomenda pronta a ser entregue ou pressione (0) voltar > ");
                            opcaoEncomenda = i.lerString();
                            if (s.tryParseInt(opcaoEncomenda)) {
                                if (Integer.parseInt(opcaoEncomenda) == 0)
                                    opcao = 0;
                            }
                        }
                        if (opcao != 0) {
                            v.loja();
                            s.sinalizaEncomendaProntaParaEntrega(opcaoEncomenda);
                            v.showMessage("\nEncomenda " + opcaoEncomenda + " processada com sucesso.");
                            while (opcao != 0) {
                                v.showMessage("\nPressione (0) voltar > ");
                                opcao = i.lerInt();
                            }
                        }
                        opcao = -1;
                        break;
                    case 2:
                        v.loja();
                        v.showMessage("\nNúmero de pessoas em fila de espera: ");
                        v.showMessage(s.pessoasEmEspera(codigo));
                        v.showMessage("\n");
                        v.showMessage("Tempo médio por pessoa: ");
                        v.showMessage(s.tempoAtendimentoPorPessoa(codigo));
                        v.showMessage("\n");
                        v.showMessage("Tempo estimado de espera: ");
                        v.showMessage(s.tempoEstimadoDeEspera(codigo));
                        v.showMessage("\n");
                        opcao = -1;
                        while (opcao != 1 && opcao != 2 && opcao != 0) {
                            v.showMessage("\nPressione (1) alterar número de pessoas em fila de espera");
                            v.showMessage("\nPressione (2) alterar tempo médio de atendimento por pessoa");
                            v.showMessage("\nPressione (0) voltar ");
                            v.showMessage("\nOpcao > ");
                            opcao = i.lerInt();
                        }
                        switch (opcao) {
                            case 1:
                                opcao = -1;
                                v.loja();
                                while (opcao < 0) {
                                    v.showMessage("\nInsira número de pessoas em espera > ");
                                    opcao = i.lerInt();
                                }
                                s.setNumeroDePessoasEmEspera(codigo, opcao);
                                v.showMessage("\nNúmero de pessoas em espera alterado com sucesso.");
                                opcao = -1;
                                while (opcao != 0) {
                                    v.showMessage("\nPressione (0) voltar > ");
                                    opcao = i.lerInt();
                                }
                                break;
                            case 2:
                                opcao = -1;
                                v.loja();
                                while (opcao < 0) {
                                    v.showMessage("\nInsira tempo médio de atendimento por pessoa (em segundos) > ");
                                    opcao = i.lerInt();
                                }
                                s.setTempoMedioAtendimentoPorPessoa(codigo, opcao);
                                v.showMessage("\nTempo médio de atendimento por pessoa alterado com sucesso.");
                                opcao = -1;
                                while (opcao != 0) {
                                    v.showMessage("\nPressione (0) voltar > ");
                                    opcao = i.lerInt();
                                }
                                break;
                        }
                        opcao = -1;
                        break;
                    case 3:
                        v.loja();
                        v.showMessage(s.historicoEncLojas(codigo));
                        opcao = -1;
                        while (opcao != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                        }
                        opcao = -1;
                        break;
                }
            } else {
                v.funcionalidadesLojaSemInfoEspera();
                opcao = i.lerInt();
                switch (opcao) {
                    case 1:
                        v.loja();
                        v.showMessage(s.imprimeEncNovasLojas(codigo));
                        opcao = -1;
                        while (opcao != 0 && !s.existeCodEnc(opcaoEncomenda)) {
                            v.showMessage("\nIntroduza o código da encomenda pronta a ser entregue ou pressione (0) voltar > ");
                            opcaoEncomenda = i.lerString();
                            if (s.tryParseInt(opcaoEncomenda)) {
                                if (Integer.parseInt(opcaoEncomenda) == 0)
                                    opcao = 0;
                            }
                        }
                        if (opcao != 0) {
                            v.loja();
                            s.sinalizaEncomendaProntaParaEntrega(opcaoEncomenda);
                            v.showMessage("\nEncomenda " + opcaoEncomenda + " processada com sucesso.");
                            while (opcao != 0) {
                                v.showMessage("\nPressione (0) voltar > ");
                                opcao = i.lerInt();
                            }
                        }
                        opcao = -1;
                        break;
                    case 2:
                        v.loja();
                        v.showMessage(s.historicoEncLojas(codigo));
                        opcao = -1;
                        while (opcao != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                        }
                        opcao = -1;
                        break;
                }
            }
        }
    }


    /**
     * Função que permite ter acesso ás várias funcionalidades do Voluntário
     * @param codigo Codigo do Voluntário
     */
    private void funcVoluntario(String codigo) {
        Input i = new Input();
        int opcao = -1;
        String opcaoS = "";
        while (opcao != 0) {
            v.funcionalidadesMeioTransporte(true);
            opcao = i.lerInt();
            switch (opcao) {
                case 1:
                    v.voluntario();
                    opcao = -1;
                    if (!s.estaNoMomentoATransportarOuEmAceitacao(codigo)) {
                        if (s.Certificado(codigo)) {
                            v.showMessage(s.mostraEstados(codigo, 1));
                            while (opcao != 1 && opcao != 0 && opcao != 2) {
                                v.showMessage("\nPressione (1) alterar disponibilidade para trabalhar");
                                v.showMessage("\nPressione (2) alterar disponibilidade para entregar encomendas médicas");
                                v.showMessage("\nPressione (0) voltar");
                                v.showMessage("\nOpção > ");
                                opcao = i.lerInt();
                            }
                        } else {
                            v.showMessage(s.mostraEstados(codigo, 2));
                            while (opcao != 1 && opcao != 0) {
                                v.showMessage("\nPressione (1) alterar disponibilidade para trabalhar");
                                v.showMessage("\nPressione (0) voltar");
                                v.showMessage("\nOpção > ");
                                opcao = i.lerInt();
                            }
                        }
                    } else {
                        v.showMessage("\nOperação indisponível.");
                        while (opcao != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                            v.clear();
                        }
                        opcao = -1;
                        break;
                    }

                    if (opcao == 1)
                        s.alteraDisponibilidadeTransporte(codigo);
                    else if (opcao == 2)
                        s.alteraDisponibilidadeTransporteMedico(codigo);
                    else {
                        opcao = -1;
                        break;
                    }
                    v.voluntario();
                    v.showMessage("\nOperacão efetuada com sucesso.");
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                        v.clear();
                    }
                    opcao = -1;
                    break;
                case 2:
                    v.voluntario();
                    opcao = -1;
                    if (!s.estaDisponivel(codigo)) {
                        v.showMessage("\nJá se encontra ocupado ou indísponivel.\n");
                    } else {
                        v.showMessage(s.verEncomendasDisponiveis(s.encomendasDisponiveis(codigo)));
                        if (!s.verEncomendasDisponiveis(s.encomendasDisponiveis(codigo)).equals("\nNão existem encomendas para entregar.\n")) {
                            while (opcao != 0 && !s.existeEncomendaNasEncomendasDisponiveis(codigo, opcaoS)) {

                                v.showMessage("\nCódigo da encomenda ou (0) voltar > ");
                                opcaoS = i.lerString();
                                if (s.tryParseInt(opcaoS)) {
                                    if (Integer.parseInt(opcaoS) == 0)
                                        opcao = 0;
                                }
                            }
                            if (opcao != 0)
                                v.showMessage(s.aceitaUmaEncomenda(codigo, opcaoS));
                            else {
                                opcao = -1;
                                break;
                            }
                        }
                    }
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                    }
                    opcao = -1;
                    break;
                case 3:
                    v.voluntario();
                    opcao = -1;
                    String CodEnc = s.finalizaUmaEnc(codigo);
                    if (CodEnc == null)
                        v.showMessage("Opção Inválida.\nDe momento não está nenhuma encomenda a ser transportada.\n");
                    else v.showMessage("\nEncomenda: " + CodEnc + ". Finalizada com sucesso.");
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                    }
                    opcao = -1;
                    break;
                case 4:
                    opcao = -1;
                    v.voluntario();
                    v.showMessage(s.historicoEncTransp(codigo));
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                    }
                    opcao = -1;
                    break;
            }
        }
    }


    /**
     * Função que permite ter acesso ás várias funcionalidades de uma empresa transportadora
     * @param codigo Codigo da Empresa transportadora
     */
    private void funcTransportadora(String codigo) {
        Input i = new Input();
        int opcao = -1;
        String opcaoS = "", dataInicio = "", dataFim = "";
        while (opcao != 0) {
            v.funcionalidadesMeioTransporte(false);
            opcao = i.lerInt();
            switch (opcao) {
                case 1:
                    v.transportadora();
                    opcao = -1;
                    if (!s.estaNoMomentoATransportarOuEmAceitacao(codigo)) {
                        if (s.Certificado(codigo)) {
                            v.showMessage(s.mostraEstados(codigo, 1));
                            while (opcao != 1 && opcao != 0 && opcao != 2) {
                                v.showMessage("\nPressione (1) alterar disponibilidade para trabalhar");
                                v.showMessage("\nPressione (2) alterar disponibilidade para entregar encomendas médicas");
                                v.showMessage("\nPressione (0) voltar");
                                v.showMessage("\nOpção > ");
                                opcao = i.lerInt();
                            }
                        } else {
                            v.showMessage(s.mostraEstados(codigo, 2));
                            while (opcao != 1 && opcao != 0) {
                                v.showMessage("\nPressione (1) alterar disponibilidade para trabalhar");
                                v.showMessage("\nPressione (0) voltar");
                                v.showMessage("\nOpção > ");
                                opcao = i.lerInt();
                            }
                        }
                    } else {
                        v.showMessage("\nOperação indisponível.");
                        while (opcao != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                            v.clear();
                        }
                        opcao = -1;
                        break;
                    }


                    if (opcao == 1)
                        s.alteraDisponibilidadeTransporte(codigo);
                    else if (opcao == 2)
                        s.alteraDisponibilidadeTransporteMedico(codigo);
                    else {
                        opcao = -1;
                        break;
                    }
                    v.transportadora();
                    v.showMessage("\nOperacão efetuada com sucesso.");
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                        v.clear();
                    }
                    opcao = -1;
                    break;
                case 2:
                    v.transportadora();
                    opcao = -1;
                    if (!s.estaDisponivel(codigo)) {
                        v.showMessage("\nJá se encontra ocupado ou indísponivel.\n");
                    } else {
                        v.showMessage(s.verEncomendasDisponiveis(s.encomendasDisponiveis(codigo)));
                        if (!s.verEncomendasDisponiveis(s.encomendasDisponiveis(codigo)).equals("\nNão existem encomendas para entregar.\n")) {
                            while (opcao != 0 && !s.existeEncomendaNasEncomendasDisponiveis(codigo, opcaoS)) {

                                v.showMessage("\nCódigo da encomenda ou (0) voltar > ");
                                opcaoS = i.lerString();
                                if (s.tryParseInt(opcaoS)) {
                                    if (Integer.parseInt(opcaoS) == 0)
                                        opcao = 0;
                                }
                            }
                            if (opcao != 0)
                                v.showMessage(s.aceitaUmaEncomenda(codigo, opcaoS));
                            else {
                                opcao = -1;
                                break;
                            }
                        }
                    }
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                    }
                    opcao = -1;
                    break;
                case 3:
                    opcaoS = "";
                    v.transportadora();
                    opcao = -1;
                    if (!s.podeTransportarVariasEncomendas(codigo)) {
                        String codEnc = s.finalizaUmaEnc(codigo);
                        if (codEnc == null)
                            v.showMessage("Opção Inválida.\nDe momento não está nenhuma encomenda a ser transportada.\n");
                        else v.showMessage("\nEncomenda: " + codEnc + ". Finalizada com sucesso.");
                        while (opcao != 0) {
                            v.showMessage("\nPressione (0) voltar > ");
                            opcao = i.lerInt();
                        }
                    } else {
                        if (s.encomendasASerEntreguesTransportadorasVariasEncomendas(codigo) == null) {
                            v.showMessage("Opção Inválida.\nDe momento não está nenhuma encomenda a ser transportada.\n");
                            while (opcao != 0) {
                                v.showMessage("\nPressione (0) voltar > ");
                                opcao = i.lerInt();
                            }
                        } else {
                            v.showMessage(s.encomendasASerEntreguesTransportadorasVariasEncomendas(codigo));
                            while (!s.existeCodEnc(opcaoS)) {
                                v.showMessage("\nCódigo da encomenda > ");
                                opcaoS = i.lerString();
                            }
                            String codEnc = s.finalizaVariasEncomendas(codigo, opcaoS);
                            v.showMessage("\nEncomenda: " + codEnc + ". Finalizada com sucesso.");

                        }

                    }
                    opcao = -1;
                    break;
                case 4:
                    opcao = -1;
                    v.transportadora();
                    v.showMessage(s.historicoEncTransp(codigo));
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                    }
                    opcao = -1;
                    break;
                case 5:
                    opcao = -1;
                    v.transportadora();
                    v.showMessage("Escolha a data inicial (p.ex 10/3/2020): ");
                    while (dataInicio.length() < 8 || dataInicio.length() > 10) {
                        v.showMessage("\nOpção > ");
                        dataInicio = i.lerString();
                    }
                    v.showMessage("\nEscolha a data final (p.ex 28/2/2020): ");
                    while (dataFim.length() < 8 || dataFim.length() > 10) {
                        v.showMessage("\nOpção > ");
                        dataFim = i.lerString();
                    }
                    LocalDate Inicio = s.dateInput(dataInicio);
                    LocalDate Fim = s.dateInput(dataFim);
                    v.transportadora();
                    v.showMessage("Total faturado entre " + dataInicio + " e " + dataFim + ": " + s.totalFaturadoEmpTrans(codigo, Inicio, Fim) + "€\n");
                    while (opcao != 0) {
                        v.showMessage("\nPressione (0) voltar > ");
                        opcao = i.lerInt();
                    }
                    opcao = -1;
                    break;
            }
        }
    }

    /**
     * Carrega o sistema a partir de ficheiro objeto (criado por nós)
     */
    public IModelo carregarSistemaDoFicheiro(){
        File f = new File("./sistemaTrazAqui.dat");

        if (f.exists() && !f.isDirectory()) {
            try {
                s = readFile("./sistemaTrazAqui.dat");
            } catch (IOException | ClassNotFoundException e) {
                e.printStackTrace();
                s = new SistemaTrazAqui();
            }
        }

        return s;
    }
}
