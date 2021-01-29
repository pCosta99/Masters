import java.text.DecimalFormat;
import java.time.DateTimeException;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Random;
import java.util.Scanner;
import java.util.TreeMap;

/**
 * Classe view que faz a interação com o utilizador, imprimindo toda a informação necessária e recebendo todos os inputs do mesmo
 */
public class View {
    private Scanner scanner;
    private Controller controller;

    /**
     * Construtor vazio
     */
    public View(){
        this.scanner = new Scanner(System.in);
        this.controller = new Controller();
    }

    /**
     * Construtor com argumentos
     * @param s Scanner
     * @param c Controller
     */
    public View(Scanner s, Controller c){
        this.scanner = s;
        this.controller = c;
    }

    /**
     * Construtor que recebe uma view
     * @param v View
     */
    public View(View v){
        this.scanner = v.scanner;
        this.controller = v.controller;
    }

    /**
     * Trata a exceção caso o input não seja um número inteiro
     * @param s String com o input do utilizador
     * @return true caso seja inteiro e false caso contrário
     */
    private Boolean verificaInteiro (String s){
        try{
            int num = Integer.parseInt(s);
            return true;
        } catch (NumberFormatException e) {
            System.out.println("Valor inválido. Introduzir novamente! ");
            return false;
        }
    }

    /**
     * Trata a exceção caso o input não seja um double
     * @param s String com o input do utilizador
     * @return true caso seja double e false caso contrário
     */
    private Boolean verificaDouble (String s){
        try{
            double num = Double.parseDouble(s);
            return true;
        } catch (NumberFormatException e) {
            System.out.println("Valor inválido. Introduzir novamente! ");
            return false;
        }
    }

    /**
     * Trata a exceção caso o input não seja um número inteiro com 9 digitos (valor de NIF válido)
     * @param s String com o input do utilizador
     * @return true caso seja um valor de NIF válido e false caso contrário
     */
    private Boolean verificaNif (String s){
        try{
            int num = Integer.parseInt(s);
            return s.length() == 9;
        } catch (NumberFormatException e) {
            System.out.println("Valor inválido. Introduzir novamente! ");
            return false;
        }
    }

    /**
     * Começa a execução do programa, imprimindo e gerindo o menu inicial da aplicação
     * @throws InterruptedException Exceção
     */
    public void run () throws InterruptedException {
        boolean flagOut = true;
        do {
            clearScreen();
            int flagLoggedIn = 0;
            int flagIn = 0;
            System.out.println("-------------------------\n");
            System.out.println(" BEM-VINDO AO TRAZAQUI!\n");
            System.out.println("-------------------------");
            Thread.sleep(1000);
            System.out.println("1 - Registar\n2 - Login\nq - Sair\n\nInsira a opção: ");
            String opcao;
            do {
                opcao = scanner.nextLine();
                Thread.sleep(1000);
                if (opcao.equals("1")) {
                    String tp;
                    do {
                        clearScreen();
                        System.out.print("Tipo de registo:\n1 - Particular\n2 - Empresa\nv - Voltar\n\nInsira a opção: ");
                        tp = scanner.nextLine();
                        if (tp.equals("1")) {
                            do {
                                clearScreen();
                                System.out.print("Registo:\n1 - Voluntario\n2 - Utilizador\nv - Voltar\n\nInsira a opção: ");
                                String opR = scanner.nextLine();
                                if (opR.equals("1")) {
                                    UserReg uV = registarVoluntario();
                                    flagLoggedIn = uV.getFlag();
                                    String xua = uV.getU().getCodigo();
                                    flagIn = logged(xua);
                                    if (flagIn == (-1)) opcao = "q";
                                    else if (flagIn == 0) opcao = "l";
                                }
                                else if (opR.equals("2")) {
                                    UserReg uR = registarUtilizador();
                                    flagLoggedIn = uR.getFlag();
                                    String xua = uR.getU().getCodigo();
                                    flagIn = loggedUser(xua);
                                    if (flagIn == (-1)) opcao = "q";
                                    else if (flagIn == 0) opcao = "l";
                                }
                                else if (opR.equals("v")) {
                                    opcao = "l";
                                    break;
                                } else System.out.println("Opção inválida! Volte a introduzir\n");
                            } while (flagLoggedIn == 0);
                        } else if (tp.equals("2")) {
                            do {
                                clearScreen();
                                System.out.print("Registo:\n1 - Loja\n2 - Transportadora\nv - Voltar\n\nInsira a opção: ");
                                String opR = scanner.nextLine();
                                if (opR.equals("1")) {
                                    UserReg uV = registarLoja();
                                    flagLoggedIn = uV.getFlag();
                                    String xua = uV.getU().getCodigo();
                                    flagIn = logged(xua);
                                    if (flagIn == (-1)) opcao = "q";
                                    else if (flagIn == 0) opcao = "l";
                                }
                                else if (opR.equals("2")) {
                                    UserReg uV = registarTransportadora();
                                    flagLoggedIn = uV.getFlag();
                                    String xua = uV.getU().getCodigo();
                                    flagIn = logged(xua);
                                    if (flagIn == (-1)) opcao = "q";
                                    else if (flagIn == 0) opcao = "l";
                                }
                                else if (opR.equals("v")) {
                                    opcao = "l";
                                    break;
                                } else System.out.println("Opção inválida! Volte a introduzir\n");
                            } while (flagLoggedIn == 0);
                        } else if (tp.equals("v")) {
                            opcao = "l";
                            break;
                        }
                    } while (flagLoggedIn == 0);
                }
                else if (opcao.equals("2")) {
                    flagLoggedIn = 0;
                    do {
                        clearScreen();
                        System.out.println("Introduzir username: ");
                        String nome = scanner.nextLine();
                        System.out.println("Introduzir password: ");
                        String pass = scanner.nextLine();
                        if (!controller.existeRegisto(nome)) {
                            String opN;
                            Thread.sleep(500);
                            clearScreen();
                            System.out.println("Username inválido!\n\nv - Voltar\nOu pressione qualquer outra tecla para tentar novamente!\n\nIntroduzir opção: ");
                            opN = scanner.nextLine();
                            if (opN.equals("v")) {flagLoggedIn=1; opcao = "l"; }
                        }
                        else if (!controller.verificaPass(nome,pass)) {
                            String opN;
                            Thread.sleep(500);
                            clearScreen();
                            System.out.println("Password inválida\n\nv - Voltar\nPressione qualquer outra tecla para tentar novamente!");
                            opN = scanner.nextLine();
                            if (opN.equals("v")) {flagLoggedIn=1; opcao = "l"; }
                        }
                        else {
                            Thread.sleep(500);
                            clearScreen();
                            if (nome.charAt(0)=='u') flagIn = loggedUser(nome);
                            else flagIn = logged(nome);
                            if (flagIn == (-1)) opcao = "q";
                            else if (flagIn == 0) opcao = "l";
                            flagLoggedIn = 1;
                        }
                    } while (flagLoggedIn == 0);
                }
            } while (!opcao.equals("q") && !opcao.equals("l"));
            if (opcao.equals("q")) flagOut = false;
        } while (flagOut);
    }

    /**
     * Efetua o registo de uma loja
     * @return Objeto que indica que a conta está logged in
     */
    public UserReg registarLoja () throws InterruptedException {
        int flagLoggedIn = 0;
        UserReg res = new UserReg();
        System.out.println("\nIntroduzir nome completo: ");
        String nome = scanner.nextLine();
        String email;
        do {
            System.out.println("Introduzir email: ");
            email = scanner.nextLine();
            if(controller.verificaEmail(email)) System.out.println("Este email já se encontra associado a uma conta. Introduza novamente!");
        }while(controller.verificaEmail(email));
        System.out.println("Introduzir password: ");
        String pass = scanner.nextLine();
        System.out.println("Introduir localização: ");
        String x;
        String y;
        do {
            System.out.println("X: ");
            x = scanner.nextLine();
        } while (!this.verificaInteiro(x));
        do {
            System.out.println("Y: ");
            y = scanner.nextLine();
        } while (!this.verificaInteiro(y));
        System.out.println("Qual é, em média, o tempo de espera (em minutos)?");
        String tp;
        do {
            tp = scanner.nextLine();
        } while (!this.verificaInteiro(tp));
        Ponto p = new Ponto(Integer.parseInt(x), Integer.parseInt(y));
        Loja u = new Loja();
        u.setNome(nome);
        u.setMail(email);
        u.setGps(p);
        u.setPassword(pass);
        u.setTempoMedioEspera(Double.parseDouble(tp));
        flagLoggedIn = 1;
        controller.adicionaRegisto(u,'l');
        clearScreen();
        System.out.println("IMPORTANTE!!!\nO seu username é: " + u.getCodigo());
	    Thread.sleep(2000);
        res.setFlag(flagLoggedIn);
        res.setU(u);
        return res;
    }

    /**
     * Efetua o registo de uma transportadora
     * @return Objeto que indica que a conta está logged in
     */
    public UserReg registarTransportadora () throws InterruptedException {
        int flagLoggedIn = 0;
        UserReg res = new UserReg();
        System.out.println("\nIntroduzir nome completo: ");
        String nome = scanner.nextLine();
        String email;
        do {
            System.out.println("Introduzir email: ");
            email = scanner.nextLine();
            if(controller.verificaEmail(email)) System.out.println("Este email já se encontra associado a uma conta. Introduza novamente!");
        }while(controller.verificaEmail(email));
        System.out.println("Introduzir password: ");
        String pass = scanner.nextLine();
        System.out.println("Introduir localização: ");
        String x;
        String y;
        do {
            System.out.println("X: ");
            x = scanner.nextLine();
        } while (!this.verificaInteiro(x));
        do {
            System.out.println("Y: ");
            y = scanner.nextLine();
        } while (!this.verificaInteiro(y));
        String cert;
        do {
            System.out.print("Tem certificado médico?\n1 - Sim\n2 - Não\n\nInsira a opção: ");
            cert = scanner.nextLine();
            if ((!cert.equals("1")) && (!cert.equals("2"))) System.out.println("Opção inválida! Volte a introduzir\n");
        }
        while ((!cert.equals("1")) && (!cert.equals("2")));
        System.out.println("Introduzir taxa do peso: ");
        String peso;
        do {
            peso = scanner.nextLine();
        } while (!this.verificaDouble(peso));
        System.out.println("Introduzir preço por km: ");
        String km;
        do {
            km = scanner.nextLine();
        } while (!this.verificaDouble(km));
        System.out.println("Introduzir raio: ");
        String raio;
        do {
            raio = scanner.nextLine();
        } while (!this.verificaDouble(raio));
        System.out.println("Introduzir NIF: ");
        String nif;
        do {
            nif = scanner.nextLine();
        } while (!this.verificaNif(nif));
        Ponto p = new Ponto(Integer.parseInt(x), Integer.parseInt(y));
        Transportadora u = new Transportadora();
        u.setNif(nif);
        u.setMail(email);
        u.setRaio(Double.parseDouble(raio));
        u.setNome(nome);
        u.setGps(p);
        u.setPassword(pass);
        u.setCertMed(cert.equals("1"));
        u.setPrecoKm(Double.parseDouble(km));
        u.setTaxaPeso(Double.parseDouble(peso));
        flagLoggedIn = 1;
        controller.adicionaRegisto(u,'t');
        clearScreen();
        System.out.println("IMPORTANTE!!!\nO seu username é: " + u.getCodigo());
	    Thread.sleep(2000);
        res.setFlag(flagLoggedIn);
        res.setU(u);
        return res;
    }

    /**
     * Efetua o registo de um voluntário
     * @return Objeto que indica que a conta está logged in
     */
    public UserReg registarVoluntario () throws InterruptedException {
        int flagLoggedIn = 0;
        UserReg res = new UserReg();
        System.out.println("\nIntroduzir nome completo: ");
        String nome = scanner.nextLine();
        String email;
        do {
            System.out.println("Introduzir email: ");
            email = scanner.nextLine();
            if(controller.verificaEmail(email)) System.out.println("Este email já se encontra associado a uma conta. Introduza novamente!");
        }while(controller.verificaEmail(email));
        System.out.println("Introduzir password: ");
        String pass = scanner.nextLine();
        System.out.println("Introduir localização: ");
        String x;
        String y;
        do {
            System.out.println("X: ");
            x = scanner.nextLine();
        } while (!this.verificaInteiro(x));
        do {
            System.out.println("Y: ");
            y = scanner.nextLine();
        } while (!this.verificaInteiro(y));
        String cert;
        do {
            System.out.print("Tem certificado médico?\n1 - Sim\n2 - Não\n\nInsira a opção: ");
            cert = scanner.nextLine();
            if ((!cert.equals("1")) && (!cert.equals("2"))) System.out.println("Opção inválida! Volte a introduzir\n");
        } while ((!cert.equals("1")) && (!cert.equals("2")));
        System.out.println("Introduzir raio: ");
        String raio;
        do {
            raio = scanner.nextLine();
        } while (!this.verificaDouble(raio));
        Ponto p = new Ponto(Integer.parseInt(x), Integer.parseInt(y));
        Voluntario u = new Voluntario();
        u.setRaio(Double.parseDouble(raio));
        u.setNome(nome);
        u.setMail(email);
        u.setGps(p);
        u.setPassword(pass);
        u.setCertMed(cert.equals("1"));
        flagLoggedIn = 1;
        controller.adicionaRegisto(u,'v');
        clearScreen();
        System.out.println("IMPORTANTE!!!\nO seu username é: " + u.getCodigo());
	    Thread.sleep(2000);
        res.setFlag(flagLoggedIn);
        res.setU(u);
        return res;
    }

    /**
     * Efetua o registo de um utilizador
     * @return Objeto que indica que a conta está logged in
     */
    public UserReg registarUtilizador () throws InterruptedException {
        int flagLoggedIn = 0;
        UserReg res = new UserReg();
        System.out.println("\nIntroduzir nome completo: ");
        String nome = scanner.nextLine();
        String email;
        do {
            System.out.println("Introduzir email: ");
            email = scanner.nextLine();
            if(controller.verificaEmail(email)) System.out.println("Este email já se encontra associado a uma conta. Introduza novamente!");
        }while(controller.verificaEmail(email));
        System.out.println("Introduzir password: ");
        String pass = scanner.nextLine();
        System.out.println("Introduir localização: ");
        String x;
        String y;
        do {
            System.out.println("X: ");
            x = scanner.nextLine();
        } while (!this.verificaInteiro(x));
        do {
            System.out.println("Y: ");
            y = scanner.nextLine();
        } while (!this.verificaInteiro(y));
        Ponto p = new Ponto(Integer.parseInt(x), Integer.parseInt(y));
        Utilizador u = new Utilizador();
        u.setNome(nome);
        u.setGps(p);
        u.setMail(email);
        u.setPassword(pass);
        flagLoggedIn = 1;
        controller.adicionaRegisto(u,'u');
        clearScreen();
        System.out.println("IMPORTANTE!!!\nO seu username é: " + u.getCodigo());
	    Thread.sleep(2000);
        res.setFlag(flagLoggedIn);
        res.setU(u);
        return res;
    }

    /**
     * Apresenta e gere o menu de login de uma distribuidora/loja
     * @param d Código distrivuidora/loja
     * @return 0 caso se faça logout ou -1 caso se saia da aplicação
     * @throws InterruptedException Exceção
     */
    public int logged (String d) throws InterruptedException {
        String opcao,s,data1,data2;
        DecimalFormat f1 = new DecimalFormat("#.##");
        if(d.charAt(0)=='t') {
            do {
                clearScreen();
                System.out.println("---------------------------------------");
                System.out.println(" CONECTADO COMO TRANSPORTADORA - " + d);
                System.out.println("---------------------------------------");
                System.out.print("1 - Encomendas\n2 - Aceitar/Rejeitar Encomendas\n3 - Top 10 Utilizadores\n4 - Top 10 Transportadoras\n5 - Faturações\nl - Logout\nq - Sair\n\nInsira a opção: ");
                opcao = scanner.next();
                if (opcao.equals("1")) {
                    clearScreen();
                    if (controller.devolveMyEncsToString(d).equals("")) System.out.println("Nenhuma encomenda disponível!");
                    System.out.println(controller.devolveMyEncsToString(d));
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    }while(!s.equals("v"));
                }
                else if (opcao.equals("2")) {
                    clearScreen();
                    tratamentoEncomendas(d);
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    }while(!s.equals("v"));
                }
                else if (opcao.equals("3")) {
                    clearScreen();
                    System.out.println(controller.top10Utilizadores());
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    } while (!s.equals("v"));
                } else if (opcao.equals("4")) {
                    clearScreen();
                    System.out.println(controller.top10Transportadoras());
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    } while (!s.equals("v"));
                } else if (opcao.equals("5")) {
                    do {
                        int flagfeia;
                        clearScreen();
                        System.out.println("1 - Introduzir período");
                        System.out.println("v - Voltar\n");
                        System.out.println("Insira opção: ");
                        s = scanner.next();
                        if (s.equals("1")) {
                            System.out.println("Introduza o período de datas:");
                            do {
                                System.out.println("Data inicial (aa-mm-dd):");
                                data1 = scanner.next();
                                try{
                                    if (!validateInputDate(data1)){
                                        System.out.println("Input Inválido");
                                        flagfeia = 1;
                                    }
                                    else flagfeia = 0;
                                }
                                catch (NumberFormatException e){
                                    System.out.println("Input Inválido");
                                    flagfeia = 1;
                                }
                            } while (flagfeia == 1);
                            do {
                                System.out.println("Data final (aa-mm-dd):");
                                data2 = scanner.next();
                                try{
                                    if (!validateInputDate(data2)){
                                        System.out.println("Input Inválido");
                                        flagfeia = 1;
                                    }
                                    else flagfeia = 0;
                                }
                                catch (NumberFormatException e){
                                    System.out.println("Input Inválido");
                                    flagfeia = 1;
                                }
                            } while (flagfeia == 1);
                            System.out.println("Faturação entre " + data1 + " e " + data2 + " : " + f1.format(controller.faturacaoPeriodoT(data1,data2,d)) + "\n");
                            System.out.println("v - Voltar\n\nInsira opção:");
                            do {
                                s = scanner.next();
                            } while (!s.equals("v"));
                        }
                    } while (!s.equals("v"));
                }
                if (opcao.equals("l")) return 0;
                else if (opcao.equals("q")) return (-1);
            } while (true);
        }
        if (d.charAt(0)=='v') {
            do {
                clearScreen();
                System.out.println("---------------------------------------");
                System.out.println(" CONECTADO COMO VOLUNTÁRIO - " + d);
                System.out.println("---------------------------------------");
                System.out.print("1 - Todas Encomendas\n2 - Aceitar/Rejeitar Encomendas\n3 - Top 10 Utilizadores\n4 - Top 10 Transportadoras\nl - Logout\nq - Sair\n\nInsira a opção: ");
                opcao = scanner.next();
                if (opcao.equals("1")) {
                    clearScreen();
                    if (controller.devolveMyEncsToString(d).equals("")) System.out.println("Nenhuma encomenda disponível!");
                    System.out.println(controller.devolveMyEncsToString(d));
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    }while(!s.equals("v"));
                }
                else if (opcao.equals("2")) {
                    clearScreen();
                    tratamentoEncomendas(d);
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    }while(!s.equals("v"));
                }
                else if (opcao.equals("3")) {
                    clearScreen();
                    System.out.println(controller.top10Utilizadores());
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    } while (!s.equals("v"));
                } else if (opcao.equals("4")) {
                    clearScreen();
                    System.out.println(controller.top10Transportadoras());
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    } while (!s.equals("v"));
                }
                if (opcao.equals("l")) return 0;
                else if (opcao.equals("q")) return (-1);
            }while (true);
        }
        else {
            do {
                clearScreen();
                System.out.println("---------------------------------------");
                System.out.println(" CONECTADO COMO LOJA - " + d);
                System.out.println("---------------------------------------");
                System.out.print("1 - Encomendas\n2 - Top 10 Utilizadores\n3 - Top 10 Transportadoras\nl - Logout\nq - Sair\n\nInsira a opção: ");
                opcao = scanner.next();
                if (opcao.equals("1")) {
                    clearScreen();
                    if (controller.devolveMyEncsToString(d).equals("")) System.out.println("Nenhuma encomenda disponível!");
                    System.out.println(controller.devolveMyEncsToString(d));
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    }while(!s.equals("v"));
                }
                else if (opcao.equals("2")) {
                    clearScreen();
                    System.out.println(controller.top10Utilizadores());
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    } while (!s.equals("v"));
                } else if (opcao.equals("3")) {
                    clearScreen();
                    System.out.println(controller.top10Transportadoras());
                    System.out.println("v - Voltar\n\nInsira opção:");
                    do {
                        s = scanner.next();
                    } while (!s.equals("v"));
                }
                if (opcao.equals("l")) return 0;
                else if (opcao.equals("q")) return (-1);
            }while (true);
        }
    }

    /**
     * Gere a aceitação/rejeição de uma encomenda por parte de um distribuidor
     * @param d Código do distribuidor
     */
    private void tratamentoEncomendas(String d) {
        Scanner s = new Scanner(System.in);
        List<Encomenda> encs = controller.devolveEncsPendentes(d);
        if (encs != null && encs.size() != 0) {
            System.out.println(controller.devolveEncsPendentesToString(d));
            String op;
            int opr;
            do {
                System.out.println("Escolha uma encomenda. Introduza a opcão:");
                op = s.next();
                try{
                    opr = Integer.parseInt(op);
                } catch (NumberFormatException e) {
                    opr = 0;
                }
                if (opr <= 0 || opr > encs.size()) System.out.println("Opcao inválida");
            } while (opr <= 0 || opr > encs.size());
            do {
                System.out.println("\n1 - Aceitar\n2 - Rejeitar\n\nIntroduza a opcão:");
                op = s.next();
                if (!op.equals("1") && !op.equals("2")) System.out.println("Opcão inválida");
            } while (!op.equals("1") && !op.equals("2"));
            Encomenda e = encs.get(opr-1);
            if (op.equals("1")) {
                controller.aceitarEncomenda(e, d);
                System.out.println("Encomenda aceite\n");
                controller.adicionaMyEncAv(e, d);
            }
            else {
                controller.rejeitarEncomenda(e, d);
                System.out.println("Encomenda rejeitada\n");
            }
        }
        else System.out.println("Nenhuma encomenda pendente! Bom trabalho! Ou não...\n");
    }

    /**
     * Apresenta e gere o menu de login de um utilizador
     * @param u Código do utilizador
     * @return 0 caso se faça logout ou -1 caso se saia da aplicação
     * @throws InterruptedException Exceção
     */
    public int loggedUser (String u) throws InterruptedException {
        int f = 0;
        do {
            clearScreen();
            String opcao, opcao1,s,op,op2, k;
            int x, fl = 0;
            double y;
            System.out.println("---------------------------------------");
            System.out.println(" CONECTADO COMO UTILIZADOR - " + u);
            System.out.println("---------------------------------------");
            System.out.print("1 - Realizar encomenda\n2 - Minhas encomendas\n3 - Top 10 Utilizadores\n4 - Top 10 Transportadoras\nl - Logout\nq - Sair\n\nInsira a opção: ");
            opcao = scanner.next();
            if (opcao.equals("l")) return 0;
            else if (opcao.equals("q")) return (-1);
            else if (opcao.equals("1")) f = realizaEncomenda(u);
            else if (opcao.equals("2")) {
                do {
                    clearScreen();
                    fl = 0;
                    System.out.println("1 - Mostrar minhas encomendas\n2 - Classificar serviços de entrega\n3 - Rastreamento de encomenda\nv - Voltar\n\nInsira a opção: ");
                    opcao1 = scanner.next();
                    if (!opcao1.equals("1") && !opcao1.equals("2") && !opcao1.equals("3") && !opcao1.equals("v")) System.out.println("Opção inválida!");
                    else if (opcao1.equals("1")) {
                        clearScreen();
                        String aux1 = controller.devolveMyEncsToString(u);
                        if (aux1.equals("")) System.out.println("Nenhuma encomenda disponível!");
                        System.out.println(aux1);
                        System.out.println("v - Voltar\n\nInsira opção: ");
                        do {
                            s = scanner.next();
                        } while (!s.equals("v"));
                    }
                    else if (opcao1.equals("2")) {
                        clearScreen();
                        List<EncDistr> aux = controller.devolveMyEncsAv(u);
                        int auxSize = aux.size();
                        do {
                            if (aux.size() == 0) System.out.println("Nenhuma encomenda por avaliar!\n");
                            else {
                                System.out.println(controller.devolveMyEncsAvToString(u));
                                System.out.println("a - Escolher a encomenda a avaliar");
                            }
                            System.out.println("v - Voltar\n\nInsira a opção: ");
                            op = scanner.next();
                            if (op.equals("a")) {
                                do {
                                    System.out.println("i - Inserir número de encomenda\nv - Voltar\n\nInsira a opção:");
                                    op2 = scanner.next();
                                    if (op2.equals("i")) {
                                        do {
                                            System.out.println("Número da encomenda: ");
                                            try {
                                                k = scanner.next();
                                                x = Integer.parseInt(k);
                                                if (x > auxSize || x < 1) System.out.println("Número inválido. Volte a inserir!");
                                            } catch (NumberFormatException e) {
                                                System.out.println("Número inválido. Volte a inserir!");
                                                x = 0;
                                            }
                                        } while (x > auxSize || x < 1);
                                        EncDistr e = aux.get(x-1);
                                        String t = e.getDistribuidora();
                                        do {
                                            System.out.println("Classificação (0-5): ");
                                            try {
                                                k = scanner.next();
                                                y = Double.parseDouble(k);
                                                if (y < 0 || y > 5) System.out.println("Classificação inválida");
                                            } catch (NumberFormatException i) {
                                                System.out.println("Número inválido. Volte a inserir!");
                                                y = -1;
                                            }
                                        } while (y < 0 || y > 5);
                                        if (t.charAt(0)=='t') {
                                            Transportadora tp = controller.classificaTransportadora(t,y);
                                            System.out.println("Classificação atualizada de " + tp.getNome() + " : " + tp.classificacaoMedia() + " estrelas\n");
                                        }
                                        else if (t.charAt(0)=='v') {
                                            Voluntario vl = controller.classificaVoluntario(t,y);
                                            System.out.println("Classificação atualizada de " + vl.getNome() + " : " + vl.classificacaoMedia() + " estrelas\n");
                                        }
                                        controller.removeMyEncAv(u,e);
                                        fl = 1;
                                        System.out.println("v - Voltar\n\nInsira opção: ");
                                        do {
                                            s = scanner.next();
                                        } while (!s.equals("v"));
                                    }
                                } while (!op2.equals("v") && fl == 0);
                            }
                        } while (!op.equals("v") && fl == 0);

                    }
                    else if (opcao1.equals("3")) {
                        clearScreen();
                        List<EncDistr> aux = controller.devolveMyEncs(u);
                        int auxSize = aux.size();
                        do {
                            if (aux.size() == 0) System.out.println("Nenhuma encomenda para rastrear!\n");
                            else {
                                System.out.println(controller.devolveMyEncsToString(u));
                                System.out.println("a - Escolher a encomenda a rastrear");
                            }
                            System.out.println("v - Voltar\n\nInsira a opção: ");
                            op = scanner.next();
                            if (op.equals("a")) {
                                do {
                                    System.out.println("i - Inserir número de encomenda\nv - Voltar\n\nInsira a opção:");
                                    op2 = scanner.next();
                                    if (op2.equals("i")) {
                                        System.out.println("Número da encomenda: ");
                                        do {
                                            try {
                                                k = scanner.next();
                                                x = Integer.parseInt(k);
                                                if (x > auxSize || x < 1) System.out.println("Número inválido. Volte a inserir!");
                                            } catch (NumberFormatException e) {
                                                System.out.println("Número inválido. Volte a inserir!");
                                                x = 0;
                                            }
                                        } while (x > auxSize || x < 1);
                                        EncDistr e = aux.get(x-1);
                                        Encomenda enc = e.getEncomenda();
                                        if (enc.getEstado().equals("Entregue")) {
                                            System.out.println("\nEncomenda entregue"+ "\n---------------------\n" +
                                                    enc.getDataA() + " : " + "Requirida" + "\n" +
                                                    enc.getDataP() + " : " + "Em preparação" + "\n" +
                                                    enc.getDataL() + " : " + "Pronta a ser entregue pela Loja" + "\n" +
                                                    enc.getDataR() + " : " + "Recolhida" + "\n" +
                                                    enc.getDataV() + " : " + "Em viagem" + "\n" +
                                                    enc.getDataE() + " : " + "Entregue\n");
                                        }
                                        else if (enc.getEstado().equals("Pendente")) {
                                            System.out.println("\nEncomenda pendente"+ "\n---------------------\n" +
                                                    enc.getDataA() + " : " + "Requirida\n");
                                        }
                                        else if (enc.getEstado().equals("Cancelada")) {
                                            System.out.println("\nEncomenda cancelada"+ "\n---------------------\n" +
                                                    enc.getDataA() + " : " + "Requirida\n" +
                                                    enc.getDataP() + " : " + "Cancelada\n");
                                        }
                                    }
                                    if (op2.equals("v")) fl = 1;
                                } while (!op2.equals("v"));
                            }
                        } while (!op.equals("v") && fl == 0);
                    }
                }while(!opcao1.equals("v"));
            }
            else if (opcao.equals("3")) {
                clearScreen();
                System.out.println(controller.top10Utilizadores());
                System.out.println("v - Voltar\n\nInsira opção:");
                do {
                    s = scanner.nextLine();
                } while(!s.equals("v"));
            }
            else if (opcao.equals("4")) {
                clearScreen();
                System.out.println(controller.top10Transportadoras());
                System.out.println("v - Voltar\n\nInsira opção:");
                do {
                    s = scanner.nextLine();
                } while(!s.equals("v"));
            }
        } while (true);
    }

    /**
     * Limpa a consola
     */
    public static void clearScreen() {
        System.out.print("\033[H\033[2J");
        System.out.flush();
    }

    /**
     * Gere um pedido de encomenda por parte de um utilizaodr
     * @param u Código utilizador
     * @return 0 no final da função
     * @throws InterruptedException Exceção
     */
    public int realizaEncomenda(String u) throws InterruptedException {
        String op,op2,x,opcao;
        int fM, codL = 0;
        DecimalFormat f1 = new DecimalFormat("#.##");
        Encomenda e = new Encomenda();
        e.setCodEncomenda(controller.geraCodEncomenda());
        Loja l;
        do {
            clearScreen();
            System.out.println("Em que loja pretende realizar a encomenda?");
            System.out.println(controller.devolveLojas());
            System.out.println("Introduzir código: ");
            op2 = scanner.next();
            l = (Loja) controller.devolveRegistoTULV(op2);
            if (l == null) {
                System.out.println("Loja inválida! Introduza novamente!");
                Thread.sleep(2000);
            }
        }while (l==null);
        do {
            System.out.println("\nQual o tipo de encomenda?\n1 - Normal\n2 - Médica\n\nIntroduzir opção: ");
            op = scanner.next();
        }while(!op.equals("1") && !op.equals("2"));
        if(op.equals("1")) {
            e.setEncMedica(false);
            fM = 1;
        }
        else {
            e.setEncMedica(true);
            fM = 2;
        }
        Scanner ex = new Scanner(System.in);
        do {
            System.out.println("\nQual é o produto que deseja encomendar?\n\nIntroduzir produto: ");
            op = ex.nextLine();
            LinhaEncomenda l1 = new LinhaEncomenda();
            l1.setDescricao(op);
            l1.setCodProd(controller.geraCodProd(codL));
            String sp[] = l1.getCodProd().split("p");
            codL = Integer.parseInt(sp[1]) + 1;
            Random rs = new Random();
            double preco = 20 * rs.nextDouble();
            l1.setValorUnitario(preco);
            do {
                System.out.println("\nQue quantidade deseja comprar?\n\nIntroduzir quantidade: ");
                x = scanner.next();
            } while (!this.verificaInteiro(x));
            l1.setQuantidade(Double.parseDouble(x));
            do {
                System.out.println("\nQual é o peso aproximado da encomenda?\n\nIntroduzir peso em Kgs: ");
                x = scanner.next();
            } while (!this.verificaDouble(x));
            e.setPeso(e.getPeso() + Double.parseDouble(x) * l1.getQuantidade());
            e.adicionaLinha(l1);
            do {
                System.out.println("\nDeseja encomendar mais algum produto?\n1 - Sim\n2 - Não\n\nInsira a opção: ");
                opcao = scanner.next();
            } while (!opcao.equals("1") && !opcao.equals("2"));
        } while (!opcao.equals("2"));
        e.setCodUtilizador(u);
        e.setCodLoja(l.getCodigo());
        Transportadora t = controller.devolveTranspFaster(u,l,fM);
        int flagTrans = 1, flagVol = 1;
        if (t == null) {
            System.out.println("Não há nenhuma transportadora disponível!");
            flagTrans = 0;
        }
        else System.out.println("A transportadora mais rápida é: " + t.getNome() + ", tem o custo de " + f1.format(t.calculoPreco(l,(Utilizador) controller.devolveRegistoTULV(u),e)) + " euros" + " e seu tempo de viagem é de aproximadamente " + f1.format(t.calculoTempo(l,(Utilizador) controller.devolveRegistoTULV(u))) + " horas");
        Voluntario v = controller.devolveVolFaster(u,l,fM);
        if (v == null) {
            System.out.println("Não há nenhum voluntário disponível!");
            flagVol = 0;
        }
        else System.out.println("O Voluntário mais rápido é : " + v.getNome()  + " e seu tempo de viagem é de aproximadamente " + f1.format(v.calculoTempo(l,(Utilizador) controller.devolveRegistoTULV(u))) + " horas");
        if (flagTrans == 0 && flagVol == 0) {
            System.out.println("Não há meios de entrega disponíveis!");
            Thread.sleep(2000);
            return 0;
        }
        else if (flagTrans == 0) {
            System.out.println("A encomenda será entregue pelo voluntário " + v.getNome());
            double pr = e.calculaPrecoTotal();
            System.out.println("Custo total : " + f1.format(pr) + " euros");
            System.out.println("Custo dos produtos: " + f1.format(pr) + " euros");
            System.out.println("Custo do transporte: Grátis");
            System.out.println("\nDeseja finalizar a encomenda?\n1 - Sim\n2 - Não\n\nInsira a opção: ");
            op = scanner.next();
            if (op.equals("1")) {
                controller.finalizaEncomendaV(e,u,v,l);
            } else return 0;
        }
        else if (flagVol == 0) {
            System.out.println("A encomenda será entregue pela transportadora " + t.getNome());
            double pr = e.calculaPrecoTotal() + t.calculoPreco(l, (Utilizador) controller.devolveRegistoTULV(u), e);
            System.out.println("Custo total : " + f1.format(pr) + " euros");
            System.out.println("Custo dos produtos: " + f1.format(e.calculaPrecoTotal()) + " euros");
            System.out.println("Custo do transporte: " + t.calculoPreco(l, (Utilizador) controller.devolveRegistoTULV(u), e) + " euros");
            System.out.println("\nDeseja finalizar a encomenda?\n1 - Sim\n2 - Não\n\nInsira a opção: ");
            op = scanner.next();
            if (op.equals("1")) {
                controller.finalizaEncomendaT(e,u,t,l);
            } else return 0;
        }
        else {
            System.out.println("\nDe que maneira pretende que o seu produto seja entregue?\n1 - Express\n2 - Low Cost\n\nInsira a opção: ");
            op = scanner.next();
            if (op.equals("1")) {
                double pr = e.calculaPrecoTotal() + t.calculoPreco(l, (Utilizador) controller.devolveRegistoTULV(u), e);
                System.out.println("Custo total : " + f1.format(pr) + " euros");
                System.out.println("Custo dos produtos: " + f1.format(e.calculaPrecoTotal()) + " euros");
                System.out.println("Custo do transporte: " + t.calculoPreco(l, (Utilizador) controller.devolveRegistoTULV(u), e) + " euros");
                System.out.println("\nDeseja finalizar a encomenda?\n1 - Sim\n2 - Não\n\nInsira a opção: ");
                op = scanner.next();
                if (op.equals("1")) {
                    controller.finalizaEncomendaT(e,u,t,l);
                } else return 0;
            }
            else {
                double pr = e.calculaPrecoTotal();
                System.out.println("Custo total : " + f1.format(pr) + " euros");
                System.out.println("Custo dos produtos: " + f1.format(pr) + " euros");
                System.out.println("Custo do transporte: Grátis");
                System.out.println("\nDeseja finalizar a encomenda?\n1 - Sim\n2 - Não\n\nInsira a opção: ");
                op = scanner.next();
                if (op.equals("1")) {
                    controller.finalizaEncomendaV(e,u,v,l);
                } else return 0;
            }
        }
        return 0;
    }

    /**
     * Valida uma data introduzida por um utilizador
     * @param isoDate Data recebida no input
     * @return true se válida ou faslse caso contrário
     */
    public static boolean validateInputDate(String isoDate) {
        String[] dateProperties = isoDate.split("-");
        if(dateProperties != null) {
            int year = Integer.parseInt(dateProperties[0]);
            int month = dateProperties.length > 1 ? Integer.parseInt(dateProperties[1]) : 1;
            int day = dateProperties.length > 2 ? Integer.parseInt(dateProperties[2]) : 1;
            try {
                LocalDate.of(year, month, day);
                return true;
            }
            catch(DateTimeException e){
                return false;
            }
        }
        return false;
    }
}

