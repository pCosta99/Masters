package Projeto;

import java.util.ArrayList;
import java.util.Scanner;

public class Menu {
    private menuOption option;
    private boolean isUp;
    private String scan;

    public void menu() {
        this.option = menuOption.Login;
        this.isUp = true;
    }

    public enum menuOption {
        Login,
        RegistoCliente;
    }

    public boolean getIsUP() {
        return this.isUp;
    }

    public menuOption getOption() {
        return this.option;
    }

    public void options() {
        System.out.println("1- Login");
        System.out.println("2- Register");

    }
    //so entra neste menu se for um utilizador
    public void secondaryMenuAfterUserLogin(){
        System.out.println("3- Inserir pedidos de uma encomenda");
    }

    public NovoLogin novoLogin() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Username:");
        String username = scanner.nextLine();
        System.out.println("Password");
        String password = scanner.nextLine();

        return new NovoLogin(username, password);
    }

    public Loja novoRegistoLoja() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Username(numero inteiro):");
        int number = scanner.nextInt();
        String user = "l" + String.valueOf(number);
        System.out.println("Nome:");
        String nome1 = scanner.nextLine();
        String nome = scanner.nextLine();
        System.out.println("password:");
        String password = scanner.nextLine();
        System.out.println("Introduza a sua localização:");
        double x = scanner.nextDouble();
        double y = scanner.nextDouble();
        Posicao pos = new Posicao(x, y);

        ArrayList<LinhaEncomendas> linhaEncomendas = new ArrayList<LinhaEncomendas>();

        System.out.println("Deseja introduzir um produto no banco de dados desta loja? Sim/Nao");
        String get = scanner.nextLine();
        while (get.equals("Sim")) {
            String temp = scanner.nextLine();
            get = temp;
            String cod = scanner.nextLine();
            String desc = scanner.nextLine();
            double quantidade = scanner.nextDouble();
            double valor = scanner.nextDouble();

            LinhaEncomendas newLinha = new LinhaEncomendas(cod, desc, quantidade, valor);
            linhaEncomendas.add(newLinha);
        }

        return new Loja(user, nome, password, pos, linhaEncomendas);
    }

    public RegistoVoluntario novoRegistoVoluntario() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Username(Introduza um número inteiro):");
        int number = scanner.nextInt();
        System.out.println("Nome:");
        String nome1 = scanner.nextLine();
        String nome = scanner.nextLine();
        System.out.println("password:");
        String password = scanner.nextLine();
        System.out.println("Introduza a sua localização:");
        double x = scanner.nextDouble();
        double y = scanner.nextDouble();
        Posicao pos = new Posicao(x, y);
        System.out.println("Introduza o seu raio de ação");
        double raio = scanner.nextDouble();
        String user = "v" + String.valueOf(number);

        return new RegistoVoluntario(user, nome, password, pos, raio);
    }

    public RegistoUtilizador novoRegistoUtilizador() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Username(numero inteiro):");
        int number = scanner.nextInt();
        String user = "u" + String.valueOf(number);
        System.out.println("Nome:");
        String nome1 = scanner.nextLine();
        String nome = scanner.nextLine();
        System.out.println("password:");
        String password = scanner.nextLine();
        System.out.println("Introduza a sua localização:");
        double x = scanner.nextDouble();
        double y = scanner.nextDouble();
        Posicao pos = new Posicao(x, y);

        return new RegistoUtilizador(user, nome, password, pos);
    }

    public RegistoEmpresa novoRegistoEmpresa() {
        Scanner scanner = new Scanner(System.in);
        System.out.println("Username(Introduza um número inteiro):");
        int number = scanner.nextInt();
        String user = "t" + String.valueOf(number);
        System.out.println("Nome:");
        String nome1 = scanner.nextLine();
        String nome = scanner.nextLine();
        System.out.println("password:");
        String password = scanner.nextLine();
        System.out.println("Introduza a sua localização:");
        double x = scanner.nextDouble();
        double y = scanner.nextDouble();
        Posicao pos = new Posicao(x, y);
        System.out.println("Introduza o seu NIF");
        int nif = scanner.nextInt();
        System.out.println("Introduza o seu raio de ação");
        double raio = scanner.nextDouble();
        System.out.println("Introduza a sua taxa");
        int taxa = (int) scanner.nextDouble();

        return new RegistoEmpresa(user, pos, nif,taxa,raio);
    }

    public Encomenda novoPedidoEncomenda(String user) {
        Scanner scanner = new Scanner(System.in);
        System.out.println("\n código de encomenda:");
        int num = scanner.nextInt();
        String code = "e" + Integer.toString(num);

        //System.out.println(Arrays.toString(model.getLojas().toArray()));

        //return new Encomenda("21","21","21","21",new ArrayList<LinhaEncomendas>());

        System.out.println("Loja:");
        String loja1 = scanner.nextLine();
        String loja = scanner.nextLine();
        System.out.println("Peso");
        String peso = scanner.nextLine();

        ArrayList<LinhaEncomendas> linhaEncomendas = new ArrayList<LinhaEncomendas>();
        while (true) {
            System.out.println("Escolha Sim ou Nao para adicionar produtos \n");
            scanner.nextLine();
            String get = scanner.nextLine();

            if (get.equals("Nao"))
                break;

            System.out.println("Codigo do produto:");
            String cod = scanner.nextLine();
            System.out.println("Descrição:");
            String desc = scanner.nextLine();
            System.out.println("quantidade:");
            double quantidade = scanner.nextDouble();
            System.out.println("valor:");
            double valor = scanner.nextDouble();

            LinhaEncomendas newLinha = new LinhaEncomendas(cod, desc, quantidade, valor);
            linhaEncomendas.add(newLinha);
        }
        System.out.println("Encomenda criada");

        Encomenda enc = new Encomenda(code, user, loja, peso, linhaEncomendas);

        System.out.println(enc.toString());
        return enc;
    }

    public void print(String input) {
        System.out.println(input);
    }

    public String scanOption() {
        Scanner scanner = new Scanner(System.in);
        return scan = scanner.nextLine();
    }
}
