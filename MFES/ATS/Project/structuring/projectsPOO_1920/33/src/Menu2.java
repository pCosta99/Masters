import java.io.IOException;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Menu2 {}
   /* public static void menu() {
        System.out.println("1 - Ler os dados do logs.cvs");
        System.out.println("2 - Ler os dados do logs2.cvs");
        System.out.println("3 - Adicionar um utlilizador");
        System.out.println("4 - Adicionar um voluntário");
        System.out.println("5 - Adicionar uma transportadora");
        System.out.println("6 - Adicionar uma loja");
        System.out.println("7 - Adicionar uma encomenda");
        System.out.println("8 - Adicionar uma encomenda aceite");
        System.out.println("9 - Imprimir utilizadores");
        System.out.println("10 - Imprimir voluntários");
        System.out.println("11 - Imprimir Transportadoras");
        System.out.println("12 - Imprimir Lojas");
        System.out.println("13 - Imprimir Encomendas");
        System.out.println("14 - Imprimir Encomendas aceites");
        System.out.println("15 - Guardar ficheiro");
        System.out.println("-1 : Sair");
        System.out.print("Opção escolhida: ");
    }

    public static void main(String[] args) throws IOException {
        Parse p = new Parse();

        p.parse("src/logs.csv");

            }

}*/

/*
    public static void main(String[] args) throws IOException {
        Parse p = new Parse();
        int sair = 0;
        Scanner in = new Scanner(System.in);
        do {
            menu();
            sair = in.nextInt();
            switch (sair) {
                case 1:
                    p.parse("src/logs.csv");
                    break;

                case 2:
                    p.parse("src/logs2.csv");
                    break;
/*
                case 3:
                    Utilizador u100 = new Utilizador("u100", "Hugo André Coelho Cardoso", 57.245117, 19.557358,"teste","1234",);
                    p.insereUtilizador(u100);
                    System.out.println("Utilizador inserido com sucesso.");
                    break;

                case 4:
                    Voluntario v100 = new Voluntario("v100", "Hugo Cardoso", 57.245117, 19.557358,10,"teste","1234");
                    p.insereVoluntario(v100);
                    System.out.println("Voluntário inserido com sucesso.");
                    break;

                case 5:
                    Transportadora t100 = new Transportadora("t100", "DHL", 62.14214, 32.123213, "123456789", 100, 0.74,"t100@","1234");
                    p.insereTransportadora(t100);
                    System.out.println("Transportadora inserida com sucesso.");
                    break;

                case 6:
                    Loja l100 = new Loja("l100", "Loja de teste", 62.14214, 32.123213,"l100@","1234");
                    p.insereLoja(l100);
                    System.out.println("Loja inserida com sucesso.");
                    break;

                case 7:
                    LinhaEncomenda le100 = new LinhaEncomenda("p100","Produto de teste",10.32, 1.53);
                    LinhaEncomenda le101 = new LinhaEncomenda("p101","Teste de produto",11.32, 2.53);
                    LinhaEncomenda le102 = new LinhaEncomenda("p102","Produto para testar",12.32, 3.53);
                    List<LinhaEncomenda> linhas = new ArrayList<>();
                    linhas.add(le100);
                    linhas.add(le101);
                    linhas.add(le102);
                    Encomenda e100 = new Encomenda("e100", "e100", "l100", 32.23, linhas, LocalDate.now());
                    p.insereEncomenda(e100);
                    System.out.println("Encomenda insirida com sucesso!");
                    break;

                case 8:
                    Aceite a100 = new Aceite("a100");
                    p.insereEncomendaAceite(a100);
                    System.out.println("Encomenda aceite insirida com sucesso!");
                    break;

                case 9:
                    System.out.println(p.utilizadores.toString());
                    break;

                case 10:
                    System.out.println(p.voluntarios.toString());
                    break;

                case 11:
                    System.out.println(p.transportadoras.toString());
                    break;

                case 12:
                    System.out.println(p.lojas.toString());
                    break;

                case 13:
                    System.out.println(p.encomendas.toString());
                    break;

                case 14:
                    System.out.println(p.encomendasAceites.toString());
                    break;

                case 15:
                    p.guardaEstado("teste");
                    break;

                case 16:
                    String email = p.criaEmail("u100");
                    System.out.println(email);


                case 17:
                    System.out.println(p.distanciaLojaTransportadora("l13","t26"));
                    break;
                default:
                    break;
            }
        }while (sair != -1);
        }
}*/


