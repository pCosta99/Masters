import java.util.*;

public class Main {

    public static void main(String[] args) {

        Scanner scan = new Scanner(System.in);

        Parse parser = Parse.leFicheiro();
        parser.parse(parser);

        parser.parseProdutos(parser.getEncomendas());

        for (Encomenda e : parser.getEncomendas().values()) {
            for (String a : parser.getAceite().keySet()) {
                if (parser.getEncomendas().containsKey(a)) {
                    e.setUserEntrega("default");
                }
            }
        }

        parser.adiciona_encomendas_aceites();
        parser.add_hist_user();
        parser.calcula_KM();
        parser.historico_Lojas();

        /**
         * Menu inicial
         */
        String user = "0";
        int opcao;
        Menus menus = new Menus();
        do {
            System.out.println("Escolha a opção desejada:");
            System.out.println("\t1 - Login");
            System.out.println("\t2 - Registo");
            System.out.println("\t3 - Top 10 Utilizadores");
            System.out.println("\t4 - Top 10 Transportadoras");
            System.out.println("\t0 - Sair");
            String opcaoIni = scan.next();
            switch (opcaoIni) {
                case "1":
                    Parse login = new Parse();
                    System.out.println("Introduza o seu id de utilizador");
                    String idUser = scan.next();
                    System.out.println("Introduza a sua password");
                    String password = scan.next();
                    Utilizador u = login.loginUtilizador(parser, idUser, password);
                    if (u == null) {
                        System.out.println("login invalido");
                    }else {
                        if (parser.getVoluntarios().containsKey(idUser)) {
                            Voluntario v = parser.getVoluntarios().get(idUser).clone();
                            menus.menuVoluntario(v, parser);
                        } else if (parser.getTransportadoras().containsKey(idUser)) {
                            Transportadora v = parser.getTransportadoras().get(idUser).clone();
                            menus.menuTransportadora(v, parser);
                        } else if (parser.getLojas().containsKey(idUser)) {
                            Loja v = parser.getLojas().get(idUser).clone();
                            menus.menuLoja(v, parser);
                        }else {
                            menus.menuUtilizador(u, parser);
                        }
                    }
                    break;
                case "2":
                    do {
                        opcao = menus.menuRegisto();
                        switch (opcao){
                            case 1:
                                menus.registoUtilizador(parser);
                                opcao=0;
                                break;
                            case 2:
                                menus.registoVoluntario(parser);
                                opcao=0;
                                break;
                            case 3:
                                menus.registoTransportadora(parser);
                                opcao=0;
                                break;
                            case 4:
                                menus.registoLoja(parser);
                                opcao=0;
                                break;
                            case 0:
                                System.out.println("Logout");
                                break;
                        }
                    }while (opcao!=0);
                    break;
                case "3":
                    for (Utilizador e : parser.top10()){
                        System.out.println(e.getNome() + ": " + e.getUser_historico().size());
                    }
                    System.out.println("\t");
                    break;
                case "4":
                    for (Transportadora t : parser.top10_Tranportadoras()){
                        System.out.println(t.getNome() + ": " + t.getKm_Percorridos());
                    }
                    System.out.println("\t");
                    break;
                case "0":
                    parser.gravaEmFicheiro();
                    System.exit(0);
                    break;
                default:
                    System.out.println("\nOpção inválida\n\n");
                    break;
            }
        } while (user.equals("0"));
    }
}