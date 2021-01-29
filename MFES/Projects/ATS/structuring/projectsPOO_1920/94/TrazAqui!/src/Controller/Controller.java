package Controller;

import Models.Encomenda;
import Models.FileLoaders;
import Models.Produto;
import Models.Sistema;
import View.View;
import View.ViewError;
import View.ViewMenu;

import java.io.IOException;
import java.security.InvalidKeyException;
import java.security.InvalidParameterException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static View.View.MenuOption.*;

/**
 * Classe que implementa o controlador do projecto
 */
public class Controller {
    private static final Scanner sc = new Scanner(System.in);
    private static final FileLoaders f = new FileLoaders();
    private static final String datafile = "estado.data";
    private LocalDateTime clock1;

    public Controller() {
    }

    /**
     * Passa o input do utilizador (sim/não) a um boolean (true/false)
     *
     * @return Boolean true(sim) ou false(não)
     */
    public static boolean yes_or_no() {
        switch (sc.nextLine()) {
            case "s", "S", "sim", "SIM", "Sim", "y", "Y" -> {
                return true;
            }
            default -> {
                return false;
            }
        }
    }

    public void app(Sistema s) {

        clock1 = LocalDateTime.now();
        while (true) {

            View.clrscr();
            ViewMenu.mainmenu();

            switch (sc.nextLine()) {
                case "1" -> {
                    try {
                        s = f.carregaEstado(datafile);
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
                case "2" -> {
                    try {
                        f.guardaEstado(datafile, s);
                    } catch (IOException e) {
                        ViewError.show(e);
                    }
                }
                case "3" -> registermenu(s);

                case "4" -> statsmenu(s);

                case "5" -> usermenu(s);

                case "6" -> agentmenu(s);

                case "0" -> {
                    return;
                }
                default -> ViewError.show(new InvalidKeyException("Opção inválida."));
            }
            View.show(Enter);
            sc.nextLine();
        }

    }

    /**
     * Menu que permite registar novos Utilizadores/Voluntarios/Empresas/Lojas no sistema
     *
     * @param s Sistema onde os dados serão inseridos
     */
    public void registermenu(Sistema s) {
        String nome = null;
        String cod = null;
        double x = 0;
        double y = 0;

        View.clrscr();
        ViewMenu.register();
        String op = sc.nextLine();

        switch (op){
            case "1","2","3","4" -> {
                View.show(Nome);
                nome = sc.nextLine();
                View.show(Code);
                cod = sc.nextLine();
                View.show(X);
                try {
                    x = Double.parseDouble(sc.nextLine());
                    View.show(Y);
                    y = Double.parseDouble(sc.nextLine());
                } catch (NullPointerException | NumberFormatException e) {
                    ViewError.show(new IllegalArgumentException("Coordenada Inválida.", e));
                    return;
                }
            }
        }

        switch (op) {
            case "1" -> {
                View.show(Email);
                String email = sc.nextLine();
                View.show(Pass);
                String pass = sc.nextLine();

                try {
                    s.registarUtilizador(cod, email, pass, nome, x, y);
                } catch (InvalidParameterException e) {
                    ViewError.show(e);
                }
            }
            case "2" -> {
                View.show(Raio);
                double r;
                try {
                    r = Double.parseDouble(sc.nextLine());
                } catch (NullPointerException | NumberFormatException e) {
                    ViewError.show(new IllegalArgumentException("Raio Inválido."));
                    return;
                }
                View.show(AceitaEncomendaMedica);
                boolean med = yes_or_no();
                View.show(Disponibilidade);
                boolean d = yes_or_no();

                try {
                    s.registarVoluntario(cod, nome, x, y, r, med, d);
                } catch (InvalidParameterException e) {
                    ViewError.show(e);
                }
            }
            case "3" -> {
                View.show(NIF);
                String nif = sc.nextLine();
                View.show(Raio);
                double r;
                try {
                    r = Double.parseDouble(sc.nextLine());
                } catch (NullPointerException | NumberFormatException e) {
                    ViewError.show(new IllegalArgumentException("Raio Inválido."));
                    return;
                }
                View.show(Tax);
                double taxa;
                try {
                    taxa = Double.parseDouble(sc.nextLine());
                } catch (NullPointerException | NumberFormatException e) {
                    ViewError.show(new IllegalArgumentException("Taxa Inválida."));
                    return;
                }
                View.show(Cap);
                int cap;
                try {
                    cap = Integer.parseInt(sc.nextLine());
                } catch (NullPointerException | NumberFormatException e) {
                    ViewError.show(new IllegalArgumentException("Capacidade Inválida."));
                    return;
                }
                View.show(AceitaEncomendaMedica);
                boolean med = yes_or_no();
                View.show(Disponibilidade);
                boolean d = yes_or_no();

                try {
                    s.registarEmpresa(cod, nome, x, y, r, taxa, cap, nif, med, d);
                } catch (InvalidParameterException e) {
                    ViewError.show(e);
                }
            }
            case "4" -> {
                View.show(Filas);
                boolean filas = yes_or_no();
                View.show(TiposLoja);
                String tipo = sc.next();

                try {
                    s.registarLoja(cod, nome, x, y, filas, tipo);
                } catch (InvalidParameterException e) {
                    ViewError.show(e);
                }
            }
            case "0" -> {
            }
            default -> ViewError.show(new InvalidKeyException("Opção inválida."));
        }
    }

    /**
     * Menu com opções para devolver alguns dados estatísticos
     *
     * @param s Sistema a partir do qual se recolhem os dados
     */
    public void statsmenu(Sistema s) {
        View.clrscr();
        ViewMenu.stats();
        switch (sc.nextLine()) {
            case "1" -> { //por default, devolve a faturaçao calculada desde o inicio do programa até agora
                View.show(Cod_Emp);
                String code = sc.nextLine();

                LocalDateTime clock2 = LocalDateTime.now();
                s.fatemp(code, clock1, clock2);
            }
            case "2" -> {
                View.show(Max);
                int x;
                try {
                    x = Integer.parseInt(sc.nextLine());
                } catch (NullPointerException | NumberFormatException e) {
                    ViewError.show(new IllegalArgumentException("Valor Inválido."));
                    return;
                }
                s.TopXUsers(x);
            }
            case "3" -> {
                View.show(Max);
                int x;
                try {
                    x = Integer.parseInt(sc.nextLine());
                } catch (NullPointerException | NumberFormatException e) {
                    ViewError.show(new IllegalArgumentException("Valor Inválido."));
                    return;
                }
                s.TopXEmpresas(x);
            }
            case "0" -> {
            }
            default -> ViewError.show(new InvalidKeyException("Opção inválida."));
        }
    }


    /**
     * Menu que permite o login do Utilizador através das suas credenciais, e a sua interação com o sistema.
     *
     * @param s Sistema com o qual o utilizador interage
     */
    public void usermenu(Sistema s) {

        View.show(Email);
        String email = sc.nextLine();
        View.show(Pass);
        String pass = sc.nextLine();
        View.clrscr();
        try {
            if (s.loginUser(email, pass)) {
                while (true) {

                    ViewMenu.user();

                    switch (sc.nextLine()) {
                        case "1" -> {
                            if (s.getUserStatus(email)) View.show(EncComing);
                            else shopping_cart(s, email);
                        }

                        case "2" -> { // lista de encomendas prontas a entregar
                            View.show(Enc_Prontas);
                            s.encomendasProntas();
                        }
                        case "3" -> { // ve historico de encs entregues por agent (falta por periodo de tempo)
                            View.show(Cod_Agente);
                            String agente = sc.nextLine();

                            View.show(Enc_Entregues);
                            s.historicoEncs(agente, email);
                        }
                        case "4" -> { // classifica um agente
                            View.show(Cod_Agente);
                            String agente = sc.nextLine();
                            View.show(Classif);
                            try {
                                double rating = Double.parseDouble(sc.nextLine());
                                s.rate_agent(rating, agente);
                            } catch (NullPointerException | NumberFormatException e) {
                                ViewError.show(new IllegalArgumentException("Classificação inválida.", e));
                            } catch (InvalidParameterException e) {
                                ViewError.show(e);
                            }
                        }

                        case "5" -> {
                            if (!s.getUserStatus(email)) View.show(No_Enc);
                            else {
                                s.remove_encomenda(email,LocalDateTime.now());
                            }
                        }

                        case "0" -> {
                            return;
                        }
                        default -> ViewError.show(new InvalidKeyException("Opção inválida."));
                    }
                    View.show(Enter);
                    sc.nextLine();
                    View.clrscr();
                }
            }
        } catch (InvalidParameterException e) {
            ViewError.show(e);
        }
    }

    /**
     * Menu que permite o login de um Agente (Voluntário ou Empresa) através do seu código, e a sua interação com o sistema.
     *
     * @param s Sistema com o qual o agente interage
     */
    public void agentmenu(Sistema s) {
        View.show(Cod_Agente);
        String cod = sc.nextLine();

        try {
            if (s.loginAgent(cod)) {
                while (true) {

                    s.verClassif(cod);
                    View.aceitaEncs(s.getDispAgente(cod)); // sinaliza se pode ou nao recolher mais encomendas
                    ViewMenu.agent();

                    switch (sc.nextLine()) {
                        case "1" -> { // transportar enc
                            View.show(Code);
                            String enc = sc.nextLine();
                            try {
                                s.agenteAceitarEnc(cod, enc);
                            } catch (InvalidParameterException e) {
                                ViewError.fullShow(e);
                            }
                        }
                        case "2" -> { // altera manualmente a disponibilidade de um agente
                            View.show(Disponibilidade);
                            s.setDispAgente(cod, yes_or_no());
                        }
                        case "3" -> { // ver encomendas pendentes
                            View.show(Enc_Pendentes);
                            s.availableEncs();
                        }
                        case "4" -> { // ver historico
                            View.show(Enc_Entregues);
                            s.historicoEncs(cod, null);
                        }
                        case "0" -> {
                            return;
                        }
                        default -> ViewError.show(new InvalidKeyException("Opção inválida."));
                    }
                    View.show(Enter);
                    sc.nextLine();
                    View.clrscr();
                }
            }
        } catch (InvalidParameterException e) {
            ViewError.show(e);
        }
    }

    /**
     * @param s Sistema
     * @param email String com o email do utilizador
     */
    public void shopping_cart(Sistema s, String email) { // pedir nova encomenda (wip)
        View.showList(s.getLojas());
        View.show(Cod_Loja);

        String loja = "";

        while (!s.existeLoja(loja)) {
            loja = sc.nextLine();
        }

        List<Produto> produtos = s.getProductsFromLoja(loja);
        Map<String, Integer> carrinho_compras = new HashMap<>();
        String produto;

        View.showList(s.getProductsStringFromLoja(loja));
        while (true){
            View.showMap(carrinho_compras, p -> "\n[ Produto: " + p, q -> " | Quantidade: " + q + "]");
            ViewMenu.carrinho();
            switch (sc.nextLine()){
                case "1" -> { //adiciona produto
                    View.show(Add_Prod);
                    produto=sc.nextLine();
                    if (Produto.is_valid_product(produto, produtos)) {
                        try {
                            int t0 = carrinho_compras.get(produto);
                            carrinho_compras.put(produto, ++t0);
                        } catch (NullPointerException e) {
                            carrinho_compras.put(produto, 1);
                        }
                    }
                    else View.show(Inv_Prod);
                }
                case "2" -> { //remove produto
                    View.show(Rem_Prod);
                    produto=sc.nextLine();
                    if(Produto.is_valid_product(produto, produtos)) {
                        try{
                            int t0 = carrinho_compras.get(produto);
                            if (t0>1) carrinho_compras.put(produto, --t0);
                            else if (t0==1) carrinho_compras.remove(produto);
                        }catch (NullPointerException e) {
                            View.show(Inv_Prod);
                        }
                    }
                    else View.show(Inv_Prod);
                }
                case "3" -> View.showList(s.getProductsStringFromLoja(loja));

                case "4" -> { //conclui encomenda
                    Set<Produto> res = carrinho_compras.entrySet()
                            .stream()
                            .map(a -> new Produto(Objects.requireNonNull(Produto.get_by_name(a.getKey(), produtos)), a.getValue()))
                            .collect(Collectors.toSet());

                    s.userPedirEnc(email, loja, res);
                    return;
                }
                case "0" -> {
                    return;
                }
                default -> ViewError.show(new InvalidKeyException("Opção inválida."));
            }
        }
/*
        while (!produto.equals("Pronto")) {
            View.showMap(carrinho_compras, p -> "[ Produto: " + p, q -> " | Quantidade: " + q + "]\n");
            produto = sc.nextLine();
            try {
                if (produto.equals("remover")) {
                    View.remove_produto();
                    produto=sc.nextLine();
                    if(Produto.is_valid_product(produto, produtos)) {
                        int t0 = carrinho_compras.get(produto);
                        if (t0>1) carrinho_compras.put(produto, --t0);
                        else if (t0==0) View.no_such_product();
                        else if (t0==1) carrinho_compras.remove(produto);
                    }
                }
                else if (Produto.is_valid_product(produto, produtos)) {
                    try {
                        int t0 = carrinho_compras.get(produto);
                        carrinho_compras.put(produto, ++t0);
                    } catch (NullPointerException e) {
                        carrinho_compras.put(produto, 1);
                    }
                }
            } catch (Exception ignored) {
            }
        }

        Set<Produto> res = carrinho_compras.entrySet()
                .stream()
                .map(a -> new Produto(Objects.requireNonNull(Produto.get_by_name(a.getKey(), produtos)), a.getValue()))
                .collect(Collectors.toSet());

        s.userPedirEnc(email, loja, res); */

    }
}
