import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class TesteGestaoAplicacao {

    public static void main(String[] args) {
        GestaoAplicacao gestao = new GestaoAplicacao();
        Parser p  = new Parser();
        p.parse(gestao);


        User u = new User();

        gestao.registarUtilizador("monteiro06", "umEmail", "umaPassword");


        Distribuidora d1 = new Distribuidora();
        Distribuidora d2 = new Distribuidora();
        Distribuidora d3 = new Distribuidora();
        Distribuidora d4 = new Distribuidora();

        Loja l1 = new Loja();
        l1.setCodigo("123");

        Encomenda e1 = new Encomenda();
        Encomenda e2 = new Encomenda();
        Encomenda e3 = new Encomenda();
        Encomenda e4 = new Encomenda();
        Encomenda e5 = new Encomenda();
        Encomenda e6 = new Encomenda();
        Encomenda e7 = new Encomenda();
        Encomenda e8 = new Encomenda();
        Encomenda e9 = new Encomenda();
        Encomenda e10 = new Encomenda();
        Encomenda e11 = new Encomenda();
        Encomenda e12 = new Encomenda();
        Encomenda e13 = new Encomenda();
        Encomenda e14 = new Encomenda();
        Encomenda e15 = new Encomenda();
        Encomenda e16 = new Encomenda();
        Encomenda e17 = new Encomenda();
        Encomenda e18 = new Encomenda();
        Encomenda e19 = new Encomenda();
        Encomenda e20 = new Encomenda();

        Estafeta v1 = new Estafeta();
        Estafeta v2 = new Estafeta();
        Estafeta v3 = new Estafeta();
        Estafeta v4 = new Estafeta();
        Estafeta v5 = new Estafeta();
        Estafeta v6 = new Estafeta();
        Estafeta v7 = new Estafeta();
        Estafeta v8 = new Estafeta();
        Estafeta v9 = new Estafeta();

        d1.setCodigo("Empresa 1");
        d2.setCodigo("Empresa 2");
        d3.setCodigo("Empresa 3");
        d4.setCodigo("Empresa 4");

        v1.setCodigo("Estafeta 1");
        v2.setCodigo("Estafeta 2");
        v3.setCodigo("Estafeta 3");
        v4.setCodigo("Estafeta 4");
        v5.setCodigo("Estafeta 5");
        v6.setCodigo("Estafeta 6");
        v7.setCodigo("Estafeta 7");
        v8.setCodigo("Estafeta 8");
        v9.setCodigo("Estafeta 9");

        d1.addEstafeta(v1);
        d1.addEstafeta(v2);
        d1.addEstafeta(v3);
        d2.addEstafeta(v4);
        d2.addEstafeta(v5);
        d3.addEstafeta(v6);
        d3.addEstafeta(v7);
        d3.addEstafeta(v8);
        d4.addEstafeta(v9);

        e1.setEstafeta(v1.getCodigo());    e1.setValor(12);             e1.setEstabelecimento(l1.getCodigo());   e1.setCodigo("1");     System.out.println(e1.getCodigo());
        e2.setEstafeta(v1.getCodigo());    e2.setValor(123);            e2.setEstabelecimento(l1.getCodigo());   e2.setCodigo("2");     System.out.println(e2.getCodigo());
        e3.setEstafeta(v1.getCodigo());    e3.setValor(43);             e3.setEstabelecimento(l1.getCodigo());   e3.setCodigo("3");     System.out.println(e3.getCodigo());
        e4.setEstafeta(v2.getCodigo());    e4.setValor(35);             e4.setEstabelecimento(l1.getCodigo());   e4.setCodigo("4");     System.out.println(e4.getCodigo());
        e5.setEstafeta(v1.getCodigo());    e5.setValor(232);            e5.setEstabelecimento(l1.getCodigo());   e5.setCodigo("5");     System.out.println(e5.getCodigo());
        e6.setEstafeta(v1.getCodigo());    e6.setValor(532);            e6.setEstabelecimento(l1.getCodigo());   e6.setCodigo("6");     System.out.println(e6.getCodigo());
        e7.setEstafeta(v1.getCodigo());    e7.setValor(321);            e7.setEstabelecimento(l1.getCodigo());   e7.setCodigo("7");     System.out.println(e7.getCodigo());
        e8.setEstafeta(v2.getCodigo());    e8.setValor(3);              e8.setEstabelecimento(l1.getCodigo());   e8.setCodigo("8");     System.out.println(e8.getCodigo());
        e9.setEstafeta(v2.getCodigo());    e9.setValor(3);              e9.setEstabelecimento(l1.getCodigo());   e9.setCodigo("9");     System.out.println(e9.getCodigo());
        e10.setEstafeta(v3.getCodigo());   e10.setValor(324.534);       e10.setEstabelecimento(l1.getCodigo());  e10.setCodigo("10");   System.out.println(e10.getCodigo());
        e11.setEstafeta(v4.getCodigo());   e11.setValor(44);            e11.setEstabelecimento(l1.getCodigo());  e11.setCodigo("11");   System.out.println(e11.getCodigo());
        e12.setEstafeta(v4.getCodigo());   e12.setValor(11);            e12.setEstabelecimento(l1.getCodigo());  e12.setCodigo("12");   System.out.println(e12.getCodigo());
        e13.setEstafeta(v4.getCodigo());   e13.setValor(12);            e13.setEstabelecimento(l1.getCodigo());  e13.setCodigo("13");   System.out.println(e13.getCodigo());
        e14.setEstafeta(v6.getCodigo());   e14.setValor(67.534);        e14.setEstabelecimento(l1.getCodigo());  e14.setCodigo("14");   System.out.println(e14.getCodigo());
        e15.setEstafeta(v6.getCodigo());   e15.setValor(765.543);       e15.setEstabelecimento(l1.getCodigo());  e15.setCodigo("15");   System.out.println(e15.getCodigo());
        e16.setEstafeta(v4.getCodigo());   e16.setValor(2.312);         e16.setEstabelecimento(l1.getCodigo());  e16.setCodigo("16");   System.out.println(e16.getCodigo());
        e17.setEstafeta(v2.getCodigo());   e17.setValor(33.123);        e17.setEstabelecimento(l1.getCodigo());  e17.setCodigo("17");   System.out.println(e17.getCodigo());
        e18.setEstafeta(v3.getCodigo());   e18.setValor(12.12);         e18.setEstabelecimento(l1.getCodigo());  e18.setCodigo("18");   System.out.println(e18.getCodigo());
        e19.setEstafeta(v2.getCodigo());   e19.setValor(23);            e19.setEstabelecimento(l1.getCodigo());  e19.setCodigo("19");   System.out.println(e19.getCodigo());
        e20.setEstafeta(v3.getCodigo());   e20.setValor(21);            e20.setEstabelecimento(l1.getCodigo());  e20.setCodigo("20");   System.out.println(e20.getCodigo());

        try {
            gestao.registarLoja(l1);
            gestao.registarEmpresa(d1);
            gestao.registarEmpresa(d2);
            gestao.registarEmpresa(d3);
            gestao.registarEmpresa(d4);
        } catch (EntidadeRepetidaException e) {
            e.getMessage();
        }



        try {
            gestao.registarEstafeta(v1);
            gestao.registarEstafeta(v2);
            gestao.registarEstafeta(v3);
            gestao.registarEstafeta(v4);
            gestao.registarEstafeta(v5);
            gestao.registarEstafeta(v6);
            gestao.registarEstafeta(v7);
            gestao.registarEstafeta(v8);
            gestao.registarEstafeta(v9);
        }catch (EntidadeRepetidaException e){
            e.getMessage();
        }

        gestao.registarEncomenda(e1);
        gestao.registarEncomenda(e2);
        gestao.registarEncomenda(e3);
        gestao.registarEncomenda(e4);
        gestao.registarEncomenda(e5);
        gestao.registarEncomenda(e6);
        gestao.registarEncomenda(e7);
        gestao.registarEncomenda(e8);
        gestao.registarEncomenda(e9);
        gestao.registarEncomenda(e10);
        gestao.registarEncomenda(e11);
        gestao.registarEncomenda(e12);
        gestao.registarEncomenda(e13);
        gestao.registarEncomenda(e14);
        gestao.registarEncomenda(e15);
        gestao.registarEncomenda(e16);
        gestao.registarEncomenda(e17);
        gestao.registarEncomenda(e18);
        gestao.registarEncomenda(e19);
        gestao.registarEncomenda(e20);
        System.out.println("Encomenda com código " + e1.getCodigo());


        /**PROCESSO DE UMA ENCOMENDA**/
        ;
        Produto p1 = new Produto("p124", "Laptop", 2, 405.30, 3-34, false);
        Loja l = new Loja("l0","Fnac", new GPS(8,8), new ArrayList<>(), new HashMap<>());
        try {
            gestao.registarLoja(l);
        } catch (EntidadeRepetidaException e) {
            e.printStackTrace();
        }

        Cliente c = new Cliente("c0", "umCliente",new GPS(0,0));
        Estafeta e = new Estafeta("e0", "umEstafeta", new GPS(5,20), 30, false, true);
        l.adicionaProduto(p1);

        System.out.println(gestao.getLojas());
        Map<String, Integer> produtos = new HashMap<String, Integer>();
/*
        Encomenda enc = gestao.criaEncomenda(c.getCodigo(), l.getCodigo(),produtos);

        List<String> encomendas = gestao.getPertoEstafeta(e.getCodigo(), "Encomenda");
        System.out.println(encomendas);
        //Estafeta escolheu a primeira encomenda
        if(encomendas.size() > 0) {
            String encomenda = encomendas.get(0);

            //Ao alterar o estado de uma encomenda pedida, esta vai para a loja
            gestao.alterarEstadoEncomenda(e.getCodigo(), encomenda, "Estafeta");

            //Loja verifica que encomendas tem de preparar
            gestao.getEncomendas("l", "Loja", 1);

        }


*/


    }
}
