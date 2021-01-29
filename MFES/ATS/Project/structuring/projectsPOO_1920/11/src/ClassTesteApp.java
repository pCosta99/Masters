import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class ClassTesteApp {

    public static void main(String args[]){
        /**
        Voluntario pedro = new Voluntario("Pedro","1234",new Localizacao(),
                                        true,false,new HashMap<String,Integer>(),
                                        new HashMap<String,List<Encomenda>>(),23,"non-binary");
        Empresa sonae = new Empresa("Sonae","123",new Localizacao(),
                                    true,true, new HashMap<String,Integer>(),
                                    new HashMap<String,List<Encomenda>>(),10.0,5.0);
        Utilizador rui = new Utilizador("Rui","12",new Localizacao(2.2,3.3),30,"masculino");

        Loja bolama = new Loja("Bolama","12345",new Localizacao(12.2,13.2),
                                        new ArrayList<Utilizador>());

        pedro.addClassificacao("Joel",3);
        pedro.addClassificacao("Manuel",5);
        sonae.addClassificacao("Pedro",1);
        System.out.println(pedro.toString());
        System.out.println(sonae.toString());
        System.out.println(rui.toString());
        System.out.println(bolama.toString());
        pedro.removeClassificacao("Joel");
        System.out.println(pedro.toString() + "A Classificação é: " + pedro.getClassificacao("Manuel"));

        Encomenda e = new Encomenda("Fernando","Lidl", LocalDateTime.now(),2.2,new ArrayList<String>(),false,false);
        Encomenda i = new Encomenda("Fernando","Lidl", LocalDateTime.now(),2.2,new ArrayList<String>(),false,false);
        System.out.println(e.toString());
        System.out.println(e.equals(i));
        List<String> l = new ArrayList<>();
        l.add("Queijo");
        l.add("Pão");
        i.setProdutos(l);
        System.out.println(i.toString());
        System.out.println(e.equals(i));
        l.remove(0);
        l.remove(0);
        System.out.println(l.toString());
        System.out.println(i.toString());
        System.out.println(e.equals(i));
        i.setProdutos(l);
        System.out.println(e.equals(i));

        EncomendasDB edb = new EncomendasDB();
        Encomenda a = new Encomenda("Fernando","Farmacia", LocalDateTime.now(),2.1,new ArrayList<String>(),true,true);
        Encomenda b = new Encomenda("Fernando","Lidl", LocalDateTime.now(),2.2,new ArrayList<String>(),false,false);
        Encomenda c = new Encomenda("Fernando","Lidl", LocalDateTime.now(),2.2,new ArrayList<String>(),false,false);
        Encomenda d = new Encomenda("Quim","Lidl", LocalDateTime.now(),2.2,new ArrayList<String>(),false,false);
        edb.addEncomenda(a);
        edb.addEncomenda(b);
        edb.addEncomenda(c);
        edb.addEncomenda(d);
        System.out.println("_____BASE DE DADOS: ENCOMENDAS_____\n" + edb.toString());
        edb.removeEncomenda(a);
        edb.addUser("Manuel");
        System.out.println("_____BASE DE DADOS: ENCOMENDAS_____\n" + edb.toString());
        edb.removeUser("Manuel");
         */
         /**
         * teste que verifica se é possivel alterar o list do map a partir da
         * classe Encomenda(no map tem de aparecer "Lidl" em vez de "Continente")
         */
        /**
         d.setLoja("Continente");
        System.out.println("_____BASE DE DADOS: ENCOMENDAS_____\n" + edb.toString());
        */
    }
}
