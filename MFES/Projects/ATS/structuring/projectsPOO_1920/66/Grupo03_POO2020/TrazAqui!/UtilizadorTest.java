import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.*;

class UtilizadorTest extends Utilizador {
    Utilizador u;
    Ponto p;
    EncDistr ed1;
    Encomenda e1;
    LinhaEncomenda l1,l2,l3;
    ArrayList<EncDistr> encsAv;
    TreeMap<String,EncDistr> encs;


    @org.junit.jupiter.api.BeforeEach
    void setUp() {
        l1 = new LinhaEncomenda("p12","kermit porta-chaves",2,2.5);
        l2 = new LinhaEncomenda("p10","kermit peluche",5,7);
        l3 = new LinhaEncomenda("p11","kermit caneca",3,4);
        TreeMap<String,LinhaEncomenda> linha = new TreeMap<>();
        linha.put(l1.getCodProd(),l1);
        linha.put(l2.getCodProd(),l2);
        linha.put(l3.getCodProd(),l3);
        LocalDateTime n = LocalDateTime.now();
        e1 = new Encomenda("e45",true,"u0","l54",3.0,linha,n,n,n,n,n,n,"Entregue");
        ed1 = new EncDistr(e1,"t0");
        encs = new TreeMap<>();
        encsAv = new ArrayList<>();
        encs.put(e1.getCodEncomenda(),ed1);
        encsAv.add(ed1);
        p = new Ponto(1,1) ;
        u = new Utilizador("u0","João", p, encs, "p1", "u0@kermit.com",encsAv);
    }

    @org.junit.jupiter.api.AfterEach
    void tearDown() {
    }

    @org.junit.jupiter.api.Test
    void testGetMyEncsAvSize() {
        int valorEsperado = encsAv.size();
        int valorObtido = u.getMyEncsAvSize();
        assertEquals(valorEsperado,valorObtido,0.1,"Encomendas por avaliar com tamanho errado");
    }

    @org.junit.jupiter.api.Test
    void testAdicionaMyEncAv() {
        LocalDateTime n = LocalDateTime.now();
        e1 = new Encomenda("e45", true, "u0", "l54", 3.0, new TreeMap<>(), n, n, n, n, n, n, "Entregue");
        ed1 = new EncDistr(e1, "t0");
        assertFalse(u.getMyEncsAv().contains(ed1), "Encomenda inexistente presente em EncsAv");
        u.adicionaMyEncAv(ed1);
        assertTrue(u.getMyEncsAv().contains(ed1), "Encomenda existente não presente em EncsAv");
    }

    @org.junit.jupiter.api.Test
    void testRemoveMyEncAv() {
        LocalDateTime n = LocalDateTime.now();
        e1 = new Encomenda("e45", true, "u0", "l54", 3.0, new TreeMap<>(), n, n, n, n, n, n, "Entregue");
        ed1 = new EncDistr(e1, "t0");
        List<EncDistr> l = u.getMyEncsAv();
        l.add(ed1);
        u.setMyEncsAv(l);
        assertTrue(u.getMyEncsAv().contains(ed1),"Encomenda existente não presente em EncsAv");
        u.removeMyEncAv(ed1);
        assertFalse(u.getMyEncsAv().contains(ed1), "Encomenda inexistente presente em EncsAv");

    }
}