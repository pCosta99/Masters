import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.*;

class RegistoTULVTest extends RegistoTULV {
    RegistoTULV r;
    Ponto p;
    EncDistr ed1;
    Encomenda e1;
    LinhaEncomenda l1,l2,l3;
    TreeMap<String,EncDistr> encs;
    TreeMap<String,LinhaEncomenda> linha;
    LocalDateTime n;

    @BeforeEach
    void setUp() {
        l1 = new LinhaEncomenda("p12","kermit porta-chaves",2,2.5);
        l2 = new LinhaEncomenda("p10","kermit peluche",5,7);
        l3 = new LinhaEncomenda("p11","kermit caneca",3,4);
        linha = new TreeMap<>();
        linha.put(l1.getCodProd(),l1);
        linha.put(l2.getCodProd(),l2);
        linha.put(l3.getCodProd(),l3);
        n = LocalDateTime.now();
        e1 = new Encomenda("e45",true,"u0","l54",3.0,linha,n,n,n,n,n,n,"Entregue");
        ed1 = new EncDistr(e1,"t0");
        encs = new TreeMap<>();
        encs.put(e1.getCodEncomenda(),ed1);
        p = new Ponto(1,1) ;
        r = new RegistoTULV("u0","João", p, "p1",encs, "u0@kermit.com");
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void testInserirEncomenda() {
        EncDistr ed = new EncDistr(new Encomenda(),"t1");
        assertFalse(r.getEncomendas().containsValue(ed),"Encomenda não existente presente nas encomendas");
        r.inserirEncomenda(ed);
        assertTrue(r.getEncomendas().containsValue(ed),"Encomenda existente não presente nas encomendas");

    }

    @Test
    void testGetEncsSize() {
        int valorEsperado = encs.size();
        int valorObtido = r.getEncsSize();
        assertEquals(valorEsperado, valorObtido, 0.1, "Cálculo do número de encomendas incorreto");
    }

    @Test
    void testGetEncPendentes() {
        List<Encomenda> valorEsperado = new ArrayList<>();
        List<Encomenda> valorObtido = r.getEncPendentes();
        assertEquals(valorEsperado, valorObtido, "Lista de encomendas pendentes incorreto");
    }

    @Test
    void testAlteraEstadoEncomenda() {
        Encomenda e = new Encomenda("e45",true,"u0","l54",3.0,linha,n,n,n,n,n,n,"Cancelada");
        EncDistr ed = new EncDistr(e,"t0");
        assertFalse(r.getEncomendas().containsValue(ed) ,"Alteração estado encomenda incorreto");
        e1.rejeitaEncomenda();
        r.alteraEstadoEncomenda(e1);
        assertTrue(r.getEncomendas().containsValue(ed) ,"Alteração estado encomenda incorreto");

    }
}