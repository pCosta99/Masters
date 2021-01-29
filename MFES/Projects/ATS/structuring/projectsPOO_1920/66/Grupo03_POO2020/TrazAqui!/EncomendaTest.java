import java.time.LocalDateTime;
import java.util.Map;
import java.util.TreeMap;
import static org.junit.jupiter.api.Assertions.*;

class EncomendaTest extends Encomenda {
    LinhaEncomenda l1,l2,l3;
    Encomenda e;

    @org.junit.jupiter.api.BeforeEach
    void setUp() {
        l1 = new LinhaEncomenda("p12","kermit porta-chaves",2,2.5);
        l2 = new LinhaEncomenda("p10","kermit peluche",5,7);
        l3 = new LinhaEncomenda("p11","kermit caneca",3,4);
        TreeMap<String,LinhaEncomenda> a = new TreeMap<>();
        a.put(l1.getCodProd(),l1);
        a.put(l2.getCodProd(),l2);
        a.put(l3.getCodProd(),l3);
        LocalDateTime n = LocalDateTime.now();
        e = new Encomenda("e45",true,"u70","l54",3.0,a,n,n,n,n,n,n,"Pendente");
    }

    @org.junit.jupiter.api.AfterEach
    void tearDown() {
    }

    @org.junit.jupiter.api.Test
    void testCalculaPrecoTotal() {
        double valorEsperado = l1.getPrecoLinha() + l2.getPrecoLinha() +l3.getPrecoLinha();
        double valorObtido = e.calculaPrecoTotal();
        assertEquals(valorEsperado,valorObtido,0.1);
    }

    @org.junit.jupiter.api.Test
    void testAdicionaLinha() {
        LinhaEncomenda l4 = new LinhaEncomenda("p13","kermit caneta",1,1.0);
        assertFalse(e.getLinha().containsKey(l4.getCodProd()), "Linha não existente presente na encomenda");
        e.adicionaLinha(l4);
        assertTrue(e.getLinha().containsKey(l4.getCodProd()),"Linha existente não consta na encomenda");
    }

    @org.junit.jupiter.api.Test
    void testAceitaEncomenda() {
        assertNotEquals("Entregue", e.getEstado(), "Encomenda com estado errado");
        e.aceitaEncomenda();
        assertEquals("Entregue", e.getEstado(), "Encomenda com estado errado");
    }

    @org.junit.jupiter.api.Test
    void testRejeitaEncomenda() {
        assertNotEquals("Entregue", e.getEstado(), "Encomenda com estado errado");
        e.rejeitaEncomenda();
        assertEquals("Cancelada", e.getEstado(), "Encomenda com estado errado");
    }
}