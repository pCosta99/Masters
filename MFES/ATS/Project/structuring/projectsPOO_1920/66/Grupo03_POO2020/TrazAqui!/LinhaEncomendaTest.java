import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class LinhaEncomendaTest extends LinhaEncomenda {
    LinhaEncomenda l;

    @BeforeEach
    void setUp() {
        l = new LinhaEncomenda("p12","kermit porta-chaves",2,2.5);
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void testGetPrecoLinha() {
        double valorEsperado = l.getQuantidade()*l.getValorUnitario();
        double valorObtido = l.getPrecoLinha();
        assertEquals(valorEsperado,valorObtido,0.1);
    }
}