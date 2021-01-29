import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class PontoTest extends Ponto {
    double x,y,z,w;
    Ponto p,q;

    @BeforeEach
    void setUp() {
        x = 2; y = 3;
        p = new Ponto(x,y);
        z = 6; w = 8;
        q = new Ponto(z,w);
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void testDistancia() {
        double valorEsperado = Math.sqrt(Math.pow(x-z,2) + Math.pow(y-w,2));
        double valorObtido = p.distancia(q);
        assertEquals(valorEsperado,valorObtido,0.1);
    }
}