/*import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import java.util.List;
import java.util.ArrayList;

/**
 * A classe de teste EncomendaTest.
 *
public class EncomendaTest{
    private Projeto.Projeto.Encomenda.Encomenda enc;
    /**
     * Construtor default para a classe de teste EncomendaTest
     *
    public EncomendaTest(){
    }

    /**
     * Define a .
     *
     * Chamado antes de cada método de caso de teste.
     *
    @Before
    public void setUp(){
        List<Projeto.Encomenda.LinhaDeEncomenda> lista = new ArrayList<>();
        this.enc = new Projeto.Projeto.Encomenda.Encomenda("000001", "Continente", "Joao",  2, false, lista);
        Projeto.Encomenda.LinhaDeEncomenda l1 = new Projeto.Encomenda.LinhaDeEncomenda("1", "sem descriçao", 4, 2, false);
        Projeto.Encomenda.LinhaDeEncomenda l2 = new Projeto.Encomenda.LinhaDeEncomenda("2", "sem descriçao", 3, 20, false);
        Projeto.Encomenda.LinhaDeEncomenda l3 = new Projeto.Encomenda.LinhaDeEncomenda("3", "sem descriçao", 10, 0.2f, false);
        enc.insereLinhaEnc(l1);
        enc.insereLinhaEnc(l2);
        enc.insereLinhaEnc(l3);
    }

    /**
     * Tears down the test fixture.
     *
     * Chamado após cada método de teste de caso.
     *
    @After
    public void tearDown(){
    }
    
    @Test
    public void testeContainsMed(){
        assertFalse("Esta a detetar uma linha verdadeira e sao todas falsas", enc.containsMed());
        Projeto.Encomenda.LinhaDeEncomenda l = new Projeto.Encomenda.LinhaDeEncomenda("4", "sem descriçao", 1, 200, true);
        enc.insereLinhaEnc(l);
        assertTrue("Nao esta a detetar a linha verdadeira", enc.containsMed());
    }
    
    @Test
    public void testeCalculaPrecoTotal(){
        float esperado = 4 * 2 + 3 * 20 + 10 * 0.2f;
        float obtido = enc.calculaPrecoTotal();
        assertEquals("Preço total mal calculado", esperado, obtido,0.001f);
    }
}
*/