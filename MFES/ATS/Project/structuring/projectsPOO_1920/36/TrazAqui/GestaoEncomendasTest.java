import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * 
 *
 * @author  (Artur Drohobytskyy)
 * @version (1.0)
 */
public class GestaoEncomendasTest {
    
    private Produto p1, p2, p3, p4, p5;
    private Encomenda e1, e2, e3, e4, e5;
    private GestaoEncomendas ge;
    
    private Voluntario v;
    private Loja l;
    
    /**
     * Default constructor for test class GestaoEncomendasTest
     */
    public GestaoEncomendasTest() {
        
    }

    /**
     * Sets up the test fixture.
     *
     * Called before every test case method.
     */
    @Before
    public void setUp() {
       p1 = new Produto("p1", "desc1", 10, 20);
       p2 = new Produto("p2", "desc2", 15, 30);
       p3 = new Produto("p3", "desc3", 20, 40);
       
       List<Produto> produtos = new ArrayList<>();
       produtos.add(p1);
       produtos.add(p2);
       produtos.add(p3);
       
       e1 = new EncomendaNormal("e1", "u1", "l1", 10, produtos, EstadoEncomenda.SOLICITADO);
       e2 = new EncomendaNormal("e2", "u1", "l1", 20, produtos, EstadoEncomenda.SOLICITADO);

       ge = new GestaoEncomendas();
       ge.addEncomenda(e1);
       ge.addEncomenda(e2);
    }

    /**
     * Tears down the test fixture.
     *
     * Called after every test case method.
     */
    @After
    public void tearDown() {
        
    }
    
    @Test
    public void testNumeroTotalEncomendas() {
        assertEquals(2, ge.totalEncomendas());
    }
    
    @Test
    public void testExisteEncomenda() {
        assertTrue(ge.existeEncomenda("e2"));
        assertFalse(ge.existeEncomenda("e3"));
    }
    
    @Test
    public void testAddRemove() {
        e3 = new EncomendaNormal("e3", "u1", "l2", 30, new ArrayList<>(), EstadoEncomenda.SOLICITADO);
        ge.addEncomenda(e3);
        assertTrue(ge.existeEncomenda("e3"));
        
        ge.removeEncomenda("e3");
        assertFalse(ge.existeEncomenda("e3"));
    }
    
    @Test
    public void testGet() {
        assertNull(ge.getEncomenda("e4"));
        e4 = new EncomendaMedica("e4", "u2", "l3", 50, new ArrayList<>(), EstadoEncomenda.SOLICITADO);
        
        ge.addEncomenda(e4);
        Encomenda enc = ge.getEncomenda("e4");
        assertNotNull(enc);
        
        ge.removeEncomenda("e3");
    }
    
    @Test
    public void testClone() {
        p4 = new Produto("p4", "desc4", 10, 20);
        p5 = new Produto("p5", "desc5", 15, 30);
        List<Produto> produtos = new ArrayList<>();
        produtos.add(p4);
        produtos.add(p5);
        
        e5 = new EncomendaNormal("e5", "u1", "l2", 30, produtos, EstadoEncomenda.SOLICITADO);
        ge.addEncomenda(e5);
        Encomenda e = ge.getEncomenda("e5");
        assertNotSame(e5, e);
        assertEquals(e5, e);
        
        ge.removeEncomenda("e5");
    }
    
    @Test
    public void testEncomendasPendentesUtilizador() {
        int n1 = ge.getEncomendasPendentesUtilizador("u1").size();
        int n2 = ge.getCodEncomendasPendentesUtilizador("u1").size();
        assertEquals(0, n1);
        assertEquals(0, n2);
    }
    
    @Test
    public void testEncomendasPendentesLoja() {
        int n = ge.getCodEncomendasPendentesLoja("l1").size();
        assertEquals(2, n);
    }
    
    @Test
    public void testEncomendasPendentesLevantamento() {
        List<String> encs = Arrays.asList("e1","e2");
        ge.setEstadoEncomendas(encs, EstadoEncomenda.DISPONIBILIZADO_LOJA);
        int n1 = ge.getCodEncomendasPendentesLevantamento().size();
        int n2 = ge.getEncomendasPendentesLevantamento().size();
        assertEquals(2, n1);
        assertEquals(2, n2);
    }
    
    @Test
    public void testLevantarEncomendaVoluntario() {
        ge.setDistanciaEncomenda("e1", 30);
        ge.setEstadoEncomenda("e1", EstadoEncomenda.DISPONIBILIZADO_LOJA);
        
        ge.levantarEncomenda("e1", "v1");
        assertTrue(ge.getEncomenda("e1").getEstadoEncomenda() == EstadoEncomenda.ACEITE_TRANSPORTADOR);
    }
    
    @Test
    public void testTransportarEncomenda() {
        ge.setDistanciaEncomenda("e1", 30);
        ge.setEstadoEncomenda("e1", EstadoEncomenda.DISPONIBILIZADO_LOJA);
        
        ge.levantarEncomenda("e1", "v1");
        ge.transportarEncomenda("e1", "v1");
        
        assertTrue(ge.getEncomenda("e1").getEstadoEncomenda() == EstadoEncomenda.ENTREGUE);
    }
    
    @Test 
    public void testOrcamentarTransporte() {
        ge.setDistanciaEncomenda("e2", 50);
        ge.setEstadoEncomenda("e2", EstadoEncomenda.DISPONIBILIZADO_LOJA);
        ge.orcamentarTransporte("e2", "t1", 120);
        
        assertTrue(ge.getEncomenda("e2").getEstadoEncomenda() == EstadoEncomenda.ORCAMENTO_TRANSPORTADOR);
    }
    
    @Test 
    public void testEncomendasPendentesLevantamentoTransportadora() {
        ge.setDistanciaEncomenda("e2", 50);
        ge.setEstadoEncomenda("e2", EstadoEncomenda.DISPONIBILIZADO_LOJA);
        ge.orcamentarTransporte("e2", "t1", 120);
        
        List<String> encs = Arrays.asList("e2");
        ge.setEstadoEncomendas(encs, EstadoEncomenda.ACEITE_UTILIZADOR);
        int n1 = ge.getCodEncomendasPendentesLevantamentoTransportadora("t1").size();
        int n2 = ge.getEncomendasPendentesLevantamentoTransportadora("t1").size();
        assertEquals(1, n1);
        assertEquals(1, n2);
    }
    
    @Test
    public void testLevantarEncomendaTransportadora() {
        ge.setDistanciaEncomenda("e2", 50);
        ge.setEstadoEncomenda("e2", EstadoEncomenda.DISPONIBILIZADO_LOJA);
        ge.orcamentarTransporte("e2", "t1", 120);
        
        List<String> encs = Arrays.asList("e2");
        ge.setEstadoEncomendas(encs, EstadoEncomenda.ACEITE_TRANSPORTADOR);
        
        ge.levantarEncomenda("e1", "v1");
        assertTrue(ge.getEncomenda("e1").getEstadoEncomenda() == EstadoEncomenda.ACEITE_TRANSPORTADOR);
    }
    
}
