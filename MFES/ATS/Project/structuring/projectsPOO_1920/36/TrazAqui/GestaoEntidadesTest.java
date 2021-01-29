import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * 
 *
 * @author  Artur Drohobytskyy
 * @version 1.0
 */
public class GestaoEntidadesTest {
    
    private Entidade e1, e2, e3, e4, e5;
    private GestaoEntidades ge;
    /**
     * Default constructor for test class GestaoEntidadesTest
     */
    public GestaoEntidadesTest() {
        
    }

    /**
     * Sets up the test fixture.
     *
     * Called before every test case method.
     */
    @Before
    public void setUp() {
        e1 = new Utilizador("u1", "João C.", new GPS(12.52426, 32.32489), "jc@mail.pt", "J040c");
        e2 = new Transportadora("t1","Trans ltd", new GPS(16.49203, 31.0191), "t@mail.pt", "tS?12", 200, 10, 5);
        e3 = new Voluntario("v1","José R.", new GPS(14.20193, 41.58135), "j@mail,pt", "Ir29!", 50);
        
        ge = new GestaoEntidades();
        ge.addEntidade(e1);
        ge.addEntidade(e2);
        ge.addEntidade(e3);
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
    public void testNumeroTotalEntidades() {
        assertEquals(3, ge.totalEntidades());
    }
    
    @Test
    public void testExisteEntidade() {
        assertTrue(ge.existeEntidade("u1"));
        assertTrue(ge.existeEntidade("t1"));
        assertTrue(ge.existeEntidade("v1"));
        assertFalse(ge.existeEntidade("u2"));
    }
    
    @Test
    public void testAddRemove() {
        e4 = new Utilizador("u2", "Ricardo L.", new GPS(12.52426, 32.32489), "rl@mail.pt", "JHD9%");
        ge.addEntidade(e4);
        assertTrue(ge.existeEntidade("u2"));
        
        ge.removeEntidade("u2");
        assertFalse(ge.existeEntidade("u2"));
    }
    
    @Test
    public void testGet() {
        assertNull(ge.getEntidade("u3"));
        e5 = new Utilizador("u3", "Jorge S.", new GPS(12.52426, 32.32489), "js@mail.pt", "JHD9%");
        
        ge.addEntidade(e5);
        Entidade e = ge.getEntidade("u3");
        assertNotNull(e);
    }
    
    @Test
    public void testClone() {
        e4 = new Utilizador("u2", "Ricardo L.", new GPS(12.52426, 32.32489), "rl@mail.pt", "JHD9%");
        ge.addEntidade(e4);
        Entidade e = ge.getEntidade("u2");
        assertNotSame(e4, e);
        assertEquals(e4, e);
    }
    
}
