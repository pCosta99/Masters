/*import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * A classe de teste GPSTest.
 *
 * @author  (seu nome)
 * @version (um número de versão ou data)
 */
//public class GPSTest{
    /*
     * Construtor default para a classe de teste GPSTest
     */
    /*public GPSTest()
    {
    }

    /**
     * Define a .
     *
     * Chamado antes de cada método de caso de teste.
     *
    @Before
    public void setUp(){
    }*/

    /*
     * Tears down the test fixture.
     *
     * Chamado após cada método de teste de caso.
     */
    /*@After
    public void tearDown(){
    }
    
    @Test
    public void testeValido(){
        Projeto.Util.GPS gps1 = new Projeto.Util.GPS(15, 40);
        assertTrue("A validaçao de um Projeto.Util.GPS valido deu erro", gps1.eValido());
        Projeto.Util.GPS gps2 = new Projeto.Util.GPS(-102,20);
        assertFalse("A validaçao de um Projeto.Util.GPS invalido deu erro", gps2.eValido());
    }
    
    @Test
    public void testeDistancia(){
        Projeto.Util.GPS gps1 = new Projeto.Util.GPS(15, 40);
        Projeto.Util.GPS gps2 = new Projeto.Util.GPS(30, 10);
        double valorEsperado = 3489269;
        double valorObtido = gps1.distancia(gps2);
        assertEquals("O calculo da distancia deu erro", valorEsperado, valorObtido, 3000);
    }
    
    @Test
    public void testeEquals(){
        Projeto.Util.GPS gps1 = new Projeto.Util.GPS(15, 40);
        Projeto.Util.GPS gps2 = new Projeto.Util.GPS(15, 10);
        assertFalse("Deu erro no equals de dois valores diferentes", gps1.equals(gps2));
        Projeto.Util.GPS gps3 =  new Projeto.Util.GPS(15,10);
        assertTrue("Deu erro no equals de dois valores iguais", gps2.equals(gps3));
    }
    
    @Test
    public void testeIsInside(){
        Projeto.Util.GPS gps1 = new Projeto.Util.GPS(15, 40);
        Projeto.Util.GPS gps2 = new Projeto.Util.GPS(30, 10);
        double raioFalso = 4000000;
        double raioVerdade = 50;
        assertTrue("O isValid deu erro num raio valido", gps1.isInsideRaio(raioVerdade, gps2));
        assertFalse("O isValid deu erro num raio invalido", gps1.isInsideRaio(raioFalso, gps2));
    }
}*/