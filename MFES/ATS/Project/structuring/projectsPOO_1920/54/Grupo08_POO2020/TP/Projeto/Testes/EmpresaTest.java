/*import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import java.util.List;
import java.util.ArrayList;

/**
 * A classe de teste EmpresaTest.
 *
 * @author  (seu nome)
 * @version (um número de versão ou data)
 *
public class EmpresaTest{
    private Projeto.Entidades.Empresa e;
    
    /**
     * Construtor default para a classe de teste EmpresaTest
     *
    public EmpresaTest(){
    }

    /**
     * Define a .
     *
     * Chamado antes de cada método de caso de teste.
     *
    @Before
    public void setUp(){
        Projeto.Encomenda.LinhaDeEncomenda l1, l2, l3;
        List<Projeto.Projeto.Encomenda.Encomenda> encs = new ArrayList<>();
        List<Float> vels =  new ArrayList<>();
        List<Integer> cls = new ArrayList<>();
        this.e = new Projeto.Entidades.Empresa("001", "Fedex", new Projeto.Util.GPS(12,12), encs, vels, 100, true, cls, new Projeto.Util.Estado("Livre"),10, 1.1f, "");
        
        List<Projeto.Encomenda.LinhaDeEncomenda> lista = new ArrayList<>();
        Projeto.Projeto.Encomenda.Encomenda enc = new Projeto.Projeto.Encomenda.Encomenda("000001", "Continente", "Joao",  2, false, lista);
        l1 = new Projeto.Encomenda.LinhaDeEncomenda("pao", "sem descriçao", 4, 2, false);
        l2 = new Projeto.Encomenda.LinhaDeEncomenda("chocolate", "sem descriçao", 3, 20, false);
        l3 = new Projeto.Encomenda.LinhaDeEncomenda("bananas", "sem descriçao", 10, 0.2f, false);
        enc.insereLinhaEnc(l1);
        enc.insereLinhaEnc(l2);
        enc.insereLinhaEnc(l3);
        
        List<Projeto.Encomenda.LinhaDeEncomenda> lista1 = new ArrayList<>();
        Projeto.Projeto.Encomenda.Encomenda enc1 = new Projeto.Projeto.Encomenda.Encomenda("000002", "Continente", "Joao",  0.5f, true, lista1);
        l1 = new Projeto.Encomenda.LinhaDeEncomenda("pao", "sem descriçao", 4, 0.1f, false);
        l2 = new Projeto.Encomenda.LinhaDeEncomenda("fiambre", "sem descriçao", 16, 0.2f, false);
        l3 = new Projeto.Encomenda.LinhaDeEncomenda("queijo", "sem descriçao", 32, 0.3f, false);
        enc1.insereLinhaEnc(l1);
        enc1.insereLinhaEnc(l2);
        enc1.insereLinhaEnc(l3);
        
        List<Projeto.Encomenda.LinhaDeEncomenda> lista2 = new ArrayList<>();
        Projeto.Projeto.Encomenda.Encomenda enc2 = new Projeto.Projeto.Encomenda.Encomenda("000002", "Continente", "Joao",  3, true, lista1);
        l1 = new Projeto.Encomenda.LinhaDeEncomenda("chocolate", "sem descriçao", 1, 5, false);
        l2 = new Projeto.Encomenda.LinhaDeEncomenda("vinho", "sem descriçao", 1, 5, false);
        enc1.insereLinhaEnc(l1);
        enc1.insereLinhaEnc(l2);
        
        List<Projeto.Encomenda.LinhaDeEncomenda> lista3 = new ArrayList<>();
        Projeto.Projeto.Encomenda.Encomenda enc3 = new Projeto.Projeto.Encomenda.Encomenda("000002", "Continente", "Joao",  1, false, lista1);
        l1 = new Projeto.Encomenda.LinhaDeEncomenda("Bruffen", "sem descriçao", 1, 6, true);
        l2 = new Projeto.Encomenda.LinhaDeEncomenda("Agua", "sem descriçao", 4, 0.1f, false);
        enc1.insereLinhaEnc(l1);
        enc1.insereLinhaEnc(l2);
        
        this.e.adicionaEnc(enc); this.e.adicionaEnc(enc1); this.e.adicionaEnc(enc2); this.e.adicionaEnc(enc3);
        this.e.insereClassificacao(10); this.e.insereClassificacao(5); this.e.insereClassificacao(8); this.e.insereClassificacao(7); this.e.insereClassificacao(9);
        this.e.insereVel(10); this.e.insereVel(11); this.e.insereVel(20); this.e.insereVel(5); this.e.insereVel(15); this.e.insereVel(9); this.e.insereVel(12);
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
    public void testeVelocidadeMedia(){
        float esperado = (float) (10 + 11 + 20 + 5 + 15 + 9 + 12) / 7;
        float obtido = this.e.calculaVelMed();
        assertEquals("Velociade media errada", esperado, obtido, 0.01f);
    }
    
    @Test
    public void testeClassificacaoMedia(){
        float esperado = (float) (10 + 5 + 8 + 7 + 9) / 5;
        float obtido = this.e.calculaClassMed();
        assertEquals("Classificacao media errada", esperado, obtido, 0.1f);
    }
    
    @Test
    public void testeTotalFaturado(){
        float esperado = 70 + 13.2f + 10 + 6.4f;
        float obtido = this.e.calculaTotalFaturado();
        assertEquals("Total faturado errado", esperado, obtido, 0.001f);
    }
}*/