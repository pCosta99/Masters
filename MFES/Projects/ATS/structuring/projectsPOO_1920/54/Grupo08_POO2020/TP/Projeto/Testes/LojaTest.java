/*import static org.junit.Assert.*;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import java.util.List;
import java.util.ArrayList;

/**
 * A classe de teste LojaTest.
 *
public class LojaTest{
    private Projeto.Entidades.Loja loja;
    private Projeto.Projeto.Encomenda.Encomenda enc1;
    private Projeto.Projeto.Encomenda.Encomenda enc2;
    
    /**
     * Construtor default para a classe de teste LojaTest
     *
    public LojaTest(){
    }

    /**
     * Define a Projeto.Entidades.Loja.
     *
     * Chamado antes de cada método de caso de teste.
     *
    @Before
    public void setUp(){
        Projeto.Encomenda.LinhaDeEncomenda l1, l2, l3;
        List<Projeto.Projeto.Encomenda.Encomenda> encs = new ArrayList<>();
        this.loja = new Projeto.Entidades.Loja("001", "Lidl", new Projeto.Util.GPS(13,12), encs, true, 3, 1, new ArrayList<>());
        
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
        
        this.loja.adicionaEnc(enc); this.loja.adicionaEnc(enc1); this.loja.adicionaEnc(enc2); this.loja.adicionaEnc(enc3);
        this.enc1 = new Projeto.Projeto.Encomenda.Encomenda(enc3);
        this.enc2 = new Projeto.Projeto.Encomenda.Encomenda(enc2);
    }

    /**
     * Tears down the test fixture.
     *
     * Chamado após cada método de teste de caso.
     *
    @After
    public void tearDown(){
    }
    
    /*
    @Test
    public void testeEncomendaPronta(){
        assertTrue("Nao encontrou a encomenda Pronta", this.loja.encomendaPronta(this.enc2));
        assertFalse("Deveria ser falso porque a encomenda nao esta pronta", this.loja.encomendaPronta(this.enc1));
    }*
}*/
