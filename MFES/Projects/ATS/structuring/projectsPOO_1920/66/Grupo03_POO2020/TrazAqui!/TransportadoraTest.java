import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.*;

class TransportadoraTest extends Transportadora {
    Loja l;
    Utilizador u;
    Encomenda e;
    Transportadora t;
    Ponto p;
    TreeMap<String,EncDistr> enc;
    List<EncDistr> enco;
    LinhaEncomenda l1,l2,l3;
    ArrayList<Double> cl;
    Registo r;

    @BeforeEach
    void setUp() {
        enc = new TreeMap<>();
        enco = new ArrayList<>();
        cl = new ArrayList<>();
        p = new Ponto(2,4);
        t = new Transportadora("t5","n",p,"pw",2,4,enc,true,5,"123456789","era",67,cl);
        u = new Utilizador("u78","Antonio",p,enc,"pw","email",enco);
        l = new Loja("l7","fr",p,"era",enc,5,"mail");
        l1 = new LinhaEncomenda("p12","kermit porta-chaves",2,2.5);
        l2 = new LinhaEncomenda("p10","kermit peluche",5,7);
        l3 = new LinhaEncomenda("p11","kermit caneca",3,4);
        TreeMap<String,LinhaEncomenda> a = new TreeMap<>();
        a.put(l1.getCodProd(),l1);
        a.put(l2.getCodProd(),l2);
        a.put(l3.getCodProd(),l3);
        LocalDateTime n = LocalDateTime.now();
        r = new Registo();
        e = new Encomenda("e45",true,"u70","l54",3.0,a,n,n,n,n,n,n,"Pendente");
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void testCalculoPreco() {
        double valorEsperado = t.getGps().distancia(l.getGps())*t.getPrecoKm() + l.getGps().distancia(u.getGps()) *  t.getPrecoKm() + e.getPeso() * t.getTaxaPeso();
        double valorObtido = t.calculoPreco(l,u,e);
        assertEquals(valorEsperado,valorObtido,0.1);
    }

    @Test
    void testCalculoTempo() {
        double valorEsperado = t.getGps().distancia(l.getGps()) / t.getVelocidade() + l.getGps().distancia(u.getGps()) / t.getVelocidade() + l.getTempoMedioEspera();
        double valorObtido = t.calculoTempo(l,u);
        assertEquals(valorEsperado,valorObtido,0.1);
    }

    @Test
    void testCalculoDistancia() {
        double valorEsperado = t.getGps().distancia(l.getGps()) + u.getGps().distancia(l.getGps());
        double valorObtido = t.calculoDistancia(l,u);
        assertEquals(valorEsperado,valorObtido,0.1);
    }

    @Test
    void testInRaio() {
        boolean valorEsperado = p.distancia(p)<=t.getRaio();
        boolean valorObtido = t.inRaio(l);
        assertEquals(valorObtido, valorEsperado, "Verificação do raio correta");
    }

    @Test
    void testClassificacaoMedia() {
        double valorEsperado =  cl.stream().reduce(0.0, Double::sum) / cl.size();
        double valorObtido = t.classificacaoMedia();
        assertEquals(valorEsperado, valorObtido, 0.1, "Cálculo da classificação média incorreto");
    }

    @Test
    void testAddClassi() {
        assertFalse(t.getClassificacao().contains(2.0),"Classificação inexistente presente no array");
        t.addClassi(2.0);
        assertTrue(t.getClassificacao().contains(2.0),"Classificação existente não presente no array");
    }

    @Test
    void testFaturacaoPeriodo() {
        LocalDateTime t1 = LocalDateTime.now();
        LocalDateTime t2 = LocalDateTime.of(2020,03,05,00,00);
        double valorEsperado = 0;
        TreeMap<String, RegistoTULV> r1 = r.getRegistos();
        for(EncDistr en : this.getEncomendas().values()) {
            LocalDateTime aux = en.getEncomenda().getDataE();
            if (aux.isAfter(t1) && aux.isBefore(t2)) {
                valorEsperado += calculoPreco((Loja) r1.get(en.getEncomenda().getCodLoja()), (Utilizador) r1.get(en.getEncomenda().getCodUtilizador()), en.getEncomenda());
            }
        }
        double valorObtido = t.faturacaoPeriodo("2015-03-02","2020-01-12",r);
        assertEquals(valorEsperado,valorObtido,0.1);
    }
}