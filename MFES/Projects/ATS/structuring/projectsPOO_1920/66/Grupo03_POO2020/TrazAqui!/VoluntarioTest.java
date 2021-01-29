import com.sun.management.VMOption;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.*;

class VoluntarioTest extends Voluntario {
    Voluntario v;
    Utilizador u;
    ArrayList<EncDistr> encsAv;
    Loja l;
    Ponto p;
    EncDistr ed1;
    Encomenda e1;
    LinhaEncomenda l1,l2,l3;
    TreeMap<String,EncDistr> encs;
    ArrayList<Double> classi;
    LocalDateTime n;

    @BeforeEach
    void setUp() {
        l1 = new LinhaEncomenda("p12","kermit porta-chaves",2,2.5);
        l2 = new LinhaEncomenda("p10","kermit peluche",5,7);
        l3 = new LinhaEncomenda("p11","kermit caneca",3,4);
        TreeMap<String,LinhaEncomenda> linha = new TreeMap<>();
        linha.put(l1.getCodProd(),l1);
        linha.put(l2.getCodProd(),l2);
        linha.put(l3.getCodProd(),l3);
        n = LocalDateTime.now();
        e1 = new Encomenda("e45",true,"u0","l54",3.0,linha,n,n,n,n,n,n,"Entregue");
        ed1 = new EncDistr(e1,"t0");
        encs = new TreeMap<>();
        encs.put(e1.getCodEncomenda(),ed1);
        p = new Ponto(1,1) ;
        classi = new ArrayList<>();
        classi.add(5.0);
        encsAv = new ArrayList<>();
        encs.put(e1.getCodEncomenda(),ed1);
        v = new Voluntario("v0","Giovani", p, "p1", encs, false, false, 5, "v0@kermit.com", classi);
        u = new Utilizador("u0","João", p, encs, "p1", "u0@kermit.com",encsAv);
        l = new Loja("u0","João", p, "p1", encs, 9.0, "l0@kermit.com");


    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void testAceitoTransporteMedicamentos() {
        boolean valorEsperado = !v.getOcup() && v.getCertMed();
        boolean valorObtido = v.aceitoTransporteMedicamentos();
        assertEquals(valorObtido, valorEsperado, "Aceitação transporte medicamentos errada");
    }

    @Test
    void testCalculoTempo() {
        double tempoAteLoja = p.distancia(p) / v.getVelocidade();
        double tempoAteUser = p.distancia(p) / v.getVelocidade();
        double tempoNaLoja = l.getTempoMedioEspera();
        double valorEsperado = tempoNaLoja + tempoAteLoja + tempoAteUser;
        double valorObtido = v.calculoTempo(l,u);
        assertEquals(valorObtido, valorEsperado, 0.1, "Calculo do tempo incorreto");
    }

    @Test
    void testInRaio() {
        boolean valorEsperado = p.distancia(p)<=v.getRaio();
        boolean valorObtido = v.inRaio(l);
        assertEquals(valorObtido, valorEsperado, "Verificação do raio correta");
    }

    @Test
    void testAddClassi() {
        assertFalse(v.getClassificacao().contains(2.0),"Classificação inexistente presente no array");
        v.addClassi(2.0);
        assertTrue(v.getClassificacao().contains(2.0),"Classificação existente não presente no array");
    }

    @Test
    void testClassificacaoMedia() {
        double valorEsperado =  classi.stream().reduce(0.0, Double::sum) / classi.size();
        double valorObtido = v.classificacaoMedia();
        assertEquals(valorEsperado, valorObtido, 0.1, "Cálculo da classificação média incorreto");
    }

    @Test
    void testDataUltEncomenda() {
        LocalDateTime valorEsperado = n;
        LocalDateTime valorObtido = v.dataUltEncomenda();
        assertEquals(valorEsperado, valorObtido, "Última data de encomenda incorreto");
    }
}