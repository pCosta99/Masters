import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.TreeMap;

import static org.junit.jupiter.api.Assertions.*;

class RegistoTest extends Registo {
    Voluntario v;
    Utilizador u;
    ArrayList<EncDistr> encsAv;
    Loja l;
    Transportadora t;
    Ponto p;
    EncDistr ed1;
    Encomenda e1;
    LinhaEncomenda l1, l2, l3;
    TreeMap<String, EncDistr> encs;
    ArrayList<Double> classi;
    LocalDateTime n;
    Registo r;
    TreeMap<String, RegistoTULV> regs;
    TreeMap<String, LinhaEncomenda> linha;
    ArrayList<Double> cl;
    TreeMap<String, EncDistr> enc;

    @BeforeEach
    void setUp() {
        enc = new TreeMap<>();
        cl = new ArrayList<>();
        l1 = new LinhaEncomenda("p12", "kermit porta-chaves", 2, 2.5);
        l2 = new LinhaEncomenda("p10", "kermit peluche", 5, 7);
        l3 = new LinhaEncomenda("p11", "kermit caneca", 3, 4);
        linha = new TreeMap<>();
        linha.put(l1.getCodProd(), l1);
        linha.put(l2.getCodProd(), l2);
        linha.put(l3.getCodProd(), l3);
        n = LocalDateTime.now();
        p = new Ponto(1, 1);
        e1 = new Encomenda("e45", true, "u0", "l54", 3.0, linha, n, n, n, n, n, n, "Pendente");
        ed1 = new EncDistr(e1, "t5");
        encs = new TreeMap<>();
        encs.put(e1.getCodEncomenda(), ed1);
        t = new Transportadora("t5", "n", p, "pw", 2, 4, encs, true, 5, "123456789", "era", 67, cl);
        classi = new ArrayList<>();
        classi.add(5.0);
        encsAv = new ArrayList<>();
        v = new Voluntario("v0", "Giovani", p, "p1", encs, true, true, 5, "v0@kermit.com", classi);
        u = new Utilizador("u0", "João", p, encs, "p1", "u0@kermit.com", encsAv);
        l = new Loja("l54", "João", p, "p1", encs, 9.0, "l0@kermit.com");
        regs = new TreeMap<>();
        regs.put(v.getCodigo(), v);
        regs.put(t.getCodigo(), t);
        regs.put(u.getCodigo(), u);
        regs.put(l.getCodigo(), l);
        r = new Registo(regs);
    }

    @AfterEach
    void tearDown() {
    }

    @Test
    void testAdicionaEncomendaLU() {
        Encomenda e2 = new Encomenda("e40", true, "u0", "l54", 3.0, linha, n, n, n, n, n, n, "Pendente");
        Utilizador u = (Utilizador) r.getRegistos().get("u0");
        assertFalse(u.getEncomendas().containsKey("e40"), "Linha não existente presente na encomenda");
        r.adicionaEncomendaLU(e2);
        u = (Utilizador) r.getRegistos().get("u0");
        assertTrue(u.getEncomendas().containsKey("e40"), "Linha existente não consta na encomenda");
    }

    @Test
    void testTranspFaster() {
        Transportadora valorEsperado = t;
        Transportadora valorObtido = r.transpFaster(u, l, 1);
        assertEquals(valorEsperado, valorObtido);
    }

    @Test
    void testVolFaster() {
        Voluntario valorEsperado = v;
        Voluntario valorObtido = r.volFaster(u, l, 1);
        assertEquals(valorEsperado, valorObtido);
    }

    @Test
    void testGeraCodProduto() {
        String valorEsperado = "p0";
        String valorObtido = r.geraCodProduto(0);
        assertEquals(valorEsperado, valorObtido);
    }

    @Test
    void testGeraCodEncomenda() {
        String valorEsperado = "e0";
        String valorObtido = r.geraCodEncomenda();
        assertEquals(valorEsperado, valorObtido);
    }

    @Test
    void testAtualizaRegistos() {
        Voluntario vl = new Voluntario("v67", "Mário", p, "p1", encs, true, true, 5, "v67@kermit.com", classi);
        Utilizador us = new Utilizador("u23", "Miguel", p, encs, "p1", "u23@kermit.com", encsAv);
        Transportadora tr = new Transportadora("t55", "n", p, "pw", 2, 4, enc, true, 5, "123456789", "era", 67, cl);
        assertNull(r.getRegistoTULV(vl.getCodigo()), "Voluntario não existente presente no registo");
        assertNull(r.getRegistoTULV(us.getCodigo()), "Utilizador não existente presente no registo");
        assertNull(r.getRegistoTULV(tr.getCodigo()), "Transportadora não existente presente no registo");
        r.atualizaRegistos(vl, us, tr);
        assertNotNull(r.getRegistoTULV(vl.getCodigo()), "Voluntario existente não presente no registo");
        assertNotNull(r.getRegistoTULV(us.getCodigo()), "Utilizador existente não presente no registo");
        assertNotNull(r.getRegistoTULV(tr.getCodigo()), "Transportadora existente não presente no registo");
    }

    @Test
    void testTop10Utilizadores() {
        StringBuilder sb = new StringBuilder();
        sb.append(u.getNome()).append(" (").append(u.getCodigo()).append(") ").append("-> ").append(u.getEncsSize()).append(" encomendas").append("\n");
        String valorEsperado = sb.toString();
        String valorObtido = r.top10Utilizadores();
        System.out.println(valorObtido);
        assertEquals(valorEsperado, valorObtido);
    }

    @Test
    void testTop10Transportadoras() {
        StringBuilder sb = new StringBuilder();
        sb.append(t.getNome()).append(" (").append(t.getCodigo()).append(") ").append("-> ").append(t.getNumKm()).append(" km").append("\n");
        String valorEsperado = sb.toString();
        String valorObtido = r.top10Transportadoras();
        System.out.println(valorObtido);
        assertEquals(valorEsperado, valorObtido);
    }

    @Test
    void testPutRegistoTULV() {
        Voluntario vl = new Voluntario("v92", "Joaquim", p, "p1", encs, true, true, 5, "v92@kermit.com", classi);
        assertNull(r.getRegistoTULV(vl.getCodigo()), "Voluntario não existente presente no registo");
        r.putRegistoTULV(vl);
        assertNotNull(r.getRegistoTULV(vl.getCodigo()), "Voluntario existente não presente no registo");
    }

    @Test
    void testAlteraEstadoEncomenda() {
        e1.setEstado("Entregue");
        RegistoTULV aux = r.getRegistoTULV("u0");
        EncDistr x = aux.getEncomendas().get("e45");
        Encomenda a = x.getEncomenda();
        assertNotEquals(a.getEstado(), "Entregue");
        r.alteraEstadoEncomenda(t.getCodigo(), e1);
        RegistoTULV aux2 = r.getRegistoTULV("u0");
        EncDistr y = aux2.getEncomendas().get("e45");
        Encomenda b = y.getEncomenda();
        assertEquals(b.getEstado(), "Entregue");
    }

    @Test
    void testAdicionaMyEncAv() {
        Encomenda e2 = new Encomenda("e40", true, "u0", "l54", 3.0, linha, n, n, n, n, n, n, "Pendente");
        EncDistr ed2 = new EncDistr(e2, "t0");
        Utilizador u = (Utilizador) r.getRegistos().get("u0");
        assertFalse(u.getMyEncsAv().contains(ed2), "Linha não existente presente na encomenda");
        r.adicionaMyEncAv(ed2);
        u = (Utilizador) r.getRegistos().get("u0");
        assertTrue(u.getMyEncsAv().contains(ed2), "Linha existente não consta na encomenda");
    }

    @Test
    void testAceitaEncomendaLU() {
        EncDistr x = r.getRegistos().get(u.getCodigo()).getEncomendas().get("e45");
        Encomenda y = x.getEncomenda();
        assertNotEquals("Entregue", y.getEstado(), "Encomenda com estado errado");
        r.aceitaEncomendaLU(e1.getCodEncomenda());
        EncDistr a = r.getRegistos().get(u.getCodigo()).getEncomendas().get("e45");
        Encomenda b = a.getEncomenda();
        assertEquals("Entregue", b.getEstado(), "Encomenda com estado errado");
    }
}