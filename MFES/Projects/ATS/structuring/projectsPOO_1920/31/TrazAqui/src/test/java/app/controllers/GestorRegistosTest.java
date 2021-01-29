package app.controllers;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import app.models.RegistoEncomenda;
import app.models.Utilizador;
import app.models.Voluntario;
import app.enums.EstadosEncomendaEnum;
import app.enums.EstadosTransportadorEnum;
import app.exceptions.EmpresaTransportadoraNaoExistenteException;
import app.exceptions.EncomendaJaCanceladaException;
import app.exceptions.EncomendaNaoExistenteException;
import app.exceptions.EstadoRegressivoException;
import app.exceptions.SemPermissaoAprovarTransporteException;
import app.exceptions.SemPermissaoEfectuarAgendamentoEnvioException;
import app.exceptions.SemPermissaoEfectuarEncomendaException;
import app.exceptions.SemPermissaoTransporteEncException;
import app.exceptions.SemPermissaoTransporteEncMedicaException;
import app.exceptions.TransportadorOcupadoException;
import app.exceptions.UtilizadorJaExistenteException;
import app.interfaces.IInfo;
import app.models.EmpresaTransportadora;
import app.models.Encomenda;
import app.models.EncomendaMedica;
import app.models.LinhaEncomenda;
import app.models.Localizacao;
import app.models.Loja;
import app.models.Produto;

public class GestorRegistosTest {

    private static GestorRegistos gestor;
    private static RegistosEncomendas registosEncomendas;
    private static RegistosUtilizador registoUtilizadores;
    private static RegistosProdutos registosProdutos;
    // Registos

    private static RegistoEncomenda r1;
    private static RegistoEncomenda r2;
    private static RegistoEncomenda r3;
    private static RegistoEncomenda r4;
    private static RegistoEncomenda r5;
    private static RegistoEncomenda r6;
    private static RegistoEncomenda r7;
    private static RegistoEncomenda r8;
    private static RegistoEncomenda r9;
    private static RegistoEncomenda r10;
    private static RegistoEncomenda r11;
    private static RegistoEncomenda r13;

    private static Encomenda e1;
    private static Encomenda e2;
    private static Encomenda e3;
    private static Encomenda e4;
    private static Encomenda e5;
    private static Encomenda e6;
    private static Encomenda e7;
    private static Encomenda e8;
    private static Encomenda e9;
    private static Encomenda e10;
    private static Encomenda e12;
    private static Encomenda e13;

    private static LinhaEncomenda le1;
    private static LinhaEncomenda le2;
    private static LinhaEncomenda le3;
    private static LinhaEncomenda le4;

    private static Produto p1;
    private static Produto p2;
    private static Produto p3;
    private static Produto p4;
    private static Produto p5;
    private static Produto p6;
    private static Produto p7;
    private static Produto p8;
    private static Produto p9;
    private static Produto p10;


    private static List<Produto> lp1;
    private static List<Produto> lp2;
    private static List<Produto> lp3;
    private static List<Produto> lp4;
    private static List<Produto> lp5;
    private static List<Produto> lp6;

    private static Map<String, List<Produto>> registosP;

    // Utilizadores
    private static Utilizador u1;
    private static Utilizador u2;
    private static Utilizador u3;

    // Voluntarios

    private static Voluntario v1;
    private static Voluntario v2;
    private static Voluntario v3;

    // Transportadoras

    private static EmpresaTransportadora et1;
    private static EmpresaTransportadora et2;
    private static EmpresaTransportadora et3;

    // Lojas

    private static Loja l1;
    private static Loja l2;
    private static Loja l3;

    @BeforeEach
    void init() {

        p1 = new Produto("p1", "Batatas", 4.5);
        p2 = new Produto("p2", "Cebolas", 2.5);
        p3 = new Produto("p3", "Cenouras", 1.5);
        p4 = new Produto("p4", "Tomates", 5.5);
        p5 = new Produto("p5", "Monitor", 150);
        p6 = new Produto("p6", "Rato", 30);
        p7 = new Produto("p7", "Teclado", 55);
        p8 = new Produto("p8", "Sabão", 3);
        p9 = new Produto("p9", "Coca-Cola", 1.5);
        p10 = new Produto("p10", "Café", 2);

        lp1 = List.of(p1, p2, p7);
        lp2 = List.of(p3, p4, p5);
        lp3 = List.of(p6, p8);
        lp4 = List.of(p9, p10);
        lp5 = List.of(p1, p2, p3, p4, p8, p9, p10);
        lp6 = List.of(p5, p6, p7);

        registosP = new HashMap<>();

        registosP.put("l1", lp1);
        registosP.put("l2", lp2);
        registosP.put("l3", lp3);
        registosP.put("l4", lp4);
        registosP.put("l5", lp5);
        registosP.put("l6", lp6);

        registosProdutos = new RegistosProdutos(registosP);

        le1 = new LinhaEncomenda(p1, 1);
        le2 = new LinhaEncomenda(p2, 2);
        le3 = new LinhaEncomenda(p3, 3);
        le4 = new LinhaEncomenda(p4, 4);

        e1 = new EncomendaMedica("AAAA1", "u1", "l1", 10.7, LocalDateTime.of(2020, 01, 23, 12, 45),
                List.of(le1, le2));
        e2 = new Encomenda("AAAA2", "u2", "l3", 8.7, LocalDateTime.of(2020, 02, 23, 13, 39),
                List.of(le1));
        e3 = new EncomendaMedica("AAAA3", "u3", "l3", 4, LocalDateTime.of(2020, 06, 1, 16, 45),
                List.of(le2));
        e4 = new Encomenda("AAAA4", "u1", "l3", 15, LocalDateTime.of(2020, 04, 14, 17, 30),
                List.of(le3, le2));
        e5 = new Encomenda("AAAA5", "u3", "l2", 12, LocalDateTime.of(2020, 06, 30, 21, 45),
                List.of(le4, le3));
        e6 = new Encomenda("AAAA6", "u3", "l1", 1.3, LocalDateTime.of(2020, 07, 15, 23, 30),
                List.of(le1, le4));
        e7 = new Encomenda("AAAA7", "u2", "l2", 14, LocalDateTime.of(2020, 01, 12, 01, 45),
                List.of(le1, le2, le3, le4));
        e8 = new Encomenda("AAAA8", "u2", "l2", 11, LocalDateTime.of(2020, 04, 10, 05, 30),
                List.of(le1, le2, le3));
        e9 = new Encomenda("AAAA9", "u1", "l1", 16, LocalDateTime.of(2020, 05, 26, 07, 45),
                List.of(le1, le2, le4));
        e10 = new Encomenda("AAAA10", "u2", "l2", 3.5, LocalDateTime.of(2020, 01, 23, 07, 30),
                List.of(le2, le3, le4));



        e12 = new Encomenda("AAAA12", "u2", "l1", 23.5, LocalDateTime.of(2020, 01, 25, 07, 30),
                List.of(le2, le3, le4, le1));
        e13 = new Encomenda("AAAA13", "u3", "l3", 4, LocalDateTime.of(2020, 06, 1, 16, 45),
                List.of(le2));


        r1 = new RegistoEncomenda(e1, EstadosEncomendaEnum.ENVIADA, "v1", -1,
                LocalDateTime.of(2020, 01, 23, 13, 45), LocalDateTime.of(2020, 01, 23, 16, 45),
                null, 0);
        r2 = new RegistoEncomenda(e2, EstadosEncomendaEnum.ABERTA, null, -1, null, null, null, 0);
        r3 = new RegistoEncomenda(e3, EstadosEncomendaEnum.AGUARDAENVIO, null, -1,
                LocalDateTime.of(2020, 06, 1, 17, 45), null, null, 0);
        r4 = new RegistoEncomenda(e4, EstadosEncomendaEnum.RECEBIDA, "v3", 10,
                LocalDateTime.of(2020, 04, 14, 18, 30), LocalDateTime.of(2020, 04, 14, 23, 30),
                LocalDateTime.of(2020, 04, 15, 12, 30), 0);
        r5 = new RegistoEncomenda(e5, EstadosEncomendaEnum.ENVIADA, "et2", -1,
                LocalDateTime.of(2020, 06, 30, 22, 45), LocalDateTime.of(2020, 06, 30, 23, 45),
                null, 23);
        r6 = new RegistoEncomenda(e6, EstadosEncomendaEnum.RECEBIDA, "v2", 7,
                LocalDateTime.of(2020, 07, 15, 01, 30), LocalDateTime.of(2020, 07, 16, 12, 30),
                LocalDateTime.of(2020, 07, 16, 15, 30), 0);
        r7 = new RegistoEncomenda(e7, EstadosEncomendaEnum.ENVIADA, "v1", -1,
                LocalDateTime.of(2020, 01, 12, 06, 45), LocalDateTime.of(2020, 01, 12, 14, 45),
                null, 0);
        r8 = new RegistoEncomenda(e8, EstadosEncomendaEnum.AGUARDAENVIO, null, -1,
                LocalDateTime.of(2020, 04, 10, 05, 30), null, null, 0);
        r9 = new RegistoEncomenda(e9, EstadosEncomendaEnum.ABERTA, null, -1, null, null, null, 0);
        r10 = new RegistoEncomenda(e10, EstadosEncomendaEnum.ENVIADA, "et1", -1,
                LocalDateTime.of(2020, 01, 23, 10, 30), LocalDateTime.of(2020, 01, 24, 10, 30),
                null, 23);
        r11 = new RegistoEncomenda(e12, EstadosEncomendaEnum.CANCELADA, null, -1,
                LocalDateTime.of(2020, 01, 26, 07, 30), null, null, 0);

        r13 = new RegistoEncomenda(e13, EstadosEncomendaEnum.AGUARDAAPROVACAO, null, -1,
                LocalDateTime.of(2020, 06, 1, 17, 45), null, null, 0);

        Map<RegistoEncomenda, List<String>> registos = new TreeMap<>();

        registos.put(r1, List.of("v1"));
        registos.put(r2, List.of());
        registos.put(r3, List.of("et1", "et2"));
        registos.put(r4, List.of("v3"));
        registos.put(r5, List.of("et2"));
        registos.put(r6, List.of("v2"));
        registos.put(r7, List.of("v1"));
        registos.put(r8, List.of("et3"));
        registos.put(r9, List.of());
        registos.put(r10, List.of("et1"));
        registos.put(r11, List.of());
        registos.put(r13, List.of("et1"));

        registosEncomendas = new RegistosEncomendas(registos);


        u1 = new Utilizador("u1", "qwerty", "Lelo", new Localizacao(-97.28862, 59.067047));
        u2 = new Utilizador("u2", "qwerty", "MariaLelo", new Localizacao(-97.28864, 59.067048));
        u3 = new Utilizador("u3", "qwerty", "XicoLelo", new Localizacao(-97.28865, 59.067049));

        v1 = new Voluntario("v1", "qwerty", "Margarida", new Localizacao(-68.78327, -50.26914),
                EstadosTransportadorEnum.LIVRE, true, 5, 10);
        v2 = new Voluntario("v2", "qwerty", "Ana", new Localizacao(-68.78328, -50.26915),
                EstadosTransportadorEnum.OCUPADO, true, 5.3, 23);
        v3 = new Voluntario("v3", "qwerty", "Anabela", new Localizacao(-68.78329, -50.26915),
                EstadosTransportadorEnum.LIVRE, true, 5.3, 30);

        et1 = new EmpresaTransportadora("et1", "qwerty", "Zé Manel",
                new Localizacao(57.245117, 19.557358), EstadosTransportadorEnum.LIVRE, false, 7.25,
                20.0, "A", 40);
        et2 = new EmpresaTransportadora("et2", "qwerty", "Zé João",
                new Localizacao(57.245114, 19.557355), EstadosTransportadorEnum.LIVRE, false, 9.25,
                22, "B", 45);
        et3 = new EmpresaTransportadora("et3", "qwerty", "Zé Lucas",
                new Localizacao(57.245113, 19.557357), EstadosTransportadorEnum.LIVRE, false, 8.50,
                22.23, "C", 50);

        l1 = new Loja("l1", "qwert", "Skills", new Localizacao(39.627502, 33.60112), -1);
        l2 = new Loja("l2", "qwert", "Primark", new Localizacao(73474, 66.239685), -1);
        l3 = new Loja("l3", "qwert", "Nike", new Localizacao(57.339508, -86.066315), -1);


        Map<String, IInfo> utilizadores = new HashMap<>();

        utilizadores.put(u1.getEmail(), u1.clone());
        utilizadores.put(u2.getEmail(), u2.clone());
        utilizadores.put(u3.getEmail(), u3.clone());

        utilizadores.put(v1.getEmail(), v1.clone());
        utilizadores.put(v2.getEmail(), v2.clone());
        utilizadores.put(v3.getEmail(), v3.clone());

        utilizadores.put(et1.getEmail(), et1.clone());
        utilizadores.put(et2.getEmail(), et2.clone());
        utilizadores.put(et3.getEmail(), et3.clone());

        utilizadores.put(l1.getEmail(), l1.clone());
        utilizadores.put(l2.getEmail(), l2.clone());
        utilizadores.put(l3.getEmail(), l3.clone());

        registoUtilizadores = new RegistosUtilizador(utilizadores);

        gestor = new GestorRegistos(null, registoUtilizadores, registosEncomendas,
                registosProdutos);

    }

    /**
     * Test Crud
     */
    @Test
    @DisplayName("Test CRUD Operations")

    public void crudTest() {
        try {
            gestor.login(u1.getEmail(), "qwerty");
            gestor.logout();

        } catch (Exception e) {
            assertFalse(true);

        }

        IInfo newUserRepetido =
                new Utilizador("u1", "qwerty", "Lelo", new Localizacao(-97.28862, 59.067047));

        try {
            gestor.registaNovoTipoUtilizador(newUserRepetido);
            assertFalse(true);

        } catch (UtilizadorJaExistenteException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertFalse(true);
        }

        IInfo newUser =
                new Utilizador("u4", "qwerty", "Lelo", new Localizacao(-97.28862, 59.067047));

        try {
            gestor.registaNovoTipoUtilizador(newUser);
            gestor.login(newUser.getEmail(), "qwerty");
            gestor.logout();

        } catch (Exception e) {
            assertFalse(true);

        }

    }

    /**
     * Test Crud
     */
    @Test
    @DisplayName("Test CRUD Package Records")

    public void crudTestPackageRecords() {

        Encomenda newEnc =
                new Encomenda("encnova", null, null, 1, LocalDateTime.now(), List.of(le1));
        try {
            gestor.login(et1.getEmail(), "qwerty");
            gestor.inserirPedidoLoja(l1.getEmail(), newEnc);

        } catch (SemPermissaoEfectuarEncomendaException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertFalse(true);
        }

        gestor.logout();

        try {
            gestor.login(u2.getEmail(), "qwerty");
            gestor.inserirPedidoLoja(l2.getEmail(), newEnc);
            assertTrue(true);

        } catch (Exception e) {
            assertFalse(true);
        }

        gestor.logout();

    }

    /**
     * Test Crud
     */
    @Test
    @DisplayName("Test CRUD Package Ready")

    public void crudTestPackageReady() {

        try {
            gestor.login(v3.getEmail(), "qwerty");
            gestor.encomendaProntaEntrega("BBBB1");

        } catch (SemPermissaoEfectuarAgendamentoEnvioException e) {
            assertTrue(true);
        } catch (EncomendaNaoExistenteException e) {
            assertTrue(true);
        }

        catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        try {
            gestor.login(l1.getEmail(), "qwert");
            gestor.encomendaProntaEntrega(e12.getCodEnc());

        } catch (EncomendaJaCanceladaException e) {
            assertTrue(true);
        }

        catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        try {
            gestor.login(l3.getEmail(), "qwert");
            gestor.encomendaProntaEntrega(e4.getCodEnc());

        } catch (EstadoRegressivoException e) {
            assertTrue(true);
        }

        catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();
    }

    /**
     * Test Crud
     */
    @Test
    @DisplayName("Test CRUD Package For Delivery")

    public void crudTestPackageForDelivery() {

        // Utilizador nao pode indicar que envia encomenda
        try {
            gestor.login(u1.getEmail(), "qwerty");
            gestor.indicaEncomendaParaTransporte(e12.getCodEnc());
            assertTrue(false);

        } catch (SemPermissaoTransporteEncException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        // Encomenda ja foi cancelada nao pode ser enviada
        try {
            gestor.login(v3.getEmail(), "qwerty");
            gestor.indicaEncomendaParaTransporte(e12.getCodEnc());
            assertTrue(false);

        } catch (EncomendaJaCanceladaException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        // Encomenda ja foi cancelada nao pode ser enviada por estado regressivo
        try {
            gestor.login(et2.getEmail(), "qwerty");
            gestor.indicaEncomendaParaTransporte(e5.getCodEnc());
            assertTrue(false);

        } catch (EstadoRegressivoException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();
        // Empressa nao tem permissao para transporte enc medica
        try {
            gestor.login(et1.getEmail(), "qwerty");
            gestor.indicaEncomendaParaTransporte(e3.getCodEnc());
            assertTrue(false);

        } catch (SemPermissaoTransporteEncMedicaException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        // Transportador ocupado nao pode indicar disponibilidade para entrega
        try {
            gestor.login(v2.getEmail(), "qwerty");
            gestor.indicaEncomendaParaTransporte(e3.getCodEnc());

        } catch (TransportadorOcupadoException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        // Transportador esta livre e pode transportar enc medica
        try {
            gestor.login(v3.getEmail(), "qwerty");
            gestor.indicaEncomendaParaTransporte(e3.getCodEnc());
            assertTrue(true);

        } catch (Exception e) {
            assertTrue(false);
        }

    }

    /**
     * Test Crud
     */
    @Test
    @DisplayName("Test CRUD Delivery Aproved")

    public void crudTestDeliveryAproved() {

        // Transportador nao pode indicar que aprova encomenda
        try {
            gestor.login(v1.getEmail(), "qwerty");
            gestor.aprovaPreco(e13.getCodEnc(), et1.getEmail());
            assertTrue(false);

        } catch (SemPermissaoAprovarTransporteException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        // Transportador nao existente
        try {
            gestor.login(u3.getEmail(), "qwerty");
            gestor.aprovaPreco(e13.getCodEnc(), " ");
            assertTrue(false);

        } catch (EmpresaTransportadoraNaoExistenteException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);

        }
        gestor.logout();

        // Encomenda nao existe
        try {
            gestor.login(u3.getEmail(), "qwerty");
            gestor.aprovaPreco(" ", et1.getEmail());
            assertTrue(false);

        } catch (EncomendaNaoExistenteException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();
        // Encomenda ja cancelada
        try {

            gestor.login(u2.getEmail(), "qwerty");
            gestor.aprovaPreco(e12.getCodEnc(), et1.getEmail());
            assertTrue(false);

        } catch (EncomendaJaCanceladaException e) {
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        gestor.logout();

        // Encomenda aprovada para transporte
        try {

            gestor.login(u3.getEmail(), "qwerty");
            gestor.aprovaPreco(e13.getCodEnc(), et1.getEmail());
            assertTrue(true);
        } catch (Exception e) {
            assertTrue(false);
        }

        Set<RegistoEncomenda> registos = gestor.listagemPorUtilizador();

        Optional<RegistoEncomenda> optRe = registos.stream()
                .filter(re -> re.getEncomenda().getCodEnc().equals(e13.getCodEnc())).findFirst();

        if (optRe.isPresent()) {
            RegistoEncomenda re = optRe.get();
            assertSame(EstadosEncomendaEnum.APROVADA, re.getEstadoEncomenda());
            assertNotEquals(0, re.getPreco());
        } else {
            assertTrue(false);
        }

    }

    // /**
    // * Test Crud
    // */
    // @Test
    // @DisplayName("Test List 10 more Enc")

    // public void testList() {
    // List<String> espera = gestor.listagemDezUtilizadoresMaisEnc();
    // assertTrue(true);
    // }



}
