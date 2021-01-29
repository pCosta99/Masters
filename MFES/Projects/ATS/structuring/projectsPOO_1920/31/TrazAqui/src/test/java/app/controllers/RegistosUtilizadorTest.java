package app.controllers;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import app.enums.EstadosTransportadorEnum;
import app.enums.TiposUtilizadoresEnum;
import app.exceptions.UtilizadorJaExistenteException;
import app.exceptions.UtilizadorNaoExistenteException;
import app.interfaces.IInfo;
import app.models.EmpresaTransportadora;
import app.models.Localizacao;
import app.models.Loja;
import app.models.Utilizador;
import app.models.Voluntario;

public class RegistosUtilizadorTest {

        private static RegistosUtilizador registoUtilizadores;

        // Utilizadores
        private static Utilizador u1;
        private static Utilizador u2;
        private static Utilizador u3;

        // Voluntarios

        private static Voluntario v1;
        private static Voluntario v2;
        private static Voluntario v3;

        // Transportadoras

        private static EmpresaTransportadora e1;
        private static EmpresaTransportadora e2;
        private static EmpresaTransportadora e3;

        // Lojas

        private static Loja l1;
        private static Loja l2;
        private static Loja l3;

        private static Utilizador crudTestUtilizador;

        @BeforeEach
        void init() {

                u1 = new Utilizador("u1", "qwerty", "Lelo", new Localizacao(-97.28862, 59.067047));
                u2 = new Utilizador("u2", "qwerty", "MariaLelo",
                                new Localizacao(-97.28864, 59.067048));
                u3 = new Utilizador("u3", "qwerty", "XicoLelo",
                                new Localizacao(-97.28865, 59.067049));

                v1 = new Voluntario("v1", "qwerty", "Margarida",
                                new Localizacao(-68.78327, -50.26914),
                                EstadosTransportadorEnum.LIVRE, true, 5, 10);
                v2 = new Voluntario("v2", "qwerty", "Ana", new Localizacao(-68.78328, -50.26915),
                                EstadosTransportadorEnum.OCUPADO, false, 5.3, 23);
                v3 = new Voluntario("v3", "qwerty", "Anabela",
                                new Localizacao(-68.78329, -50.26915),
                                EstadosTransportadorEnum.LIVRE, true, 5.3, 30);

                e1 = new EmpresaTransportadora("e1", "qwerty", "Zé Manel",
                                new Localizacao(57.245117, 19.557358),
                                EstadosTransportadorEnum.LIVRE, false, 7.25, 20.0, "A", 40);
                e2 = new EmpresaTransportadora("e2", "qwerty", "Zé João",
                                new Localizacao(57.245114, 19.557355),
                                EstadosTransportadorEnum.LIVRE, false, 9.25, 22, "B", 45);
                e3 = new EmpresaTransportadora("e3", "qwerty", "Zé Lucas",
                                new Localizacao(57.245113, 19.557357),
                                EstadosTransportadorEnum.LIVRE, false, 8.50, 22.23, "C", 50);

                l1 = new Loja("l1", "qwert", "Skills", new Localizacao(39.627502, 33.60112), -1);
                l2 = new Loja("l2", "qwert", "Primark", new Localizacao(73474, 66.239685), -1);
                l3 = new Loja("l3", "qwert", "Nike", new Localizacao(57.339508, -86.066315), -1);

                crudTestUtilizador = new Utilizador("u4", "qwerty", "Lelão",
                                new Localizacao(64.87256, 43.373962));

                Map<String, IInfo> utilizadores = new HashMap<>();

                utilizadores.put(u1.getEmail(), u1.clone());
                utilizadores.put(u2.getEmail(), u2.clone());
                utilizadores.put(u3.getEmail(), u3.clone());

                utilizadores.put(v1.getEmail(), v1.clone());
                utilizadores.put(v2.getEmail(), v2.clone());
                utilizadores.put(v3.getEmail(), v3.clone());

                utilizadores.put(e1.getEmail(), e1.clone());
                utilizadores.put(e2.getEmail(), e2.clone());
                utilizadores.put(e3.getEmail(), e3.clone());

                utilizadores.put(l1.getEmail(), l1.clone());
                utilizadores.put(l2.getEmail(), l2.clone());
                utilizadores.put(l3.getEmail(), l3.clone());

                registoUtilizadores = new RegistosUtilizador(utilizadores);

        }

        /**
         * Test Crud
         */
        @Test
        @DisplayName("Test CRUD Operations")

        public void crudTest() {

                assertFalse(registoUtilizadores.existeUtilizador(crudTestUtilizador.getEmail()));

                try {
                        registoUtilizadores.adicionaUtilizador(crudTestUtilizador);
                        registoUtilizadores.adicionaUtilizador(u1);

                } catch (UtilizadorJaExistenteException e) {
                        assertTrue(true);
                } catch (Exception e) {
                }

                assertTrue(registoUtilizadores.existeUtilizador(crudTestUtilizador.getEmail()));

                Utilizador uTemp = null;
                try {

                        uTemp = (Utilizador) registoUtilizadores
                                        .getUtilizador(crudTestUtilizador.getEmail());
                } catch (Exception e) {
                }

                try {
                        uTemp = (Utilizador) registoUtilizadores.getUtilizador("");
                } catch (UtilizadorNaoExistenteException e) {
                        assert (true);
                } catch (Exception e) {
                }

                assertTrue(uTemp.equals(crudTestUtilizador));

                uTemp.setEmail("uTemp");

                assertFalse(uTemp.equals(crudTestUtilizador));

                try {
                        registoUtilizadores.apagaUtilizador(crudTestUtilizador);
                        assertTrue(true);
                        registoUtilizadores.apagaUtilizador(crudTestUtilizador);
                        assertTrue(false);


                } catch (UtilizadorNaoExistenteException e) {
                        assertTrue(true);
                } catch (Exception e) {

                }

                assertFalse(registoUtilizadores.existeUtilizador(crudTestUtilizador.getEmail()));

        }

        /**
         * Test Qauntity Count
         */
        @Test
        @DisplayName("Test Quantity Count Operations")

        public void countTeste() {

                assertTrue(registoUtilizadores
                                .contaUtilizadores(TiposUtilizadoresEnum.UTILIZADOR) == 3);
                assertTrue(registoUtilizadores.contaUtilizadores(TiposUtilizadoresEnum.LOJA) == 3);
                assertTrue(registoUtilizadores
                                .contaUtilizadores(TiposUtilizadoresEnum.VOLUNTARIO) == 3);
                assertTrue(registoUtilizadores
                                .contaUtilizadores(TiposUtilizadoresEnum.EMPRESA) == 3);

        }

}
