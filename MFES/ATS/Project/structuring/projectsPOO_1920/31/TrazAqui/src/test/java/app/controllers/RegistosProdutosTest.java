package app.controllers;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import app.exceptions.CodigoProdutoJaExistenteException;
import app.exceptions.CodigoProdutoNaoExistenteException;
import app.models.Produto;

public class RegistosProdutosTest {

        private static RegistosProdutos registosProdutos;


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
        private static Produto p11;


        private static List<Produto> lp1;
        private static List<Produto> lp2;
        private static List<Produto> lp3;
        private static List<Produto> lp4;
        private static List<Produto> lp5;
        private static List<Produto> lp6;

        private static Map<String, List<Produto>> registos;

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
                p11 = new Produto("p11", "Tapete", 25);

                lp1 = List.of(p1, p2, p7);
                lp2 = List.of(p3, p4, p5);
                lp3 = List.of(p6, p8);
                lp4 = List.of(p9, p10);
                lp5 = List.of(p1, p2, p3, p4, p8, p9, p10);
                lp6 = List.of(p5, p6, p7);

                registos = new HashMap<>();

                registos.put("l1", lp1);
                registos.put("l2", lp2);
                registos.put("l3", lp3);
                registos.put("l4", lp4);
                registos.put("l5", lp5);
                registos.put("l6", lp6);

                registosProdutos = new RegistosProdutos(registos);

        }

        /**
         * Test Crud
         */
        @Test
        @DisplayName("Test CRUD Operations")

        public void crudTest() {

                Map<String, List<Produto>> mapActual = registosProdutos.getProdutos();

                assertTrue(Objects.equals(registos, mapActual));

                mapActual.remove("l1");

                assertFalse(Objects.equals(registos, mapActual));

                mapActual = registosProdutos.getProdutos();
                assertTrue(Objects.equals(registos, mapActual));


                List<Produto> listaActual = registosProdutos.listaProdutos("l1");

                assertTrue(Objects.equals(lp1, listaActual));

                listaActual.remove(p1);

                List<Produto> listaEsperado = registosProdutos.listaProdutos("l1");

                assertFalse(Objects.equals(listaEsperado, listaActual));

                listaEsperado.add(p11);

                try {
                        registosProdutos.adicionaProduto("l1", p11);
                } catch (CodigoProdutoJaExistenteException e) {
                        e.printStackTrace();
                }

                listaActual = registosProdutos.listaProdutos("l1");

                assertTrue(Objects.equals(listaEsperado, listaActual));

                p11.setPrecoUni(100);

                listaEsperado = lp1.stream().collect(Collectors.toList());

                listaEsperado.add(p11);

                registosProdutos.atualizaProduto("l1", p11);

                listaActual = registosProdutos.listaProdutos("l1");

                assertTrue(Objects.equals(listaEsperado, listaActual));

                listaEsperado.remove(p11);

                try {
                        registosProdutos.removeProduto("l1", p11);
                } catch (CodigoProdutoNaoExistenteException e) {
                        e.printStackTrace();
                }

                listaActual = registosProdutos.listaProdutos("l1");

                assertTrue(Objects.equals(listaEsperado, listaActual));


        }

}
