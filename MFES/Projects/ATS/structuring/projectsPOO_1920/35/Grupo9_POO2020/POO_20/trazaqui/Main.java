import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.util.List;
import java.util.ArrayList;
import java.time.LocalDate;
import java.time.Month;


/**
 * Write a description of class Main here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Main{
    
    public static void main(String [] args) throws UtilizadorExistenteException{
        TrazAqui j1 = new TrazAqui();
        
        //CLIENTES
        
        //1
        Cliente c1 = new Cliente();
        c1.setId("c1001");
        c1.setNome("Xavier Lopes");
        Coordenadas c1C = new Coordenadas();
        c1C.setLatitude(10.8);
        c1C.setLongitude(12.5);
        c1.setCoordenadas(c1C);
        c1.setEmail("xavier@live.pt");
        c1.setPassword("xavier123");
        c1.setMorada("Rua do Cliente nº1");
        c1.setNifC("123321231");
        j1.registaUtilizador(c1);
        
        //2
        Cliente c2 = new Cliente();
        c2.setId("c2002");
        c2.setNome("Joaquim Pinto");
        Coordenadas c2C = new Coordenadas();
        c2C.setLatitude(11.8);
        c2C.setLongitude(13.5);
        c2.setCoordenadas(c2C);
        c2.setEmail("joaquim@live.pt");
        c2.setPassword("joaquim123");
        c2.setMorada("Rua do Cliente nº2");
        //c2.setHist = 
        c2.setNifC("243234223");
        j1.registaUtilizador(c2);
        
        //3
        Cliente c3 = new Cliente();
        c3.setId("c3003");
        c3.setNome("Andre Pereira");
        Coordenadas c3C = new Coordenadas();
        c3C.setLatitude(7.9);
        c3C.setLongitude(9.7);
        c3.setCoordenadas(c3C);
        c3.setEmail("andre@gmail.com");
        c3.setPassword("andre123");
        c3.setMorada("Rua do Cliente nº3");
        //c3.setHist = 
        c3.setNifC("16546643");
        j1.registaUtilizador(c3);
                
        //4
        Cliente c4 = new Cliente();
        c4.setId("c4004");
        c4.setNome("Lucia Jesus");
        Coordenadas c4C = new Coordenadas();
        c4C.setLatitude(18.8);
        c4C.setLongitude(8.5);
        c4.setCoordenadas(c4C);
        c4.setEmail("lucia@gmail.com");
        c4.setPassword("lucia123");
        c4.setMorada("Rua do Cliente nº4");
        //c4.setHist = 
        c4.setNifC("329098909");
        j1.registaUtilizador(c4);
        
        //5
        Cliente c5 = new Cliente();
        c5.setId("c5345");
        c5.setNome("Luis Andre");
        Coordenadas c5C = new Coordenadas();
        c5C.setLatitude(13.9);
        c5C.setLongitude(15.5);
        c5.setCoordenadas(c5C);
        c5.setEmail("luis@gmail.com");
        c5.setPassword("luis123");
        c5.setMorada("Rua do Cliente nº5");
        //c5.setHist = 
        c5.setNifC("23425124");
        j1.registaUtilizador(c5);
        
        //6
        Cliente c6 = new Cliente();
        c6.setId("c6456");
        c6.setNome("Adriana Martins");
        Coordenadas c6C = new Coordenadas();
        c6C.setLatitude(10.8);
        c6C.setLongitude(10.5);
        c6.setCoordenadas(c6C);
        c6.setEmail("adriana@live.pt");
        c6.setPassword("adri123");
        c6.setMorada("Rua do Cliente nº6");
        //c6.setHist = 
        c6.setNifC("987567009");
        j1.registaUtilizador(c6);
        
        //7
        Cliente c7 = new Cliente();
        c7.setId("c7234");
        c7.setNome("Andre Sousa");
        Coordenadas c7C = new Coordenadas();
        c7C.setLatitude(2.8);
        c7C.setLongitude(2.5);
        c7.setCoordenadas(c7C);
        c7.setEmail("sousaandre@gmail.com");
        c7.setPassword("sousa123");
        c7.setMorada("Rua do Cliente nº7");
        //c7.setHist = 
        c7.setNifC("000400029");
        j1.registaUtilizador(c7);
        
        //8
        Cliente c8 = new Cliente();
        c8.setId("c9283");
        c8.setNome("Shahzod Yusupov");
        Coordenadas c8C = new Coordenadas();
        c8C.setLatitude(19.8);
        c8C.setLongitude(19.6);
        c8.setCoordenadas(c1C);
        c8.setEmail("shahzod@gmail.com");
        c8.setPassword("yusupov123");
        c8.setMorada("Rua do Cliente nº8");
        //c8.setHist = 
        c8.setNifC("213243562");
        j1.registaUtilizador(c8);
        
        //9
        Cliente c9 = new Cliente();
        c9.setId("c9999");
        c9.setNome("Ines Rute");
        Coordenadas c9C = new Coordenadas();
        c9C.setLatitude(9.9);
        c9C.setLongitude(1.5);
        c9.setCoordenadas(c9C);
        c9.setEmail("ruteines@gmail.com");
        c9.setPassword("ines123");
        c9.setMorada("Rua do Cliente nº9");
        //c9.setHist = 
        c9.setNifC("99989097");
        j1.registaUtilizador(c9);
        
        //10
        Cliente c10 = new Cliente();
        c10.setId("10122");
        c10.setNome("Rute Rita");
        Coordenadas c10C = new Coordenadas();
        c10C.setLatitude(1.6);
        c10C.setLongitude(16.6);
        c10.setCoordenadas(c10C);
        c10.setEmail("rutinha@gmail.com");
        c10.setPassword("rutee123");
        c10.setMorada("Rua do Cliente nº10");
        //c4.setHist = 
        c10.setNifC("998878899");
        j1.registaUtilizador(c10);
        
        //11
        Cliente c11 = new Cliente();
        c11.setId("c8877");
        c11.setNome("Jorge Jesus");
        Coordenadas c11C = new Coordenadas();
        c11C.setLatitude(30.8);
        c11C.setLongitude(13.5);
        c11.setCoordenadas(c11C);
        c11.setEmail("jj@gmail.com");
        c11.setPassword("jjesus123");
        c11.setMorada("Rua do Cliente nº11");
        //c11.setHist = 
        c11.setNifC("311111111");
        j1.registaUtilizador(c11);
        
        //12
        Cliente c12 = new Cliente();
        c12.setId("c122221");
        c12.setNome("Leonel Messi");
        Coordenadas c12C = new Coordenadas();
        c12C.setLatitude(12.8);
        c12C.setLongitude(22.5);
        c12.setCoordenadas(c12C);
        c12.setEmail("messi@hotmail.com");
        c12.setPassword("messi123");
        c12.setMorada("Rua do Cliente nº12");
        //c12.setHist = 
        c12.setNifC("329098909");
        j1.registaUtilizador(c12);
        
        //13
        Cliente c13 = new Cliente();
        c13.setId("c98778");
        c13.setNome("Cristiano Ronaldo");
        Coordenadas c13C = new Coordenadas();
        c13C.setLatitude(32.8);
        c13C.setLongitude(34.5);
        c13.setCoordenadas(c13C);
        c13.setEmail("cr7@gmail.com");
        c13.setPassword("cr7");
        c13.setMorada("Rua do Cliente nº13");
        //c13.setHist = 
        c13.setNifC("777777770");
        j1.registaUtilizador(c13);
        
        //14
        Cliente c14 = new Cliente();
        c14.setId("c41004");
        c14.setNome("Maria Leal");
        Coordenadas c14C = new Coordenadas();
        c14C.setLatitude(9.4);
        c14C.setLongitude(5.6);
        c14.setCoordenadas(c14C);
        c14.setEmail("marialeal@gmail.com");
        c14.setPassword("marialeal123");
        c14.setMorada("Rua do Cliente nº14");
        //c14.setHist = 
        c14.setNifC("009098900");
        j1.registaUtilizador(c14);
        
        //15
        Cliente c15 = new Cliente();
        c15.setId("c15567");
        c15.setNome("Rosa Rio");
        Coordenadas c15C = new Coordenadas();
        c15C.setLatitude(50.8);
        c15C.setLongitude(15.5);
        c15.setCoordenadas(c15C);
        c15.setEmail("rosario@gmail.com");
        c15.setPassword("rosario123");
        c15.setMorada("Rua do Cliente nº15");
        //c15.setHist = 
        c15.setNifC("333254255");
        j1.registaUtilizador(c15);
        
        //16
        Cliente c16 = new Cliente();
        c16.setId("c666");
        c16.setNome("Antonio Costa");
        Coordenadas c16C = new Coordenadas();
        c16C.setLatitude(16.8);
        c16C.setLongitude(42.5);
        c16.setCoordenadas(c16C);
        c16.setEmail("antoniocosta@gmail.com");
        c16.setPassword("ac123");
        c16.setMorada("Rua do Cliente nº16");
        //c16.setHist = 
        c16.setNifC("098765432");
        j1.registaUtilizador(c16);
        
        //17
        Cliente c17 = new Cliente();
        c17.setId("c03824");
        c17.setNome("Marisa Matias");
        Coordenadas c17C = new Coordenadas();
        c17C.setLatitude(17.8);
        c17C.setLongitude(17.5);
        c17.setCoordenadas(c17C);
        c17.setEmail("mariasaaa@gmail.com");
        c17.setPassword("marisa123");
        c17.setMorada("Rua do Cliente nº17");
        //c17.setHist = 
        c17.setNifC("329098909");
        j1.registaUtilizador(c17);
        
        //18
        Cliente c18 = new Cliente();
        c18.setId("c978221");
        c18.setNome("Vera Lucia");
        Coordenadas c18C = new Coordenadas();
        c18C.setLatitude(18.9);
        c18C.setLongitude(18.5);
        c18.setCoordenadas(c18C);
        c18.setEmail("vera@gmail.com");
        c18.setPassword("luciaaa123");
        c18.setMorada("Rua do Cliente nº18");
        //c18.setHist = 
        c18.setNifC("888767880");
        j1.registaUtilizador(c18);
        
        //19
        Cliente c19 = new Cliente();
        c19.setId("c0909");
        c19.setNome("Eduardo Madeira");
        Coordenadas c19C = new Coordenadas();
        c19C.setLatitude(60.8);
        c19C.setLongitude(77.5);
        c19.setCoordenadas(c19C);
        c19.setEmail("edumad@gmail.com");
        c19.setPassword("madeira123");
        c19.setMorada("Rua do Cliente nº19");
        //c219.setHist = 
        c19.setNifC("096468293");
        j1.registaUtilizador(c19);
        
        //20
        Cliente c20 = new Cliente();
        c20.setId("c777666");
        c20.setNome("Pedro Alves");
        Coordenadas c20C = new Coordenadas();
        c20C.setLatitude(40.8);
        c20C.setLongitude(42.5);
        c20.setCoordenadas(c20C);
        c20.setEmail("alves@gmail.com");
        c20.setPassword("pedroalves123");
        c20.setMorada("Rua do Cliente nº20");
        //c20.setHist = 
        c20.setNifC("101010304");
        j1.registaUtilizador(c20);
        
        //LOJAS
        
        //1
        Loja l1 = new Loja();
        l1.setId("l1110");
        l1.setNome("Sport Zone");
        Coordenadas l1L = new Coordenadas();
        l1L.setLatitude(11.1);
        l1L.setLongitude(12.2);
        l1.setCoordenadas(l1L);
        l1.setEmail("sportzone@gmail.com");
        l1.setPassword("sp123");
        l1.setMorada("Rua da Loja nº1");
        l1.setEncParaEntrega(true);
        l1.setTamanhoFila(9);
        j1.registaUtilizador(l1);
        
        //2
        Loja l2 = new Loja();
        l2.setId("l2210");
        l2.setNome("Burger Rei");
        Coordenadas l2L = new Coordenadas();
        l2L.setLatitude(12.1);
        l2L.setLongitude(11.2);
        l2.setCoordenadas(l2L);
        l2.setEmail("burgerei@gmail.com");
        l2.setPassword("burgerei123");
        l2.setMorada("Rua da Loja nº2");
        l2.setEncParaEntrega(true);
        l2.setTamanhoFila(15);
        j1.registaUtilizador(l2);
        
        //3
        Loja l3 = new Loja();
        l3.setId("l3370");
        l3.setNome("Livraria Minho");
        Coordenadas l3L = new Coordenadas();
        l3L.setLatitude(54.1);
        l3L.setLongitude(31.2);
        l3.setCoordenadas(l3L);
        l3.setEmail("livrariaminho@gmail.com");
        l3.setPassword("livros123");
        l3.setMorada("Rua da Loja nº3");
        l3.setEncParaEntrega(true);
        l3.setTamanhoFila(1);
        j1.registaUtilizador(l3);
        
        //4
        Loja l4 = new Loja();
        l4.setId("l4532");
        l4.setNome("Adidas");
        Coordenadas l4L = new Coordenadas();
        l4L.setLatitude(34.1);
        l4L.setLongitude(33.2);
        l4.setCoordenadas(l4L);
        l4.setEmail("adidas@gmail.com");
        l4.setPassword("adidas123");
        l4.setMorada("Rua da Loja nº4");
        l4.setEncParaEntrega(false);
        l4.setTamanhoFila(0);
        j1.registaUtilizador(l4);

        //5
        Loja l5 = new Loja();
        l5.setId("l0005");
        l5.setNome("Maxmat");
        Coordenadas l5L = new Coordenadas();
        l5L.setLatitude(19.1);
        l5L.setLongitude(91.2);
        l5.setCoordenadas(l5L);
        l5.setEmail("maxmat@gmail.com");
        l5.setPassword("maxmat123");
        l5.setMorada("Rua da Loja nº5");
        l5.setEncParaEntrega(true);
        l5.setTamanhoFila(0);
        j1.registaUtilizador(l5);
        
        //6
        Loja l6 = new Loja();
        l6.setId("l0006");
        l6.setNome("TekLife");
        Coordenadas l6L = new Coordenadas();
        l6L.setLatitude(56.1);
        l6L.setLongitude(66.6);
        l6.setCoordenadas(l6L);
        l6.setEmail("teklife@gmail.com");
        l6.setPassword("teklife");
        l6.setMorada("Rua da Loja nº6");
        l6.setEncParaEntrega(true);
        l6.setTamanhoFila(8);
        j1.registaUtilizador(l6);
        
        //7
        Loja l7 = new Loja();
        l7.setId("l0007");
        l7.setNome("Tintas Hue");
        Coordenadas l7L = new Coordenadas();
        l7L.setLatitude(57.9);
        l7L.setLongitude(71.2);
        l7.setCoordenadas(l7L);
        l7.setEmail("tintas@gmail.com");
        l7.setPassword("tintas123");
        l7.setMorada("Rua da Loja nº7");
        l7.setEncParaEntrega(false);
        l7.setTamanhoFila(0);
        j1.registaUtilizador(l7);
        
        //8
        Loja l8 = new Loja();
        l8.setId("l4532");
        l8.setNome("Sarah Fashion");
        Coordenadas l8L = new Coordenadas();
        l8L.setLatitude(84.1);
        l8L.setLongitude(81.8);
        l3.setCoordenadas(l8L);
        l8.setEmail("sarahfashion@gmail.com");
        l8.setPassword("sarahfashion123");
        l8.setMorada("Rua da Loja nº8");
        l8.setEncParaEntrega(true);
        l8.setTamanhoFila(0);
        j1.registaUtilizador(l8);
        
        //9
        Loja l9 = new Loja();
        l9.setId("l0009");
        l9.setNome("Verniz Nails");
        Coordenadas l9L = new Coordenadas();
        l9L.setLatitude(96.1);
        l9L.setLongitude(91.2);
        l9.setCoordenadas(l9L);
        l9.setEmail("nails@gmail.com");
        l9.setPassword("nails123");
        l9.setMorada("Rua da Loja nº9");
        l9.setEncParaEntrega(false);
        l9.setTamanhoFila(0);
        j1.registaUtilizador(l9);
        
        //10
        Loja l10 = new Loja();
        l10.setId("l0010");
        l10.setNome("Meias lda");
        Coordenadas l10L = new Coordenadas();
        l10L.setLatitude(54.0);
        l10L.setLongitude(55.2);
        l10.setCoordenadas(l10L);
        l10.setEmail("meias@gmail.com");
        l10.setPassword("meias123");
        l10.setMorada("Rua da Loja nº10");
        l10.setEncParaEntrega(false);
        l10.setTamanhoFila(0);
        j1.registaUtilizador(l10);
        
        //11
        Loja l11 = new Loja();
        l11.setId("l4532");
        l11.setNome("Sushi Co");
        Coordenadas l11L = new Coordenadas();
        l11L.setLatitude(100.1);
        l11L.setLongitude(102.2);
        l11.setCoordenadas(l11L);
        l11.setEmail("sushi@gmail.com");
        l11.setPassword("sushi123");
        l11.setMorada("Rua da Loja nº11");
        l11.setEncParaEntrega(true);
        l11.setTamanhoFila(7);
        j1.registaUtilizador(l11);
        
        //12
        Loja l12 = new Loja();
        l12.setId("l0012");
        l12.setNome("Jogos S.A");
        Coordenadas l12L = new Coordenadas();
        l12L.setLatitude(107.0);
        l12L.setLongitude(102.2);
        l12.setCoordenadas(l12L);
        l12.setEmail("jogos@gmail.com");
        l12.setPassword("jogos123");
        l12.setMorada("Rua da Loja nº12");
        l12.setEncParaEntrega(true);
        l12.setTamanhoFila(2);
        j1.registaUtilizador(l12);
        
        //13
        Loja l13 = new Loja();
        l13.setId("l0013");
        l13.setNome("Decoracao Int");
        Coordenadas l13L = new Coordenadas();
        l13L.setLatitude(94.0);
        l13L.setLongitude(25.2);
        l13.setCoordenadas(l13L);
        l13.setEmail("decor@gmail.com");
        l13.setPassword("decor123");
        l13.setMorada("Rua da Loja nº13");
        l13.setEncParaEntrega(true);
        l13.setTamanhoFila(2);
        j1.registaUtilizador(l13);
        
        //14
        Loja l14 = new Loja();
        l14.setId("l''14");
        l14.setNome("Pizzas Yummy");
        Coordenadas l14L = new Coordenadas();
        l14L.setLatitude(103.3);
        l14L.setLongitude(98.2);
        l14.setCoordenadas(l14L);
        l14.setEmail("pizzaria@gmail.com");
        l14.setPassword("pizza123");
        l14.setMorada("Rua da Loja nº14");
        l14.setEncParaEntrega(true);
        l14.setTamanhoFila(0);
        j1.registaUtilizador(l14);
        
        //15
        Loja l15 = new Loja();
        l15.setId("l0015");
        l15.setNome("Moveis IQEA");
        Coordenadas l15L = new Coordenadas();
        l15L.setLatitude(93.0);
        l15L.setLongitude(105.2);
        l15.setCoordenadas(l15L);
        l15.setEmail("iqea@gmail.com");
        l15.setPassword("iqea123");
        l15.setMorada("Rua da Loja nº15");
        l15.setEncParaEntrega(true);
        l15.setTamanhoFila(0);
        j1.registaUtilizador(l15);
        
        //16
        Loja l16 = new Loja();
        l16.setId("l0016");
        l16.setNome("Farmacia Gomes");
        Coordenadas l16L = new Coordenadas();
        l16L.setLatitude(35.9);
        l16L.setLongitude(113.5);
        l16.setCoordenadas(l16L);
        l16.setEmail("farmaciag@gmail.com");
        l16.setPassword("farmgomes123");
        l16.setMorada("Rua da Loja nº16");
        l16.setEncParaEntrega(true);
        l16.setTamanhoFila(1);
        j1.registaUtilizador(l16);
        
        //17
        Loja l17 = new Loja();
        l17.setId("l0017");
        l17.setNome("Mundo Animal");
        Coordenadas l17L = new Coordenadas();
        l17L.setLatitude(95.6);
        l17L.setLongitude(44.4);
        l17.setCoordenadas(l17L);
        l17.setEmail("mundoanimal@gmail.com");
        l17.setPassword("mundoanimal123");
        l17.setMorada("Rua da Loja nº17");
        l17.setEncParaEntrega(true);
        l17.setTamanhoFila(2);
        j1.registaUtilizador(l17);
        
        //18
        Loja l18 = new Loja();
        l18.setId("l0018");
        l18.setNome("Perfumes Op");
        Coordenadas l18L = new Coordenadas();
        l18L.setLatitude(123.0);
        l18L.setLongitude(102.2);
        l18.setCoordenadas(l18L);
        l18.setEmail("perfumaria@gmail.com");
        l18.setPassword("perfumes123");
        l18.setMorada("Rua da Loja nº18");
        l18.setEncParaEntrega(true);
        l18.setTamanhoFila(0);
        j1.registaUtilizador(l18);
        
        //19
        Loja l19 = new Loja();
        l19.setId("l0019");
        l19.setNome("Nespresso");
        Coordenadas l19L = new Coordenadas();
        l19L.setLatitude(51.1);
        l19L.setLongitude(54.1);
        l19.setCoordenadas(l19L);
        l19.setEmail("nespresso@gmail.com");
        l19.setPassword("nespresso123");
        l19.setMorada("Rua da Loja nº19");
        l19.setEncParaEntrega(false);
        l19.setTamanhoFila(0);
        j1.registaUtilizador(l19);
        
        //20
        Loja l20 = new Loja();
        l20.setId("l0020");
        l20.setNome("Continente");
        Coordenadas l20L = new Coordenadas();
        l20L.setLatitude(24.0);
        l20L.setLongitude(52.2);
        l20.setCoordenadas(l20L);
        l20.setEmail("continente@gmail.com");
        l20.setPassword("continente123");
        l20.setMorada("Rua da Loja nº20");
        l20.setEncParaEntrega(true);
        l20.setTamanhoFila(15);
        j1.registaUtilizador(l20);
        
        //VOLUNTÁRIOS

        //1
        Voluntario v1 = new Voluntario();
        v1.setId("v1232");
        v1.setNome("Andreia Moreira");
        Coordenadas v1V = new Coordenadas();
        v1V.setLatitude(22.0);
        v1V.setLongitude(22.2);
        v1.setCoordenadas(v1V);
        v1.setEmail("andreia@gmail.com");
        v1.setPassword("andreia123");
        v1.setMorada("Rua do Voluntario nº1");
        v1.setClassificacao(3.2);
        v1.setRaio(3);
        v1.setEMedicas(false);
        v1.setDisponivel(false);
        j1.registaUtilizador(v1);

        //2
        Voluntario v2 = new Voluntario();
        v2.setId("v2347");
        v2.setNome("Lopes Antunes");
        Coordenadas v2V = new Coordenadas();
        v2V.setLatitude(202.0);
        v2V.setLongitude(202.2);
        v2.setCoordenadas(v2V);
        v2.setEmail("lopes@gmail.com");
        v2.setPassword("lopes123");
        v2.setMorada("Rua do Voluntario nº2");
        v2.setClassificacao(7.8);
        v2.setRaio(7);
        v2.setEMedicas(true);
        v2.setDisponivel(true);
        j1.registaUtilizador(v2);

        //3
        Voluntario v3 = new Voluntario();
        v3.setId("v2143");
        v3.setNome("Priscila Olinda");
        Coordenadas v3V = new Coordenadas();
        v3V.setLatitude(223.0);
        v3V.setLongitude(231.2);
        v3.setCoordenadas(v3V);
        v3.setEmail("priscila@gmail.com");
        v3.setPassword("priscila123");
        v3.setMorada("Rua do Voluntario nº3");
        v3.setClassificacao(3.4);
        v3.setRaio(5);
        v3.setEMedicas(false);
        v3.setDisponivel(true);
        j1.registaUtilizador(v3);

        //4
        Voluntario v4 = new Voluntario();
        v4.setId("v6784");
        v4.setNome("Ivo Rocha");
        Coordenadas v4V = new Coordenadas();
        v4V.setLatitude(222.3);
        v4V.setLongitude(223.2);
        v4.setCoordenadas(v4V);
        v4.setEmail("ivo@gmail.com");
        v4.setPassword("ivo123");
        v4.setMorada("Rua do Voluntario nº4");
        v4.setClassificacao(5.4);
        v4.setRaio(6);
        v4.setEMedicas(true);
        v4.setDisponivel(true);
        j1.registaUtilizador(v4);

        //EMPRESAS TRANSPORTADORAS
        
        //1
        EmpresaTransp d1 = new EmpresaTransp();
        d1.setId("d7363");
        d1.setNome("Transportes Lopes");
        Coordenadas d1D = new Coordenadas();
        d1D.setLatitude(164.0);
        d1D.setLongitude(149.2);
        d1.setCoordenadas(d1D);
        d1.setEmail("transplopes@gmail.com");
        d1.setPassword("transplopes123");
        d1.setMorada("Rua da Transportadora nº1");
        d1.setClassificacao(7.4);
        d1.setRaio(11);
        d1.setEMedicas(true);
        d1.setDisponivel(true);
        d1.setNifE(123321221);
        d1.setKms(12312.2);
        d1.setPrecoKm(0.65);
        j1.registaUtilizador(d1);
   
        
        //2
        EmpresaTransp d2 = new EmpresaTransp();
        d2.setId("d1932");
        d2.setNome("Transportes Pinto");
        Coordenadas d2D = new Coordenadas();
        d2D.setLatitude(324.0);
        d2D.setLongitude(399.2);
        d2.setCoordenadas(d2D);
        d2.setEmail("transpinto@gmail.com");
        d2.setPassword("transpinto123");
        d2.setMorada("Rua da Transportadora nº2");
        d2.setClassificacao(6.4);
        d2.setRaio(10);
        d2.setEMedicas(false);
        d2.setDisponivel(true);
        d2.setNifE(12999921);
        d2.setKms(19762.1);
        d2.setPrecoKm(0.55);
        j1.registaUtilizador(d2);

        //3
        EmpresaTransp d3 = new EmpresaTransp();
        d3.setId("d3202");
        d3.setNome("JPL Transportes");
        Coordenadas d3D = new Coordenadas();
        d3D.setLatitude(236.3);
        d3D.setLongitude(199.9);
        d3.setCoordenadas(d3D);
        d3.setEmail("jpl@gmail.com");
        d3.setPassword("jpl123");
        d3.setMorada("Rua da Transportadora nº3");
        d3.setClassificacao(7.1);
        d3.setRaio(12);
        d3.setEMedicas(true);
        d3.setDisponivel(true);
        d3.setNifE(987456767);
        d3.setKms(20865.4);
        d3.setPrecoKm(0.60);
        j1.registaUtilizador(d3);
        
        //4
        EmpresaTransp d4 = new EmpresaTransp();
        d4.setId("d0912");
        d4.setNome("Super Transportes");
        Coordenadas d4D = new Coordenadas();
        d4D.setLatitude(214.0);
        d4D.setLongitude(287.2);
        d4.setCoordenadas(d4D);
        d4.setEmail("transuper@gmail.com");
        d4.setPassword("super123");
        d4.setMorada("Rua da Transportadora nº4");
        d4.setClassificacao(6.6);
        d4.setRaio(10);
        d4.setEMedicas(true);
        d4.setDisponivel(false);
        d4.setNifE(13325971);
        d4.setKms(9429);
        d4.setPrecoKm(0.62);
        j1.registaUtilizador(d4);
        //5
        EmpresaTransp d5 = new EmpresaTransp();
        d5.setId("d3912");
        d5.setNome("Super Entrega");
        Coordenadas d5D = new Coordenadas();
        d5D.setLatitude(214.0);
        d5D.setLongitude(700.2);
        d5.setCoordenadas(d5D);
        d5.setEmail("superEntre@gmail.com");
        d5.setPassword("entre123");
        d5.setMorada("Rua da Transportadora nº5");
        d5.setClassificacao(5.6);
        d5.setRaio(13);
        d5.setEMedicas(true);
        d5.setDisponivel(false);
        d5.setNifE(133525971);
        d5.setKms(20);
        d5.setPrecoKm(0.75);
        j1.registaUtilizador(d5);
        //6
        EmpresaTransp d6 = new EmpresaTransp();
        d6.setId("d1612");
        d6.setNome("Entregas Ja");
        Coordenadas d6D = new Coordenadas();
        d6D.setLatitude(2.0);
        d6D.setLongitude(64.2);
        d6.setCoordenadas(d6D);
        d6.setEmail("entreja@gmail.com");
        d6.setPassword("enja124");
        d6.setMorada("Rua da Transportadora nº6");
        d6.setClassificacao(4.6);
        d6.setRaio(15);
        d6.setEMedicas(false);
        d6.setDisponivel(false);
        d6.setNifE(17325971);
        d6.setKms(20);
        d6.setPrecoKm(0.62);
        j1.registaUtilizador(d6);
        //7
        EmpresaTransp d7 = new EmpresaTransp();
        d7.setId("d0912");
        d7.setNome("Entre Entregas");
        Coordenadas d7D = new Coordenadas();
        d7D.setLatitude(514.0);
        d7D.setLongitude(387.2);
        d7.setCoordenadas(d7D);
        d7.setEmail("entreentre@gmail.com");
        d7.setPassword("entent523");
        d7.setMorada("Rua da Transportadora nº7");
        d7.setClassificacao(8.6);
        d7.setRaio(15);
        d7.setEMedicas(true);
        d7.setDisponivel(true);
        d7.setNifE(17325971);
        d7.setKms(250);
        d7.setPrecoKm(0.60);
        j1.registaUtilizador(d7);
        //8
        EmpresaTransp d8 = new EmpresaTransp();
        d8.setId("d7812");
        d8.setNome("Ja A Porta");
        Coordenadas d8D = new Coordenadas();
        d8D.setLatitude(514.0);
        d8D.setLongitude(87.2);
        d8.setCoordenadas(d8D);
        d8.setEmail("japorta@gmail.com");
        d8.setPassword("ja123porta");
        d8.setMorada("Rua da Transportadora nº8");
        d8.setClassificacao(7.0);
        d8.setRaio(14);
        d8.setEMedicas(false);
        d8.setDisponivel(false);
        d8.setNifE(16855971);
        d8.setKms(300);
        d8.setPrecoKm(0.65);
        j1.registaUtilizador(d8);
        //9
        EmpresaTransp d9 = new EmpresaTransp();
        d9.setId("d9012");
        d9.setNome("Transportes Mario");
        Coordenadas d9D = new Coordenadas();
        d9D.setLatitude(150.0);
        d9D.setLongitude(176.2);
        d9.setCoordenadas(d9D);
        d9.setEmail("transmario@gmail.com");
        d9.setPassword("trans123");
        d9.setMorada("Rua da Transportadora nº9");
        d9.setClassificacao(6.6);
        d9.setRaio(10);
        d9.setEMedicas(true);
        d9.setDisponivel(false);
        d9.setNifE(13215487);
        d9.setKms(9429);
        d9.setPrecoKm(0.62);
        j1.registaUtilizador(d9);
       
        // 10
        /*
        EmpresaTransp d10 = new EmpresaTransp();
        d10.setId("d8012");
        d10.setNome("Em Casa Ja");
        Coordenadas d10D = new Coordenadas();
        d10D.setLatitude(250.0);
        d10D.setLongitude(276.2);
        d10.setCoordenadas(d10D);
        d10.setEmail("ecasaj@gmail.com");
        d10.setPassword("ecj123");
        d10.setMorada("Rua da Transportadora nº10");
        d10.setClassificacao(7.6);
        d10.setRaio(18);
        d10.setEMedicas(false);
        d10.setDisponivel(false);
        d10.setNifE(19825555);
        d10.setKms(9529);
        d10.setPrecoKm(0.60);
        j1.registaUtilizador(d10);
        */
        //ENCOMENDAS
        
        //1
        LocalDate df1 = LocalDate.of(2018,Month.JULY,29);
        Encomenda e1 = new Encomenda();
        e1.setIdEncomenda("e0972");
        e1.setData(df1);
        //e1.transp
        //e1.loja
        //e1.clie
        e1.setEncAceite(true);
        //e1.
        j1.addEncomenda(e1);
     
        
        
        
        
        
        
        
        String file_name = "trazaqui.data";
        try{
            j1.gravar(file_name); 
        }
        catch(IOException e){
            System.out.println("Erro: " + e);
        }
        
        
        TrazAqui dados = null;
        
        try {
            TrazAqui bd = TrazAqui.initApp(file_name);
            System.out.println("DADOS DA BD");
            System.out.println(bd.toString());
            }
        catch (IOException e) {
            dados = new TrazAqui();
            System.out.println("Não consegui ler os dados!\nErro de leitura.");
        }
        catch (ClassNotFoundException e) {
            dados = new TrazAqui();
            System.out.println("Não consegui ler os dados!\nFicheiro com formato desconhecido.");
        }
        catch (ClassCastException e) {
            dados = new TrazAqui();
            System.out.println("Não consegui ler os dados!\nErro de formato.");
        }
    
    }
    
}