import java.time.*;
import static org.junit.Assert.*;
import org.junit.Test;
import java.util.Scanner;
import java.io.*;
import Exceptions.*;
public class teste
{

    public static void teste1()
    {
        /*LocalDate dataDe = LocalDate.parse("2000-06-02");
        System.out.println(dataDe.toString());
        LocalDate dataAte = LocalDate.parse("2001-06-02");
        System.out.println(dataAte.isAfter(dataDe));
        System.out.println("e"+15);
        String cod="e"+15;
        System.out.println(cod);*/
        Central c = new Central ();
        String res;
        int i;
        double b;
        try{
            System.out.println("\nAdicionando utilizador u40");
                c.adicionaUtilizador("u40,Luis,99.15073,-45.263195");
            System.out.println("\nAdicionando loja l29 e l40");
                c.adicionaLoja("l29,TV e TelecomunicaÃ§Ãµes,39.627502,33.60112");
                c.adicionaLoja("l40,TV e TelecomunicaÃ§Ãµes,39.627502,33.60112");
            System.out.println("\nShowing Catalogo l29 antes de adiçao");
                res=c.showCatalogo("l29");
                System.out.println(res);
                assertEquals("",res);
            System.out.println("Adicionando produtos ao catalogo de loja29");
                c.adicionaProduto("l29","p1","Plasma",300.0);
                c.adicionaProduto("l29","p2","DvDTitanic",30.0);
            System.out.println("Showing Catalogo apos adiçao");
                res=c.showCatalogo("l29");
                System.out.println(res);
                assertEquals("Produto:p1,Plasma,300.0\nProduto:p2,DvDTitanic,30.0\n",res);
            System.out.println("\nMostrando showLojas:");
                res=c.showLojas();
                System.out.println(res);
                assertEquals("Loja:l29,TV e TelecomunicaÃ§Ãµes,39.627502,33.601120\nLoja:l40,TV e TelecomunicaÃ§Ãµes,39.627502,33.601120\n",res);
            System.out.println("\nAdicionando Voluntario v73");
                c.adicionaVoluntario("v73,Rui Pedro Gomes Coelho,-45.424522,-79.517136,1500.0");
            System.out.println("\nAdicionando Transportadora t26");
                c.adicionaTransportadora("t26,TERCARGO - TRANSITÃRIOS,68.8905,-38.96917,539968772,1190.0,15");
            System.out.println("\nAdicionando encomenda e6813 e e6000");
                c.adicionaEncomenda("e6813,u40,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");
                c.adicionaEncomenda("e6000,u40,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");
                //c.adicionaEncomenda("e6813,u30,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");
            System.out.println("\nAdicionando proposta de t26 para encomenda e6813");
                c.adicionaProposta("t26","e6813","30.0");
            System.out.println("\nMostrando propostas de u40(e6813)");
                res=c.verPropostas("u40");
                System.out.println(res);
                assertEquals("Encomenda:e6813 Entregador:t26 Preco:30.0\n",res);
            System.out.println("\nAceitandoProposta u40 de t26");
                c.aceitarProposta("u40","t26","e6813");
            System.out.println("\nVoluntario v73 aceita entregar e6000");
                c.aceitarEntregar("e6000","v73");
            System.out.println("\nVendo pontos v73 antes da entrega");
                b=c.getPontos("v73");
                System.out.println(b);
                //assertEquals(0,i);
            System.out.println("\nadicionando entregas de e6000 e e6813(preco 30,data 2020-05-30)");
                c.adicionarEntrega("e6000","v73","2020-05-30",0);
                c.adicionarEntrega("e6813","t26","2020-05-30",30.0);
            System.out.println("\nVendo entregas de 2000 a 2030(todas)");
                res=c.verEntregasTempo("2000-01-01","2030-05-02");
                System.out.println(res);
                assertEquals("Entrega:e6813,t26,u40,2020-05-30,30.0\nEntrega:e6000,v73,u40,2020-05-30,0.0\n",res);
            System.out.println("\nVendo entregas de t26");
                res=c.verEntregasDe("t26");
                System.out.println(res);
                assertEquals("Entrega:e6813,t26,u40,2020-05-30,30.0\n",res);
            System.out.println("\nVendo entregas de u40");
                res=c.minhasEntregas("u40");
                System.out.println(res);
                assertEquals("Entrega:e6813,t26,u40,2020-05-30,30.0\nEntrega:e6000,v73,u40,2020-05-30,0.0\n",res);
            System.out.println("\nPontos de v73 apos entrega");
                b=c.getPontos("v73");
                System.out.println(b);
                //assertTrue(i!=0);
            System.out.println("\nClassificaçao de t26 antes e depois de votar 10");
                b=c.veClassificacao("t26");
                System.out.println(b);
                assertTrue(b==0);
                c.classifica("t26",10);
                b=c.veClassificacao("t26");
                System.out.println(b);
                assertTrue(b==10);
            System.out.println("\nVer faturacao de t26");
                b=c.faturacaoTransportadora("t26","2000-01-01","2030-05-02");
                System.out.println(b);
                assertTrue(b==30);
       }catch (Exception e){e.printStackTrace();}
    }
    public static void teste2(){
       Central c=new Central();
       String s1=null;
       try{
       c.adicionaUtilizador("u40,Luis,99.15073,-45.263195");
       c.adicionaLoja("l29,TV e TelecomunicaÃ§Ãµes,39.627502,33.60112");
       c.adicionaProduto("l29","p1","Plasma",300.0);
       c.adicionaProduto("l29","pm2","Vitaminas",30.0);
       c.adicionaEncomenda("u40","l29","p1,Plasma,300,2",30);
       c.adicionaEncomenda("u40","l29","pm2,Vitaminas,30,2",30);
       c.adicionaVoluntario("v73,Rui Pedro Gomes Coelho,-45.424522,-79.517136,1500.0");
       System.out.println("Antes de por Certificado de Medicamento:");
       s1=c.encQuePossoEntregar("v73");
       System.out.println(s1);
       c.aceitaMedicamentos("v73",true);
       System.out.println("Depois de por Certificado de Medicamento:");
       s1=c.encQuePossoEntregar("v73");
       System.out.println(s1);
     }catch(Exception e){e.printStackTrace();}
    }
    public static void carregaDados (){
        Central c = new Central();
       try{
       c.load("../logs.txt");
      }catch(Exception e) {e.printStackTrace();}
      try{
        c.adicionaProduto("l29","p1","Plasma",300.0);
        c.adicionaProduto("l58","p2","Computador",300.0);
        c.adicionaProduto("l8","p3","Chocolate",300.0);
        c.adicionaProduto("l83","p4","Arroz",300.0);
        c.adicionaProduto("l13","p5","Caderno",300.0);
        c.adicionaEncomenda("u48","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("u97","l8","p3,Chocolate,300,2",30);
        c.adicionaEncomenda("u82","l83","p4,Arroz,300,2",30);
        c.adicionaEncomenda("u7","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("u80","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("u33","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("u78","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("u40","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("u68","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("u81","l29","p1,Plasma,300,2",30);
        c.adicionaEncomenda("e6029,u48,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6029","30.0");
        c.adicionaEncomenda("e6030,u97,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6030","30.0");
        c.adicionaEncomenda("e6021,u82,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6021","30.0");
        c.adicionaEncomenda("e6022,u7,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6022","30.0");
        c.adicionaEncomenda("e6023,u80,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6023","30.0");
        c.adicionaEncomenda("e6024,u33,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6024","30.0");
        c.adicionaEncomenda("e6025,u78,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6025","30.0");
        c.adicionaEncomenda("e6026,u40,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6026","30.0");
        c.adicionaEncomenda("e6027,u68,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6027","30.0");
        c.adicionaEncomenda("e6028,u81,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");c.adicionaProposta("t26","e6028","30.0");
        c.adicionarEntrega("e1014","t24","2020-05-30",50.0);
        c.adicionarEntrega("e3319","t26","2020-01-30",30.0);
        c.adicionarEntrega("e6465","t9","2020-09-30",30.0);
      }catch(Exception e){e.printStackTrace();}
      System.out.println(c.toString());
      try{
        ObjectOutputStream os=new ObjectOutputStream(new FileOutputStream("../central.obj"));
        os.writeObject(c);
        }catch (FileNotFoundException e){e.printStackTrace();}
        catch (IOException e){e.printStackTrace();}
    }
    public static void testeDistancia(){
       Central c=new Central();
       String s1=null;
       double d;
       try{
         
       c.adicionaTransportadora("t26,TERCARGO - TRANSITÃRIOS,0,0,539968772,1190.0,15");
       c.adicionaTransportadora("t23,TERCARGO - TRANSITÃRIOS,0,0,539968772,1190.0,15");
       
       c.adicionaLoja("l29,TV e TelecomunicaÃ§Ãµes,3,0");
       
       c.adicionaUtilizador("u40,Luis,4,0");
       c.adicionaUtilizador("u20,Luis,5,0");
       
       
       c.adicionaEncomenda("e6813,u40,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");
       c.adicionaEncomenda("e6000,u20,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");
       c.adicionaEncomenda("e6001,u20,l29,77.31938,p34,Farinha de trigo,2.2728467,5.5159483");
       System.out.println(c.verificarEntrega("e6813"));
       //VelMedia=50, tempoEsperaMedia=0.8 , 15=precoPorKm, 1=Fator
       System.out.println(c.precoRotaTransportadora("e6813,e6000,e6001","t26")); //percorre 5km ((5/50 + (0*0.8))*50*15*1) / 3 encomendas
       c.setFila("l29",10);                                                  
       System.out.println(c.precoRotaTransportadora("e6813,e6000,e6001","t26"));
       c.atualizaFator(3,3,true);
       System.out.println(c.precoRotaTransportadora("e6813,e6000,e6001","t26"));
       c.atualizaFator(3,3,false);
       
       //ha uma grande disparidade de valores porque distancias sao pequenas e a fila
       //ocupa muito tempo comparado ao percorrer o percurso
       c.adicionaProposta("t26","e6813","30.0");
       c.aceitarProposta("u40","t26","e6813");
       c.adicionaProposta("t23","e6000","50.0");
       c.aceitarProposta("u20","t23","e6000");
       c.adicionaProposta("t23","e6001","50.0");
       c.aceitarProposta("u20","t23","e6001");
       
       System.out.println(c.verificarEntrega("e6813"));
       
       c.adicionarEntrega("e6000","t23","2020-05-30",50.0);
       c.adicionarEntrega("e6813","t26","2020-05-30",30.0);
       c.adicionarEntrega("e6001","t23","2020-05-30",30.0);
       
       System.out.println(c.verificarEntrega("e6813"));
       
       //System.out.println(c.toString());
       System.out.println(c.top10Transportadoras());
       System.out.println(c.top10Utilizadores());
      
    }catch(Exception e){e.printStackTrace();}
    }
    
}
