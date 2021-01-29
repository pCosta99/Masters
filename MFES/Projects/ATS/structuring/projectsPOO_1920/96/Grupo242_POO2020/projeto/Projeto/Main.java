package Projeto;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

public class Main{

        public static void main(String[] args) {
            TrazAqui model = new TrazAqui();
            //try {
            //    model = TrazAqui();
            //}
            Controller controller = new Controller(model);
            controller.run();
        }


        public void populate() throws FileNotFoundException, UnsupportedEncodingException {

            //o estado da aplicação deverá estar pré-carregado com um conjunto de dados significativos,
            //que permita testar toda a aplicação no dia da entrega.

            Utilizador utilizador1 = new Utilizador("u01","Jose","123",new Posicao(3,4));
            Utilizador utilizador2 = new Utilizador("u02","Maria","1234",new Posicao(4,5));
            Utilizador utilizador3 = new Utilizador("u03","Luis","12345",new Posicao(6,7));
            Utilizador utilizador4 = new Utilizador("u04","Portuga","123456",new Posicao(8,9));

            Voluntario voluntario1 = new Voluntario("v05","Pedro","ksa",new Posicao(1,1),2);
            Voluntario voluntario2 = new Voluntario("v06","Ines","ksa1",new Posicao(2,1),3);
            Voluntario voluntario3 = new Voluntario("v07","Sofia","ksa2",new Posicao(3,1),4);
            Voluntario voluntario4 = new Voluntario("v08","Gustavo","ksaPortuga",new Posicao(4,1),5);

            Empresa empresa1 = new Empresa(new Posicao(10,10),250300500,25,25);
            Empresa empresa2 = new Empresa(new Posicao(15,15),250300999,30,30);

            Loja loja1 = new Loja("l01","Loja do Manel","lojnha",new Posicao(7,7),null);

            LinhaEncomendas linhaEncomendas1 = new LinhaEncomendas("p01","Farinha",50,100);
            LinhaEncomendas linhaEncomendas2 = new LinhaEncomendas("p02","ovos",10,200);
            LinhaEncomendas linhaEncomendas3 = new LinhaEncomendas("p03","Milho",30,300);
            LinhaEncomendas linhaEncomendas4 = new LinhaEncomendas("p04","Cogumelos",40,400);
            LinhaEncomendas linhaEncomendas5 = new LinhaEncomendas("p05","Trigo",60,500);
            LinhaEncomendas linhaEncomendas6 = new LinhaEncomendas("p06","Acucar",70,600);

            ArrayList<LinhaEncomendas> detalhesEncomenda = new ArrayList<>();

            detalhesEncomenda.add(linhaEncomendas1);
            detalhesEncomenda.add(linhaEncomendas2);
            detalhesEncomenda.add(linhaEncomendas3);
            detalhesEncomenda.add(linhaEncomendas4);
            detalhesEncomenda.add(linhaEncomendas5);
            detalhesEncomenda.add(linhaEncomendas6);

            Encomenda encomenda1 = new Encomenda("e01","u03","l01","50",detalhesEncomenda);


            PrintWriter writer = new PrintWriter("the-file-name.txt", "UTF-8");
            writer.println(utilizador1);
            writer.println(utilizador2);
            writer.println(utilizador3);
            writer.println(utilizador4);

            writer.println(voluntario1);
            writer.println(voluntario2);
            writer.println(voluntario3);
            writer.println(voluntario4);

            writer.println(empresa1);
            writer.println(empresa2);
            writer.close();
        }
}
