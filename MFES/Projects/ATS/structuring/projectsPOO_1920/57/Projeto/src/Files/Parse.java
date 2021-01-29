/**
 * classe que lê um ficheiro de dados e carraga-os para as classes
 */
package Files;

import Model.*;

import java.io.IOException;
import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.TreeSet;

public class Parse implements Serializable, IParse {

    /**
     * Método que lê o ficheiro logs.txt e carrega as estruturas
     *
     * @param c GestTrazAqui
     */
    public void parse(GestTrazAqui c){
        List<String> linhas = lerFicheiro("files/logs.txt");
        String [] linhaPartida;
        Login log;

        for(String linha: linhas) {
            linhaPartida = linha.split(":",2);
            switch (linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]);
                    c.addUser(u);
                    log = new Login(u.getCodigoUtilizador(),u.getCodigoUtilizador(),"Utilizador",u.getName());
                    c.addLogin(log);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    c.addLoja(l);
                    log = new Login(l.getStoreCode(),l.getStoreCode(),"Loja",l.getStoreName());
                    c.addLogin(log);
                    break;
                case "Voluntario":
                    Estafeta v = parseVoluntario(linhaPartida[1]);
                    c.addEstafeta(v);
                    log = new Login(v.getCode(),v.getCode(),"Voluntario",v.getName());
                    c.addLogin(log);
                    break;
                case "Transportadora":
                    Estafeta t = parseTransportadora(linhaPartida[1]);
                    c.addEstafeta(t);
                    log = new Login(t.getCode(),t.getCode(),"Transportadora",t.getName());
                    c.addLogin(log);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    c.addEncomenda(e);
                    break;
                case "Aceite":
                    c.aceitarEncomenda(linhaPartida[1]);
                    break;
            }
        }

        parseProdutosTxt(c);
    }

    /**
     * Método que lê um ficheiro
     *
     * @param filename nome ficheiro
     * @return         list de strings lida
     */
    private List<String> lerFicheiro(String filename){
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(filename));
        } catch (IOException ioex) {
            System.out.println(ioex.getMessage() + "Erro a ler ficheiro");
        }
        return lines;
    }

    /**
     * Divide input e cria um utilizador
     *
     * @param input String lida
     * @return      novo utilizador
     */
    private Utilizador parseUtilizador(String input){
        String [] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        Coordenadas gps = new Coordenadas(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));

        return new Utilizador(codUtilizador,nome,gps,new TreeSet<>(),new TreeSet<>(),new ArrayList<>());
    }

    /**
     * Divide input e cria uma loja
     *
     * @param input String lida
     * @return      nova loja
     */
    private Loja parseLoja(String input){
        String [] campos = input.split(",");
        String storeCode = campos[0];
        String storeName = campos[1];
        Coordenadas gps = new Coordenadas(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));

        return new Loja(storeCode,storeName,gps,false, (Math.random() * 10), (int) (Math.random() * 20), new ArrayList<>(), new ArrayList<>());
    }

    /**
     * Divide input e cria um voluntário
     *
     * @param input String lida
     * @return      novo voluntario
     */
    private Estafeta parseVoluntario(String input){
        String [] campos = input.split(",");
        String voluntaryCode = campos[0];
        String name = campos[1];
        Coordenadas gps = new Coordenadas(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
        double raio = Double.parseDouble(campos[4]);

        return new Estafeta(voluntaryCode, name, gps, raio, 50, 0, true, false, 0, 0, "Voluntario",false,new ArrayList<>());
    }

    /**
     * Divide input e cria uma transportadora
     *
     * @param input String lida
     * @return      nova transportadora
     */
    private Estafeta parseTransportadora(String input){
        String [] campos = input.split(",");
        String companyCode = campos[0];
        String companyName = campos[1];
        Coordenadas gps = new Coordenadas(Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
        int nif = Integer.parseInt(campos[4]);
        double raio = Double.parseDouble(campos[5]);
        double precoPorKm = Double.parseDouble(campos[6]);

        return new Transportadora(companyCode,companyName,gps,raio,60,0,true,false,0,0,nif,precoPorKm,0.05, 5,false,new ArrayList<>());
    }

    /**
     * Divide input e cria uma encomenda
     *
     * @param input String lida
     * @return      nova encomenda
     */
    private Encomenda parseEncomenda(String input){
        String [] campos = input.split(",");
        String encCode = campos[0];
        String userCode = campos[1];
        String storeCode = campos[2];
        double weight = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> linha = new ArrayList<>();
        int size = campos.length;
        for(int i = 4;i < size - 1;i+=4)
            linha.add(parseLinhaEncomenda(campos[i],campos[i+1],campos[i+2],campos[i+3]));

        return new Encomenda(encCode,userCode,"",storeCode,weight,false, LocalDateTime.now(),false,linha,false,0, false);
    }

    /**
     * Método que cria linha de encomenda
     *
     * @param code          codigo produto
     * @param description   descrição
     * @param qt            quantidade
     * @param price         preço
     * @return              nova linhaEncomenda
     */
    private LinhaEncomenda parseLinhaEncomenda(String code,String description,String qt,String price){
        return new LinhaEncomenda(code,description,Double.parseDouble(qt),Double.parseDouble(price));
    }

    /**
     * Método que lê o ficheiro produtos.txt e adiciona-os às lojas
     *
     * @param c GestTrazAqui
     */
    private void parseProdutosTxt(GestTrazAqui c) {
        List<String> linhas = lerFicheiro("files/produtos.txt");
        String[] linhaPartida;

        for(String linha: linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "produto":
                    Produto p = parseProduto(linhaPartida[1]);
                    c.addProduto(p);
                    break;
                case "loja":
                    linhaPartida = linhaPartida[1].split(":");
                    List<String> produtos = parseProdutosLoja(linhaPartida[1]);
                    c.addProdLoja(linhaPartida[0], produtos);
                    break;
            }

        }
    }

    /**
     * Método que cria um produto através de uma string
     *
     * @param str   string que representa um produto
     * @return      novo produto
     */
    private Produto parseProduto (String str) {
        String[] campos = str.split(",");
        String code = campos[0];
        String name = campos[1];
        double weight = Double.parseDouble(campos[2]);
        double price = Double.parseDouble(campos[3]);
        boolean isMedic = (Integer.parseInt(campos[4]) == 1);

        return new Produto(code, name, weight,price,isMedic);
    }

    /**
     * Método que cria list de produtos para serem adicionados a uma loja
     *
     * @param str   string de produtos
     * @return      list de produtos
     */
    private List<String> parseProdutosLoja(String str) {
        String[] campos = str.split(",");
        return new ArrayList<>(Arrays.asList(campos));
    }
}
