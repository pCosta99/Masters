package Modelo;
import java.io.IOException;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

public class Parse implements Serializable {
    /**
     * Método que lê o ficheiro logs.txt e carrega um SistemaGestaoEntregas
     * @param sge SistemaGestaoEntregas
     */

    public void parse(SistemaGestaoEntregas sge){
        List<String> linhas = lerFicheiro("src/logs.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    //System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                    sge.addUtilizador(u);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    //System.out.println(l.toString());
                    sge.addLoja(l);
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    //System.out.println(t.toString());
                    sge.addEmpresa(t);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    //System.out.println(v.toString());
                    sge.addVoluntario(v);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    //System.out.println(e.toString());
                    sge.addEncomenda(e);
                    sge.addProduto(sge.qualLoja(e),parseProduto(linhaPartida[1]));
                    break;
                case "Aceite" :
                    sge.addAceite(linhaPartida[1]);
                    break;
                default:
                    System.out.println("Linha inválida.");
                    break;
            }

        }
        System.out.println("done!");
    }

    /**
     * Divide uma linha e cria um Utilizador
     * @param input String lida
     * @return      Utilizador
     */
    public static Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        return new Utilizador(codUtilizador , nome ,new Gps(gpsx , gpsy), codUtilizador , new TreeSet<>());
    }

    /**
     * Divide uma linha e cria uma Loja
     * @param input String lida
     * @return      Loja
     */
    public static Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        List<String> encomendas = new ArrayList<>();
        Set<Produto> catalogo = new TreeSet<>();
        return new Loja(codLoja , nomeLoja, new Gps(gpsx , gpsy), true, codLoja, catalogo, encomendas );
    }

    /**
     * Divide uma linha e cria uma Transportadora
     * @param input String lida
     * @return Transportadora
     */
    public static Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);
        Set<String> entregues = new TreeSet<>();
        return new Transportadora(codEmpresa , nome , new Gps(gpsx , gpsy) , nif , raio , precoKm, codEmpresa, 0.1 , false , true, entregues, 0 , 0);
    }

    /**
     * Divide uma linha e cria um Voluntário
     * @param input String lida
     * @return Voluntário
     */
    public static Voluntario parseVoluntario(String input) {
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario, nome, new Gps(gpsx, gpsy), raio, new TreeSet<>(), codVoluntario , true , false);
    }

    /**
     * Divide uma linha e cria um Set de Produtos
     * @param input String lida
     * @return      Set de Produtos
     */
    public static Set<Produto> parseProduto(String input){
        String[] campos = input.split(",");
        Set<Produto> produtos = new TreeSet<>();
        for (int i =4 ; i<campos.length ; i+=4){
            produtos.add(new Produto(campos[i] , campos[i+1] , Double.parseDouble(campos[i+3]) , false));
        } return produtos;
    }

    /**
     * Divide uma linha e cria uma Encomenda
     * @param input String lida
     * @return      Encomenda
     */
    public static Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double preco = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> res = new ArrayList<>();
        for (int i =4 ; i<campos.length ; i+=4){
            res.add(new LinhaEncomenda(campos[i], campos[i + 1], Double.parseDouble(campos[i+2]),Double.parseDouble( campos[i + 3]), 0 , false));
        }
        return new Encomenda(codEncomenda , codUtilizador , codLoja , preco ,res , false, false, true, 0 );
    }

    /**
     * Método que lê um ficheiro
     * @param nomeFich Nome do ficheiro
     * @return         Array de strings lidas
     */
    public static List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch(IOException exc) {
            System.out.println("Erro a ler ficheiro.");
            System.out.println(exc.getMessage());
        }
        return lines;
    }

}
