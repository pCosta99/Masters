import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Parser {

    public void parse(GestaoAplicacao gestor){
        List<String> linhas = lerFicheiro("LogsGerados.csv"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "User":
                    User u = parseUser(linhaPartida[1]); // criar um Utilizador
                    System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                    try{
                        gestor.registarUser(u);
                    } catch (UserJaExisteException | NullPointerException e) {
                        e.getMessage();
                    }
                    break;
                case "Utilizador":
                    Cliente c = parseCliente(linhaPartida[1]);
                    System.out.println(c.toString());
                    try {
                        gestor.registarCliente(c);
                    } catch (EntidadeRepetidaException | NullPointerException e) {
                        e.getMessage();
                    }
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    System.out.println(l.toString());
                    try{
                        gestor.registarLoja(l);
                    } catch (EntidadeRepetidaException | NullPointerException e) {
                        e.getMessage();
                    }
                    break;
                case "Voluntario":
                    Estafeta e = parseEstafeta(linhaPartida[1]);
                    System.out.println(e.toString());
                    try{
                        gestor.registarEstafeta(e);
                    } catch (EntidadeRepetidaException | NullPointerException ex) {
                        ex.getMessage();
                    }
                    break;
                case "Transportadora":
                    Distribuidora d = parseDistribuidora(linhaPartida[1]);
                    System.out.println(d.toString());
                    try{
                        gestor.registarEmpresa(d);
                    } catch (EntidadeRepetidaException | NullPointerException ex) {
                        ex.getMessage();
                    }

                    break;
                case "Encomenda":
                    Encomenda enc = parseEncomenda(linhaPartida[1]);
                    System.out.println(enc);
                    gestor.registarEncomenda(enc);

                    break;
                case "Aceite":
                    System.out.println("Encomenda " + linhaPartida[1] + " foi aceite!");
                    break;
                default:
                    System.out.println("Linha inválida.");
                    break;
            }

        }
        System.out.println("done!");
    }

    public User parseUser(String input){
        String[] campos = input.split(",");
        String nome = campos[0];
        String codUtilizador = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        User umUser = new User();
        umUser.setName(nome);
        return umUser;
    }

    public Cliente parseCliente(String input){
        String [] campos = input.split(",");
        String codigoCLiente = campos[0];
        String nomeCliente = campos[1];
        double latitude=  Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);

        return new Cliente(codigoCLiente, nomeCliente, new GPS(longitude, latitude));
    }

    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codigoLoja = campos[0];
        String nomeLoja = campos[1];
        double latitude =  Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        return new Loja(codigoLoja, nomeLoja, new GPS(latitude, longitude));
    }

    public Estafeta parseEstafeta(String input){
        String [] campos = input.split(",");
        String codigoEstafeta = campos[0];
        String nomeEstafeta = campos[1];
        double latitude=  Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        double umRaio = Double.parseDouble(campos[4]);

        return new Estafeta(codigoEstafeta, nomeEstafeta, new GPS(latitude, longitude), umRaio);
    }

    public Distribuidora parseDistribuidora(String input){
        String [] campos = input.split(",");
        String codigoDistribuidora = campos[0];
        String nomeDistribuidora = campos[1];
        double latitude=  Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        float taxa = Float.parseFloat(campos[6]);


        return new Distribuidora(codigoDistribuidora, nomeDistribuidora, new GPS(latitude, longitude), nif, raio, taxa);
    }

    public Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codigoEncomenda = campos[0];
        String codigoClinte = campos[1];
        String codigoLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);

        Encomenda e = new Encomenda();
        e.setCodigo(codigoEncomenda);
        e.setDestinatario(codigoClinte);
        e.setEstabelecimento(codigoLoja);
        e.setPeso(peso);

        for (int i = 4; i + 3 <= campos.length; i += 4) { //i+3 <= campos.length garante que exitem pelo menos mais 4 posicoes para a frente
            String codigoProduto = campos[i];
            String descricao = campos[i + 1];
            float quantidade = Float.parseFloat(campos[i + 2]);
            double valor = Double.parseDouble(campos[i + 3]);

            Produto p = new Produto(codigoProduto, descricao, quantidade, valor);
            e.adicionaProduto(p);
        }

        return e;
    }

    public Produto parseProduto(String input){
        String[] campos = input.split(",");
        String codigoProduto = campos[0];
        String descricao = campos[1];
        int quantidade = Integer.parseInt(campos[2]);
        double valor = Double.parseDouble(campos[4]);

        return new Produto(codigoProduto, descricao, quantidade, valor);
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}
