import java.io.IOException;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

/**
 * Classe responsável pelo parsing
 */
public class Parsing implements Serializable {

    /**
     * Responsável pelo parsing do utilizador
     */
    public Utilizador parseUtilizador(String input) {
        String[] campos = input.split(",");
        String id = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS gps = new GPS(gpsx, gpsy);
        return new Utilizador(id, nome, gps);
    }

    /**
     * Responsável pelo parsing da loja
     */
    public Loja parseLoja(String input) {
        String[] campos = input.split(",");
        String codigo = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS gps = new GPS(gpsx, gpsy);
        return new Loja(codigo, nome, gps);
    }

    /**
     * Responsável pelo parsing do voluntário
     */
    public Voluntario parseVoluntario(String input) {
        String[] campos = input.split(",");
        String id = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS gps = new GPS(gpsx, gpsy);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(id, nome, gps, raio, false, 30.0);
    }


    /**
     * Responsável pelo parsing da transportadora
     */
    public MeioTransporte parseTransportadora(String input) {
        String[] campos = input.split(",");
        String id = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS gps = new GPS(gpsx, gpsy);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double preco = Double.parseDouble(campos[6]);
        return new Transportadora(id, nome, gps, raio, false, 30.0, nif, preco, 0.5, false);
    }

    /**
     * Responsável pelo parsing da encomenda
     */
    public Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codEnc = campos[0];
        String codUser = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        Encomenda e = new Encomenda(codEnc, codUser, codLoja, peso, "", false);
        int i = 4;
        int size = campos.length;
        while (i < size) {
            String codP = campos[i++];
            String desc = campos[i++];
            double preco = Double.parseDouble(campos[i++]);
            double qtd = Double.parseDouble(campos[i++]);
            double pesoProduto = 0.23 * qtd * preco; /* Peso gerado para os logs, no caso de quem cria conta
             o peso de cada produto é pedido ao utilizador do programa */
            LinhaEncomenda le = new LinhaEncomenda(codP, desc, preco, qtd, pesoProduto);
            e.addLinha(le.clone());
        }
        return e;
    }

    /**
     * Auxiliar para carregar os logs
     */
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }
}
