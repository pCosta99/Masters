
import java.io.*;
import java.lang.reflect.Array;
import java.util.*;
import java.nio.file.*;
import java.nio.charset.*;

public class Parser {

    private Dados dados = new Dados();

    public Dados parse() throws Exception{
        List<String> linhas = lerFicheiro("logsGerados.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    parseUtilizador(linhaPartida[1]);
                    break;
                case "Voluntario":
                    parseVoluntario(linhaPartida[1]);
                    break;
                case "Transportadora":
                    parseEmpresa(linhaPartida[1]);
                    break;
                case "Loja":
                    parseLoja(linhaPartida[1]);
                    break;
                case "Encomenda":
                    parseEncomenda(linhaPartida[1]);
                    break;
                default:
                    break;
            }

        }
        return this.dados.clone();
    }


    public void parseUtilizador(String input) throws RegistarException{
        String[] campos = input.split(",");
        String email = campos[0];
        String password = email;
        String nome = campos[1];
        float pos_x = Float.parseFloat(campos[2]);
        float pos_y = Float.parseFloat(campos[3]);

        this.dados.registarUtilizador(email,nome,password,pos_x,pos_y);
    }

    public void parseVoluntario(String input) throws RegistarException{
        String[] campos = input.split(",");
        String email = campos[0];
        String password = email;
        String nome = campos[1];
        float pos_x = Float.parseFloat(campos[2]);
        float pos_y = Float.parseFloat(campos[3]);
        double raio = Double.parseDouble(campos[4]);

        this.dados.registarVoluntario(email,nome,password,pos_x,pos_y,raio);
    }

    public void parseEmpresa(String input) throws RegistarException{
        String[] campos = input.split(",");
        String email = campos[0];
        String password = email;
        String nome = campos[1];
        float pos_x = Float.parseFloat(campos[2]);
        float pos_y = Float.parseFloat(campos[3]);
        double raio = Double.parseDouble(campos[5]);
        float custo = Float.parseFloat(campos[6]);

        this.dados.registarEmpresa(email,nome,password,pos_x,pos_y,raio,custo);
    }

    public void parseLoja(String input) throws RegistarException{
        String[] campos = input.split(",");
        String email = campos[0];
        String password = email;
        String nome = campos[1];
        float pos_x = Float.parseFloat(campos[2]);
        float pos_y = Float.parseFloat(campos[3]);

        this.dados.registarLoja(email,nome,password,pos_x,pos_y);
    }

    public void parseEncomenda(String input) throws LojaInvalidaException{
        String[] campos = input.split(",");
        int N = campos.length;
        String idEncomenda = campos[0];
        String email = campos[1];
        String loja = campos[2];
        List<String> artigos = new ArrayList<>();

        String idArtigo,descricao;
        float pesoProd=0, valor=0;

        for(int j= 4; j<N;j+=4){
            idArtigo = campos[j];
            descricao = campos[j+1];
            pesoProd = Float.parseFloat(campos[j+2]);
            valor = Float.parseFloat(campos[j+3]);
            artigos.add(idArtigo);

            this.dados.adicionarArtigo(idArtigo,descricao,pesoProd,valor,loja);
        }

        this.dados.adicionarEncomenda(idEncomenda,email,loja,artigos);
    }

    public void parseAceite(String input){
        String[] campos = input.split(",");
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}
