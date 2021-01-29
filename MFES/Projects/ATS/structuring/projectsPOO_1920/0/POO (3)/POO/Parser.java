import java.io.*;
import java.util.*;
import java.nio.file.*;
import java.nio.charset.*;

public class Parser {

    private Dados dados = new Dados();

    public Dados parse() throws Exception{
        List<String> linhas = lerFicheiro("logs.txt"); //alterar nome do ficheiro
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
        return this.dados;
    }


    public void parseUtilizador(String input) throws UserException{
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String email = campos[0];
        String password = campos[0];
        String nome = campos[1];
        double pos_x = Float.parseFloat(campos[2]);
        double pos_y = Float.parseFloat(campos[3]);
        Cliente c = new Cliente(codUtilizador, nome, email, password,pos_x,pos_y, new ArrayList<>(), new ArrayList<>());
        dados.registarUtilizador(c);
        dados.registarUser(c);
        System.out.println(c.toString());
        System.out.println(dados.getUser(codUtilizador).toString());
    }

    public void parseVoluntario(String input) throws UserException{
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String email = campos[0];
        String password = email;
        String nome = campos[1];
        double pos_x = Double.parseDouble(campos[2]);
        double pos_y = Double.parseDouble(campos[3]);
        int raio = Integer.parseInt(campos[4]);
        Voluntario  v= new Voluntario(codVoluntario, nome, email, password,  pos_x,  pos_y, raio,0,0,new ArrayList<>());
        dados.registarVoluntario(v);
        dados.registarUser(v);
    }
    
    public void parseEmpresa(String input) throws UserException{
        String[] campos = input.split(",");
        String codEmpresa= campos[0];
        String email = campos[0];
        String password = email;
        String nome = campos[1];
        double pos_x = Double.parseDouble(campos[2]);
        double pos_y = Double.parseDouble(campos[3]);
        int nif= Integer.parseInt(campos[4]);
        int raio = Integer.parseInt(campos[5]);
        float custo = Float.parseFloat(campos[6]);
        Random num = new Random();
        double precoKm = num.nextInt(10);
        double precoKilo = num.nextInt(3);
        double taxa = num.nextInt(10);
        boolean disponivel = true;
        
        Transportadora t= new Transportadora(codEmpresa,  nome, email, password, pos_x, pos_y,nif,raio,precoKm, precoKilo,taxa,  disponivel, 0,0,new ArrayList<>());
        dados.registarTransportadora(t);
        dados.registarUser(t);
    }

    public void parseLoja(String input) throws UserException{
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String email = campos[0];
        String password = email;
        String nome = campos[1];
        double pos_x = Double.parseDouble(campos[2]);
        double pos_y = Double.parseDouble(campos[3]);
        
        Loja l = new Loja( codLoja,nome,email, password,  pos_x, pos_y,new HashMap<String,LinhaEncomenda> (),new ArrayList<>(), new ArrayList<>());
        dados.registarLoja(l);
        dados.registarUser(l);
    }

    public void parseEncomenda(String input) throws Exception{
        String[] campos = input.split(",");
        int N = campos.length;
        
        String codEncomenda = campos[0];
        String email = campos[1];
        String loja = campos[2];
        ArrayList<LinhaEncomenda> produtos = new ArrayList<>();

        String codProduto,descricao;
        double pesoProd=0, valor=0;
        double peso =0;
        

        for(int j= 4; j<N;j+=4){
            codProduto = campos[j];
            descricao = campos[j+1];
            pesoProd = Double.parseDouble(campos[j+2]);
            valor = Double.parseDouble(campos[j+3]);
            peso+=pesoProd;
            LinhaEncomenda linha = new LinhaEncomenda(codProduto,descricao,50,valor);
            LinhaEncomenda le = new LinhaEncomenda(codProduto,descricao,pesoProd,valor);

            produtos.add(le.clone());
            this.dados.registarProduto(linha,loja);
            
        }
        
        Encomenda e = new Encomenda(codEncomenda,email,loja,peso,produtos);
        this.dados.registarEncomenda(e);
    }

    
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(Exception exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}
