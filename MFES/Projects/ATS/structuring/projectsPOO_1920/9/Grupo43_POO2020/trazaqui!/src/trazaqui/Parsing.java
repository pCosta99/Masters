package trazaqui;

import trazaqui.Exceptions.*;

import java.util.List;
import java.util.*;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.io.*;

public class Parsing {

    private static Armazena a_armazena;
    private static BaseDados b_dados;

    public void parse(Armazena a_armazena) throws CodigoJaEstaEmUsoException, TransportadoraExisteException, UtilizadorExisteException, LojaExisteException, VoluntarioExisteException, EncomendaExisteException {
        List<String> linhas = lerFicheiro("LogsTeste.csv"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    a_armazena.novoUtilizador(u);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    a_armazena.novaLoja(l);
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]); // criar um Utilizador
                    a_armazena.novaTransportadora(t);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]); // criar um Utilizador
                    a_armazena.novoVoluntario(v);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);// criar um Utilizador
                    CatalogoProdutos c = parseCatalogo(linhaPartida[1]);
                    a_armazena.novaEncomenda(e);
                    a_armazena.novoCatalogoProdutos(c);
                    break;
                case "Aceite":
                    String cod=linhaPartida[1];
                    a_armazena.adicionaCodigo(cod);
                    break;
                default:
                    System.out.println("Linha invÃ¡lida.");
                    break;
            }
        }
        System.out.println("done!");
    }

    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Localizacao pos = new Localizacao(gpsx,gpsy);
        return new Utilizador(codUtilizador,nome,pos);
    }

    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Localizacao pos = new Localizacao(gpsx,gpsy);
        return new Loja(codLoja,nome,pos);
    }

    public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Localizacao pos = new Localizacao(gpsx,gpsy);
        double raio = Double.parseDouble(campos[5]);
        double precoporkm = Double.parseDouble(campos[6]);
        String nif = campos[4];
        return new Transportadora(codEmpresa,nome,pos,nif,raio,precoporkm);
    }

    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Localizacao pos = new Localizacao(gpsx,gpsy);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario,nome,pos,raio);
    }

    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        String[] tralhas= tralha(campos);
        ArrayList<LinhaEncomenda> l= new ArrayList<>();
        for(int i=0; i<tralhas.length;i+=4){
            if (i+4>campos.length) break;
            LinhaEncomenda linha = parseLinhaEncomenda(tralhas,i);
            l.add(linha);
        }
        return new Encomenda(codEncomenda,codUtilizador,codLoja,peso,l);
    }

    public CatalogoProdutos parseCatalogo(String input){
        String[] campos = input.split(",");
        String[] tralhas= tralha(campos);
        ArrayList<Produto> l= new ArrayList<>();
        for(int i=0; i<tralhas.length;i+=4){
            if (i+4>campos.length) break;
            Produto p = parseProduto(tralhas,i);
            l.add(p);
        }
        return new CatalogoProdutos(campos[2],l);
    }

    public LinhaEncomenda parseLinhaEncomenda(String[] campos, int i){
        String codProd = campos[i];
        String descricao = campos[i+1];
        double quantidade = Double.parseDouble(campos[i+2]);
        double preco = Double.parseDouble(campos[i+3]);
        return new LinhaEncomenda(codProd,descricao,preco,quantidade);
    }

    public Produto parseProduto(String[] campos, int i){
        String codProd = campos[i];
        String descricao = campos[i+1];
        double quantidade = Double.parseDouble(campos[i+2]);
        double preco = Double.parseDouble(campos[i+3]);
        return new Produto(codProd,descricao,preco,quantidade);
    }


    public String[] tralha(String[] campos){
        String[] tralha= new String[campos.length-4];
        for (int i=4,j=0; i<campos.length;i++,j++) tralha[j]=campos[i];
        return tralha;
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try{lines = Files.readAllLines(Paths.get("LogsTeste.csv"), StandardCharsets.UTF_8);}
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }

    public Parsing(Armazena armazena) throws UtilizadorExisteException, VoluntarioExisteException, LojaExisteException, TransportadoraExisteException, CodigoJaEstaEmUsoException, EncomendaExisteException {
        parse(armazena);
    }
}