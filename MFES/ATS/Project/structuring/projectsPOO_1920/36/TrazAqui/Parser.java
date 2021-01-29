
/**
 * 
 *
 * @author Artur Drohobytskyy
 * @version 1.0
 */
import java.util.*;
import java.io.*;
import java.nio.*;
import java.nio.file.Files;
import java.nio.file.Paths;

public class Parser {
    
  public GestaoApp parse(){
        GestaoEncomendas gestEncs = new GestaoEncomendas();
        GestaoEntidades gestEnts = new GestaoEntidades();
        List<String> linhas = lerFicheiro("Logs.txt"); 
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                case "Utilizador": 
                        Utilizador u = parseUtilizador(linhaPartida[1]); 
                        System.out.println(u.toString());
                        gestEnts.addEntidade(u);
                        break;
                case "Voluntario": 
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        System.out.println(v.toString()); 
                        gestEnts.addEntidade(v);
                        break;
                case "Transportadora": 
                        Transportadora t = parseTransportadora(linhaPartida[1]);
                        System.out.println(t.toString());
                        gestEnts.addEntidade(t);
                        break;
                case "Loja": 
                        Loja l = parseLoja(linhaPartida[1]);
                        System.out.println(l.toString());
                        gestEnts.addEntidade(l);
                        break;
                case "Encomenda": 
                        EncomendaNormal e = parseEncomenda(linhaPartida[1]);
                        System.out.println(e.toString());
                        gestEncs.addEncomenda(e);
                        break;
                case "Aceite": 
                        String str = parseAceite(linhaPartida[1]);
                        System.out.println(str); 
                        break;
                default: 
                        System.out.println("Linha inválida.");
                        break;
                }
        }
        
        GestaoApp gestApp = new GestaoApp(gestEnts, gestEncs);
        System.out.println("done!");
        return gestApp.clone();
  }
                                

  public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Utilizador(codUtilizador,nome, new GPS(gpsx, gpsy), codUtilizador, "");
  }
  
  public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codUtilizador,nome, new GPS(gpsx, gpsy), codUtilizador, "", raio);
  }
  
  public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Transportadora(codUtilizador,nome, new GPS(gpsx, gpsy), codUtilizador, "", raio, 1, 1);
  }
  
  public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Loja(codUtilizador,nome, new GPS(gpsx, gpsy), codUtilizador, "");
  }
  
  public EncomendaNormal parseEncomenda(String input){
        String[] linhaPartida;
        linhaPartida = input.split(",p", 2);
        String[] campos = linhaPartida[0].split(",");
        String codigoEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codigoLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        List<Produto> produtos = parseProduto(linhaPartida[1]);
        return new EncomendaNormal(codigoEncomenda, codUtilizador, codigoLoja, peso, produtos, EstadoEncomenda.SOLICITADO);
  }
  
  public List<Produto> parseProduto(String input){
       List <Produto> produtos = new ArrayList<>();
       String[] linhaPartida;
       linhaPartida = input.split(",p");
       System.out.println(input);
       for (String s : linhaPartida) {
           String[] campos = input.split(",");
           String codProduto = "p" + campos[0];
           String descricao = campos[1];
           double quantidade = Double.parseDouble(campos[2]);
           double valorUnitario = Double.parseDouble(campos[3]);
           produtos.add(new Produto(codProduto, descricao, quantidade, valorUnitario));  
       }
       return produtos;
  }
  
  public String parseAceite(String input) {
      return "Aceite:" + input;
  }

  public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich)); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
  }
  
}
