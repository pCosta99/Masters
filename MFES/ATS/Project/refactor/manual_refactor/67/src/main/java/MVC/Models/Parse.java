package MVC.Models;

import MVC.Models.BaseModels.*;
import MVC.Models.Catalogs.*;

import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;

/**
 * Write a description of class Parse here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class Parse {

    /**
     * Método que trata do Parsing do Ficheiro de Logs.
     * @param us Catálogo de Utilizador.
     * @param vs Catálogo de Voluntario.
     * @param ts Catálogo de Transportadora.
     * @param ls Catálogo de Loja.
     * @param es Catálogo de Encomenda.
     * @param eas Lista de Códigos de Encomendas Aceites.
     */
    public void parse(Utilizadores us, Voluntarios vs, Transportadoras ts, Lojas ls, Encomendas es, List<String> eas){
        List<String> linhas = lerFicheiro("dados/logs.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                    
                case "Utilizador":
                        Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                        us.addUtilizador(u);
                        break;
                        
                case "Voluntario":
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        vs.addVoluntario(v);
                        break;
                        
                case "Transportadora":
                        Transportadora t = parseTransportadora(linhaPartida[1]);
                        ts.addTransportadora(t);
                        break;
                        
                case "Loja":
                        Loja l = parseLoja(linhaPartida[1]);
                        ls.addLoja(l);
                        break;
                case "Encomenda":
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        String codLoja = e.getCodLoja();
                        for(LinhaEncomenda le : e.getLinhas()){
                            ls.addProdutoLoja(new Produto(le.getCodigo(),le.getNome(),(le.getPreco()/le.getQuantidade())),codLoja);
                        }
                        es.addEncomenda(e);
                        break;
                case "Aceite":
                        String s = linhaPartida[1];
                        eas.add(s);
                        break;
                default: 
                        System.out.println("Linha inválida.");
                        break;
                }

        }
    }

    /**
     * Método que trata do Parsing e da criação de um Utilizador.
     * @param input Linha que contém dados de um Utilizador.
     * @return Utilizador resultante.
     */
    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Utilizador(codUtilizador, nome, gpsx, gpsy);
    }

    /**
     * Método que trata do Parsing e da criação de um Voluntario.
     * @param input Linha que contém dados de um Voluntario.
     * @return Voluntario resultante.
     */
    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario, nome, gpsx, gpsy, raio);
    }

    /**
     * Método que trata do Parsing e da criação de uma Loja.
     * @param input Linha que contém dados de uma Loja.
     * @return Loja resultante.
     */
    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Loja(codLoja, nomeLoja, gpsx, gpsy);
    }

    /**
     * Método que trata do Parsing e da criação de uma Transportadora.
     * @param input Linha que contém dados de uma Transportadora.
     * @return Transportadora resultante.
     */
    public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codTransport = campos[0]; 
        String nomeTransport = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double preco = Double.parseDouble(campos[6]);
        return new Transportadora(codTransport, nomeTransport, gpsx, gpsy,nif, raio, preco);
    }

    /**
     * Método que trata do Parsing e da criação de uma Encomenda.
     * @param input Linha que contém dados de uma Encomenda.
     * @return Encomenda resultante.
     */
    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEnc = campos[0]; 
        String codUser = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        int i;
        List<LinhaEncomenda> linhasEnc = new ArrayList<>();
        for(i=4; i<campos.length; i+=4){
            String ref = campos[i];
            String desc = campos[i+1];
            double quant = Double.parseDouble(campos[i+2]);
            double preco = Double.parseDouble(campos[i+3]);
            linhasEnc.add(new LinhaEncomenda(ref,desc,quant,preco));
        }
        return new Encomenda(codEnc, codUser, codLoja, peso, linhasEnc);
    }

    /**
     * Método que trata de ler o Ficheiro de Logs e guarda todas as linhas para uma Lista.
     * @param nomeFich Caminho para o Ficheiro de Logs.
     * @return Lista que contém todas as Linhas do Ficheiro de Logs.
     */
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}