package controller;

import interfaces.*;
import model.*;

import java.time.LocalDateTime;
import java.util.List;
import java.util.ArrayList;
import java.nio.charset.StandardCharsets;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.lang.String;
import java.util.Map;
import java.util.HashMap;

/**
 * Classe que lê os ficheiros e preenche as estruturas de dados
 */
public class Parse {

    /**
     * Construtor vazio do parse
     */
    public Parse(){}

    /**
     * Método principal que lê os ficheiros e preenche as estruturas de dados
     * @param login login
     * @return sistema
     */
    public ISistema parse(ILogin login){
        List<String> linhas = lerFicheiro("./dados/LogsGerados.csv");
        String[] linhaPartida;
        ISistema s = new Sistema();

        for (String linha : linhas) {

            linhaPartida = linha.split(":", 2);

            switch(linhaPartida[0]){

                case "Utilizador":
                        IUser u = parseUtilizador(linhaPartida[1],login);
                        s.addUser(u);
                        break;
                case "Loja":
                        ILoja l = parseLoja(linhaPartida[1],login);
                        s.addLoja(l);
                        break;
                case "Voluntario":
                        IVoluntario v = parseVoluntario(linhaPartida[1],login);
                        s.addVoluntario(v);
                        break;
                case "Transportadora":
                        IEmpresa t = parseEmpresa(linhaPartida[1],login);
                        s.addEmpresa(t);
                        break;
                case "Encomenda":
                        IEncomenda e = parseEncomenda(linhaPartida[1]);
                        s.addEncomenda(e);
                        break;
                case "Aceite":
                        String ea = linhaPartida[1];
                        s.addEncomendaAceite(ea);
                        break;
                default:
                        InterfaceGeral.message("Linha inválida.\n");
                        break;
            }
            s.addProdutosALoja();
            s.addEncomendaACliente();
        }
        return s;
    }

    /**
     * Cria um novo utilizador lido do ficheiro
     * @param input linha do ficheiro
     * @param login login
     * @return utilizador
     */
    public IUser parseUtilizador(String input, ILogin login){
        String[] campos = input.split(",");

        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        login.addUtilizador(cod,cod);

        return new User(nome,gpsx,gpsy,new HashMap<>(),cod);
    }

    /**
     * Cria uma nova loja lida do ficheiro
     * @param input linha do ficheiro
     * @param login login
     * @return loja
     */
    public ILoja parseLoja(String input, ILogin login){
        String[] campos = input.split(",");

        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        boolean fila = Boolean.parseBoolean(campos[4]);
        int fila_size = Integer.parseInt(campos[5]);

        login.addLoja(codLoja,codLoja);

        return new Loja(codLoja,nomeLoja,fila,fila_size,gpsx,gpsy,new HashMap<>());
    }

    /**
     * Cria um novo voluntário lido do ficheiro
     * @param input linha do ficheiro
     * @param login login
     * @return voluntário
     */
    public IVoluntario parseVoluntario(String input, ILogin login) {

        String[] campos = input.split(",");

        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        double velocidade = Double.parseDouble(campos[5]);
        boolean aceitaMed = Boolean.parseBoolean(campos[6]);
        Double rating = Double.parseDouble(campos[7]);
        boolean livre = Boolean.parseBoolean(campos[8]);

        login.addVoluntario(cod,cod);

        return new Voluntario(cod,nome,gpsx,gpsy,raio,velocidade,aceitaMed,rating,livre,new HashMap<>());
    }

    /**
     * Cria uma nova empresa lida do ficheiro
     * @param input linha do ficheiro
     * @param login login
     * @return empresa
     */
    public IEmpresa parseEmpresa(String input, ILogin login) {

        String[] campos = input.split(",");

        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double taxa = Double.parseDouble(campos[6]);
        double velocidade = Double.parseDouble(campos[7]);
        boolean aceitaMed = Boolean.parseBoolean(campos[8]);
        Double rating = Double.parseDouble(campos[9]);
        boolean livre = Boolean.parseBoolean(campos[10]);
        boolean umaEnc = Boolean.parseBoolean(campos[11]);
        int numEnc = Integer.parseInt(campos[12]);

        login.addEmpresa(cod,cod);

        return new Empresa(cod,nome,nif,taxa,gpsx,gpsy,raio,velocidade,aceitaMed,rating,livre,umaEnc,numEnc,new HashMap<>());
    }

    /**
     * Cria uma nova encomenda lida do ficheiro
     * @param input linha do ficheiro
     * @return encomenda
     */
    public IEncomenda parseEncomenda(String input) {

        String[] campos = input.split(",",6);

        String code = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        boolean meds = Boolean.parseBoolean(campos[4]);

        Map<String,IProduto> prods = parseProduto(campos[5]);

        return new Encomenda(code, codLoja, LocalDateTime.MIN, peso, meds, prods, codUtilizador, false);
    }

    /**
     * Cria um map de produtos lido do ficheiro
     * @param input linha do ficheiro
     * @return produtos
     */
    public Map<String, IProduto> parseProduto(String input) {

        Map<String,IProduto> aux = new HashMap<>();
        int i = 0;
        String[] campos = input.split(",");

        while (i<(campos.length -1)){

            String codProduto = campos[i];
            String nome = campos[i+1];
            double qtd = Double.parseDouble(campos[i+2]);
            double valor = Double.parseDouble(campos[i+3]);
            double peso = Double.parseDouble(campos[i+4]);
            boolean med = false;
            if(nome.equals("Detergente") || nome.equals("Alcool") || nome.equals("Desinfetante"))
                med = true;

            IProduto p = new Produto(codProduto, nome, qtd, valor,peso,med);
            aux.put(codProduto,p.clone());

            i += 5;
        }
        return aux;
    }


    /**
     * Lê o ficheiro
     * @param nomeFich nome do ficheiro
     * @return devolve as linhas do ficheiro
     */
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();

        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        }
        catch(IOException exc) {
            exc.getMessage();
        }

        return lines;
    }
}
