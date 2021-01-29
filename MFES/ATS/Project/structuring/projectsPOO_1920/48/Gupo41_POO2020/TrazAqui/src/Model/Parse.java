package Model;

import Model.Exceptions.EmailExistenteException;
import Model.Exceptions.EncomendaInexistenteException;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class Parse {

    /**
     * Lê ficheiro e faz parse
     * @param model
     */
    public static void parse(ITrazAquiModel model) {
        List<String> linhas = lerFicheiro("logs_20200416.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    if(linhaPartida[1].charAt(0) != '<') {
                        Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                        try {
                            model.registaUtilizador(u);
                        }
                        catch (EmailExistenteException e){
                            System.out.println("Email já existe");
                        }
                        /*System.out.println(u.toString()); //enviar para o ecrán apenas para teste*/
                    }
                    break;
                case "Voluntario":
                    if(linhaPartida[1].charAt(0) != '<') {
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        try {
                            model.registaVoluntario(v);
                        }
                        catch (EmailExistenteException e){
                            System.out.println("Email já existe");
                        }
                        /*System.out.println(v.toString());*/
                    }
                    break;
                case "Transportadora":
                    if(linhaPartida[1].charAt(0) != '<') {
                        EmpresaTransporte e = parseTransportadora(linhaPartida[1]);
                        try {
                            model.registaEmpresaTransporte(e);
                        }
                        catch (EmailExistenteException f){
                            System.out.println("Email já existe");
                        }
                        /*System.out.println(e.toString());*/
                    }
                    break;
                case "Loja":
                    if(linhaPartida[1].charAt(0) != '<') {
                        Loja l = parseLoja(linhaPartida[1]);
                        try {
                            model.registaLoja(l);
                        }
                        catch (EmailExistenteException e){
                            System.out.println("Email já existe");
                        }
                        /*System.out.println(l.toString());*/
                    }
                    break;
                case "Encomenda":
                    if(linhaPartida[1].charAt(0) != '<') {
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        try {
                            model.registaEncomenda(e.clone());
                        }
                        catch (EmailExistenteException j){
                            System.out.println("Email já existe");
                        }
                        /*System.out.println(e.toString());*/
                    }
                    break;
                case "Aceite":
                    if(linhaPartida[1].charAt(0) != '<') {
                        String e = linhaPartida[1];
                        try {
                            model.aceitaEncomenda(e);
                        }
                        catch (EncomendaInexistenteException j){
                            System.out.println("Não existe a encomenda: " + j.getMessage());
                        }
                        /*System.out.println(e);*/
                    }
                    break;
                //...
                default:
                    /*System.out.println("Linha inválida.");*/
                    break;
            }

        }
        /*System.out.println("done!");*/
    }


    /**
     * Cria e devolve um Utilizador a partir da informação representa numa String
     * @param input
     * @return
     * @throws NumberFormatException
     */
    public static Utilizador parseUtilizador(String input) throws NumberFormatException{
        String[] campos = input.split(",");
        String nome = campos[1];
        String codUtilizador = campos[0];
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);

        Coordenadas cord = new Coordenadas(latitude, longitude);

        return new Utilizador(codUtilizador,codUtilizador, "",nome,codUtilizador, cord);
    }

    /**
     * Cria e devolve um Voluntario a partir da informação representa numa String
     * @param input
     * @return
     */
    public static Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);

        Coordenadas cord = new Coordenadas(latitude, longitude);

        return new Voluntario(codVoluntario, codVoluntario, "", nome, codVoluntario, cord, raio,0);
    }

    /**
     * Cria e devolve uma Empresa a partir da informação representa numa String
     * @param input
     * @return
     */
    public static EmpresaTransporte parseTransportadora(String input){
        String[] campos = input.split(",");
        String codTrans = campos[0];
        String nome = campos[1];
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);

        Coordenadas cord = new Coordenadas(latitude, longitude);

        return new EmpresaTransporte(codTrans, codTrans, nif, nome, codTrans, cord, raio, precoKm,0,0, false,45.5, 0);
    }

    /**
     * Cria e devolve um Loja a partir da informação representa numa String
     * @param input
     * @return
     */
    public static Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);

        Coordenadas cord = new Coordenadas(latitude, longitude);

        return new Loja(codLoja, codLoja, "", nomeLoja, codLoja, cord, 0,  Duration.ZERO);
    }

    /**
     * Cria e devolve uma Encomenda a partir da informação representa numa String
     * @param input
     * @return
     */
    public static Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEnc = campos[0];
        String codUt = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);

        List<LinhaDeEncomenda> encomendas = new ArrayList<>();

        String cod;
        String desc;
        double peso1;
        double preco;

        for (int i = 4; i<campos.length; i+=4){
            cod = campos[i];
            desc = campos[i+1];
            peso1 = Double.parseDouble(campos[i+2]);
            preco = Double.parseDouble(campos[i+3]);

            Random r = new Random();
            LinhaDeEncomenda encomenda = new LinhaDeEncomenda(cod, desc, r.nextDouble()*5, preco);

            encomendas.add(encomenda.clone());
        }

        return new Encomenda(codEnc, codLoja, codUt, LocalDateTime.now(), peso, encomendas);
    }

    /**
     * Lê um ficheiro para uma lista de String
     * @param nomeFich
     * @return
     */
    public static List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println("ERRO: " + exc.getMessage()); }
        return lines;
    }


}
