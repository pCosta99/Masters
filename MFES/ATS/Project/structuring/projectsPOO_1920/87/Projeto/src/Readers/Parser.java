package Readers;

import Geral.GPS;
import Modelos.TrazAqui;
import Modelos.TrazAquiModel;
import Stock.Encomenda;
import Stock.LinhaEncomenda;
import Users.*;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Parser {

    /**
     * Método que divide as linhas lidas de um dado ficheiro de texto e insere nas respetivas estruturas.
     * @param traz Modelo onde serão inseridas as informações.
     */
    public static void parse(TrazAquiModel traz) {
        TrazAqui trazAqui = (TrazAqui) traz;
        List<String> linhas = lerFicheiro("src/Resources/newlogs.txt");
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]);
                    trazAqui.addUtilizador(u);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    trazAqui.addUtilizador(v);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    trazAqui.addUtilizador(l);
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    trazAqui.addUtilizador(t);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    trazAqui.addEncomenda(e);
                    trazAqui.addEncomendaALoja(e);
                    trazAqui.addProdutoALoja(e);
                    trazAqui.addEncomendaToUtilizador(e);
                    break;
                default:
                    break;
            }

        }

        linhas = lerFicheiro("src/Resources/pw.txt");
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            trazAqui.setPassword(linhaPartida[0],linhaPartida[1]);
        }
    }

    /**
     * Método que separa uma linha correspondente a um Utilizador.
     * @param input Linha do Utilizador.
     * @return Utilizador inicializado com as informações lidas.
     */
    private static Utilizador parseUtilizador(String input) {
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Utilizador(codUtilizador, nome, "", new GPS(gpsx, gpsy));
    }

    /**
     * Método que separa uma linha correspondente a um Voluntário.
     * @param input Linha do Voluntário.
     * @return Voluntário inicializado com as informações lidas.
     */
    public static Voluntario parseVoluntario(String input) {
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario, nome, "", new GPS(gpsx, gpsy),false, raio);
    }

    /**
     * Método que separa uma linha correspondente a uma Loja.
     * @param input Linha da Loja.
     * @return Loja inicializado com as informações lidas.
     */
    public static Loja parseLoja(String input) {
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Loja l = new Loja();
        l.setCodigo(codLoja);
        l.setNome(nomeLoja);
        l.setGps(new GPS(gpsx, gpsy));
        return l;
    }

    /**
     * Método que separa uma linha correspondente a uma Transportadora.
     * @param input Linha da Transportadora.
     * @return Transportadora inicializada com as informações lidas.
     */
    public static Transportadora parseTransportadora(String input) {
        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nomeEmpresa = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double ppk = Double.parseDouble(campos[6]);
        Transportadora t = new Transportadora();
        t.setCodigo(codEmpresa);
        t.setNome(nomeEmpresa);
        t.setGps(new GPS(gpsx, gpsy));
        t.setNif(nif);
        t.setRaio(raio);
        t.setPPK(ppk);
        return t;
    }

    /**
     * Método que separa uma linha correspondente a uma Encomenda.
     * @param input Linha da Encomenda.
     * @return Encomenda inicializada com as informações lidas.
     */
    public static Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> encomendas = new ArrayList<>();

        int i;
        for(i=4;i<campos.length;){
            String codProd = campos[i++];
            String descricao = campos[i++];
            double quantidade = Double.parseDouble(campos[i++]);
            double valorUnitario = Double.parseDouble(campos[i++]);
            LinhaEncomenda l = new LinhaEncomenda(codProd,descricao,quantidade,valorUnitario);
            encomendas.add(l);
        }
        return new Encomenda(codEncomenda,codUtilizador,codLoja,peso,encomendas);
    }

    public static List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }

    /**
     * Método que guarda em ficheiros a informação referente às encomendas e utilizadores presentes no Sistema.
     * @param trazAqui Modelo que contém as informações.
     */
    public void wparser(TrazAqui trazAqui) {
        try {
            BufferedWriter bw = new BufferedWriter(new FileWriter("src/Resources/newlogs.txt"));
            BufferedWriter pw = new BufferedWriter(new FileWriter("src/Resources/pw.txt"));

            List<User> u = trazAqui.getListaUsers();
            List<Encomenda> es = trazAqui.getEncomendas();
            for (User user : u) {
                switch(user.getClass().getName()){
                    case "Users.Utilizador":
                        Utilizador utilizador = (Utilizador) user;
                        bw.write(utilizador.toString() + '\n');
                        pw.write(utilizador.getCode()+":"+utilizador.getPassword()+'\n');
                        break;
                    case "Users.Voluntario":
                        Voluntario voluntario = (Voluntario) user;
                        bw.write(voluntario.toString() + '\n');
                        pw.write(voluntario.getCode()+":"+voluntario.getPassword()+'\n');
                        break;
                    case "Users.Loja":
                        Loja l = (Loja) user;
                        bw.write(l.toString() + '\n');
                        pw.write(l.getCode()+":"+l.getPassword()+'\n');
                        break;
                    case "Users.Transportadora":
                        Transportadora t = (Transportadora) user;
                        bw.write(t.toString() + '\n');
                        pw.write(t.getCode()+":"+t.getPassword()+'\n');
                        break;
                    default:
                        break;
                }
            }
            for(Encomenda encomenda : es)
                bw.write(encomenda.toString()+'\n');
            bw.close();
            pw.close();
        } catch (IOException e) {
            e.printStackTrace();
        }

    }


}
