package app;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import app.controllers.GestorRegistos;
import app.enums.EstadosTransportadorEnum;
import app.exceptions.CodigoProdutoJaExistenteException;
import app.exceptions.EncomendaJaCanceladaException;
import app.exceptions.EncomendaNaoExistenteException;
import app.exceptions.EstadoRegressivoException;
import app.exceptions.UtilizadorJaExistenteException;
import app.models.EmpresaTransportadora;
import app.models.Encomenda;
import app.models.LinhaEncomenda;
import app.models.Localizacao;
import app.models.Loja;
import app.models.Produto;
import app.models.Utilizador;
import app.models.Voluntario;

public class Parser {

    private GestorRegistos gr;
    private String password = "qwerty";

    public Parser(GestorRegistos gr) {
        this.gr = gr;
    }

    public void parse() {
        List<String> linhas = lerFicheiro("LogsGerados.csv"); // alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    try {
                        gr.registaNovoTipoUtilizador(u);
                    } catch (UtilizadorJaExistenteException e1) {
                        System.out.println(e1.getMessage());
                    }
                    System.out.println(u.toString()); // enviar para o ecrán apenas para teste
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    try {
                        gr.registaNovoTipoUtilizador(l);
                    } catch (UtilizadorJaExistenteException e1) {
                        System.out.println(e1.getMessage());
                    }
                    System.out.println(l.toString());
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]); // criar um Utilizador
                    try {
                        gr.registaNovoTipoUtilizador(v);
                    } catch (UtilizadorJaExistenteException e1) {
                        System.out.println(e1.getMessage());
                    }
                    System.out.println(v.toString()); // enviar para o ecrán apenas para teste
                    break;
                case "Transportadora":
                    EmpresaTransportadora t = parseTransportadora(linhaPartida[1]);
                    try {
                        gr.registaNovoTipoUtilizador(t);
                    } catch (UtilizadorJaExistenteException e1) {
                        System.out.println(e1.getMessage());
                    }
                    System.out.println(t.toString());
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    gr.inserirEncomendaParser(e);
                    System.out.println(e.toString());
                    break;
                case "Aceite":
                    String codEnc = linhaPartida[1];
                    try {
                        gr.encomendAceiteParser(codEnc);
                    } catch (EncomendaNaoExistenteException | EncomendaJaCanceladaException
                            | EstadoRegressivoException e1) {
                        System.out.println(e1.getMessage());
                    } ;
                    System.out.println(codEnc);
                    break;
                default:
                    System.out.println("Linha inválida.");
                    break;
            }

        }
        System.out.println("done!");
    }

    private Utilizador parseUtilizador(String input) {
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        return new Utilizador(codUtilizador, password, nome, new Localizacao(gpsx, gpsy));
    }

    private Loja parseLoja(String input) {
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Loja(codLoja, password, nomeLoja, new Localizacao(gpsx, gpsy), -1);
    }


    private Voluntario parseVoluntario(String input) {
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nomeVoluntario = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario, password, nomeVoluntario, new Localizacao(gpsx, gpsy),
                EstadosTransportadorEnum.LIVRE, false, raio, 1);
    }

    private EmpresaTransportadora parseTransportadora(String input) {
        String[] campos = input.split(",");
        String codTransportadora = campos[0];
        String nomeTransportadora = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);
        return new EmpresaTransportadora(codTransportadora, password, nomeTransportadora,
                new Localizacao(gpsx, gpsy), EstadosTransportadorEnum.LIVRE, false, precoKm, raio,
                nif, 1);
    }

    private Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codEnc = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> linhas = new ArrayList<>();
        for (int i = 4; i < campos.length; i++) {
            String codProduto = campos[i++];
            String descricaoProduto = campos[i++];
            double quantidade = Double.parseDouble(campos[i++]);
            double precoUni = Double.parseDouble(campos[i]);
            Produto p = new Produto(codProduto, descricaoProduto, precoUni);
            try {
                gr.adicionaProduto(codLoja, p);
            } catch (CodigoProdutoJaExistenteException e1) {
                System.out.println(e1.getMessage());
            }
            LinhaEncomenda le = new LinhaEncomenda(p, quantidade);
            linhas.add(le);
        }
        return new Encomenda(codEnc, codUtilizador, codLoja, peso, LocalDateTime.now(), linhas);
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        } catch (IOException exc) {
            System.out.println(exc.getMessage());
        }
        return lines;
    }

    // mais métodos

}
