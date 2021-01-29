package model;

import exceptions.EmailJaExisteException;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Parser {
    private TrazAquiModel model;

    public Parser(TrazAquiModel new_model){
        this.model = new_model;
    }

    public Parser() {this.model = null; }


    public List<String> lerFicheiro(String nomeFich) throws IOException {
        List<String> lines;
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { throw new IOException(); }
        
        return lines;
    }
    
    public void loadData(String filename) throws EmailJaExisteException, IOException {
        if (filename == null) filename = "src/logs.txt";
        List<String> linhas = lerFicheiro(filename);
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    if(checkUtilizador(u)) model.addUtilizador(u);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    if(checkVoluntario(v)) model.addVoluntario(v);
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    if(checkLoja(l)) model.addLoja(l);
                    break;
                case "Transportadora":
                    Empresa t = parseEmpresa(linhaPartida[1]);
                    if(checkEmpresa(t)) model.addEmpresa(t);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    if(checkEncomenda(e)) model.addEncomenda(e);
                    break;
                case "Aceite":
                    String ea = linhaPartida[1];
                    model.addEncomendaAceite(ea);
                    break;
                default:
                    System.out.println("Linha inválida.");
                    System.out.println(linha);
                    break;
            }

        }
        System.out.println("Dados Carregados!");
    }



    public boolean checkUtilizador(Utilizador utilizador){
        String codUtilizador = utilizador.getCodUtilizador();
        if(! (codUtilizador.charAt(0) == 'u')) return false;
        int nr =  Integer.parseInt(codUtilizador.substring(1));
        if(this.model.getnUtilizadores() < nr) model.setnUtilizadores(++nr);

        return true;
    }

    public boolean checkVoluntario(Voluntario voluntario) {
        String codVoluntario = voluntario.getCodVoluntario();
        if(! (codVoluntario.charAt(0) == 'v')) return false;
        int nr =  Integer.parseInt(codVoluntario.substring(1));
        if(this.model.getnVoluntarios() < nr) model.setnVoluntarios(++nr);

        return true;
    }

    public boolean checkLoja(Loja loja){
        String codLoja = loja.getCodLoja();
        if(! (codLoja.charAt(0) == 'l')) return false;
        int nr =  Integer.parseInt(codLoja.substring(1));
        if(this.model.getnLojas() < nr) model.setnLojas(++nr);

        return true;
    }

    public boolean checkEmpresa(Empresa empresa){
        String codEmpresa = empresa.getCodEmpresa();
        if(! (codEmpresa.charAt(0) == 't')) return false;
        int nr =  Integer.parseInt(codEmpresa.substring(1));
        if(this.model.getnEmpresas() < nr) model.setnEmpresas(++nr);

        return true;
    }

    public boolean checkEncomenda(Encomenda encomenda){
        String codEncomenda = encomenda.getCodEncomenda();
        if(! (codEncomenda.charAt(0) == 'e')) return false;
        int nr =  Integer.parseInt(codEncomenda.substring(1));
        if(this.model.getnEncomendas() < nr) model.setnEncomendas(++nr);

        return true;
    }

    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Utilizador(codUtilizador, nome, new GPS(gpsx, gpsy));
    }

    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        int nr =  Integer.parseInt(codVoluntario.substring(1));
        if(this.model.getnVoluntarios() < nr) model.setnVoluntarios(++nr);
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario, nome, new GPS(gpsx, gpsy), raio);
    }

    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        int nr = Integer.parseInt(codLoja.substring(1));
        if(this.model.getnLojas() < nr) model.setnLojas(++nr);
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Loja(codLoja, nomeLoja, new GPS(gpsx,gpsy));
    }

    public Empresa parseEmpresa(String input){
        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio= Double.parseDouble(campos[5]);
        double precokm= Double.parseDouble(campos[6]);

        return new Empresa(codEmpresa, nome, new GPS(gpsx, gpsy), nif, raio, precokm);
    }

    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");

        String codEncomenda = campos[0];
        int nr = Integer.parseInt(codEncomenda.substring(1));
        if(this.model.getnEncomendas() < nr) model.setnEncomendas(++nr);

        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        ArrayList<Linha_Encomenda> lle = new ArrayList<>();
        for(int i=4 ;i<(campos.length);i=i+4){
            Linha_Encomenda le = new Linha_Encomenda(campos[i], campos[i+1], Double.parseDouble(campos[i+2]), Double.parseDouble(campos[i+3]));
            lle.add(le);
        }

        return new Encomenda(codEncomenda, codUtilizador, codLoja, peso , lle);
    }
}
