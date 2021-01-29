package Controller;

import Model.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Controller {

    private Sistema sistema;

    public Controller(Sistema s) {
        this.sistema = s;
    }

    public Controller() {
        this.sistema = new Sistema();
    }

    public Map<String, SystemUser> getUtilizadores() {
        return this.sistema.getUtilizadores();
    }

    public Map<String, SystemUser> getLojas() {
        return this.sistema.getLojas();
    }

    public Map<String, SystemUser> getTransportadoras() {
        return this.sistema.getTransportadoras();
    }

    public Map<String, SystemUser> getVoluntarios() {
        return this.sistema.getVoluntarios();
    }

    public SystemUser getSystemUser(Character type, String cod) {
            return this.sistema.getSystemUser(type, cod);
    }

    public List<Encomenda> encsEmRaioSorted(Voluntario vol) {
        return this.sistema.encsEmRaioSorted(vol);
    }


    public List<String> getLojasNames(){
        return this.sistema.getLojasNames();
    }

    public void loadFromLog(String path) {
        List<String> allLines = null;
        Path p = Paths.get(path);
        try {
            allLines = Files.readAllLines(p);
        } catch (IOException e) {
            e.printStackTrace();
        }
        String[] linhaPartida;
        for (String linha : allLines) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                    //System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                    sistema.registarUtilizador(u);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    //System.out.println(l.toString());
                    sistema.registarLoja(l);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    //System.out.println(v.toString());
                    sistema.registarVoluntario(v);
                    break;
                case "Transportadora":
                    Transportadora t = parseTransportadora(linhaPartida[1]);
                    //System.out.println(t.toString());
                    sistema.registarTransportadora(t);
                    break;
                case "Encomenda":
                    Encomenda e = parseEncomenda(linhaPartida[1]);
                    //System.out.println(e.toString());
                    sistema.addEmEspera(e);
                    break;
                case "Aceite":
                    String codEnc = (linhaPartida[1]);
                    //System.out.println(e.toString());
                    sistema.aceitaEncomenda(codEnc);
                    break;
                //...
                default:
                    //System.out.println("Linha invÃ¡lida.");
                    break;
            }

        }
        //System.out.println(this.sistema.getBiggestProductID());
        this.sistema.setCurrentLastProdID(this.sistema.getBiggestProductID());
        this.sistema.generateProducts();
        this.sistema.setCurrentLastEncID(this.sistema.getBiggestEncID());
        this.sistema.setCurrentLastUserID(this.sistema.getBiggestUserID());
        this.sistema.setCurrentLastVolID(this.sistema.getBiggestVolID());
        this.sistema.setCurrentLastLojaID(this.sistema.getBiggestLojaID());
        this.sistema.setCurrentLastTranspID(this.sistema.getBiggestTranspID());
        this.sistema.giveEncsToTransps();


        //System.out.println(sistema);

        //System.out.println(this.sistema.getLojas());
        //System.out.println("done!");


    }

    public String nextLojaId() {
        return this.sistema.nextLojaId();
    }
    public String nextTranspID() {
        return this.sistema.nextTranspId();
    }

    public String nextEncomedaID() {
        return this.sistema.nextEncId();
    }

    public String nextUtilizadorID() {
        return this.sistema.nextUserId();
    }
    public String nextVolID() {
        return this.sistema.nextVolId();
    }

    public void requestEncomeda(Encomenda enc) {
        this.sistema.registarEncomenda(enc);
    }

    public Loja getLoja(int i) {
        return this.sistema.getLojasSorted().get(i);
    }

    public Loja getLoja(String cod) {
        return this.sistema.getLoja(cod);
    }

    public Voluntario getVoluntario(String cod) {
        return this.sistema.getVoluntario(cod);
    }

    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS gps = new GPS(gpsx,gpsy);
        Utilizador u = new Utilizador(codUtilizador,nome,gps);
        u.addObserver(sistema);
        return u;
    }

    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS gps = new GPS(gpsx,gpsy);
        Loja loja = new Loja(codLoja,nomeLoja,gps);
        loja.addObserver(sistema);
        return loja;
    }

    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        GPS gps = new GPS(gpsx,gpsy);
        return new Voluntario(codVoluntario,nome,gps,raio);
    }

    public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codTransp = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double preçoKm = Double.parseDouble(campos[5]);
        GPS gps = new GPS(gpsx,gpsy);
        return new Transportadora(codTransp,nome,gps,raio,nif,preçoKm);
    }

    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEnc = campos[0];
        String codUser = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> linhasEnc = new ArrayList<>();
        int i = 4;
        while(i<campos.length) {
            linhasEnc.add(new LinhaEncomenda(campos[i],campos[i+1],Double.parseDouble(campos[i+2]),Double.parseDouble(campos[i+3])));
            i+=4;
        }
        return new Encomenda(codEnc,codUser,codLoja,this.sistema.getLoja(codLoja).getGps(),this.sistema.getUtilizador(codUser).getGps(),peso,linhasEnc);
    }

    public Utilizador getUtilizador(String cod) {
        return this.sistema.getUtilizador(cod);
    }


    public void transportByVol(String codVol, String codEnc) {
        this.sistema.transportByVol(codVol,codEnc);
    }


    public void printSystem() {
        System.out.println(this.sistema);
    }

    public void save(String s) throws IOException {
        this.sistema.save(s);
    }

    public void read(String s) throws IOException, ClassNotFoundException {
        this.sistema = this.sistema.read(s);
    }

    public Sistema getSistema() {
        return this.sistema;
    }

    public void addUtilizador(Utilizador u) {
        this.sistema.addUtilizador(u);
    }

    public void addVol(Voluntario vol) {
        this.sistema.addVol(vol);
    }

    public void addLoja(Loja l) {
        this.sistema.addLoja(l);
    }


    public void addTransp(Transportadora t) {
        this.sistema.addTransp(t);
    }
}
