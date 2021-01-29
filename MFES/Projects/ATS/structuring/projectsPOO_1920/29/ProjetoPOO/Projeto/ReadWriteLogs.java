import java.util.ArrayList;
import java.util.List;
import java.io.IOException;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.Iterator;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.Serializable;
public class ReadWriteLogs implements Serializable
{   
    public static List<String> lerFicheiroTexto(String nomeFich) throws IOException {
        List<String> lines = new ArrayList<>();
        lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        return lines;
    }
    
    public static void leCSV(String nomeFicheiro, Login app) throws IOException, JaRegistadoException{
        List<String> linhas = lerFicheiroTexto(nomeFicheiro);
        for (String l: linhas)
            parseClass(l, app);  
    }
    
    public static void parseClass(String classe, Login app) throws JaRegistadoException {
        String[] campos = classe.split(":");
        if(campos[0].equals("Utilizador"))  parseUtilizador(campos[1], app);
        if(campos[0].equals("Voluntario")) parseVoluntario(campos[1], app);
        if(campos[0].equals("Transportadora")) parseTransportadora(campos[1], app);
        if(campos[0].equals("Loja")) parseLoja(campos[1], app);
        if(campos[0].equals("Encomenda")) parseEncomenda(campos[1], app);
        if(campos[0].equals("Aceite")) parseEncomendaAceite(campos[1], app);
    }
    
    public static void parseUtilizador(String utilizador, Login app) throws JaRegistadoException {
        String[] campos = utilizador.split(",");
        String codU = campos[0];
        String nome = campos[1];
        GPS gps = new GPS(Double.valueOf(campos[2]), Double.valueOf(campos[3]));
        String pass = campos[4];
        app.readregistarUtilizador(codU, nome, gps, pass);
    }    
    
    public static void parseVoluntario(String voluntario, Login app) throws JaRegistadoException {
        String[] campos = voluntario.split(",");
        String codV = campos[0];
        String nome = campos[1];
        GPS gps = new GPS(Double.valueOf(campos[2]), Double.valueOf(campos[3]));
        double raio = Double.valueOf(campos[4]);
        String pass = campos[5];
        int classi = Integer.parseInt(campos[6]);
        app.readregistarVoluntario(codV, nome, gps,raio, pass, classi);
    }  
    
    public static void parseTransportadora(String transportadora, Login app) throws JaRegistadoException {
        String[] campos = transportadora.split(",");
        String codT = campos[0];
        String nome = campos[1];
        GPS gps = new GPS(Double.valueOf(campos[2]), Double.valueOf(campos[3]));
        String nif = campos[4];
        double raio = Double.valueOf(campos[5]);
        double preco = Double.valueOf(campos[6]);
        String pass = campos[7];
        int classi = Integer.parseInt(campos[8]);
        double fat = Double.valueOf(campos[9]);
        double km = Double.valueOf(campos[10]);
        app.readregistarTransportadora(codT, nome, gps, nif, raio, preco, pass,classi, fat,km);  
        app.adicionarKm(km);
    }    
    
    public static void parseLoja(String loja, Login app) throws JaRegistadoException {
        String[] campos = loja.split(",");
        String codL = campos[0];
        String nome = campos[1];
        GPS gps = new GPS(Double.valueOf(campos[2]), Double.valueOf(campos[3]));
        String pass = campos[4];
        app.readregistarLoja(codL, nome, gps, pass);
    }
    
    public static void parseEncomenda(String encomenda, Login app) throws JaRegistadoException {
        ArrayList<LinhaEncomenda> le = new ArrayList<>();
        String[] campos = encomenda.split(",");
        String codE = campos[0];
        String codU = campos[1];
        String codL = campos[2];
        double peso = Double.valueOf(campos[3]);
        for(int i = 4; i < campos.length; i+=4) {
            String cod = campos[i];
            String desc = campos[i+1];
            double quat = Double.valueOf(campos[i+2]);
            double valor = Double.valueOf(campos[i+3]);
            le.add(new LinhaEncomenda(cod, desc, quat, valor));
        }
        app.readregistarEncomenda(codE, codU, codL, peso, le);
    }
    
    public static void parseEncomendaAceite(String encomenda, Login app) {
        app.registarEncomendaAceite(encomenda);
    }  
    
    public static void escreveEmFicheiroTexto(String nomeFicheiro, Login app) throws FileNotFoundException{
        PrintWriter fich = new PrintWriter(nomeFicheiro);
        for(Utilizador u: app.getAllUtilizadores().values()) {
            fich.println(u.toStringCSV());
        }
        for(Voluntario v: app.getAllVoluntarios().values()){
            fich.println(v.toStringCSV());
        }
        for(Transportadora t: app.getAllTransportadoras().values()){
            fich.println(t.toStringCSV());
        }
        for(Encomenda e: app.getPedidosEncomendas().values())  
            fich.println(e.toStringCSV());
        
        for (Encomenda e: app.getEncConcluidas().values())
        fich.println("Aceite:" + e.getCodEncomenda()); 
        
        fich.flush();
        fich.close();
    }    
}
