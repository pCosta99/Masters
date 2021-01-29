import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class Parser {

    //Tratar da parte das aceites.


    public void parse(TrazAquiModel m,String path){
        List<String> aceites = new ArrayList<>();
        List<Encomenda> encs = new ArrayList<>();
        List<String> linhas = lerFicheiro(path);
        String[] linhaPartida;
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador":
                    Utilizador u = parseUtilizador(linhaPartida[1]);
                    m.addUtilizador(u);
                    break;
                case "Loja":
                    Loja l = parseLoja(linhaPartida[1]);
                    m.addLoja(l);
                    break;
                case "Voluntario":
                    Voluntario v = parseVoluntario(linhaPartida[1]);
                    m.addVoluntario(v);
                    break;
                case "Transportadora":
                    Transportadoras t = parseTransportadoras(linhaPartida[1]);
                    m.addTransportadora(t);
                    break;
                case "Encomenda":
                    Encomenda enc = parseEncomenda(linhaPartida[1]);
                    encs.add(enc);
                    break;
                case "Aceite":
                    aceites.add(linhaPartida[1]);
                default:
                    break;
            }
        }
        for (Encomenda e :encs)
            if(aceites.contains(e.getCodEncomenda()))
                m.addEncomenda(e);
    }

    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Utilizador(cod,nome,new GPS(gpsx,gpsy),new ArrayList<>());
    }

    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(cod,nome,new GPS(gpsx,gpsy),raio);
    }

    public Transportadoras parseTransportadoras(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        int nif = Integer.parseInt(campos[4]);
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);
        return  new Transportadoras(cod,nome,new GPS(gpsx,gpsy),nif,raio,precoKm,0);
    }

    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Loja(cod,nome,new GPS(gpsx,gpsy));
    }

    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEnc = campos[0];
        String codUt = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> aux = new ArrayList<>();
        int i = 4;
        while(i<campos.length){
            String codProd = campos[i++];
            String desc = campos[i++];
            double quant = Double.parseDouble(campos[i++]);
            double preco = Double.parseDouble(campos[i++]);
            aux.add(new LinhaEncomenda(codProd,desc,quant,preco));
        }
        return new Encomenda(codEnc,codUt,codLoja,peso,aux,"",false, LocalDateTime.of(0,1,1,0,0),LocalDateTime.of(0,1,1,0,0),false);
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}
