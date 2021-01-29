import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

public class Parse implements Serializable{
    
    private Map<String, AllUsers> infoAll;
    private Map<String, Encomenda> allEnc;
    private Map<String, EncomendasAceites> encAceites;
    private Map<String, String> allLogins;
    
    public Parse(){
        this.infoAll= new HashMap<String,AllUsers>();
        this.allEnc = new HashMap<String,Encomenda>();
        this.encAceites = new HashMap <String,EncomendasAceites>();
        this.allLogins = new HashMap <String,String>();
    }

    public Map<String,AllUsers> getInfoAll(){
        Map<String,AllUsers> ret = new HashMap<>();
        for (Map.Entry<String,AllUsers> a : this.infoAll.entrySet())
            ret.put(a.getKey(),a.getValue().clone());

        return ret;
  
    }
    
    public Map<String,Encomenda> getAllEnc(){
        Map<String,Encomenda> enc = new HashMap<>();
        for (Encomenda e : this.allEnc.values())
            enc.put(e.getOrderCode(),e);

        return enc;
  
    }
    
    public Map<String,EncomendasAceites> getEncAceites(){
        Map<String,EncomendasAceites> aceites = new HashMap<>();
        for (Map.Entry<String,EncomendasAceites> e : this.encAceites.entrySet())
            aceites.put(e.getKey(),e.getValue().clone());

        return aceites;
  
    }

    public Map<String,String> getAllLogins(){
        Map<String,String> newlogs = new HashMap<>();
        for (Map.Entry<String,String> l : this.allLogins.entrySet())
            newlogs.put(l.getKey(),l.getValue());

        return newlogs;
  
    }

    //SET methods
    public void setInfoAll(Map<String,AllUsers> infoAll){
        this.infoAll = new HashMap<>(); //diamond notation
        infoAll.entrySet().forEach(a -> this.infoAll.put(a.getKey(), a.getValue().clone()));
    }
    
    public void setAllEnc(Map<String,Encomenda> enc){
        this.allEnc = new HashMap<>(); //diamond notation
        enc.entrySet().forEach(a -> this.allEnc.put(a.getKey(), a.getValue()));
    }
    
    public void setEncAceites(Map<String,EncomendasAceites> aceites){
        this.encAceites = new HashMap<>(); //diamond notation
        aceites.entrySet().forEach(a -> this.encAceites.put(a.getKey(), a.getValue().clone()));
    }

    public void setAllLogins(Map<String,String> newlogins){
        this.allLogins = new HashMap<>(); //diamond notation
        newlogins.entrySet().forEach(a -> this.allLogins.put(a.getKey(), a.getValue()));
    }
    
    public void parse() throws FileNotFoundException{
        List<String> linhas = lerFicheiroCSV("./LogsGerados.csv"); //alterar nome do ficheiro
        String[] linhaPartida;
        
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                case "Utilizador":
                        Utilizadores u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                        infoAll.put(u.getCode(), u);
                        allLogins.put(u.getEmail(), u.getPassword());
                        System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                        break;
                case "Loja":
                        Lojas l = parseLoja(linhaPartida[1]);
                        infoAll.put(l.getCode(), l);
                        allLogins.put(l.getEmail(), l.getPassword());
                        System.out.println(l.toString());
                        break;                                   
                
                case "Transportadora":
                        Transportadoras t = parseTransportadora(linhaPartida[1]);
                        infoAll.put(t.getCode(), t);
                        allLogins.put(t.getEmail(), t.getPassword());
                        System.out.println(t.toString());
                        break;
                
                case "Voluntario":
                        Voluntarios v = parseVoluntario(linhaPartida[1]);
                        infoAll.put(v.getCode(), v);
                        allLogins.put(v.getEmail(), v.getPassword());
                        System.out.println(v.toString());
                        break;
                       
                case "Linha de Encomenda":
                        LinhaEncomenda le = parseLinhaEncomenda(linhaPartida[1]);
                        System.out.println(le.toString());
                        break;
                        
                case "Encomenda":
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        allEnc.put(e.getOrderCode(), e);
                        System.out.println(e.toString());
                        break;
                        
                case "Aceite":
                        EncomendasAceites ea = parseAceites(linhaPartida[1]);
                        encAceites.put(ea.getCodigoEncAceite(), ea);
                        System.out.println(ea.toString());
                        break;

                        
                default: 
                        System.out.println("Linha invalida.");
                        break;
                }

        }
        System.out.println("done!");
    }
                                

    public Utilizadores parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas coord = new Coordenadas();
        coord.setLat(gpsx);
        coord.setLon(gpsy);
        String email= codUtilizador+"@email.com";
        String password = "qwerty123";
        ArrayList<Integer> avaliacoes = new ArrayList<>();
        ArrayList<String> porClassificar = new ArrayList<>();
        ArrayList<Encomenda> historico = new ArrayList<>();

        if(campos.length < 5){
            return new Utilizadores(codUtilizador, nome, coord, email, password, porClassificar, historico, avaliacoes);
        }
        else{
            email= campos[4];
            password = campos[5];
            for(int i = 6; i < campos.length; ++i)
                porClassificar.add(campos[i]);
            
            return new Utilizadores(codUtilizador, nome, coord, email, password, porClassificar, historico, avaliacoes);   
        }
    }
    

    public Lojas parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas coord = new Coordenadas();
        coord.setLat(gpsx);
        coord.setLon(gpsy);        
        String email= codLoja+"@email.com";
        String password = "qwerty123";
        ArrayList<Integer> avaliacoes = new ArrayList<>();
        if(campos.length < 5)
            return new Lojas(codLoja, nomeLoja, coord, email, password, avaliacoes);

        else{
            email = campos[4];
            password = campos[5];
            return new Lojas(codLoja, nomeLoja, coord, email, password, avaliacoes);
        }
    }
  
    public Transportadoras parseTransportadora(String input){
        String[] campos = input.split(",");
        String codTransportadora = campos[0];
        String nomeTransportadora = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas coord = new Coordenadas();
        coord.setLat(gpsx);
        coord.setLon(gpsy);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double ppKm = Double.parseDouble(campos[6]);
        double totalKm = 0;
        int numenc=1;
        ArrayList<Integer> avaliacoes = new ArrayList<>();
        boolean aceitamed = false;
        boolean medcert = false;
        boolean ready = true;
        String email= codTransportadora+"@email.com";
        String password = "qwerty123";
        ArrayList<Encomenda> historico = new ArrayList<>();  
        
        if (campos.length < 8) {
            return new Transportadoras(codTransportadora, nomeTransportadora, coord, nif, raio, ppKm, totalKm, numenc, aceitamed, medcert, email, password, ready, avaliacoes, historico);
        }
        else {
            totalKm = Double.parseDouble(campos[7]);
            aceitamed = Boolean.parseBoolean(campos[8]);
            medcert = Boolean.parseBoolean(campos[9]);
            email = campos[10];
            password = campos[11];
            ready = Boolean.parseBoolean(campos[12]);
            int z;
            for (z = 13; campos[z].charAt(0) != 'e'; ++z)
                avaliacoes.add(Integer.parseInt(campos[z]));
    
            return new Transportadoras(codTransportadora, nomeTransportadora, coord, nif, raio, ppKm, totalKm, numenc, aceitamed, medcert, email, password, ready, avaliacoes, historico);
        }
    }
  
    public Voluntarios parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nomeVoluntario = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas coord = new Coordenadas();
        coord.setLat(gpsx);
        coord.setLon(gpsy);  
        double raio = Double.parseDouble(campos[4]);
        String email= codVoluntario+"@email.com";
        String password = "qwerty123";
        ArrayList<Integer> avaliacoes = new ArrayList<>();
        boolean aceitamed = false;
        boolean medcert = false;
        boolean ready = true;
        ArrayList<Encomenda> historico = new ArrayList<>();

        
        if (campos.length < 6) {
            return new Voluntarios(codVoluntario, nomeVoluntario, coord, raio, email, password, aceitamed, medcert, ready, avaliacoes, historico);
        }
        else {
            email = campos[5];
            password = campos[6];
            aceitamed = Boolean.parseBoolean(campos[7]);
            medcert = Boolean.parseBoolean(campos[8]);
            ready = Boolean.parseBoolean(campos[9]);

            int z;
            for (z = 10; campos[z].charAt(0) != 'e'; ++z)
                avaliacoes.add(Integer.parseInt(campos[z]));
            return new Voluntarios(codVoluntario, nomeVoluntario, coord, raio, email, password, aceitamed, medcert, ready, avaliacoes, historico);
        }
    }
  
    public LinhaEncomenda parseLinhaEncomenda(String input){
        String[] campos = input.split(",");
        String codProduto = campos[0];
        String nomeProduto = campos[1];
        double quantidade = Double.parseDouble(campos[2]);
        double valor = Double.parseDouble(campos[3]);
        return new LinhaEncomenda(codProduto, nomeProduto, quantidade, valor);
    }
  
    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso  = Double.parseDouble(campos[3]);
        LocalDateTime data = null;
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");
        boolean ready = false;
        ArrayList<LinhaEncomenda> encomendas = new ArrayList<>();
        if (campos[4].charAt(0) == '2'){

            data = LocalDateTime.parse(campos[4], formatter);
            ready = Boolean.parseBoolean(campos[5]);
            int i = 6;
            while (i < campos.length){
                String codProd = campos[i];
                String nome = campos[i+1];
                double quant = Double.parseDouble(campos[i+2]);
                double valor = Double.parseDouble(campos[i+3]);
                LinhaEncomenda linha =  new LinhaEncomenda(codProd, nome, quant, valor);
                encomendas.add(linha);
                i += 4;
                
            }
        }
        else{
            int i = 4;
            while (i < campos.length){
                String codProd = campos[i];
                String nome = campos[i+1];
                double quant = Double.parseDouble(campos[i+2]);
                double valor = Double.parseDouble(campos[i+3]);
                LinhaEncomenda linha =  new LinhaEncomenda(codProd, nome, quant, valor);
                encomendas.add(linha);
                i += 4;
            }
            
        }
        return new Encomenda(codEncomenda, codUtilizador, codLoja, peso, data, ready, encomendas);
    }
    
    public EncomendasAceites parseAceites(String input){
        String[] campos = input.split(",");
        String cod = campos[0];

        return new EncomendasAceites(cod);
    }
  
    public List<String> lerFicheiroCSV(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); 
        }
        catch(IOException exc) { 
            System.out.println(exc.getMessage());
        }
        return lines;
    }
  
}
