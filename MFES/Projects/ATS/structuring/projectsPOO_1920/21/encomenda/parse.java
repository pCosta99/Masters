import java.util.List;
import java.util.ArrayList;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Set;
import java.util.HashSet;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.Iterator;
/**
 * Escreva a descrição da classe parse aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class parse {
    public Set<lojas> ml;
    public Set<voluntarios> mv;
    public Set<utilizador> mu;
    public Set<transportadores> mt;
    public Set<String> a;
    public GestaoEncomendas ge;
    

    public void parse(){
        System.out.println("assdf");
        List<String> linhas = lerFicheiro("C:/Users/Helder/Desktop/tbpoo.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        mu= new HashSet();
        ml= new HashSet();
        mv= new HashSet();
        mt= new HashSet();
        System.out.println("arroz");
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch(linhaPartida[0]){
                case "Utilizador": 
                    utilizador u = parseUtilizador(linhaPartida[1]); 
                    mu.add(u);
                    break;
                case "Loja": 
                    lojas l = parseLoja(linhaPartida[1]);
                    ml.add(l);
                    break;                                   
                case "Voluntario":
                    voluntarios v=parsevoluntarios(linhaPartida[1]);
                    mv.add(v);
                    break;
                case "Transportadora":
                    transportadores t = parsetransportador(linhaPartida[1]);
                    mt.add(t);
                    break;
                case "Encomenda":
                    Encomenda e= parseEncomenda(linhaPartida[1]);
                    ge.addEncomenda(e);
                    break;
                case "Aceite":
                    a.add(linhaPartida[1]);
                    break;
                default: 
                    System.out.println("Linha invalida.");
                    break;
            }

        }
        System.out.println("done!");
    }

    public utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String nome = campos[0]; 
        String codUtilizador = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS g = new GPS(gpsx,gpsy);
        return new utilizador(codUtilizador,nome,g);
    }

    public lojas parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS g = new GPS(gpsx,gpsy);
        return new lojas(codLoja,nomeLoja,g);
    }
    
    public voluntarios parsevoluntarios(String input){
        String[] campos = input.split(",");
        String nome = campos[0]; 
        String codUtilizador = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        float r = Float.parseFloat(campos[4]);
        GPS g = new GPS(gpsx,gpsy);
        return new voluntarios(codUtilizador,r,g,nome);
    }
    
    public transportadores parsetransportador(String input){
        String[] campos = input.split(",");
        String nome = campos[0]; 
        String codUtilizador = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS g = new GPS(gpsx,gpsy);
        int nif = Integer.parseInt(campos[4]);
        float r = Float.parseFloat(campos[5]);
        float p = Float.parseFloat(campos[6]);
        return new transportadores(codUtilizador,nome,g,nif,r,p);
    }
    
    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String code = campos[0]; 
        String codu = campos[1];
        String codl= campos[2];
        double peso = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> l=new ArrayList<>();
        Integer i =4;
        double qnt=0;
        String cod="";
        String nome="";
        double vu=0;
        while(i<campos.length){
            if(i%6==0){
                qnt = Double.parseDouble(campos[i]);
            }
            else if (i%4==0){
                cod = campos[i];
            }
            else if (i%5==0){
                nome = campos[i];
            }
            else if (i%7==0){
                vu = Double.parseDouble(campos[i]);
                l.add(new LinhaEncomenda(cod,nome,vu,qnt));
            }
        }
        return new Encomenda(code,codu,codl,peso,l);
    }
    

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
    
    public Set<utilizador> getmu(){
        return this.mu.stream().map(utilizador::clone).collect(Collectors.toSet());
    }
    public Set<voluntarios> getmv(){
        return this.mv.stream().map(voluntarios::clone).collect(Collectors.toSet());
    }
    public Set<transportadores> getmt(){
        return this.mt.stream().map(transportadores::clone).collect(Collectors.toSet());
    }
    public Set<lojas> getml(){
        return this.ml.stream().map(lojas::clone).collect(Collectors.toSet());
    }
    public Set<String> geta(){
        return this.a;
    }
    
}