
/**
 * Classe Parser
 * 
 * @author (João Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset;
public class Parser{
    
  
    public Infos parse(Infos i) throws Exception{
        List<String> linhas = lerFicheiro("LogsGerados.txt");
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                    case "Utilizador": 
                        Utilizador u = parseUtilizador(linhaPartida[1]);
                        i.addUtilizador(u);
                        break;
                    case "Loja": 
                        Loja l = parseLoja(linhaPartida[1]);
                        i.addLoja(l);
                        break;                                   
                    case "Voluntario":
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        i.addVoluntario(v);
                        break;
                    case "Transportadora":
                        Transportadora t = parseTransportadora(linhaPartida[1]);
                        i.addTransportadora(t);
                        break;
                    case "Encomenda":
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        i.addEncomenda(e);
                        break;
                    case "Aceite":
                        EncomendasAceites ea = parseAceite(linhaPartida[1]);
                        i.addEncomendaAceite(ea);
                        break;
                    default: 
                        System.out.println("Linha invalida.");
                        break;
                }

        }
        return i;
    }
                                

    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String codUtilizador = campos[0]; 
        String nomeUtilizador = campos[1];
        double coordX = Double.parseDouble(campos[2]);
        double coordY = Double.parseDouble(campos[3]);
        
        return new Utilizador(codUtilizador,nomeUtilizador,coordX,coordY);
    }

    public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
        double coordX = Double.parseDouble(campos[2]);
        double coordY = Double.parseDouble(campos[3]);
        
        return new Loja(codLoja,nomeLoja,coordX,coordY);
    }
    
    public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0]; 
        String nomeVoluntario = campos[1];
        double coordX = Double.parseDouble(campos[2]);
        double coordY = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        
        return new Voluntario(codVoluntario,nomeVoluntario,coordX,coordY,raio);
    }
    
    public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codEmpresa = campos[0]; 
        String nomeEmpresa = campos[1];
        double coordX = Double.parseDouble(campos[2]);
        double coordY = Double.parseDouble(campos[3]);
        long nif = Long.parseLong(campos[4]);
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);
        
        return new Transportadora(codEmpresa,nomeEmpresa,coordX,coordY,nif,raio,precoKm);
    }
    
    public Encomenda parseEncomenda(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0]; 
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        List<LinhaEncomenda> le = new ArrayList<>();
        int i = 4;
        while (i<campos.length && campos[i] != null && campos[i+1] != null && campos[i+2] != null && campos[i+3] != null){
            le.add(parseLE(input,i));
            i+=4;
            if(i>=campos.length){
                 break;
            }
        }
        if (!le.equals(null)){
            return new Encomenda(codEncomenda,codUtilizador,codLoja,peso,le);
        }
        return null;
    }
    
    public LinhaEncomenda parseLE(String input, int i){
        String[] campos = input.split(",");
        String codProduto = campos[i];
        String descricao = campos[i+1];
        double quantidade = Double.parseDouble(campos[i+2]);
        double valorUnitario = Double.parseDouble(campos[i+3]);
        
        return new LinhaEncomenda(codProduto,descricao,quantidade,valorUnitario);
    }
        
    
    public EncomendasAceites parseAceite(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        
        return new EncomendasAceites(codEncomenda);
    }
    
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}
