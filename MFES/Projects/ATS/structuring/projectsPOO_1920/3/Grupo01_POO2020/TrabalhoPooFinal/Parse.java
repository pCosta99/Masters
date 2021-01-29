import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
/**
 * Escreva a descrição da classe Parse aqui.
 * 
 /**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Parse{

  public void parse(DataBase db){
        List<String> linhas = lerFicheiro("logins2.txt"); //alterar nome do ficheiro
        String[] linhaPartida;
        for (String linha : linhas) {
                 linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                    case "Utilizador": 
                            Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                            //System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                            db.addUser(u);
                            break;
                    case "Loja": 
                            Loja l = parseLoja(linhaPartida[1]);
                            //System.out.println(l.toString());
                            db.addUser(l);
                            break;                                   
                    
                    case "Voluntario":
                            Voluntario v = parseVoluntario(linhaPartida[1]);
                            //System.out.println(v.toString());
                            db.addUser(v);
                            break;
                            
                    case "Transportadora":
                            Transportadora t = parseTransportadora(linhaPartida[1]);
                            //System.out.println(t.toString());
                            db.addUser(t);
                            break;
                            
                    case "Encomenda":
                            Encomenda e = parseEncomenda(linhaPartida[1]);
                           // System.out.println(e.toString());
                            db.addEncomenda(e);
                            break;        
           
                            
                    default: 
                        //System.out.println("Linha inválida.");
                        break;
                }

        }
        System.out.println("done!");
  }
                                

  public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        Utilizador util = new Utilizador();
        
        String tag = campos[0];
        String nome = campos[1];
        String password = "default";
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        
        Localizacao l = new Localizacao();
        l.setLatitude(latitude);
        l.setLongitude(longitude);
        util.setPosicao(l);
        
        util.setTag(tag);
        util.setNome(nome);
        
        return util;
  }

  public Loja parseLoja(String input){
        String[] campos = input.split(",");
        Loja loja = new Loja();
        
        String email =campos[0]; // adicionar este campo nos testes
        String nome = campos[1];
        String password = "default";
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        
        Localizacao l = new Localizacao();
        l.setLatitude(latitude);
        l.setLongitude(longitude); 
        loja.setPosicao(l);
        
        loja.setTag(email);
        loja.setNome(nome);
        
        return loja;
  }

  public Voluntario parseVoluntario(String input){
   
        String[] campos = input.split(",");
        Voluntario vol = new Voluntario();
        
        String tag = campos[0]; // adicionar este campo nos testes
        String nome = campos[1];
        String password = "default";
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        boolean disponivel = false;
        
        Localizacao l = new Localizacao();
        l.setLatitude(latitude);
        l.setLongitude(longitude);
        
        vol.setPosicao(l);
        vol.setTag(tag);
        vol.setRaio(raio);
        vol.setNome(nome);
        
        double classificacao=0;
        vol.setClassificacao(classificacao);
        double velocidade = 50;
        vol.setVelocidade(velocidade);
        
        
        
        return vol;
  }
  
  public Transportadora parseTransportadora(String input){
        Transportadora trans = new Transportadora();
        String[] campos = input.split(",");
        
        
        String email = campos[0]; // adicionar este campo nos testes
        String nome = campos[1];
        double latitude = Double.parseDouble(campos[2]);
        double longitude = Double.parseDouble(campos[3]);
        int nif = Integer.parseInt(campos[4]);
        double raio = Double.parseDouble(campos[5]);
        boolean disponivel = false; 
        double precoKm = Double.parseDouble(campos[6]);
        //double velocidade = Double.parseDouble(campos[6]);
        
        trans.setTag(email);
        trans.setNome(nome);
        trans.setRaio(raio);
        trans.setPrecoKm(precoKm);
        trans.setNif(nif);
        
        double velocidade = 50;
        trans.setVelocidade(velocidade);
        double kmsTotal = 0;
        trans.setKmsTotal(kmsTotal);
        double classificacao = 0;
        trans.setClassificacao(classificacao);
        double totalFacturado=0;
        trans.setTotalFacturado(totalFacturado);
        
        
        
        Localizacao l = new Localizacao();
        l.setLatitude(latitude);
        l.setLongitude(longitude);
        trans.setPosicao(l);
        
        return trans;
  }
 
  public Encomenda parseEncomenda(String input){
        Encomenda enc = new Encomenda();
        String[] campos = input.split(",");
        int tamanho = campos.length;
        
        String codEncomenda = campos[0];
        String codUtilizador = campos[1]; 
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        
        enc.setCodEncomenda(codEncomenda);
        enc.setEmailUtilizador(codUtilizador);
        enc.setEmailLoja(codLoja);
        enc.setPeso(peso);
        
        int i = 4;
        
        while(i<tamanho){
            LinhaEncomenda a = new LinhaEncomenda();
            
            String codPro = campos[i];
            String desc = campos[i+1];
            double valUni=Double.parseDouble(campos[i+3]);
            double quant=Double.parseDouble(campos[i+2]);
            
            a.setCodProduto(codPro);
            a.setQuantidade(quant);
            a.setValorUnitario(valUni);
            a.setDescricao(desc);
            
            i+=4;
            enc.addLinhaEncomenda(a);
        }
       
        return enc;
  }
  
  
  public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
  }


   
}
