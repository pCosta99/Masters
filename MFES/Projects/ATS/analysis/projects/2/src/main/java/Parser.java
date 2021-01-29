import java.util.*;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
/**
 * Escreva a descrição da classe Parser aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Parser
{
  public  void parse(DataBase db) throws  EncomendaJaExisteException{
   List<String> linhas = lerFicheiro("Logs.csv"); //alterar nome do ficheiro
   String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                    case "Utilizador": 
                        Utilizador u = parseUtilizador(linhaPartida[1]);// criar um Utilizador
                        db.add(u);
                        
                        break;
                      
                    case "Loja": 
                        Loja l = parseLoja(linhaPartida[1]);
                        db.add(l);
                        break;
                        
                    case "Transportadora": 
                        Empresa e = parseEmpresa(linhaPartida[1]);
                        db.add(e);
                        break;
                        
                    case "Voluntario": 
                        Voluntario  v = parseVoluntario(linhaPartida[1]);
                        db.add(v);
                        break;
                        
                    case "Encomenda": 
                        Encomenda enc = parseEncomenda(linhaPartida[1]);
                        db.addEnc(enc);
                        db.getUtilizador(enc.getUser()).incNumEnc();
                        break;
                        
                    case "Aceite": 
                        String encv = parseEncomendaAceite(linhaPartida[1]);
                        db.addEncAceite(encv);
                        break;
                        
                    default: 
                        System.out.println("Linha invÃ¡lida.");
                        break;
                }

        }
        System.out.println("done!");
     }
     
  public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String email = campos[0]; 
        String nome = campos[1];
        String password= "default";
        double gpsx = Double.parseDouble(campos[2]);
         double gpsy = Double.parseDouble(campos[3]);
        Ponto2D a= new Ponto2D();
        a.setX(gpsx);
        a.setY(gpsy);
        int numEnc=0;
       return new Utilizador(email,nome,password,a,numEnc);
    }
    
  public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String email = campos[0]; 
        String nome = campos[1];
        String password= "default";
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Ponto2D a= new Ponto2D();
        a.setX(gpsx);
        a.setY(gpsy);
        int espera=0;
        String morada="default";
        
       
        return new Loja(email,nome,password,a,espera,morada);
    }
    
  public Empresa parseEmpresa(String input){
        String[] campos = input.split(",");
        String email = campos[0]; 
        String nomeEmpresa = campos[1];
         String password= "default";
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Ponto2D a= new Ponto2D();
        a.setX(gpsx);
        a.setY(gpsy);
        double raio= Double.parseDouble(campos[5]);
        int velmed=0;
        double precoKm=Double.parseDouble(campos[6]);
        int licMedicamentos=0;//Se pode transportar medicamentos
        String nif=campos[4];
        float precoKg=0;
        
        return new Empresa (email,nomeEmpresa,password,a,raio,velmed,licMedicamentos,nif,precoKg,precoKm);
    }
    
  public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String email = campos[0]; 
        String nome = campos[1];
        String password= "default";
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Ponto2D a= new Ponto2D();
        a.setX(gpsx);
        a.setY(gpsy);
        double raio= Double.parseDouble(campos[4]);
        int velmed=0;
        boolean estado=false;
        int licMedicamentos=0;
        int NMaxEncomendas = 1;
        
        return new Voluntario (email,nome,password,a,raio,velmed,licMedicamentos,estado);
    }
    
  public Encomenda parseEncomenda(String input){
        Encomenda enc= new Encomenda();
        String[] campos = input.split(",");
        int tamanho= campos.length;
        
        String ref= campos[0];
        String user=campos[1];
        String loja=campos[2];
        double peso= Double.parseDouble(campos[3]);
        
        enc.setEnc(ref);
        enc.setUser(user);
        enc.setLoja(loja);
        enc.setPeso(peso);
      
        int i=4;
       
        while(i<tamanho){
            LinhaEncomenda a= new LinhaEncomenda();
            
            String codPro=campos[i];
            String desc =campos[i+1];
            double valUni=Double.parseDouble(campos[i+2]);
            double qtd=Double.parseDouble(campos[i+3]);
            
            a.setReferencia(codPro);
            a.setDescricao(desc);
            a.setValorUni(valUni);
            a.setQuantidade(qtd);
            
            i+=4;
            enc.addLinhaEncomenda(a);
        }
       
        
        int aceitaCusto=0;// 0-nao respondeu,1-sim,-1-nao
        int medicamentos=0;//se e de medicamentos
        int estado=0;//0->encomenda nao pronta//->1 encomenda pronta//-1 encomenda entregue
        String nomTrans="";//Se e empresa ou voluntario
        float tempo=0;
        double custo=0;
        double classificacao=0;
        
        enc.setAceitaCusto(aceitaCusto);
        enc.setMedicamentos(medicamentos);
        enc.setEstado(estado);
        enc.setNomTrans(nomTrans);
        enc.setTempo(tempo);
        enc.setCusto(custo);
        enc.setClassificacao(classificacao);
    
        return enc;
    }
    
  public String parseEncomendaAceite(String input){
       String[] campos = input.split(",");
       String codEncomenda = campos[0]; 
       return  codEncomenda;
   }
    
  public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
        }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
   }
}
