package Model;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import Exceptions.*;

/**
 * Classe que importa a informação dos logs
 */
public class Parse {
    
    public UsersDB users = new UsersDB(); 
    public LojasDB lojas = new LojasDB();
    public VoluntariosDB voluntarios = new VoluntariosDB();
    public EmpresasTransDB empresas = new EmpresasTransDB();
    public EncomendasDB encomendas = new EncomendasDB();
    public EncomendasAceitesDB encomendasA = new EncomendasAceitesDB();
    public int count;
    
        
    public void parse(String file) throws UserJaExisteException,JaExisteLojaException,
                                          JaExisteVoluntarioException,JaExisteEmpresaException,
                                          JaExisteEncomendaException,JaExisteEncomendaAceiteException{
                                              
          List<String> linhas = read(file);
          String[] linhaPartida;
          for (String linha : linhas) {
                  linhaPartida = linha.split(":", 2);
                  switch(linhaPartida[0]){
                  case "Utilizador": 
                          User u = parseUtilizador(linhaPartida[1]); 
                          users.addUtilizador(u);
                          break;
                  case "Loja": 
                          Lojas l = parseLoja(linhaPartida[1]);
                          lojas.addLoja(l);
                          break;                                   
                  case "Voluntario":
                          Voluntarios v = parseVoluntario(linhaPartida[1]);
                          voluntarios.addVoluntario(v);
                          break;
                  case "Transportadora": 
                          EmpresasTrans et = parseTransportadoras(linhaPartida[1]);
                          empresas.addEmpresa(et);
                          break;
                 case "Encomenda": 
                          Encomendas e = parseEncomendas(linhaPartida[1]);
                          encomendas.addEncomenda(e);
                          users.addEncomenda(e);
                          lojas.addEncomenda(e);
                          break;
                 case "Aceite":
                          EncomendaAceite ea = parseEncomendaAceite(linhaPartida[1]);
                          encomendasA.addEncomendaAceite(ea);
                          break;
                  default: 
                          //System.out.println("Linha invalida.");
                          //count++;
                          break;
                  }
  
          }
          //System.out.println(count);
          System.out.println("Leitura Feita!");
    }
                                  
    public User parseUtilizador(String input){
          String[] campos = input.split(",");
          String nome = campos[1]; 
          String codUtilizador = campos[0];
          double gpsx = Double.parseDouble(campos[2]);
          double gpsy = Double.parseDouble(campos[3]);
          return new User(codUtilizador,nome,codUtilizador + "@uminho","logs",gpsx,gpsy);            
    }

    List<EncomendaAceite> ea = new ArrayList<>();
    public Lojas parseLoja(String input){
          String[] campos = input.split(",");
          String codLoja = campos[0]; 
          String nomeLoja = campos[1];
          return new Lojas("logs", codLoja,nomeLoja,0.0,0.0,0.0,false);
    }


    public Voluntarios parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0]; 
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raioA = Double.parseDouble(campos[4]);
       return new Voluntarios("logs", codVoluntario, nome, gpsx, gpsy,raioA,false,0.0);
  }



    public EmpresasTrans parseTransportadoras(String input){
        String[] campos = input.split(",");

        String codEmpresa = campos[0]; 
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raioA = Double.parseDouble(campos[5]);
        double taxa = Double.parseDouble(campos[6]);   
        return new EmpresasTrans("logs",codEmpresa,nome,gpsx,gpsy,nif,raioA, taxa,false,0,0.0,0.0);

}

ArrayList<LinhaEncomenda> listaL = new ArrayList<>();

public Encomendas parseEncomendas(String input){
    String[] campos = input.split(",");
    String codEncomenda = campos[0]; 
    String codUtilizador = campos[1];
    String codLoja = campos[2];
    double peso = Double.parseDouble(campos[3]);
    int i = 4;
    ArrayList<LinhaEncomenda> listaL = new ArrayList<>();
    while(i < campos.length){ 
    String codProduto = campos[i];
    String descricao = campos[i+1];
    double quantidade = Double.parseDouble(campos[i+2]);
    double preco = Double.parseDouble(campos[i+3]);
    i += 4;
    LinhaEncomenda l = new LinhaEncomenda(codProduto,descricao,quantidade,preco,0.0);
    listaL.add(l);
    }
    return new Encomendas(codEncomenda, codUtilizador, codLoja,peso, false, listaL);
}

public EncomendaAceite parseEncomendaAceite(String input){
    return new EncomendaAceite(input); 
}


public static List<String> read(String file){
    BufferedReader br = null;
    String line = null;
    List<String> res = new ArrayList<String>();

    try{
        br = new BufferedReader(new FileReader(file));
        line = br.readLine();

        while(line != null){
            res.add(line);
            line = br.readLine();
        }
    }
    catch(IOException e){
        System.out.println(e);
    }

    return res;
}
    
   
}