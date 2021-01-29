import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.time.LocalDate;

/**
 * classe utilizada para ler um ficheiro de texto 
 */
public class Parse{
      
      private Map < String , User > contas;
      private EncomendasAceites encomendasAceites;
      private Map <String , Encomenda> encomendas;

      public Parse (){
          
          this.contas = new HashMap <> ();
          this.encomendasAceites = new EncomendasAceites ();
          this.encomendas = new HashMap<>();
            
      }

      /**
       * metodo que recebe uma string com o nome do ficheiro, separa as linhas do
       * ficheiro e trata cada linha
       * @throws FileNotFoundException
       */
      public void parse(String ficheiro) throws FileNotFoundException {
            List<String> linhas = lerFicheiro(ficheiro + ".csv"); //alterar nome do ficheiro
            String[] linhaPartida;

            for (String linha : linhas) {
                  linhaPartida = linha.split(":", 2);
                  switch(linhaPartida[0]){
                  case "Utilizador": 
                        Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                        this.contas.put (u.getId() , u.clone());
                        break;

                  case "Loja": 
                        Lojas l = parseLoja(linhaPartida[1]);
                        this.contas.put (l.getId() , l.clone());
                        break; 
                  
                  case "Voluntario": 
                        Voluntarios v = parseVoluntarios(linhaPartida[1]);
                        this.contas.put (v.getId() , v.clone());
                        break;      
                  
                  case "Transportadora":
                        Transportadoras t = parseTransportadora(linhaPartida[1]);
                        this.contas.put (t.getId() , t.clone());
                        break;
                 
                  case "Encomenda":
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        encomendas.put (e.getCodEncomenda() , e.clone());
                        break;

                  case "Aceite":
                        String codEncomenda = linhaPartida[1];
                        Encomenda enc = encomendas.get(codEncomenda);
                        this.encomendasAceites.addEncomendaAceite(enc);
                        break;
                  default: 
                        System.out.println("Linha invalida!");
                        break;
                  }
  
          }
            System.out.println("Ficheiro CSV Lido!");
    }

    /**
     * metodo que a partir da informaçoes de uma linha cria uma encomenda
     */
      private Encomenda parseEncomenda(String input){
            String[] campos = input.split(",");
            String codEncomenda = campos[0]; 
            String codUtilizador = campos[1];
            String codLoja = campos[2];
            double peso = Double.parseDouble(campos[3]);
            
            List <LinhaEncomenda> linhas = new ArrayList<>();
            int i = 4;

            while  (i != campos.length) {
                  LinhaEncomenda linha = new LinhaEncomenda(campos[i++], campos[i++], Double.parseDouble(campos[i++]), Double.parseDouble(campos[i++]));
                  linhas.add(linha.clone());
            }

            return new Encomenda(codEncomenda, codUtilizador, codLoja, peso, LocalDate.now(), linhas);
      }
      /**
     * metodo que a partir da informaçoes de uma linha cria uma Loja
     */
      private Lojas parseLoja(String input){
            String[] campos = input.split(",");
            String codLoja = campos[0]; 
            String nomeLoja = campos[1];
            double latitude = Double.parseDouble(campos[2]);
            double longitude = Double.parseDouble(campos[3]);
            GPS coordenadas = new GPS(latitude, longitude);

            return new Lojas(codLoja, nomeLoja, coordenadas);
      }

      /**
      * metodo que a partir da informaçoes de uma linha cria uma transportadora
      */
      private Transportadoras parseTransportadora(String input){
            String[] campos = input.split(",");
            String codEmpresa = campos[0];
            String nomeEmpresa = campos[1]; 
            double latitude = Double.parseDouble(campos[2]);
            double longitude = Double.parseDouble(campos[3]);
            GPS coordenadas = new GPS(latitude, longitude);
            String nif = campos[4];
            double raio = Double.parseDouble(campos[5]);
            double preco_por_km = Double.parseDouble(campos[6]);

  
            return new Transportadoras(codEmpresa, nomeEmpresa, "123", coordenadas, nif, raio, preco_por_km);
      }


      /**
       * metodo que a partir da informaçoes de uma linha cria um Utilizador
       
       */
      private Utilizador parseUtilizador(String input){
            String[] campos = input.split(",");
            String codUtilizador = campos[0];
            String nome = campos[1]; 
            double latitude = Double.parseDouble(campos[2]);
            double longitude = Double.parseDouble(campos[3]);
            GPS coordenadas = new GPS(latitude, longitude);
                  
            return new Utilizador(codUtilizador, nome, coordenadas);
    }

      /**
       * metodo que a partir da informaçoes de uma linha cria um voluntario
       
       */
      private Voluntarios parseVoluntarios (String input){
            String[] campos = input.split(",");
            String codVoluntario = campos[0]; 
            String nome = campos[1];
            double latitude = Double.parseDouble(campos[2]);
            double longitude = Double.parseDouble(campos[3]);
            GPS coordenadas = new GPS(latitude, longitude);
            double raio = Double.parseDouble(campos[4]);

            return new Voluntarios(codVoluntario, nome, coordenadas, raio);
      }
      

      /**
       * metodo que le o ficheiro nomeFich e devolve uma lista com as linhas do ficheiro
       */
      private List<String> lerFicheiro(String nomeFich) throws FileNotFoundException {
            List<String> lines = new ArrayList<>();
            if (Files.notExists(Paths.get(nomeFich))) throw new FileNotFoundException("Ficheiro nao encontrado!");
            try {
                  lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8);
            } catch (Exception exc) {
                  System.out.println(exc.getMessage());
            }
            return lines;
      }


      /**
       * metodo que devolve as contas lidas no ficheiro
       */
      public Map < String , User> getContas (){

            Map < String , User > ans = new HashMap <> ();
    
            for ( Map.Entry < String , User > conta : this.contas.entrySet() ){
                
                ans.put ( conta.getKey() , conta.getValue().clone());
    
            }
    
            return ans;
        }

        /**
       * metodo que devolve as Encomendas lidas no ficheiro
       */
      public List <Encomenda> getEncomendas () {
            List<Encomenda> ret = new ArrayList<>();

            for (Encomenda enc : this.encomendas.values()){
                  if (encomendasAceites.buscaEncomenda(enc.getCodEncomenda()) == null){
                        ret.add(enc);
                  }
            }
            return ret;
      }

      /**
       * metodo que devolve as encomendasAceites lidas no ficheiro
       */
      public List <Encomenda> getEncomendasAceites (){
            return encomendasAceites.getEncomendas();
      }
    

  
}