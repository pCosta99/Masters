import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;

public interface Parse {
  	
  public static void parse(SGE sge){
        List<String> linhas = lerFicheiro("logs_20200416.txt");
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                    case "Utilizador": {
                        Utilizador u = parseUtilizador(linhaPartida[1]);

                        /*ADICIONAR UTILIZADOR AO SGE*/
                        sge.addUtilizador(u);
                        break;
                    }

                    case "Voluntario": {
                        Voluntario v = parseVoluntario(linhaPartida[1]);

                        /*ADICIONAR VOLUNTARIO AO SGE*/
                        sge.addVoluntario(v);

                        break;
                    }

                    case "Transportadora": {
                        Transportadora t = parseTransportadora(linhaPartida[1]);

                        /*ADICIONAR TRANSPORTADORA A LISTA*/
                        sge.addTransportadora(t);

                        break;
                    }

                    case "Loja": {
                        Loja l = parseLoja(linhaPartida[1]);

                        /*ADICIONAR LOJA A LISTA*/
                        sge.addLoja(l);

                        break;
                    }

                    case "Encomenda": {
                        Encomenda enc = parseEncomenda(linhaPartida[1]);

                        /*Adiciona a encomenda ao sistema*/
                        sge.addEncomenda(enc);

                        /*Dono da encomenda*/
                        Utilizador u = sge.getUtilizadores().get(enc.getCodUtilizador());

                        /*Adiciona a encomenda ao utilizador*/
                        u.addEncomenda(enc);

                        break;
                    }

                    case "Aceite": {
                        /*Codigo da Encomenda que foi aceite*/
                        String encAceite = parseAceite(linhaPartida[1]);

                        /*Verificar se a encomenda existe*/
                        if(!sge.getEncomendas().containsKey(encAceite)){
                            View.showError("Encomenda nao existe!");
                            break;
                        }

                        /*Encomenda*/
                        Encomenda enc = sge.getEncomenda(encAceite);

                        /*User*/
                        Utilizador u = sge.getUtilizador(enc.getCodUtilizador());

                        /*Loja*/
                        Loja l = sge.getLoja(enc.getCodLoja());

                        /*Adiçao aos registos da loja*/
                        l.addEncsRegistos(enc);

                        /*Adiçao a fila de recolha da loja*/
                        l.addEncsFilaR(enc);

                        /*Muda o estado da encomenda para aceite*/
                        u.aceitaEncomenda(enc.getCodEncomenda());

                        break;
                    }

                    default:
                        break;
                }

        }
  }

  public static Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");

        String codUtilizador = campos[0];
        String nome = campos[1];

        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        return new Utilizador(new TreeMap<>(), gpsx, gpsy, nome, codUtilizador);
  }

  public static Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");

        String codVoluntario = campos[0];
        String nome = campos[1];

        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        double raio = Double.parseDouble(campos[4]);

        return new Voluntario(gpsx, gpsy, raio, 120, new TreeMap<>(), codVoluntario, nome, false, false, false, null, new ArrayList<>());
  }

  public static Transportadora parseTransportadora(String input){
      String[] campos = input.split(",");

      String codTransportadora = campos[0];
      String nome = campos[1];

      double gpsx = Double.parseDouble(campos[2]);
      double gpsy = Double.parseDouble(campos[3]);

      String nif = campos[4];

      double raio = Double.parseDouble(campos[5]);

      double ppkm = Double.parseDouble(campos[6]);

      /*dar randomize ao peso*/
      return new Transportadora(gpsx, gpsy, codTransportadora, nome, nif, raio, ppkm, 0, 100, 200, false, false, false, new TreeMap<>(), new ArrayList<>(), new ArrayList<>());
  }

  public static Loja parseLoja(String input){
        String[] campos = input.split(",");

        String codLoja = campos[0]; 
        String nomeLoja = campos[1];

        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);

        return new Loja(codLoja, nomeLoja, gpsx, gpsy, new TreeMap<>(), new ArrayList<>(), new TreeMap<>());
  }

  public static Encomenda parseEncomenda(String input){
      String[] campos = input.split(",");

      String codEncomenda = campos[0];
      String codUtilizador = campos[1];
      String codLoja = campos[2];

      double peso = Double.parseDouble((campos[3]));

      List<Produto> prods = new ArrayList<>();

      for(int i = 4; i < campos.length; i += 4){
          prods.add(parseProdutos(campos[i], campos[i+1], campos[i+2], campos[i+3]));
      }

      return new Encomenda(codEncomenda, codUtilizador, codLoja, null, peso, false, false, false, false, prods);
  }

  public static Produto parseProdutos(String campo1, String campo2, String campo3, String campo4){
      double qtdd = Double.parseDouble(campo3);
      double valorUnitario = Double.parseDouble(campo4);

      return new Produto(campo1, campo2, qtdd, valorUnitario);
  }

  public static String parseAceite(String input){
      String[] campos = input.split(",");

      String str = campos[0];

      return str;
  }

  public static List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        BufferedReader br;

        try {
            br = new BufferedReader(new FileReader(nomeFich));
            String line = br.readLine();

            while(!line.equals("Dados de LOGS:")){
                line = br.readLine();
            }

            while(line != null) {
                lines.add(line);
                line = br.readLine();
            }

            br.close();
        }

        catch(IOException exc) {
            System.out.println(exc.getMessage());
        }

        return lines;
  }

}