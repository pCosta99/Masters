/**
 * A classe Parse tem como finalidade analisar os logs
 * contendo os utilizadores, voluntário, etc...
 * disponibilizados pelos docentes no decorrer do projecto.
 *
 * Um objecto Parse contém uma colecção de utilizadores,
 * lojas, voluntários, transporatdoras e encomendas. 
 * Contém também informação sobre o caminho do ficheiro
 * de logs utilizado nas sua criação.
 * @author grupo60
 * @version 1.0
 */
import java.util.ArrayList;
import java.util.List;
import java.io.IOException;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.LocalDateTime;

public class Parse {

	private String file_path;
    private List<Utilizador> utilizadores;
    private List<Loja> lojas;
    private List<Voluntario> voluntarios;
    private List<Transportadora> transportadoras;
    private List<Encomenda> encomendas;
    private List<Servico> servicos;
    private List<String> aceites;


    /**
    * Construtor por omissão para a classe Parse.
    */
    public Parse(){
        this.file_path = "";
        this.utilizadores = new ArrayList<Utilizador>();
        this.lojas = new ArrayList<Loja>();
        this.voluntarios = new ArrayList<Voluntario>();
        this.transportadoras = new ArrayList<Transportadora>();
        this.encomendas = new ArrayList<Encomenda>();
        this.servicos = new ArrayList<Servico>();
        this.aceites = new ArrayList<>();
    }

    /**
    * Construtor parametrizado para a classe Parse.
    * @param file_path O caminho para o ficheiro de logs.
    */
    public Parse (String file_path){
        this.file_path = file_path;
        this.utilizadores = new ArrayList<Utilizador>();
        this.lojas = new ArrayList<Loja>();
        this.voluntarios = new ArrayList<Voluntario>();
        this.transportadoras = new ArrayList<Transportadora>();
        this.encomendas = new ArrayList<Encomenda>();
        this.servicos = new ArrayList<Servico>();
        this.aceites = new ArrayList<>();
    }

    /**
    * Construtor cópia para a classe Parse.
    * @param parse Uma instância da classe Parse.
    */
    public Parse (Parse parse){
        this.file_path = parse.getFilePath();
        this.utilizadores = parse.getUtilizadores();
        this.lojas = parse.getLojas();
        this.voluntarios = parse.getVoluntarios();
        this.transportadoras = parse.getTransportadoras();
        this.encomendas = parse.getEncomendas();
        this.servicos = parse.getServicos();
        this.aceites = parse.getAceites();
    }

    /**
    * Método Getter para a classe Parse.
    * @return String que corresponde ao caminho do ficheiro
    * de logs usado na criação do objecto Parse.
    */
    public String getFilePath(){
        return this.file_path;
    }

    /**
    * Método Getter da colecção de Utilizadores de uma
    * instância da classe Parse.
    * @return Uma colecção de Utilizador.
    */
    public List<Utilizador> getUtilizadores(){
        return new ArrayList<Utilizador>(this.utilizadores);
    }

    /**
    * Método Getter da colecção de lojas de uma
    * instância da classe Parse.
    * @return Uma colecção de Loja.
    */
    public List<Loja> getLojas(){
        return new ArrayList<Loja>(this.lojas);
    }

    /**
    * Método Getter da colecção de voluntários de uma
    * instância da classe Parse.
    * @return Uma colecção de Voluntario.
    */
    public List<Voluntario> getVoluntarios(){
        return new ArrayList<Voluntario>(this.voluntarios);
    }

    /**
    * Método Getter da colecção de transportadoras de uma
    * instância da classe Parse.
    * @return Uma colecção de Transportadora.
    */
    public List<Transportadora> getTransportadoras(){
        return new ArrayList<Transportadora>(this.transportadoras);
    }

    /**
    * Método Getter da colecção de encomendas de uma
    * instância da classe Parse.
    * @return Uma colecção de Encomenda.
    */
    public List<Encomenda> getEncomendas(){
        return new ArrayList<Encomenda>(this.encomendas);
    }

    /**
    * Método Getter da colecção de serviços de uma
    * instância da classe Parse.
    * @return Uma colecção de Serviço.
    */
    public List<Servico> getServicos(){
        return new ArrayList<Servico>(this.servicos);
    }



    public List<String> getAceites(){return new ArrayList<>(this.aceites);}


    /**
    * Método Setter da variável de instância file_path
    * da classe Parse.
    * @param file_path Um caminho para o ficheiro logs
    * usado na criação de uma instância da classe Parse.
    */
    public void setFilePath(String file_path){
        this.file_path = file_path;
    }

    /**
    * Método Setter da colecção de utilizadores de uma
    * instância da classe Parse.
    * @param utilizadores Uma colecção de Utilizador.
    */
    public void setUtilizadores(List<Utilizador> utilizadores){
        List<Utilizador> clone = new ArrayList<Utilizador>(utilizadores);
        this.utilizadores = clone;
    }

    /**
    * Método Setter da colecção de lojas de uma
    * instância da classe Parse.
    * @param lojas Uma colecção de Loja.
    */
     public void setLojas(List<Loja> lojas){
        List<Loja> clone = new ArrayList<Loja>(lojas);
        this.lojas = clone;
    }

    /**
    * Método Setter da colecção de voluntarioss de uma
    * instância da classe Parse.
    * @param voluntarios Uma colecção de Voluntario.
    */
     public void setVoluntarios(List<Voluntario> voluntarios){
        List<Voluntario> clone = new ArrayList<Voluntario>(voluntarios);
        this.voluntarios = clone;
    }

    /**
    * Método Setter da colecção de transportadoras de uma
    * instância da classe Parse.
    * @param transportadoras Uma colecção de Transportadora.
    */
     public void setTransportadoras(List<Transportadora> transportadoras){
        List<Transportadora> clone = new ArrayList<Transportadora>(transportadoras);
        this.transportadoras = clone;
    }

    /**
    * Método Setter da colecção de encomendas de uma
    * instância da classe Parse.
    * @param encomendas Uma colecção de Encomenda.
    */
     public void setEncomendas(List<Encomenda> encomendas){
        List<Encomenda> clone = new ArrayList<Encomenda>(encomendas);
        this.encomendas = clone;
    }

    /**
    * Método Setter da colecção de serviços de uma
    * instância da classe Parse.
    * @param servicos Uma colecção de Serviço.
    */
     public void setServicos(List<Servico> servicos){
        List<Servico> clone = new ArrayList<Servico>(servicos);
        this.servicos = clone;
    }


    public void setAceites(List<String> aceites){
         List<String> clone = new ArrayList<>(aceites);
         this.aceites=clone;
    }

    /**
    * Método que analisa as linhas de um ficheiro de logs.
    * Cria instâncias de Utilizador, Loja, Volntario, Transportadora
    *  encomenda e aceites.
    */
	public void parse(){
		
		List<String> linhas = lerFicheiro(file_path);
		String[] linhaPartida;
		for (String linha: linhas){
			linhaPartida = linha.split(":",2);
			switch(linhaPartida[0]){
				        case "Utilizador":

				        if (linhaPartida[1].charAt(0) == '<') {
				        	break;
				        }

                        Utilizador u = parseUtilizador(linhaPartida[1]);
                        this.utilizadores.add(u);
                        break;
                case "Loja": 
                if (linhaPartida[1].charAt(0) == '<') {
				        	break;
				        }
                        Loja l = parseLoja(linhaPartida[1]);
                        this.lojas.add(l);
                        break;                                   
                case "Voluntario":
                if (linhaPartida[1].charAt(0) == '<') {
				        	break;
				        }
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        this.voluntarios.add(v);
                        break;
                case "Transportadora":
                if (linhaPartida[1].charAt(0) == '<') {
				        	break;
				        }
                        Transportadora t = parseTransportadora(linhaPartida[1]);
                        this.transportadoras.add(t);
                        break;
                case "Encomenda":
                if (linhaPartida[1].charAt(0) == '<') {
				        	break;
				        }
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        this.encomendas.add(e);
                        break;
                case "Aceite":
                if (linhaPartida[1].charAt(0) == '<') {
				        	break;
				        }
                        String s = parseAceites(linhaPartida[1]);
                        this.aceites.add(s);
                        break;

                case "Servico":
                		Servico ser = parseServico(linhaPartida[1]);
                		this.servicos.add(ser);
                		break;
                default:
                        break;
                }

        }
        System.out.println("done!");
  }

  /**
  * Método que cria uma instância de Utilizador.
  * @param input Uma String com a informação de um utilizador.
  * @return Um Utilizador.
  */
    public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String nome = campos[1]; 
        String codUtilizador = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        return new Utilizador(codUtilizador, nome, new GPS(gpsx, gpsy));
  }

  /**
  * Método que cria uma instância de Loja.
  * @param input Uma String com a informação de uma loja.
  * @return Uma Loja.
  */
  public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
      double gpsx = Double.parseDouble(campos[2]);
      double gpsy = Double.parseDouble(campos[3]);
        return new Loja(codLoja, nomeLoja,new GPS(gpsx,gpsy));
  }

  /**
  * Método que cria uma instância de Voluntario.
  * @param input Uma String com a informação de um voluntario.
  * @return Um Voluntario.
  */
  public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String nome = campos[1]; 
        String codVoluntario = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        return new Voluntario(codVoluntario, nome, new GPS(gpsx, gpsy), raio);
  }

  /**
  * Método que cria uma instância de Transportadora.
  * @param input Uma String com a informação de uma transportadora.
  * @return Uma Transportadora.
  */
  public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String codTransportadora = campos[0];
        String nome = campos[1];
         double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double precoKm = Double.parseDouble(campos[6]);
        return new Transportadora(codTransportadora, nome,
            new GPS (gpsx, gpsy), nif, raio, precoKm);
  }

  /**
  * Método que cria uma instância de Encomenda.
  * @param input Uma String com a informação de uma encomenda.
  * @return Uma Encomenda.
  */
  public Encomenda parseEncomenda(String input) {
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String codUtilizador = campos[1];
        String codLoja = campos[2];
        double peso = Double.parseDouble(campos[3]);
        ArrayList<LinhaEncomenda> l = new ArrayList<LinhaEncomenda>();

        int size = campos.length;
        size -= 4;
        int numLinhasEncomendas = size /4;
        int i = 0;

        for (i= 0; i < numLinhasEncomendas; i++) {
            String linhas[] = new String[4];
            linhas[0] = campos[4 + i *4];
            linhas[1] = campos[5 + i*4];
            linhas[2] = campos[6+ i*4];
            linhas[3] = campos[7+ i*4];
            l.add(parseLinhaEncomenda(linhas));
        }

        return new Encomenda(codEncomenda, codUtilizador,
            codLoja, peso, l);

  }

  /**
  * Método que cria uma instância de LinhaEncomenda.
  * @param input Uma String com a informação de uma linha de encomenda.
  * @return Uma LinhaEncomenda.
  */
  public LinhaEncomenda parseLinhaEncomenda(String input[]){

        return new LinhaEncomenda (input[0], input[1], 
            Double.parseDouble(input[2]),
            Double.parseDouble(input[3]));
  }


  /**
  * Método que cria uma instância de Serviço.
  * @param input Uma String com a informação de um Serviço.
  * @return Um Serviço.
  */
  public Servico parseServico (String input) {
      Servico s = new Servico();
      String[] campos = input.split(",");
      ArrayList<LinhaEncomenda> l = new ArrayList<LinhaEncomenda>();
     
      s.setCodServico(campos[0]);
    
      s.setClassificacao(Double.parseDouble(campos[1]));
	
      if (campos[2].equals("true")) {
      	s.setConcluido(true);
      }
      else if (campos[2].equals("false")) {
      	s.setConcluido(false);
      }
      
      Encomenda e = new Encomenda();
      e.setCodigoEncomenda(campos[3]);
      
      e.setCodigoUtilizador(campos[4]);
      s.setCodUtilizador(campos[4]);
      
      e.setCodigoLoja(campos[5]);
      s.setCodTranportador(campos[6]);

      e.setPeso(Double.parseDouble(campos[7]));

      int i = 8;

      while (campos[i].charAt(0) == 'p'){
      	 String linhas[] = new String[4];
      	 linhas[0] = campos[i];
      	 linhas[1] = campos[i+1];
      	 linhas[2] = campos[i+2];
      	 linhas[3] = campos[i+3];
      	 i+= 4;
      	 e.setLinhas(parseLinhaEncomenda(linhas));

      }

      s.setEncomenda(e);

      //2020-04-08 campos[0]
      //12:30 campos[1]
      String[] campos2 = campos[i].split(" ");

      // 2020
      // 04
      // 08
      String[]campos3 = campos2[0].split("-");


      String[]campos4 = campos2[1].split(":");

      LocalDateTime t1 = LocalDateTime.of(Integer.parseInt(campos3[0]), Integer.parseInt(campos3[1]), Integer.parseInt(campos3[2]),
      	Integer.parseInt(campos4[0]),Integer.parseInt(campos4[1]));

      s.setDataInicio(t1);

      i += 1;

      //2020-04-08 campos[0]
      //12:30 campos[1]
      campos2 = campos[i].split(" ");

      // 2020
      // 04
      // 08
      campos3 = campos2[0].split("-");


      campos4 = campos2[1].split(":");

      LocalDateTime t2 = LocalDateTime.of(Integer.parseInt(campos3[0]), Integer.parseInt(campos3[1]), Integer.parseInt(campos3[2]),
      	Integer.parseInt(campos4[0]),Integer.parseInt(campos4[1]));

      s.setDataFim(t2);

      i+= 1;

      s.setKmPercorridos(Double.parseDouble(campos[i]));
      i+=1;

      s.setPreco(Double.parseDouble(campos[i]));
      i+=1;
      s.setDias(Integer.parseInt(campos[i]));




      //System.out.println(campos2[0]);
      //System.out.println(campos2[1]);
      

      return s;
  }


  public String parseAceites(String input){
      return input;
  }

  /**
  * Método que carrega as linhas de um ficheiro de logs para uma
  * colecção de Strings.
  * @param nomeFich String que corresponde ao nome do ficheiro de logs
  * a analisar.
  * @return Uma colecção de Strings.
  */
  public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(file_path), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
  }

  /**
  * Método toString para a classe Parse.
  * @return Uma String com a informação sobre o estado de
  * uma instância da classe Parse. 
  */
  public String toString(){
    StringBuilder sb = new StringBuilder();
    sb.append("file_path: ").append(this.file_path).append("\n");
    for(Utilizador u: this.utilizadores){
        sb.append(u.toString()).append("\n");
    }
    for(Loja l: this.lojas){
        sb.append(l.toString()).append("\n");
    }
    for(Voluntario v: this.voluntarios){
        sb.append(v.toString()).append("\n");
    }
    for(Transportadora t: this.transportadoras){
        sb.append(t.toString()).append("\n");
    }
    for(Encomenda e: this.encomendas){
        sb.append(e.toString()).append("\n");
    }

    for (Servico s: this.servicos){
        sb.append(s.getEncomenda().getCodigoEncomenda()).append("\n");
    }
    return sb.toString();
  }

  /**
  * Método clone para a classe Parse.
  * @return Um clone de um objecto da classe Parse.
  */
  public Parse clone(){
    return new Parse(this);
  }
}