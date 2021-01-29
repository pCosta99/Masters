import java.util.Map;
import java.util.TreeMap;
import java.util.HashMap;
import java.util.List;
import java.util.*;
import java.io.*;
import java.lang.management.LockInfo;
import java.lang.reflect.Array;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.security.SecureRandom;
/**
 * Write a description of class ConjUtilizadores here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class ConjUtilizadores

{
  /** Map de utilizadores identificados pelo seu codigo */
  private Map<String,Utilizador> utilizadores;
  private HashMap<String,String> codEmail = new HashMap<String,String>();
  /** Lista de utilizadores **/
  private List<Utilizador> lUtilizadores = new ArrayList<>();

    /** Ficheiros **/
    private File fileteste = new File ("teste.txt");
    private File filecredencais = new File ("credenciais.txt");
    private Path pteste = Paths.get("teste.txt");
    private Path pcredenciais = Paths.get("credenciais.txt");

    /** HashMap das credenciais de incio de sessao**/
    private HashMap<String,String> dados = new HashMap<String,String>();
    private HashMap<String,String> exemplo = new HashMap<String,String>();

  /** HashMap das credenciais de inicio de sessão que identificados pelo codigo de utilizaadores**/
    private HashMap<HashMap<String, String>,Utilizador> dadosf = new HashMap<HashMap<String, String>,Utilizador>();
   
  /** Construtor nulo */
  public ConjUtilizadores() throws IOException {
      this.utilizadores = new HashMap<String,Utilizador>();
      if (!fileteste.createNewFile()) ler();
      if (!filecredencais.createNewFile()) lerCredenciais();
  }
  
  /** Construtor parametrizado para a classe ConjUtilizadores */
  public ConjUtilizadores(Map<String,Utilizador> map){
      this.utilizadores = new HashMap<String,Utilizador>();
      for(Utilizador u: map.values()){
          this.utilizadores.put(u.getCodUtilizador(),u.clone());
      }
  }
  
  /** Construtor de cópia */
  public ConjUtilizadores(ConjUtilizadores cu){
      this.setUtilizadores(cu.getUtilizadores());
  }
  
  /** Método que retorna um map dos utilizadores */
  public Map<String,Utilizador> getUtilizadores(){
      Map<String,Utilizador> map = new HashMap<>();
      for(Utilizador u : this.utilizadores.values() ){
          map.put(u.getCodUtilizador(),u);
      }
      return map;
  }
   public String getUtilizadorByEmail(String email) {
      String result = codEmail.get(email);
      return result;
  }
  public Utilizador getUtilizadorBycod(String cod) {
      Utilizador result = new Utilizador();
      for(Utilizador u : lUtilizadores) {
        if(u.getCodUtilizador().equals(cod)) {
            result = u;
        }
        
    }
    return result;
}
  
  /** Método que define o map dos utilizadores */
  public void setUtilizadores(Map<String,Utilizador> map){
      this.utilizadores = new HashMap<>();
      for(Utilizador u : map.values()){
          this.utilizadores.put(u.getCodUtilizador(),u.clone());
      }
  }
  
  /** Método que clona um ConjUtilizadores */
  public ConjUtilizadores clone(){
      return new ConjUtilizadores(this);
  }
  
  /** Método que devolve um boolean true caso os ConjUtilizadores sejam iguais e false caso não sejam */
  public boolean equals(Object o){
      if (o==this) return true;
      if (o==null || o.getClass() != this.getClass()) return false;
      ConjUtilizadores cu = (ConjUtilizadores) o;
      return this.utilizadores.equals(cu.getUtilizadores());
  }
  
  /** Método que cria uma string com a informação do ConjUtilizadores */
  public String toString(){
      StringBuilder sb = new StringBuilder();
      for(Utilizador u : this.utilizadores.values()){
          sb.append(u.toString()+"\n");
      }
      return sb.toString();
  }
  
  /** Método que adiciona um Utilizador */
    public void adicionaUtilizador(Utilizador u){
        this.utilizadores.put(u.getCodUtilizador(),u);
  }

  /** Metodo que adiciona uma nova conta de utilizador **/
    public boolean addDados (String cod, String nome, double x, double y, String email, String pass) throws IOException {
        HashMap<String, String> exemplo = new HashMap<String, String>();
        Utilizador u =  new Utilizador();
        Localizacao l = new Localizacao();
        u.setNome(nome);
        u.setCodUtilizador(cod);
        l.setX(x);
        l.setY(y);
        u.setPosicao(l);
        if (dados.containsKey(email)) {
            System.out.printf("Erro, dados já existem.\n");
            return false;
        }
        else{
            if(lerCredenciais().contains(cod)){
                System.out.printf("Erro, dados já existem.\n");
                return false;
            }
            else{
                dados.put(email,pass);
                exemplo.put(email,pass);
                dadosf.put(exemplo,u);
                System.out.printf("Dados guardados com sucesso.\n");
                exemplo.clear();
                escreveCredenciais(u,email,pass);
                escreveUtilizador(u);
                lUtilizadores.add(u);
            }
        }
        return true;
    }

    /** Método que valida os dados de um utilizador ao tentar iniciar sessão. **/
    public boolean validaDados (String email, String pass){
        Utilizador u = new Utilizador();
        HashMap<String, String> exemplo = new HashMap<String, String>();
        String value;
        if (dados.containsKey(email)){
            value = dados.get(email);
            if (value.equals(pass)){
                exemplo.put(email,pass);
                u = dadosf.get(exemplo);
                exemplo.clear();
                System.out.printf("Inicio de sessão bem sucedido.\n");
                return true;
            }
            else System.out.printf("Dados invalidos.\n");

        }
        else System.out.printf("Dados invalidos.\n");
        return false;
    }

    /** Método que escreve num ficheiro as credênciais do utilizador (código, email e pass). **/
    public void escreveCredenciais (Utilizador nome, String email, String pass) throws IOException {
        String c = nome.getCodUtilizador();
        if (filecredencais.createNewFile() || !filecredencais.createNewFile()){
            try{
                PrintWriter pW = new PrintWriter (new FileWriter(filecredencais,true));
                pW.printf("Utilizador:%s,%s,%s\n",c,email,pass);
                pW.close ();
            }
            catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }
    }

    /** Método que escreve num ficheiro as informações sobre um utilizador.**/
    public void escreveUtilizador (Utilizador nome) throws IOException {
        String n = nome.getNome();
        String c = nome.getCodUtilizador();
        Localizacao l = nome.getPosicao();
        double x = l.getX();
        double y = l.getY();

        String xf = String.valueOf(x);
        String yf = String.valueOf(y);

        try{
            PrintWriter pW = new PrintWriter (new FileWriter(fileteste,true));
            pW.printf("\nUtilizador:%s,%s,%s,%s",c,n,xf,yf);
            pW.close ();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    /** Método que lê o ficheiro que contem as informações sobre os utilizadores **/
    public void ler() {
        Localizacao lc = new Localizacao();
        
        try {
            String e1, e2;
            String u = "Utilizador:";
            List<String> l = Files.readAllLines(pteste);
            int i, j, l1, l2, n = 0;
            double num, num2;
            for (String s : l) {
                Utilizador utilizador = new Utilizador();
                if(s.length() > 0) {
                if (s.substring(0, 11).equals(u)) {
                    for (i = 11; i > 0; i++) {
                        if ((e1 = s.substring(i, i + 1)).equals(",")) {
                            e1 = s.substring(11, i);
                            utilizador.setCodUtilizador(e1);
                            n = i + 1;
                            break;
                        }
                    }
                    for (j = n; j > 0; j++) {
                        if ((e1 = s.substring(j, j + 1)).equals(",")) {
                            e1 = s.substring(n, j);
                            utilizador.setNome(e1);
                            n = j + 1;
                            break;
                        }
                    }
                    for (l1 = n; l1 > 0; l1++) {
                        if ((e1 = s.substring(l1, l1 + 1)).equals(",")) {
                            e1 = s.substring(n, l1);
                            n = l1 + 1;
                            e2 = s.substring(n);
                            num = Double.parseDouble(e1);
                            num2 = Double.parseDouble(e2);
                            lc.setX(num);
                            lc.setY(num2);
                            utilizador.setPosicao(lc);
                            break;
                        }
                    }
                }
                lUtilizadores.add(utilizador);
            }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /** Método que lê o ficheiro que contêm as informações sobre as credenciais dos utilizadores **/
    public List<String> lerCredenciais() {
        List<String> codigos = new ArrayList<String>();
        try {
            String e1, e2, cd;
            String code = "";
            String u = "Utilizador:";
            List<String> l = Files.readAllLines(pcredenciais);
            int i, n = 0, c1, c2;
            double num, num2;
            for (String s : l) {
                if (s.substring(0, 11).equals(u)) {
                    for (i = 11; i > 0; i++) {
                        if ((e1 = s.substring(i,i+1)).equals(",")) {
                            cd = s.substring(11,i);
                            code = cd;
                            codigos.add(cd);
                            n = i + 1;
                            break;
                        }
                    }
                    for (c1 = n; c1 > 0; c1++) {
                        if ((e1 = s.substring(c1,c1+1)).equals(",")) {
                            e1 = s.substring(n,c1);
                            n = c1+1;
                            e2 = s.substring(n);
                            dados.put(e1,e2);
                            codEmail.put(e1,code);
                            break;
                        }
                    }
                }
            }
        }
        catch(IOException e2){
            e2.printStackTrace();
        }
        return codigos;
    }
     /** Método para gerar o código aleatório para o aluguer */
    
    private static final String ALPHABET = "0123456789";
    private static final SecureRandom RANDOM = new SecureRandom();

    public String geraCodigo( String u) {
        StringBuilder sb = new StringBuilder();
        sb.append(u);
        for (int i = 0; i < 2; ++i) {
            sb.append(ALPHABET.charAt(RANDOM.nextInt(ALPHABET.length())));
        }
        return sb.toString();
    }
}
    










