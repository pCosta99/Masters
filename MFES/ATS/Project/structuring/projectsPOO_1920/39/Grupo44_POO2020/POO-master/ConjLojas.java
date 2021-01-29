import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.TreeMap;
import java.util.HashMap;
import java.util.List;
import java.util.*;
import java.security.SecureRandom;
/**
 * Write a description of class ConjLoja here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class ConjLojas
{
    /** Map de utilizadores identificados pelo seu codigo */
  private Map<String,Loja> lojas;
  private HashMap<String,String> dados = new HashMap<String,String>();
  private HashMap<String,String> exemplo = new HashMap<String,String>();
  private HashMap<HashMap<String, String>,Loja> dadosL = new HashMap<HashMap<String, String>,Loja>();

  /** Lista de lojas**/
  private List<Loja> lLojas;

  /** Ficheiros **/
  private File fileteste = new File ("TestesL.txt");
  private File filecredencais = new File ("CredenciaisL.txt");
  private Path pteste = Paths.get("TestesL.txt");
  private Path pcredenciais = Paths.get("CredenciaisL.txt");

  /** Construtor nulo */
  public ConjLojas() throws IOException {
      this.lojas = new HashMap<String,Loja>();
      this.lLojas = new ArrayList<>();
      if (!fileteste.createNewFile()) ler();
      if (!filecredencais.createNewFile()) lerCredenciais();
  }
  
  /** Construtor parametrizado para a classe ConjLojas */
  public ConjLojas(Map<String,Loja> map){
      this.lojas = new HashMap<String,Loja>();
      for(Loja l: map.values()){
          this.lojas.put(l.getCodLoja(),l.clone());
      }
  }
  
  /** Construtor de cópia */
  public ConjLojas(ConjLojas cl){
      this.lLojas = cl.getLL();
      this.setLojas(cl.getLojas());
  }
  
  /** Retorna a lista das Lojas **/
    public List<Loja> getLL(){
        return this.lLojas;
    }

    /** Define a lista das Lojas **/
    public void setLL(List<Loja> cl){
        this.lLojas = cl;
    }
  /** Método que retorna um map das lojas */
  public Map<String,Loja> getLojas(){
      Map<String,Loja> map = new HashMap<>();
      for(Loja l : this.lojas.values() ){
          map.put(l.getCodLoja(),l);
      }
      return map;
  }
  
    public List<Loja> getListaLojas(){
        return lLojas;
  }
  
  /** Método que define o map das lojas */
  public void setLojas(Map<String,Loja> map){
      this.lojas = new HashMap<>();
      for(Loja l : map.values()){
          this.lojas.put(l.getCodLoja(),l.clone());
      }
  }
  
  /** Método que clona um ConjLojas */
  public ConjLojas clone(){
      return new ConjLojas(this);
  }
  
  /** Método que devolve um boolean true caso os ConjLojas sejam iguais e false caso não sejam */
  public boolean equals(Object o){
      if (o==this) return true;
      if (o==null || o.getClass() != this.getClass()) return false;
      ConjLojas cl = (ConjLojas) o;
      return this.lojas.equals(cl.getLojas());
  }
  
  /** Método que cria uma string com a informação do ConjLojas */
  public String toString(){
      StringBuilder sb = new StringBuilder();
      for(Loja l : this.lojas.values()){
          sb.append(l.toString()+"\n");
      }
      return sb.toString();
  }
  
  /** Método que adiciona uma Loja */
    public void adicionaLoja(Loja l){
        this.lojas.put(l.getCodLoja(),l);
  }

  /** Adiciona uma nova loja**/
    public boolean addDados (String cod, String nome, double x, double y, String email, String pass) throws IOException {
        Loja loja =  new Loja();
        Localizacao l = new Localizacao();
        loja.setNomeLoja(nome);
        loja.setCodLoja(cod);
        l.setX(x);
        l.setY(y);
        loja.setGps(l);
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
                dadosL.put(exemplo,loja);
                System.out.printf("Dados guardados com sucesso.\n");
                exemplo.clear();
                escreveCredenciais(loja,email,pass);
                escreveLoja(loja);
                lLojas.add(loja);
            }
        }
        return true;
    }

    /** Verifica se os dados inseridos estão corretos **/
    public boolean validaDados (String email, String pass){
        Loja loja = new Loja();
        String value;
        if (dados.containsKey(email)){
            value = dados.get(email);
            if (value.equals(pass)){
                exemplo.put(email,pass);
                loja = dadosL.get(exemplo);
                exemplo.clear();
                System.out.printf("Inicio de sessão bem sucedido.\n");
                return true;
            }
            else System.out.printf("Dados invalidos.\n");

        }
        else System.out.printf("Dados invalidos.\n");
        return false;
    }

    /** Escreve as credências de inicio de sessão **/
    public void escreveCredenciais (Loja l, String email, String pass) throws IOException {
        String c = l.getCodLoja();
        if (filecredencais.createNewFile() || !filecredencais.createNewFile()){
            try{
                PrintWriter pW = new PrintWriter (new FileWriter(filecredencais,true));
                pW.printf("Loja:%s,%s,%s\n",c,email,pass);
                pW.close ();
            }
            catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }
    }

    /** Escreve as informações da loja **/
    public void escreveLoja (Loja nome) throws IOException {
        String n = nome.getNomeLoja();
        String c = nome.getCodLoja();
        Localizacao l = nome.getGps();
        double x = l.getX();
        double y = l.getY();

        String xf = String.valueOf(x);
        String yf = String.valueOf(y);

        try{
            PrintWriter pW = new PrintWriter (new FileWriter(fileteste,true));
            pW.printf("\nLoja:%s,%s,%s,%s",c,n,xf,yf);
            pW.close ();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    /** Lê o ficheiros de informações das lojas **/
    public List<Loja> ler() {
        Localizacao lc = new Localizacao();
        
        try {
            String e1, e2;
            String u = "Loja:";
            List<String> l = Files.readAllLines(pteste);
            int i, j, l1, l2, n = 0;
            double num, num2;
            for (String s : l) {
                if(s.length() > 0) {
                    Loja loja = new Loja();
                if (s.substring(0, 5).equals(u)) {
                    for (i = 5; i > 0; i++) {
                        if ((e1 = s.substring(i, i + 1)).equals(",")) {
                            e1 = s.substring(5, i);
                            loja.setCodLoja(e1);
                            n = i + 1;
                            break;
                        }
                    }
                    for (j = n; j > 0; j++) {
                        if ((e1 = s.substring(j, j + 1)).equals(",")) {
                            e1 = s.substring(n, j);
                            loja.setNomeLoja(e1);
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
                            loja.setGps(lc);
                            break;
                        }
                    }
                }
                lLojas.add(loja);
            }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return lLojas;
    }

    /** Lê o ficheiro das credencias das lojas **/
    public List<String> lerCredenciais() {
        List<String> codigos = new ArrayList<String>();
        try {
            String e1, e2, cd;
            String u = "Loja:";
            List<String> l = Files.readAllLines(pcredenciais);
            int i, n = 0, c1, c2;
            double num, num2;
            for (String s : l) {
                if (s.substring(0, 5).equals(u)) {
                    for (i = 5; i > 0; i++) {
                        if ((e1 = s.substring(i,i+1)).equals(",")) {
                            cd = s.substring(5,i);
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






