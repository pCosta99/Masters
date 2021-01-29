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
 * Write a description of class ConjTransportadoraa here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class ConjTransportadores extends Localizacao
{
  /** Map de Transportadores identificados pelo seu codigo */
  private Map<String,Transportadora> transportadores;
  private HashMap<String,String> dados = new HashMap<String,String>();
  private HashMap<String,String> exemplo = new HashMap<String,String>();
  private HashMap<HashMap<String, String>,Transportadora> dadosT = new HashMap<HashMap<String, String>,Transportadora>();
  
  /** HashMap que relaciona as transportadoras com a sua avaliação**/
    private HashMap<Transportadora,Avaliar> avaliacao = new HashMap<Transportadora,Avaliar>();
  /** Lista das transportadoras**/
  private List<Transportadora> lTransportadora = new ArrayList<>();
  private HashMap<Transportadora,Utilizador> relaT = new HashMap<Transportadora,Utilizador>();

  /** Ficheiros **/
  private File fileteste = new File ("TestesT.txt");
  private File filecredencais = new File ("CredenciaisT.txt");
  private Path pteste = Paths.get("TestesT.txt");
  private Path pcredenciais = Paths.get("CredenciaisT.txt");
   
  /** Construtor nulo */
  public ConjTransportadores() throws IOException {
      this.transportadores = new HashMap<String,Transportadora>();
      this.lTransportadora = new ArrayList<>();
      if (!fileteste.createNewFile()) ler();
      if (!filecredencais.createNewFile()) lerCredenciais();
  }
  
  /** Construtor parametrizado para a classe ConjTransportadores */
  public ConjTransportadores(Map<String,Transportadora> map){
      this.transportadores = new HashMap<String,Transportadora>();
      for(Transportadora t: map.values()){
          this.transportadores.put(t.getCodEmpresa(),t.clone());
      }
  }
  
  /** Retorna a lista das transportadoras **/
    public List<Transportadora> getLT(){
        return this.lTransportadora;
    }

    /** Define a lista das transportadoras **/
    public void setCodLT(List<Transportadora> cl){
        this.lTransportadora = cl;
    }
  
  /** Construtor de cópia */
  public ConjTransportadores(ConjTransportadores ct){
      this.lTransportadora = ct.getLT();
      this.setTransportadores(ct.getTransportadores());
  }
  
  /** Método que retorna um map dos Transportadores */
  public Map<String,Transportadora> getTransportadores(){
      Map<String,Transportadora> map = new HashMap<>();
      for(Transportadora t : this.transportadores.values() ){
          map.put(t.getCodEmpresa(),t);
      }
      return map;
  }
  
  /** Método que define o map dos Transportadores */
  public void setTransportadores(Map<String,Transportadora> map){
      this.transportadores = new HashMap<>();
      for(Transportadora t : map.values()){
          this.transportadores.put(t.getCodEmpresa(),t.clone());
      }
  }
  
  /** Método que clona um ConjTransportadores */
  public ConjTransportadores clone(){
      return new ConjTransportadores(this);
  }
  
  /** Método que devolve um boolean true caso os ConjTransportadores sejam iguais e false caso não sejam */
  public boolean equals(Object o){
      if (o==this) return true;
      if (o==null || o.getClass() != this.getClass()) return false;
      ConjTransportadores ct = (ConjTransportadores) o;
      return this.transportadores.equals(ct.getTransportadores());
  }
  
  /** Método que cria uma string com a informação do ConjTransportadores */
  public String toString(){
      StringBuilder sb = new StringBuilder();
      for(Transportadora t : this.transportadores.values()){
          sb.append(t.toString()+"\n");
      }
      return sb.toString();
  }
  
  /** Método que adiciona um Transportadora */
    public void adicionaTransportadora(Transportadora t){
        this.transportadores.put(t.getCodEmpresa(),t);
  }

    public boolean addDados (String cod, String nome, double x, double y, double r, double p, int nif, String email, String pass) throws IOException {
        Transportadora t =  new Transportadora();
        Localizacao l = new Localizacao();

        t.setCodEmpresa(cod);
        t.setNome(nome);
        t.setNif(nif);
        l.setX(x);
        l.setY(y);
        t.setRaio(r);
        t.setPrecokm(p);
        t.setPosicao(l);

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
                dadosT.put(exemplo,t);
                System.out.printf("Dados guardados com sucesso.\n");
                exemplo.clear();
                escreveCredenciais(t,email,pass);
                escreveUtilizador(t);
                lTransportadora.add(t);
            }
        }
        return true;
    }

    public boolean validaDados (String email, String pass){
        Transportadora u = new Transportadora();
        String value;
        if (dados.containsKey(email)){
            value = dados.get(email);
            if (value.equals(pass)){
                exemplo.put(email,pass);
                u = dadosT.get(exemplo);
                exemplo.clear();
                System.out.printf("Inicio de sessão bem sucedido.\n");
                return true;
            }
            else System.out.printf("Dados invalidos.\n");

        }
        else System.out.printf("Dados invalidos.\n");
        return false;
    }

    public void escreveCredenciais (Transportadora nome, String email, String pass) throws IOException {
        String c = nome.getCodEmpresa();
        if (filecredencais.createNewFile() || !filecredencais.createNewFile()){
            try{
                PrintWriter pW = new PrintWriter (new FileWriter(filecredencais,true));
                pW.printf("Transportadora:%s,%s,%s\n",c,email,pass);
                pW.close ();
            }
            catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }
    }

         public Transportadora aceitaEncT (List<Transportadora> t, Utilizador u, Encomenda e, List<Loja> lj){
        Localizacao l1 = u.getPosicao();
        int sizeV = t.size();
        int sizeL = lj.size();
        String l = e.getLoja();
        Transportadora tf = null;
        for (int i = 0; i < sizeV; i++){
            Transportadora t2 = t.get(i);
            Localizacao l2 = t2.getPosicao();
            double r = t2.getRaio();
            if ((Math.abs(distancia(l2,l1))) <= r){
                for (int j = 0; j < sizeL; j++){
                    Loja ljs = lj.get(j);
                    String lj1 = ljs.getCodLoja();
                    if (lj1.equals(l)){
                        Localizacao l3 = ljs.getGps();
                        if((Math.abs(distancia(l3,l2))) <= r){
                            relaT.put(t2,u);
                            String p = "S";
                            e.setEncomenda(p);
                            return t2;
                        }
                    }
                }
            }
        }
        return tf;
    }

    public void escreveUtilizador (Transportadora nome) throws IOException {
        String n = nome.getNome();
        String c = nome.getCodEmpresa();
        Localizacao l = nome.getPosicao();
        int nif = nome.getNif();
        double r = nome.getRaio();
        double p = nome.getPrecokm();
        double x = l.getX();
        double y = l.getY();

        String xf = String.valueOf(x);
        String yf = String.valueOf(y);
        String rf = String.valueOf(r);
        String pf = String.valueOf(p);

        try{
            PrintWriter pW = new PrintWriter (new FileWriter(fileteste,true));
            pW.printf("\nTransportadora:%s,%s,%s,%s,%d,%s,%s",c,n,xf,yf,nif,rf,pf);
            pW.close ();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    public void ler() {
        Localizacao lc = new Localizacao();
        Transportadora transportadora = new Transportadora();
        try {
            String e1, e2;
            String u = "Transportadora:";
            List<String> l = Files.readAllLines(pteste);
            int i, j, l1, l2, n = 0, l3, l4, nif;
            double num = 0, num2 = 0,r = 0, p = 0;
            for (String s : l) {
                if(s.length() > 0) {
                if (s.substring(0, 15).equals(u)) {
                    for (i = 15; i > 0; i++) {
                        if ((e1 = s.substring(i, i + 1)).equals(",")) {
                            e1 = s.substring(15, i);
                            transportadora.setCodEmpresa(e1);
                            n = i + 1;
                            break;
                        }
                    }
                    for (j = n; j > 0; j++) {
                        if ((e1 = s.substring(j, j + 1)).equals(",")) {
                            e1 = s.substring(n, j);
                            transportadora.setNome(e1);
                            n = j + 1;
                            break;
                        }
                    }
                    for (l1 = n; l1 > 0; l1++) {
                        if ((e1 = s.substring(l1, l1 + 1)).equals(",")) {
                            e1 = s.substring(n, l1);
                            num = Double.parseDouble(e1);
                            n = l1 + 1;
                            break;
                        }
                    }

                    for (l2 = n; l2 > 0; l2++) {
                        if ((e2 = s.substring(l2, l2 + 1)).equals(",")) {
                            e2 = s.substring(n, l2);
                            num = Double.parseDouble(e2);
                            lc.setX(num);
                            lc.setY(num2);
                            transportadora.setPosicao(lc);
                            n = l2 + 1;
                            break;
                        }
                    }

                    for (l3 = n; l3 > 0; l3++) {
                        if ((e1 = s.substring(l3, l3 + 1)).equals(",")) {
                            e1 = s.substring(n, l3);
                            nif = Integer.parseInt(e1);
                            transportadora.setNif(nif);
                            n = l3 + 1;
                            break;
                        }
                    }

                    for (l4 = n; l4 > 0; l4++) {
                        if ((e1 = s.substring(l4, l4 + 1)).equals(",")) {
                            e1 = s.substring(n, l4);
                            r = Double.parseDouble(e1);
                            transportadora.setRaio(r);
                            n = l4 + 1;
                            e2 = s.substring(n);
                            p = Double.parseDouble(e2);
                            transportadora.setPrecokm(p);
                            break;
                        }
                    }
                }
                lTransportadora.add(transportadora);
            }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public List<String> lerCredenciais() {
        List<String> codigos = new ArrayList<String>();
        try {
            String e1, e2, cd;
            String u = "Transportadora:";
            List<String> l = Files.readAllLines(pcredenciais);
            int i, n = 0, c1, c2;
            double num, num2;
            for (String s : l) {
                if (s.substring(0, 15).equals(u)) {
                    for (i = 15; i > 0; i++) {
                        if ((e1 = s.substring(i,i+1)).equals(",")) {
                            cd = s.substring(15,i);
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
/*
    public List<Transportadora> maisTUsaram(List<Transportadora> transp){
        List<Transportadora> t = new ArrayList<>();
        List<String> cT = new ArrayList<>();
        List<Integer> numb = new ArrayList<>();
        String tC;
        int count = 0;
        double pKm;
        int size = transp.size();
        if (size <= 10) return transp;
        else{
            for(int i = 0; i<10 ; i++){
                tC = t.get(i).getCodEmpresa();
                count = x;
                cT.add(tC);
                numb.add(count);
                t.add(transp.get(i));
            }
            for (int j = 9; j < size; j++){
                tC = t.get(j).getCodEmpresa();
                count = x;
                for (int k = 0; k < 10; k++){
                    if (count > numb.get(k)){
                        numb.remove(k);
                        numb.add(count);
                        cT.remove(k);
                        cT.add(tC);
                        t.remove(k);
                        t.add(transp.get(j));
                        break;
                    }
                }
            }
        }
        return t;
    }
    */
   public void AvaliaT (Transportadora t, double a){
        if(avaliacao.containsKey(t)){
            Avaliar av = avaliacao.get(t);
            int q = av.getQuantidade();
            double x = av.getAvaliacao()*q + a;
            av.setQuantidade(q+1);
            av.setAvaliacao(x/(q+1));
            avaliacao.put(t,av);
        }
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
