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
 * Write a description of class ConjVoluntarios here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class ConjVoluntarios extends Localizacao {
    /** Conjunto de Voluntarios identificadas pelo seu respetivo código */
    private Map<String,Voluntario> voluntarios;
    private HashMap<String,String> dados = new HashMap<String,String>();
    private HashMap<String,String> exemplo = new HashMap<String,String>();
    private HashMap<HashMap<String, String>,Voluntario> dadosf = new HashMap<HashMap<String, String>,Voluntario>();
    /** HashMap que relaciona os voluntarios com a sua avaliação**/
    private HashMap<Voluntario,Avaliar> avaliacao = new HashMap<Voluntario,Avaliar>();
    /** Lista de Voluntarios **/
    private List<Voluntario> lVoluntario = new ArrayList<>();
    private HashMap<Voluntario,Utilizador> relaV = new HashMap<Voluntario,Utilizador>();
    /** Ficheiros **/
    private File fileteste = new File ("TestesV.txt");
    private File filecredencais = new File ("CredenciaisV.txt");
    private Path pteste = Paths.get("TestesV.txt");
    private Path pcredenciais = Paths.get("CredenciaisV.txt");

    /** Construtor nulo */
    public ConjVoluntarios() throws IOException {
        this.voluntarios = new HashMap<>();
        this.lVoluntario = new ArrayList<>();
        if (!fileteste.createNewFile()) ler();
        if (!filecredencais.createNewFile()) lerCredenciais();
    }
    
    /** Construtor parametrizado para a classe ConjVoluntarios */
    public ConjVoluntarios(Map<String,Voluntario> map){
        this.voluntarios = new HashMap<>();
        for(Voluntario v : map.values()){
            this.voluntarios.put(v.getCodVoluntario(), v.clone());
        }
    }
    
    /** Construtor por cópia */
    public ConjVoluntarios(ConjVoluntarios cv){
        this.lVoluntario = cv.getLV();
        this.setVoluntarios(cv.getVoluntarios());
    }
    
     /** Retorna a lista de voluntarios **/
    public List<Voluntario> getLV(){
        return this.lVoluntario;
    }

    /** Define a lista de voluntarios **/
    public void setLV(List<Voluntario> vl){
        this.lVoluntario = vl;
    }
    /** Método que retorna um map das Voluntarios */
    public Map<String,Voluntario> getVoluntarios(){
        Map<String,Voluntario> lista = new HashMap<>();
        for(Voluntario v : this.voluntarios.values() ){
            lista.put(v.getCodVoluntario(),v);
        }
        return lista;
    }
    
    /** Método que define o map dos Voluntarios */
    public void setVoluntarios(Map<String,Voluntario> map){
        this.voluntarios = new HashMap<>();
        for(Voluntario v : map.values() ){
            this.voluntarios.put(v.getCodVoluntario(),v.clone());
        }
    }
    
    /** Método que clona um ConjVoluntarios */
    public ConjVoluntarios clone(){
        return new ConjVoluntarios(this);
    }
    
    /** Método que devolve um boolean true caso os ConjVoluntarios sejam iguais e false caso não sejam */
    public boolean equals(Object o){
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        ConjVoluntarios cv = (ConjVoluntarios) o;
        return this.voluntarios.equals(cv.getVoluntarios());
    }
    
    /** Método que cria uma string com a informação do ConjVoluntarios */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        for(Voluntario e : this.voluntarios.values()){
            sb.append(e.toString()+"\n");
        }
        return sb.toString();
    }
    
    /** Método que adiciona uma Voluntario */
    public void adicionaVoluntario(Voluntario v){
        this.voluntarios.put(v.getCodVoluntario(),v);
    }

    public boolean addDados (String cod, String nome, double x, double y, double r, String email, String pass) throws IOException {
        Voluntario v =  new Voluntario();
        Localizacao l = new Localizacao();

        v.setRaio(r);
        v.setCodVoluntario(cod);
        v.setNome(nome);
        l.setX(x);
        l.setY(y);
        v.setPosicao(l);
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
                dadosf.put(exemplo,v);
                System.out.printf("Dados guardados com sucesso.\n");
                exemplo.clear();
                escreveCredenciais(v,email,pass);
                escreveVoluntario(v);
                lVoluntario.add(v);
            }
        }
        return true;
    }

    /** Metodo que valida os dados inseridos **/
    public boolean validaDados (String email, String pass){
        Voluntario v = new Voluntario();
        String value;
        if (dados.containsKey(email)){
            value = dados.get(email);
            if (value.equals(pass)){
                exemplo.put(email,pass);
                v = dadosf.get(exemplo);
                exemplo.clear();
                System.out.printf("Inicio de sessão bem sucedido.\n");
                return true;
            }
            else System.out.printf("Dados invalidos.\n");

        }
        else System.out.printf("Dados invalidos.\n");
        return false;
    }

    /** Escreve as credênciais **/
    public void escreveCredenciais (Voluntario nome, String email, String pass) throws IOException {
        String c = nome.getCodVoluntario();
        if (filecredencais.createNewFile() || !filecredencais.createNewFile()){
            try{
                PrintWriter pW = new PrintWriter (new FileWriter(filecredencais,true));
                pW.printf("Voluntario:%s,%s,%s\n",c,email,pass);
                pW.close ();
            }
            catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }
    }

    /** Escreve as informções do voluntario **/
    public void escreveVoluntario (Voluntario nome) throws IOException {
        String n = nome.getNome();
        String c = nome.getCodVoluntario();
        Localizacao l = nome.getPosicao();
        double x = l.getX();
        double y = l.getY();
        double r = nome.getRaio();

        String xf = String.valueOf(x);
        String yf = String.valueOf(y);
        String rf = String.valueOf(r);

        try{
            PrintWriter pW = new PrintWriter (new FileWriter(fileteste,true));
            pW.printf("\nUtilizador:%s,%s,%s,%s,%s",c,n,xf,yf,rf);
            pW.close ();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    /** Le o ficheiro que contêm as informações dos voluntarios **/
    public void ler() {
        Localizacao lc = new Localizacao();
        Voluntario voluntario = new Voluntario();
        try {
            double num = 0.0, num2, num3;
            String e1, e2, ef = null;
            String e = "Voluntario:";
            List<String> l = Files.readAllLines(pteste);
            int i, j, l1, l2, n = 0;
            double peso;
            for (String s : l) {
                if(s.length() > 0) {
                if (s.substring(0, 11).equals(e)) {
                    for (i = 11; i > 0; i++) {
                        if ((e1 = s.substring(i, i + 1)).equals(",")) {
                            ef = s.substring(11, i);
                            voluntario.setCodVoluntario(ef);
                            n = i + 1;
                            break;
                        }
                    }
                    for (j = n; j > 0; j++) {
                        if ((e1 = s.substring(j, j + 1)).equals(",")) {
                            e1 = s.substring(n, j);
                            voluntario.setNome(e1);
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
                    for (l2 = n; l2 > 0; l2++){
                        if ((e2 = s.substring(l2, l2 + 1)).equals(",")){
                            e2 = s.substring(n, l2);
                            num2 = Double.parseDouble(e2);
                            lc.setX(num);
                            lc.setY(num2);
                            voluntario.setPosicao(lc);
                            n = l2 + 1;
                            e2 = s.substring(n);
                            num = Double.parseDouble(e2);
                            voluntario.setRaio(num);
                            break;
                        }
                    }
                }
                lVoluntario.add(voluntario);
            }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /** Lê o ficheiros das credênciais do Voluntario **/
    public List<String> lerCredenciais() {
        List<String> codigos = new ArrayList<String>();
        try {
            String e1, e2, cd;
            String u = "Voluntario:";
            List<String> l = Files.readAllLines(pcredenciais);
            int i, n = 0, c1, c2;
            double num, num2;
            for (String s : l) {
                if (s.substring(0, 11).equals(u)) {
                    for (i = 11; i > 0; i++) {
                        if ((e1 = s.substring(i,i+1)).equals(",")) {
                            cd = s.substring(11,i);
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
    public void AvaliaV (Voluntario v, double a){
        if(avaliacao.containsKey(v)){
            Avaliar av = avaliacao.get(v);
            int q = av.getQuantidade();
            double x = av.getAvaliacao()*q + a;
            av.setQuantidade(q+1);
            av.setAvaliacao(x/(q+1));
            avaliacao.put(v,av);
        }
    }
     /** Função que aceita encomendas **/
    public Voluntario aceitaEncV (List<Voluntario> v, Utilizador u, Encomenda e, List<Loja> lj){
        Localizacao l1 = u.getPosicao();
        int sizeV = v.size();
        int sizeL = lj.size();
        String l = e.getLoja();
        Voluntario vf = null;
        for (int i = 0; i < sizeV; i++){
            Voluntario v2 = v.get(i);
            Localizacao l2 = v2.getPosicao();
            double r = v2.getRaio();
            if ((Math.abs(distancia(l2,l1))) <= r){
                for (int j = 0; j < sizeL; j++){
                    Loja ljs = lj.get(j);
                    String lj1 = ljs.getCodLoja();
                    if (lj1.equals(l)){
                        Localizacao l3 = ljs.getGps();
                        if((Math.abs(distancia(l3,l2))) <= r){
                            relaV.put(v2,u);
                            String p = "S";
                            e.setEncomenda(p);
                            return v2;
                        }
                    }
                }
            }
        }
        return vf;
    }

}











