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
import java.util.Random;
/**
 * Write a description of class ConjEncomendas here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class ConjEncomendas 
{
    /** Conjunto de encomendas identificadas pelo seu respetivo código */
    private Map<String,Encomenda> encomendas;
    private HashMap<String,Encomenda> exemplo = new HashMap<String,Encomenda>();

    /** Conjunto dos códigos de utilizadores identificados pelo seu respetivo código de encomenda.**/
    private HashMap<String, String> encomendasCred = new HashMap<String,String>();
    
    private HashMap<String, String> encomendasCredReverse = new HashMap<String,String>();
    /** Lista de encomendas**/
    private List<Encomenda> lEncomendas = new ArrayList<>();

    /** Ficheiros **/
    private File fileteste = new File ("TestesE.txt");
    private File fileencomendas = new File ("Encomendas.txt");
    private File fileencomendasCred = new File ("EncomendasCred.txt");
    private Path pteste = Paths.get("TestesE.txt");
    private Path pencomendas = Paths.get("Encomendas.txt");
    private Path pencomendasCred = Paths.get("EncomendasCred.txt");

        /** Construtor nulo */
    public ConjEncomendas() throws IOException {
        this.encomendas = new HashMap<>();
        if (!fileteste.createNewFile()) ler();
        if (!fileencomendasCred.createNewFile()) lerCredenciais();
    }
    
    /** Construtor parametrizado para a classe ConjEncomendas */
    public ConjEncomendas(Map<String,Encomenda> map){
        this.encomendas = new HashMap<>();
        for(Encomenda e : map.values()){
            this.encomendas.put(e.getEncomenda(), e.clone());
        }
    }
    
    /** Construtor por cópia */
    public ConjEncomendas(ConjEncomendas ce){
        this.setEncomendas(ce.getEncomendas());
    }
    
    /** Método que retorna um map das encomendas */
    public Map<String,Encomenda> getEncomendas(){
        Map<String,Encomenda> lista = new HashMap<>();
        for(Encomenda e : this.encomendas.values() ){
            lista.put(e.getEncomenda(),e);
        }
        return lista;
    }
    
    /** Método que define as encomendas */
    public void setEncomendas(Map<String,Encomenda> map){
        this.encomendas = new HashMap<>();
        for(Encomenda e : map.values() ){
            this.encomendas.put(e.getEncomenda(),e.clone());
        }
    }
     public Encomenda getEncomendaByco(String cod) {
        Encomenda result = encomendas.get(cod);
        return result;
    }
    
      public Encomenda getUtilizadorBycod(String cod) {
      Encomenda result = new Encomenda();
      for(Encomenda e : lEncomendas) {
        if(e.getUtilizador().equals(cod)) {
            result = e;
        }
        
    }
    return result;
}

    public String getEncomendabyCodU(String codU) {
        String result = encomendasCredReverse.get(codU);
        return result;
    }
    
    /** Método que clona um ConjEncomendas */
    public ConjEncomendas clone(){
        return new ConjEncomendas(this);
    }
    
    /** Método que devolve um boolean true caso os ConjEncomendas sejam iguais e false caso não sejam */
    public boolean equals(Object o){
        if (o==this) return true;
        if (o==null || o.getClass() != this.getClass()) return false;
        ConjEncomendas ce = (ConjEncomendas) o;
        return this.encomendas.equals(ce.getEncomendas());
    }
    
    /** Método que cria uma string com a informação do ConjEncomendas */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        for(Encomenda e : this.encomendas.values()){
            sb.append(e.toString()+"\n");
        }
        return sb.toString();
    }
    
    
    
        /** Adiciona uma encomenda **/
    public boolean addDados (String codE, String codU, String codL, double peso, List<LinhaEncomenda> listaC) throws IOException {
        Encomenda e = new Encomenda();
        e.setEncomenda(codE);
        e.setUtilizador(codU);
        e.setLoja(codL);
        e.setPeso(peso);
        e.setListaCompras(listaC);
        e.setEstado("N");
        if (encomendasCred.containsKey(codE)) {
            System.out.printf("Erro, encomenda já existe.\n");
            return false;
        }
        else{
            if(lerCredenciais().contains(codE)){
                System.out.printf("Erro, encomenda já existe.\n");
                return false;
            }
            else{
                encomendas.put(codE,e);
                encomendasCred.put(codE,codU);
                encomendasCredReverse.put(codU,codE);
                System.out.printf("Dados guardados com sucesso.\n");
                escreveEncomenda(e);
                escreveCredenciais(codE,codU);
                lEncomendas.add(e);
            }
        }
        return true;
    }

    /** Vai encontrar uma encomenda **/
    public Encomenda buscarEncomenda (String codE){
        String value;
        Encomenda result = new Encomenda();
        /*if (encomendasCred.containsKey(codE)){
            e = encomendas.get(codE);
            return e;
        }*/
        for(Encomenda e : lEncomendas) {
            if(e.getEncomenda().equals(codE)){
                result = e;
                Random rand = new Random();
                String c = rand.nextBoolean() ? "aceite" : "pendente";
                result.setEstado(c);
        }
    }
        //else System.out.printf("Encomenda não existe.\n");
        return result;
    }

    /** Escreve o codigo da encomenda e o codigo do utilizador **/
    public void escreveCredenciais (String codE, String codU) throws IOException {
        if (fileencomendasCred.createNewFile() || !fileencomendasCred.createNewFile()){
            try{
                PrintWriter pW = new PrintWriter (new FileWriter(fileencomendasCred,true));
                pW.printf("Dados Encomenda:%s,%s\n",codU,codE);
                pW.close ();
            }
            catch (FileNotFoundException e) {
                e.printStackTrace();
            }
        }
    }

    /** Escreve a encomenda **/
    public void escreveEncomenda (Encomenda enc) throws IOException {
        String cU = enc.getUtilizador();
        String cE = enc.getEncomenda();
        String cL = enc.getLoja();
        double p = enc.getPeso();
        List<LinhaEncomenda> lE = enc.getListaCompras();

        String peso = String.valueOf(p);

        try{
            PrintWriter pW = new PrintWriter (new FileWriter(fileteste,true));
            pW.printf("Encomenda:%s,%s,%s,%s,%s\n",cE,cU,cL,peso,lE);
            pW.close ();
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    /** Cria uma lista de encomendas a partir de uma String **/
    public List<LinhaEncomenda> addListaEnc(String lista,List<LinhaEncomenda> LE) {
        
        boolean virgula = false;
        int contador = 0;
        int oldContador = 0;
        String e1, e2;
        int n = 0, s = 0, size;
        double peso, qtd, preco;
        size = lista.length();
        while (contador < size) {
             LinhaEncomenda le = new LinhaEncomenda();
            /** Cada parte do produto **/
            for (int i = 0; i < 4; i++) {
               
                System.out.printf("Z %d\n",i);
                while (contador < size) {
                    if ((e1 = lista.substring(contador, contador + 1)).equals(",")) {
                        e1 = lista.substring(oldContador, contador);
                        switch (i) {
                            case 0:
                                le.setCodProduto(e1);
                                break;
                            case 1:
                                le.setNomeProduto(e1);
                                break;
                            case 2:
                                qtd = Double.parseDouble(e1);
                                le.setQtdProduto(qtd);
                                break;
                            case 3:
                                preco = Double.parseDouble(e1);
                                le.setPrecoProduto(preco);
                                break;
                            default:
                                break;
                        }
                        oldContador = contador + 1;
                        contador++;
                        break;
                    }
                    contador++;
                }
            }
            LE.add(le);
        }
        return LE;
    }

    /** Le um ficheiro de encomendas **/
    public void ler() {
     
        try {
            String e1, e2, ef = null;
            String e = "Encomenda:";
            List<String> l = Files.readAllLines(pteste);
            int i, j, l1, l2, n = 0;
            double peso;
            for (String s : l) {
                Encomenda enc = new Encomenda();
                List<LinhaEncomenda> lE = new ArrayList<>();
                List<LinhaEncomenda> lista = new ArrayList<>();
                if(s.length() > 0){
                if (s.substring(0, 10).equals(e)) {
                    for (i = 10; i > 0; i++) {
                        if ((e1 = s.substring(i, i + 1)).equals(",")) {
                            ef = s.substring(10, i);
                            enc.setEncomenda(ef);
                            n = i + 1;
                            break;
                        }
                    }
                    for (j = n; j > 0; j++) {
                        if ((e1 = s.substring(j, j + 1)).equals(",")) {
                            e1 = s.substring(n, j);
                            enc.setUtilizador(e1);
                            n = j + 1;
                            break;
                        }
                    }
                    for (l1 = n; l1 > 0; l1++) {
                        if ((e1 = s.substring(l1, l1 + 1)).equals(",")) {
                            e1 = s.substring(n, l1);
                            enc.setLoja(e1);
                            n = l1 + 1;
                            break;
                        }
                    }
                    for (l2 = n; l2 > 0; l2++){
                        if ((e1 = s.substring(l2, l2 + 1)).equals(",")){
                            e1 = s.substring(n, l2);
                            peso = Double.parseDouble(e1);
                            enc.setPeso(peso);
                            n = l2 + 1;
                            e2 = s.substring(n);
                            lE = addListaEnc(e2,lista);
                            enc.setListaCompras(lE);
                            break;
                        }
                    }
                }
                encomendas.put(ef,enc);
                lEncomendas.add(enc);
            }
        }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    /** Le o ficheiro que contem os codigos de encomenda e de utilizador **/
    public List<String> lerCredenciais() {
        List<String> codigos = new ArrayList<String>();
        try {
            String e1, e2;
            String u = "Dados Encomenda:";
            List<String> l = Files.readAllLines(pencomendasCred);
            int i, n = 0, c1, c2;
            double num, num2;
            for (String s : l) {
                if (s.substring(0, 16).equals(u)) {
                    for (i = 16; i > 0; i++) {
                        if ((e1 = s.substring(i,i+1)).equals(",")) {
                            e1 = s.substring(16,i);
                            codigos.add(e1);
                            n = i+1;
                            e2 = s.substring(n);
                            encomendasCred.put(e1,e2);
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

    /** Retorna a lista dos 10 utilizadores que mais encomendas fizeram **/
    public List<Utilizador> maisUsaram(List<Utilizador> ut, HashMap<String,String> lk) {
        List<Utilizador> u = new ArrayList<>();
        List<String> utl = new ArrayList<>();
        List<Integer> numb = new ArrayList<>();
        int count = 0;
        int size = ut.size();
        String t;
        if (size <= 10) return ut;
        else {
            for (int i = 0; i < 10;i++) {
                t = ut.get(i).getCodUtilizador();
                if (lk.containsValue(t)) {
                    count = Collections.frequency(lk.values(), t);
                    utl.add(t);
                    numb.add(count);
                    u.add(ut.get(i));
                }
            }
            for (int j = 9; j < size; j++){
                t = ut.get(j).getCodUtilizador();
                if (lk.containsValue(t)){
                    count = Collections.frequency(lk.values(), t);
                    for (int k = 0; k < 10; k++){
                        if (count > numb.get(k)){
                            numb.remove(k);
                            numb.add(count);
                            utl.remove(k);
                            utl.add(t);
                            u.remove(k);
                            u.add(ut.get(j));
                            break;
                        }
                    }
                }
            }
        }
        return u;
    }
       /** Método para gerar o código aleatório para o aluguer */
    
    private static final String ALPHABET = "0123456789";
    private static final SecureRandom RANDOM = new SecureRandom();

    public String geraCodigo( String u) {
        StringBuilder sb = new StringBuilder();
        sb.append(u);
        for (int i = 0; i < 3; ++i) {
            sb.append(ALPHABET.charAt(RANDOM.nextInt(ALPHABET.length())));
        }
        return sb.toString();
    }
}

