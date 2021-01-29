

/**
 * Write a description of class Main here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

import java.util.Arrays;
import java.util.Scanner;
import static java.lang.System.out;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.stream.Collectors;
import java.util.Comparator;
import java.lang.reflect.*;

import java.io.PrintWriter;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.Serializable;
import java.nio.file.Paths;
import java.nio.file.Files;
import java.nio.charset.StandardCharsets;
import java.io.Serializable;

//Classe Principal
public class Model implements Serializable
{
    private  BaseDeDados dados;
    private  ArrayList<String> queue;
    private  ArrayList<String> disponiveis;   //Transportadores disponiveis
    private  Map <String, ArrayList<String>> pendentes;  //cod utilizador -> par (codEncomenda, preco);

    private  int cod;   //códigos a serem atribuidos aos novos utilizadores

    public Model()
    {
        this.dados = new BaseDeDados();
        this.queue = new ArrayList<String>();
        this.disponiveis = new ArrayList<String>();
        this.pendentes = new HashMap <String, ArrayList<String>>();  //cod utilzador -> par (codEncomenda, preco);

        this.cod = 1;
    }

    //Métodos

    //Pendentes
    public boolean inPendentes(Utilizador u){
        return pendentes.keySet().stream().anyMatch(key -> key.equals(u.getCod()));
    }
    public boolean inPendentes(String cod){
        return pendentes.keySet().stream().anyMatch(key -> key.equals(cod));
    }

    public void addPendentes(String dest, String enc){
        if (inPendentes(dest)){
            pendentes.get(dest).add(enc);
        }
        else{
            ArrayList novo = new ArrayList<String> ();
            novo.add(enc);
            pendentes.put(dest, novo);
        }
    }

    public ArrayList<String> getPendentes(String cod){
        return this.pendentes.get(cod);
    }

    public Map getPendentes(){
        return this.pendentes;
    }
    
    public void removePendentes(String cod){
        this.pendentes.remove(cod);
    }

    //Queue
    public boolean inQueue(String cod){
        return this.queue.contains(cod);
    }
    
    public ArrayList<String> getQueue(){
        return this.queue;
    }

    public int getQueueSize(){
        return this.queue.size();
    }

    public String getQueue(int i){
        return this.queue.get(i);
    }

    public void removeQueue(String codEnc){
        this.queue.remove(codEnc);
    }

    public  void toQueue(String cod){
        queue.add(cod);
    }

    //Disponiveis
    public  boolean inDisponiveis(String cod){
        return disponiveis.stream().anyMatch(c -> c.equals(cod));
    }

    public  int addDisponivel(String cod){
        if (!inDisponiveis(cod)){
            disponiveis.add(cod);
        }
        return 0;
    }

    public ArrayList<String>  getDisponiveis(){
        return this.disponiveis;
    }

    //Encomenda
    public Encomenda getEncomenda(String codEnc) throws ExceptionEncomendaNaoEncontrada{
        
        String loja = dados.qualLoja(codEnc);
        if (loja == null){
            throw new ExceptionEncomendaNaoEncontrada ("Loja nao encontrada");
        }
        Encomenda res = null;
        res = dados.getLoja(loja).getEncomenda(codEnc);

        if (res == null){
            throw new ExceptionEncomendaNaoEncontrada ("Encomenda pode nao existir ou ainda nao estar pronta");
        }
        else{
            return res;
        }
    }

    public String addEncomenda(String loja, Encomenda enc){
        dados.getLoja(loja).add(enc);
        dados.addQualLoja(enc.getCod(),loja);
        return enc.getCod();
    }

    public  boolean inLoja(String cod){
        if (dados.qualLoja(cod) == null){
            return false;
        }
        String nomeLoja = dados.qualLoja(cod);

        Loja loja = dados.getLoja(nomeLoja);

        return loja.inLoja(cod);
    }

    //Getters
    public Transportador getTransportador(String cod)
    {
        return this.dados.getTransportador(cod);
    }

    public Utilizador getUtilizador(String cod){
        return this.dados.getUtilizador(cod);
    }

    public Loja getLoja(String cod){
        return this.dados.getLoja(cod);
    }

    public BaseDeDados getDados(){
        return this.dados;
    }

    public int getCod(){
        return this.cod;
    }
    
    public String getLojaProdutos(String codLoja){
        String res = "";
        int t=0;
        Loja loja = this.dados.getLoja(codLoja);
        try{
            for(LinhaEncomenda linha : loja.getLinhas()){
                if (t==0){
                    res += linha.getDescricao();
                    t=1;
                }
                else{
                    res+="\n";
                    res += linha.getDescricao();
                }
            }
        }
        catch(NullPointerException e){
            out.println(e.getMessage());
        }
        
        return res;
    }
    
    public String getLojasString(){
        String res="";
        int t=0;
        for(String codLoja : dados.getNomeLoja().keySet()){
          if (t==0){
              res += this.dados.getLoja(codLoja).getNome() + ": " + codLoja;
              t=1;
            }
            else{
              res+="\n";
              res += this.dados.getLoja(codLoja).getNome() + ": " + codLoja;
            }
        }
        return res;
    }

    //File
    public void gravar(String nome) throws IOException{
        ObjectOutputStream oot = new ObjectOutputStream (new FileOutputStream(nome));

        oot.writeObject(this);
        oot.flush();
        oot.close();
    }

    public void carregar(String nome) throws IOException, ClassNotFoundException{
        ObjectInputStream oit = new ObjectInputStream (new FileInputStream(nome));

        Model res = (Model) oit.readObject();

        this.dados = res.getDados();
        this.queue = res.getQueue();
        this.disponiveis = res.getDisponiveis();
        this.pendentes = res.getPendentes();  //nome utilzador -> par (codEncomenda, preco);

        this.cod = res.getCod();

        oit.close();
    }

    public void carregarLogs(String fileName)  throws IOException, ClassNotFoundException, ExceptionEncomendaNaoEncontrada{
        List<String> linhas = lerFicheiro(fileName);
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                case "Utilizador":
                        Utilizador u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                        System.out.println(u.toString()); //enviar para o ecrÃ¡n apenas para teste
                        break;
                case "Loja":
                        Loja l = parseLoja(linhaPartida[1]);
                        System.out.println(l.toString());
                        break;
                case "Voluntario":
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        System.out.println(v.toString());
                        break;
                case "Transportadora":
                        Empresa e = parseEmpresa(linhaPartida[1]);
                        System.out.println(e.toString());
                        break;
                case "Encomenda":
                        Encomenda enc = parseEncomenda(linhaPartida[1]);
                        System.out.println(enc.toString());
                        break;
                case "Aceite":
                        Encomenda encLog = getEncomenda(linhaPartida[1]);
                        dados.addEntregue(encLog);
                        System.out.println(encLog.toString());
                        break;
                default:
                        System.out.println("Linha invÃ¡lida.");
                        break;
                }

        }
        //System.out.println("done!");
    }

    public List<String> lerFicheiro(String nomeFich) {
          List<String> lines = new ArrayList<>();
          try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
          catch(IOException exc) { System.out.println(exc.getMessage()); }
          return lines;
    }

    public Utilizador parseUtilizador(String input){
          String[] campos = input.split(",");
          String nome = campos[0];
          String codUtilizador = campos[1];
          GPS gps = new GPS (Double.parseDouble(campos[2]), Double.parseDouble(campos[3]));
          Utilizador u = new Utilizador(codUtilizador,nome,gps);
          dados.addUtilizador(u);
          return u;
    }

    public Loja parseLoja(String input){
          String[] campos = input.split(",");
          String codLoja = campos[0];
          String nomeLoja = campos[1];
          GPS gps = new GPS (Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
          Loja l = new Loja(codLoja,nomeLoja,gps);
          dados.addLoja(l);
          return l;
    }

    public Voluntario parseVoluntario(String input){
          String[] campos = input.split(",");
          String codVoluntario = campos[0];
          String nome = campos[1];
          GPS gps = new GPS (Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
          double raio = Double.parseDouble(campos[4]);
          Voluntario v = new Voluntario(codVoluntario,nome,gps,raio);
          dados.addVoluntario(v);
          return v;
    }

    public Empresa parseEmpresa(String input){
          String[] campos = input.split(",");
          String codEmpresa = campos[0];
          String nome = campos[1];
          GPS gps = new GPS (Double.parseDouble(campos[2]),Double.parseDouble(campos[3]));
          String nif = campos[4];
          double raio = Double.parseDouble(campos[5]);
          double preco = Double.parseDouble(campos[6]);
          Empresa e = new Empresa(codEmpresa,nome,gps,raio,nif,preco);
          dados.addEmpresa(e);
          return e;
    }

    public Encomenda parseEncomenda(String input){
          String[] campos = input.split(",");
          String codEncomenda = campos[0];
          String codUtilizador = campos[1];
          String codLoja = campos[2];
          double peso = Double.parseDouble(campos[3]);

          Encomenda enc = new Encomenda(codEncomenda,codUtilizador,codLoja,peso);

          addEncomenda(codLoja,enc);          

          int p = 4;
          while(p<campos.length){
              String codProduto = campos[p];
              String descricao = campos[p+1];
              double quantidade = Double.parseDouble(campos[p+2]);
              double preco = Double.parseDouble(campos[p+3]);
              p=p+4;
              LinhaEncomenda e = new LinhaEncomenda(codProduto,descricao,quantidade,preco);
              enc.adicionaLinha(e);
          }
          return enc;
    }

    //Conta

    public  Object login (Scanner scanner, Pair loginInfo){
        String tipo = dados.getTipo(loginInfo);
        String cod = dados.getCod(loginInfo);

        if (tipo.equals("utilizador") || tipo.equals("user")){
            return dados.getUtilizador(cod);
        }
        else if (tipo.equals("voluntario") || tipo.equals("empresa") || tipo.equals("transportadora")){
            return dados.getTransportador(cod);
        }
        else if (tipo.equals("loja")){
            return dados.getLoja(cod);
        }
        else{
            return null; //Este caso não é suposto acontecer
        }

    }

    public  boolean usernameExistente(String nome){
        return dados.getLogin().keySet().stream().anyMatch(key -> key.p1().equals(nome));
    }

    public int criarUtilizador(Pair username_senha, String nome, GPS local){
        String cod = Integer.toString(this.cod);
        this.cod++;

        dados.addUtilizador(new Utilizador(cod,nome,local));
        int r = dados.addLogin(username_senha, new Pair("utilizador", cod));
        return r;
    }

    public int criarVoluntario(Pair username_senha, String nome, GPS local, double raio){
        String cod = Integer.toString(this.cod);
        this.cod++;

        dados.addVoluntario(new Voluntario(cod,nome,local,raio));
        int r = dados.addLogin(username_senha, new Pair("voluntario", cod));
        return r;
    }

    public int criarEmpresa(Pair username_senha, String nome, GPS local, double raio, String nif, double preco){
        String cod = Integer.toString(this.cod);
        this.cod++;

        dados.addEmpresa (new Empresa(cod,nome,local,raio, nif, preco));
        int r = dados.addLogin(username_senha, new Pair("empresa", cod));
        return r;
    }

    public int criarLoja(Pair username_senha, String nome, GPS local){
        String cod = Integer.toString(this.cod);
        this.cod++;

        dados.addLoja (new Loja(cod,nome,local));
        int r = dados.addLogin(username_senha, new Pair("loja", cod));
        return r;
    }

    //Loja
    public int addProdutoDisponivel(String codLoja, String ref, String descricao, double preco){
        LinhaEncomenda novoProduto = new LinhaEncomenda(ref, descricao, 1, preco);
        
        return this.dados.adicionarProdutoDisponivel(codLoja, novoProduto);
    }

    public boolean produtoDisponivel(String codLoja, String nomeProduto){
        LinhaEncomenda linha = this.getLoja(codLoja).getLinha(nomeProduto);
        if (linha == null){
            return false;
        }
        else{
            return true;
        }
    }
    
    public String criarEncomenda(double peso, String nif, ArrayList <Pair> produtos, String codUtilizador, String codLoja){
        ArrayList <LinhaEncomenda> linhas = new ArrayList <LinhaEncomenda>();
        
        produtos.stream().forEach(pair -> linhas.add(new LinhaEncomenda(this.dados.getLoja(codLoja).getLinha(pair.p1()), Double.parseDouble(pair.p2()))));
        
        return this.addEncomenda(codLoja, new Encomenda(peso, Integer.parseInt(nif), codUtilizador, codLoja, linhas));
        
    }
    
    public String getTopTenUtil(){
        String res = "";
        int num = 0;
        Object []top = this.dados.getUtilizadores().values().stream().sorted(new ComparatorUtil()).limit(10).toArray();
        for (Object u: top){
            Utilizador b = (Utilizador) u;
            res += num + ": " + b.getNome() + "\n"; 
            num += 1;
        }
        return res;
    }
}
