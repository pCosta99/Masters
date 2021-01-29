import java.util.ArrayList;
import java.util.List;
import java.io.File;
import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.lang.Object;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Iterator;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Scanner;
import java.util.Random;
import java.util.TreeSet;
import java.util.Map;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.LinkedHashMap;


public class Parse {
    private List<Queue> teste = new ArrayList<>() ;
    private List<Usuário> users= new ArrayList<Usuário>();
    private Pedido p= new Pedido() ;
    private List<Loja> lo = new ArrayList<Loja>();
    private List<Voluntario> vol = new ArrayList<Voluntario>();
    private List<Empresa> emp = new ArrayList<Empresa>() ;
    private List<Pedido> pe= new ArrayList<Pedido>() ; 
    private Map<Double,Voluntario> pai = new HashMap<>();
    private Map<Double,Voluntario> filho = new HashMap<>();
    
  public void parse() throws IOException{
        List<String> linhas = lerFicheiro("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt");//C:\\Users\\Rui\\Desktop\\teste2.txt
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                case "Utilizador": 
                        Usuário u = parseUtilizador(linhaPartida[1]); // criar um Utilizador
                        users.add(u);
                        break;
                case "Loja": 
                        Loja l = parseLoja(linhaPartida[1]);
                        lo.add(l);
                        break;                                   
                //...
                case "Voluntario":
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        vol.add(v);
                        break;
                case "Voluntario Med": 
                        Voluntario q = parseVoluntariomed(linhaPartida[1]);
                        vol.add(q);
                        break;
                case "Transportadora":
                        Empresa e = parseEmpresa(linhaPartida[1]);
                        emp.add(e);
                        break;
                case "Encomenda": 
                       p = parsePedido(linhaPartida[1]);
                       pe.add(p);
                       break;
                case "Aceite" :
                       Accepted pedido = parseAceite(linhaPartida[1]);
                       break;
                default:
                        break;
                }

        }
       
  }
  
   public Usuário parseUtilizador(String input){
        String[] campos = input.split(",");
        String nome = campos[0]; 
        String codigoUsuario = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas e = new Coordenadas(gpsx,gpsy);
        String password = campos[4];
        return new Usuário(nome,codigoUsuario,e,password);
  }
      
  public Accepted parseAceite(String input){
      String [] campos = input.split(",");
      String cod = campos[0];
      List<String> Accepted = new ArrayList<>();
      Accepted.add(cod);
      return new Accepted(Accepted);
  }
  
  public List<Queue> parseteste(String[] input) throws IOException{
      int i,j,k,l;
      List<Queue> ret = new ArrayList<>();
      
      for (i = 0; i < input.length; i += 4) // anda de 4 em 4
      { 
          String codprod = input[i];
          String desc = input[i+1];
          double quant = Double.parseDouble(input[i+2]);
          double preco = Double.parseDouble(input[i+3]);
          Queue pedido = new Queue(codprod, desc, quant, preco);
          teste.add(pedido);
          guardaprodutos(pedido);
          ret.add(pedido);
        }
        
      return ret;
  }
  
  public Pedido parsePedido(String input) throws IOException{
        List<Queue> pedidos = new ArrayList<>();
        String[] campos = input.split(","); 
        String cod = campos[0]; //codigo da encomenda
        String user = campos[1]; // codigo do utilizador
        String loja = campos[2]; // codigo da loja
        double peso = Double.parseDouble(campos[3]); 
        String[] restOfFields = Arrays.copyOfRange(campos,4,campos.length);
        pedidos = parseteste(restOfFields);
        return new Pedido(cod,user,loja,peso,pedidos);
  }
  

  public Empresa parseEmpresa(String input){
      String[] campos = input.split(",");
      String cod = campos[0];
      String nome = campos[1];
      double gpsx = Double.parseDouble(campos[2]);
      double gpsy = Double.parseDouble(campos[3]);
      int nif = Integer.parseInt(campos[4]);
      double raio = Double.parseDouble(campos[5]);
      double preco = Double.parseDouble(campos[6]);
      String password = campos[7];
      Coordenadas e = new Coordenadas(gpsx, gpsy);
      return new Empresa(cod,nome,e,nif,raio,preco,password);
  }
  
  
  public Voluntario parseVoluntariomed(String input){
        String[] campos = input.split(",");
        String codVol = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        String password = campos[5];
        Coordenadas e = new Coordenadas(gpsx,gpsy);
        return new Voluntario(codVol,nome,e,raio,true,password);
  }

  
  public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVol = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        String password = campos[5];
        Coordenadas e = new Coordenadas(gpsx,gpsy);
        return new Voluntario(codVol,nome,e,raio,password);
  }

  public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String codigoLoja = campos[0]; 
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas e = new Coordenadas(gpsx,gpsy);
        String password = campos[4];
        return new Loja(codigoLoja,nomeLoja,e,password);
  }

  public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
  }
  
  
  
  
  public void empresas(){            //imprime as empresas
  for(Empresa e : emp){
  System.out.println(e);    
  }
  }
 
  public void voluntarios(){      //imprime voluntários
  for(Voluntario e : vol){
  System.out.println(e);    
  }
  }
  
  //avaliar
  
  public void avaliaEmpresa(Empresa e, int av){
  e.addAvaliacao(av);    
  }
  
  public void avaliaLoja(Loja e, int av){          
  e.addAvaliacao(av);    
  }
  
  public void avaliaVoluntario(Voluntario e, int av){
  e.addAvaliacao(av);    
  }
  //tempos
  public Long tempoVol (Usuário e , Voluntario v)throws InterruptedException {    // calcula a distancia
  Coordenadas a = e.getgps(); 
  Coordenadas b = v.getgps(); 
  double dist = v.distancia2pontos(b,a)*1500;
  long tempo = Math.round(dist);
  return tempo; 
  }
  
  public Long tempoEmp(Usuário e , Empresa v)throws InterruptedException {    // calcula o tempo demorado mas da empresa que é mais rapida
  Coordenadas a = e.getgps(); 
  Coordenadas b = v.getgps(); 
  double dist = v.distancia2pontos(b,a)*500;
  long tempo = Math.round(dist);
  System.out.println(tempo); 
  return tempo; 
  }
  
  
  public void setdispvol (Voluntario e){          // colocar voluntario disponivel
  e.setdisponibilidade(true);    
  }
  
  public void setdispemp (Empresa e){            // colocar empresa disponivel
  e.setdisponibilidade(true);    
  }
  
  public Empresa selecionaempresa (String cod)throws IOException{       // escolhe empresa mais perto
  Empresa v = identificaEmpresa(cod); 
  ArrayList<Empresa> y = new ArrayList<>();
  Map<Double,Empresa> ret = new HashMap<>();
  Coordenadas a = v.getgps();
  
  for(Empresa n : emp) {
  Coordenadas hm = n.getgps();
  Double dista = v.distancia2pontos (hm,a);
  if(n.getraio()>=dista && n.disponivel()){
  ret.put(dista,n);
  }
  }
  for(Map.Entry<Double,Empresa> q : ret.entrySet()){
  ret= 
  ret.entrySet().stream()  
  .sorted(Entry.comparingByKey())             //organizar por ordem crescente de distância
  .collect(Collectors.toMap(Entry::getKey, Entry::getValue,
                            (e1, e2) -> e1, LinkedHashMap::new));
  y.add(q.getValue());
  
}
  int quant = 0; 
  for(Empresa e : y){
  quant+=1;     
  }
  if(quant == 0) return null; 
  Empresa s = new Empresa(y.get(0));
  s.setdisponibilidade(false); 
  return s;
}
  
  public Voluntario primeiro (String cod)throws IOException{          // escolhe voluntario mais proximo
  Loja v = identifica(cod); 
  ArrayList<Voluntario> y = new ArrayList<>();
  Map<Double,Voluntario> ret = new HashMap<>();
  Coordenadas a = v.getgps();
  
  for(Voluntario n : vol) {
  Coordenadas hm = n.getgps();
  Double dista = v.distancia2pontos (hm,a);
  if(n.getraio()>=dista && n.disponivel()){
  ret.put(dista,n);
  }
  }
  for(Map.Entry<Double,Voluntario> q : ret.entrySet()){
  ret= 
  ret.entrySet().stream()  
  .sorted(Entry.comparingByKey())              //organizar por ordem crescente de distância
  .collect(Collectors.toMap(Entry::getKey, Entry::getValue,
                            (e1, e2) -> e1, LinkedHashMap::new));
  y.add(q.getValue());
  
}
  int quant = 0; 
  for(Voluntario e : y){
  quant+=1;     
  }
  if(quant == 0) return null; 
  Voluntario s = new Voluntario(y.get(0));
  s.setdisponibilidade(false); 
  return s;
}
  
  public boolean wait(boolean e,String codigoLoja){      // espera que a encomenda esteja pronta
  for(Loja c : lo){
  if(c.getcodigoLoja().equals(codigoLoja))
  return c.estado(e);
  }
  return false;    
  }
  
    
  public double calculaprecoemp(Pedido e,Empresa x){          //calcula o preço
  return e.calculapreco()+x.getcustotransporte(); 
  }
  
  public double calculapreco(Pedido e){
  return e.calculapreco(); 
  }
  
  public List<Double> distancias(Loja e) throws IOException{     // distancia que o voluntario que está da loja
  Parse p = new Parse(); 
  p.parse(); 
  List<Double> ret = new ArrayList<>();
  Coordenadas loja = e.getgps();
  for(Voluntario x : vol){
  Coordenadas vol = x.getgps();
  Double dist = x.distancia2pontos (vol,loja);
  ret.add(dist);
  }
  return ret; 
  }
  
  public String codigoLojanome(String nome){   //return do codigo da loja atraves do nome
  
  for(Loja e : lo){
  if(nome.equals(e.getnome())) return e.getcodigoLoja();    
  }
  return null;   
  }
  
  public String coduserlogin(String nome, String password){       //fazer login
  String cod = new String();     
  
  for(Usuário e : users){
  if(e.getnome().equals(nome) && e.getpassword().equals(password)) return e.getcodigoUsuario();    
  }
  
  return cod; 
  }

 public String geracoduser(){          //gera codigo usuario
 StringBuilder sb = new StringBuilder();
 Random random = new Random();
 int num = random.nextInt(100);
 String ret = new String();
 sb.append("u").append(String.valueOf(num)); 
 ret=sb.toString(); 
 
 for(Usuário e : users){             //se existir
 if(e.getcodigoUsuario().equals(ret)) geracoduser();
 }
 return ret ;      
 }
 
 public String geracodvol(){               //gera codigo voluntario
 StringBuilder sb = new StringBuilder();
 Random random = new Random();
 int num = random.nextInt(100);
 String ret = new String();
 sb.append("v").append(String.valueOf(num)); 
 ret=sb.toString(); 
 
 for(Voluntario e : vol){                              //se existir
 if(e.getcodigoVoluntario().equals(ret)) geracodvol();
 }
 return ret ;      
 }
 
 public String geracodempresa(){                
 StringBuilder sb = new StringBuilder();
 Random random = new Random();
 int num = random.nextInt(100);
 String ret = new String();
 sb.append("t").append(String.valueOf(num)); 
 ret=sb.toString(); 
 
 for(Empresa e : emp){
 if(e.getcodigoEmpresa().equals(ret)) geracodempresa();
 }
 return ret ;      
 }
 
 public String geracodigoLoja(){
 StringBuilder sb = new StringBuilder();
 Random random = new Random();
 int num = random.nextInt(100);
 String ret = new String();
 sb.append("l").append(String.valueOf(num)); 
 ret=sb.toString(); 
 
 for(Loja e : lo){
 if(e.getcodigoLoja().equals(ret)) geracodigoLoja();
 }
 return ret ;      
 }
 
 public String geracodenc(){                          //gera código para uma encomenda
 StringBuilder sb = new StringBuilder();
 Random random = new Random();
 int num = random.nextInt(100);
 String ret = new String();
 sb.append("e").append(String.valueOf(num)); 
 ret=sb.toString(); 
 
 for(Pedido e : pe){
 if(e.getcodigoEncomenda().equals(ret)) geracodenc();
 }
 return ret ;      
 }
 
 public String geracodprod()throws IOException{
 StringBuilder sb = new StringBuilder();
 Random random = new Random();
 int num = random.nextInt(100);
 String ret = new String();
 sb.append("p").append(String.valueOf(num)); 
 ret=sb.toString(); 
 
 for(Queue e : teste){
 if(e.getcodigoProduto().equals(ret)) geracodprod();
 }
 return ret ;      
     
 }
 
 public double gerapreco(String nome)throws IOException{
 parse();
 for(Queue e : teste){
 if(e.getdesc().equals(nome)) 
 return e.getvalorPreco(); 
 }    
 
 Random random = new Random();
 Double preco = random.nextDouble();
 return preco;    
 }
 
 public double gerapeso(String codenc)throws IOException{
 parse();
 for(Pedido e : pe){
 if(e.getcodigoEncomenda().equals(codenc)) 
 return e.getPeso(); 
 }
 Random random = new Random();
 Double peso = random.nextDouble();
 return peso;    
 }
 
 public Queue criapedido(String codprod,String nome,int quantidade)throws IOException{
 double preco = gerapreco(nome);
 return new Queue(codprod,nome,quantidade,preco); 
 }
 
 
 
 public void alerta(Loja e)throws InterruptedException{    //alerta quando encomenda está pronta
 for(Loja a : lo){
 if(a.getcodigoLoja().equals(e.getcodigoLoja())) 
 e.encomendaReady();
 }
 }
  
 public Usuário identificaUsuario (String e){   //através do codigo dá-nos o usuario
 Usuário ret = new Usuário();
 for(Usuário a: users){
 if(e.equals(a.getcodigoUsuario())) {
 String nome = a.getnome(); 
 String password = a.getpassword();
 Coordenadas c = a.getgps(); 
 ret = new Usuário(e,nome,c,password);
 }
 } 
 return ret;   
 }
 
 public Voluntario identificaVoluntarionome (String e){       //através do nome dá-nos o voluntário
 Voluntario ret = new Voluntario();
 for(Voluntario a: vol){
 if(e.equals(a.getnome())) {
 String nome = a.getcodigoVoluntario(); 
 String password = a.getpassword();
 Coordenadas c = a.getgps(); 
 double raio = a.getraio();
 ret = new Voluntario(nome,e,c,raio,password);
 }
 } 
 return ret;   
 }
 
 public Voluntario identificaVoluntario (String e){     
 Voluntario ret = new Voluntario();
 for(Voluntario a: vol){
 if(e.equals(a.getcodigoVoluntario())) {
 String nome = a.getnome(); 
 String password = a.getpassword();
 Coordenadas c = a.getgps(); 
 double raio = a.getraio();
 ret = new Voluntario(e,nome,c,raio,password);
 }
 } 
 return ret;   
 }
 
 public Loja identifica (String e){ //através do nome da loja dá as restantes informações
 Loja ret = new Loja();
 for(Loja a: lo){
 if(e.equals(a.getnome())) {
 String cod = a.getcodigoLoja(); 
 String password = a.getpassword();
 Coordenadas c = a.getgps(); 
 ret = new Loja(cod,e,c,password);
 }
 } 
 return ret;   
 }
 
 public Empresa identificaEmpresa (String e){
 Empresa ret = new Empresa();
 for(Empresa a: emp){
 if(e.equals(a.getnome())) {
 String cod = a.getcodigoEmpresa(); 
 String password = a.getpassword();
 Coordenadas c = a.getgps(); 
 int nif = a.getnif();
 double raio = a.getraio();
 double custo = a.getcustotransporte();
 ret = new Empresa(cod,e,c,nif,raio,custo,password);
 }
 } 
 return ret;   
 }
 
 public Loja identificacod (String e){
 Loja ret = new Loja();
 for(Loja a: lo){
 if(e.equals(a.getcodigoLoja())) {
 String cod = a.getnome(); 
 String password = a.getpassword();
 Coordenadas c = a.getgps(); 
 ret = new Loja(e,cod,c,password);
 }
 } 
 return ret;   
 }
  
 
 public String desctocod(String desc)throws IOException{   //return codigo produto atraves da descrição
 parse();
 String codigoProduto = new String (); 
 for(Queue e : teste){
 codigoProduto = e.nameToCode(desc);
 }
  return codigoProduto;
 }
 
 public boolean existeprod(String desc) throws IOException{  //verificar se existe produto
 for(Queue e : teste){
 if(e.getdesc().equals(desc)) return true;    
 }
 return false;
 }
  
 public Pedido criapedido2 (List<Queue> u, String coduser, String codigoLoja)throws IOException{  //criar um pedido
 String cod = geracodenc();   
 double peso = gerapeso(cod);
 Pedido c = new Pedido(cod,coduser,codigoLoja,peso,u);
 return c; 
 }
 
 public void criapedido (List<Queue> u, String coduser, String codigoLoja)throws IOException{  //guardar encomendas no bloco de notas
 String cod = geracodenc();   
 double peso = gerapeso(cod);
 Pedido c = new Pedido(cod,coduser,codigoLoja,peso,u);
 guardencomenda(c);   
 }
  
public void alerta(String e)throws InterruptedException,IOException{  // alertar loja que vai receber encomenda 
parse();
alerta(identifica(e)); 
}
 
  
public void adduser(String nome, Coordenadas a,String password) throws IOException{  //criar usuario e guarda bloco de notas
String cod = geracoduser();
Usuário e = new Usuário(cod,nome,a,password); 
guardauser(e);
users.add(e);
}  
  
  public void addloja(String nome, Coordenadas a,String password) throws IOException{  //criar loja e guarda bloco de notas
  String cod = geracodigoLoja();
  Loja e = new Loja(cod,nome,a,password); 
  guardaloja(e);
  lo.add(e);
  }
  
  public void addtrans(String nome, Coordenadas a,int nif,double raio,double custo,String password) throws IOException{  //criar empresa e guarda bloco de notas
  String cod = geracodempresa();
  Empresa e = new Empresa(cod,nome,a,nif,raio,custo,password); 
  guardaemp(e);
  emp.add(e);
  }

  public void addvolmed(String nome, Coordenadas a,double raio,String password) throws IOException{  //criar voluntario que aceita medicação e guarda bloco de notas
  String cod = geracodvol();
  Voluntario e = new Voluntario(cod,nome,a,raio,true,password); 
  guardavolmed(e);
  vol.add(e);
  }

  
  public void addvol(String nome, Coordenadas a,double raio,String password) throws IOException{   //criar voluntário que nao aceita medicamentos e guarda bloco de notas
  String cod = geracodvol();
  Voluntario e = new Voluntario(cod,nome,a,raio,password); 
  guardavol(e);
  vol.add(e);
  }

  public String listaprodutos(){    // return todos os produtos que já existem
  return p.produtos(); 
  }
  
public boolean login(String nome, String password){                //login
for(Usuário e : users){
if(e.getnome().equals(nome) && e.getpassword().equals(password)) return true;    
}
return false;
}
  
  public void lojas(){
  List<String> linhas = lerFicheiro("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt");    //C:\\Users\\Rui\\Desktop\\teste2.txt
  String[] linhaPartida;
  for (String linha : linhas) {
  linhaPartida = linha.split(":", 2);
  switch(linhaPartida[0]){
  case "Loja":     
  Loja l = parseLoja(linhaPartida[1]);
  System.out.println(l.toString());
  break; 
  default: 
  break; 
}
}

}
  
public boolean existeloja ( String nome){  //verificar se existe ou nao loja
for(Loja e : lo){
    if(nome.equals(e.getnome())) return true; 
}
return false; 
}      
   
  public boolean existeproduto(String produto)throws IOException{        //verificar se existe produto
  parse();
  for(Queue e : teste){
  if(produto.equals(e.getdesc())) return true ;     
  }
  return false;
  }
  

  
  public String aux2(String input){
  String[] campos = input.split(",");
  return campos[1];   
  }
  
  
  public String aux(String input){
  String[] campos = input.split(",");
  return campos[0];   
  }

  public int consultapedidos(String cod){      // dá pedidos feitos por um utilizador
  int quant = 0; 
  for(Pedido e : pe){
  if(cod.equals(e.getcodigoUsuario())){
  quant +=1;
  System.out.println("Encomenda:\n");
  e.toStringlinha();}
  }
  return quant; 
  }
  
 
  
  
  // para guardar 
  
  public void guardencomenda(Pedido e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nEncomenda: " + e.getcodigoEncomenda() + "," + e.getcodigoUsuario()
  + "," + e.getcodigoLoja() + "," + e.getPeso() + e.toStringlinha());
  f.close();
  }  
  
  public void guardaemp(Empresa e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nTransportadora: " + e.getcodigoEmpresa() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getnif()
  + "," + e.getraio() + "," + e.getcustotransporte() + "," + e.getpassword());
  f.close();
  }  
  
  public void guardauser(Usuário e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nUtilizador: " + e.getcodigoUsuario() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," 
  + e.getpassword());
  f.close();
  }
  
  public void guardaloja(Loja e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nLoja: " + e.getcodigoLoja() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getpassword());
  f.close();
  }
  
  public void guardavol(Voluntario e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nVoluntario: " + e.getcodigoVoluntario() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getraio()
  + "," + e.getpassword());
  f.close();
  }
  
  public void guardavolmed(Voluntario e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nVoluntario Med: " + e.getcodigoVoluntario() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getraio()
  +"," + "true" + "," + e.getpassword());
  f.close();
  }
  
  public void guardaprodutos(Queue e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Paulo Costa\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nProduto: " + e.getdesc() + "," + e.getcodigoProduto() + "," +  e.getvalorPreco());
  f.close();    
  }

}