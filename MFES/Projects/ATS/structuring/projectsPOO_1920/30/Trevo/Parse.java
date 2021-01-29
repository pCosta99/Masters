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
        import java.util.Set;
        import java.util.TreeSet;
        public class Parse {
            private List<Linhapedidos> teste = new ArrayList<>() ;
            private List<UmUser> users= new ArrayList<UmUser>();
            private Pedido p= new Pedido() ;
            private List<UmaLoja> lo = new ArrayList<UmaLoja>();
            private List<UmVoluntario> vol = new ArrayList<UmVoluntario>();
            private List<UmaEmpresa> emp = new ArrayList<UmaEmpresa>() ;
            private List<Pedido> pe= new ArrayList<Pedido>() ; 
            private Map<Double,UmVoluntario> pai = new HashMap<>();
            private Map<Double,UmVoluntario> filho = new HashMap<>();
            private Pedidosaceites aceites = new Pedidosaceites();
            
            public void parse() throws IOException{
                List<String> linhas = lerFicheiro("C:\\Users\\Pestana\\Desktop\\teste2.txt");
                String[] linhaPartida;
                for (String linha : linhas) {
                        linhaPartida = linha.split(":", 2);
                        switch(linhaPartida[0]){
                        case "Utilizador": 
                                UmUser u = parseUtilizador(linhaPartida[1]);
                                users.add(u);
                                break;
                        case "Loja": 
                                UmaLoja l = parseLoja(linhaPartida[1]);
                                lo.add(l);
                                break;                                   
                        //...
                        case "Voluntario":
                                UmVoluntario v = parseVoluntario(linhaPartida[1]);
                                vol.add(v);
                                break;
                        case "Voluntario Med": 
                                UmVoluntario q = parseVoluntariomed(linhaPartida[1]);
                                vol.add(q);
                                break;
                        case "Transportadora":
                                UmaEmpresa e = parseEmpresa(linhaPartida[1]);
                                emp.add(e);
                                break;
                        case "Encomenda": 
                               p = parsePedido(linhaPartida[1]);
                               pe.add(p);
                               break;
                        case "Aceite" :
                               Pedidosaceites pedido = parseAceite(linhaPartida[1]);
                               break;
                        default:
                                break;
                        }
        
                }
               
                }      
     
     public void top10 (){ //top 10 dos users com mais encomendas feitas 
     Set<UmUser> ret = new TreeSet<>(new Comparatora()); 
     ArrayList<UmUser> e = new ArrayList<>();
     ArrayList<Integer> h = new ArrayList<>();
     int nro = 0; 
     for(UmUser a : users){
     ret.add(a.clone());
     }
     for(UmUser k : ret){
     e.add(k.clone());  
     h.add(nropedidosuser(k.getcodutilizador()));
     nro+=1;
     }
     
     if(nro>=10){
     for(int i = 0 ; i<9 ; i++){
     System.out.println("\n\nTop: " + (i+1));
     System.out.println(e.get(i) + "\n" +  "Numero de pedidos:"+ h.get(i));
     }
     }
     if(nro<10){
     for(int i = 0 ; i<nro-1; i++){
     System.out.println("\n\nTop: " + (i+1));
     System.out.println(e.get(i) + "\n"+ "Numero de pedidos:"+h.get(i));
     }
     }
     }
                
     public int nropedidosuser(String cod){ //numero de pedidos feito por um user
     int ret = 0;    
     for(Pedido x : pe){
     if(cod.equals(x.getcodut()))
     ret+=1;
     }
     return ret; 
     }   
     public void empresas(){ //imprime todas as empresas 
     for(UmaEmpresa e : emp){
     System.out.println(e);    
     }
     }
 
     public void voluntarios(){ //imprime todos os voluntarios
     for(UmVoluntario e : vol){
     System.out.println(e);    
     }
     }
  
     public void avaliaemp(UmaEmpresa e, int av){ //avalia empresa
     e.addAvaliacao(av);    
     }
  
     public void avalialoja(UmaLoja e, int av){ //avalia loja
     e.addAvaliacao(av);    
     }
  
     public void avaliavol(UmVoluntario e, int av){ //avalia voluntario
     e.addAvaliacao(av);    
     }
  
     public Long distemp (UmUser e , UmaEmpresa v)throws InterruptedException { //calcula a distancia de um user ate uma empresa
     Coordenadas a = e.getgps(); 
     Coordenadas b = v.getgps(); 
     double distancia = v.distanciadoispontos(b,a)*1000;
     long tempoespera = Math.round(distancia);
     return tempoespera; 
     }
  
  
    public Long dist (UmUser e , UmVoluntario v)throws InterruptedException { //calcula a distancia de um user ate um voluntario
    Coordenadas a = e.getgps(); 
    Coordenadas b = v.getgps(); 
    double distancia = v.distanciadoispontos(b,a)*5000;
    long tempoespera = Math.round(distancia);
    return tempoespera; 
    }
  
    public void setdispvol (UmVoluntario e){ //Muda a disponibilidade de um voluntario para true
    e.setdisponibilidade(true);    
    }
  
    public void setdispemp (UmaEmpresa e){ //Muda a disponibilidade de uma empresa para true
    e.setdisponibilidade(true);    
    }
  
    public UmaEmpresa selecionaempresa (String cod,String coduser)throws IOException{ //seleciona a empresa que esta mais perto da loja em questao
    UmaLoja v = identificacod(cod); 
    ArrayList<UmaEmpresa> y = new ArrayList<>();
    Map<Double,UmaEmpresa> ret = new HashMap<>();
    Coordenadas a = v.getgps();
    UmUser k = identificauser(coduser);
    Coordenadas user = k.getgps();
    
    for(UmaEmpresa n : emp) {
    Coordenadas hm = n.getgps();
    Double dista = v.distanciadoispontos (hm,a);
    Double dista2 = v.distanciadoispontos (hm,user);
    if(n.getraio()>=dista && n.getraio()>=dista2 && n.disponivel()){ //so vai adicionar a empresa ao map se esta estiver disponivel e o raio>= 
    ret.put(dista,n);                         //que a distancia da empresa à loja 
    }
    }
    for(Map.Entry<Double,UmaEmpresa> q : ret.entrySet()){  //organiza o map por ordem crescente (compara as keys)
    ret=                                                   //As keys sao o valor da distancia das empresas à loja 
    ret.entrySet().stream()  
    .sorted(Entry.comparingByKey())
    .collect(Collectors.toMap(Entry::getKey, Entry::getValue,
                            (e1, e2) -> e1, LinkedHashMap::new));
    y.add(q.getValue());
  
    }
    int quant = 0; 
    for(UmaEmpresa e : y){      //Conta o numero de empresas que podem fazer a entrega 
    quant+=1;                   //Se nao houver empresas disponiveis return null, caso contrario seleciona a primeira da lista
    }                           //Sendo esta a empresa que esta mais proxima da loja 
    if(quant == 0) return null; 
    UmaEmpresa s = new UmaEmpresa(y.get(0));
    s.setdisponibilidade(false); 
    return s;
    }

    public UmVoluntario primeiromed (String cod,String coduser)throws IOException{//seleciona o voluntario medico que esta mais perto da loja em questao
    UmaLoja v = identificacod(cod); 
    ArrayList<UmVoluntario> y = new ArrayList<>();
    Map<Double,UmVoluntario> ret = new HashMap<>();
    Coordenadas a = v.getgps();
    UmUser k = identificauser(coduser);
    Coordenadas user = k.getgps();
    for(UmVoluntario n : this.vol) {//so vai adicionar o voluntario ao map se este estiver disponivel e o raio>=
    Coordenadas hm = n.getgps();    //que a distancia do voluntario à loja 
    Double dista = v.distanciadoispontos (hm,a);
    Double dista2 = v.distanciadoispontos (hm,user);
    if(n.getraio()>=dista && n.getraio()>=dista2  && n.disponivel() && n.getaceita()){
    ret.put(dista,n);
  
    }
    }
    for(Map.Entry<Double,UmVoluntario> q : ret.entrySet()){ //organiza o map por ordem crescente (compara as keys)
    ret=                                                    //As keys sao o valor da distancia dos voluntarios à loja
    ret.entrySet().stream()  
    .sorted(Entry.comparingByKey())
    .collect(Collectors.toMap(Entry::getKey, Entry::getValue,
                            (e1, e2) -> e1, LinkedHashMap::new));
    y.add(q.getValue());
  
    }
    int quant = 0; 
    for(UmVoluntario e : y){  //Conta o numero de voluntarios que podem fazer a entrega 
    quant+=1;                 //Se nao houver voluntarios disponiveis return null, caso contrario seleciona o primeira da lista
    }
    if(quant == 0) return null; 
    UmVoluntario s = new UmVoluntario(y.get(0));
    s.setdisponibilidade(false); 
    return s;
    }
  
    public List<UmVoluntario> distancias(UmaLoja e,UmUser k) throws IOException{ //calcula as distancias de um vol a uma loja
    Parse p = new Parse(); 
    p.parse(); 
    List<UmVoluntario> ret = new ArrayList<>();
    Coordenadas loja = e.getgps();
    Coordenadas user = k.getgps();
    for(UmVoluntario x : vol){
    Coordenadas vol = x.getgps();
    Double dist = x.distanciadoispontos (vol,loja);
    Double dist2 = x.distanciadoispontos (vol,user);
    if(dist<=x.getraio() && dist2<=x.getraio() && x.disponivel()){ //adiciona o voluntario se estiver disponivel e se o raio deste for > distancia
    ret.add(x);
    }
    }
    return ret; 
    }
  
  
    public Map<Double,UmVoluntario> primeiroaux (String cod,String coduser)throws IOException{
    UmaLoja v = identificacod(cod); 
    UmUser k = identificauser(coduser);
    List<UmVoluntario> y = distancias(v,k);  
    Map<Double,UmVoluntario> ret = new HashMap<>();
    Coordenadas a = v.getgps();
  
    for(UmVoluntario n : y) {
    Coordenadas w = n.getgps();  //adiciona a um map o valor da distancia dos voluntarios e os voluntarios
    double dist = n.distanciadoispontos(w,a);
    ret.put(dist,n);
    }
    return ret;
    }

    public List<UmVoluntario> primeiroaux2(String cod,String coduser)throws IOException{
    Map<Double,UmVoluntario> ret = primeiroaux(cod,coduser);
    List<UmVoluntario> y = new ArrayList<>();
    for(Map.Entry<Double,UmVoluntario> q : ret.entrySet()){ //organiza os voluntarios pela sua distancia à loja em questao 
    ret= 
    ret.entrySet().stream()  
    .sorted(Entry.comparingByKey())
    .collect(Collectors.toMap(Entry::getKey, Entry::getValue,
                            (e1, e2) -> e1, LinkedHashMap::new));
    y.add(q.getValue());
    }
    return y; 
    }
 
    public UmVoluntario primeiro(String cod,String coduser) throws IOException{
    List<UmVoluntario> y = primeiroaux2(cod,coduser);
    int quant = 0; 
    for(UmVoluntario e : y){ //seleciona o voluntario mais perto
    quant+=1;     
    }
    if(quant == 0) return null; 
    UmVoluntario s = new UmVoluntario(y.get(0));
    s.setdisponibilidade(false); 
    return s; 
    }
    

    public boolean calma(boolean e,String codloja){ //altera o estado da encomenda na loja 
    for(UmaLoja c : lo){
    if(c.getcodloja().equals(codloja))
    return c.estado(e);
    }
    return false;    
    }
  
    
    public double calculaprecoemp(Pedido e,UmaEmpresa x){ //calcula o preço com o transporte (preco normal + preco p/km)
    String coduser = e.getcodut(); 
    UmUser r = identificauser(coduser);
    Coordenadas k = r.getgps(); 
    Coordenadas y = x.getgps(); 
    Double dista = x.distanciadoispontos(k,y);
    return e.calculapreco()+((x.getcustotransporte()*dista)); 
    }
  
    public double calculapreco(Pedido e){ // calcula o preço 
    return e.calculapreco(); 
    }
  
    public String codlojanome(String nome){ //return do codigo da loja atraves do nome 
  
    for(UmaLoja e : lo){
    if(nome.equals(e.getnome())) return e.getcodloja();    
    }
    return null;   
    }

    public String coduserlogin(String nome, String pass){ //faz o login de um user
    String cod = new String();     
    for(UmUser e : users){
    if(e.getnome().equals(nome) && e.getpass().equals(pass)) return e.getcodutilizador();    
    }
    return cod; 
    }
    
    public String geracoduser(){ //gera um codigo de um utilizador 
    StringBuilder sb = new StringBuilder();
    Random random = new Random();
    int num = random.nextInt(100);
    String ret = new String();
    sb.append("u").append(String.valueOf(num)); 
    ret=sb.toString(); 
 
    for(UmUser e : users){
    if(e.getcodutilizador().equals(ret)) geracoduser();
    }
    return ret ;      
    }
 
    public String geracodvol(){ //gera o codigo de um voluntario
    StringBuilder sb = new StringBuilder();
    Random random = new Random();
    int num = random.nextInt(100);
    String ret = new String();
    sb.append("v").append(String.valueOf(num)); 
    ret=sb.toString(); 
 
    for(UmVoluntario e : vol){
    if(e.getcodvoluntario().equals(ret)) geracodvol();
    }
    return ret ;      
    }
  
    public String geracodempresa(){ //gera o codigo de uma empresa
    StringBuilder sb = new StringBuilder();
    Random random = new Random();
    int num = random.nextInt(100);
    String ret = new String();
    sb.append("t").append(String.valueOf(num)); 
    ret=sb.toString(); 
 
    for(UmaEmpresa e : emp){
    if(e.getcod().equals(ret)) geracodempresa();
    }
    return ret ;      
    }
    
    public String geracodloja(){ //gera codigo de uma loja
    StringBuilder sb = new StringBuilder();
    Random random = new Random();
    int num = random.nextInt(100);
    String ret = new String();
    sb.append("l").append(String.valueOf(num)); 
    ret=sb.toString(); 
 
    for(UmaLoja e : lo){
    if(e.getcodloja().equals(ret)) geracodloja();
    }
    return ret ;      
    }
 
    public void avisa(UmaLoja e)throws InterruptedException{ //espera algum tempo ate a encomenda estar pronta 
    for(UmaLoja a : lo){
    if(a.getcodloja().equals(e.getcodloja())) 
    e.encomendapronta();
    }
    }
  
    public UmUser identificauser (String e){ //identifica user atraves do codigo
    UmUser ret = new UmUser();
    for(UmUser a: users){
    if(e.equals(a.getcodutilizador())) {
    String nome = a.getnome(); 
    String pass = a.getpass();
    Coordenadas c = a.getgps(); 
    ret = new UmUser(e,nome,c,pass);
    }
    } 
    return ret;   
    }
 
    public UmVoluntario identificavolnome (String e){ //identifica o voluntario atraves do nome
    UmVoluntario ret = new UmVoluntario();
    for(UmVoluntario a: vol){
    if(e.equals(a.getnome())) {
    String nome = a.getcodvoluntario(); 
    String pass = a.getpass();
    Coordenadas c = a.getgps(); 
    double raio = a.getraio();
    ret = new UmVoluntario(nome,e,c,raio,pass);
    }
    } 
    return ret;   
    }
 
    public UmVoluntario identificavol (String e){ //identifica voluntario atraves do codigo
    UmVoluntario ret = new UmVoluntario();
    for(UmVoluntario a: vol){
    if(e.equals(a.getcodvoluntario())) {
    String nome = a.getnome(); 
    String pass = a.getpass();
    Coordenadas c = a.getgps(); 
    double raio = a.getraio();
    ret = new UmVoluntario(e,nome,c,raio,pass);
    }
    } 
    return ret;   
    }
 
    public UmaLoja identifica (String e){ //identifica loja atraves do nome
    UmaLoja ret = new UmaLoja();
    for(UmaLoja a: lo){
    if(e.equals(a.getnome())) {
    String cod = a.getcodloja(); 
    String pass = a.getpass();
    Coordenadas c = a.getgps(); 
    ret = new UmaLoja(cod,e,c,pass);
    }
    } 
    return ret;   
    }
 
    public UmaEmpresa identificaemp (String e){ //identifica empresa atraves do nome
    UmaEmpresa ret = new UmaEmpresa();
    for(UmaEmpresa a: emp){
    if(e.equals(a.getnome())) {
    String cod = a.getcod(); 
    String pass = a.getpass();
    Coordenadas c = a.getgps(); 
    int nif = a.getnif();
    double raio = a.getraio();
    double custo = a.getcustotransporte();
    ret = new UmaEmpresa(cod,e,c,nif,raio,custo,pass);
    }
    } 
    return ret;   
    }
 
    public UmaLoja identificacod (String e){ //identifica loja atraves do codigo
    UmaLoja ret = new UmaLoja();
    for(UmaLoja a: lo){
    if(e.equals(a.getcodloja())) {
    String cod = a.getnome(); 
    String pass = a.getpass();
    Coordenadas c = a.getgps(); 
    ret = new UmaLoja(e,cod,c,pass);
    }
    } 
    return ret;   
    }
  
    public String geracodenc(){ //gera o codigo de uma encomenda 
    StringBuilder sb = new StringBuilder();
    Random random = new Random();
    int num = random.nextInt(100);
    String ret = new String();
    sb.append("e").append(String.valueOf(num)); 
    ret=sb.toString(); 
 
    for(Pedido e : pe){
    if(e.getcodencomenda().equals(ret)) geracodenc();
    }
    return ret ;      
    }
 
    public String geracodprod()throws IOException{ //gera o codigo de um produto caso este nao exista 
    StringBuilder sb = new StringBuilder();        //Se o produto ja existir, este usa o mesmo codigo
    Random random = new Random();
    int num = random.nextInt(1000);
    String ret = new String();
    sb.append("p").append(String.valueOf(num)); 
    ret=sb.toString(); 
 
    for(Linhapedidos e : teste){
    if(e.getcodproduto().equals(ret)) geracodprod();
    }
    return ret ;      
     
    }
   
    public double gerapreco(String nome)throws IOException{ //gera um preço aleatorio caso o produto nao exista 
    parse();                                                //se existir usa o mesmo preço
    for(Linhapedidos e : teste){
    if(e.getdescricao().equals(nome)) 
    return e.getvalorunitario(); 
    }    
 
    Random random = new Random();
    Double preco = random.nextDouble();
    return preco;    
    }
 
    public double gerapeso(String codenc)throws IOException{ //gera um peso aleatorio
    parse();
    for(Pedido e : pe){
    if(e.getcodencomenda().equals(codenc)) 
    return e.getPeso(); 
    }
    Random random = new Random();
    Double peso = random.nextDouble();
    return peso;    
    }
 
    public Linhapedidos criapedido(String codprod,String nome,int quantidade)throws IOException{ //cria um novo pedido
    double preco = gerapreco(nome);
    return new Linhapedidos(codprod,nome,quantidade,preco); 
    }
 
    public String desctocod(String desc)throws IOException{ //Atraves do nome do produto return do codigo 
    parse();
    String codproduto = new String (); 
    for(Linhapedidos e : teste){
    if(e.getdescricao().equals(desc))
    codproduto = e.nometocod(desc);
    }
    return codproduto;
    }
 
    public boolean existeprod(String desc) throws IOException{ //Existe produto ou nao
    for(Linhapedidos e : teste){
    if(e.getdescricao().equals(desc)) return true;    
    }
    return false;
    }
     
    public Pedido geraPedido (List<Linhapedidos> u, String coduser, String codloja)throws IOException{ //cria uma encomenda
    String cod = new String();   
    cod=geracodenc();
    double peso = gerapeso(cod);
    Pedido c = new Pedido(cod,coduser,codloja,peso,u); 
    return c; 
    }
   
    public void naosei (List<Linhapedidos> u, String coduser, String codloja)throws IOException{
    String cod = geracodenc();   
    double peso = gerapeso(cod);
    Pedido c = new Pedido(cod,coduser,codloja,peso,u);
    pe.add(c); 
    String teste = c.getcodencomenda(); 
    aceites.addcodencomenda(teste);
    guardaceita(aceites);
    guardencomenda(c);   
    }
  
    public void alerta(String e)throws InterruptedException,IOException{ 
    parse();
    avisa(identifica(e)); 
    }
 
  
     public void adduser(String nome, Coordenadas a,String pass) throws IOException{ //regista um user 
     String cod = geracoduser();
     UmUser e = new UmUser(cod,nome,a,pass); 
     guardauser(e);
     users.add(e);
     }  
  
     public void addloja(String nome, Coordenadas a,String pass) throws IOException{ //regista loja 
     String cod = geracodloja();
     UmaLoja e = new UmaLoja(cod,nome,a,pass); 
     guardaloja(e);
     lo.add(e);
     }
  
     public void addtrans(String nome, Coordenadas a,int nif,double raio,double custo,String pass) throws IOException{ //regista transportadora 
     String cod = geracodempresa();
     UmaEmpresa e = new UmaEmpresa(cod,nome,a,nif,raio,custo,pass); 
     guardaemp(e);
     emp.add(e);
     }

     public void addvolmed(String nome, Coordenadas a,double raio,String pass) throws IOException{ //regista voluntario meds 
     String cod = geracodvol();
     UmVoluntario e = new UmVoluntario(cod,nome,a,raio,true,pass); 
     guardavolmed(e);
     vol.add(e);
     }

  
     public void addvol(String nome, Coordenadas a,double raio,String pass) throws IOException{ //regista voluntario
     String cod = geracodvol();
     UmVoluntario e = new UmVoluntario(cod,nome,a,raio,pass); 
     guardavol(e);
     vol.add(e);
     }

     public boolean login(String nome, String pass){ //faz login
     for(UmUser e : users){ 
     if(e.getnome().equals(nome) && e.getpass().equals(pass)) return true;    
     }
     return false;
     }
  
     public void lojas(){ //imprime todas as lojas 
     List<String> linhas = lerFicheiro("C:\\Users\\Pestana\\Desktop\\teste2.txt");//C:\\Users\\Pestana\\Desktop\\teste2.txt
     String[] linhaPartida;
     for (String linha : linhas) {
     linhaPartida = linha.split(":", 2);
     switch(linhaPartida[0]){
     case "Loja":     
     UmaLoja l = parseLoja(linhaPartida[1]);
     System.out.println(l.toString());
     break; 
     default: 
     break; 
     }
     }

     }
  
     public boolean existeloja ( String nome){ //existe ou nao loja 
     for(UmaLoja e : lo){
     if(nome.equals(e.getnome())) return true; 
     }
     return false; 
     }      
   
     public boolean existeproduto(String produtoo)throws IOException{ //existe ou nao produto
     parse();
     for(Linhapedidos e : teste){
     if(produtoo.equals(e.getdescricao())) return true ;     
     }
     return false;
     }
  
     public int consultapedidos(String cod){  //consulta todos os pedidos feitos por um utilizador 
     int quant = 0; 
     for(Pedido e : pe){
     if(cod.equals(e.getcodut())){
     quant +=1;
     System.out.println("Encomenda:\n");
     e.toStringlinhalinda();}
     }
     return quant; 
     }
  
     public UmUser parseUtilizador(String input){
     String[] campos = input.split(",");
     String nome = campos[0]; 
     String codUtilizador = campos[1];
     double gpsx = Double.parseDouble(campos[2]);
     double gpsy = Double.parseDouble(campos[3]);
     Coordenadas e = new Coordenadas(gpsx,gpsy);
     String pass = campos[4];
     return new UmUser(nome,codUtilizador,e,pass);
  }
      
  public Pedidosaceites parseAceite(String input){
      String [] campos = input.split(",");
      String cod = campos[0];
      List<String> pedidosaceites = new ArrayList<>();
      pedidosaceites.add(cod);
      return new Pedidosaceites(pedidosaceites);
  }
  public List<Linhapedidos> parseteste(String[] input) throws IOException{
      int i,j,k,l;
      List<Linhapedidos> ret = new ArrayList<>();
      
      for (i = 0; i < input.length; i += 4) // moves 4 at a time
      { 
          String codprod = input[i];
          String desc = input[i+1];
          double quant = Double.parseDouble(input[i+2]);
          double preco = Double.parseDouble(input[i+3]);
          Linhapedidos pedido = new Linhapedidos(codprod, desc, quant, preco);
          teste.add(pedido);
          guardaprodutos(pedido);
          ret.add(pedido);
        }
        
      return ret;
  }
  
  public Pedido parsePedido(String input) throws IOException{
        List<Linhapedidos> pedidos = new ArrayList<>();
        String[] campos = input.split(","); 
        String cod = campos[0];
        String user = campos[1]; 
        String loja = campos[2]; 
        double peso = Double.parseDouble(campos[3]); 
        String[] restOfFields = Arrays.copyOfRange(campos,4,campos.length);
        pedidos = parseteste(restOfFields);
        return new Pedido(cod,user,loja,peso,pedidos);
  }
  

  public UmaEmpresa parseEmpresa(String input){
      String[] campos = input.split(",");
      String cod = campos[0];
      String nome = campos[1];
      double gpsx = Double.parseDouble(campos[2]);
      double gpsy = Double.parseDouble(campos[3]);
      int nif = Integer.parseInt(campos[4]);
      double raio = Double.parseDouble(campos[5]);
      double preco = Double.parseDouble(campos[6]);
      String pass = campos[7];
      Coordenadas e = new Coordenadas(gpsx, gpsy);
      return new UmaEmpresa(cod,nome,e,nif,raio,preco,pass);
  }
  
  
  public UmVoluntario parseVoluntariomed(String input){
        String[] campos = input.split(",");
        String codVol = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        String pass = campos[5];
        Coordenadas e = new Coordenadas(gpsx,gpsy);
        return new UmVoluntario(codVol,nome,e,raio,true,pass);
  }

  
  public UmVoluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String codVol = campos[0];
        String nome = campos[1]; 
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        String pass = campos[5];
        Coordenadas e = new Coordenadas(gpsx,gpsy);
        return new UmVoluntario(codVol,nome,e,raio,pass);
  }

  public UmaLoja parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        Coordenadas e = new Coordenadas(gpsx,gpsy);
        String pass = campos[4];
        return new UmaLoja(codLoja,nomeLoja,e,pass);
  }

  public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
  }
  //GRAVAR TODOS OS REGISTOS NO FICHEIRO LOGS
  
  public void guardaceita(Pedidosaceites e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nAceite: " + e.pedidos());
  f.close();
  }  
  
  
  public void guardencomenda(Pedido e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nEncomenda: " + e.getcodencomenda() + "," + e.getcodut()
  + "," + e.getcodloja() + "," + e.getPeso() + e.toStringlinha());
  f.close();
  }  
  
  public void guardaemp(UmaEmpresa e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nTransportadora: " + e.getcod() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getnif()
  + "," + e.getraio() + "," + e.getcustotransporte() + "," + e.getpass());
  f.close();
  }  
  
  public void guardauser(UmUser e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nUtilizador: " + e.getcodutilizador() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," 
  + e.getpass());
  f.close();
  }
  
  public void guardaloja(UmaLoja e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nLoja: " + e.getcodloja() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getpass());
  f.close();
  }
  
  public void guardavol(UmVoluntario e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nVoluntario: " + e.getcodvoluntario() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getraio()
  + "," + e.getpass());
  f.close();
  }
  
  public void guardavolmed(UmVoluntario e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\teste2.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nVoluntario Med: " + e.getcodvoluntario() + "," + e.getnome() + "," + e.getx() + "," + e.gety() + "," + e.getraio()
  +"," + "true" + "," + e.getpass());
  f.close();
  }
  
  public void guardaprodutos(Linhapedidos e) throws IOException{
  FileWriter f = new FileWriter("C:\\Users\\Pestana\\Desktop\\produtos.txt",true); 
  PrintWriter gravarArq = new PrintWriter(f);
  
  gravarArq.printf("\nProduto: " + e.getdescricao() + "," + e.getcodproduto() + "," +  e.getvalorunitario());
  f.close();    
  }

}