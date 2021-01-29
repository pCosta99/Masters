import java.lang.String;
import java.util.List;
import java.util.ArrayList;
import java.io.Reader; 
import java.lang.Object;
import java.util.Scanner; 
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.Scanner;
 


public class NovoMenu{
    private String codloja = new String(); 
    private String coduser = new String();
    private List<Linhapedidos> ret = new ArrayList<>();
    public NovoMenu()throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();    
        
    System.out.println("****************Bem vindo ao trevo :)))****************");
    System.out.println("Escolha:\n1)Login\n2)Registar\n3)Consultar avaliacoes\n4)Top 10 users\n0)Sair");
    Scanner s = new Scanner (System.in);
    String escolha=s.nextLine();    
    switch (escolha){
    case ("1"): 
    login();
    break; 
    case ("2"): 
    regista();
    new NovoMenu();
    break;
    case("3"):
    verclass();
    new NovoMenu();
    break;
    case("4"):
    p.top10();
    new NovoMenu();
    break;
    case("0"): 
    System.exit(0); 
    break;
    default: 
    System.out.println("Opcao invalida :(");
    new NovoMenu();
    break; 
    }
    }
    
    public void consultaenc(String cod)throws IOException,InterruptedException{
    Parse p = new Parse(); 
    p.parse();
    System.out.println("Encomendas feitas: ");
    if(p.consultapedidos(cod)==0) {System.out.println("Ainda nao fez encomendas :("); new NovoMenu();}
    new NovoMenu();     
    }
    
    public void login()throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();       
    Scanner s = new Scanner (System.in);
    Scanner y = new Scanner (System.in);
    System.out.println("Nome do user: ");
    String nome = s.nextLine();
    System.out.println("Password: ");
    String pass = y.nextLine();    
    if(p.login(nome,pass)) {    
    this.coduser= p.coduserlogin(nome,pass);
    System.out.println("Login feito com sucesso");
    System.out.println("Deseja:\n1)Fazer encomenda\n2)Consultar todas as encomendas feitas pelo user");
    String escolha = s.nextLine(); 
    switch(escolha){
    case("1"):
    fazencomenda();
    new NovoMenu(); 
    break; 
    case("2"): 
    consultaenc(this.coduser);
    break; 
    default: 
    System.out.println("Opcao desconhecida :("); 
    new NovoMenu(); 
    }
    }    
    else{System.out.println("1)Tentar outra vez\n2)Registar");
    String escolhe = s.nextLine();
    switch (escolhe){
     case("1"): 
     login();
     break; 
     case("2"):
     regista();
     break;
     default: 
     System.out.println("Opcao invalida :(("); 
     new NovoMenu();
    }
    }    
    }
    
    public void regista() throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();       
    Scanner s = new Scanner (System.in);       
    System.out.println("Deseja registar:\n1)User\n2)Voluntario\n3)Transportadora\n4)Loja\n5)Login");
    String a=s.nextLine();
    switch(a){
    case("1"):
    registauser();
         new NovoMenu(); 
    break;
    case("2"):
    registavol();
         new NovoMenu(); 
    break;
    case("3"):
    registatrans();
         new NovoMenu(); 
    break;
    case("4"): 
    registaloja();
         new NovoMenu(); 
    break;
    case("5"):
    login();
    break;
    default: 
    System.out.println("Opcao invalida :((");
    new NovoMenu();
    break;        
    }
    }
    
    public void registauser()throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse(); 
    Scanner s = new Scanner(System.in);
    System.out.println("Nome do user:");
    String nome = s.nextLine();
    System.out.println("Password:");
    String pass = s.nextLine();
    System.out.println("Coordenadas (primeiro x e depois y): ");
    double x = s.nextDouble();
    double y = s.nextDouble(); 
    Coordenadas c = new Coordenadas(x,y);
    p.adduser(nome,c,pass);       
    }
    
    public void registavol() throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse(); 
    Scanner x = new Scanner (System.in);       
    System.out.println("Nome do voluntario");
    String vol = x.nextLine();
    System.out.println("Password:");
    String pass = x.nextLine();
    System.out.println("Coordenadas (primeiro x e depois y)");
    double aa = x.nextDouble();
    double bb = x.nextDouble();
    Coordenadas c = new Coordenadas(aa,bb);
    System.out.println("Raio");
    double raio = x.nextDouble();
    System.out.println("Aceita fazer transporte de medicamentos?\n1)Sim\n2)Nao");
    Scanner r = new Scanner (System.in);
    String aceita = r.nextLine();
    switch(aceita){
     case("1"): 
     p.addvolmed(vol,c,raio,pass);
     break; 
     case("2"):
     p.addvol(vol,c,raio,pass);
     break; 
     default: 
     System.out.println("Opcao desconhecida :(("); 
     new NovoMenu();
     break;
    }
     
    }
    
    public void registatrans() throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse(); 
    Scanner y = new Scanner (System.in);   
    System.out.println("Nome do transporte: ");
    String nome = y.nextLine();
    System.out.println("Password:");
    String pass = y.nextLine();
    System.out.println("Coordenadas (primeiro x e depois y)");
    double aa = y.nextDouble();
    double bb = y.nextDouble(); 
    Coordenadas c = new Coordenadas(aa,bb);
    System.out.println("Raio");
    double raio = y.nextDouble();
    System.out.println("Nif");
    int nif = y.nextInt();
    System.out.println("Custo por km");
    double custo = y.nextDouble();
    p.addtrans(nome,c,nif,raio,custo,pass);   
    }
    
    public void registaloja() throws IOException{
    Parse p = new Parse();
    p.parse(); 
    Scanner w = new Scanner(System.in);
    System.out.println("Nome da loja:");
    String nome = w.nextLine();
    System.out.println("Password:");
    String pass = w.nextLine();
    System.out.println("Coordenadas (primeiro x e depois y): ");
    double x = w.nextDouble();
    double y = w.nextDouble(); 
    Coordenadas c = new Coordenadas(x,y);
    p.addloja(nome, c,pass);      
    }
    
    public void verclass()throws IOException,InterruptedException {
    Parse p = new Parse();
    p.parse();
    Scanner s = new Scanner(System.in); 
    System.out.println("Ver classificacoes de:\n1)Lojas\n2)Voluntarios\n3)Empresas");    
    String escolha = s.nextLine(); 
    switch(escolha){
    case("1"):      
    System.out.println("Lojas disponiveis: ");
    p.lojas();
    System.out.println("Nome da loja: "); 
    String loja = s.nextLine(); 
    verclassloja(loja);
    break; 
    case("2"): 
    System.out.println("Voluntarios: ");
    p.voluntarios();
    System.out.println("Nome do voluntario: "); 
    String vol = s.nextLine(); 
    verclassvol(vol);
    break; 
    case("3"): 
    System.out.println("Empresas: ");
    p.empresas();
    System.out.println("Nome da empresa: "); 
    String emp = s.nextLine(); 
    verclassemp(emp);
    break; 
    default: 
    new NovoMenu(); 
    }
    }
    
    public void verclassloja(String nome)throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse(); 
    UmaLoja e = p.identifica(nome); 
    System.out.println("Media das avaliacoes: " + e.mediaavaliaca());
    }
    
    public void verclassvol(String nome)throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();
    UmVoluntario e = p.identificavol(nome); 
    System.out.println("Media das avaliacoes: " + e.mediaavaliaca());
    }
    
    public void verclassemp(String nome)throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();
    UmaEmpresa e = p.identificaemp(nome); 
    System.out.println("Media das avaliacoes: " + e.mediaavaliaca());
    }
    
    public void avaliavol(int av,UmVoluntario e)  throws IOException, InterruptedException{
    e.addAvaliacao(av);
    }
    
    public void avalialoja(int av,UmaLoja e)  throws IOException, InterruptedException{
    e.addAvaliacao(av);
    }
    
    public void avaliatrans(int av,UmaEmpresa e)  throws IOException, InterruptedException{
    e.addAvaliacao(av);
    }
    
    public void selecionaloja() throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();     
    Scanner s = new Scanner(System.in); 
    System.out.println("Lojas disponiveis: ");
    p.lojas();
    System.out.println("Indique o nome da loja a encomendar"); 
    String loja = s.nextLine();
    if(p.existeloja(loja)){
    UmaLoja e = p.identifica(loja); 
    this.codloja = p.codlojanome(loja);
    }
    else {System.out.println("Loja indisponivel :(("); 
        new NovoMenu();
    }
    }
    
   
    
    public void entregavol() throws IOException, InterruptedException{
    Parse p = new Parse();
    p.parse();
    System.out.println("A preparar encomenda"); 
    p.calma(false, this.codloja);
    Thread.sleep(4000); //tempo de espera ate a encomenda estar pronta a ser recolhida 
    p.calma(true, this.codloja);
    System.out.println("Encomenda pronta");
    System.out.println("A escolher voluntario: "); 
    UmVoluntario h = p.primeiro(this.codloja,this.coduser);
    if(p.primeiro(this.codloja,this.coduser) == null){
    System.out.println("Nao existem voluntarios disponiveis de momento :(\n1)Entrega por transportadora\n2)Tentar mais tarde");
    Scanner s = new Scanner (System.in); 
    String ups = s.nextLine(); 
    switch(ups){
     case("1"):
     entregaemp(); 
     break;
     case("2"):
     new NovoMenu(); 
     break; 
     default: 
     System.out.println("Opcao invalida :("); 
     new NovoMenu();
     break;        
    }
    }
    System.out.println(h);
    String codvol = h.getcodvoluntario();
    p.identificavol(codvol);
    System.out.println("Voluntario a caminho :)");
    UmUser k = p.identificauser(coduser);
    Coordenadas corduser = k.getgps(); 
    Long tempo = p.dist(k,h);
    Thread.sleep(tempo); 
    System.out.println("Encomenda entregue :)");
    Pedido c = p.geraPedido(ret,coduser,codloja);
    p.naosei(ret,coduser,codloja);
    calculapreco(c);
    UmVoluntario f = p.identificavol(codvol);
    f.setgps(corduser);
    p.setdispvol(f);   
    System.out.println("Deseja avaliar:\n1)Loja\n2)Voluntario\n3)Sair");
    Scanner b = new Scanner(System.in); 
    String oi = b.nextLine();
    switch(oi){
     case("1"): 
     UmaLoja o = p.identifica(this.codloja); 
     Scanner t = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int ava = t.nextInt(); 
     avalialoja(ava,o);
     new NovoMenu(); 
     break; 
     case("2"): 
     Scanner w = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int avalia = w.nextInt(); 
     avaliavol(avalia,f); 
     new NovoMenu(); 
     break; 
     case("3"):
     System.exit(0); 
     break;
     default: 
     new NovoMenu(); 
    }
    }
    
    
    public void entregavolmel() throws IOException, InterruptedException{
    Parse p = new Parse();
    p.parse();
    System.out.println("A preparar encomenda"); 
    p.calma(false, this.codloja);
    Thread.sleep(4000);
    p.calma(true, this.codloja);
    System.out.println("Encomenda pronta");
    System.out.println("A escolher voluntario: "); 
    UmVoluntario h = p.primeiromed(this.codloja,this.coduser);
    if(p.primeiromed(this.codloja,this.coduser) == null){
    System.out.println("Nao existem voluntarios disponiveis de momento :(\n1)Entrega por transportadora\n2)Tentar mais tarde");
    Scanner s = new Scanner (System.in); 
    String ups = s.nextLine(); 
    switch(ups){
     case("1"):
     entregaemp(); 
     break;
     case("2"):
     new NovoMenu(); 
     break; 
     default: 
     System.out.println("Opcao invalida :("); 
     new NovoMenu();
     break;        
    }
    }
    System.out.println(h);
    String codvol = h.getcodvoluntario();
    p.identificavol(codvol);
    System.out.println("Voluntario a caminho :)");
    UmUser k = p.identificauser(coduser);
    Coordenadas corduser = k.getgps(); 
    Long tempo = p.dist(k,h);
    Thread.sleep(tempo); 
    System.out.println("Encomenda entregue :)");
    Pedido c = p.geraPedido(ret,coduser,codloja);
    p.naosei(ret,coduser,codloja);
    calculapreco(c);
    UmVoluntario f = p.identificavol(codvol);
    f.setgps(corduser);
    p.setdispvol(f);   
    System.out.println("Deseja avaliar:\n1)Loja\n2)Voluntario\n3)Voltar ao menu\n4)Sair");
    Scanner b = new Scanner(System.in); 
    String oi = b.nextLine();
    switch(oi){
     case("1"): 
     UmaLoja o = p.identifica(this.codloja); 
     Scanner t = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int ava = t.nextInt(); 
     avalialoja(ava,o);
     new NovoMenu(); 
     break; 
     case("2"): 
     Scanner w = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int avalia = w.nextInt(); 
     avaliavol(avalia,f); 
     new NovoMenu(); 
     break; 
     case("3"):
     new NovoMenu(); 
     break;
     case("4"):
     System.exit(0);
     default: 
     new NovoMenu(); 
    }
    }
    
    
    public void entregaemp() throws IOException, InterruptedException{
    Parse p = new Parse();
    p.parse();
    System.out.println("A preparar encomenda"); 
    p.calma(false, this.codloja);
    Thread.sleep(4000);
    p.calma(true, this.codloja);
    System.out.println("Encomenda pronta");
    System.out.println("A escolher transortadora: "); 
    UmaEmpresa j = p.selecionaempresa(this.codloja,this.coduser);
    if(p.selecionaempresa(this.codloja,this.coduser)==null){System.out.println("Nao existem meios de transporte disponiveis de momento :("); new NovoMenu();}
    System.out.println(j);
    String nomeemp = j.getnome();
    p.identificaemp(nomeemp);
    System.out.println("Transportadora a caminho :)");
    UmUser w = p.identificauser(coduser);
    Coordenadas corduser = w.getgps(); 
    Long tempoo = p.distemp(w,j);
    Thread.sleep(tempoo); 
    System.out.println("Encomenda entregue :)");
    Pedido c = p.geraPedido(ret,coduser,codloja);
    System.out.println("Preco antes do transporte");
    calculapreco(c);
    System.out.println("Preco com transporte: "+p.calculaprecoemp(c,j));
    p.naosei(ret,coduser,codloja);
    j.setgps(corduser);
    p.setdispemp(j);
    System.out.println("Deseja avaliar:\n1)Loja\n2)Transportadora\n3)Voltar ao menu\n4)Sair");
    Scanner bb = new Scanner(System.in); 
    String oii = bb.nextLine(); 
    switch(oii){
     case("1"): 
     UmaLoja o = p.identifica(this.codloja); 
     Scanner t = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int ava = t.nextInt(); 
     avalialoja(ava,o);
     new NovoMenu(); 
     break; 
     case("2"): 
     Scanner ww = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int avalia = ww.nextInt(); 
     avaliatrans(avalia,j); 
     new NovoMenu(); 
     break; 
     case("3"):
     new NovoMenu(); 
     break;
     case("4"):
     System.exit(0);
     break;
     default: 
     new NovoMenu(); 
    }     
    }
    
    public void quementrega()throws IOException,InterruptedException{
    Parse p = new Parse(); 
    p.parse();
    Pedido c = new Pedido(); 
    UmUser k = new UmUser(); 
    Coordenadas corduser = new Coordenadas(); 
    Scanner s = new Scanner(System.in); 
    String escolha = new String(); 
    System.out.println("Entrega por:\n1)Voluntario\n2)Voluntario que pode entregar medicamentos\n3)Transportadora");    
    escolha = s.nextLine(); 
    switch(escolha) {
    case("1"): 
    entregavol();
    break; 
    case("2"): 
    entregavolmel();
    break;
    case("3"):
    entregaemp();
    break; 
    default: 
    System.out.println("Opcao desconhecida :(");
    new NovoMenu(); 
    break; 
    }
    }
    
    
    public void ciclo() throws IOException,InterruptedException{
    Parse p = new Parse();
    System.out.println("Quer adicionar mais produtos:\n1)Sim\n2)Nao"); 
    Scanner e = new Scanner(System.in);
    String escolha = e.nextLine(); 
    switch(escolha){
    case("1"): 
    this.ret.add(pede());
    ciclo();
    break; 
    case("2"):
    quementrega();
    break;
    default:
    System.out.println("Opcao invalida :(");
    new NovoMenu();    
    }
    }
    
    public void calculapreco(Pedido e) throws IOException{
    Parse p = new Parse(); 
    p.parse(); 
    System.out.println("Preco a pagar: " + p.calculapreco(e));
    }
    
    public void fazencomenda() throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();
    selecionaloja();
    this.ret.add(pede());
    ciclo();
    }
    
    public List<Linhapedidos> h() throws IOException{
    List<Linhapedidos> func = new ArrayList<>();
    func.add(pede()); 
    return func; 
    }
    
    public Linhapedidos pede() throws IOException{
    Scanner s = new Scanner(System.in); 
    String nome = descprod(); 
    int quant = quantprod(); 
    return addpedido(nome,quant);
    }
    
    public String descprod() throws IOException{
    Parse p = new Parse();
    p.parse();
    Scanner s = new Scanner(System.in);
    String nome = new String(); 
    System.out.println("Nome do produto a encomendar: "); 
    nome=s.nextLine(); 
    return nome; 
    }
    
    public int quantprod() throws IOException{
    Parse p = new Parse();
    p.parse();
    Scanner s = new Scanner(System.in);
    int nome = 0;     
    System.out.println("Quantidade a encomendar: "); 
    nome=s.nextInt(); 
    return nome; 
    }
    
    public Linhapedidos addpedido(String desc,int quant)throws IOException{
    Parse p = new Parse();
    p.parse();  
    String cod = new String();
    if(p.existeprod(desc)){
    cod=p.desctocod(desc); 
    return p.criapedido(cod,desc,quant); 
    }   
    cod=p.geracodprod(); 
    return p.criapedido(cod,desc,quant); 
    }
    
}