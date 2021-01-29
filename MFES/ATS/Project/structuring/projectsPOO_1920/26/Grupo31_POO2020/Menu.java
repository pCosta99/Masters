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
 


public class Menu{
    private String codloja = new String(); 
    private String coduser = new String();
    private List<Queue> ret = new ArrayList<>();
    public Menu()throws IOException,InterruptedException{ // menu inicial
    Parse p = new Parse();
    p.parse();    
        
    System.out.println("################# !Bem vindo! ################# ");
    System.out.println("Opções:\n1)Login\n2)Registar\n3)Consultar avaliacoes\n0)Sair");
    Scanner s = new Scanner (System.in);
    String escolha=s.nextLine();    
    switch (escolha){
    case ("1"): 
    login();
    break; 
    case ("2"): 
    regista();
    break;
    case("3"):
    verclassificaçao();
    break;
    case("0"): 
    System.exit(0); 
    break;
    default: 
    System.out.println("Opcao invalida :(");
    new Menu();
    break; 
    }
    }
    
    public void login()throws IOException,InterruptedException{ //efetuar o login
    Parse p = new Parse();
    p.parse();       
    Scanner s = new Scanner (System.in);
    Scanner y = new Scanner (System.in);
    System.out.println("Nome do usuário: ");
    String nome = s.nextLine();
    System.out.println("Password: ");
    String pass = y.nextLine();    
    if(p.login(nome,pass)) {
    this.coduser= p.coduserlogin(nome,pass);
    System.out.println(coduser);
    System.out.println("Login feito com sucesso");
    fazencomenda();
    
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
     new Menu();
    }
    }    
    }
    
    public void regista() throws IOException,InterruptedException{ //efetuar o registo
    Parse p = new Parse();
    p.parse();       
    Scanner s = new Scanner (System.in);       
    System.out.println("Deseja registar um:\n1)Usuario\n2)Voluntario\n3)Transportadora\n4)Loja");
    String reg=s.nextLine();
    switch(reg){
    case("1"):
    registaUsuario();
    break;
    case("2"):
    registaVoluntario();
    break;
    case("3"):
    registaTransportadora();
    break;
    case("4"): 
    registaLoja();
    break;
    
    default: 
    System.out.println("Opcao invalida ");
    new Menu();
    break;        
    }
    }
    
    //registos especificos
    public void registaUsuario()throws IOException{
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
    Coordenadas coord = new Coordenadas(x,y);
    p.adduser(nome,coord,pass);       
    }
    
    public void registaVoluntario() throws IOException{
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
    String meds = r.nextLine();
    switch(meds){
     case("1"): 
     p.addvolmed(vol,c,raio,pass);
     break; 
     case("2"):
     p.addvol(vol,c,raio,pass);
     break; 
     default: 
     System.out.println("Opcao desconhecida "); 
     break;
    }
     
    }
    
    public void registaTransportadora() throws IOException{ // registar as empresas
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
    Coordenadas coord = new Coordenadas(aa,bb);
    System.out.println("Raio");
    double raio = y.nextDouble();
    System.out.println("Nif");
    int nif = y.nextInt();
    System.out.println("Custo por km");
    double custo = y.nextDouble();
    p.addtrans(nome,coord,nif,raio,custo,pass);   
    }
    
    public void registaLoja() throws IOException{
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
    Coordenadas coord = new Coordenadas(x,y);
    p.addloja(nome, coord,pass);      
    }
    //classificacoes
    public void verclassificaçao()throws IOException,InterruptedException { // ver avaliacoes
    Parse p = new Parse();
    p.parse();
    Scanner s = new Scanner(System.in); 
    System.out.println("Classificacoes de:\n1)Lojas\n2)Voluntarios\n3)Empresas");    
    String opçoes = s.nextLine(); 
    switch(opçoes){
    case("1"):      
    System.out.println("Lojas disponiveis: ");
    p.lojas();
    System.out.println("Nome da loja: "); 
    String loja = s.nextLine(); 
    verclassificaçaoLoja(loja);
    break; 
    case("2"): 
    System.out.println("Voluntarios: ");
    p.voluntarios();
    System.out.println("Nome do voluntario: "); 
    String vol = s.nextLine(); 
    verclassificaçaoVoluntario(vol);
    break; 
    case("3"): 
    System.out.println("Empresas: ");
    p.empresas();
    System.out.println("Nome da empresa: "); 
    String emp = s.nextLine(); 
    verclassificaçaoEmpresa(emp);
    break; 
    default: 
    new Menu(); 
    }
    }
    
    public void verclassificaçaoLoja(String nome)throws IOException,InterruptedException{ // ver avaliacoes das lojas
    Parse p = new Parse();
    p.parse(); 
    Loja e = p.identifica(nome); 
    System.out.println("Media das avaliacoes: " + e.media());
    new Menu();
    }
    
    public void verclassificaçaoVoluntario(String nome)throws IOException,InterruptedException{ // ver avaliacoes dos voluntarios
    Parse p = new Parse();
    p.parse();
    Voluntario e = p.identificaVoluntario(nome); 
    System.out.println("Media das avaliacoes: " + e.media());
    new Menu();
    }
    
    public void verclassificaçaoEmpresa(String nome)throws IOException,InterruptedException{ // ver avaliacoes das empresas
    Parse p = new Parse();
    p.parse();
    Empresa e = p.identificaEmpresa(nome); 
    System.out.println("Media das avaliacoes: " + e.media());
    new Menu();
    }
    
    public void avaliaVoluntario(int av,Voluntario e)  throws IOException, InterruptedException{
    e.addAvaliacao(av);
    }
    
    public void avaliaLoja(int av,Loja e)  throws IOException, InterruptedException{         //adicionar avaliacoes
    e.addAvaliacao(av);
    }
    
    public void avaliaEmpresa(int av,Empresa e)  throws IOException, InterruptedException{
    e.addAvaliacao(av);
    }
    
    //fazer encomenda
    public void fazencomenda() throws IOException,InterruptedException{
    Parse p = new Parse();
    p.parse();
    selecionaloja();
    this.ret.add(pedir());
    ciclo();
    }
    
    public List<Queue> h() throws IOException{
    List<Queue> func = new ArrayList<>();
    func.add(pedir()); 
    return func; 
    }
    
    public Queue pedir() throws IOException{
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
    
    public Queue addpedido(String desc,int quant)throws IOException{
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
    
    public void selecionaloja() throws IOException,InterruptedException{ // escolher a loja 
    Parse p = new Parse();
    p.parse();     
    Scanner s = new Scanner(System.in); 
    System.out.println("Lojas disponiveis: ");
    p.lojas();
    System.out.println("Qual o nome da loja pretendida?"); 
    String loja = s.nextLine();
    if(p.existeloja(loja)){
    Loja e = p.identifica(loja); 
    this.codloja = p.codigoLojanome(loja);
    }
    else {System.out.println("Loja indisponivel "); 
         selecionaloja();
    }
    }
    
    //Próximas 3 funções relativas á entrega ou por parte de um voluntário ou por uma empresa transportador
    
    public void quementrega()throws IOException,InterruptedException{ // escolha de quem entrega
    Parse p = new Parse(); 
    p.parse();
    Pedido c = new Pedido(); 
    Usuário k = new Usuário(); 
    Coordenadas corduser = new Coordenadas(); 
    Scanner s = new Scanner(System.in); 
    String escolha = new String(); 
    System.out.println("Entrega por:\n1)Voluntario\n2)Transportadora");    
    String escolher = s.nextLine(); 
    switch(escolher){
    case("1"): 
    entregavol();
    break; 
    case("2"): 
    entregaemp();
    break;
    default: 
    System.out.println("Opcao desconhecida ");
    quementrega(); 
    break; 
    }
    }
    
    public void entregavol() throws IOException, InterruptedException{ // entrega por parte do voluntario
    Parse p = new Parse();
    p.parse();
    System.out.println("A preparar encomenda!"); 
    p.wait(false, this.codloja);
    Pedido c = p.criapedido2(ret,coduser,codloja);
    p.criapedido(ret,coduser,codloja);
    calculapreco(c);
    Thread.sleep(4000);
    p.wait(true, this.codloja);
    System.out.println("Encomenda pronta!");
    System.out.println("A escolher voluntario: "); 
    Voluntario h = p.primeiro(this.codloja);
    if(p.primeiro(this.codloja) == null){
    System.out.println("Nao existem voluntarios disponiveis de momento :(\n1)Entrega por transportadora\n2)Tentar mais tarde");
    Scanner s = new Scanner (System.in); 
    String ups = s.nextLine(); 
    switch(ups){
     case("1"):
     entregaemp(); 
     break;
     case("2"):
     new Menu(); 
     break; 
     default: 
     System.out.println("Opcao invalida "); 
     new Menu();
     break;        
    }
    }
    System.out.println(h);
    String codvol = h.getcodigoVoluntario();
    p.identificaVoluntario(codvol);
    System.out.println("Voluntario a caminho");
    Usuário k = p.identificaUsuario(coduser);
    Coordenadas corduser = k.getgps(); 
    Long tempo = p.tempoVol(k,h);
    Thread.sleep(tempo); 
    System.out.println("Encomenda entregue!");
    Voluntario f = p.identificaVoluntario(codvol);
    f.setgps(corduser);
    p.setdispvol(f);   
    System.out.println("Deseja avaliar?:\n1)Loja\n2)Voluntario\n3)Sair"); //depois da entrega ocorre a avaliacao
    Scanner b = new Scanner(System.in); 
    String aval = b.nextLine();
    switch(aval){
     case("1"): 
     Loja o = p.identifica(this.codloja); 
     Scanner t = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int avaliacao = t.nextInt(); 
     avaliaLoja(avaliacao,o);
     new Menu(); 
     break; 
     case("2"): 
     Scanner w = new Scanner (System.in); 
     System.out.println("Insira a avalicao"); 
     int avaliacao2 = w.nextInt(); 
     avaliaVoluntario(avaliacao2,f); 
     new Menu(); 
     break; 
     case("3"):
     System.exit(0); 
     break;
     default: 
     new Menu(); 
    }
    }
    
    public void entregaemp() throws IOException, InterruptedException{ // entrega por parte da empresa
    Parse p = new Parse();
    p.parse();
    System.out.println("A preparar encomenda!"); 
    p.wait(false, this.codloja);
    Pedido c = p.criapedido2(ret,coduser,codloja);
    p.criapedido(ret,coduser,codloja);
    System.out.println("Preco antes do transporte");
    calculapreco(c);
    Thread.sleep(4000);
    p.wait(true, this.codloja);
    System.out.println("Encomenda pronta!");
    System.out.println("A escolher empresa: "); 
    Empresa j = p.selecionaempresa(this.codloja);
    System.out.println("Preco com transporte: "+p.calculaprecoemp(c,j));
    System.out.println(j);
    String nomeemp = j.getnome();
    p.identificaEmpresa(nomeemp);
    System.out.println("Transportadora a caminho!");
    Usuário w = p.identificaUsuario(coduser);
    Coordenadas corduser = w.getgps(); 
    Long tempo2 = p.tempoEmp(w,j);
    Thread.sleep(tempo2); 
    System.out.println("Encomenda entregue!");
    j.setgps(corduser);
    p.setdispemp(j);
    System.out.println("Deseja avaliar?:\n1)Loja\n2)Transportadora\n3)Sair");
    Scanner bb = new Scanner(System.in); 
    String aval2 = bb.nextLine(); 
    switch(aval2){
     case("1"): 
     Loja o = p.identifica(this.codloja); 
     Scanner t = new Scanner (System.in); 
     System.out.println("Insira a avalicao:"); 
     int avaliacao = t.nextInt(); 
     avaliaLoja(avaliacao,o);
     new Menu(); 
     break; 
     case("2"): 
     Scanner ww = new Scanner (System.in); 
     System.out.println("Insira a avalicao:"); 
     int avaliacao2 = ww.nextInt(); 
     avaliaEmpresa(avaliacao2,j); 
     new Menu(); 
     break; 
     case("3"):
     System.exit(0); 
     break;
     default: 
     new Menu(); 
    }     
    }
    
    
    public void ciclo() throws IOException,InterruptedException{ // para termos mais do que um produto
    Parse p = new Parse();
    System.out.println("Quer adicionar mais produtos:\n1)Sim\n2)Nao"); 
    Scanner e = new Scanner(System.in);
    String escolha = e.nextLine(); 
    switch(escolha){
    case("1"): 
    this.ret.add(pedir());
    ciclo();
    break; 
    case("2"):
    quementrega();
    break;
    default:
    System.out.println("Opcao invalida :(");
    new Menu();    
    }
    }
    
    public void calculapreco(Pedido e) throws IOException{
    Parse p = new Parse(); 
    p.parse(); 
    System.out.println("Preco a pagar: " + p.calculapreco(e));
    }
    
    
}