import java.util.*;
import java.util.stream.Collectors;
import java.time.LocalDate;
import java.util.Comparator;
import java.lang.String;
import java.util.HashSet;
import java.util.Scanner;
import java.util.ArrayList;
import java.io.BufferedWriter;
import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.FileReader;
import java.util.HashMap;
import java.lang.Object;
import java.nio.charset.StandardCharsets;
import java.time.DateTimeException;
import java.nio.file.*;
import java.awt.Desktop;
import java.time.LocalDate;
import java.nio.file.*;
import java.io.*;


public class main
{  
    //ainda nao criadas.
    private HashMap<String,User> users; // codigo , user 
    private HashMap<String,Encomenda> Encomendas; // codigoenc , encomenda 
    
    public main (){
        this.users= new HashMap<String,User>(); // codigo , user 
        this.Encomendas= new HashMap<String,Encomenda> ();
    }
    //Utilizador:<CodUtilizador>,<Nome>,<GPS>
    private static Buyer readBuyerLOG(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String username = b[1];
        double lx = Double.parseDouble(b[2]);
        double ly = Double.parseDouble(b[3]);
        Buyer B = new Buyer (username,codigo,codigo,lx,ly,codigo);
        return B;
    }
        
    //Loja:<CodLoja>, <NomeLoja>,<GPS>
    private static Loja readLojaLOG(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String username = b[1];
        double lx = Double.parseDouble(b[2]);
        double ly = Double.parseDouble(b[3]);
        //ArrayList queue = new ArrayList<>();
        Loja loja = new Loja(username,codigo,codigo,lx,ly,codigo);
        return loja;
    }
        
   
    //Voluntario:<CodVoluntÃ¡rio>, <Nome>,<GPS> ,<Raio>Â 
    private static Voluntario readVoluntarioLOG(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String username = b[1];
        double lx = Double.parseDouble(b[2]);
        double ly = Double.parseDouble(b[3]);
        double raio = Double.parseDouble(b[4]);
        Voluntario v = new Voluntario(username,codigo,codigo,lx,ly,codigo, raio,true,0,0);
        return v;
    }
        
    //Transportadora:<CodEmpresa>,<NomeEmpresa>,<GPS>,<NIF>,<raio>,<preco-por-km>
    private static Transportadora readTransporteLOG(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String username = b[1];
        double lx = Double.parseDouble(b[2]);
        double ly = Double.parseDouble(b[3]);
        int nif = Integer.parseInt(b[4]);
        double raio = Double.parseDouble(b[5]);
        double custokm = Double.parseDouble(b[6]);
        Transportadora  e = new Transportadora(username,codigo,codigo,lx,ly,codigo, custokm , raio ,nif , 0, 0);
        return e;
    }
        
    //<CodEncomenda>, <CodUtilizador>, <CodLoja>, <Peso>, <LinhaEncomenda>+
    private static Encomenda readEncomendaLOG(String read){
        int i=4;
        String[] b = read.split(",");
        String codenc = b[0];
        String codbuyer = b[1];
        String codloja = b[2];
        //String coddeliever = b[3];
        double peso = Double.parseDouble(b[3]);
        //LocalDate data = LocalDate.parse(b[5]);
        //int state = Integer.parseInt(b[6]);
        ArrayList<LinhaEncomenda> produto = new ArrayList<>();
        
        //<CodProduto>, <DescriÃ§Ã£o>, <Quantidade>, <ValorUnitÃ¡rio>
        while(i<(b.length)){
             
            String codlinha = b[i];
            String descricao = b[i+1];
            double preco = Double.parseDouble(b[i+3]);
            double quantidade = Double.parseDouble(b[i+2]);
            LinhaEncomenda l = new LinhaEncomenda(codlinha,descricao,preco,quantidade);
            
            produto.add(l);
            i+=4;
            
        }
        Encomenda  e = new Encomenda(codenc,codbuyer,codloja,"", peso , null , 2 , produto);
        return e;
    }
    
    public void writeFile2(String nomeFicheiro) throws FileNotFoundException,IOException{
        PrintWriter pw = new PrintWriter(nomeFicheiro);
           
        for(User u : this.users.values()){
                String s = u.stringtoFile();
                pw.println(s);
        }
        
        for(Encomenda e : this.Encomendas.values()){
            String s = e.stringtoFile();
            pw.println(s);
        }
        
        pw.flush();
        pw.close();
    }
    
    public User getUserByEmail (String email){
        User x = null ; 
        for(User a : users.values()){
            if(a.getEmail().equals(email)){
                return a.clone();
            }
            
    }
    return null ; 
    }
    
    public ArrayList<Encomenda> getEncomendasbyCodigoUser(String codigo){
        ArrayList<Encomenda> lista = new ArrayList<>();
        
        for(Encomenda e : this.Encomendas.values()){
            if(e.getBuyer().equals(codigo)){
                lista.add(e.clone());
            }
        }
        return lista;
    }
    //retorna lista com todas as lojas 
    public ArrayList<Loja> Tlojas(){
        ArrayList<Loja> r = new ArrayList<>();
        
        for(User u : this.users.values()){
            if(u.getClass().getName().equals("Loja")){
                r.add((Loja) u.clone());
            }
        }
        return r;
    }
    //retorna lista com todas as Transportadoras 
    public ArrayList<Transportadora> Ttransportadoras(){
        ArrayList<Transportadora> r = new ArrayList<>();
        
        for(User u : this.users.values()){
            if(u.getClass().getName().equals("Transportadora")){
                r.add((Transportadora) u.clone());
            }
        }
        return r;
    }
    //retorna lista com todas os voluntarios 
    public ArrayList<Voluntario> Tvoluntarios(){
        ArrayList<Voluntario> r = new ArrayList<>();
        
        for(User u : this.users.values()){
            if(u.getClass().getName().equals("Voluntario")){
                r.add((Voluntario) u.clone());
            }
        }
        return r;
    }
    //vai buscar o user pelo codigo 
    public User getUserByCodigo (String codigo){
        User a = null ; 
        if(this.users.containsKey(codigo)){
              a = users.get(codigo).clone();
            }
        return a ;    
    }
    

    public void mudaestadoVoluntario(User a){
        Voluntario b=(Voluntario) a; 
        if(b.getlivre()){b.setlivre(false);System.out.println("Voluntario está agora Ocupado");}
        else{b.setlivre(true);System.out.println("Voluntario está agora Livre");}
    
    
    }
    
    //verificar se codigo existe 
    public boolean checkCodigo (String codigo){
        boolean a = true ; 
        if(this.users.containsKey(codigo)){
              return false ;
            }
        return a ;    
    }
    //adiciona buyer
    public int addBuy(Buyer u){
        if(this.users.containsValue(u)) return -1;
        else{
            this.users.put(u.getCodigo(),u.clone());
            return 1;
        }
    }
    //adiciona voluntario 
    public int addVol(Voluntario u){
        if(this.users.containsValue(u)) return -1;
        else{
            this.users.put(u.getCodigo(),u.clone());
            return 1;
        }
    }
    
    public int addLoja (Loja u){
        if(this.users.containsValue(u)) return -1;
        else{
            this.users.put(u.getCodigo(),u.clone());
            return 1;
        }
    }
    
    public int addTrans(Transportadora u){
        if(this.users.containsValue(u)) return -1;
        else{
            this.users.put(u.getCodigo(),u.clone());
            return 1;
        }
    }
    
    public int addEnc(Encomenda e){
        if(this.Encomendas.containsValue(e)) return -1;
        else{
           Encomenda enc = e.clone();
           this.Encomendas.put(enc.getcodEncomenda(),enc);
           return 1;
        }
    }
    
    public static  main parse (String file, int tipo){
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(file), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        main b = new main();
        String[] s;
        //int i=0;
        for (String line : lines) {
             s = line.split(":", 2); 
             //System.out.println(i);
             //i+=1;
            if(s[0].equals("Utilizador")){
                try{
                    if(tipo==1){
                    Buyer u = readBuyerLOG(s[1]);
                    
                    b.addBuy(u);}
                    else{
                    Buyer u = readBuyer(s[1]);
                    b.addBuy(u);
                
                }
                    
                    
                }
                catch(NullPointerException e){
                    
                }
            }
            
            if(s[0].equals("Loja")){
                try{
                    if(tipo==1){
                    Loja l = readLojaLOG(s[1]);
                    b.addLoja(l);}else{Loja l = readLoja(s[1]);b.addLoja(l); }
                }
                catch(NullPointerException e){
                    }
            }
          
            if(s[0].equals("Voluntario")){
                 try{
                     if(tipo==1){
                     Voluntario v = readVoluntarioLOG(s[1]);
                     
                     b.addVol(v);}
                     else{Voluntario v = readVoluntario(s[1]);b.addVol(v);}
                 }
                 catch(NullPointerException e){
                    }
            }
          
            if(s[0].equals("Transportadora")){
                 try{if(tipo==1){
                     Transportadora t = readTransporteLOG(s[1]);
                     
                     b.addTrans(t);}
                     else{
                         Transportadora t = readTransporte(s[1]);
                         b.addTrans(t);
                        }
                    }
                 catch(NullPointerException e){
                    }
            }
          
            if(s[0].equals("Encomenda")){
                try{if(tipo==1){
                    Encomenda e = readEncomendaLOG(s[1]);
                    
                    b.addEnc(e);}
                    else{
                        Encomenda e = readEncomenda(s[1]);
                        b.addEnc(e);
                    }
                }
                catch(NullPointerException e){
                }
            }
            
            if(s[0].equals("Aceite")){
            try{
                    String a = s[1];
                    b.mudaEstado(a);

                }
                catch(NullPointerException e){
                }
            }
          
        }
    
        return b;
    
    }
    //muda estado de encomenda dps de ler aceite para pronta para ser recolhida por um voluntario
    public void mudaEstado(String u){
        
        if(this.Encomendas.containsKey(u)){
        Encomenda e = Encomendas.get(u);
        mudaestadoENC(u,3);
        }
        return ; 
    
    }
        
    
    
    //String username , String codigo , String password , double locationX, double locationY, ArrayList encomenda){

    private static Buyer readBuyer(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String password = b[1];
        String username = b[2];
        double lx = Double.parseDouble(b[3]);
        double ly = Double.parseDouble(b[4]);
        String email =b[5];
        Buyer B = new Buyer (username,codigo,password,lx,ly,email);
        return B;
    }
        
    //String username , String codigo , String password , double locationX, double locationY,boolean filadeEspera,ArrayList <Encomenda> queue
    //falta ver queue lembrar --------------------------------
    private static Loja readLoja(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String password = b[1];
        String username = b[2];
        double lx = Double.parseDouble(b[3]);
        double ly = Double.parseDouble(b[4]);
        String email = b[5];
        //ArrayList queue = new ArrayList<>();
        Loja loja = new Loja(username,codigo,password,lx,ly,email);
        return loja;
    }
        
    //String username , String codigo , String password , double locationX, 
    //double locationY, double raio , boolean livre, double numr , double aval)
    private static Voluntario readVoluntario(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String password = b[1];
        String username = b[2];
        double lx = Double.parseDouble(b[3]);
        double ly = Double.parseDouble(b[4]);
        String email = b[5];
        double raio = Double.parseDouble(b[6]);
        boolean livre = Boolean.parseBoolean(b[7]);
        double numreviews = Double.parseDouble(b[8]);
        double avaliacao = Double.parseDouble(b[9]);
        Voluntario v = new Voluntario(username,codigo,password,lx,ly,email, raio,livre,numreviews,avaliacao);
        return v;
    }
        
    //String username , String codigo , String password , double locationX,
   // double locationY, double custoKM, double raio , int nif , double aval , double numrev
    private static Transportadora readTransporte(String read){
        String[] b = read.split(",");
        String codigo = b[0];
        String password = b[1];
        String username = b[2];
        double lx = Double.parseDouble(b[3]);
        double ly = Double.parseDouble(b[4]);
        String email = b[5];
        double raio = Double.parseDouble(b[6]);
        double custokm = Double.parseDouble(b[7]);
        double numrev = Double.parseDouble(b[8]);
        double aval = Double.parseDouble(b[9]);
        int nif = Integer.parseInt(b[10]);
        Transportadora  e = new Transportadora(username,codigo,password,lx,ly,email, custokm , raio ,nif , aval, numrev);
        return e;
    }
        
    //Cria uma encomenda com os dados do ficheiro
    //String codEncomenda, String buyer ,String loja,String transport,double peso, 
    //LocalDate data, int estado,ArrayList<LinhaEncomenda> produto
    private static Encomenda readEncomenda(String read){
        int i=7;
        String[] b = read.split(",");
        String codenc = b[0];
        String codbuyer = b[1];
        String codloja = b[2];
        String coddeliever = b[3];
        double peso = Double.parseDouble(b[4]);
        LocalDate data = parseData(b[5]);
        int state = Integer.parseInt(b[6]);
        ArrayList<LinhaEncomenda> produto = new ArrayList<>();
        
        //String codLinha, String descricao, double preco, double quantidade)
        while(i<(b.length)){
            
            String codlinha = b[i];
            String descricao = b[i+1];
            double preco = Double.parseDouble(b[i+2]);
            double quantidade = Double.parseDouble(b[i+3]);
            LinhaEncomenda l = new LinhaEncomenda(codlinha,descricao,preco,quantidade);
            
            produto.add(l);
            i+=4;
        }
        Encomenda  e = new Encomenda(codenc,codbuyer,codloja,coddeliever, peso , data ,state , produto);
        return e;
    }
    //da parse a data de uma encomenda 
    private static LocalDate parseData(String s){
        LocalDate x = null ;
        try{
        if(s.equals("null")){return x;}
        else{x=LocalDate.parse(s);}}
        catch(DateTimeException e ){
                System.out.println("data incorreta");
                return null;
            }
        return x ;
    }
    //da quantos km percuridos dado uma transportadora funciona para todas as encomendas entregues ate o momento 
    public double KmsPercurridos(Transportadora e){
        double totalKms =0;
        for(Encomenda a : this.Encomendas.values()){
            if( (e.getCodigo().equals(a.getcodDeliver()))    &&  (a.getestado() == 7 || a.getestado() == 1)){
            Buyer b= (Buyer) users.get(a.getBuyer());
            Loja l= (Loja) users.get(a.getLoja()); 
            
            double transtoloja = Math.sqrt(Math.pow(e.locationX - l.getLocationX(), 2) +
                     Math.pow(e.locationY - l.getLocationY(), 2));
            double lojatoBuyer = Math.sqrt(Math.pow(l.getLocationX() - b.getLocationX(), 2) +
                     Math.pow(l.getLocationY() - b.getLocationY(), 2));
            totalKms= totalKms + lojatoBuyer+transtoloja;
        }
        }
        return totalKms; 
    }
    
    
    public ArrayList<Encomenda> EncomendasParaVoluntarioDentroRaio(String codVol){
        ArrayList<Encomenda> r =new ArrayList<Encomenda>(); 
        User v1 = users.get(codVol);
        Voluntario v =(Voluntario) v1;
        for(Encomenda a : this.Encomendas.values()){
        if(a.getestado()==3){
        String codBuyer= a.getBuyer();
        String codLoja = a.getLoja();
        User b = users.get(codBuyer);
        User l = users.get(codLoja);
        Buyer b1 = (Buyer) b;
        Loja l1 = (Loja) l;
        double transtoloja = Math.sqrt(Math.pow(v.locationX - l1.getLocationX(), 2) +
                     Math.pow(v.locationY - l1.getLocationY(), 2));
        double transtobuyer =Math.sqrt(Math.pow(v.locationX - b1.getLocationX(), 2) +
                     Math.pow(v.locationY - b1.getLocationY(), 2));
        if(transtoloja<= v.getraio() && transtobuyer<= v.getraio()){r.add(a);}}
        }
        return r ;
    }
    
    //retorna 1 se transportadora 2 se voluntario 0 se nenhum dos dois 
    public int TransportadoraOuVoluntario(String codigoenc){
        int i=0;
        if(Encomendas.containsKey(codigoenc)){
        Encomenda t = Encomendas.get(codigoenc);
        
        String a=t.getcodDeliver();
        User u = users.get(a);
        if (u.getClass().getName().equals("Transportadora")){return 1;}
        else if(u.getClass().getName().equals("Voluntario")){return 2;}
            }
    
        return i ;
    }
    //retorna o codigo da transportadaora mais barata para uma certa distancia e peso
    public String TransportadoraMaisBarata(String codBuyer,String codLoja, double peso){
        String codigo = "n";
        User b = users.get(codBuyer);
        User l = users.get(codLoja);
        Buyer b1 = (Buyer) b;
        Loja l1 = (Loja) l;
        double precoF=99999999;
        double precoi=99999999;
        ArrayList<Transportadora> r =Ttransportadoras();
        for(Transportadora t1 : r){ 
        double transtoloja = Math.sqrt(Math.pow(t1.locationX - l1.getLocationX(), 2) +
                     Math.pow(t1.locationY - l1.getLocationY(), 2));
        double transtobuyer =Math.sqrt(Math.pow(t1.locationX - b1.getLocationX(), 2) +
                     Math.pow(t1.locationY - b1.getLocationY(), 2));
        if(transtoloja<= t1.getraio() && transtobuyer<= t1.getraio()){
            precoi = custoTransporte1(codBuyer, t1.getCodigo(),codLoja,peso );
        }
        
        if(precoi<precoF){codigo =t1.getCodigo();precoF=precoi;  }
        
        }
        
        return codigo;
    

    }
    
    
    public  double custoTransporte1 (String codBuyer , String codTransp,String codLoja, double peso ){
        User t = users.get(codTransp);
        User b = users.get(codBuyer);
        User l = users.get(codLoja);
        Buyer b1 = (Buyer) b;
        Loja l1 = (Loja) l;
        Transportadora t1 = (Transportadora) t;
        double preco=0;
        double transtoloja = Math.sqrt(Math.pow(t1.locationX - l1.getLocationX(), 2) +
                     Math.pow(t1.locationY - l1.getLocationY(), 2));
           double lojatoBuyer = Math.sqrt(Math.pow(l1.getLocationX() - b1.getLocationX(), 2) +
                     Math.pow(l1.getLocationY() - b1.getLocationY(), 2));
           preco=peso*t1.gettaxapeso()+(transtoloja+lojatoBuyer)*t1.getcustoKM();
        
        return preco; 
    }
    
    //custo do transporte de uma dada encomenda por uma transportadora
    public  double custoTransporte(String codEncomenda , String codTransp){
       double preco=0;
       if(this.Encomendas.containsKey(codEncomenda) ){
           User u = users.get(codTransp);
           Transportadora e = (Transportadora) u;
           Encomenda a = Encomendas.get(codEncomenda);
           
           Buyer b = (Buyer) users.get(a.getBuyer());
           Loja l = (Loja) users.get(a.getLoja());
           
           double transtoloja = Math.sqrt(Math.pow(e.locationX - l.getLocationX(), 2) +
                     Math.pow(e.locationY - l.getLocationY(), 2));
           double lojatoBuyer = Math.sqrt(Math.pow(l.getLocationX() - b.getLocationX(), 2) +
                     Math.pow(l.getLocationY() - b.getLocationY(), 2));
           preco=a.getpeso()*e.gettaxapeso()+(transtoloja+lojatoBuyer)*e.getcustoKM();
        
        }
  
       return preco;
    }
    // devolve top 10 empresas com mais km feitos
    public void top10Empresas (){
        List<Transportadora> top = new ArrayList<Transportadora>();
        Comparator<Transportadora> bykm = (Transportadora emp1, Transportadora emp2) ->{
        return Double.compare(KmsPercurridos(emp1), KmsPercurridos(emp2)); };
        
        for ( User a : users.values()){
            if (a.getClass().getName().equals("Transportadora")){
                Transportadora b =(Transportadora) a; 
                top.add(b.clone());
            } 
        }
        Collections.sort(top,bykm); 
        System.out.println("Top 10 Companies:"); 
        int n = top.size();
        int i=1;
        if(n>10){n=10;}
        for(Transportadora a : top){
            if(i<=n){ System.out.println(i+"º:"+ a.getUserName());i++;
            }
            else{break;}
  
        }
    }
   
    //numero de encomendas que um buyer fez 
    public int getNumencomendas(Buyer b){
        int i =0;
        for(Encomenda a : this.Encomendas.values()){
           if(b.equals(a.getBuyer())){
           i=i+1;
        }
      }
       return i ; 
    }

   //top 10 users que mais encomendas fizeram 
    public void top10Users(){
        List<Buyer> top = new ArrayList<Buyer>();
        Comparator<Buyer> byEnc = (Buyer emp1, Buyer emp2) ->{
        return Double.compare(getNumencomendas(emp1), getNumencomendas(emp2)); };
        for ( User a : users.values()){
            if (a.getClass().getName().equals("Buyer")){
                Buyer b =(Buyer) a; 
                top.add(b.clone());
            } 
        }
        Collections.sort(top,byEnc); 
        System.out.println("Top 10 Users:"); 
        int n = top.size();
        int i=1;
        if(n>10){n=10;}
        for(Buyer a : top){
            if(i<=n){ System.out.println(i+"º:"+ a.getUserName());i++;
            }
            else{break;}
  
        }
    }
    //quanto uma empresa fez num dado periodo de tempo menu 
    public void faturadoEmp(){
        double total =0 ;
        LocalDate d1= null;
        LocalDate d2= null ;
        int nif = 0;
        System.out.println("Insert companie's NIF:");
        Scanner n = new Scanner(System.in);
        nif = n.nextInt();
        System.out.println("Insira a data de incio, irá ter de insirir da forma ano-mes-dia");
        Scanner day1 = new Scanner(System.in);
        String dayI = day1.nextLine();
        
        try{d1=LocalDate.parse(dayI);
        }catch (DateTimeException e){
            System.out.println("data incorreta");
            return; 
        }
        System.out.println("Insira a data de incio, irá ter de insirir da forma ano-mes-dia");
        Scanner day2 = new Scanner(System.in);
        String dayF = day1.nextLine();
        
        
        try{d2=LocalDate.parse(dayF);
        }catch (DateTimeException e){
            System.out.println("data incorreta");
            return ; 
        }
        
        for( User a : users.values()){
            if(a.getClass().getName().equals("Transportadora")){
            Transportadora b =(Transportadora) a; 
            if(b.getnif()==nif){
                total = fatura(b,d1,d2);
                System.out.println("A empresa faturou:"+total);
            }
        }
        }
        
        
    }
    //quanto uma empresa faturou num dado periodo de tempo.
    public double fatura (Transportadora e ,LocalDate d1, LocalDate d2){
    double total =0; 
    for(Encomenda a : this.Encomendas.values()){
        if(e.getCodigo().equals(a.getcodDeliver()) && a.getData().isAfter(d1) && a.getData().isBefore(d2)  ){
        total = total + custoTransporte(a.getcodEncomenda(),e.getCodigo());
        
        }  
    
    }
    return total; 
    }
    
    
    //da print ao historico de encomendas do transportador 
    public void PrintHistT(String codigo){ 
        if(this.users.containsKey(codigo)){
             User e = users.get(codigo);
            if (e.getClass().getName().equals("Transportadora")){
        for(Encomenda a : this.Encomendas.values()){
           if(e.getCodigo().equals(a.getcodDeliver())){
            String buyer= users.get(a.getBuyer()).getUserName();
            String store= users.get(a.getLoja()).getUserName();
            System.out.println("Buyer: " + buyer +",Store: " + store );
        }}}}
    }
    //da print ao historico de encomendas do voluntario
    public void PrintHistV(String codigo){
        if(this.users.containsKey(codigo)){
             User e = users.get(codigo);
            if (e.getClass().getName().equals("Voluntario")){
        for(Encomenda a : this.Encomendas.values()){
            if(e.getCodigo().equals(a.getcodDeliver())){
            String buyer= users.get(a.getBuyer()).getUserName();
            String store= users.get(a.getLoja()).getUserName();
            System.out.println("Buyer: " + buyer +",Store: " + store );}
        } }}
    }
    // confirma se o email e a pass estao no sistema 
    public boolean LoginInfo(String email ,String pass){
        for ( User a : users.values()){
           if(a.getEmail().equals(email)){return a.getPassword().equals(pass);}
        }
        return false;
    }
    //retorna lista com todas encomendas entregues a um user sem review 
    public ArrayList EncomendasEntreguesSemReviews(String e){
         ArrayList<Encomenda> lista = getEncomendasbyCodigoUser(e);
         ArrayList<Encomenda> listaf= new ArrayList<>();
         for(Encomenda a : lista){
             if(a.getestado() == 1){
                listaf.add(a);}
            }
            return listaf; 

    }
    // dado uma encomenda entregue  sem review avaliar a transportadora ou voluntario
    public boolean reviews(String codigoEnc,double review){
        boolean works =false ; 
        if(this.Encomendas.containsKey(codigoEnc)){
        Encomenda a= Encomendas.get(codigoEnc); 
        String cod = a.getcodDeliver();
        User u = users.get(cod);
        if (u.getClass().getName().equals("Transportadora")){ReviewTransportadora(cod,review);works =true;}
        else if(a.getClass().getName().equals("Voluntario")){ ReviewVoluntario(cod,review);works =true;}
        }
        return works;
    }
    
    
    
    public void ReviewTransportadora(String codigo , double review){
        if(this.users.containsKey(codigo)){
             User a = users.get(codigo);
            if (a.getClass().getName().equals("Transportadora")){
                Transportadora e = (Transportadora) a;
                double aval= e.getavaliacao();
                double numrev = e.getnumreviews();
                double nreview = (aval*numrev+review)/(numrev+1);
                e.setavaliacao(nreview);
                e.setnumreviews(numrev+1);
                
            }
        }
    }
        
    public void ReviewVoluntario( String codigo , double review){
        if(this.users.containsKey(codigo)){
             User a = users.get(codigo);
            if (a.getClass().getName().equals("Voluntario")){
                Voluntario e = (Voluntario) a;
                double aval= e.getavaliacao();
                double numrev = e.getnumreviews();
                double nreview = (aval*numrev+review)/(numrev+1);
                e.setavaliacao(nreview);
                e.setnumreviews(numrev+1);}
        }
    }
    //retorna se tiver o codigo de uma encomenda atual 
    public String EncomendaAtualV( String codvol ){
        User u = users.get(codvol);
        Voluntario e = (Voluntario) u ;
        String resultado = "free";
        for(Encomenda a : this.Encomendas.values()){
            if(e.getCodigo().equals(a.getcodDeliver())){
            if(a.getestado()==6){resultado =a.getcodEncomenda();}
            
            }

        }
    
    
        return resultado ; 
   }
   //muda o estado de uma encomenda 
   public void mudaestadoENC(String codigoenc, int estado ){
       if(this.Encomendas.containsKey(codigoenc) ){
           Encomenda e = Encomendas.get(codigoenc);
           e.setestado(estado);

       }

    }
    
   //voluntario escolhe se aceita ou nao encomenda 
   public void recebeEnc(String codigoenc , String codvol ){
        
        if(this.Encomendas.containsKey(codigoenc) && this.users.containsKey(codvol)){
            User a = users.get(codvol);
            if (a.getClass().getName().equals("Voluntario")){
            Encomenda e = Encomendas.get(codigoenc);
            Voluntario v = (Voluntario) a;
            System.out.println("Do you accept the order (yes, no):");
            a.toString();
            int breakp=0;
            Scanner myObj = new Scanner(System.in);
        
        while( breakp ==0){ 
         String res = myObj.nextLine().toLowerCase();
         if (res.equals("yes")){v.setlivre(false);e.setestado(6); 
              }
         else if(res.equals("no")){breakp =1;}
         else {System.out.println("Invalid Input, answer yes or no : ");}
        }
        return ; 
        }}}
   

   //Voluntarios livres
   
   public ArrayList<Voluntario> Vlivre(){
       ArrayList<Voluntario> x = new ArrayList<Voluntario>();
       
       ArrayList<Voluntario> r = Tvoluntarios();
       
       for(Voluntario a : r){
           if(a.getlivre()){
               
               x.add(a);
            
            }
        
        }
    
       return x ;
   }
   
  public String getVolLivre(String codEnc){
       ArrayList<Voluntario> x = Vlivre();
       Encomenda e = Encomendas.get(codEnc);
       String codBuyer = e.getBuyer();
       String codLoja = e.getLoja();
       User b = users.get(codBuyer);
       User l = users.get(codLoja);
       String bla = "n";
       for(Voluntario vl : x){
        double transtoloja = Math.sqrt(Math.pow(vl.locationX - l.getLocationX(), 2) +
                     Math.pow(vl.locationY - vl.getLocationY(), 2));
        double transtobuyer =Math.sqrt(Math.pow(vl.locationX - b.getLocationX(), 2) +
                     Math.pow(vl.locationY - b.getLocationY(), 2));
        if(transtoloja<= vl.getraio() && transtobuyer<= vl.getraio()){
            bla=vl.getCodigo();
            break;
        }
           
        
      }
      return bla;
   
   
   
   }
   
  public String EncPedidas ( String codv){
       for(Encomenda e : this.Encomendas.values()){
           if(e.getestado()==8 && e.getcodDeliver().equals(codv)){
               return e.getcodEncomenda();
            }
           
    }
    return "n";
  }
  public void setDelieverV ( String codv, String codenc ){
      
      Encomenda e = Encomendas.get(codenc);
      e.setcodDeliver(codv);
      
  
    }
}
   