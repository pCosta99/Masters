
/**
 * Escreva a descrição da classe Parse aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.util.*;
import java.io.*;
import java.nio.file.*;
import java.nio.charset.*;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.time.*;
import java.text.*;
import java.util.stream.Collectors;
public class Parse
{
  
  private RandomCoordenates r = new RandomCoordenates();
  private List<Utilizador> u = new ArrayList<>();
  private List<Lojas> l = new ArrayList<>();
  private List<Transportadoras> t = new ArrayList<>();
  private List<Voluntarios> v = new ArrayList<>();
  private List<EncomendasAceites> ea = new ArrayList<>();
  private List<Encomenda> e = new ArrayList<>();
  private List<EncomendaMedica> em = new ArrayList<>();
  
  public List<Transportadoras> UT(){
        Comparator<Transportadoras> c = Comparator.comparing( x -> x.getKMPQ());
        return this.getAT().stream()
                    .sorted(c.reversed())
                    .limit(10)
                    .map(Transportadoras::clone)
                    .collect(Collectors.toList());
                    }
    public List<Utilizador> UU(){
        Comparator<Utilizador> c = Comparator.comparing(x -> x.AllEncomendasEntreguesUtilizador(this));
        return this.getAU().stream()
                         .sorted(c.reversed())
                         .limit(10)
                         .map(Utilizador::clone)
                         .collect(Collectors.toList());
                        }
  
  //Gets
  
  public List<Utilizador> getAU(){
      return this.u.stream().map(Utilizador::clone).collect(Collectors.toList());
    }
    
  public List<Lojas> getAL(){
      return this.l.stream().map(Lojas::clone).collect(Collectors.toList());
    }
    
  public List<Transportadoras> getAT(){
      return this.t.stream().map(Transportadoras::clone).collect(Collectors.toList());
    }
    
  public List<Voluntarios> getAV(){
      return this.v.stream().map(Voluntarios::clone).collect(Collectors.toList());
    }
    
  public List<EncomendasAceites> getAEA(){
      return this.ea.stream().map(EncomendasAceites::clone).collect(Collectors.toList());
    }
    
  public List<Encomenda> getAE(){
      return this.e.stream().map(Encomenda::clone).collect(Collectors.toList());
    }
  
  public List<EncomendaMedica> getAEM(){
      return this.em.stream().map(EncomendaMedica::clone).collect(Collectors.toList());
    }
  
    
  //Sets  
    
  public void setAU(List<Utilizador> au){
    this.u=au.stream().map(Utilizador::clone).collect(Collectors.toList());
    }
    
  public void setAL(List<Lojas> al){
    this.l=al.stream().map(Lojas::clone).collect(Collectors.toList());
    }
  
  public void setAT(List<Transportadoras> at){
    this.t=at.stream().map(Transportadoras::clone).collect(Collectors.toList());
    }
    
  public void setAEA(List<EncomendasAceites> aea){
    this.ea=aea.stream().map(EncomendasAceites::clone).collect(Collectors.toList());
    }
    
  public void setAE(List<Encomenda> ae){
    this.e=ae.stream().map(Encomenda::clone).collect(Collectors.toList());
    }
  
  public void setAEM(List<EncomendaMedica> ae){
    this.em=ae.stream().map(EncomendaMedica::clone).collect(Collectors.toList());
    }
  
    
    
  public void setAV(List<Voluntarios> av){
    this.v=av.stream().map(Voluntarios::clone).collect(Collectors.toList());
    }

//Procura de elementos nos ArrayList 
    
  public Encomenda getEncomenda(String codencomenda) throws EncomendaException{  
    Encomenda encomenda = new Encomenda();
    int a=0;
    
    for (Encomenda enc: this.e){
        if (enc.getCodencomenda().equals(codencomenda)) {encomenda=enc;a=0;break;}
        else a=1;
    }
    
    if (a==0)    return encomenda;
    else throw new EncomendaException("Codigo de Encomenda Errado");
}

public Encomenda getEncomenda1(String codencomenda){
    Encomenda encomenda = new Encomenda();
    for (Encomenda enc: this.e){
        if (enc.getCodencomenda().equals(codencomenda)) {encomenda=enc;break;}
    }
    
    return encomenda;
}

public EncomendaMedica getEncomendaMedica(String codencomenda) throws EncomendaException{
    EncomendaMedica encomenda = new EncomendaMedica();
    int a=0;
    
    for (EncomendaMedica enc: this.em){
        if (enc.getCodencomenda().equals(codencomenda)) {encomenda=enc;a=0;break;}
        else a=1;
    }
    
    if (a==0)    return encomenda;
    else throw new EncomendaException("Codigo de Encomenda Errado");
}

public EncomendaMedica getEncomendaMedica1(String codencomenda){
    EncomendaMedica encomenda = new EncomendaMedica();
    for (EncomendaMedica enc: this.em){
        if (enc.getCodencomenda().equals(codencomenda)) {encomenda=enc;break;}
    }
    
    return encomenda;
}
   
public Transportadoras getTransportadoras(String nome,String codtransportadoras) throws TransportadoraException{
    Transportadoras transportadoras = new Transportadoras();
    int a=0;
    
    for (Transportadoras enc: this.t){
        if (enc.getCodEmp().equals(codtransportadoras) && enc.getNome().equals(nome)) {transportadoras=enc;a=0;break;}
        else a=1;
    }
    
    if (a==0)    return transportadoras;
    else throw new TransportadoraException("Email ou password errados");
}
 
public Transportadoras getTransportadoras(String codtransportadoras) {
    Transportadoras transportadoras = new Transportadoras();
    
    for (Transportadoras enc: this.t){
        if (enc.getCodEmp().equals(codtransportadoras)) {transportadoras=enc;break;}

    }
    
    return transportadoras;
    
}
 
public Voluntarios getVoluntarios(String nome,String codvoluntarios) throws VoluntariosException{
    Voluntarios voluntarios = new Voluntarios();
    int a=0;
    
    for (Voluntarios enc: this.v){
        if (enc.getCodVol().equals(codvoluntarios) && enc.getNome().equals(nome)) {voluntarios=enc;a=0;break;}
        else a=1;
    }
    
    if (a==0)    return voluntarios;
    else throw new VoluntariosException("Email ou password errados");
}
  
public Utilizador getUtilizador(String nome,String codutilizador) throws UtilizadorException{
    Utilizador utilizador = new Utilizador();
    int a =0;
    
    for (Utilizador enc: this.u){
        if (enc.getCodigo().equals(codutilizador)&& enc.getNome().equals(nome)) {utilizador=enc;a=0;break;}
        else a=1;
    }
    
    if (a==0)    return utilizador;
    else throw new UtilizadorException("Email ou password errados");
} 

public Utilizador getUtilizador(String codutilizador){
    Utilizador utilizador = new Utilizador();
    
    
    for (Utilizador enc: this.u){
        if (enc.getCodigo().equals(codutilizador)) {utilizador=enc;break;}
       
    }
    
    return utilizador;
    
} 

public Lojas getLojas(String nome,String codLojas) throws LojasException{
    Lojas lojas = new Lojas();
    int a=0;
    
    for (Lojas enc: this.l){
        if (enc.getCodloja().equals(codLojas)&& enc.getNomeloja().equals(nome)) {lojas=enc;a=0;break;}
        else a=1;
    }
    
    if (a==0)    return lojas;
    else throw new LojasException("Email ou password errados");
} 

public Lojas getLojas(String codLojas) {
    Lojas lojas = new Lojas();
   
    
    for (Lojas enc: this.l){
        if (enc.getCodloja().equals(codLojas)) {lojas=enc;break;}
        
    }
    
   return lojas;
    
} 

public EncomendasAceites getEAceite(String codea){
    for (EncomendasAceites vr: this.ea){
        if (vr.getEA().equals(codea)) return vr;
    }
    
    return new EncomendasAceites(codea,"");
}  

//Todas as Encomendas de um Utilizador

public List<Encomendas> AllEncomendasEntreguesUtilizador(String codutilizador) throws EncomendaException{
    List<Encomendas> encomenda = new ArrayList<Encomendas>();
    List<Encomendas> encomendas = new ArrayList<Encomendas>();
    for (Encomenda enc: this.e){
        if (enc.getCodutilizador().equals(codutilizador)) encomenda.add(enc);
    }
    
    for (EncomendaMedica enc: this.em){
        if (enc.getCodutilizador().equals(codutilizador)) encomenda.add(enc);
    }
    for(Encomendas enc:encomenda){
        for(Lojas loj:this.l){
            if(loj.getE().equals(enc.getCodencomenda()))encomendas.add(enc);
        }
    }
    
        
    if (encomendas.size()==0)throw new EncomendaException("Nao fez nenhuma encomenda");
    else return encomenda;
}

public List<Encomendas> AllEncomendasNEntreguesUtilizador(String codutilizador){
    List<Encomendas> encomenda = new ArrayList<Encomendas>();
    List<Encomendas> encomendas = new ArrayList<Encomendas>();
     for (Encomenda enc: this.e){
        if (enc.getCodutilizador().equals(codutilizador)) encomenda.add(enc);
    }
    
    for (EncomendaMedica enc: this.em){
        if (enc.getCodutilizador().equals(codutilizador)) encomenda.add(enc);
    }
    for(Encomendas enc:encomenda){
        for(Lojas loj:this.l){
            for(String pe:loj.getPE())if(pe.equals(enc.getCodencomenda()))encomendas.add(enc);
            for(String pe:loj.getAP())if(pe.equals(enc.getCodencomenda()))encomendas.add(enc);
        }
    }
    return encomendas;
}
//Adicionar um Elemento ao Array/Criar um "Utilizador" do APP
public String addEncomenda(String codu,String codl) {
    int code=0;
    
    for (Encomenda enc: this.e){
        if (Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())))>=code){
           code = Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())));
        }
        
    }
    
    for (EncomendaMedica enc: this.em){
        if (Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())))>=code){
           code = Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())));
        }
        
    }
    
    Encomenda a = new Encomenda();
    a.setCodencomenda("e"+Integer.toString(code+1));
    a.setCodutilizador(codu);
    a.setCodloja(codl);
    a.setPeso(r.randomInterval(100,0));
    this.e.add(a);
    
    return a.getCodencomenda();
} 


public String addEncomendaMedica(String codu,String codl) {
    int code=0;
    
    for (Encomenda enc: this.e){
        if (Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())))>=code){
           code = Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())));
        }
        
    }
    
    for (EncomendaMedica enc: this.em){
        if (Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())))>=code){
           code = Integer.parseInt(enc.getCodencomenda().substring(1,(enc.getCodencomenda().length())));
        }
        
    }
    
    EncomendaMedica a = new EncomendaMedica();
    a.setCodencomenda("e"+Integer.toString(code+1));
    a.setCodutilizador(codu);
    a.setCodloja(codl);
    a.setPeso(r.randomInterval(100,0));
    this.em.add(a);
    
    return a.getCodencomenda();
} 

public void addEncomendasAceites(String codencomenda,String codentregador) {
    EncomendasAceites a=new EncomendasAceites(codencomenda,codentregador);
    this.ea.add(a);
} 

public String addUtilizador(String nome) {
    Utilizador utilizador = new Utilizador();
    int code=0;
    
    for (Utilizador enc: this.u){
        if (Integer.parseInt(enc.getCodigo().substring(1,(enc.getCodigo().length())))>=code){
           code = Integer.parseInt(enc.getCodigo().substring(1,(enc.getCodigo().length())));
        }
        
    }
    
    Utilizador a = new Utilizador(nome,"u"+Integer.toString(code+1),this.r.randomInterval(300, -300),this.r.randomInterval(300, -300),new ArrayList<Pedidos>());
    this.u.add(a);
    
    return a.getCodigo();
} 

public String addVoluntario(String nome) {
    Voluntarios vol = new Voluntarios();
    int code=0;
    
    for (Voluntarios enc: this.v){
        if (Integer.parseInt(enc.getCodVol().substring(1,(enc.getCodVol().length())))>=code){
           code = Integer.parseInt(enc.getCodVol().substring(1,(enc.getCodVol().length())));
        }
    }
    
    Voluntarios a = new Voluntarios(nome,"v"+Integer.toString(code+1),this.r.randomInterval(300, -300),this.r.randomInterval(300, -300),this.r.randomInterval(100, 0),false,0,0,false);
    this.v.add(a);
    
    return a.getCodVol();
} 

public String addLoja(String nome) {
    Lojas vol = new Lojas();
    int code=0;
    
    for (Lojas enc: this.l){
        if (Integer.parseInt(enc.getCodloja().substring(1,(enc.getCodloja().length())))>=code){
           code = Integer.parseInt(enc.getCodloja().substring(1,(enc.getCodloja().length())));
        }
    }
    
    Lojas a = new Lojas(nome,"l"+Integer.toString(code+1),this.r.randomInterval(300, -300),this.r.randomInterval(300, -300),true ,new ArrayList<String>(),new ArrayList<String>(),new ArrayList<String>(),new ArrayList<LinhaDeEncomenda>());
    this.l.add(a);
    
    return a.getCodloja();
} 

public String addTransp(String nome,Double preco) {
    Transportadoras vol = new Transportadoras();
    int code=0;
    
    for (Transportadoras enc: this.t){
        if (Integer.parseInt(enc.getCodEmp().substring(1,(enc.getCodEmp().length())))>=code){
           code = Integer.parseInt(enc.getCodEmp().substring(1,(enc.getCodEmp().length())));
        } 
    }
    
    Transportadoras a = new Transportadoras(nome,"t"+Integer.toString(code+1),this.r.randomInterval(300, -300),this.r.randomInterval(4000, 0),this.r.randomInterval(300, -300),preco,0,0,0,false,false);
    this.t.add(a);
    
    return a.getCodEmp();
} 

//Ler Linhas de ficheiro
public void parse(String nomeFich) {
    List<String> linhas=new ArrayList<>();
    
        try{
            linhas = lerFicheiro(nomeFich);
        }
        
        catch (FileNotFoundException e) {
            System.out.println("Nao Existe  esse Ficheiro\n");  
        }
        
        catch (IOException e) {
            System.out.println("Ops! Erro de leitura!\n");     
        }
        
        catch (ClassNotFoundException e) {
            System.out.println("Ops! Formato de ficheiro de dados errado!");
            
        }
        
        String[] linhaPartida;
        
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                
                    case "Utilizador": 
                        Utilizador u = parseUtilizador(linhaPartida[1]);
                        this.u.add(u);
                        
                        break;
                
                    case "Loja": 
                        Lojas l = parseLoja(linhaPartida[1]);
                        this.l.add(l);
                        
                        break;                                   
                
                    case "Transportadora":
                        Transportadoras t = parseTransportadora(linhaPartida[1]);
                        this.t.add(t);
                        
                        break;
                
                    case "Voluntario":
                        Voluntarios v = parseVoluntario(linhaPartida[1]);
                        this.v.add(v);
                        
                        break;
                
                    case "Encomenda":
                        try{
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        this.e.add(e);
                    }
                    
                        catch(ParseException f){
                        System.out.println("Nao deu");
                    }
                        
                        break;
                
                    case "EncomendaMedica":
                        try{
                        EncomendaMedica em = parseEncomendaMedica(linhaPartida[1]);
                        this.em.add(em);
                    }
                    
                        catch(ParseException f){
                        System.out.println("Nao deu");
                    }
                    break;
                    
                    case "Aceite":
                        EncomendasAceites ea = parseAceite(linhaPartida[1]);
                        this.ea.add(ea);
                        
                        break;                
                
                    default: 
                        System.out.println("Linha invalida.");
                        break;
             }
      }
     
      List<EncomendasAceites> novo=new ArrayList<>();
      for(EncomendasAceites ea:this.getAEA()){
          if(!ea.getQ().equals(""))novo.add(ea);
        }
        this.setAEA(novo);
        
       for(Encomenda ea:this.getAE()){
          this.getLojas(ea.getCodloja()).adicionaLinha2(ea.getCodencomenda());
        }
        
        
}
                                
//Parse de cada classe

public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String nome = campos[1]; 
        String codUtilizador = campos[0];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        List<Pedidos> a=new ArrayList<Pedidos>();
        if(campos.length!=4){
            int x=4;
            for(;x<campos.length;x+=2){
                String codtransp=campos[x+1];
                String codenc=campos[x];
                
                Pedidos linha=new Pedidos(codenc,codtransp);
                a.add(linha);
            }
        }
        return new Utilizador(nome,codUtilizador,gpsx,gpsy,a);
  }

public Lojas parseLoja(String input){
        String[] campos = input.split(",");
        String codLoja = campos[0]; 
        String nomeLoja = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        
        if (campos.length==4){
            
        return new Lojas(nomeLoja,codLoja,gpsx,gpsy,true,new ArrayList<String>(),new ArrayList<String>(),new ArrayList<String>(),new ArrayList<LinhaDeEncomenda>());
        }     
        else{
            
            boolean bol=Boolean.parseBoolean(campos[4]);
            
            int x=6;
            List<String> a= new ArrayList<>();
            
            while(!campos[x].equals("Por Entregar") && x<campos.length-1){
                a.add(campos[x]);
                x+=1;
            }
            x+=1;
            List<String> b= new ArrayList<>();
            
            while(!campos[x].equals( "A preparar")&& x<campos.length-1){
                b.add(campos[x]);
                x+=1;
            }
            x+=1;
            List<String> c= new ArrayList<>();
            
            while(!campos[x].equals("Stock") && x<campos.length-1){
                b.add(campos[x]);
                x+=1;
            }
            x+=1;
            List<LinhaDeEncomenda> d= new ArrayList<>();
            
            for(;x<campos.length;x+=4){
                String desc=campos[x+1];
                String codprod=campos[x];
                double preco = Float.parseFloat(campos[x+3]);
                double quant=Float.parseFloat(campos[x+2]);
                LinhaDeEncomenda linha=new LinhaDeEncomenda(desc,quant,codprod,preco);
                d.add(linha);
            }
            
            
            
            return new Lojas(nomeLoja,codLoja,gpsx,gpsy,bol,b,a,c,d);
        }
}

public Transportadoras parseTransportadora(String input){
        String[] campos = input.split(",");
        String codempresa = campos[0]; 
        String nomeempresa= campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        double raio = Double.parseDouble(campos[4]);
        double precokm = Double.parseDouble(campos[5]);
        
        boolean disp=false;
        boolean med=false;
        float c=0;
        int n=0;
        double kmpq=0;
        if (campos.length==11){
            c=Float.parseFloat(campos[8]);
            n=Integer.parseInt(campos[9]);
            kmpq=Double.parseDouble(campos[10]);
            disp = Boolean.parseBoolean(campos[6]);
            med = Boolean.parseBoolean(campos[7]);
        }
        
        return new Transportadoras(nomeempresa,codempresa,gpsx,gpsy,raio,precokm,c,n,kmpq,disp,med);

}

  
public Encomenda parseEncomenda(String input) throws ParseException{
      String[] campos = input.split(",");
      ArrayList<LinhaDeEncomenda> l = new ArrayList<LinhaDeEncomenda>();
      String codenc=campos[0];
      String codu = campos[1];
      String codl= campos[2];
      
      
      
      double peso = Double.parseDouble(campos[3]);
      
      int x=0;
      boolean a;
      Encomenda e=new Encomenda( codenc,codu,codl,peso,l,LocalDate.now(),LocalDate.now());
      
      try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            sdf.parse(campos[4]);
            a= true;
        } 
      
      catch (ParseException ex) {
            a= false;
        }
        
      if (a==true){
          LocalDate tempo=LocalDate.parse(campos[4]);
          e.setTempo(tempo);
          x=5;
        } 
      
      else x=4;
      
      try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            sdf.parse(campos[x]);
            a= true;
        } 
      
      catch (ParseException ex) {
            a= false;
        }
        
      if (a==true){
          LocalDate tempo=LocalDate.parse(campos[4]);
          e.setTempo(tempo);
          x+=1;
        } 
      
        
      
      for(;x<campos.length-1;x+=4){
          String desc=campos[x+1];
          String codprod=campos[x];
          double preco = Float.parseFloat(campos[x+3]);
          double quant=Float.parseFloat(campos[x+2]);
          LinhaDeEncomenda linha=new LinhaDeEncomenda(desc,quant,codprod,preco);
          e.adicionaLinha(linha);
        }
      
      
      
      return e;
    }
  
public EncomendaMedica parseEncomendaMedica(String input) throws ParseException{
      String[] campos = input.split(",");
      ArrayList<LinhaDeEncomenda> l = new ArrayList<LinhaDeEncomenda>();
      String codenc=campos[0];
      String codu = campos[1];
      String codl= campos[2];
      
      double peso = Double.parseDouble(campos[3]);
      
      int x=0;
      boolean a;
      
      EncomendaMedica e=new EncomendaMedica( codenc,codu,codl,peso,l,LocalDate.now(),LocalDate.now());
      
      try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            sdf.parse(campos[4]);
            a= true;
        } 
      
      catch (ParseException ex) {
            a= false;
        }
        
      if (a==true){
          LocalDate tempo=LocalDate.parse(campos[4]);
          e.setTempo(tempo);
          x=5;
        } 
      
      else x=4;
      
      try {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
            sdf.parse(campos[x]);
            a= true;
        } 
      
      catch (ParseException ex) {
            a= false;
        }
        
      if (a==true){
          LocalDate tempo=LocalDate.parse(campos[4]);
          e.setTempo(tempo);
          x+=1;
        } 
      
        
      
      for(;x<campos.length-1;x+=4){
          String desc=campos[x+1];
          String codprod=campos[x];
          double preco = Float.parseFloat(campos[x+3]);
          double quant=Float.parseFloat(campos[x+2]);
          LinhaDeEncomenda linha=new LinhaDeEncomenda(desc,quant,codprod,preco);
          e.adicionaLinha(linha);
        }
      
      return e;
    }
  
public Voluntarios parseVoluntario(String input){
        String[] campos = input.split(",");
        String codv = campos[0]; 
        String nomev= campos[1];
        double gpsx = Float.parseFloat(campos[2]);
        double gpsy = Float.parseFloat(campos[3]);
        double raio = Float.parseFloat(campos[4]);
        
        if (campos.length<=5){
        return new Voluntarios(nomev,codv,gpsx,gpsy,raio,false,0,0,false);
        }
        
        else{
        boolean disp=Boolean.parseBoolean(campos[5]);
        float c = Float.parseFloat(campos[6]);
        int n=Integer.parseInt(campos[7]);
       
        boolean med=Boolean.parseBoolean(campos[8]);
        return new Voluntarios(nomev,codv,gpsx,gpsy,raio,disp,c,n,med);}
    }
    
    
public EncomendasAceites parseAceite(String input){
        String[] campos = input.split(",");
        String codea = campos[0];
        
        EncomendasAceites a = new EncomendasAceites(codea,"");
        if (campos.length>1){
        
        String quem=campos[1];
       
        a.setQ(quem);
        }
        
        return a;
    }



    
//Ler ficheiro    
    
public List<String> lerFicheiro(String nomeFich) throws FileNotFoundException,ClassNotFoundException,IOException{
        List<String> lines = new ArrayList<>();
        try { 
            lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); 
        }
        
        catch(IOException exc) { 
            System.out.println(exc.getMessage()); 
        }
        
        return lines;
  }

//Escrever no ficheiro
  
public void escrevernoficheiro(String nomeFich) throws FileNotFoundException,ClassNotFoundException,IOException{ 
     PrintWriter arq = new PrintWriter(nomeFich);
     
     
        
     for( Lojas l:this.l){
         arq.println(l.toString());
        }
        
     for( Transportadoras t:this.t){
         arq.println(t.toString());
        }
        
     for( Utilizador u:this.u){
         arq.println(u.toString());
        }
        
     for(Voluntarios v :this.v){
         arq.println(v.toString());
        }
        
     for(Encomenda e:this.e){
         arq.println(e.toString());
        }
        
     for(EncomendaMedica em:this.em){
         arq.println(em.toString());
        }   
     for(EncomendasAceites ea:this.ea){
         arq.println(ea.toString());
        }
     
     
        
     arq.flush();
     arq.close();
    }
}
