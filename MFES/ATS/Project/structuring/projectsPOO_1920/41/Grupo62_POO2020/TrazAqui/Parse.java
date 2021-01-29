import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;
import java.util.*;
import java.util.stream.*;
import java.time.*;
import java.io.*;
import java.lang.Math.*;

public class Parse {
    
    /** Inteiro que indica a fase do menu atual*/
    private int fase;
    /**
     * Construtor vazio
     */
    public Parse(){
        fase = 0;
    }
    
  public void parse(){
        List<String> linhas = lerFicheiro("logsTrazAqui.txt");
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                case "Utilizador": 
                        Utilizador u = parseUtilizador(linhaPartida[1]);
                        System.out.println(u.toString());
                        break;
                case "Voluntario":
                        Voluntario v = parseVoluntario(linhaPartida[1]);
                        System.out.println(v.toString());
                        break;
                case "Transportadora":
                        Transportadora t = parseTransportadora(linhaPartida[1]);
                        System.out.println(t.toString());
                        break;
                case "Loja": 
                        Loja l = parseLoja(linhaPartida[1]);
                        System.out.println(l.toString());
                        break;
                case "Encomenda": 
                        Encomenda e = parseEncomenda(linhaPartida[1]);
                        System.out.println(e.toString());
                        break;
                case "Aceite": 
                        String a = parseAceite(linhaPartida[1]);
                        System.out.println(a.toString());
                        break;
                default: 
                        System.out.println("Linha inválida.");
                        break;
                }

        }
        System.out.println("Done!");
  }
  
  public Utilizador parseUtilizador(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double x = Double.parseDouble(campos[2]);
        double y = Double.parseDouble(campos[3]);
        
        Utilizador u = new Utilizador(cod,nome,x,y,"email@gmail.com","pass");
        
        return u;
  }
  
  public Voluntario parseVoluntario(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String nome = campos[1];
        double x = Double.parseDouble(campos[2]);
        double y = Double.parseDouble(campos[3]);
        double raioVol = Double.parseDouble(campos[4]);
        
        Voluntario v = new Voluntario(cod,nome,x,y,"email@gmail.com","pass",raioVol,"Inativo");
        
        return v;
  }
  
  public Transportadora parseTransportadora(String input){
        String[] campos = input.split(",");
        String cod = campos[0]; 
        String nome = campos[1];
        double x = Double.parseDouble(campos[2]);
        double y = Double.parseDouble(campos[3]);
        int nifEmp = Integer.parseInt(campos[4]);
        double raioEmp = Double.parseDouble(campos[5]);
        double precoPorKm = Double.parseDouble(campos[6]);
        
        Transportadora t = new Transportadora(cod,nome,x,y,"email@gmail.com","pass",nifEmp,raioEmp,precoPorKm);
        
        return t;
  }
  
  public Loja parseLoja(String input){
        String[] campos = input.split(",");
        String cod = campos[0]; 
        String nome = campos[1];
        double x = Double.parseDouble(campos[2]);
        double y = Double.parseDouble(campos[3]);
        
        Loja l = new Loja(cod,nome,x,y,"email@gmail.com","pass",0);
        
        return l;
  }

  public Encomenda parseEncomenda(String input){
      String[] campos = input.split(",");
      String codEnc = campos[0];
      String codUser = campos[1];
      String codLoja = campos[2];
      double peso = Double.parseDouble(campos[3]);
      List<LinhaEncomenda> lEnc = new ArrayList<>();
        for (int i = 4; i < lEnc.size(); i+=4) {
            lEnc.add(parseLinhaEncomenda(input,i));
        }
        
      Encomenda e = new Encomenda(codEnc,codUser,codLoja,"t6",peso,LocalDate.now(),lEnc,lEnc.size(),"A preparar");
      
      return e;
  }
  
  public LinhaEncomenda parseLinhaEncomenda(String input, int i){
      String[] campos = input.split(",");
      String codProd = campos[i];
      String descProd = campos[i+1];
      double quantProd = Double.parseDouble(campos[i+2]);
      double valorProd = Double.parseDouble(campos[i+3]);
      
      LinhaEncomenda le = new LinhaEncomenda(codProd,descProd,quantProd,valorProd);
      
      return le;
  }
  
  public String parseAceite(String input){
      String codEnc = input;
      
      return codEnc;
  }
  
  /**
     * Metodo main
     * 
     * @param path  Caminho para o ficheiro
     */
    public void main(String path){
        Scanner sc = new Scanner(System.in);
        String [] menuInicial = {"Sair", "Login Utilizador","Login Voluntario", "Login Transportadora", "Login Loja", 
            "Registar Utilizador", "Registar Voluntario", "Registar Transportadora", "Registar Loja", "Administrador"};
        String [] menuUtilizador = {"Sair", "Pedir Encomenda", "Pedir Transporte", "Verificar Estado de Encomenda"};
        String [] menuVoluntario = {"Sair", "Aceitar Encomenda", "Entregar Encomenda", "Mudar Estado"};
        String [] menuTransportadora = {"Sair", "Aceitar Encomenda", "Calcular Preço de Entrega"};
        String [] menuLoja = {"Sair", "Encomenda Pronta", "Encomendas em Espera"};
        String [] menuAdmin = {"Sair","Top 10 Utilizadores", "Top 10 Empresas"};
        
        int ultima = -1;
        Sistema s = new Sistema();
        
        try{
            s = Sistema.carregaEstado("Estado");
        }
        catch(FileNotFoundException exc){
            System.out.println(exc);
            criaSistema(s);
        }
        catch(IOException exc){
            System.out.println(exc);
            criaSistema(s);
        }
        catch(ClassNotFoundException exc){
            System.out.println(exc);
            criaSistema(s);
        }
        
        String str;
        Utilizador u = null;
        Voluntario v = null;
        Transportadora t = null;
        Loja l = null;
        boolean b;
        
        while(fase != -1){
            switch(fase){
                case 0: printMenu(menuInicial);
                do{
                    b = true;
                    try{
                        ultima = sc.nextInt();
                        b = false;
                    }
                    catch(Exception exc){
                        System.out.println(exc);
                    }
                }
                while(b);
                switch(ultima){
                    case 0: System.out.println("A sair");
                    fase = -1;
                    break;
                    case 1: u = loginUtilizador(s);
                    break;
                    case 2: v = loginVoluntario(s);
                    break;
                    case 3: t = loginTransportadora(s);
                    break;
                    case 4: l = loginLoja(s);
                    break;
                    case 5: u = registaUtilizador(s);
                    fase = 1;
                    break;
                    case 6: v = registaVoluntario(s);
                    fase = 2;
                    break;
                    case 7: t = registaTransportadora(s);
                    fase = 3;
                    break;
                    case 8: l = registaLoja(s);
                    fase = 4;
                    break;
                    case 9: logAdmin(s);
                    fase = 5;
                    break;
                }
                break;
                case 1: printMenu(menuUtilizador);
                do{
                    b = true;
                    try{
                        ultima = sc.nextInt();
                        b = false;
                    }
                    catch(Exception exc){
                        System.out.println(exc);
                    }
                }
                while(b);
                switch(ultima){
                    case 0: fase = 0;
                    break;
                    case 1: criaEncomenda(s,u);
                    break;
                    case 2: pedeTransporte(s);
                    break;
                }
                break;
                case 2: printMenu(menuVoluntario);
                do{
                    b = true;
                    try{
                        ultima = sc.nextInt();
                        b = false;
                    }
                    catch(Exception exc){
                        System.out.println(exc);
                    }
                }
                while(b);
                switch(ultima){
                    case 0: fase = 0;
                    break;
                    case 1: aceitaTrabalho(s,v);
                    break;
                    case 2: encEntregue(s);
                    break;
                }
                break;
                case 3: printMenu(menuTransportadora);
                do{
                    b = true;
                    try{
                        ultima = sc.nextInt();
                        b = false;
                    }
                    catch(Exception exc){
                        System.out.println(exc);
                    }
                }
                while(b);
                switch(ultima){
                    case 0: fase = 0;
                    break;
                }
                break;
                case 4: printMenu(menuLoja);
                do{
                    b = true;
                    try{
                        ultima = sc.nextInt();
                        b = false;
                    }
                    catch(Exception exc){
                        System.out.println(exc);
                    }
                }
                while(b);
                switch(ultima){
                    case 0: fase = 0;
                    break;
                    case 1: encomendaPronta(s);
                    break;
                }
                break;
                case 5: printMenu(menuAdmin);
                do{
                    b = true;
                    try{
                        ultima = sc.nextInt();
                        b = false;
                    }
                    catch(Exception exc){
                        System.out.println(exc);
                    }
                }
                while(b);
                switch(ultima){
                    case 0: fase = 0;
                    break;
                    case 1: printTop10Users(s);
                    break;
                    case 2: printTop10Transportadoras(s);
                    break;
                }
                break;
            }
        }
        try{
            s.guardaEstado("Estado");
        }
        catch(FileNotFoundException exc){
            System.out.println(exc);
        }
        catch(IOException exc){
            System.out.println(exc);
        }
    }
    
    /**
     * Metodo que cria um sistema
     * 
     * @param s Sistema em que são guardadas as alterações
     */
    public void criaSistema(Sistema s){
        try{
            Utilizador u1 = new Utilizador("u14","Pessoa que escreveu o seu nome aqui",14.04,19.99,"a00000@alunos.uminho.pt","POO2020");
            Utilizador u2 = new Utilizador("u213","Alex Telles",12.34,56.78,"alextelles@hotmail.com","antelles13");
            Utilizador u3 = new Utilizador("u420","Calvin Cordozar Broadus Jr.",-42.00,42.00,"snoopdogg@hotmail.com","sweveryday420");
            Utilizador u4 = new Utilizador("u1","Douglas Jay Falcon",01.01,-90.00,"captainfalcon@gmail.com","falconpunch01");
            Utilizador u5 = new Utilizador("u11","Moussa Marega",50.50,19.91,"marega11@gmail.com","Marega11");
            s.adicionaUtilizador(u1);
            s.adicionaUtilizador(u2);
            s.adicionaUtilizador(u3);
            s.adicionaUtilizador(u4);
            s.adicionaUtilizador(u5);
            
            Voluntario v1 = new Voluntario("v29", "Vo Lun Tario", -30.00, 65.65, "voluntariado@gmail.com","vlt65",65.65,"Ativo");
            Voluntario v2 = new Voluntario("v77", "Aju Dante", 40.00, 62.82, "danteaju@gmail.com","dantea77",50.75,"Ativo");
            Voluntario v3 = new Voluntario("v48", "N. Tregas", 37.73, -26.62, "lapracasa@hotmail.com","tnt48",35.00,"Ativo");
            Voluntario v4 = new Voluntario("v32", "Vay A. Schkompras", 47.47, -56.65, "compratudo@gmail.com","pdvas21",82.00,"Ativo");
            Voluntario v5 = new Voluntario("v8", "Traschkum Ida", -10.67, 89.98, "comida@hotmail.com","trasch8",26.26,"Ativo");
            s.adicionaVoluntario(v1);
            s.adicionaVoluntario(v2);
            s.adicionaVoluntario(v3);
            s.adicionaVoluntario(v4);
            s.adicionaVoluntario(v5);
            
            Transportadora t1 = new Transportadora("t22", "Camiões", 22.22, 22.22, "camioes@gmail.com","camioes22",123456789,44.44,1.0);
            Transportadora t2 = new Transportadora("t33", "Carrinhas", -33.33, 66.66, "carrinhas@gmail.com","carrinhas33",987654321,55.55,1.5);
            Transportadora t3 = new Transportadora("t55", "Carros", 00.05, -50.00, "carros@gmail.com","carros55",824688642,25.00,2.5);
            Transportadora t4 = new Transportadora("t88", "Motas", -88.88, -44.44, "motas@gmail.com","motas88",307458888,30.00,3.0);
            Transportadora t5 = new Transportadora("t6", "Trotinetas", 6.06, -6.06, "trotinetas@gmail.com","trotinetas6",666666666,66.66,0.3);
            s.adicionaTransportadora(t1);
            s.adicionaTransportadora(t2);
            s.adicionaTransportadora(t3);
            s.adicionaTransportadora(t4);
            s.adicionaTransportadora(t5);
            
            Loja l1 = new Loja("l92", "Pingo Doce", 30.36, -79.43, "pingodoce@gmail.com","dingopoce",0);
            Loja l2 = new Loja("l60", "Intermarché", 74.23, -32.47, "intermarche@gmail.com","minterarche",0);
            Loja l3 = new Loja("l78", "Continente", 61.21, 21.61, "continente@gmail.com","nonticente",0);
            Loja l4 = new Loja("l12", "FNAC", -85.33, -59.41, "fnac@gmail.com","nfac",1);
            Loja l5 = new Loja("l67", "Amazon", -24.24, -12.13, "amazon@gmail.com","zamaon",0);
            s.adicionaLoja(l1);
            s.adicionaLoja(l2);
            s.adicionaLoja(l3);
            s.adicionaLoja(l4);
            s.adicionaLoja(l5);
            
            LinhaEncomenda le1 = new LinhaEncomenda("p26","Telemóvel",1.0,150.0);
            LinhaEncomenda le2 = new LinhaEncomenda("p43","Bicicleta",3.0,100.0);
            LinhaEncomenda le3 = new LinhaEncomenda("p75","Ovos",20.0,1.50);
            
            List<LinhaEncomenda> list1 = new ArrayList<>();
            List<LinhaEncomenda> list2 = new ArrayList<>();
            List<LinhaEncomenda> list3 = new ArrayList<>();
            
            list1.add(le1);
            list2.add(le2); 
            list3.add(le1); list3.add(le3);
            
            Encomenda e1 = new Encomenda("e1","u14","l12","t22",15.15,LocalDate.of(2020,3,5),list1,list1.size(),"A preparar");
            Encomenda e2 = new Encomenda("e2","u14","l12","v32",15.15,LocalDate.of(2020,3,5),list2,list2.size(),"A preparar");
            Encomenda e3 = new Encomenda("e3","u14","l12","t6",15.15,LocalDate.of(2020,3,5),list3,list3.size(),"A preparar");
            s.adicionaEncomenda(e1);
            s.adicionaEncomenda(e2);
            s.adicionaEncomenda(e3);
            
            Administrador admin = new Administrador("Admin", "admin");
            s.setAdministrador(admin);
        }
        catch(ExisteEncomendaException exc){
            System.out.println(exc);
        }
        catch(NaoExisteUtilizadorException exc){
            System.out.println(exc);
        }
        catch(ExisteCodSistemaException exc){
            System.out.println(exc);
        }
    }
    
    /**
     * Metodo que imprime um menu no ecra
     * 
     * @param Array com as opções do menu
     */
    public void printMenu(String [] menu){
        for(int i = 0; i < menu.length; i++){
            System.out.println(i + "-" + menu[i]);
        }
    }
    
    /**
     * Metodo que imprime os 10 utilizadores que mais encomendaram
     * 
     * @param s Sistema que contem toda a informação
     */
    public void printTop10Users(Sistema s){
        try{
            int x = 1;
            Set<String> top = s.top10EncUser();
            for(String i: top){
                System.out.println(x + "-" + i);
                x++;
            }    
        }
        catch(NaoExisteUtilizadorException exc){
            System.out.println(exc);
        }
        catch (Exception exc){
            System.out.println(exc);
        }
    }
    
    /**
     * Metodo que imprime os 10 utilizadores que mais encomendaram
     * 
     * @param s Sistema que contem toda a informação
     */
    public void printTop10Transportadoras(Sistema s){
        try{
            int x = 1;
            Set<String> top = s.top10Empresas();
            for(String i: top){
                System.out.println(x + "-" + i);
                x++;
            }    
        }
        catch(NaoExisteTransportadoraException exc){
            System.out.println(exc);
        }
        catch (Exception exc){
            System.out.println(exc);
        }
    }
    
    public Utilizador loginUtilizador(Sistema s){
        boolean b;
        String n = "";
        Utilizador u = new Utilizador();
        Scanner sc = new Scanner(System.in);
        String pass;
        try{
            System.out.println("Insira código utilizador");
            n = sc.nextLine();
            System.out.println("Insira palavra passe");
            pass = sc.nextLine();
            b = s.validaAcesso(n, pass);
            if(b){
                u = (Utilizador) s.getInfo().get(n);
                fase = 1;
                return u;
            }
        }
        catch (NaoExisteCodException exc){
            System.out.println(exc);
        }
        catch(PasseErradaException exc){
            System.out.println(exc);
        }
        catch(Exception exc){
            System.out.println(exc);
        }
        return u;
    }
    
    public Voluntario loginVoluntario(Sistema s){
        boolean b;
        String n = "";
        Voluntario v = new Voluntario();
        Scanner sc = new Scanner(System.in);
        String pass;
        try{
            System.out.println("Insira código voluntario");
            n = sc.nextLine();
            System.out.println("Insira palavra passe");
            pass = sc.nextLine();
            b = s.validaAcesso(n, pass);
            if(b){
                v = (Voluntario) s.getInfo().get(n);
                fase = 2;
                return v;
            }
        }
        catch (NaoExisteCodException exc){
            System.out.println(exc);
        }
        catch(PasseErradaException exc){
            System.out.println(exc);
        }
        catch(Exception exc){
            System.out.println(exc);
        }
        return v;
    }
    
    public Transportadora loginTransportadora(Sistema s){
        boolean b;
        String n = "";
        Transportadora t = new Transportadora();
        Scanner sc = new Scanner(System.in);
        String pass;
        try{
            System.out.println("Insira código transportadora");
            n = sc.nextLine();
            System.out.println("Insira palavra passe");
            pass = sc.nextLine();
            b = s.validaAcesso(n, pass);
            if(b){
                t = (Transportadora) s.getInfo().get(n);
                fase = 3;
                return t;
            }
        }
        catch (NaoExisteCodException exc){
            System.out.println(exc);
        }
        catch(PasseErradaException exc){
            System.out.println(exc);
        }
        catch(Exception exc){
            System.out.println(exc);
        }
        return t;
    }
    
    public Loja loginLoja(Sistema s){
        boolean b;
        String n = "";
        Loja l = new Loja();
        Scanner sc = new Scanner(System.in);
        String pass;
        try{
            System.out.println("Insira código loja");
            n = sc.nextLine();
            System.out.println("Insira palavra passe");
            pass = sc.nextLine();
            b = s.validaAcesso(n, pass);
            if(b){
                l = (Loja) s.getInfo().get(n);
                fase = 4;
                return l;
            }
        }
        catch (NaoExisteCodException exc){
            System.out.println(exc);
        }
        catch(PasseErradaException exc){
            System.out.println(exc);
        }
        catch(Exception exc){
            System.out.println(exc);
        }
        return l;
    }
    
    /**
     * Metodo para o administrador entrar no sistema
     * 
     * @param s Sistema no qual o administrador pretende aceder
     */
    public void logAdmin(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        do{
            System.out.println("Insira a pass admin");
            str = sc.nextLine();
        }
        while(!s.getAdministrador().getPassword().equals(str));
    }
    
    /**
     * Metodo que regista um utilizador num sistema
     * 
     * @param s Sistema no qual o utilizador vai ser inserido
     */
    public Utilizador registaUtilizador(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        Utilizador u = new Utilizador();
        boolean b;
        double j;
        try{
            System.out.println("Email");
            str = sc.nextLine();
            u.setEmail(str);
            System.out.println("Nome");
            str = sc.nextLine();
            u.setNome(str);
            System.out.println("Codigo");
            str = sc.nextLine();
            u.setCod(str);
            System.out.println("Password");
            str = sc.nextLine();
            u.setPassword(str);
            System.out.println("Latitude");
            j = sc.nextDouble();
            u.setX(j);
            System.out.println("Longitude");
            j = sc.nextDouble();
            u.setY(j);
            s.adicionaUtilizador(u);
            System.out.println("Acabou de se registar");
            fase = 1;
            return u;
        }
        catch(ExisteCodSistemaException exc){
            System.out.println(exc);
        }
        catch (Exception exc){
            System.out.println(exc);
        }
        return u;
    }
    
    /**
     * Metodo que regista um voluntario num sistema
     * 
     * @param s Sistema no qual o voluntario vai ser inserido
     */
    public Voluntario registaVoluntario(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        Voluntario v = new Voluntario();
        boolean b;
        double j = 0;
        try{
            System.out.println("Email");
            str = sc.nextLine();
            v.setEmail(str);
            System.out.println("Nome");
            str = sc.nextLine();
            v.setNome(str);
            System.out.println("Codigo");
            str = sc.nextLine();
            v.setCod(str);
            System.out.println("Password");
            str = sc.nextLine();
            v.setPassword(str);
            System.out.println("Raio de Ação");
            j = sc.nextDouble();
            v.setRaioVol(j);
            System.out.println("Latitude");
            j = sc.nextDouble();
            v.setX(j);
            System.out.println("Longitude");
            j = sc.nextDouble();
            v.setY(j);
            v.setEstado("Inativo");
            s.adicionaVoluntario(v);
            System.out.println("Acabou de se registar");
            fase = 2;
            return v;
        }
        catch(ExisteCodSistemaException exc){
            System.out.println(exc);
        }
        catch (Exception exc){
            System.out.println(exc);
        }
        return v;
    }
    
    /**
     * Metodo que regista uma transportadora num sistema
     * 
     * @param s Sistema no qual a transportadora vai ser inserida
     */
    public Transportadora registaTransportadora(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        Transportadora t = new Transportadora();
        boolean b;
        int i = 0;
        double j = 0;
        try{
            System.out.println("Email");
            str = sc.nextLine();
            t.setEmail(str);
            System.out.println("Nome");
            str = sc.nextLine();
            t.setNome(str);
            System.out.println("Codigo");
            str = sc.nextLine();
            t.setCod(str);
            System.out.println("Password");
            str = sc.nextLine();
            t.setPassword(str);
            System.out.println("Raio de Ação");
            j = sc.nextDouble();
            t.setRaioEmp(j);
            System.out.println("Preço por km");
            j = sc.nextDouble();
            t.setPrecoPorKm(j);
            System.out.println("NIF");
            i = sc.nextInt();
            t.setNifEmp(i);
            System.out.println("Latitude");
            j = sc.nextDouble();
            t.setX(j);
            System.out.println("Longitude");
            j = sc.nextDouble();
            t.setY(j);
            s.adicionaTransportadora(t);
            System.out.println("Acabou de se registar");
            fase = 3;
            return t;
        }
        catch(ExisteCodSistemaException exc){
            System.out.println(exc);
        }
        catch (Exception exc){
            System.out.println(exc);
        }
        return t;
    }
    
    /**
     * Metodo que regista uma loja num sistema
     * 
     * @param s Sistema no qual a loja vai ser inserida
     */
    public Loja registaLoja(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        Loja l = new Loja();
        boolean b;
        double j = 0.0;
        try{
            System.out.println("Email");
            str = sc.nextLine();
            l.setEmail(str);
            System.out.println("Nome");
            str = sc.nextLine();
            l.setNome(str);
            System.out.println("Codigo");
            str = sc.nextLine();
            l.setCod(str);
            System.out.println("Password");
            str = sc.nextLine();
            l.setPassword(str);
            System.out.println("Latitude");
            j = sc.nextDouble();
            l.setX(j);
            System.out.println("Longitude");
            j = sc.nextDouble();
            l.setY(j);
            l.setFila(0);
            s.adicionaLoja(l);
            System.out.println("Acabou de se registar");
            fase = 4;
            return l;
        }
        catch(ExisteCodSistemaException exc){
            System.out.println(exc);
        }
        catch (Exception exc){
            System.out.println(exc);
        }
        return l;
    }
    
    public Encomenda criaEncomenda(Sistema s, Utilizador u){
        String str;
        Scanner sc = new Scanner(System.in);
        Encomenda e = new Encomenda();
        LinhaEncomenda le = new LinhaEncomenda();
        LocalDate d = LocalDate.now();
        boolean b;
        int i = 0;
        double j = 0.0;
        try{
            System.out.println("Código da Encomenda");
            str = sc.nextLine();
            e.setCodEnc(str);
            e.setCodUser(u.getCod());
            System.out.println("Código da Loja");
            str = sc.nextLine();
            e.setCodLoja(str);
            e.setCodEnt("n/a");
            System.out.println("Peso");
            j = sc.nextDouble();
            e.setPeso(j);
            e.setData(d);
            System.out.println("Numero de Produtos");
            i = sc.nextInt();
            e.setNProd(i);
            i = 0;
            while(i < e.getNProd()){
                System.out.println("Código do Produto");
                str = sc.nextLine();
                le.setCodProd(str);
                System.out.println("Nome do Produto");
                str = sc.nextLine();
                le.setDescProd(str);
                System.out.println("Quantidade do Produto");
                j = sc.nextDouble();
                le.setQuantProd(j);
                System.out.println("Valor do Produto");
                j = sc.nextDouble();
                le.setValorProd(j);
                e.insereLinhaEncomenda(le);
                i++;
            }
            e.setEstado("A preparar");
            s.adicionaEncomenda(e);
            System.out.println("Encomenda Criada");
            fase = 4;
            return e;
        }
        catch (Exception exc){
            System.out.println(exc);
        }
        return e;
    }
        
    public void encomendaPronta(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        Encomenda e = new Encomenda();
        try{
            System.out.println("Código da Encomenda");
            str = sc.nextLine();
            e = s.getEncomenda(str);
        if (e.getEstado() == "A preparar") e.setEstado("Pronta");
        }
        catch (Exception exc){
            System.out.println(exc);
        }
    }
    
    public void pedeTransporte(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        Encomenda e = new Encomenda();
        try{
            System.out.println("Código da Encomenda");
            str = sc.nextLine();
            e = s.getEncomenda(str);
        if (e.getEstado() == "Pronta"){ e.setEstado("Procura Transporte");
        System.out.println("A procurar transporte");}
        else System.out.println("Encomenda não está pronta");
        }
        catch (Exception exc){
            System.out.println(exc);
        }
    }
    
    public void aceitaTrabalho(Sistema s, Entidade ent){
        String str;
        Scanner sc = new Scanner(System.in);
        Encomenda e = new Encomenda();
        List<Encomenda> lista = s.getEncEntrega();
        try{
            for(int j = 0; j < lista.size(); j++){
                    System.out.println(lista.get(j));
                }
                System.out.println("Qual encomenda quer aceitar?");
                str = sc.nextLine();
                e = s.getEncomenda(str);
            if (e.getEstado() == "Procura Transporte"){ 
                e.setEstado("A entregar");
                e.setCodEnt(ent.getCod());
                System.out.println("O trabalho foi aceite");}
            else System.out.println("Encomenda não disponível para entrega");
            }
            catch (Exception exc){
                System.out.println(exc);
        }
    }
    
    public void encEntregue(Sistema s){
        String str;
        Scanner sc = new Scanner(System.in);
        Encomenda e = new Encomenda();
        try{
            System.out.println("Código da Encomenda");
            str = sc.nextLine();
            e = s.getEncomenda(str);
        if (e.getEstado() == "A entregar") e.setEstado("Entregue");
        }
        catch (Exception exc){
            System.out.println(exc);
        }
    }
    
    /**
     * Metodo que carrega informação para um sistema
     * 
     * @param path caminho para o ficheiro
     * 
     * @return Sistema que carregou a informação do ficheiro
     */
    public Sistema carregaSistema(String path){
        Sistema s = new Sistema();
        try{
            s.carregaEstado(path);
        }
        catch(FileNotFoundException exc){
            System.out.println(exc);
        }
        catch(IOException exc){
            System.out.println(exc);
        }
        catch(ClassNotFoundException exc){
            System.out.println(exc);
        } 
        return s;
    } 
    
    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
}
