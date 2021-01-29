import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Stream;
import java.util.stream.Collectors;
import java.time.LocalDate;
import java.util.List;
import java.util.ArrayList;
import java.util.Scanner;

public class Menu{
    public static Set<lojas> ml;
    public static Set<voluntarios> mv;
    public static Set<utilizador> mu;
    public static Set<transportadores> mt;
    public static Set<String> a;
    public static Map<String,String> mln;
    public static Map<String,String> mvn;
    public static Map<String,String> mun;
    public static Map<String,String> mtn;
    public static GestaoEncomendas ge;
    public static Set<encomendaSinalizada> es;
    
    
    public static void menu(utilizador u){
        System.out.println("escolha uma das opcoes:");
        System.out.println("(1) Nova encomenda;");
        System.out.println("(2) propostas de entrega;");
        System.out.println("(3) encomendas aceites.");
        System.out.println("(0) exit");
        Scanner sc = new Scanner(System.in);
        int tipo= sc.nextInt();
        if(tipo == 1){
            System.out.println("digite o codigo da encomenda");
            sc.nextLine();
            String cde=sc.nextLine();
            System.out.println("digite o codigo da loja");
            String cdl=sc.nextLine();
            Double peso=sc.nextDouble();
            List<LinhaEncomenda> le=new ArrayList<>();
            int i=1;
            do{
                System.out.println("insira a referencia do item");
                sc.nextLine();
                String rf=sc.nextLine();
                System.out.println("insira a descriçao do item");
                String di=sc.nextLine();
                System.out.println("insira o preco do item");
                Double pi=sc.nextDouble();
                System.out.println("insira a quantidade");
                Double qt=sc.nextDouble();
                LinhaEncomenda l= new LinhaEncomenda(rf,di,pi,qt);
                le.add(l);
                System.out.println("pertende adicionar mais items a encomenda?");
                System.out.println("(1) Sim");
                System.out.println("(0) Nao");
                i=sc.nextInt();
            }while(i==1);
            Encomenda e = new Encomenda(cde,u.getCodUtilizador(),cdl,peso,le);
            ge.addEncomenda(e);
            menu(u);
            return;
        }
        else if(tipo==2){
            for (encomendaSinalizada e:es){
                if(ge.getEncomendas().get(e.getcde()).getcodcliente().equals(u.getCodUtilizador())){
                    Encomenda enc= ge.getEncomendas().get(e.getcde());
                    
                    for(transportadores t:mt){
                        if(!t.getd());
                        else if(t.getGPS().distancia(e.getgu())< t.getraio() && t.getGPS().distancia(e.getgl())< t.getraio()){
                            System.out.println("A empresa "+ t.getnome()+" esta disponivel para realizar a entrega:");
                            double r=t.getpreco()*(t.getGPS().distancia(e.getgu())+t.getGPS().distancia(e.getgl()));
                            System.out.println("portes de envio-->"+r);
                            r=r+enc.calculaValorTotal();
                            System.out.println("preço total-->"+r);
                            System.out.println("Deseja proceder com esta proposta:");
                            System.out.println("(1) Sim");
                            System.out.println("(2) Nao,procurar mais propostas");
                            System.out.println("(0) Exit");
                            tipo=sc.nextInt();
                            switch(tipo){
                                case 1:
                                    a.add(e.getcde());
                                    break;
                                case 2:
                                    break;
                                default:
                                    return;
                            }
                            
                        }
                    }
                }
            }
            menu(u);
            return;
        }
        else if(tipo==3){
            System.out.println("encomendas ja aceites:");
            for(String s : a){
                if(u.getCodUtilizador().equals( ge.getEncomendas().get(s).getcodcliente() )){
                    System.out.println(ge.getEncomendas().get(s).toString());
                }
            }
            menu(u);
            return;
        }
        return;
    }
    public static void menv(voluntarios v){
        System.out.println("escolha uma das opcoes:");
        System.out.println("(1) sinalizar atividade");
        System.out.println("(2) sinalizar inatividade");
        System.out.println("(3) procurar encomendas na area");
        System.out.println("(0) exit");
        Scanner sc = new Scanner(System.in);
        int tipo= sc.nextInt();
        if (tipo==1) {
            v.setd(true);
            menv(v);
        }
        else if (tipo==2) {
            v.setd(false);
            menv(v);
        }
        else if (tipo==3){
            for (encomendaSinalizada e:es){
                if(v.getGPS().distancia(e.getgu())< v.getraio() && v.getGPS().distancia(e.getgl())< v.getraio()){
                    System.out.println(ge.getEncomendas().get(e.getcde()).toString());
                    System.out.println("aceita realizar esta encomenda?");
                    System.out.println("(1) Sim");
                    System.out.println("(2) Nao");
                    System.out.println("(3) procurar mais encomendas");
                    tipo=sc.nextInt();
                    if(tipo==1){
                        a.add(e.getcde());
                        return ;
                    }
                    else if(tipo==2)return;
                }
            }
            System.out.println("Não ha mais encomendas pendentes na sua area de atividade");
        }
        return;
    }
    public static void ment(transportadores t){
        System.out.println("escolha uma das opcoes:");
        System.out.println("(1) sinalizar atividade");
        System.out.println("(2) sinalizar inatividade");
        System.out.println("(3) determinar o preco por km");
        System.out.println("(0) exit");
        Scanner sc = new Scanner(System.in);
        int tipo= sc.nextInt();
        if (tipo==1) {
            t.setd(true);
            ment(t);
        }
        else if (tipo==2) {
            t.setd(false);
            ment(t);
        }
        else if (tipo==3){
            System.out.println("insira o valor:");
            float arroz = sc.nextFloat();
            t.setprecopk(arroz);
            ment(t);
        }
        return;
        
    }
    public static void menl(lojas l){
        System.out.println("escolha uma das opcoes:");
        System.out.println("(1) sinalizar uma encomenda");
        System.out.println("(0) exit");
        Scanner sc = new Scanner(System.in);
        int tipo= sc.nextInt();
        if (tipo==1){
            for(Encomenda e:ge.getEncomendas().values()){
                if(e.getcodloja() == l.getcodl()){
                    GPS ut=new GPS();
                    for(utilizador u:mu){
                        if(u.getCodUtilizador().equals(e.getcodcliente())){
                            ut = u.getGPS();
                            break;
                        }
                    }
                    encomendaSinalizada encs= new encomendaSinalizada(e.getcodenc(),ut,l.getGPS());
                    es.add(encs);
                    menl(l);
                    return;
                }
            }
            System.out.println("Nao existem propostas de encomenda para a sua loja");
        }
        return;
    }
    
    
    
    public static void login(){
        Scanner sc = new Scanner(System.in);
        mun=new HashMap<>();
        System.out.println("=====TrazAqui!=====");
        Integer tipo=0;
        while(tipo!=1 && tipo != 2 && tipo != 3 && tipo!=4){
        System.out.println("Insira o tipo de utilizador:");
        System.out.println("(1) cliente");
        System.out.println("(2) voluntario");
        System.out.println("(3) transportador");
        System.out.println("(4) loja");
        tipo= sc.nextInt();
        if (tipo<1 || tipo>4) System.out.println("tipo de utilizador invalido porfavor repita este processo.");
        }
        sc.nextLine();
        System.out.println("Insira o nickname:");
        String nick= sc.nextLine();
        System.out.println("Insira a password:");
        String pass = sc.nextLine(); 
        if(tipo == 1){
            if(mun.containsKey(nick)){
                if( mun.get(nick).equals(pass)){
                    for(utilizador u:mu){
                        if(u.getnome().equals(nick)){
                            menu(u);
                            break;
                        }
                    }
                }
                else{
                    System.out.println("password errada tente novamente:");
                    while(!mun.get(nick).equals(pass)){
                        pass=sc.nextLine();
                    }
                }
            }
            else{
                System.out.println("nickname nao registado porfavor registe-se:");
                registerUtilizador();
            }
        }
        if(tipo == 2){
            if(mvn.containsKey(nick)){
                if( mvn.get(nick).equals(pass)){
                    for(voluntarios u:mv){
                        if(u.getnome().equals(nick)){
                            menv(u);
                            break;
                        }
                    }
                }
                else{
                    System.out.println("password errada tente novamente:");
                    while(!mvn.get(nick).equals(pass)){
                        pass=sc.nextLine();
                    }
                }
            }
            else{
                System.out.println("nickname nao registado porfavor registe-se:");
                registerVoluntario();
            }
        }
        if(tipo == 3){
            if(mtn.containsKey(nick)){
                if( mtn.get(nick).equals(pass)){
                    for(transportadores u:mt){
                        if(u.getnome().equals(nick)){
                            ment(u);
                            break;
                        }
                    }
                }
                else{
                    System.out.println("password errada tente novamente:");
                    while(!mtn.get(nick).equals(pass)){
                        pass=sc.nextLine();
                    }
                }
            }
            else{
                System.out.println("nickname nao registado porfavor registe-se:");
                registerTransportador();
            }
        }
        if(tipo == 4){
            if(mln.containsKey(nick)){
                if(mln.get(nick).equals(pass)){
                    for(voluntarios u:mv){
                        if(u.getnome().equals(nick)){
                            menv(u);
                            break;
                        }
                    }
                }
                else{
                    System.out.println("password errada tente novamente:");
                    while(!mln.get(nick).equals(pass)){
                        pass=sc.nextLine();
                    }
                }
            }
            else{
                System.out.println("nickname nao registado porfavor registe-se:");
                registerloja();
            }
        }
    }
    
    public static void registerUtilizador(){
        Scanner sc2 = new Scanner(System.in);
        System.out.println("=====TrazAqui!=====");
        System.out.println("Insira o seu codigo:");
        String codigo = sc2.nextLine();
        System.out.println(codigo);
        System.out.println("Insira o seu nome:");
        String nome = sc2.nextLine();
        System.out.println(nome);
        System.out.println("Insira a sua localização:");
        Double gpsx = sc2.nextDouble();        
        Double gpsy = sc2.nextDouble();
        System.out.println(gpsx+"x "+gpsy+"y");
        System.out.println("Insira a sua password:");
        sc2.nextLine();
        String pass = sc2.nextLine();
        System.out.println("Confirme a sua password:");
        String pass1 = sc2.nextLine();
        while(!(pass.equals(pass1))){
            System.out.println("A password e a sua confirmação nao correspondem porfavor repita o processo");
            System.out.println("Insira a sua password:");
            pass = sc2.nextLine();
            System.out.println("Confirme a sua password:");
            pass1 = sc2.nextLine();
        }
        mun.put(nome,pass);
        GPS g = new GPS(gpsx,gpsy);
        utilizador u = new utilizador(codigo,nome,g);
        mu.add(u);
    }
    
    public static void registerTransportador(){
        Scanner sc2 = new Scanner(System.in);
        System.out.println("=====TrazAqui!=====");
        System.out.println("Insira o seu codigo:");
        String codigo = sc2.nextLine();
        System.out.println("Insira o seu nome da empresa:");
        String nome = sc2.nextLine();
        System.out.println("Insira a sua localização:");
        Double gpsx = sc2.nextDouble();
        Double gpsy = sc2.nextDouble();
        System.out.println("Insira o seu NIF:");
        int nif = sc2.nextInt();
        System.out.println("Insira o seu raio de ação (em Km):" );
        float km = sc2.nextFloat();
        System.out.println("Insira o preço/Km base:");
        float pk = sc2.nextFloat();
        System.out.println("Insira a sua password:");
        sc2.nextLine();
        String pass = sc2.nextLine();
        System.out.println("Confirme a sua password:");
        String pass1 = sc2.nextLine();
        while(!(pass.equals(pass1))){
            System.out.println("A password e a sua confirmação nao correspondem porfavor repita o processo");
            System.out.println("Insira a sua password:");
            pass = sc2.nextLine();
            System.out.println("Confirme a sua password:");
            pass1 = sc2.nextLine();
        }
        mtn.put(nome,pass);
        GPS g = new GPS(gpsx,gpsy);
        transportadores t= new transportadores(codigo,nome,g,nif,km,pk);
        mt.add(t);
    }
    
    public static void registerVoluntario(){
        Scanner sc2 = new Scanner(System.in);
        System.out.println("=====TrazAqui!=====");
        System.out.println("Insira o seu codigo:");
        String codigo = sc2.nextLine();
        System.out.println("Insira o seu nome:");
        String nome = sc2.nextLine();
        System.out.println("Insira a sua localização:");
        Double gpsx = sc2.nextDouble();
        Double gpsy = sc2.nextDouble();
        System.out.println("Insira o seu raio de ação (em Km):" );
        float km = sc2.nextFloat();  
        System.out.println("Insira a sua password:");
        sc2.nextLine();
        String pass = sc2.nextLine();
               
        System.out.println("Confirme a sua password:");
        String pass1 = sc2.nextLine();
        while(!(pass.equals(pass1))){
            System.out.println("A password e a sua confirmação nao correspondem porfavor repita o processo");
            System.out.println("Insira a sua password:");
            pass = sc2.nextLine();
            System.out.println("Confirme a sua password:");
            pass1 = sc2.nextLine();
        } 
        mvn.put(nome,pass);
        GPS g = new GPS(gpsx,gpsy);
        voluntarios v= new voluntarios(codigo,km,g,nome);
        mv.add(v);
    }
    
    public static void registerloja(){
        Scanner sc2 = new Scanner(System.in);
        System.out.println("=====TrazAqui!=====");
        System.out.println("Insira o seu codigo:");
        String codigo = sc2.nextLine();
        System.out.println("Insira o seu nome:");
        String nome = sc2.nextLine();
        System.out.println("Insira a sua localização:");
        Double gpsx = sc2.nextDouble();
        Double gpsy = sc2.nextDouble();
        System.out.println("Insira a sua password:");
        sc2.nextLine();
        String pass = sc2.nextLine();
               
        System.out.println("Confirme a sua password:");
        String pass1 = sc2.nextLine();
        while(!(pass.equals(pass1))){
            System.out.println("A password e a sua confirmação nao correspondem porfavor repita o processo");
            System.out.println("Insira a sua password:");
            pass = sc2.nextLine();
            System.out.println("Confirme a sua password:");
            pass1 = sc2.nextLine();
        } 
        mln.put(nome,pass);
        GPS g = new GPS(gpsx,gpsy);
        lojas l = new lojas(codigo,nome,g);
        ml.add(l);
    }
    
                  
    public interface interUtilizador{            
        }
    public interface interTransportadores{
        }
    public interface interVoluntarios{
        }
    public interface interLojas{
        }
    
    public static void main(String[] args){
      parse p = new parse();
      System.out.println("sad");
      mu=p.mu;
      mv=p.mv;
      mt=p.mt;
      ml=p.ml;
      a=p.a;
      ge=p.ge;
      login();
    }
}
