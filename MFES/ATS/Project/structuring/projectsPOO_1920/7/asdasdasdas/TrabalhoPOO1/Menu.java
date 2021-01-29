import java.util.Scanner;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.io.*;
import java.awt.Desktop;
import java.util.List;
import java.time.DateTimeException;
import java.util.Random;
import java.util.*; 
import java.nio.charset.StandardCharsets; 
import java.nio.file.*; 
import java.io.*;
import java.lang.Class; 
import java.lang.String;


public class Menu implements Serializable
{     private main t;
    
    
    public static void main(){
        new Menu().menuInicial();
    }
        
    private Menu(){
           Scanner escolha = new Scanner(System.in);
           System.out.println("Escolha 1 para logs do stor, escolha 2 para logs do sistema:");
           int es = escolha.nextInt();
           if(es==1){this.t = main.parse("logs.txt",1);}
           else if (es==2){this.t = main.parse("logs2.txt",2);}
    }
    
    
    
    private void save(){
        try{ 
                this.t.writeFile2("logs2.txt");
        }catch (FileNotFoundException f){
                System.out.print("Erro na gravação!");
        }catch(IOException f){
                System.out.print("Erro na gravação!");
        }
    }
    
    //Corre a aplicação
    public void menuInicial(){
    int flag =0;
        while(flag ==0){
        Scanner escolha = new Scanner(System.in);
        System.out.println("1: Login");
        System.out.println("2: Register");
        System.out.println("3: Top10 Transportadoras ");
        System.out.println("4: Top10 Users ");
        System.out.println("5: Historico de um Voluntario ou Transportadora");
        System.out.println("6: Total Faturado Por uma empresa num determinado periodo");
        System.out.println("0: Close");
        int es = escolha.nextInt();
        if(es==1){
            Scanner e = new Scanner(System.in);
            System.out.println("Introduza o seu e-mail:");
            String email = e.nextLine();
            Scanner password = new Scanner(System.in);
            System.out.println("Introduza a sua password:");
            String p = password.nextLine();
            if(t.LoginInfo(email,p)){
                User x= null; 
                if(t.getUserByEmail(email).equals(x)){
                    System.out.println("Email não existe");
                    this.save(); return ; 
                }
                x=t.getUserByEmail(email);
                String cod = t.getUserByEmail(email).getCodigo();
                if(x.getClass().getName().equals("Buyer") ){this.menuB(cod);}
                else if(x.getClass().getName().equals("Voluntario") ){this.menuV(cod);}
                else if(x.getClass().getName().equals("Loja") ){this.menuL(cod);}
                else if(x.getClass().getName().equals("Transportadora") ){}
                
                }
            else{System.out.println("Email ou pw nao existem");this.save();this.t = main.parse("logs2.txt",2);}
            }
        else if(es==2){
            this.register();
        }
        else if(es==3){t.top10Empresas();}
        else if(es==4){t.top10Users();}
        else if(es==5){t.faturadoEmp();}
        else if(es==6){t.faturadoEmp();}
        else { save();break;  }
            
    }
}
    
public void register(){
        System.out.println("Quer se registar como?");
        System.out.println("1: Buyer");
        System.out.println("2: Voluntario ");
        System.out.println("3: Transportadora ");
        System.out.println("4: loja");
        System.out.println("0: Close");
        Scanner escolha = new Scanner(System.in);
        int es = escolha.nextInt(); 
        if(es==1){String codbuyerF=registerbuyer();if(codbuyerF.equals("0")){}else{this.menuB(codbuyerF);}}
        else if(es==2){String codvolF=registerVoluntario();if(codvolF.equals("0")){}else{this.menuV(codvolF);}}
        else if(es==3){String codtransF=registerTransportadora();if(codtransF.equals("0")){}else{this.menuT(codtransF);}}
        else if(es==4){String codLojF=registerLoja();if(codLojF.equals("0")){}else{this.menuL(codLojF);}}
        else{save();
            this.t = main.parse("logs2.txt",2);}

    }
    // username,codigo,password,lx,ly,email, custokm , raio ,nif , aval, numrev
    public String registerTransportadora(){
        Transportadora b= null ;
        String result ="0";
        Scanner e = new Scanner(System.in);
        System.out.println("insira email:");
        String email = e.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("insira uma password: ");
        String password = p.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("insira o nome da empresa: ");
        String name = n.nextLine();
        
        
        Scanner x1 = new Scanner(System.in);
        System.out.println("insira as suas coordenadas:");
        System.out.println("Coordenada X");
        String x = x1.nextLine();
        Scanner y1 = new Scanner(System.in);
        System.out.println("Coordenada Y");
        String y = y1.nextLine();
        
        Scanner custo1 = new Scanner(System.in);
        System.out.println("insira o custo por km:");
        String custo = custo1.nextLine();
        
        Scanner r1 = new Scanner(System.in);
        System.out.println("insira o raio em que pode fazer entregas:");
        String r = r1.nextLine();
        
        Scanner nif1 = new Scanner(System.in);
        System.out.println("insira o nif da empresa:");
        String nif = nif1.nextLine();
        
        String codigo = "t"+this.getRandomNumberString();
        while(t.checkCodigo(codigo) == false ){
            codigo = "l"+this.getRandomNumberString();}
        
        try{
             b= new Transportadora(name,codigo,password,Double.parseDouble(x),Double.parseDouble(y),email,
             Double.parseDouble(custo), Double.parseDouble(r),Integer.parseInt(nif), 0, 0);
        }
        catch(NullPointerException erro){
            System.out.println("Erro no preenchimento!");
            
            return result;
        }
        
        if (this.t.addTrans(b)==1){System.out.println("registo feito com sucesso ");save();return codigo; }
        return result; 
    }
    //username,codigo,password,lx,ly,email, raio,livre,numreviews,avaliacao
    public String registerVoluntario(){
        Voluntario b= null ;
        String result ="0";
        Scanner e = new Scanner(System.in);
        System.out.println("insira email:");
        String email = e.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("insira uma password: ");
        String password = p.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("insira o seu nome: ");
        String name = n.nextLine();
        
        
        Scanner x1 = new Scanner(System.in);
        System.out.println("insira as suas coordenadas:");
        System.out.println("Coordenada X");
        String x = x1.nextLine();
        Scanner y1 = new Scanner(System.in);
        System.out.println("Coordenada Y");
        String y = y1.nextLine();
        
        Scanner r1 = new Scanner(System.in);
        System.out.println("insira o raio em que pode fazer entregas:");
        String r = r1.nextLine();
        
        String codigo = "v"+this.getRandomNumberString();
        while(t.checkCodigo(codigo) == false ){
            codigo = "l"+this.getRandomNumberString();}
        
        try{
             b= new Voluntario(name,codigo,password,Double.parseDouble(x),Double.parseDouble(y),email,
             Double.parseDouble(r),true, 0, 0);
        }
        catch(NullPointerException erro){
            System.out.println("Erro no preenchimento!");
            return result;
        }
        
        if (this.t.addVol(b)==1){System.out.println("registo feito com sucesso ");save();return codigo; }
        
      return result;  
    }
    //username,codigo,password,lx,ly,email
    public String registerLoja(){
        Loja b= null ;
        String result ="0";
        Scanner e = new Scanner(System.in);
        System.out.println("insira email:");
        String email = e.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("insira uma password: ");
        String password = p.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("insira o nome da loja: ");
        String name = n.nextLine();
        
        
        Scanner x1 = new Scanner(System.in);
        System.out.println("insira as suas coordenadas:");
        System.out.println("Coordenada X");
        String x = x1.nextLine();
        Scanner y1 = new Scanner(System.in);
        System.out.println("Coordenada Y");
        String y = y1.nextLine();
        
        String codigo = "l"+this.getRandomNumberString();
        while(t.checkCodigo(codigo) == false ){
            codigo = "l"+this.getRandomNumberString();
        }
        
        try{
            b = new Loja(name,codigo,password,Double.parseDouble(x),Double.parseDouble(y),email);
        }
        catch(NullPointerException erro){
            System.out.println("Erro no preenchimento!");
            return result; 
        }
        
        if (this.t.addLoja(b)==1){System.out.println("registo feito com sucesso ");save();return codigo;}
        return result ; 
    }
    //username,codigo,password,lx,ly,email
    public String registerbuyer(){
        Buyer b= null ;String result ="0";
        Scanner e = new Scanner(System.in);
        System.out.println("insira email:");
        String email = e.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("insira uma password: ");
        String password = p.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("insira o seu nome: ");
        String name = n.nextLine();
        
        
        Scanner x1 = new Scanner(System.in);
        System.out.println("insira as suas coordenadas:");
        System.out.println("Coordenada X");
        String x = x1.nextLine();
        Scanner y1 = new Scanner(System.in);
        System.out.println("Coordenada Y");
        String y = y1.nextLine();
        
        String codigo = "u"+this.getRandomNumberString();
        while(t.checkCodigo(codigo) == false ){
            codigo = "u"+this.getRandomNumberString();}
        
        try{
            b = new Buyer(name,codigo,password,Double.parseDouble(x),Double.parseDouble(y),email);
        }
        catch(NullPointerException erro){
            System.out.println("Erro no preenchimento!");
            return result;
        }
        
        if (this.t.addBuy(b)==1){System.out.println("registo feito com sucesso ");save();return codigo; }
        return result ; 
    }
    
    
    
    
    public String getRandomNumberString() {
     // It will generate 6 digit random Number.
     // from 0 to 999999
     Random rnd = new Random();
     int number = rnd.nextInt(999999);

     // this will convert any number sequence into 6 character.
     return String.format("%06d", number);
    }
    
    public void menuT(String codigo){
        int i =0;
        User volun = t.getUserByCodigo(codigo);

        while(i==0){
            Scanner optn = new Scanner(System.in);
            System.out.println("1: Levantar Encomenda"); // pronto para fazer encomendas ou nao 
            System.out.println("2: Marcar encomenda entregue");
            System.out.println("3: Historico");
            //System.out.println("4: Encomendas durante uma certa data"); // se tiver tempo----------- fazer tb voluntariado nao esquecer 
            System.out.println("0: Sair");
            int op = optn.nextInt();
            
            if(op==1){
                System.out.println("Insira codigo de encomenda para ser levantada");
                Scanner codEncMenuT1 = new Scanner(System.in);
                String codEncMenuT = codEncMenuT1.nextLine();
                
                t.mudaestadoENC(codEncMenuT,6);
                
                
                
            }else if(op==2){
                System.out.println("Insira codigo de encomenda que foi entregue");
                Scanner codEncMenuT1 = new Scanner(System.in);
                String codEncMenuT = codEncMenuT1.nextLine();
                
                t.mudaestadoENC(codEncMenuT,6);
                 
                      
                
                
                
            }else if(op==3){t.PrintHistT(codigo);
            }
            else{this.save();break;}
       }
     }
    
    public void menuL(String codigo){
        int i =0;
        User cloja = t.getUserByCodigo(codigo);

        while(i==0){
            Scanner optn = new Scanner(System.in);
            System.out.println("1: Encomenda pronta"); // pronto para fazer encomendas ou nao 
             System.out.println("2: Pedir random voluntario");
            System.out.println("0: Sair");
            int op = optn.nextInt();
            
            if(op==1){
                System.out.println("Insira codigo de encomenda para ser levantada");
                Scanner codEncMenuL1 = new Scanner(System.in);
                String codEncMenu = codEncMenuL1.nextLine();
                if(t.TransportadoraOuVoluntario(codEncMenu)==1){t.mudaestadoENC(codEncMenu,4);}
                else if(t.TransportadoraOuVoluntario(codEncMenu)==2){t.mudaestadoENC(codEncMenu,3);}
                else if(t.TransportadoraOuVoluntario(codEncMenu)==0){System.out.println(" codigo de encomenda errado ");}
                
                
                }else if(op==2) {
                System.out.println("Insira codigo de encomenda para pedir a um voluntario");
                Scanner codEncMenuL2 = new Scanner(System.in);
                String codEncMenu1 = codEncMenuL2.nextLine();
                
               
                    
                    
                if(t.TransportadoraOuVoluntario(codEncMenu1)==0){
                    String codV = t.getVolLivre(codEncMenu1);
                    t.setDelieverV(codV,codEncMenu1);
                    t.mudaestadoENC(codEncMenu1,8);
                }
                    
                    
                }
                
            else{this.save();break;}
                
        
        }
    }

    
    
    public void menuV(String codigo){
        int i =0;
        User volun = t.getUserByCodigo(codigo);

        while(i==0){
            Scanner optn = new Scanner(System.in);
            System.out.println("1: Mudar Estado"); // pronto para fazer encomendas ou nao 
            System.out.println("2: Escolher encomendas disponiveis dentro de raio: "); 
            System.out.println("3: Marcar encomenda entregue");
            System.out.println("4: Historico");
             System.out.println("5: Ver Encomendas para aceitar");
            System.out.println("0: Sair");
            int op = optn.nextInt();
            
            if(op==1){
                if(t.EncomendaAtualV(codigo).equals("free")){
                t.mudaestadoVoluntario(volun);}
                else{System.out.println("Tem de entregar a encomenda atual");}
                
            }else if(op==2){
                if(t.EncomendaAtualV(codigo).equals("free")){
                List<Encomenda> encomendas = this.t.EncomendasParaVoluntarioDentroRaio(codigo);
                Scanner l = new Scanner(System.in);
                int i1 = 0;
               
                System.out.println("Escolha uma Encomenda: ");
                
                for(Encomenda b : encomendas){
                    System.out.println(i1 + " - " + b.getcodEncomenda());
                    i1++;
                }
                int input = l.nextInt();
                if(0<=input && input <i1){
                    Encomenda encf = (Encomenda) encomendas.get(input);
                    String codigoenc= encf.getcodEncomenda();
                    t.mudaestadoENC(codigoenc,6);
                    t.mudaestadoVoluntario(volun);
                    t.setDelieverV(codigo,codigoenc);
                    
                }else{System.out.println("Tem de entregar a encomenda atual");}}
                               
                
                
            }else if(op==3){
                 if(t.EncomendaAtualV(codigo).equals("free")){System.out.println("Não tem encomendas para marcar como entregues");
                    }
                 else{String codigoenc= t.EncomendaAtualV(codigo);
                      t.mudaestadoENC(codigoenc,1);
                      
                    }   
                
                
                
            }else if(op==4){t.PrintHistV(codigo);}
             else if(op==5){
                 if(t.EncPedidas(codigo)!= "n"){
                     String s = t.EncPedidas(codigo);
                     Scanner optn1 = new Scanner(System.in);
                     System.out.println("Aceita a encomenda"+s+"?");
                     System.out.println("1.sim 2.nao:");
                     int op1 = optn1.nextInt();
                     if(op==1){
                            t.mudaestadoENC(s,6);
                            t.mudaestadoVoluntario(volun);
                            t.setDelieverV(codigo,s);
                        }
                }
            }   
            
            else{this.save();break;}
       }
     }
    
    
    
    public void menuB(String codigo){
        int i =0;

        while(i==0){
            Scanner optn = new Scanner(System.in);
            System.out.println("1: Fazer uma encomenda");
            System.out.println("2: Classificar Entregas");
            System.out.println("4: Historico");
            System.out.println("0: Sair");
            int op = optn.nextInt();
            
            if(op==1){
                List<Loja> lojas = this.t.Tlojas();
                Scanner l = new Scanner(System.in);
                int i1 = 0;
               
                System.out.println("Escolha uma loja: ");
                
                for(Loja loja : lojas){
                    System.out.println(i1 + " - " + loja.getUserName());
                    i1++;
                }
                int input = l.nextInt();
                if(0<=input && input <i1){
                    Loja lojaf = (Loja) lojas.get(input);
                    Encomenda e = new Encomenda();
                    String codenc = "e"+this.getRandomNumberString();
                    String codu = codigo;
                    String codlo=  lojaf.getEmail();
                    String codT = "";
                    int estado = 2;
                
                    ArrayList<LinhaEncomenda> produto = new ArrayList<>();
                    int flag =0;
                    double peso2=0;
                    while(flag==0){
                        int escolha2=0;
                        System.out.println("insira codigo do produto ");
                        Scanner cp = new Scanner(System.in);
                        String codp = cp.nextLine();
                        
                        System.out.println("insira a quantidade");
                        Scanner qp = new Scanner(System.in);
                        Double quantp = qp.nextDouble();
                        
                        double preco=3;
                        double preco2=3.0*quantp;
                        peso2=peso2+1*quantp;
                        
                        LinhaEncomenda linha = new LinhaEncomenda(codp,"",preco2,quantp);
                        produto.add(linha);
                        
                        System.out.println("quer continuar a comprar?");
                        System.out.println("1: Sim");
                        System.out.println("2: Nao");
                        Scanner oplinhaenc = new Scanner(System.in);
                        escolha2 = oplinhaenc.nextInt();
                        if(escolha2==2){flag=1;} 
                        
                        
                    }

                    System.out.println("quer Transporte?");
                    System.out.println("1: Sim");
                    System.out.println("2: Nao");
                    Scanner querTrans1 = new Scanner(System.in);
                    int querTrans = querTrans1.nextInt();
                    if(querTrans ==1){
                        //TransportadoraMaisBarata(String codBuyer,String codLoja, double peso)
                        String trasnporte =t.TransportadoraMaisBarata(codu,codlo,peso2);
                        if(trasnporte.equals("n")){}
                        else{
                        //custoTransporte1 (String codBuyer , String codTransp,String codLoja, double peso )
                        double custoTrans=t.custoTransporte1(codu,trasnporte,codlo,peso2);
                        System.out.println("Pela Transportadora mais barata o preco fica:"+custoTrans); 
                        System.out.println("Aceita?");
                        System.out.println("1: Sim");
                        System.out.println("2: Nao");
                        Scanner aceitaTrans1 = new Scanner(System.in);
                        int aceitaTrans = querTrans1.nextInt();
                        if(aceitaTrans ==1){codT=trasnporte;estado=5;}}
                    }
                    
                    Encomenda  novaenc = new Encomenda(codenc,codu,codlo,codT, peso2 , null ,estado , produto);
                    System.out.println("Encomenda feita com sucesso");
                    this.t.addEnc(novaenc);
                    
                }
                else{
                    System.out.println("Tente outra opcao");
                }
                
            }else if(op==2){
                
                List<Encomenda> enc = this.t.EncomendasEntreguesSemReviews(codigo);
                
                System.out.println("As suas encomendas:");
                    for(Encomenda e : enc){
                        System.out.println("Encomenda :" + e.getcodEncomenda() + "; Entregue por: " + e.getcodDeliver() + t.getUserByCodigo(e.getcodDeliver()).getUserName());
                    
                }
                System.out.println("Escreva um codigo de encomenda para avaliar");
                Scanner codencaval1 = new Scanner(System.in);
                String codencaval = codencaval1.nextLine();
                
                int flagcod =1;
                for(Encomenda e : enc){
                    if(e.getcodEncomenda().equals(codencaval1)){flagcod=0;break;}
                    
                }
                
                if(flagcod==0){
                System.out.println("a sua review entre 0 e 10");
                Scanner reviewenc1 = new Scanner(System.in);
                double reviewenc= reviewenc1.nextDouble();
                if(0<= reviewenc && reviewenc<=10){
                 if(t.reviews(codencaval,reviewenc)){System.out.println("review feita com sucesso");}
                }}
      
                
                
                
            }
            else{ this.save();break;}
       }
     }
}
        

    
    
    