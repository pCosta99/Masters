import java.io.*;
import java.util.*;
import java.util.Scanner;
import java.time.LocalDate;
import java.util.stream.Collectors;
import java.util.Comparator;
public class TrazAquiApp {

    private Parse p =new Parse();
    
    private String nomeFich1,nomeFich2;
    private Menu menuPrincipal, menu1,menuU,menuV,menuL,menuT,menuProd,menuEnc,menuA;
    
    /**
     * O método main cria a aplicação e invoca o método run()
     */
    public static void main(String[] args) {
        new TrazAquiApp().run();
    }
    
    /**
     * Construtor.
     * 
     * Cria os menus.
     */
    
    private TrazAquiApp() {
        // Criar o menu 
        String[] opcoes = {
            "Utilizador",//feito
            "Voluntario",//feito
            "Loja",//feito
            "Transportadora"//feito
        };
        
        
        String[] opcoes1 = {
            "Já possui conta",//feito
            "Não possui conta",//feito
        };
                                 
        String[] opcoesU={
            "Alterar morada",//Feito
            "Criar encomenda",//Feito
            "Aceitar Entregas",
            "Ver Encomendas Já Recebidas",//Feito
            "Ver Encomendas por Receber",//Feito
            "Classificar Entregas",//Feito
            "10 Utilizadores Que Mais Utilizam O APP",//Feito
            "10 Transportadoras Que Mais Utilizam O APP"//Feito
        };
        
        
    
        String[] opcoesT={
            "Alterar morada",//Feito
            "Alterar Raio de Atividade",//Feito
            "Mudar Preços",//Feito
            "Alterar Disponibilidade",//Feito
            "Transportar Encomendas",//Feito
            "Ver Encomendas Entregues",//Feito
            "Ver Encomendas Por Entregar",//Feito
            "Registar Encomenda Como Entregue",//Feito
            "Ver Total Fatorado",//Feito
            "10 Utilizadores Que Mais Utilizam O APP",//Feito
            "10 Transportadoras Que Mais Utilizam O APP",//Feito
            "Pode Levar Encomendas Medicas"//Feito
            };
            
        String[] opcoesV={
            "Alterar morada",//Feito
            "Alterar Raio de Atividade",//Feito
            "Alterar Disponibilidade",//Feito
            "Transportar Encomendas",//Feito
            "Ver Encomendas Entregues",//Feito
            "Ver Encomendas Por Entregar",//Feito
            "Registar Encomenda Como Entregue",//Feito
            "10 Utilizadores Que Mais Utilizam O APP",//Feito
            "10 Transportadoras Que Mais Utilizam O APP",//Feito
            "Pode Levar Encomendas Medicas"//Feito
        };
        
        String[] opcoesL={
            "Alterar morada",//Feito
            "Acrescentar Produtos",//Feito
            "Alterar Disponibilidade",//Feito
            "Encomendas Prontas",//Feito
            "Encomendas Por Fazer",//Feito
            "Registar Encomenda Como Feita",//Feito
            "10 Utilizadores Que Mais Utilizam O APP",//Feito
            "10 Transportadoras Que Mais Utilizam O APP",//Feito
            "Retirar Produto"//Feito
        };
         String[] opcoesA={"Aceitar","Não Aceitar"};
        String[] opcoesProd={"Adicionar Produto","Finalizar Encomenda"};
        String[] opcoesEnc={"Encomenda Medica","Encomenda Normal"};
        this.menuPrincipal = new Menu(opcoes);        
        this.menu1 = new Menu(opcoes1);
        this.menuU=new Menu(opcoesU);
        this.menuV=new Menu(opcoesV);
        this.menuL=new Menu(opcoesL);
        this.menuT=new Menu(opcoesT);
        this.menuA=new Menu(opcoesA);
        this.menuProd=new Menu(opcoesProd);
        this.menuEnc=new Menu(opcoesEnc);
        
        this.LerEscrever("a.txt","a.txt");
        
    }

    
    
    
    
    private void LerEscrever(String a,String b){
        Scanner input = new Scanner(System.in);
    
            
            this.nomeFich1=a;
            this.p.parse(this.nomeFich1);
            
        try {  
            this.nomeFich2=b;
            this.p.escrevernoficheiro(this.nomeFich2);
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
}
    
private void CriaEncomendaMedica(String u,List<LinhaDeEncomenda> ale,String nloja,String codl){
     
     do{menuProd.executa("");
       switch(menuProd.getOpcao()){
       case 1:Scanner input = new Scanner(System.in);
               System.out.println("Diga o Codigo do Produto");
              String codprod=input.nextLine();
              
              System.out.println("Diga o Nome do Produto");
              String nl=input.nextLine();
             
              System.out.println("Diga a Quantidade");
              int quant=input.nextInt();
              System.out.println(u);
              int r=0;
              LinhaDeEncomenda linha=new LinhaDeEncomenda(nl,quant,codprod,0);
              for(LinhaDeEncomenda l:ale){
                  
                  
                  if(l.equals(linha)){
                      linha.setPreco(l.getPreco());
                      r=1;
                      break;
                    }
                  
                }
             
              if(r==0){ 
                  System.out.println("Deve Ter Escrito Mal o Nome ou o Codigo");
                  break;
                }
              else{
                  System.out.println(u); 
                  try{
                  this.p.getEncomendaMedica(u).adicionaLinha(linha);
                }
                catch(EncomendaException er){
                    System.out.println("Erro");
                    break;
                }
                 }
                
           
              input.close();
              break;
    case 2: 
           try{
                    this.p.getLojas(nloja,codl).adicionaLinha1(u);
                }
                
                catch(LojasException ex){
                    System.out.println("Erro");
                    break;
                }
           this.Escrever();
           break;
            }
    }while(menuProd.getOpcao()!=0 && menuProd.getOpcao()!=2);
}


    
private void CriaEncomenda(String u,List<LinhaDeEncomenda> ale,String nloja,String codl){
     
     do{menuProd.executa("");
       switch(menuProd.getOpcao()){
       case 1:
              Scanner input = new Scanner(System.in);
              System.out.println("Diga o Codigo do Produto");
              String codprod=input.nextLine();
              
              System.out.println("Diga o Nome do Produto");
              String nl=input.nextLine();
              
              System.out.println("Diga a Quantidade");
              int quant=input.nextInt();
              System.out.println(u);
              int r=0;
              LinhaDeEncomenda linha=new LinhaDeEncomenda(nl,quant,codprod,0);
              for(LinhaDeEncomenda l:ale){
                  if(l.equals(linha)){
                      linha.setPreco(l.getPreco());
                      r=1;
                      break;
                    }
                  
                }
                
              
              
              if(r==0){ 
                  System.out.println("Deve Ter Escrito Mal o Nome ou o Codigo");
                  break;
                }
              
                else{
                  System.out.println(u); 
                  try{
                  this.p.getEncomenda(u).adicionaLinha(linha);
                }
                catch(EncomendaException er){
                    System.out.println("Erro");
                    break;
                }
            }
               
            
              input.close();
              break;
            
    case 2:  try{
                    this.p.getLojas(nloja,codl).adicionaLinha1(u);
                }
                
                catch(LojasException ex){
                    System.out.println("Erro");
                    break;
                }
           this.Escrever();
           break;
            }
    }while(menuProd.getOpcao()!=0 && menuProd.getOpcao()!=2);
}



        
    private void Escrever(){
        try {  
            this.p.escrevernoficheiro(this.nomeFich2);
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
    
    }

    /**
     * Executa o menu principal e invoca o método correspondente à opção seleccionada.
     */
    
        
    private void run() {
        
        
        Scanner input = new Scanner(System.in);
        do {
            String m="\nJá possui conta?\n";
            menu1.executa(m);
            
            
            switch (menu1.getOpcao()) {
                
                case 1: System.out.println("\nOk\n");
                        
                do {
                    String v="Você é?";
                    menuPrincipal.executa(v);
                    
                    switch (menuPrincipal.getOpcao()) {
                        
                        case 1: input = new Scanner(System.in);
                    
                                System.out.println("Utilizador\n");
                        
                                System.out.println("Diga o seu email(nome)\n");
                                String nomeu=input.nextLine();
                                
                                System.out.println("Diga a sua password(codigo)\n");
                                String codu=input.nextLine();
                                
                                Utilizador u=new Utilizador();
                                try{
                                     u=this.p.getUtilizador(nomeu,codu);
                                    
                                }
                                
                                catch(UtilizadorException e){
                                
                                System.out.println("Email ou password errados\n");
                                break;
                                }
                                input.close();
                                
                                do{
                                    String mu="\nUtilizador\n";
                                    menuU.executa(mu);
                                    
                                    switch (menuU.getOpcao()) {
                
                                     case 1:
                                            input = new Scanner(System.in);
                                            System.out.println("Alterar morada");
                                            System.out.println("Diga a latitude");
                                            double lat=input.nextDouble();
                                            System.out.println("Diga a longitude");
                                            double lon=input.nextDouble();
                                            u.setX(lat);
                                            u.setY(lon);
                                            input.close();
                                            this.Escrever();
                                            
                                            break;
                                             
                                     case 2:
                                            System.out.println("CriarEncomeda");
                                            
                                            for(Lojas l:this.p.getAL()){
                                                if(l.getDisp()==true){
                                                    if(l.getStock().size()==0)
                                                        System.out.println("Codigo:"+l.getCodloja()
                                                        +",Nome:"+l.getNomeloja()
                                                        +",Não Tem Stock"
                                                        +","+l.TempodeEspera()+" h Tempo Medio de Espera" );
                                                    else System.out.println("Codigo:"+l.getCodloja()
                                                    +",Nome:"+l.getNomeloja()
                                                    +",Tem Stock"
                                                    +","+l.TempodeEspera()+" h Tempo Medio de Espera");
                                                }
                                            }
                                            
                                            input = new Scanner(System.in);
                                            System.out.println("Diga o Codigo da Loja");
                                            String codl=input.nextLine();
                                            System.out.println("Diga o Nome da Loja");
                                            String nl=input.nextLine();
                                            input.close();
                                            
                                            try{Lojas loja=this.p.getLojas(nl,codl);
                                                if(loja.getStock().size()==0) {System.out.println("Não Escolheu um loja com stock");break;}
                                                else {
                                                    for(LinhaDeEncomenda lde:loja.getStock()){
                                                        System.out.println("Codigo:"+lde.getCod()+",Nome:"+lde.getDescricao()+",Preço:"+lde.getPreco());
                                                    }
                                                }  
                                                    do{
                                                        menuEnc.executa("");
                                                        switch(menuEnc.getOpcao()){
                                                            case 1:System.out.println("Criando Encomenda Medica");
                                                                   String encm=this.p.addEncomendaMedica(u.getCodigo(),codl);
                                                                   this.CriaEncomendaMedica(encm,loja.getStock(),nl,codl);
                                                                   break;
                                                            
                                                            case 2:System.out.println("Criando Encomenda");
                                                                   String enc=this.p.addEncomenda(u.getCodigo(),codl);
                                                                   this.CriaEncomenda(enc,loja.getStock(),nl,codl);
                                                                   break;
                                                        }
                                                        
                                                }while(menuEnc.getOpcao()!=0);
                                            }
                                            catch(LojasException ex){System.out.println("Escreveu mal");break;}
                                            break;

                                     case 3:
                                            System.out.println("Aceitar Entregas");
                                            for(Pedidos p:u.getPedidos()){
                                                String cd=this.p.getEncomenda1(p.getCodenc()).getCodloja();
                                                double x=this.p.getTransportadoras(p.getCodtransp()).precoencomenda(u.getX(),u.getY(),this.p.getLojas(cd).getX(),this.p.getLojas(cd).getY(),this.p.getEncomenda1(p.getCodenc()).getPeso());
                                                System.out.println(p.toString()+",Preco: "+x);
                                            }
                                            do{
                                                        menuA.executa("");
                                                        switch(menuA.getOpcao()){
                                                            case 1:System.out.println("Aceitar");
                                                                   input = new Scanner(System.in);
                                                                   System.out.println("Diga o Codigo de Encomenda");
                                                                   String ej=input.nextLine();
                                                                   List<Pedidos> ped=new ArrayList<>();
                                                                   
                                                                   for(Pedidos pe:u.getPedidos()){
                                                                       if(pe.getCodenc().equals(ej))this.p.addEncomendasAceites(ej,pe.getCodtransp());
                                                                       else ped.add(pe);
                                                                    }
                                                                    
                                                                   
                                                                   
                                                                   u.setPedidos(ped);
                                                                   input.close();
                                                                   
                                                                   this.Escrever();
                                                                   break;
                                                                   
                                                            case 2:System.out.println("Não Aceitar");
                                                                   input = new Scanner(System.in);
                                                                   System.out.println("Diga o Codigo de Encomenda");
                                                                   String ed=input.nextLine();
                                                                   List<Pedidos> pei=new ArrayList<>();
                                                                   
                                                                   for(Pedidos p:u.getPedidos()){
                                                                       if(!p.getCodenc().equals(ed))pei.add(p);
                                                                    }
                                                                   
                                                                    u.setPedidos(pei);
                                                                   input.close();
                                                                   
                                                                   this.Escrever();
                                                                   break;
                                                        }
                                                        
                                                }while(menuA.getOpcao()!=0);
                                            break;
                                            
            
                                     case 4:
                                            System.out.println("Ver Encomendas Já Recebidas");
                                            try{
                                                List<Encomendas> enc=this.p.AllEncomendasEntreguesUtilizador(u.getCodigo());
                                            for(Encomendas e:enc){
                                                System.out.println(e.toString());
                                            }
                                        }
                                            catch(EncomendaException ex){
                                                System.out.println("Nao fez nenhuma encomenda");
                                            }
                                            break;
                                            
                                     case 5:
                                            System.out.println("Ver Encomendas por Receber");
                                            List<Encomendas> enc=this.p.AllEncomendasNEntreguesUtilizador(u.getCodigo());
                                            
                                            for(Encomendas e:enc){
                                                System.out.println(e.toString());
                                            }
                                            
                                            break;
                                            
                                     case 6:       
                                            System.out.println("Classificar");
                                            for(Transportadoras te:this.p.getAT())System.out.println(te.toString()+"\n");
                                            
                                            input = new Scanner(System.in);
                                            System.out.println("Escolha a Transportadora");
                                            System.out.println("Diga o Nome");
                                            String nt=input.nextLine();
                                            System.out.println("Diga o Codigo");
                                            String ct=input.nextLine();
                                            System.out.println("Diga a Classificação");
                                            int c=input.nextInt();
                                            input.close();
                                            
                                            try{
                                                this.p.getTransportadoras(nt,ct).setC(c);
                                            }
                                            catch(TransportadoraException te){
                                                System.out.println("Escreveu algo errado");
                                            }
                                            
                                            this.Escrever();
                                            break;
                                     case 7:
                                            System.out.println("10 Utilizadores Que Mais Utilizam O APP");
                                            for(Utilizador ju:this.p.UU())System.out.println(ju+"\n");
                                            break;
                                            
                                     case 8:
                                            System.out.println("10 Transportadoras Que Mais Utilizam O APP");
                                            for(Transportadoras ju:this.p.UT())System.out.println(ju+"\n");
                                            break;
                                        }
                                    }while (menuU.getOpcao()!=0); 
                                    System.out.println("\nAté breve!...\n");// A opção 0 é usada para sair do menu.
                                     
                                    break;
                            
                        case 2: 
                                System.out.println("Voluntario\n");
                                input = new Scanner(System.in);
                                System.out.println("Diga o seu email(nome)\n");
                                String nomev=input.nextLine();
                                
                                System.out.println("Diga a sua password(codigo)\n");
                                String codv=input.nextLine();
                                input.close();
                                Voluntarios vol = new Voluntarios();
                                try{
                                    vol=this.p.getVoluntarios(nomev,codv);
                                }
                                
                                catch(VoluntariosException e){
                                System.out.println("Email ou password errados\n");
                                break;
                                }
                                
                                do{
                                String mv="\nVoluntarios\n";
                                menuV.executa(mv);
                                switch (menuV.getOpcao()) {
                
                                     case 1:
                                            input = new Scanner(System.in);
                                            System.out.println("Alterar morada");
                                            System.out.println("Diga a latitude");
                                            double lat=input.nextDouble();
                                            System.out.println("Diga a longitude");
                                            double lon=input.nextDouble();
                                            vol.setX(lat);
                                            vol.setY(lon);
                                            input.close();
                                            this.Escrever();
                                            
                                            break;
                                             
                                     case 2:
                                            input = new Scanner(System.in);
                                            System.out.println("Alterar Raio de Entrega");
                                            System.out.println("Diga o Novo Raio de Entrega");
                                            double raio=input.nextDouble();
                                            vol.setRaio(raio);
                                            input.close();
                                            this.Escrever();
                                            break;
           
                                            
                                            
                                     case 3:
                                            System.out.println("Alterar Disponibilidade");
                                            if(vol.getDisp()==true){
                                                vol.setDisp(false);
                                                System.out.println("Mudado para indisponivel");
                                            }
                                            else{
                                                vol.setDisp(true);
                                                System.out.println("Mudado para disponivel");
                                            }
                                            this.Escrever();
                                            
                                            break;
                                            
                                     case 4:
                                            System.out.println("Transportar Encomendas");
                                            String cu="";
                                            for(Lojas lj:this.p.getAL()){
                                                if(vol.dentroRaio(lj.getX(),lj.getY())==true){
                                                    for(String lkj:lj.getPE()){
                                                    try{
                                                        cu=this.p.getEncomenda(lkj).getCodutilizador();
                                                    }
                                                    catch(EncomendaException ex){
                                                        if(vol.getMed()==true){
                                                        try{
                                                            cu=this.p.getEncomendaMedica(lkj).getCodutilizador();
                                                        }
                                                    catch(EncomendaException e){
                                                        System.out.println("Erroer");
                                                    }
                                                }
                                                        
                                                    }
                                                
                                                    if(vol.dentroRaio2(this.p.getUtilizador(cu).getX(),this.p.getUtilizador(cu).getY())==true)System.out.println(lkj);}
                                                }
                                            }
                                            
                                            input = new Scanner(System.in);
                                            System.out.println("Diga o numero de encomenda");
                                            String nenc=input.nextLine();
                                            if(nenc.equals("")){System.out.println("Não escreveu nenhum codigo");break;}
                                            try{
                                                this.p.getEncomenda(nenc);
                                            }
                                            catch(EncomendaException ex){
                                                try{
                                                    this.p.getEncomendaMedica(nenc);
                                                }
                                                    catch(EncomendaException e){
                                                        System.out.println("N Escreveu Nenhuma Encomenda Valida");break;
                                                    }
                                                }
                                                    
                                            input.close();
                                            this.p.addEncomendasAceites(nenc,vol.getCodVol());
                                            this.Escrever();
                                            break;
                                            
                                     case 5:
                                             System.out.println("Ver Encomendas Entregues");
                                            for(EncomendasAceites ea:this.p.getAEA()){
                                            for(Lojas lj:this.p.getAL()){
                                            for(String ee:lj.getE()){
                                            if(ee.equals(ea.getEA())){
                                                if(ea.getQ().equals(vol.getCodVol())){
                                                    try{
                                                        System.out.println(this.p.getEncomenda(ea.getEA()));
                                                    }
                                                    catch(EncomendaException e){
                                                    try{System.out.println(this.p.getEncomendaMedica(ea.getEA()));
                                                    }
                                                    catch(EncomendaException ex){
                                                    System.out.println("Nao Entregou Nenhuma Encomenda");
                                                }
                                            }}
                                        }
                                    }
                                }
                            }
                                            
                                            break;
                                            
                                            
                                     case 6:
                                            System.out.println("Ver Encomendas Por Entregar");
                                            for(EncomendasAceites ea:this.p.getAEA()){
                                            for(Lojas lj:this.p.getAL()){
                                            for(String ee:lj.getPE()){
                                            if(ee.equals(ea.getEA())){
                                                if(ea.getQ().equals(vol.getCodVol())){
                                                    try{
                                                        System.out.println(this.p.getEncomenda(ea.getEA()));
                                                    }
                                                    catch(EncomendaException e){
                                                    try{
                                                        System.out.println(this.p.getEncomendaMedica(ea.getEA()));
                                                    }
                                                    catch(EncomendaException ex){
                                                    System.out.println("Nao Tem Nenhuma Encomenda Por Entregar");
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                                            break;
                                           
                                            
                                     case 7:
                                            System.out.println("Registar Encomenda Como Entregue");
                                            input = new Scanner(System.in);
                                            System.out.println("Diga o numero de encomenda");
                                            String numenc=input.nextLine();
                                            input.close();
                                            String nloja;
                                            try{
                                                nloja=this.p.getEncomenda(numenc).getCodloja();this.p.getEncomenda(numenc).setTempo1(LocalDate.now());
                                            }
                                            catch(EncomendaException ex){
                                            try{
                                                nloja=this.p.getEncomendaMedica(numenc).getCodloja();
                                                this.p.getEncomendaMedica(numenc).setTempo1(LocalDate.now());
                                            }
                                            catch(EncomendaException e){
                                                System.out.println("Diga um numero de necomenda valido");break;
                                            }
                                        }
                                            List<String> kj=new ArrayList<>();
                                            
                                            for(String g:this.p.getLojas(nloja).getPE()){
                                                if(!g.equals(numenc))kj.add(g);
                                            }
                                            
                                            this.p.getLojas(nloja).setPE(kj);
                                            this.p.getLojas(nloja).adicionaLinha3(numenc);
                                            
                                            
                                           this.Escrever();
                                            break;
                                            
                                     case 8:
                                            System.out.println("10 Utilizadores Que Mais Utilizam O APP");
                                            for(Utilizador ju:this.p.UU())System.out.println(ju+"\n");
                                            break;
                                            
                                            
                                     case 9:
                                            System.out.println("10 Transportadoras Que Mais Utilizam O APP");
                                            for(Transportadoras ju:this.p.UT())System.out.println(ju+"\n");
                                            break;
                                     
                                    case 10:
                                          System.out.println("Posso levar Encomendas Medicas");
                                            if(vol.getMed()==true){
                                                vol.setMed(false);
                                                System.out.println("Mudado para Não");
                                            }
                                            else{
                                                vol.setMed(true);
                                                System.out.println("Mudado para Sim");
                                            }
                                            this.Escrever();
                                            
                                            break;
                                            
                                        }
                                    }while (menuV.getOpcao()!=0); 
                                    System.out.println("\nAté breve!...\n");// A opção 0 é usada para sair do menu.
                                     
                                    break;
                                
                                
                                
                        case 3: input = new Scanner(System.in);
                                System.out.println("Loja\n");
                        
                                System.out.println("Diga o seu email(nome)\n");
                                String nomel=input.nextLine();
                               
                                System.out.println("Diga a sua password(codigo)\n");
                                String codl=input.nextLine();
                                input.close();
                                Lojas l=new Lojas();
                                try{
                                    l=this.p.getLojas(nomel,codl);
                                }
                                
                                catch(LojasException e){
                                
                                System.out.println("Email ou password errados\n");
                                break;
                                }
                                
                                do{
                                String ml="\nLoja\n";
                                menuL.executa(ml);
                                switch (menuL.getOpcao()) {
                
                                     case 1:input = new Scanner(System.in);
                                            System.out.println("Alterar morada");
                                            System.out.println("Diga a latitude");
                                            double lat=input.nextDouble();
                                            System.out.println("Diga a longitude");
                                            double lon=input.nextDouble();
                                            l.setX(lat);
                                            l.setY(lon);
                                            input.close();
                                            this.Escrever();
                                            
                                            break;
                                             
                                     case 2:
                                            for(LinhaDeEncomenda line:l.getStock()){
                                                System.out.println("Produto:"+line.getDescricao()+",Codigo:"+line.getCod());
                                            }
                                            input = new Scanner(System.in);
                                            System.out.println("Acrescentar Produtos");
                                            System.out.println("Insira o nome do produto");
                                            String descricao  =input.nextLine();
                                            
                                            System.out.println("Insira o preço");
                                            double preco=input.nextDouble();
                                            input.close();
                                            int code=0;
                                            
                                            for (LinhaDeEncomenda enc: l.getStock()){
                                                if (Integer.parseInt(enc.getCod().substring(1,(enc.getCod().length())))>=code){
                                                   code = Integer.parseInt(enc.getCod().substring(1,(enc.getCod().length())));
                                                }
                                                
                                            }
                                            String cod="p"+(code+1);
                                            
                                            int a=0;
                                            
                                            LinhaDeEncomenda ll=new LinhaDeEncomenda(descricao,0,cod,preco);
                                            l.adicionaLinha(ll);
                                            this.Escrever();
                                            break;
           
                                            
                                            
                                     case 3:
                                            System.out.println("Alterar Disponibilidade");
                                            if(l.getDisp()==true){
                                                l.mudarparaIndisponivel();
                                                System.out.println("Mudado para indisponivel");
                                            }
                                            else{
                                                l.mudarparaDisponivel();
                                                System.out.println("Mudado para disponivel");
                                            }
                                            this.Escrever();
                                            break;
                                            
                                     case 4:
                                            System.out.println("Encomendas Prontas");
                                            for(String encomendas:l.getPE())System.out.println("PorEntregar:"+encomendas);
                                            for(String encomendas:l.getE())System.out.println("Entregues:"+encomendas);
                                            break;
                                            
                                     case 5:
                                            System.out.println("Encomendas Por Fazer");
                                            for(String encomendas:l.getAP())System.out.println(encomendas);
                                            break;
                                            
                                     case 6:
                                            System.out.println("Registar Encomenda Como Feita");
                                            input = new Scanner(System.in);
                                            System.out.println("Diga o Codigo da Encomenda");
                                            String enc= input.nextLine();
                                            input.close();
                                            List enco=new ArrayList<String>();
                                            for(String encomendas:l.getAP()){
                                            if(!encomendas.equals(enc))enco.add(encomendas);
                                        }
                                            l.setAP(enco);
                                            l.adicionaLinha2(enc);
                                            this.Escrever();
                                            break;
                                            
                                     
                                     case 7:
                                            System.out.println("10 Utilizadores Que Mais Utilizam O APP");
                                            for(Utilizador ju:this.p.UU())
                                            System.out.println(ju+"\n");
                                            break;
                                            
                                     case 8:
                                           System.out.println("10 Transportadoras Que Mais Utilizam O APP");
                                            for(Transportadoras ju:this.p.UT())
                                            System.out.println(ju+"\n");
                                            break;
                                     
                                     case 9: 
                                             System.out.println("Retirar Produto");
                                             input=new Scanner(System.in);
                                             for(LinhaDeEncomenda lde:l.getStock())System.out.println(lde);
                                             System.out.println();
                                             System.out.println("Diga o Codigo do Produto");
                                             String cof=input.nextLine();
                                             input.close();
                                             List<LinhaDeEncomenda>nod=new ArrayList<>();
                                             for(LinhaDeEncomenda lde:l.getStock()){
                                                 if(!lde.getCod().equals(cof))nod.add(lde);
                                        }
                                        l.setStock(nod);
                                    }
                                    }while (menuL.getOpcao()!=0); 
                                    System.out.println("\nAté breve!...\n");// A opção 0 é usada para sair do menu.
                                     
                                    break;
                                
                            
                        case 4:
                                System.out.println("Transportadora\n");
                                input = new Scanner(System.in);
                                System.out.println("Diga o seu email(nome)\n");
                                String nomet=input.nextLine();
                                
                                System.out.println("Diga a sua password(codigo)\n");
                                String codt=input.nextLine();
                                
                                input.close();
                                Transportadoras t= new Transportadoras();
                                try{
                                    t=this.p.getTransportadoras(nomet,codt);
                                }
                                
                                catch(TransportadoraException e){
                                
                                System.out.println("Email ou password errados\n");
                                break;
                                }
                                
                                do{
                                String mt="\nTransportadora\n";
                                menuT.executa(mt);
                                switch (menuT.getOpcao()) {
                
                                     case 1:input = new Scanner(System.in);
                                            System.out.println("Alterar morada");
                                            System.out.println("Diga a latitude");
                                            double lat=input.nextDouble();
                                            System.out.println("Diga a longitude");
                                            double lon=input.nextDouble();
                                            t.setX(lat);
                                            t.setY(lon);
                                            input.close();
                                            this.Escrever();
                                            
                                            break;
                                             
                                     case 2:input = new Scanner(System.in);
                                            System.out.println("Alterar Raio de Entrega");
                                            System.out.println("Diga o Novo Raio de Entrega");
                                            double raio=input.nextDouble();
                                            t.setRaio(raio);
                                            input.close();
                                            this.Escrever();
                                            break;
           
                                            
                                            
                                     case 3:input = new Scanner(System.in);
                                            System.out.println("Mudar Preços");
                                            System.out.println("Diga o Novo Preço");
                                            double price=input.nextDouble();
                                            t.setPrecoKm(price);
                                            input.close();
                                            this.Escrever();
                                            break;
                                            
                                     case 4:
                                            System.out.println("Alterar Disponibilidade");
                                            if(t.getDisp()==true){
                                                t.setDisp(false);
                                                System.out.println("Mudado para indisponivel");
                                            }
                                            else{
                                                t.setDisp(true);
                                                System.out.println("Mudado para disponivel");
                                            }
                                            this.Escrever();
                                            break;
                                            
                                     case 5:
                                            System.out.println("Transportar Encomendas");
                                            String cu="";
                                            for(Lojas lj:this.p.getAL()){
                                                if(t.dentroRaio(lj.getX(),lj.getY())==true){
                                                    for(String lkj:lj.getPE()){
                                                    try{cu=this.p.getEncomenda(lkj).getCodutilizador();}
                                                    catch(EncomendaException ex){
                                                        if(t.getMed()==true){
                                                        try{cu=this.p.getEncomendaMedica(lkj).getCodutilizador();}
                                                    catch(EncomendaException e){System.out.println("Erroer");}}
                                                       
                                                    }
                                                    if(t.dentroRaio2(this.p.getUtilizador(cu).getX(),this.p.getUtilizador(cu).getY())==true)
                                                    System.out.println(lkj);}
                                                }
                                            }
                                            input = new Scanner(System.in);
                                            System.out.println("Diga o numero de encomenda");
                                            String nenc=input.nextLine();
                                            try{
                                                this.p.getEncomenda(nenc);
                                                this.p.getEncomenda(nenc).setTempo1(LocalDate.now());
                                            }
                                            catch(EncomendaException ex){ 
                                                try{
                                                    this.p.getEncomendaMedica(nenc);
                                                    this.p.getEncomendaMedica(nenc).setTempo1(LocalDate.now());
                                                }
                                                catch(EncomendaException e){
                                                    System.out.println("N Escreveu Nenhuma Encomenda Valida");
                                                    break;
                                                }
                                             }
                                             
                                            input.close();
                                            
                                            if(!this.p.getEncomenda1(nenc).getCodutilizador().equals(""))cu=this.p.getEncomenda1(nenc).getCodutilizador();
                                            else if(!this.p.getEncomendaMedica1(nenc).getCodutilizador().equals(""))cu=this.p.getEncomendaMedica1(nenc).getCodutilizador();
                                            
                                            this.p.getUtilizador(cu).adicionaLinha(new Pedidos(nenc,t.getCodEmp()));
                                            this.Escrever();
                                            
                                            break;
                                           
                                            
                                     case 6:
                                            System.out.println("Ver Encomendas Entregues");
                                            for(EncomendasAceites ea:this.p.getAEA()){
                                            for(Lojas lj:this.p.getAL()){
                                            for(String ee:lj.getE()){
                                            if(ee.equals(ea.getEA())){
                                                if(ea.getQ().equals(t.getCodEmp())){
                                                    try{
                                                        System.out.println(this.p.getEncomenda(ea.getEA()));
                                                    }
                                                    catch(EncomendaException e){
                                                    try{
                                                        System.out.println(this.p.getEncomendaMedica(ea.getEA()));
                                                    }
                                                    catch(EncomendaException ex){
                                                    System.out.println("Nao Tem Nenhuma Encomenda Por Entregar");
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                        
                                            break;
                                            
                                            
                                     case 7:
                                            System.out.println("Ver Encomendas Por Entregar");
                                            for(EncomendasAceites ea:this.p.getAEA()){
                                            for(Lojas lj:this.p.getAL()){
                                            for(String ee:lj.getPE()){
                                            if(ee.equals(ea.getEA())){
                                                if(ea.getQ().equals(t.getCodEmp())){
                                                    try{
                                                        System.out.println(this.p.getEncomenda(ea.getEA()));
                                                    }
                                                    catch(EncomendaException e){
                                                    try{System.out.println(this.p.getEncomendaMedica(ea.getEA()));
                                                    }
                                                    catch(EncomendaException ex){
                                                    System.out.println("Nao Entregou Nenhuma Encomenda");
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                                            break;
                                    
                                     case 8:
                                            input = new Scanner(System.in);
                                            System.out.println("Registar Encomenda Como Entregue");
                                            System.out.println("Diga o numero de encomenda");
                                            String numenc=input.nextLine();
                                            input.close();
                                            String nloja;
                                            double ux,uy,peso;
                                            try{
                                                nloja=this.p.getEncomenda(numenc).getCodloja();
                                                ux=this.p.getUtilizador(this.p.getEncomenda1(numenc).getCodutilizador()).getX();
                                                uy=this.p.getUtilizador(this.p.getEncomenda1(numenc).getCodutilizador()).getY();
                                                peso=this.p.getEncomenda1(numenc).getPeso();
                                            }
                                            catch(EncomendaException ex){
                                            try{
                                                nloja=this.p.getEncomendaMedica(numenc).getCodloja();
                                                ux=this.p.getUtilizador(this.p.getEncomendaMedica1(numenc).getCodutilizador()).getX();
                                                uy=this.p.getUtilizador(this.p.getEncomendaMedica1(numenc).getCodutilizador()).getY();
                                                peso=this.p.getEncomendaMedica1(numenc).getPeso();
                                            }
                                            catch(EncomendaException e){
                                                System.out.println("Diga um numero de necomenda valido");
                                                break;
                                            }
                                        }
                                            List<String> kj=new ArrayList<>();
                                            for(String g:this.p.getLojas(nloja).getPE()){
                                                if(!g.equals(numenc))kj.add(g);
                                            }
                                            
                                            this.p.getLojas(nloja).setPE(kj);
                                            this.p.getLojas(nloja).adicionaLinha3(numenc);
                                            t.setKMPQ(t.getKMPQ()+t.precoencomenda(ux,uy,
                                            this.p.getLojas(nloja).getX(),
                                            this.p.getLojas(nloja).getY(),peso));
                                            
                                           this.Escrever();
                                            break;
                                            
                                     case 9:
                                            System.out.println("Ver Total Fatorado");
                                            System.out.println(t.getKMPQ()*t.getPrecoKm());
                                            break;
                                    case 10:
                                            System.out.println("10 Utilizadores Que Mais Utilizam O APP");
                                            for(Utilizador ju:this.p.UU())System.out.println(ju+"\n");
                                            break;
                                            
                                            
                                     case 11:
                                            System.out.println("10 Transportadoras Que Mais Utilizam O APP");
                                            for(Transportadoras ju:this.p.UT())System.out.println(ju+"\n");
                                            break;
                                     
                                     case 12:
                                          System.out.println("Posso levar Encomendas Medicas");
                                            if(t.getMed()==true){
                                                t.setMed(false);
                                                System.out.println("Mudado para Não");
                                            }
                                            else{
                                                t.setMed(true);
                                                System.out.println("Mudado para Sim");
                                            }
                                            this.Escrever();
                                            
                                            break;       
                                        }
                                    }while (menuT.getOpcao()!=0); 
                                    System.out.println("\nAté breve!...\n");// A opção 0 é usada para sair do menu.
                                     
                                    break;
                            }
                        }  while (menuPrincipal.getOpcao()!=0);
                        System.out.println("\nAté breve!...\n");// A opção 0 é usada para sair do menu.
                        
                        break;
    
                case 2: 
                        System.out.println("\nCriar\n");
                        
                        do {
                            
                            String v="Quer ser?";
                            menuPrincipal.executa(v);
                            this.Escrever();
                            switch (menuPrincipal.getOpcao()) {
                                
                                case 1: 
                                       input = new Scanner(System.in);
                                       System.out.println("\nUtilizador\n");
                                       
                                       System.out.println("\nDiga o seu nome:\n");
                                       String nome=input.nextLine();
                                       
                                       System.out.println("\nEste é o seu codigo\n");
                                       System.out.println(this.p.addUtilizador(nome));
                                       input.close();
                                       this.Escrever();
                                       
                                       break;
                                       
                                case 2: 
                                       input = new Scanner(System.in);
                                       System.out.println("Voluntario\n");
                                       
                                       System.out.println("\nDiga o seu nome:\n");
                                       String nomev=input.nextLine();
                                       
                                       System.out.println("\nEste é o seu codigo\n");
                                       System.out.println(this.p.addVoluntario(nomev));
                                       input.close();
                                       this.Escrever();
                                       
                                       break;
                                       
                              case 3: 
                                     System.out.println("Loja\n");
                                     input = new Scanner(System.in);
                                     System.out.println("\nDiga o seu nome:\n");
                                     String nomel=input.nextLine();
                                    
                                     System.out.println("\nEste é o seu codigo\n");
                                     System.out.println(this.p.addLoja(nomel));
                                     input.close();
                                     this.Escrever();
                                     
                                     break;
                              
                              case 4:
                                     System.out.println("Transportadora\n");
                                     input = new Scanner(System.in);
                                     System.out.println("\nDiga o seu nome:\n");
                                     String nomet=input.nextLine();
                                      
                                     System.out.println("\nDiga o seu custo de entrega:\n");
                                     Double preco=input.nextDouble();
                                     
                                     System.out.println("\nEste é o seu codigo\n");
                                     System.out.println(this.p.addTransp(nomet,preco));
                                     input.close();
                                     this.Escrever();
                                     
                                     break;
                                    
                                    }
                                    break;
                                }  while (menuPrincipal.getOpcao()!=0);
                                   System.out.println("Até breve!...\n");// A opção 0 é usada para sair do menu.
                                
                                   break;
                                   
                            }
                            
                        } while (menu1.getOpcao()!=0); // A opção 0 é usada para sair do menu.
                        System.out.println("Até breve!...\n");
                        
                        
                       
    }
    
}
    




