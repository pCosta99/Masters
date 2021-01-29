
import java.util.*;
import java.util.Scanner;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.text.SimpleDateFormat;
import java.text.ParsePosition;
import java.util.Set;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import static java.lang.Math.sqrt;
import static java.lang.Math.pow;
import static java.lang.Math.abs;
import java.time.LocalDateTime; 


public class Main 
{
    /**
     * Função principal da aplicação
     * 
     * @return  
     */
    public static void main(String[] args){
        //DataBase db = new DataBase();
        String ficheiro = "save";
        // carrega ficheiro
        DataBase db = new DataBase();
        
      
        Parser w= new Parser();
        try {
            w.parse(db);
        }
       catch(EncomendaJaExisteException e){
           System.out.println("Erro!!!");
        }
        
       try {
            ObjectInputStream oin = new ObjectInputStream(new FileInputStream("save"));
            db = (DataBase) oin.readObject();
            oin.close();
         }
         catch(IOException e)
            { System.out.println(e.getMessage()); }
         catch(ClassNotFoundException e)
            { System.out.println(e.getMessage()); }
       //Para encher a base dados
      
        
       
        
        //##############testes###########
        /*
        Set<String> teste= db.getPerfis();
        System.out.println(teste);
        Set<Encomenda> teste1= db.getEncomendas();
        
        for(Encomenda a: teste1){
            System.out.println(a.getEnc().toString());
        }
        
        try{
            Encomenda ex= db.getEncomenda("e1440");
            System.out.println(ex);
        }
        catch(EncomendaNaoExisteException f){
            System.out.println("erro");
        }
        */
        //Comparator c= new ComparatorUser();
        //List<Utilizador> tes = db.ordenarUsers(c);
        //System.out.println(tes);
        
        //Encomenda ex1= db.getEncomenda("e3333");
        //System.out.println(ex);
        //System.out.println(ex1);
        
       
        //#################################
         
        int opcao = -1;
        
        while( opcao != 4 ){
            System.out.println(" *** Entrega ***\n1-Login \n2-Sign In\n3-Utilizadores por ordem crescente de nºencomendas\n4-Sair\n");
         
            do{
                opcao = leInteiro(4);
            }while( opcao == -1);
            
            switch( opcao ){
                case 1: login(db);
                        break;
                        
                case 2: signIn(db);
                        break;
                        
                case 3: Comparator c= new ComparatorUser();
                        List<Utilizador> tes = db.ordenarUsers(c);
                        System.out.println(tes);
                        break;
                        
                case 4: break;
            }
           
            //salva ficheiro
            try{
                db.gravaEmObjStream(ficheiro);
            }
            catch( IOException e)
                { System.out.println(e.getMessage()); }
            
        }
        
         System.out.println("Obrigado pela seu preferencia, ate sempre");
        //prepara o programa para sair
    
    }
    
    private static int leInteiro(int x){
        Scanner input = new Scanner(System.in);
        int valor;
        try{
            valor = input.nextInt();
        }
        catch(InputMismatchException e){ // se nao for um int
            valor = -1;
        }
        if( valor <= 0 || valor > x)
            valor = -1;
         
        return valor;
    }
    /**
     * Método que faz login na aplicação
     * 
     * @return Set<Encomenda> 
     */
    private static void login(DataBase db){
        Scanner input = new Scanner(System.in);   
        
        System.out.println(" *** LogIn ***\nMail:");
        String mail = input.nextLine(); 
        System.out.println("Palavra passe: ");
        String pass = input.nextLine(); 
        
        boolean valido = db.existeUtilizador(mail) && db.getPerfil(mail).validaLogin(pass);
        
        if(valido){
            //redireciona para o menu correto
            Perfil aux = db.getPerfil(mail);
            
            if( aux instanceof Utilizador)
                menuUser( (Utilizador) aux, db);
            if( aux instanceof Empresa )
                menuEmpresa( (Empresa) aux, db);
            if( aux instanceof Voluntario)
                menuVoluntario( (Voluntario) aux, db);
            if( aux instanceof Loja )
                menuLoja( (Loja) aux, db);
        }else
            System.out.println("O utlizador e a password não correspondem");
    }
    /**
     * Método que faz registo na aplicação
     * 
     * @return
     */
    private static void signIn(DataBase db){
        Scanner input = new Scanner(System.in);
        
        System.out.println(" *** Sing In ***\n1-User\n2-Empresa\n3-Voluntario\n4-Loja\n5-Voltar ao menu");
        int opcao = -1;
        do{
            opcao = leInteiro(5);
        }while( opcao == -1);
        String email=new String();
        String nome=new String();;
        String password=new String();;
        Ponto2D local=new Ponto2D();
        if(opcao!=5){
            do{
                System.out.println("Email:");
                email = input.nextLine();
                if ( db.existeUtilizador( email))
                    System.out.println("Este email ja existe");
            }while( db.existeUtilizador( email) );
            
            System.out.println("Nome:");
            nome = input.nextLine();
            
            System.out.println("Palavra passe:");
            password = input.nextLine();
            
            System.out.println("Digite as suas coordenadas:");
            double x = input.nextDouble();
            double y = input.nextDouble();
            local= new Ponto2D(x,y);
        }
        
        switch( opcao ){
                  case 1 :
                  Utilizador u = new Utilizador(email,nome,password,local,0);
                          db.add(u);
                          System.out.println("A sua conta foi registada");
                          break;
                  case 2:Loja l = new Loja(email,nome,password,local,10,"");
                         db.add(l);
                         System.out.println("A sua conta foi registada");
                         break;
                  case 3:Empresa e = new Empresa(email,nome,password,local,0,0,0,"",0,0);
                          db.add(e);
                          System.out.println("A sua conta foi registada");
                          break;
                  case 4:Voluntario v = new Voluntario(email,nome,password,local,0,0,0,false);
                         db.add(v);
                         System.out.println("A sua conta foi registada");
                         break;
                  case 5:break;
                }
        
    }
    //---------------------------------------------------------------------------------
    //----------------------------Menu User--------------------------------------------
    /**
     * Método para o menu de um user
     * 
     * @return  
     */
    private static void menuUser( Utilizador d, DataBase db){
        int opcao = -1;
        
        while( opcao != 4){
            System.out.println(" *** Menu ***\n1-Fazer encomenda:\n2-Area pessoal:\n3-Classifica Transportadora:\n4-Log out");
            
            do{
                opcao = leInteiro(4);
            }while( opcao == -1);
                
            switch( opcao ){
                case 1: fazerEncomenda(d,db);
                        break;
                case 2: areaPessoal(d,db);
                        break;
                case 3 :classifica(d,db);
                        break;
                        
                case 4: break;
            }
                
        }
        
    }
    /**
     * Método para fazer uma encomenda
     * 
     * @return  
     */
    private static void fazerEncomenda(Utilizador d,DataBase db)  {
       Scanner input = new Scanner(System.in);
       Encomenda e = new Encomenda();
     
       System.out.println("Codigo da encomenda:");
       String codigo = input.nextLine();
       e.setEnc(codigo);
        
       e.setUser(d.getEmail());
       
       System.out.println("Loja:");
       String mail= input.nextLine();
       e.setLoja(mail);
       
       e.setEstado(-1); 
       addLinhaEncomenda(e);
        
       System.out.println("Aceita custo de transporte:\n1-Sim\n2-Nao");
       int i = input.nextInt();
           if(i==1){
                e.setAceitaCusto(1);
            }
           else{e.setAceitaCusto(0);
            }
           System.out.println("Encomenda de medicamentos:\n1-Sim\n2-Nao");
           int j = input.nextInt();
           if(j==1){
                e.setMedicamentos(1);
            }
           else{e.setMedicamentos(0);
            }
        
       try{
            db.addEnc(e);
            d.incNumEnc();
            System.out.println("###Encomenda feita com sucesso###");
        }
       catch(EncomendaJaExisteException x){
            System.out.println("Referencia da encomenda ja exite\n");
        }
       
    }
    /**
     * Método para adicinonar Linhas de encomenda a uma encomenda
     * 
     * @return  
     */
    private static void addLinhaEncomenda(Encomenda e){
         int opcao = -1;
         Scanner input = new Scanner(System.in);
        
        while( opcao != 2){
            System.out.println("1-Adicionar produto\n2-avançar\n");
            
           do{
                opcao = leInteiro(2);
            }while( opcao == -1);
                
           switch( opcao ){
                 case 1:
                         LinhaEncomenda l = new LinhaEncomenda();
            
                          System.out.println("Codigo da produto:");
                          String codigo = input.nextLine();
                          l.setReferencia(codigo);
                          System.out.println("Codigo da produto:");
                          String codigo1 = input.nextLine();
                          l.setReferencia(codigo1);
                                   
                          System.out.println("Descrição:");
                          String desc = input.nextLine();
                          l.setDescricao(desc);
                                   
                          System.out.println("Intrudoza a quantidade:");
                          double y = input.nextDouble();
                          l.setQuantidade(y);
                                   
                          System.out.println("Intrudoza o valor por unidade:");
                          double x = input.nextDouble();
                          l.setValorUni(x);
                          
                          System.out.println("Intrudoza o peso por unidade:");
                          double pes= input.nextDouble();
                          l.setPeso(pes);
                     
                          e.addLinhaEncomenda(l);
                  
                        break;
                        
                 case 2: break;
            }
                
        }
        
    }
    /**
     * Método para o menu de um user(AreaPessoal)
     * 
     * @return  
     */
    private static void areaPessoal(Utilizador d,DataBase db){
      int opcao = -1;
        
      while( opcao != 3){
            System.out.println("1-Definiçoes pessoais \n2-Historico\n 3-Voltar");
            
           do{
                opcao = leInteiro(3);
            }while( opcao == -1);
                
           switch( opcao ){
                case 1: defs(d,db);
                        
                case 2 :hist(d,db);
                        
                case 3: break;
            }
                
        }
    }
    /**
     * Método para vizualizar e alterar a defenições de um user
     * 
     * @return  
     */
    private static void defs(Utilizador d,DataBase db){
        
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 5){
            System.out.println("Escolha (1-5) para alterar os valores");
            System.out.println("1-Email: " + d.getEmail());
            System.out.println("2-Nome: " + d.getNome());
            System.out.println("3-Password: " + d.getPassword());
            System.out.println("4-Local: " + d.getLocal());
            System.out.println("5-Sair");

            do{
                opcao = leInteiro(5);
            }while( opcao == -1);
            
            switch( opcao ){
                case 1: System.out.println("Não é possivel alterar o email\n");
                        break;
          
                case 2: System.out.println("Insira o nome\n");
                        d.setNome(input.nextLine());
                        break;
                        
                case 3: System.out.println("Insira a pass\n");
                        d.setPassword( input.nextLine());
                        break;
                        
                case 4: Ponto2D a = new Ponto2D();
                        System.out.println("Insira x:\n");
                        double x = input.nextInt();
                        System.out.println("Insira y:\n");
                        double y = input.nextInt();
                        a.setX(x);
                        a.setY(y);
                        d.setLocal(a);
                        break;
                        
                case 5: break;
            }
        }
     
    }
    /**
     * Método para classificar o tranpsorte de uma encomenda
     * 
     * @return  
     */
    private static void classifica(Utilizador d, DataBase db){
     Scanner input = new Scanner(System.in);
     System.out.println("Insira a referencia da encomenda que deseja avaliar:\n");
     String ref = input.nextLine();
     
     System.out.println("Insira a classificação de 0-10:\n");
     int x = input.nextInt();
     
     try{
           db.getEncomenda(ref).setClassificacao(x);
           System.out.println("###Encomenda avaliada com sucesso###");
        }
     catch(EncomendaNaoExisteException i){
            System.out.println("Referencia da encomenda invalida\n");
        }
    }
    /**
     * Método para ver o historico 
     * 
     * @return  
     */
    private static void hist(Utilizador d,DataBase db){
      System.out.println("###As suas encomenda:###");
      Set<Encomenda> enc= db.getEncomendasUser(d.getEmail());
      if(enc.isEmpty()){
           System.out.println("###Nao existem ainda encomendas para consultar###");
        }
      else{
           System.out.println(enc);
        }
    }
    //---------------------------------------------------------------------------------
    //---------------------------------------------------------------------------------
    /**
     * Método para o menu de uma loja
     * 
     * @return  
     */
    private static void menuLoja( Loja d, DataBase db){
       int opcao = -1;
        
        while( opcao != 3){
            System.out.println(" *** Menu ***\n1-Minhas Encomendas\n2-Area pessoal\n3-Log out");
            
            do{
                opcao = leInteiro(3);
            }while( opcao == -1);
                
            switch( opcao ){
                case 1: PreparaEncomenda(d,db);
                        break;
                case 2: areaPessoal(d,db);
                        break;
                case 3: break;
            }
                
        }
        
    }
    /**
     * Método para preparar uma encomenda para envio
     * 
     * @return  
     */
    private static void PreparaEncomenda(Loja d ,DataBase db){
        Set<Encomenda> encs= db.getEncomendasLoja(d.getEmail());
        int n=db.getEncomendasLoja(d.getEmail()).size();
        Scanner input = new Scanner(System.in);
        
       if(n == 0)
            System.out.println( "Nenhuma encomenda de momento:\n" );
       else{
            System.out.println("Encomendas : \n");
            System.out.println(encs);
       }
       System.out.println("Digite o codigo da encomenda que pretende preparar para envio: \n");
       String aux= input.nextLine();
       try{
            db.getEncomenda(aux).setEstado(0);
            double p= db.getEncomenda(aux).pesoEncomenda();
            db.getEncomenda(aux).setPeso(p);
            System.out.println("###Encomenda pronta para transportar###");
       }
       catch(EncomendaNaoExisteException i){
            System.out.println("Referencia da encomenda invalida\n");
       }
       
    }
     //Area pessoal Loja
     /**
     * Método menu de uma loja(Area Pessoal)
     * 
     * @return  
     */
    private static void areaPessoal(Loja d,DataBase db){
      int opcao = -1;
      //Falta Lista de pedidos
      while( opcao != 3){
            System.out.println("1-Definiçoes pessoais \n2-Historico\n3-Voltar");
            
           do{
                opcao = leInteiro(3);
            }while( opcao == -1);
                
           switch( opcao ){
                case 1: defsL(d,db);
                        
                case 2 :histL(d,db);
                        
                case 3: break;
            }
                
        }
    }
    /**
     * Método para vizualizar e alterar a defenições de uma loja
     * 
     * @return  
     */
    private static void defsL(Loja d,DataBase db){
        
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 7){
            System.out.println("Escolha (1-7) para alterar os valores");
            System.out.println("1-Email: " + d.getEmail());
            System.out.println("2-Nome: " + d.getNome());
            System.out.println("3-Password: " + d.getPassword());
            System.out.println("4-Local: " + d.getLocal());
            System.out.println("5-Tempo de Espera: "+d.getTempoEspera());
            System.out.println("6-Morada: "+d.getMorada());
            System.out.println("7-Sair");

            do{
                opcao = leInteiro(7);
            }while( opcao == -1);
            
            switch( opcao ){
                case 1: System.out.println("Não é possivel alterar o email\n");
                        break;
          
                case 2: System.out.println("Insira o nome\n");
                        d.setNome(input.nextLine());
                        break;
                        
                case 3: System.out.println("Insira a pass\n");
                        d.setPassword( input.nextLine());
                        break;
                        
                case 4: Ponto2D a = new Ponto2D();
                        System.out.println("Insira x:\n");
                        double x = input.nextInt();
                        System.out.println("Insira y:\n");
                        double y = input.nextInt();
                        a.setX(x);
                        a.setY(y);
                        d.setLocal(a);
                        break;
                        
                case 5: System.out.println("Insira o Tempo de Espera:\n");
                        int t= input.nextInt();
                        d.setTempoEspera(t);
                        break;
                case 6: System.out.println("Insira Morada:\n");
                        d.setMorada(input.nextLine());
                        break;
                case 7: break;
            }
        }
    }
    /**
     * Método para vizualizar o historico de uma loja
     * 
     * @return  
     */
    private static void histL(Loja d,DataBase db){
        System.out.println("###As suas encomenda:###");
        Set<Encomenda> enc= db.getEncomendasLoja(d.getEmail());
        if(enc.isEmpty()){
            System.out.println("###Nao existem ainda encomendas para consultar###");
        }
        else{
            System.out.println(enc);
        }
    }

    /**
     * Método para menu de uma empresa
     * 
     * @return  
     */
    private static void menuEmpresa( Empresa d, DataBase db){
        int opcao = -1;
        
        while( opcao != 3){
            System.out.println(" *** Menu ***\n1-Transporta\n2-Area pessoal\n3-Log out");
            
            do{
                opcao = leInteiro(4);
            }while( opcao == -1);
                
            switch( opcao ){
                case 1: try{
                            transporta(d,db);
                        }
                        catch(NaoSeEncontraRaioException h){
                            System.out.println("---Loja encontra-se fora de alcance---");
                        }
                        break;
                case 2: AreaPessoal(d,db);
                        break;
                case 3: break;
            }
                
        }
     }
     /**
     * Método para uma empresa transportar uma encomenda
     * 
     * @return  
     */
    private static void transporta(Empresa d, DataBase db) throws NaoSeEncontraRaioException{
        System.out.println("Encomendas que aguardam transporte:\n");
        Set<Encomenda> e= db.getEncomendaAceitaCusto();
        if(e.isEmpty()){
          System.out.println("Nao existem encomendas para transportar\n");
        }
        else{
            try{
            System.out.println(e);
            Scanner input = new Scanner(System.in);
            System.out.println("Introduza a referencia da encomenda para transporte:\n");
            
            String aux = input.nextLine();
    
            
            db.addEncAceite(aux);
            
            Encomenda enc= db.getEncomenda(aux);
            Loja loja = db.getLoja(enc.getLoja());
            Perfil user = db.getPerfil(enc.getUser());
            Ponto2D my = d.getLocal();
            
            double x= loja.getLocal().getX();
            double y =loja.getLocal().getY();
            //distancia empresa -> loja
            double d1= abs(sqrt(pow(d.getLocal().getX()- x,2)+pow(d.getLocal().getY() - y ,2)));
            if(d1>d.getRaio()){ throw new NaoSeEncontraRaioException();}
            //distancia loja -> user
            double d2= abs(sqrt(pow(x- user.getLocal().getX(),2)+pow(x - user.getLocal().getY() ,2)));
            //-------custo de tranporte-------
            double custo= (d.getCustokm()*(d1+d2) + enc.getPeso()*0.1)*0.01 ;
            db.getEncomenda(aux).setCusto(custo);
            //--------------------------------------
            //------calcula tempo-------------------
            double tempo= (d1+d2)/d.getVelmed() + loja.getTempoEspera();
            db.getEncomenda(aux).setEstado(1);
            LocalDateTime now=LocalDateTime.now();
            db.getEncomenda(aux).setData(now);
            db.getEncomenda(aux).setTempo(tempo);
            db.getEncomenda(aux).setNomTrans(d.getEmail());
            System.out.println("#######Encomenda transportada com sucesso######");
            }
            catch(EncomendaNaoExisteException i){
                System.out.println("Referencia da encomenda invalida\n");
            }
        } 
    }
    /**
     * Método para o menu de uma empresa(Area Pessoal)
     * 
     * @return  
     */
    private static void AreaPessoal(Empresa d,DataBase db){
      int opcao = -1;
      int ano;
      int mes;
      int dia;
      Scanner input = new Scanner(System.in);
      //Falta Lista de pedidos
      while( opcao != 4){
            System.out.println("1-Definiçoes pessoais \n2-Historico\n3-Ver total faturado\n4-Voltar");
            
           do{
                opcao = leInteiro(4);
            }while( opcao == -1);
                
           switch( opcao ){
                case 1: defsE(d,db);
                        
                case 2 :histE(d,db);
                
                case 3:System.out.println("Insira a ano:");
                        ano= input.nextInt();
                       System.out.println("Insira a mes:");
                        mes= input.nextInt();
                       System.out.println("Insira a dia:");
                        dia= input.nextInt();
                        
                       double saldo=db.totalFaturado(d.getEmail(),LocalDateTime.of(ano,mes,dia,0,0));
                       System.out.println("O seu saldo é:"+saldo);
                        
                case 4: break;
            }
                
        }
    }
    /**
     * Método para vizualizar e alterar a defenições de uma empresa
     * 
     * @return  
     */
    private static void defsE(Empresa d,DataBase db){
        
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 11){
            System.out.println("Escolha (1-10) para alterar os valores");
            System.out.println("1-Email: " + d.getEmail());
            System.out.println("2-Nome: " + d.getNome());
            System.out.println("3-Password: " + d.getPassword());
            System.out.println("4-Local: " + d.getLocal());
            System.out.println("5-Raio: "+d.getRaio());
            System.out.println("6-Habilitado a transportar medicamentos: "+d.getLicMedicamentos());
            System.out.println("7-Velocidade media: "+d.getVelmed());
            System.out.println("8-Custo ao kg: "+d.getCustokg());
            System.out.println("9-Custo ao km: "+d.getCustokm());
            System.out.println("10-Nif: "+d.getNif());
            System.out.println("11-Sair");
 
            do{
                opcao = leInteiro(11);
            }while( opcao == -1);
            
            switch( opcao ){
                
          
                case 2: System.out.println("Insira o nome\n");
                        d.setNome(input.nextLine());
                        break;
                        
                case 3: System.out.println("Insira a pass\n");
                        d.setPassword( input.nextLine());
                        break;
                        
                case 4: Ponto2D a = new Ponto2D();
                        System.out.println("Insira x: \n");
                        double x = input.nextInt();
                        System.out.println("Insira y: \n");
                        double y = input.nextInt();
                        a.setX(x);
                        a.setY(y);
                        d.setLocal(a);
                        break;
                        
                case 5: System.out.println("Insira o raio: \n");
                        double r= input.nextDouble();
                        d.setRaio(r);
                        break;
                        
                case 6: System.out.println("Transporte de medicamentos:  \n");
                        int m= input.nextInt();
                        d.setLicMedicamentos(m);
                        break;
                        
                case 7: System.out.println("Insira a velocidade media:  \n");
                        int v = input.nextInt();
                        d.setVelmed(v);
                        break;
                
                case 8: System.out.println("Insira o custo por Kg:  \n");
                        double kg= input.nextDouble();
                        d.setCustokg(kg);
                        break;
                case 9: System.out.println("Insira o custo por Km:  \n");
                        double km= input.nextDouble();
                        d.setCustokm(km);
                        break;
                        
                case 10:System.out.println("Não é possivel alterar o nif\n");
                        break;
                        
                case 11:break;
            }
        }
    }
    /**
     * Método para vizualizar o historico de uma empresa
     * 
     * @return  
     */
    private static void histE(Empresa d, DataBase db){
        System.out.println("###As suas encomenda:###");
        Set<Encomenda> enc= db.getEncomendasTrans(d.getEmail());
        if(enc.isEmpty()){
            System.out.println("###Nao existem ainda encomendas para consultar###");
        }
        else{
            System.out.println(enc);
        }
    }
    /**
     * Método para o menu de um voluntario
     * @return  
     */
    private static void menuVoluntario( Voluntario d, DataBase db){
        int opcao = -1;
         
        while( opcao != 4){
            System.out.println(" *** Menu ***\n1-A trabalhar-" + d.estaDisponivel() + "\n2-Transporta\n3-Area pessoal\n4-Log out");
            
            do{
                opcao = leInteiro(4);
            }while( opcao == -1);
                
            switch( opcao ){
                case 1: boolean x = ! d.estaDisponivel();
                        d.setDisponivel(x);
                        break;
                case 2:  try{
                            transportaV(d,db);
                        }
                        catch(NaoSeEncontraRaioException h){
                            System.out.println("---Loja encontra-se fora de alcance---");
                        }
                case 3: areaPessoal(d,db);
                        break;
                case 4: break;
            }
                
        }
        
    }
     /**
     * Método para o menu de uma empresa(Area Pessoal)
     * @return  
     */
     
    private static void areaPessoal(Voluntario d,DataBase db){
      int opcao = -1;
      //Falta Lista de pedidos
      while( opcao != 3){
            System.out.println("1-Definiçoes pessoais \n2-Historico\n3-Voltar");
            
           do{
                opcao = leInteiro(3);
            }while( opcao == -1);
                
           switch( opcao ){
                case 1: defsV(d,db);
                        
                case 2 :histV(d,db);
                        
                case 3: break;
            }
                
        } 
    }
    /**
     * Método para vizualizar e alterar a defenições de um voluntario
     * 
     * @return  
     */
    private static void defsV(Voluntario d,DataBase db){
        
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 8){
            System.out.println("Escolha (1-7) para alterar os valores");
            System.out.println("1-Email: " + d.getEmail());
            System.out.println("2-Nome: " + d.getNome());
            System.out.println("3-Password: " + d.getPassword());
            System.out.println("4-Local: " + d.getLocal());
            System.out.println("5-Raio: "+d.getRaio());
            System.out.println("6-Habilitado a transportar medicamentos: "+d.getLicMedicamentos());
            System.out.println("7-Velocidade media: "+d.getVelmed());
            
            
            System.out.println("8-Sair");

            do{
                opcao = leInteiro(8);
            }while( opcao == -1);
            
            switch( opcao ){
                case 1: System.out.println("Não é possivel alterar o email\n");
                        break;
          
                case 2: System.out.println("Insira o nome\n");
                        d.setNome(input.nextLine());
                        break;
                        
                case 3: System.out.println("Insira a pass\n");
                        d.setPassword( input.nextLine());
                        break;
                        
                case 4: Ponto2D a = new Ponto2D();
                        System.out.println("Insira x:\n");
                        double x = input.nextInt();
                        System.out.println("Insira y:\n");
                        double y = input.nextInt();
                        a.setX(x);
                        a.setY(y);
                        d.setLocal(a);
                        break;
                case 5: System.out.println("Insira o raio: \n");
                        double r= input.nextDouble();
                        d.setRaio(r);
                        break;
                        
                case 6: System.out.println("Transporte de medicamentos:  \n");
                        int m= input.nextInt();
                        d.setLicMedicamentos(m);
                        break;
                        
                case 7: System.out.println("Insira a velocidade media:  \n");
                        int v = input.nextInt();
                        d.setVelmed(v);
                        break;
                
                case 8: break;
            }
        }
    }
    /**
     * Método para vizualizar o historico de um voluntario
     * 
     * @return  
     */
    private static void histV(Voluntario d, DataBase db){
        System.out.println("###As suas encomenda:###");
        Set<Encomenda> enc= db.getEncomendasTrans(d.getEmail());
        if(enc.isEmpty()){
            System.out.println("###Nao existem ainda encomendas para consultar###");
        }
        else{
            System.out.println(enc);
        }
    }
    /**
     * Método para transportar uma encomenda por parte de um voluntario
     * 
     * @return  
     */
    private static void transportaV(Voluntario d, DataBase db) throws NaoSeEncontraRaioException{
        System.out.println("Encomendas que aguardam transporte:\n");
        Set<Encomenda> e= db.getEncomendaNaoAceitaCusto();
        if(e.isEmpty()){
          System.out.println("Nao existem encomendas para transportar\n");
        }
        else{
            try{
            System.out.println(e);
            Scanner input = new Scanner(System.in);
            System.out.println("Introduza a referencia da encomenda para transporte:\n");
            
            String aux = input.nextLine();
    
            
            db.addEncAceite(aux);
            
            Encomenda enc= db.getEncomenda(aux);
            Loja loja = db.getLoja(enc.getLoja());
            Perfil user = db.getPerfil(enc.getUser());
            Ponto2D my = d.getLocal();
            
            double x= loja.getLocal().getX();
            double y =loja.getLocal().getY();
            //distancia voluntario -> loja
            double d1= abs(sqrt(pow(d.getLocal().getX()- x,2)+pow(d.getLocal().getY() - y ,2)));
            if(d1>d.getRaio()){ throw new NaoSeEncontraRaioException();}
            //distancia loja -> user
            double d2= abs(sqrt(pow(x- user.getLocal().getX(),2)+pow(x - user.getLocal().getY() ,2)));
            //------calcula tempo-------------------
            double tempo= (d1+d2)/d.getVelmed() + loja.getTempoEspera();
            db.getEncomenda(aux).setEstado(1);
            LocalDateTime now=LocalDateTime.now();
            db.getEncomenda(aux).setData(now);
            db.getEncomenda(aux).setTempo(tempo);
            db.getEncomenda(aux).setNomTrans(d.getEmail());
            System.out.println("#######Encomenda transportada com sucesso######");
            }
            catch(EncomendaNaoExisteException i){
                 System.out.println("Referencia da encomenda invalida\n");
            }
        } 
    }
}
    
 


        
  
