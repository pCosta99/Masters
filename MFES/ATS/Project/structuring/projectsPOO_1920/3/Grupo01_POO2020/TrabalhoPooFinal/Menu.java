import java.util.Scanner;
import java.util.InputMismatchException;
import java.util.Set;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.*;
/**
 * Classe principal que implementa os requisitos que 
 * o programa tem de cumprir
 /**
 * @author 
 * LCC
 * A71785 - Tiago Silva;
 * A72450 - Maria Francisca Fernandes.
 * A73169 - Fernanda Dias;
 * 
 */
public class Menu{

    public static void main(String[] args){
        DataBase db = new DataBase();
        Parse p = new Parse();
        p.parse(db);
        
        String ficheiro = "save";
        
        try {
            ObjectInputStream oin = new ObjectInputStream(new FileInputStream("save"));
            db = (DataBase) oin.readObject();
            oin.close();
        }
        catch(IOException e)
            { System.out.println(e.getMessage()); }
        catch(ClassNotFoundException e)
            { System.out.println(e.getMessage()); }
        
        
        
        /*
         * Para ver se carregou na bd
        Set<String> teste = db.getUsers();
        System.out.println(teste);
        
        Set<Encomenda> teste2 = db.getEncomendas();
        for(Encomenda a: teste2){
            System.out.println(a.getCodEncomenda().toString());
        }
        */
        
        /*
         * Primeiro menu
         */
        int opcao = -1;
   
        while(opcao != 4){
            System.out.println(" *** TrazAqui! ***\n1-Login\n2-Sign In\n3-Os que mais utilizam \n4-Sair\n");
       
            do{
                opcao = leInteiro(4);
            }while( opcao == -1);
    
            switch(opcao){
                case 1: loginUser(db);
                        break;
                case 2: signIn(db);
                        break;
                case 3: maisUtilizam(db);
                case 4: break;
            }
            
             //salva ficheiro
            try{
                db.gravaEmObjStream(ficheiro);
            }
            catch( IOException e)
                { System.out.println(e.getMessage()); }
            
        }
    }
     
    
    private static void maisUtilizam(DataBase db){
        int opcao = -1;
        //Iterator<User> it = db.ordenarUsers("kmsTotal");
        
        while( opcao != 3){
            System.out.println("*** Os que mais utilizam ***\n1- Utilizadores\n2-Transportadoras\n3-Sair");
            
            do{
                opcao = leInteiro(3);
            }while(opcao == -1);
            
            switch(opcao){
                case 1: System.out.println(db.maisUtilizamU());
                        break;
                 
                case 2: System.out.println(db.maisUtilizamT());
                        break;
                case 3: break;    
            }
        }
    }
    
    
    /**
     * Lê o inteiro que inserimos das opções do menu
     */
    private static int leInteiro(int x){
        Scanner input = new Scanner(System.in);
        int valor;
        try{
            valor = input.nextInt();
        }
        catch(InputMismatchException e){ // se nao for um int
            valor = -1;
        }
        if(valor <= 0 || valor > x) //se inserir numero fora da opçao do menu
            valor = -1;
            
            return valor;
    }
    
    /**
     * Login de qualquer utilizador, pois redireciona para o menu correto 
     */
    private static void loginUser(DataBase db){
        Scanner input = new Scanner(System.in);
        
        System.out.println("*** Login ***\nTag:");
        String mail = input.nextLine();
        System.out.println("Password: ");
        String pass = input.nextLine();
        
        boolean valido = db.existeUtilizador(mail) && db.getUser(mail).validaLogin(pass);
        
        if(valido){
            //redireciona para o menu correto
            User aux = db.getUser(mail);
            
            if(aux instanceof Utilizador)
                menuUtilizador((Utilizador) aux, db);
            if(aux instanceof Loja)
                menuLoja((Loja) aux, db);
            if(aux instanceof Transportadora)
                menuTransportadora((Transportadora) aux, db); 
            if(aux instanceof Voluntario)
                menuVoluntario((Voluntario) aux, db);    
        }else 
            System.out.println("Tag ou password incorretos");
    }
    
    
    /**
     * Menu da transportadora
     */
    private static void menuTransportadora(Transportadora t, DataBase db){
        int opcao = -1;
        DecimalFormat df = new DecimalFormat("#.0");
        while(opcao != 4){
            System.out.println("*** Menu ***\n1- A trabalhar-" + t.estaDisponivel() + "\n2-Area Pessoal\n3-Total Facturado\n4-Log out");
            
            do{
                opcao = leInteiro(4);
            }while(opcao == -1); 
            
            switch(opcao){
                case 1: boolean x = ! t.estaDisponivel();
                        t.setDisponivel(x);
                        break;
                case 2: areaPessoal(t,db);
                        break;
                case 3: System.out.println("Lucro total: "+ df.format(t.getTotalFacturado())+" euros\n");
                       
                        break;
                case 4: break;        
            }
        }
    
    }
    
    /**
     * Area pessoal da transportadora
     */
    private static void areaPessoal(Transportadora t, DataBase db){
        int opcao = -1;
        
        while( opcao != 3){
            System.out.println("*** Area Pessoal ***\n1- Listar Encomendas\n2-Dados pessoais\n3-Sair/n");
            
            do{
                opcao = leInteiro(3);
            }while(opcao == -1);
            
            switch(opcao){
                case 1: listaEncomendasT(t, db);
                    break;
                case 2: dadosPessoais(t);
                    break;
                case 3: break;    
            }
        }
    }
    
    
    /**
     * Menu da loja
     */
    private static void menuLoja(Loja l,DataBase db){
        int opcao = -1;
        
        while(opcao != 3){
            System.out.println("*** Menu ***\n1-Adicionar encomenda para entrega\n2-Area pessoal\n3-Log out");
            
            do{
                opcao = leInteiro(3);
            }while(opcao==-1);
            
            switch(opcao){
                case 1: adicionarEncomendaPronta(l,db);
                        break;
                case 2: areaPessoal(l,db);
                        break;
                case 3: break;
            }
        }
    
    }
     
    /**
     * Area pessoal da loja
     */ 
    private static void areaPessoal(Loja l, DataBase db){
        int opcao = -1;
        
        while( opcao != 3){
            System.out.println("*** Area Pessoal ***\n1- Listar Encomendas\n2-Dados pessoais\n3-Sair/n");
            
            do{
                opcao = leInteiro(3);
            }while(opcao == -1);
            
            switch(opcao){
                case 1: listaEncomendasT(l, db);
                    break;
                case 2: dadosPessoais(l);
                    break;
                case 3: break;    
            }
        }
    }
    
    
    /**
     * Menu voluntario
     */
    private static void menuVoluntario(Voluntario v, DataBase db){
        int opcao = -1;
        
        while(opcao != 3){
            System.out.println("*** Menu ***\n1- A trabalhar-" + v.estaDisponivel() + "\n2-Area Pessoal\n3-Log out");
            
            do{
                opcao = leInteiro(3);
            }while(opcao == -1); 
            
            switch(opcao){
                case 1: boolean x = ! v.estaDisponivel();
                        v.setDisponivel(x);
                        break;
                case 2: areaPessoal(v,db);
                        break;
                case 3: break;        
            }
        }
    }
    
    /**
     * Menu utilizador
     */ 
    private static void menuUtilizador(Utilizador u, DataBase db){
        int opcao = -1;
        
        while(opcao !=3){
            System.out.println("*** Menu ***\n1-Pedir entrega de encomenda\n2-Area pessoal\n3-Log out");
            
            do{
                opcao = leInteiro(3);
            }while(opcao == -1);
            
            switch(opcao){
                case 1: fazerEncomenda(u,db);
                        break;
                case 2: areaPessoal(u,db);
                        break;
                case 3: break;        
            }
        }
    }
    
    
    /**
     * Area pessoal do voluntario
     */ 
    private static void areaPessoal(Voluntario v, DataBase db){
        int opcao = -1;
        
        while( opcao != 3){
            System.out.println("*** Area Pessoal ***\n1- Listar Encomendas\n2-Dados pessoais\n3-Sair/n");
            
            do{
                opcao = leInteiro(3);
            }while(opcao == -1);
            
            switch(opcao){
                case 1: listaEncomendasT(v, db);
                    break;
                case 2: dadosPessoais(v);
                    break;
                case 3: break;    
            }
        }
    }
    
    /**
     * Area pessoal do utilizador
     */
    private static void areaPessoal(Utilizador u, DataBase db){
        int opcao = -1;
        
        while(opcao != 4){
            System.out.println("*** Area Pessoal ***\n1-Listar Encomendas\n2-Classificar entrega\n3-Dados pessoais\n4-Sair\n");
            
            do{
                opcao = leInteiro(4);
            } while(opcao == -1);
            
            switch(opcao){
                case 1: listaEncomendasT(u,db);
                        break;
                case 2: darClassificacao(u,db);
                        break;
                case 3: dadosPessoais(u);
                        break;
                case 4: break;        
            }
        }
    }
    
    /**
     * Lista as encomendas
     */
    private static void listaEncomendasT(User u, DataBase db){
        if(u.getNencomendas().size()==0)
            System.out.println("Nenhuma encomenda encontrada");
        else{
            int linhas, opcao;
            linhas = opcao = 0;
            do{
                System.out.println("Encomendas:");
                String encomendas = u.listaEncomendasT(db);
                linhas = countLines(encomendas);
                System.out.println("Escolha a encomenda 1- " + linhas + " para ver mais informação ou " +(linhas+1)+" para sair ");
                System.out.println(encomendas);
                do{
                    opcao = leInteiro(linhas +1);
                }while( opcao == -1);
                
                if( opcao <= linhas)
                    System.out.println(db.getTransporte(u.getEncomenda(opcao-1)).toString());
            }while(opcao != linhas+1);
        }    
    
    }
    
    /**
     * Menu para dar classificaçao ao transporte e atualiza a do respetivo transportador
     */
    private static void darClassificacao(Utilizador u,DataBase db){
        int classificacao, num;
        classificacao=num=-1;
        
        if(u.getNencomendas().size()==0){
            System.out.println("Nenhuma encomenda efetuada");
            return ;
        }
        
        do{
            System.out.println("Numero de encomenda(1-" + u.getNencomendas().size()+"):");
            num = leInteiro(u.getNencomendas().size());
        }while(num == -1);
        
        do{
            System.out.println("Classificacao de 0 - 10");
            classificacao = leInteiro(100);
        }while(classificacao == -1);
        
        if(db.getTransporte(num-1).getClassificacao() != -1)
            System.out.println("Já atribuiu nota a esta encomenda");
        else{
            Transporte t = db.getTransporte(num-1);
            t.setClassificacao(classificacao);
            System.out.println("Classificação atribuida com sucesso");
            User user =db.getUser(t.getTransportador());
            
            if(user instanceof Voluntario){
                Voluntario us = (Voluntario) db.getUser(t.getTransportador());
                double aux = us.getClassificacao();
                double size=(double) us.getNencomendas().size();
                us.setClassificacao((aux+classificacao)/(size));
            
            }
            else{
                Transportadora us = (Transportadora) db.getUser(t.getTransportador());
                double aux = us.getClassificacao();
                double size=(double) us.getNencomendas().size();
                us.setClassificacao((aux+classificacao)/(size));
            }
            
        }
          
    }
    
    private static void dadosPessoais(User u){
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 4){
            System.out.println("Escolha (1-3) para alterar os valores");
            System.out.println("1-Nome: " + u.getNome());
            System.out.println("2-Tag: " + u.getTag());
            System.out.println("3-Password: ******");
            System.out.println("4-Sair");
        
            do{
                opcao = leInteiro(4);
            }while( opcao == -1);
            
            switch(opcao){
                case 1: System.out.println("Insira nome");
                        u.setNome(input.nextLine());
                        break;
                case 2: System.out.println("Não é possível alterar o tag\n");
                        break;
                case 3: System.out.println("Insira a password antiga:");
                        String antiga= input.nextLine();
                        System.out.println("Insira a nova password:");
                        String nova = input.nextLine();
                        try{
                            u.setPassWord(nova, antiga);
                            System.out.println("Password alterada com sucesso\n");
                        }
                        catch(PasswordErradaException e){
                            System.out.println("Password errada\n");
                        }
                case 4: break;
                
            }
        }
        
    
    }
    
    /**
     * Dados pessoais da Loja
     */
    private static void dadosPessoais(Loja l){
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 6){
            System.out.println("Escolha (1-5) para alterar os valores");
            System.out.println("1-Nome: " + l.getNome());
            System.out.println("2-Tag: " + l.getTag());
            System.out.println("3-Password: ******");
            System.out.println("4-Numero de pessoas na fila: "+ l.getPessoasEspera());
            System.out.println("5-Encomendas para entrega: "+ l.getEncomendasProntas());
            System.out.println("6-Sair");
            

            do{
                opcao = leInteiro(6);
            }while( opcao == -1);
            
            switch(opcao){
                case 1: System.out.println("Insira nome");
                        l.setNome(input.nextLine());
                        break;
                case 2: System.out.println("Não é possível alterar o tag\n");
                        break;
                case 3: System.out.println("Insira a password antiga:");
                        String antiga= input.nextLine();
                        System.out.println("Insira a nova password:");
                        String nova = input.nextLine();
                        try{
                            l.setPassWord(nova, antiga);
                            System.out.println("Password alterada com sucesso\n");
                        }
                        catch(PasswordErradaException e){
                            System.out.println("Password errada\n");
                        }
                        break;
                case 4: try{
                             System.out.println("Atualizar o número de pessoas em espera: ");
                             int novoR=input.nextInt();
                             l.setPessoasEspera(novoR);
                             System.out.println("Atualizado com sucesso");
                            }
                            catch(InputMismatchException e){// se nao for int
                                System.out.println("Número inválido");
                        }      
                        break; 
            
                case 5: System.out.println("Não é possível alterar encomendas\n");
                        break;
  
                case 6: break;
                
            }
        }
        
    
    }
    
    /**
     * Dados pessoais da Transportadora
     */
    private static void dadosPessoais(Transportadora t){
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 10){
            System.out.println("Escolha (1-9) para alterar os valores");
            System.out.println("1-Nome: " + t.getNome());
            System.out.println("2-Tag: " + t.getTag());
            System.out.println("3-Password: ******");
            System.out.println("4-Nif: "+ t.getNif());
            System.out.println("5-Raio de acção: "+ t.getRaio());
            System.out.println("6-Velocidade média: "+ t.getVelocidade());
            System.out.println("7-Preço por km percorrido: "+ t.getPrecoKm());
            System.out.println("8-Número de kms percorridos: "+ t.getKmsTotal());
            System.out.println("9-Classificação: "+ t.getClassificacao());
            System.out.println("10-Sair");
            

            do{
                opcao = leInteiro(10);
            }while( opcao == -1);
            
            switch(opcao){
                case 1: System.out.println("Insira nome");
                        t.setNome(input.nextLine());
                        break;
                case 2: System.out.println("Não é possível alterar o tag\n");
                        break;
                case 3: System.out.println("Insira a password antiga:");
                        String antiga= input.nextLine();
                        System.out.println("Insira a nova password:");
                        String nova = input.nextLine();
                        try{
                            t.setPassWord(nova, antiga);
                            System.out.println("Password alterada com sucesso\n");
                        }
                        catch(PasswordErradaException e){
                            System.out.println("Password errada\n");
                        }
                        break;
                case 4: System.out.println("Não é possível alterar o NIF\n");
                        break; 
                        
                case 5: try{
                             System.out.println("Insira o novo raio de acção: ");
                             double novoR=input.nextDouble();
                             t.setRaio(novoR);
                             System.out.println("Raio alterado com sucesso");
                            }
                            catch(InputMismatchException e){// se nao for int
                                System.out.println("Número inválido");
                        }      
                        break;
                case 6: try{
                             System.out.println("Insira a nova velocidade média: ");
                             double novoV=input.nextDouble();
                             t.setVelocidade(novoV);
                             System.out.println("Velocidade alterada com sucesso");
                            }
                            catch(InputMismatchException e){// se nao for int
                                System.out.println("Número inválido");
                        } 
                        break;
                case 7: try{
                             System.out.println("Insira o novo preço por km: ");
                             double novoP=input.nextDouble();
                             t.setPrecoKm(novoP);
                             System.out.println("Preço alterado com sucesso");
                            }
                            catch(InputMismatchException e){// se nao for int
                                System.out.println("Número inválido");
                        } 
                        break;
                case 8: System.out.println("Não é possível alterar o número de kms percorridos\n");
                        break;        
                case 9: System.out.println("Não é possível alterar a classificação\n");
                        break;
  
                case 10: break;
                
            }
        }
        
    
    }
    
    
    /**
     * Dados pessoais do voluntário
     */
    private static void dadosPessoais(Voluntario v){
        Scanner input = new Scanner(System.in);
        int opcao = -1;
        
        while(opcao != 7){
            System.out.println("Escolha (1-5) para alterar os valores");
            System.out.println("1-Nome: " + v.getNome());
            System.out.println("2-Tag: " + v.getTag());
            System.out.println("3-Password: ******");
            System.out.println("4-Raio de acção: "+ v.getRaio());
            System.out.println("5-Velocidade média: "+ v.getVelocidade());
            System.out.println("6-Classificação: "+ v.getClassificacao());
            System.out.println("7-Sair");
            

            do{
                opcao = leInteiro(7);
            }while( opcao == -1);
            
            switch(opcao){
                case 1: System.out.println("Insira nome");
                        v.setNome(input.nextLine());
                        break;
                case 2: System.out.println("Não é possível alterar o tag\n");
                        break;
                case 3: System.out.println("Insira a password antiga:");
                        String antiga= input.nextLine();
                        System.out.println("Insira a nova password:");
                        String nova = input.nextLine();
                        try{
                            v.setPassWord(nova, antiga);
                            System.out.println("Password alterada com sucesso\n");
                        }
                        catch(PasswordErradaException e){
                            System.out.println("Password errada\n");
                        }
                        break;
                case 4: try{
                             System.out.println("Insira o novo raio de acção: ");
                             double novoR=input.nextDouble();
                             v.setRaio(novoR);
                             System.out.println("Raio alterado com sucesso");
                            }
                            catch(InputMismatchException e){// se nao for int
                                System.out.println("Número inválido");
                        } 
                        break;
                case 5: try{
                             System.out.println("Insira a nova velocidade média: ");
                             double novoV=input.nextDouble();
                             v.setVelocidade(novoV);
                             System.out.println("Velocidade alterada com sucesso");
                            }
                            catch(InputMismatchException e){// se nao for int
                                System.out.println("Número inválido");
                        }  
                        break;
                case 6: System.out.println("Não é possível alterar a classificação\n");
                        break;
  
                case 7: break;
                
            }
        }
        
    
    }
    
    public static Localizacao lePosicao(){
        Scanner input = new Scanner(System.in);
        boolean flag = true;
        double x,y; //latitude e longitude
        x=y=0;
        
        while(flag){
            try{
                System.out.println("Latitude(x) = ");
                x = input.nextDouble();
                System.out.println("Longitude(y) = ");
                y = input.nextDouble();
                flag=false;
            }
            catch(InputMismatchException e){// se nao for int
                System.out.println("Insira coordenadas validas");
                input.next();
            }
        }
        return new Localizacao(x,y);
    }
    
    /**
     * Adicionar encomenda pronta para entrega
     */
    private static void adicionarEncomendaPronta(Loja l, DataBase db){
        Scanner input = new Scanner(System.in);
        System.out.println("Insira o código da encomenda pronta para entrega:");
        String codEncomenda=input.nextLine();
        if(!db.existeEncomenda(codEncomenda)){
            System.out.println("Código de encomenda inválida");
            return ;
        }
        else{
        Encomenda e = db.getEncomenda(codEncomenda);
        l.addEncomenda(e);
       }
    }
    
    /**
     * Fazer uma Encomenda 
     */
    private static void fazerEncomenda(Utilizador u, DataBase db){
        System.out.println("*** Fazer entrega de encomenda ***\n Insira as suas coordenadas de entrega\n");
        u.setPosicao(lePosicao());
        
        System.out.print("Que loja pretende pedir entrega?\n");
        Loja l = new Loja();
        try{
            l = escolheLoja(db);
        }catch(UserNaoExistenteException e){
            System.out.println("Loja não existente");
            return;
        }
        
        String email = "";
        email = l.getTag();
        
        System.out.println("*** Escolher Encomenda ***\n");
        Encomenda en = new Encomenda();
        try{
            en = escolheEncomendaLoja(email,db);
        }catch(EncomendaNaoExistenteException e){
            System.out.println("Encomenda não existe");
            return;
        }
        
        
        int opcao=1;
        boolean flagV, flagT;
        flagT = flagV = false;
        Voluntario v = new Voluntario();
        Transportadora t = new Transportadora();
        Localizacao lol = new Localizacao();
        Localizacao lou = new Localizacao();
        
        
        while(!flagV && !flagT &&(opcao != 3)){
            System.out.println("1-Escolher Voluntário\n2-Escolher Transportadora\n3-Sair");
            do{
                opcao = leInteiro(3);
            }while(opcao == -1);
            
            switch(opcao){
                case 1: try{
                            v=escolheVoluntario(db);
                            flagV=true;
                       }
                       catch(UserNaoExistenteException y){
                           System.out.println("Voluntário não encontrado");
                       }
                       break;
                case 2: try{
                            t=escolheTransportadora(db);
                            flagT = true;
                        }
                        catch(UserNaoExistenteException y){
                            System.out.println(y);
                        }
                        break;
                case 3: break;        
            
            }
        }
        
        Localizacao x = u.getPosicao();//pega na localizacao do utilizador(onde deseja ser entregue)
        
        if(flagT){
            int opcao2 = -1;
            while(opcao2!=2){
                if(l.getPosicao().distance(x)>t.getRaio()){
                    System.out.println("Raio de acção da transportadora inferior à distancia de entrega pretendida");
                    break;
                }
                
                System.out.println("Orçamento:\n"); 
                Transporte.antesTransporteT(u,t,l,en,x,db);   
                System.out.println("1-Aceito\n2-Não aceito\n");
                do{
                    opcao2 = leInteiro(2);
                }while(opcao2 == -1);
            
                switch(opcao2){
                    case 1:try{
                                Transporte.fazTransporteT(u,t,l,en,x,db);
                                System.out.println("Transporte efectuado com sucesso\n");
                                
                            }
                           catch(UserNaoExistenteException y){
                                System.out.println("Ups, algo no transporte correu mal");
                           }
                            break;     
                    case 2: flagT=false;
                            break;        
                }  
                break;
            }                
            
        }
        else{
          boolean flag = false;  
           while(flag!=true){ 
                 
            try{
                if(l.getPosicao().distance(x)>v.getRaio()){
                    System.out.println("Raio de acção do voluntário inferior à distancia de entrega pretendida");
                    break;
                }
                Transporte.fazTransporteV(u,v,l,en,x,db);
                System.out.println("***Transporte efectuado com sucesso***\n");
                flag=true;
           }
            catch(UserNaoExistenteException y){
              System.out.println("Ups, algo no transporte correu mal");
            }
          } 
        }
    }
    
    /**
     * Escolher a Loja 
     */
    private static Loja escolheLoja(DataBase db) throws UserNaoExistenteException{
        Scanner input = new Scanner(System.in);
        System.out.println("Tag\n");
        String mail = input.next();
        if(db.existeUtilizador(mail))
            return (Loja) db.getUser(mail);   
        else
            throw new UserNaoExistenteException();
    }
    /**
     * Escolher encomenda da Loja
     */
    private static Encomenda escolheEncomendaLoja(String emailLoja,DataBase db) throws EncomendaNaoExistenteException{
        Scanner input = new Scanner(System.in);
        System.out.println("Insira o código da encomenda\n");
        String codigo = input.next();
        Loja l = (Loja)db.getUser(emailLoja);
        if(l.encomendaDisponivel(codigo))
            return (Encomenda) db.getEncomenda(codigo);
        else
            throw new EncomendaNaoExistenteException("Encomenda não pronta para entrega");
    
    }
    
    /**
     * Escolher Transportadora
     */
    private static Transportadora escolheTransportadora(DataBase db) throws UserNaoExistenteException{
        Scanner input = new Scanner(System.in);
        System.out.println("Insira a tag da transportadora\n");
        String mail = input.next();
        Transportadora t;
        if(db.existeUtilizador(mail)){
            t = (Transportadora) db.getUser(mail);
            if(t.getDisponivel())
                return t;
            else 
                throw new UserNaoExistenteException("Transportadora não está disponivel");
        }
        else
            throw new UserNaoExistenteException("Transportadora não existente");
    }
    
    /**
     * Escolher Voluntario
     */
    private static Voluntario escolheVoluntario(DataBase db) throws UserNaoExistenteException{
        Scanner input = new Scanner(System.in);
        System.out.println("Insira a tag do voluntário\n");
        String mail = input.next();
        Voluntario v;
        if(db.existeUtilizador(mail)){
            v = (Voluntario) db.getUser(mail);
            if(v.getDisponivel())
                return v;
            else 
                throw new UserNaoExistenteException("Voluntário não está disponivel");
              
        }
        else
            throw new UserNaoExistenteException("Voluntário não existente");
    }
    
    private static int countLines(String str){
        String[] lines = str.split("\r\n|\r|\n");
        return lines.length;
    }
    
    /**
     * Fazer Sign In
     */
    private static void signIn(DataBase db){
        Scanner input = new Scanner(System.in);
        
        System.out.println(" *** Sign In ***\n1-Cliente\n2-Voluntário\n3-Transportadora\n4-Loja\n5-Voltar ao menu");
        int opcao=-1;
        do{
            opcao = leInteiro(5);
        }while(opcao==-1);
        
        if(opcao == 5)
            return;
        
        String email;
        do{
            System.out.println("Tag:");
            email = input.nextLine();
            if(db.existeUtilizador(email))
                System.out.println("Este tag já existe");
        }while(db.existeUtilizador(email));
        
        System.out.println("Nome:");
        String nome = input.nextLine();
        
        System.out.println("Palavra pass:");
        String pass = input.nextLine();
        
        
        switch(opcao){
            case 1: User novoU = new Utilizador();
                    novoU = new Utilizador(email,nome,pass,new Localizacao());
                    db.add(novoU);
                    break;
                    
            case 2: User novoV = new Voluntario(); 
                    System.out.println("Insira o raio de deslocação");
                    double raiov = leInteiro(100);
                    System.out.println("Insira a velocidade por km de deslocação");
                    double velov = leInteiro(100);
                    novoV = new Voluntario(email,nome,pass,raiov,velov,false,new Localizacao());
                    db.add(novoV);
                    break;
            case 3: User novoT = new Transportadora(); 
                    System.out.println("Insira o raio de deslocação");
                    double raiot = leInteiro(100);
                    System.out.println("Insira o preço por km de deslocação");
                    double precot = leInteiro(100);
                    System.out.println("Insira o nif");
                    int nif = leInteiro(100);
                    novoT = new Transportadora(email,nome,pass,raiot,precot,nif,false,new Localizacao());
                    db.add(novoT);
                    break;
            case 4: User novoL = new Loja();
                    novoL = new Loja(email,nome,pass,new Localizacao());
                    db.add(novoL);
                    break;
        }
        System.out.println("***A sua conta foi registada***");
    }
    
    /*
    private static String totalFacturado(Transportadora t, DataBase db){
        StringBuilder sb = new StringBuilder();
        sb.append("Lucro total:");
        sb.append(t.totalFacturado(db));
        return sb.toString();
    }
    */
} 

