import java.util.Scanner;
import java.io.*;
import Exceptions.*;
public class Interface
{
    //Central c;
    
    public static void showMenu(){
    StringBuilder res=new StringBuilder();
    res.append("Para sair digite 0\n")
    .append("Para registar Utilizador digite 1\n")
    .append("Para registar Loja digite 2\n")
    .append("Para registar Voluntario digite 3\n")
    .append("Para registar Transportadora digite 4\n")
    .append("Para fazer LogIn digite 5\n")
    .append("Para Entrar Como Moderador digite 6\n")
    .append("[DEBBUG] Para Mostrar toda Informaçao Atual digite 7\n");
    System.out.println(res.toString());
    }
    public static void showMenuUtilizador(){
    StringBuilder res=new StringBuilder();
    res.append("Para sair digite 0\n")
    .append("Para Solicitar Encomenda digite 1\n")
    .append("Para Ver Lojas digite 2\n")
    .append("Para Ver Propostas digite 3\n")
    .append("Para Aceitar Proposta digite 4\n")
    .append("Para ver entregas entre um determinado Tempo digite 5\n")
    .append("Para ver entregas feitas por um Entregador digite 6\n")
    .append("Para ver minhas entregas digite 7\n")
    .append("Para Verificar minha Entrega digite 8\n")
    .append("Para classificar digite 9\n")
    .append("Para ver classificacao digite 10\n");
    System.out.println(res.toString());
    }
    public static void showMenuLoja(){
    StringBuilder res=new StringBuilder();
    res.append("Para sair digite 0\n")
    .append("Para Solicitar Encomenda digite 1\n")
    .append("Para Adicionar um Produto Ao Seu Catalogo Digite 2\n")
    .append("Para Verificar Permissao Entrega de Medicamentos digite 3\n")
    .append("Para alterar Permissao Entrega de Medicamentos digite 4\n")
    .append("Para Alterar informaçao da Fila Digite 5\n")
    .append("Para Retirar Pontos Digite 6\n")
    .append("Para Ver o seu Catalogo Digite 7\n");
    System.out.println(res.toString());
    }
    public static void showMenuVoluntario(){
    StringBuilder res=new StringBuilder();
    res.append("Para sair digite 0\n")
    .append("Para Ver Encomendas Que Posso Entregar digite 1\n")
    .append("Para Aceitar Entregar uma Encomenda digite 2\n")
    .append("Para Registar Entrega digite 3\n")
    .append("Para Registar Entrega de data Agora digite 4\n")
    .append("Para ver seus Pontos Digite 5\n")
    .append("Para ver Minhas Entregas Digite 6\n");
    System.out.println(res.toString());
    }
    public static void showMenuTransportadora(){
    StringBuilder res=new StringBuilder();
    res.append("Para sair digite 0\n")
    .append("Para Ver Encomenda Que Posso Entregar digite 1\n")
    .append("Para Calcular Preco de Entrega de 1 ou mais Encomendas digite 2\n")
    .append("Para Fazer Proposta digite 3\n")
    .append("Para Registar Entrega digite 4\n")
    .append("Para Registar Entrega agora digite 5\n")
    .append("Para Ver Top10 Utilizadores digite 6\n")
    .append("Para Ver Top10 Transportadoras digite 7\n")
    .append("Para ver Total Faturado entre um determinado Tempo digite 8\n")
    .append("Para ver Minhas entregas digite 9\n");
    System.out.println(res.toString());
    }
    public static void showMenuModerador(){
    StringBuilder sb = new StringBuilder ();
    sb.append("Para Sair Digite 0\n")
    .append("Para Alterar Condicoes Digite 1\n")
    .append("Para Alterar Tempo Por Fila Digite 2\n")
    .append("Para Alterar Velocidade Entregador Estimada Digite 3\n")
    .append("Para Apagar Entregas e Encomendas Digite 4\n")
    .append("Para Criar SCV digite 5\n")
    .append("Para Alterar PontosMedioPorKm Digite 6\n")
    .append("Para Carregar de SCV digite 7 (Atençao:Nao Carrega Propostas,Entregas ou Catalogo de Lojas)\n")
    .append("Para Substituir todos os Dados por SCV digite 8 (Atençao:Nao Carrega Propostas,Entregas ou Catalogo de Lojas)\n");
    System.out.println(sb.toString());
    }
    public static void main(String[] args)
    {
        //Central c=null;
        Central c=new Central();
        try{
           ObjectInputStream ois=new ObjectInputStream(new FileInputStream("../central.obj"));
           c=(Central) ois.readObject();}
        catch (FileNotFoundException e){e.printStackTrace();c=new Central();}
        catch (IOException e){e.printStackTrace();c=new Central();}
        catch (ClassNotFoundException e){e.printStackTrace();c=new Central();}
        //System.out.println(c.toString());
        Scanner sc = new Scanner(System.in);
        Scanner sc1=new Scanner(System.in);
        showMenu();
        int controlo=sc1.nextInt();
        while(controlo!=0){
         try{
          switch (controlo){
           case 1 : 
                System.out.println("Digite:codigo,nome,coordenadaX,coordenadaY");
                System.out.println("Digite ex: u80,Guilherme JoÃ£o Cerqueira da Mota Miranda,-18.299461,-35.02321");
                c.adicionaUtilizador(sc.nextLine());
                break;
           case 2 :
                System.out.println("Digite:codigo,nome,coordenadaX,coordenadaY");
                System.out.println("Digite ex: l29,TV e TelecomunicaÃ§Ãµes,39.627502,33.60112");
                c.adicionaLoja(sc.nextLine());
                break;
                case 3 : 
                System.out.println("Digite:codigo,nome,coordenadaX,coordenadaY,raio");
                System.out.println("Digite ex: v73,Rui Pedro Gomes Coelho,-45.424522,-79.517136,15.0");
                c.adicionaVoluntario(sc.nextLine());
                break;
           case 4 :
                System.out.println("Digite:codigo,nome,coordenadaX,coordenadaY,NIF,Raio,preçoPorKm");
                System.out.println("Digite ex: t31,ZENITHINDEX,31.47522,76.58536,599618020,192.0,1.5");
                c.adicionaTransportadora(sc.nextLine());
                break;
           case 5:
                System.out.println("Digite Seu Id:");
                String id=sc.nextLine();
                System.out.println("Digite Sua Pass(aaaa):");
                String pass=sc.nextLine();
                if(c.logIn(id,pass)){
                 switch (id.substring(0,1)){
                   case "u":menuUtilizador(c,id);break;
                   case "l":menuLoja(c,id);break;
                   case "v":menuVoluntario(c,id);break;
                   case "t":menuTransportadora(c,id);break;
                  } 
                }else System.out.println("Nao Encontramos Seus Dados No Sistema!");
                break;
           case 6:
                System.out.println("Digite Sua Pass(1234):");
                pass=sc.nextLine();
                if (c.logInModerador(pass)) menuModerador(c);
                break;
           case 7:
                System.out.println(c.toString());break;
          }
         }catch(Exception e){e.printStackTrace();}
         //System.out.println(c.toString());
        showMenu();controlo=sc1.nextInt();
      }
      try{
        ObjectOutputStream os=new ObjectOutputStream(new FileOutputStream("../central.obj"));
        os.writeObject(c);
        }catch (FileNotFoundException e){e.printStackTrace();}
        catch (IOException e){e.printStackTrace();}
       //c.guarda(".logs.txt");
       System.out.println("Adeus");
       //System.out.println(c.toString());
    }
    public static void menuUtilizador(Central c,String id){
        System.out.println("Bem Vindo Utilizador "+id);
        Scanner sc = new Scanner(System.in);
        Scanner sc1 = new Scanner(System.in);;
        showMenuUtilizador();
        int controlo=sc1.nextInt();
        String s1,s2;
        double d;
        while(controlo!=0){
         try{
             switch(controlo){
              case 1 :
                System.out.println(c.showLojas());
                System.out.println("Digite id Loja");
                s1=sc.nextLine();
                System.out.println(c.showCatalogo(s1));
                System.out.println("Digite:codigo,descricao,preco,quantidade..");
                System.out.println("exemplo:p40,Molho de pimenta,1.6600878,42.014664,pm40,Arroz,1.6600878,42.014664");
                s2=sc.nextLine();
                System.out.println("Digite Peso estimado:");
                d=sc1.nextDouble();
                System.out.println(c.adicionaEncomenda(id,s1,s2,d));break;
              case 2 :
                System.out.println(c.showLojas());   
              case 3 :
                System.out.println(c.verPropostas(id));break;
              case 4 :
                System.out.println("Digite Id de Quem fez A Proposta");s1=sc.nextLine();
                System.out.println("Digite Id Da Encomenda");s2=sc.nextLine();
                c.aceitarProposta(id,s1,s2);break;
              case 5:
                System.out.println("Digite Data Inicio(yyyy-mm-dd)");s1=sc.nextLine();
                System.out.println("Digite Data Fim(yyyy-mm-dd)");s2=sc.nextLine();
                System.out.println(c.verEntregasTempo(s1,s2));break;
              case 6:
                System.out.println("Digite IdEntregador");s1=sc.nextLine();
                System.out.println(c.verEntregasDe(s1));break;
              case 7:
                System.out.println(c.minhasEntregas(id));break;
              case 8:
                System.out.println("Digite IdEncomenda");s1=sc.nextLine();
                System.out.println(c.verificarEntrega(s1));break;
              case 9:
                System.out.println("Digite idEntregador");s1=sc.nextLine();
                System.out.println("Digite sua Classificacao");d=sc1.nextDouble();
                c.classifica(s1,d);break;
              case 10:
                System.out.println("Digite idEntregador");s1=sc.nextLine();
                System.out.println(c.veClassificacao(s1));break;
             }
           }catch(Exception e){e.printStackTrace();}
          showMenuUtilizador();controlo=sc1.nextInt();
        }
        System.out.println("Adeus");
    }
    public static void menuLoja(Central c,String id){
        System.out.println("Bem Vindo Loja "+id);
        Scanner sc = new Scanner(System.in);
        Scanner sc1 = new Scanner(System.in);;
        showMenuLoja();
        int controlo=sc1.nextInt();
        int i;
        double d;
        String s1,s2;
        boolean bol;
        while(controlo!=0){
           try{
            switch(controlo){
            case 1 :
                //System.out.println(c.showLojas());
                System.out.println("Digite id Utilizador");
                s1=sc.nextLine();
                System.out.println(c.showCatalogo(id));
                System.out.println("Digite:codigo,descricao,preco,quantidade..");
                System.out.println("exemplo:p40,Molho de pimenta,1.6600878,42.014664,pm40,Arroz,1.6600878,42.014664");
                s2=sc.nextLine();
                System.out.println("Digite Peso estimado:");
                d=sc1.nextDouble();
                System.out.println(c.adicionaEncomenda(s1,id,s2,d));break;
            case 2 :
                System.out.println("Digite novo codigo ex:p24 para Produto Normal ou pm30 para Produto Medico");
                s1=sc.nextLine();
                System.out.println("Digite Descricao ex:Computador Acer");
                s2=sc.nextLine();
                System.out.println("Digite Preço:ex 10.0");
                d=sc1.nextDouble();
                c.adicionaProduto(id,s1,s2,d);break;
            case 3 : 
                System.out.println("Digite Id de Entregador");
                System.out.println(c.aceitoTransporteMedicamentos(sc.nextLine()));break;
            case 4 : 
                System.out.println("Digite Id de Entregador");s1=sc.nextLine();
                System.out.println("Digite true or false");bol=sc.nextBoolean();
                c.aceitaMedicamentos(s1,bol);break;
            case 5 :
                System.out.println("Digite Fila");i=sc1.nextInt();
                c.setFila(id,i);break;
            case 6 :
                System.out.println("Digite id do Entregador");s1=sc.nextLine();
                System.out.println("Digite numero De Pontos");d=sc.nextDouble();
                c.retirarPontos(s1,d);
                break;
            case 7 :
                System.out.println(c.showCatalogo(id));
                break;
           }
         }catch(Exception e){e.printStackTrace();}
          showMenuLoja();controlo=sc1.nextInt();
        }
        System.out.println("Adeus");
    }
    public static void menuVoluntario(Central c,String id){
        System.out.println("Bem Vindo Voluntario "+id);
        Scanner sc = new Scanner(System.in);
        Scanner sc1 = new Scanner (System.in);
        showMenuVoluntario();
        int controlo=sc1.nextInt();
        String s1,s2;
        double d;
        while(controlo!=0){
           try{
            switch(controlo){
            case 1 :
                System.out.println(c.encQuePossoEntregar(id));break;
            case 2 :
                System.out.println("Digite Id da Encomenda");s1=sc.nextLine();
                c.aceitarEntregar(s1,id);break;
            case 3:
                System.out.println("Digite IdEncomenda");
                s1=sc.nextLine();
                System.out.println("Digite Data (yyyy-mm-dd)");
                s2=sc.nextLine();
                c.adicionarEntrega(s1,id,s2,0);break;
            case 4:
                System.out.println("Digite IdEncomenda");s1=sc.nextLine();
                c.adicionarEntrega(s1,id,"now",0);break;
            case 5:
                System.out.println(c.getPontos(id));break;
            case 6:
                System.out.println(c.verEntregasDe(id));break;
          }
         }catch(Exception e){e.printStackTrace();}
          showMenuVoluntario();controlo=sc1.nextInt();
        }
        System.out.println("Adeus");
    }
    public static void menuTransportadora(Central c,String id){
        System.out.println("Bem Vindo Transportadora "+id);
        Scanner sc = new Scanner(System.in);
        Scanner sc1 = new Scanner(System.in);
        showMenuTransportadora();
        int controlo=sc1.nextInt();
        String s1,s2;
        double d;
        while(controlo!=0){
          try{
           switch(controlo){
            case 1 :
                System.out.println(c.encQuePossoEntregar(id));
                break;            
            case 2 :System.out.println("Digite Encomendas por Ordem Que Deseja Entregar Separados por ',' ex:e1440,e1014,e8779");
                s1=sc.nextLine(); 
                System.out.println(c.precoRotaTransportadora(s1,id));
                break;
            case 3 :
                System.out.println("Digite Id Da Encomenda");s1=sc.nextLine();
                System.out.println("Digite Preco");s2=sc.nextLine();
                c.adicionaProposta(id,s1,s2);
                break;
            case 4 :
                System.out.println("Digite IdEncomenda");s1=sc.nextLine();
                System.out.println("Digite Data (yyyy-mm-dd)");s2=sc.nextLine();
                c.adicionarEntrega(s1,id,s2,0);break;
            case 5:
                System.out.println("Digite IdEncomenda");s1=sc.nextLine();
                System.out.println("Digite Preco");d=sc1.nextDouble();
                c.adicionarEntrega(s1,id,"now",d);break;
            case 6:
                System.out.println(c.top10Utilizadores());break;
            case 7:
                System.out.println(c.top10Transportadoras());break;
            case 8:
                System.out.println("Digite Data Inicio(yyyy-mm-dd)");s1=sc.nextLine();
                System.out.println("Digite Data Fim(yyyy-mm-dd)");s2=sc.nextLine();
                System.out.println(c.faturacaoTransportadora(id,s1,s2));break;
            case 9:
                System.out.println(c.verEntregasDe(id));break;
          }
         }catch(Exception e){e.printStackTrace();}
          showMenuTransportadora();controlo=sc1.nextInt();
        }
        System.out.println("Adeus");
    }
    public static void menuModerador(Central c){
        System.out.println("Bem Vindo Moderador");
        Scanner sc = new Scanner(System.in);
        Scanner sc1 = new Scanner(System.in);
        Scanner sc2 = new Scanner (System.in);
        showMenuModerador();
        int controlo=sc1.nextInt();
        int i,j,z;
        boolean b;
        String s1,s2;
        double d;
        while(controlo!=0){
          try{
            switch(controlo){
            case 1 :
                 System.out.println("Digite Metreologia Numero de 1-5 Sendo 1 pessimo e 5 otimo");i=sc1.nextInt();
                 System.out.println("Digite Transito Numero de 1-5 Sendo 1 pessimo e 5 otimo");j=sc1.nextInt();
                 System.out.println("Digite true se for tempo de ferias false senao");b=sc1.nextBoolean();
                 c.atualizaFator(i,j,b);
                 break;
            case 2 :System.out.println("Digite Tempo em horas que se espera por cliente em fila"); 
                d=0;
                d=sc2.nextDouble(); 
                c.setTempoPorFila (d);
                break;
            case 3 :
                System.out.println("Digite velocidade km/h que um EntregadorCobrador percorre");
                d=sc2.nextDouble();
                c.setVelocidadeEntregador (d);
                break;
            case 4 :
                System.out.println("Digite Data (yyyy-mm-dd),Vao ser apagados registros anteriores a esta");
                s1=sc.nextLine();c.apagar(s1);
                break;
            case 5 :
                System.out.println("Digite Path\n");
                s1=sc.nextLine();
                c.guarda(s1);
                break;
            case 6 :
                System.out.println("Digite numero de PontosMedioPorKM");
                d=sc2.nextDouble();
                c.setPontosPorKmVoluntario(d);
                break;
            case 7 :
                System.out.println("Digite Path\n");
                s1=sc.nextLine();
                c.load(s1);
            case 8 :
                c.reset();
                System.out.println("Digite Path\n");
                s1=sc.nextLine();
                c.load(s1);
            }
          }catch(Exception e){e.printStackTrace();}
          showMenuModerador();controlo=sc1.nextInt();
        }
        System.out.println("Adeus");
    }
}
