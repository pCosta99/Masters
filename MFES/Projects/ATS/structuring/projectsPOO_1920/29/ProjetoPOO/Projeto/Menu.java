import java.util.Scanner;
import java.util.List;
import java.io.IOException;
import java.util.HashMap;
import java.util.ArrayList;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.FileOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

public class Menu
{   
    public static void main(String args[]) throws IOException, JaRegistadoException, NaoExisteException, ClassNotFoundException {
        Login app = new Login();
        ReadWriteLogs readW = new ReadWriteLogs();
        int carregar = 0;
        
        Scanner input = new Scanner(System.in);
        System.out.println("Carregar estado antigo?\n1-Sim\n2-Nao\n");
        carregar = input.nextInt();
        
        if(carregar == 1) {
            try {
                ObjectInputStream ois = new ObjectInputStream(new FileInputStream("Estado.dat"));
                app = (Login) ois.readObject();
                ois.close();
            }
            catch(Exception e) {
                System.out.println("Ocorreu um erro a carregar! (" + e.getMessage() + ")");
            }    
        }    
        
        int menu = 0;
        int menuregisto =0;
        int menuusuario=0;
        int menuvoluntario = 0;
        int menutransportadora =0;
        int menuloja =0;
        int registos=0;
        
        while(menu >= 0 && menu < 8) {
            System.out.println("---------MENU INICIAL------------");
            System.out.println("1 - Login ");
            System.out.println("2 - Registar");
            System.out.println("3 - Registos efetuados ");
            System.out.println("4 - Top 10 usuarios");
            System.out.println("5 - Top 10 transportadoras");
            System.out.println("6 - Carregar ficheiro LOGS");
            System.out.println("7 - Sair");
            
            menu = input.nextInt();
            input.nextLine();
            
            if(menu == 1 ){ 
                System.out.println("Codigo: ");
                String cod = input.nextLine();

                System.out.println("Pass: ");
                String pass = input.nextLine();
                String letra = cod.substring(0, 1);
                
                if(letra.equals("u")){
                     if (app.validaUtilizador(cod, pass)){ 
                        System.out.println("Login com Sucesso!");
                         
                          while(menuusuario>=0 && menuusuario<7){ 
                             System.out.println("---------USUARIO------------");
                             System.out.println("1 - Solicitar entrega ");
                             System.out.println("2 - Entregas pendentes a aceitação");
                             System.out.println("3 - Informações encomenda ");
                             System.out.println("4 - Classificar Voluntário");
                             System.out.println("5 - Classificar Transportadora");
                             System.out.println("6 - Sair");
                             
                             
                             menuusuario = input.nextInt();
                             input.nextLine();
                             
                             if(menuusuario==1){ 
                                 System.out.println("Codigo Encomenda(exemplo:e42): ");
                                 String codencomenda = input.nextLine();
                                 String letraencomenda = codencomenda.substring(0, 1);
                                 
                                 
                                 ArrayList<LinhaEncomenda> infoprodutos = new ArrayList<>();
                                 ArrayList<String> usuariosenc = new ArrayList<>();
                                 
                                 if(!letraencomenda.equals("e")){ 
                                     while(!letraencomenda.equals("e")){
                                         System.out.println("Codigo invalido(tente novo codigo(e+numero))");
                                         codencomenda = input.nextLine();
                                         letraencomenda = codencomenda.substring(0, 1);
                                        }
                                 }
                                
                                 System.out.println("Codigo Loja na qual pretende fazer a encomenda(l+numero) : ");
                                 String codloja = input.nextLine();
                                 String letraloja = codloja.substring(0,1);
                                 if(!letraloja.equals("l")){ 
                                     while(!letraloja.equals("l")){
                                         System.out.println("Codigo de loja invalido(tente novo codigo(l+numero))");
                                         codloja = input.nextLine();
                                         letraloja = codloja.substring(0, 1);
                                        }
                                 }
                                
                                 System.out.println("Peso da Encomenda: ");
                                 Double peso = input.nextDouble();
                                 
                                 int menucompras =0;
                                 while(menucompras>=0 && menucompras<3){ 
                                           System.out.println("---------Produtos------------");
                                           System.out.println("1 - Adicionar Produto ");
                                           System.out.println("2 - Sair");
                                           menucompras = input.nextInt();
                                           
                                           if(menucompras==1){ 
                                               
                                               
                                               
                                               System.out.println("Codigo do Produto: ");
                                                input.nextLine();
                                               String codproduto = input.nextLine();
                                               
                                               
                                               System.out.println("Descrição do Produto: ");
                                               String descricao = input.nextLine();
                                               
                                               System.out.println("Quantidade: ");
                                               Double quant = input.nextDouble();
                                               
                                               System.out.println("Valor Unitario: ");
                                               Double valor = input.nextDouble();
                                               
                                               LinhaEncomenda linha = new LinhaEncomenda(codproduto,descricao,quant,valor);
                                               infoprodutos.add(linha);
                                            }
                                           if(menucompras==2){ 
                                               break;
                                            }
                                    }
                                    app.nregistarEncomenda(codencomenda,cod,codloja,peso,infoprodutos);
                                    app.adicionarUsuario(cod);
                                    
                                    System.out.println("Encomenda registada com Sucesso");                                
                             }
                               
                             if(menuusuario==2){ 
                                app.encomendasDisponiveisV(cod);
                                
                                app.encomendasDisponiveisT(cod);
                                
                                System.out.println("Escolha o encarregado da encomenda: (codigo)");
                                String codAceite = input.nextLine();
                                
                                System.out.println("Insira o codigo da loja: ");
                                String codLojaAceite = input.nextLine();
                                
                                System.out.println("Insira o codigo da encomenda: ");
                                String codEncAceite = input.nextLine();
                                
                                if(app.getAllTransportadoras().containsKey(codAceite)) {
                                    app.fatsum(cod, codAceite,codEncAceite, codLojaAceite);
                                    app.pedidoToConcluido(app.getPedidosEncomendas().get(codEncAceite));
                                    double kmtot = app.kmsum(codAceite,codLojaAceite,cod);
                                    app.adicionarKm(kmtot);
                                    
                                }   
                             }
                                
                             if(menuusuario==3){ 
                                System.out.println(app.getEncConcluidas());
                                System.out.println(app.getusuariosenc());
                                }
                                
                             if(menuusuario==4){ 
                                System.out.println("Introduza o codigo do voluntario que pretende classificar: ");
                                String codVclass = input.nextLine();
                                
                                System.out.println("Introduza a classificacao que pretende dar (de 0 a 5): ");
                                int classi = input.nextInt();
                                
                                app.classificaVoluntario(codVclass ,classi);
                                System.out.println(app.getAllVoluntarios().get(codVclass).getClassificacao());
                                }
                             
                             if(menuusuario==5){ 
                                System.out.println("Introduza o codigo da transportadora que pretende classificar: ");
                                String codTclass = input.nextLine();
                                
                                System.out.println("Introduza a classificacao que pretende dar (de 0 a 5): ");
                                int classi = input.nextInt();
                                
                                app.classificaTransportadora(codTclass ,classi);
                                System.out.println(app.getAllVoluntarios().get(codTclass).getClassificacao());
                                }   
                                
                             if(menuusuario==6){ 
                                break;
                                }
                           }
                        
                      }
                     else System.out.println("Login Falhou!");
                    }
                if(letra.equals("v")){ 
                    if(app.validaVoluntario(cod,pass)){ 
                       System.out.println("Login com Sucesso!");
                          while(menuvoluntario>=0 && menuvoluntario<3){ 
                              System.out.println("---------VOLUNTARIO------------");
                              
                              
                              System.out.println("1 - Informações encomendas concluidas ");
                              System.out.println("2- Sair");
            
                              menuvoluntario = input.nextInt();
                              input.nextLine();
                              if(menuvoluntario==1){ 
                                System.out.println(app.getEncConcluidas());
                                }
                              if(menuvoluntario==2){ 
                                break;
                                }
                        }
                       }
                    else System.out.println("Login Falhou!");
                    
                    }
                if(letra.equals("t")){ 
                   if(app.validaTransportadora(cod,pass)){
                       System.out.println("Login com Sucesso!");
                        while(menutransportadora>=0 && menutransportadora<4){ 
                             System.out.println("---------TRANSPORTADORA------------");
                             
                             System.out.println("1 - Informações encomendas concluidas ");
                             System.out.println("2 - Total faturado ");
                             System.out.println("3 - Sair");
            
                              menutransportadora = input.nextInt();
                              input.nextLine();
                             if(menutransportadora==1){ 
                                System.out.println(app.getEncConcluidas());
                                }
                             if(menutransportadora==2){ 
                                System.out.println(app.getAllTransportadoras().get(cod).getFaturado());
                        
                                }
                             if(menutransportadora==3){ 
                                break;
                                }
                        }
                    }
                   else System.out.println("Login Falhou!");
                }
                if(letra.equals("l")){
                   if(app.validaLoja(cod,pass)){ 
                       System.out.println("Login com Sucesso!");
                       while(menuloja>=0 && menuloja<4){ 
                          System.out.println("---------LOJA------------");
                          
                          System.out.println("1 - Informações encomendas concluidas ");
                          System.out.println("2 - Pedidos pendentes a aceitação");
                          System.out.println("3 - Sair");
            
                          menuloja = input.nextInt();
                          input.nextLine();
                          if(menuloja==1){ 
                            System.out.println(app.getEncConcluidas());
                            }
                          if(menuloja==1){
                             System.out.println(app.getPedidosEncomendas());
                            }
                          if(menuloja==2){ 
                                break;
                                }
                        }
                    }
                   else System.out.println("Login Falhou!");
                }
                else System.out.println("Usuário não válido");
                  
            }
            if(menu==2){ 
                
                while(menuregisto>=0 && menuregisto<6){
                
                      System.out.println("---------REGISTAR------------");
                      System.out.println("1 - Utilizador ");
                      System.out.println("2 - Voluntario");
                      System.out.println("3 - Transportadora");
                      System.out.println("4 - Loja");
                      System.out.println("5 - Voltar");
                 
            
                     menuregisto = input.nextInt();
                     input.nextLine();
                
                   if(menuregisto == 1){
                         System.out.println("Codigo Utilizador(exemplo:u42): ");
                         String codu = input.nextLine();
                         String letrau = codu.substring(0,1);
                        
                         if(!letrau.equals("u")){ 
                            while(!letrau.equals("u")){
                                System.out.println("Codigo invalido(tente novo codigo(u+numero))");
                                codu = input.nextLine();
                                letrau = codu.substring(0,1);
                            }
                           }
                
                         System.out.println("Password: ");
                         String pass = input.nextLine();
                 
                         System.out.println("Nome Utilizador: ");
                         String nome = input.nextLine();
                
                         System.out.println("Longitude: ");
                         double lat = input.nextDouble();
                
                         System.out.println("Latitude: ");
                         double longi = input.nextDouble();
                
                         GPS local = new GPS(longi, lat);
                
                      try {
                         app.nregistarUtilizador(codu, nome, local, pass);
                         System.out.println("Utilizador registado com Sucesso");
                      }    
                      catch(Exception e) {
                         System.out.println(e);
                       }
                       }
                   if(menuregisto == 2){ 
                        System.out.println("Codigo Voluntario(v+número i.e: v42): ");
                        String codv = input.nextLine();
                        String letrav = codv.substring(0, 1);
                        
                         if(!letrav.equals("v")){ 
                            while(!letrav.equals("v")){
                                System.out.println("Codigo invalido(tente novo codigo(v+numero))");
                                codv = input.nextLine();
                                letrav = codv.substring(0, 1);
                            }
                           }
                        
                        
                        System.out.println("Password: ");
                        String pass = input.nextLine();
            
                        System.out.println("Nome do Voluntario: ");
                        String nome = input.nextLine();
            
                        System.out.println("Longitude: ");
                        double lat = input.nextDouble();
            
                        System.out.println("Latitude: ");
                        double longi = input.nextDouble();
                
                        System.out.println("Raio: ");
                        double raio = input.nextDouble();
                
                        GPS local = new GPS(longi, lat);
         
                       try{ 
                            app.nregistarVoluntario(codv,nome,local,raio,pass);
                            System.out.println("Voluntário registado com Sucesso");
                       }
                      catch(Exception e){ 
                           System.out.println(e);
                       }
                        }
                   if(menuregisto == 3){ 
                       System.out.println("Codigo Transportadora(t+número i.e: t42): ");
                       String codt = input.nextLine();
                       String letrat = codt.substring(0, 1);
                         if(!letrat.equals("t")){ 
                            while(!letrat.equals("t")){
                                System.out.println("Codigo invalido(tente novo codigo(t+numero))");
                                codt = input.nextLine();
                                letrat = codt.substring(0, 1);
                            }
                           }
                       System.out.println("Password: ");
                       String pass = input.nextLine();
            
                       System.out.println("Nome da Empresa: ");
                       String nome = input.nextLine();
            
                       System.out.println("Longitude: ");
                       double lat = input.nextDouble();
            
                       System.out.println("Latitude: ");
                       double longi = input.nextDouble();
                
                       System.out.println("Nif: ");
                       String nif = input.nextLine();
                
                       System.out.println("Raio: ");
                       double raio = input.nextDouble();
                
                       System.out.println("Preço(p/km):");
                       double preco = input.nextDouble();
                
                       GPS local = new GPS(longi, lat);
               
                       try{ 
                            app.nregistarTransportadora(codt,nome,local,nif,raio,preco,pass);
                            System.out.println("Transportadora registada com Sucesso");
                         }
                       catch(Exception e){ 
                            System.out.println(e);
                           }
               
                        }
                    if(menuregisto == 4){ 
                          System.out.println("Codigo Loja(l+número i.e: l42): ");
                          String codl = input.nextLine();
                          String letral = codl.substring(0, 1);
                          
                           if(!letral.equals("l")){ 
                            while(!letral.equals("l")){
                                System.out.println("Codigo invalido(tente novo codigo(l+numero))");
                                codl = input.nextLine();
                                letral = codl.substring(0, 1);
                            }
                           }
                          System.out.println("Password: ");
                          String pass = input.nextLine();
                 
                          System.out.println("Nome da Loja:");
                          String nome = input.nextLine();
                 
                          System.out.println("Longitude: ");
                          double lat = input.nextDouble();
                 
                          System.out.println("Latitude: ");
                          double longi = input.nextDouble();
                 
                          GPS local = new GPS(longi, lat);
                       try{ 
                          app.nregistarLoja(codl,nome,local,pass);
                          System.out.println("Loja registada com Sucesso");
                         }
                       catch(Exception e){ 
                            System.out.println(e);
                          }
                
                        }
                    if(menuregisto == 5){ 
                      break;
                    }
                }
               
            
                
                
            
            }
            if (menu == 3){
                while(registos>=0 && registos<8){ 
                     System.out.println("---------REGISTOS------------");
                     System.out.println("1 - Utilizadores ");
                     System.out.println("2 - Voluntarios");
                     System.out.println("3 - Transportadoras");
                     System.out.println("4 - Lojas");
                     System.out.println("5 - Encomendas");
                     System.out.println("6 - Encomendas Aceites");
                     System.out.println("7 - Voltar");
            
                     registos = input.nextInt();
                     input.nextLine();
                    if(registos==1){ 
                      System.out.println("Utilizadores: " + app.getAllUtilizadores().toString());
                      
                    }
                    if(registos == 2){ 
                      System.out.println("Voluntarios: " + app.getAllVoluntarios().toString());
                      
                    }
                    if(registos == 3){ 
                      System.out.println("Transportadoras: " + app.getAllTransportadoras().toString());
                      
                    }
                    if(registos == 4){ 
                     System.out.println("Lojas: " + app.getAllLojas().toString());
                     
                    }
                    if(registos == 5){ 
                     System.out.println("Encomendas: " + app.getPedidosEncomendas().toString());
                     
                    }
                    if(registos == 6){ 
                     System.out.println("Encomendas: " + app.getEncConcluidas().toString());
                     
                    }
                    if(registos == 7){ 
                     break;
                    }
                }
                
            }
            if(menu == 4){ 
              
              System.out.println(app.getWordFrequencies(app.getusuariosenc()));
              
            }
            if(menu==5){ 
                
                System.out.println(app.buscatransportadoras(app.so10(app.sortlista(app.getKmT()))));
              
            }
            if(menu == 6) {
                
                readW.leCSV("LOGS.txt", app);
                menu = 0;   
                }
               
            if (menu == 7){
                save(app);
                readW.escreveEmFicheiroTexto("LOGS.txt", app);
                break;
        }   
    }
}
    
    public static void save(Login l){
        try {
            ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream("Estado.dat"));
            oos.writeObject(l);
            oos.flush();
            oos.close();
        }
        catch(Exception e) {
            System.out.println("Erro: " + e.getMessage());
        }    
    }   
       
}
