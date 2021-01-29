/**
 * Write a description of class ProjetoTrazAqui here.
 *
 * @author Francisco Luis Rodrigues Claudino, A89493
 * @version Versao 22 - 11-06-2020
 */

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.Scanner;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;
import java.lang.String;
import java.util.Map;
import java.util.HashMap;
import java.io.File;
import java.util.InputMismatchException;
import java.util.Random;
import java.time.LocalDateTime;
import java.lang.NullPointerException;
import java.util.Set;
import java.util.TreeSet;
import java.io.OutputStream;
import java.io.ObjectOutputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.Serializable;

public class ProjetoTrazAqui implements Serializable
{
    private ArrayList<UtilizadorTrazAqui> listaDeUtilizadores;
    private ArrayList<Voluntario> listaDeVoluntarios;
    private ArrayList<EmpresaTransportadora> listaDeTransportadoras;
    private ArrayList<Loja> listaDeLojas;
    private ArrayList<LinhaDeEncomenda> linhaDeEncomenda;
    private ArrayList<Encomenda> listaDeEncAceites;
    private ArrayList<Encomenda> gestaoDeEncomendas;
    private ArrayList<CatalogoDeProdutos> catalogoDeProdutos;
    private ArrayList<RegistoVoluntarios> registoDeVoluntarios;
    private ArrayList<RegistoTransportadoras> registoDeTransportadoras;
    
    public void main () throws Exception
    {   this.listaDeUtilizadores = new ArrayList<UtilizadorTrazAqui>();
        this.listaDeVoluntarios = new ArrayList<Voluntario>();
        this.listaDeTransportadoras = new ArrayList<EmpresaTransportadora>();
        this.listaDeLojas = new ArrayList<Loja>();
        this.linhaDeEncomenda = new ArrayList<LinhaDeEncomenda>();
        this.listaDeEncAceites = new ArrayList<Encomenda>();
        this.gestaoDeEncomendas = new ArrayList<Encomenda>();
        this.catalogoDeProdutos = new ArrayList<CatalogoDeProdutos>();
        this.registoDeVoluntarios = new ArrayList<RegistoVoluntarios>();
        this.registoDeTransportadoras = new ArrayList<RegistoTransportadoras>();
        
        catalogoDeProdutos.add(new CatalogoDeProdutos("Detergente","p4",2.0,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Agua sanitária","p5", 0.5, true,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Esponja de aco","p6",3.0,false,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Sabao em pedra","p7",2.0,true,1.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Sabonete","p8",1.5,true,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Shampoo","p9",1.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Condicionador","p10",1.5,false,3.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Desinfetantes","p11",1.0,true,5.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Lustra moveis","p12",3.5,false,4.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Tira manchas","p13",2.0,false,3.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Limpa vidros","p14",1.0,false,3.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Alcool","p15",1.0,true,4.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Saco do lixo 30l","p16",0.5,false,0.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Saco do lixo 50l","p17",1.0,false,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Sumo garrafa 1l","p19",1.0,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Sumo caixa 500ml","p20",0.5,false,1.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Leite integral litro","p21",1.0,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Leite desnatrado litro","p22",1.0,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Feijao 2kg","p24",2.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Macarrao","p25",2.5,false,3.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Extrato de tomate","p26",1.0,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Sal","p28",0.5,false,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Acucar","p29",0.5,false,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Bolacha","p31",1.5,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Farofa pronta","p33",0.5,false,1.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Farinha de Trigo","p34",0.5,false,1.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Farinha de milho","p35",0.5,false,1.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Farinha de mandioca","p36",0.5,false,1.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Sardiha","p37",2.5,false,5.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Atum","p38",2.5,false,5.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Molho de pimenta","p40",1.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Ervilha","p41",1.5,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Milho verde","p42",1.5,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Doce de leite","p43",2.0,false,3.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Goiabada","p44",2.5,false,3.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Creme de leite","p48",2.5,false,3.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Leite condensado","p49",1.5,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Alface","p51",1.5,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Couve","p52",1.5,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Batata","p53",1.5,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Cenoura","p55",2.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Beterraba","p56",2.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Espinafre","p59",2.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Banana","p60",1.0,false,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Ovos","p61",0.5,false,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Uva","p62",0.5,false,0.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Abacate","p63",2.5,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Melancia","p65",5.0,false,5.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Salsa","p67",0.5,false,1.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Cebola","p68",1.5,false,1.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Queijo","p69",2.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Mussarela","p70",2.0,false,3.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Queijo outros","p71",2.0,false,3.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Manteiga","p72",0.5,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Margarina","p73",0.5,false,2.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Iogurte","p74",1.0,false,2.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Peixe","p76",1.5,false,3.5).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Frango","p77",3.0,false,4.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Carne seca","p79",3.0,false,4.0).clone());
        catalogoDeProdutos.add(new CatalogoDeProdutos("Salsicha","p80",1.5,false,3.0).clone());
        
        try{
            BufferedReader buffer = new BufferedReader (new FileReader("/home/francisco/Desktop/logs20200416.txt"));
            String line;
            while ((line = buffer.readLine()) != null){
                String[] teste = line.split(":");
                String tipo = teste[0];
                String conteudo = teste[1];
                switch (tipo) {
                    case "Utilizador" :  String[] valuesU = conteudo.split(",");
                                         String codigoU = valuesU[0];
                                         String nomeU = valuesU[1];
                                         double latitudeU = Double.parseDouble(valuesU[2]);
                                         double longitudeU = Double.parseDouble(valuesU[3]);
                                         String emailU = geraEmailUser(codigoU);
                                         String passwordU = geraPasswordUser(codigoU);
                                         
                                         UtilizadorTrazAqui u = new UtilizadorTrazAqui(codigoU, nomeU, latitudeU, longitudeU, emailU, passwordU,0);
                                         listaDeUtilizadores.add(u.clone());
                                         line = new String();
                                         break;
                    
                    case "Voluntario" :  String[] valuesV = conteudo.split(",");
                                         String codigoV = valuesV[0];
                                         String nomeV = valuesV[1];
                                         double latitudeV = Double.parseDouble(valuesV[2]);
                                         double longitudeV = Double.parseDouble(valuesV[3]);
                                         double raioV = Double.parseDouble(valuesV[4]);
                                         String emailV = geraEmailVoluntario(codigoV);
                                         String passwordV = geraPasswordVoluntario(codigoV);
                                         
                                         Voluntario v = new Voluntario(codigoV, nomeV, latitudeV, longitudeV, raioV, 0, false, emailV, passwordV,0.0,0);
                                         listaDeVoluntarios.add(v.clone());
                                         line = new String();
                                         break;
                    
                    case "Transportadora" : String[] valuesT = conteudo.split(",");
                                            String codigoE = valuesT[0];
                                            String nomeE = valuesT[1];
                                            double latitudeE = Double.parseDouble(valuesT[2]);
                                            double longitudeE = Double.parseDouble(valuesT[3]);
                                            int nif = Integer.parseInt(valuesT[4]);
                                            double raioE = Double.parseDouble(valuesT[5]);
                                            double precoPorKm = Double.parseDouble(valuesT[6]);
                                            String emailE = geraEmailTransportadora(codigoE);
                                            String passwordE = geraPasswordTransportadora(codigoE);
                                            
                                            EmpresaTransportadora e = new EmpresaTransportadora(codigoE,nomeE,latitudeE,longitudeE,nif,
                                                                                         raioE,precoPorKm,0,emailE,passwordE,new ArrayList<Proposta>(),0.0,0,0.0);
                                            listaDeTransportadoras.add(e.clone());
                                            line = new String();
                                            break;
                                            
                    case "Loja": String[] valuesL = conteudo.split(",");
                                 String codigoL = valuesL[0];
                                 String nomeL = valuesL[1];
                                 double latitudeL = Double.parseDouble(valuesL[2]);
                                 double longitudeL = Double.parseDouble(valuesL[3]);
                                 String emailL = geraEmailLoja(codigoL);
                                 String passwordL = geraPasswordLoja(codigoL);
                                          
                                 Loja l = new Loja(codigoL,nomeL,latitudeL,longitudeL,new ArrayList<Encomenda>(),0,emailL,passwordL);
                                 this.listaDeLojas.add(l.clone());
                                 line = new String();
                                 break;
                                          
                    case "Encomenda" : String[] valuesE = conteudo.split(",");
                                       String codigoEnc = valuesE[0];
                                       String codigoUser = valuesE[1];
                                       String codigoLoja = valuesE[2];
                                       double pesoE = Double.parseDouble(valuesE[3]);
                                       
                                       linhaDeEncomenda = new ArrayList<LinhaDeEncomenda>();
                                       for (int i = 4; i < valuesE.length; i+= 4){
                                           String codigoProd = valuesE[i];
                                           String descricao = valuesE[i+1];
                                           double quantidade = Double.parseDouble(valuesE[i+2]);
                                           double valorUnitario = Double.parseDouble(valuesE[i+3]);
                                           
                                           LinhaDeEncomenda li = new LinhaDeEncomenda(codigoProd,descricao,quantidade,valorUnitario,0.0,false);
                                           linhaDeEncomenda.add(li);
                                       }
                                       for (LinhaDeEncomenda le : linhaDeEncomenda){
                                           double peso = pesoE/linhaDeEncomenda.size();
                                           le.setPeso(peso);
                                       }
                                       
                                       Encomenda enc = new Encomenda(codigoEnc,codigoUser,codigoLoja,pesoE,linhaDeEncomenda,false,new ArrayList<Proposta>());
                                       gestaoDeEncomendas.add(enc.clone());
                                       line = new String();
                                       break;
                                       
                    case "Aceite" : String codigoEncA = conteudo;
                                    for (Encomenda encAceite : gestaoDeEncomendas){
                                        if (codigoEncA == encAceite.getCodigoEnc()){
                                            listaDeEncAceites.add(encAceite.clone());
                                        }
                                    }
                                    line = new String();
                                    break;
                                       
                    default : break;
                }
            }                     
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        int controlo = 0;
        do {
            showMenu();
        } while (controlo == 0);
    }
    
    private void showMenu ()
    {   System.out.println("\n-------- Menu Traz Aqui --------\n\n\n");
        System.out.println("-> Esta a utilizar a aplicacao na condicao de: ");
        System.out.println("1 - Cliente / Utilizador\n2 - Estafeta / Voluntario\n3 - Empresa Transportadora\n4 - Dono de Loja\n5 - Carregar um Programa\n0 - Sair"); 
        int opcao = lerOpcao();
        switch (opcao) {
           case 1 : System.out.println("1 - Login\n2 - Registar");
                     int opcao1 = lerOpcao();
                     switch (opcao1) {
                         case 1 : UtilizadorTrazAqui user = new UtilizadorTrazAqui();
                                  ValidaCredenciaisUtilizador utilizador = validaCredenciaisUser();
                                  boolean flag = utilizador.getFlag();
                                  if (flag == true){
                                      user = utilizador.getUser();
                                      System.out.println("Bem-Vindo de volta " + user.getNomeU());
                                  } else {
                                      System.out.println("Utilizador nao encontrado! Por favor tente novamente!");
                                      break;
                                  }
                                  showMenuUtilizador(user);
                                                                                       
                          case 2 : criaNovoUser();
                                   System.out.println("Utilizador registado com sucesso!");
                                   break;                         
                        
                         default : break;
                     }
            
           case 2 : System.out.println("1 - Login\n2 - Registar");
                    int opcao2 = lerOpcao();
                    switch (opcao2) {
                        case 1 : Voluntario voluntario = new Voluntario();
                                 ValidaCredenciaisVoluntario v = validaCredenciaisVoluntario();
                                 boolean flag = v.getFlag();
                                 if (flag == true){
                                     voluntario = v.getVoluntario();
                                     System.out.println("Bem-Vindo de volta " + voluntario.getNome());
                                 } else {
                                     System.out.println("Voluntario nao encontrado! Por favor tente novamente!");
                                     break;
                                 }
                                 showMenuVoluntario(voluntario);
                         
                        case 2 : criaNovoVoluntario();
                                 System.out.println("Voluntario registado com sucesso!");
                                 break;                         
                             
                        default : break;
                    }
                     
           case 3 : System.out.println("1 - Login\n2 - Registar");
                    int opcao3 = lerOpcao();
                    switch (opcao3) {
                        case 1 : EmpresaTransportadora empresa = new EmpresaTransportadora();
                                 ValidaCredenciaisTransportadora t = validaCredenciaisTransportadora();
                                 boolean flag = t.getFlag();
                                 if (flag == true){
                                     empresa = t.getTransportadora();
                                     System.out.println("Bem-Vindo de volta " + empresa.getNome());
                                 } else {
                                      System.out.println("Empresa Transportadora nao encontrada! Por favor tente novamente!");
                                      break;
                                 }
                                 showMenuTransportadora(empresa);
                                      
                        case 2 : criaNovaTransportadora();
                                 System.out.println("Empresa Transportadora registada com sucesso!");
                                 break;
                       
                        default : break;
                    }
            
           case 4 : System.out.println("1 - Login\n2 - Registar");
                    int opcao4 = lerOpcao();
                    switch (opcao4) {
                       case 1 : Loja loja = new Loja();
                                ValidaCredenciaisLoja l = validaCredenciaisLoja();
                                boolean flag = l.getFlag();
                                if (flag == true){
                                    loja = l.getLoja();
                                    System.out.println("Bem-Vindo de volta " + loja.getNome());
                                } else {
                                    System.out.println("Loja nao encontrada! Por favor tente novamente!");
                                    break;
                                }
                                showMenuLoja(loja);
                             
                       case 2 : criaNovaLoja();
                                System.out.println("Loja registada com sucesso!");
                                break;                         
                             
                       default : break;
                    }
                    
           case 5 : try{
                        leEmObjStream("Save");
                    } catch (IOException e){ 
                        System.out.println(e.getMessage());
                    } catch (ClassNotFoundException e){ 
                        System.out.println(e.getMessage());
                    }
                    break;
           
             
           case 0 : break;         
                     
           case -1: break;
            
           default : break;
        } 
    }
    
    private int lerOpcao()
    {   int opcao;
        Scanner is = new Scanner(System.in);
        try {
            opcao = is.nextInt();
        }
        catch (InputMismatchException e) {
            opcao = -1;
        }
        return opcao;
    }
    
    private void gravaEmObjStream (String s) throws IOException
    {   try{
            ObjectOutputStream oout = new ObjectOutputStream(new FileOutputStream(s));
            oout.writeObject(this);
            oout.flush();
            oout.close();
        } catch (IOException e){ 
            System.out.println(e.getMessage());
        }
    }
    
    private ProjetoTrazAqui leEmObjStream (String filename) throws IOException , ClassNotFoundException
    {   ProjetoTrazAqui p = new ProjetoTrazAqui();
        try{
            ObjectInputStream o = new ObjectInputStream (new FileInputStream(filename));
            p = (ProjetoTrazAqui) o.readObject();
            o.close();
        } catch (IOException e){ 
            System.out.println(e.getMessage());
        } catch (ClassNotFoundException e){ 
            System.out.println(e.getMessage());
        }
        return p;
    }
    
    private ValidaCredenciaisUtilizador validaCredenciaisUser()
    {   ValidaCredenciaisUtilizador utilizador = new ValidaCredenciaisUtilizador();
        try{
            boolean flag = false;
            UtilizadorTrazAqui user = new UtilizadorTrazAqui();
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine(); 
            
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
            
            for (UtilizadorTrazAqui u : listaDeUtilizadores){
                if (u.getEmail() == emailV && u.getPassword() == passwordV){
                    flag = true;
                    user = u;
                }
            }
            utilizador = new ValidaCredenciaisUtilizador(flag,user);
          } catch (NullPointerException e){
              System.out.println(e.getMessage());
          }
        return utilizador.clone();
    }
    
    private void criaNovoUser ()
    {   try{
            StringBuilder codigoV = new StringBuilder();
            int controlo1 = 1;
            do {
               Random gerador = new Random();
               int codigoI = gerador.nextInt();
               String codigo = String.valueOf(codigoI);
               for (UtilizadorTrazAqui u : listaDeUtilizadores){
                   if (u.getCodigoU() != codigo && u.getCodigoU() != codigo){
                       codigoV.append("u").append(codigo);
                       controlo1 = 0;
                       System.out.println("depois do if");
                   }
               }
            } while (controlo1 == 1);
        
            Scanner nome = new Scanner(System.in);
            System.out.println("Nome: ");
            String nomeV = nome.nextLine();
        
            Scanner latitude = new Scanner(System.in);
            System.out.println("Latitude: ");
            double latitudeV = latitude.nextDouble();
        
            Scanner longitude = new Scanner(System.in);
            System.out.println("Longitude: ");
            double longitudeV = longitude.nextDouble();
        
            int controlo2 = 1;
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine();
            do {
               for (UtilizadorTrazAqui u : listaDeUtilizadores){
                   if (u.getEmail() == emailV){
                       System.out.println("Email nao disponivel!\nTente novamente!");
                   } else {
                       controlo2 = 0;
                   }
               }
            } while (controlo2 == 1);
        
            int controlo3 = 1;
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
            do {
               for (UtilizadorTrazAqui u : listaDeUtilizadores){
                   if (u.getPassword() == passwordV){
                       System.out.println("Password nao disponivel!");
                   } else {
                       controlo3 = 0;
                   }
               }
            } while (controlo3 == 1);
        
            UtilizadorTrazAqui u = new UtilizadorTrazAqui(codigoV.toString(), nomeV, latitudeV, longitudeV, emailV, passwordV,0);
            listaDeUtilizadores.add(u.clone());
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }  
    }
    
    private ValidaCredenciaisVoluntario validaCredenciaisVoluntario ()
    {   ValidaCredenciaisVoluntario voluntarioV = new ValidaCredenciaisVoluntario();
        try{
            boolean flag = false;
            Voluntario voluntario = new Voluntario();
        
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine(); 
        
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
        
            for (Voluntario v : listaDeVoluntarios){
                if (v.getEmail() == emailV && v.getPassword() == passwordV){
                    flag = true;
                    voluntario = v;
                }
            } 
            voluntarioV = new ValidaCredenciaisVoluntario(flag,voluntario);
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        } 
        return voluntarioV.clone();
    }
    
    private void criaNovoVoluntario ()
    {   try{
            StringBuilder codigoV = new StringBuilder();
            int controlo1 = 1;
            do {
               Random gerador = new Random();
               int codigoI = gerador.nextInt();
               String codigo = String.valueOf(codigoI);
               for (Voluntario v : listaDeVoluntarios){
                   if (v.getCodigoV() != codigo){
                       codigoV.append("v").append(codigo);
                       controlo1 = 0;
                   }
               }
            } while (controlo1 == 1);
        
            Scanner nome = new Scanner(System.in);
            System.out.println("Nome: ");
            String nomeV = nome.nextLine();
        
            Scanner latitude = new Scanner(System.in);
            System.out.println("Latitude: ");
            double latitudeV = latitude.nextDouble();
        
            Scanner longitude = new Scanner(System.in);
            System.out.println("Longitude: ");
            double longitudeV = longitude.nextDouble();
        
            Scanner raioGeografico = new Scanner(System.in);
            System.out.println("RaioGeografico: ");
            double raioGeograficoV = raioGeografico.nextDouble();
        
            int controlo2 = 1;
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine();
            do {
               for (Voluntario v : listaDeVoluntarios){
                   if (v.getEmail() == emailV){
                       System.out.println("Email nao disponivel!\nTente novamente!");
                   } else {
                       controlo2 = 0;
                   }
               }
            } while (controlo2 == 1);
        
            int controlo3 = 1;
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
            do {
               for (Voluntario v : listaDeVoluntarios){
                   if (v.getPassword() == passwordV){
                       System.out.println("Password nao disponivel!");
                   } else {
                       controlo3 = 0;
                   }
               }
            } while (controlo3 == 1);
        
            Voluntario v = new Voluntario(codigoV.toString(), nomeV, latitudeV, longitudeV, raioGeograficoV, 0, false, emailV, passwordV,0.0,0);
            listaDeVoluntarios.add(v.clone());
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        } 
    }
    
    private ValidaCredenciaisTransportadora validaCredenciaisTransportadora ()
    {   ValidaCredenciaisTransportadora transportadora = new ValidaCredenciaisTransportadora();
        try{
            boolean flag = false;
            EmpresaTransportadora empresa = new EmpresaTransportadora();
        
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine(); 
        
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
        
            for (EmpresaTransportadora e : listaDeTransportadoras){
                if (e.getEmail() == emailV && e.getPassword() == passwordV){
                    flag = true;
                    empresa = e;
                }
            }
            transportadora = new ValidaCredenciaisTransportadora(flag,empresa);
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }
        return transportadora.clone();
    }
    
    private void criaNovaTransportadora ()
    {   try{
            StringBuilder codigoV = new StringBuilder();
            int controlo1 = 1;
            do {
               Random gerador = new Random();
               int codigoI = gerador.nextInt();
               String codigo = String.valueOf(codigoI);
               for (EmpresaTransportadora e : listaDeTransportadoras){
                   if (e.getCodigoE() != codigo){
                       codigoV.append("t").append(codigo);
                       controlo1 = 0;
                   }
               }
            } while (controlo1 == 1);
        
            Scanner nome = new Scanner(System.in);
            System.out.println("Nome: ");
            String nomeV = nome.nextLine();
        
            Scanner latitude = new Scanner(System.in);
            System.out.println("Latitude: ");
            double latitudeV = latitude.nextDouble();
        
            Scanner longitude = new Scanner(System.in);
            System.out.println("Longitude: ");
            double longitudeV = longitude.nextDouble();
        
            Scanner nif = new Scanner(System.in);
            System.out.println("NIF: ");
            int nifV = nif.nextInt();
        
            Scanner raioAcao = new Scanner(System.in);
            System.out.println("RaioAcao: ");
            double raioAcaoV = raioAcao.nextDouble();
        
            Scanner precoPorKm = new Scanner(System.in);
            System.out.println("PrecoPorKm: ");
            double precoPorKmV = precoPorKm.nextDouble();
        
            int controlo2 = 1;
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine();
            do {
               for (EmpresaTransportadora e : listaDeTransportadoras){
                   if (e.getEmail() == emailV){
                       System.out.println("Email nao disponivel!\nTente novamente!");
                   } else {
                       controlo2 = 0;
                   }
               }
            } while (controlo2 == 1);
        
            int controlo3 = 1;
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
            do {
               for (EmpresaTransportadora e : listaDeTransportadoras){
                   if (e.getPassword() == passwordV){
                       System.out.println("Password nao disponivel!");
                   } else {
                       controlo3 = 0;
                   }
               }
            } while (controlo3 == 1);
        
            EmpresaTransportadora e = new EmpresaTransportadora(codigoV.toString(), nomeV, latitudeV, longitudeV, nifV,
                                                              raioAcaoV, precoPorKmV, 0, emailV, passwordV,new ArrayList<Proposta>(),0.0,0,0.0);
            listaDeTransportadoras.add(e.clone());
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }
    }
    
    private ValidaCredenciaisLoja validaCredenciaisLoja ()
    {   ValidaCredenciaisLoja lojaL = new ValidaCredenciaisLoja();
        try{
            boolean flag = false;
            Loja loja = new Loja();
        
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine(); 
        
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
        
            for (Loja l : listaDeLojas){
                if (l.getEmail() == emailV && l.getPassword() == passwordV){
                    flag = true;
                    loja = l;
                }
            }
            lojaL = new ValidaCredenciaisLoja(flag,loja);
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }
        return lojaL.clone();
    }
    
    private void criaNovaLoja ()
    {   try{
            StringBuilder codigoV = new StringBuilder();
            int controlo1 = 1;
            do {
               Random gerador = new Random();
               int codigoI = gerador.nextInt();
               String codigo = String.valueOf(codigoI);
               for (Loja l : listaDeLojas){
                   if (l.getCodigoL() != codigo){
                       codigoV.append("l").append(codigo);
                       controlo1 = 0;
                   }
               }
            } while (controlo1 == 1);
        
            Scanner nome = new Scanner(System.in);
            System.out.println("Nome: ");
            String nomeV = nome.nextLine();
        
            Scanner latitude = new Scanner(System.in);
            System.out.println("Latitude: ");
            double latitudeV = latitude.nextDouble();
        
            Scanner longitude = new Scanner(System.in);
            System.out.println("Longitude: ");
            double longitudeV = longitude.nextDouble();
                
            int controlo2 = 1;
            Scanner email = new Scanner(System.in);
            System.out.println("Email: ");
            String emailV = email.nextLine();
            do {
               for (Loja l : listaDeLojas){
                   if (l.getEmail() == emailV){
                       System.out.println("Email nao disponivel!\nTente novamente!");
                   } else {
                       controlo2 = 0;
                   }
               }
            } while (controlo2 == 1);
        
            int controlo3 = 1;
            Scanner password = new Scanner(System.in);
            System.out.println("Password: ");
            String passwordV = password.nextLine();
            do {
               for (Loja l : listaDeLojas){
                   if (l.getPassword() == passwordV){
                       System.out.println("Password nao disponivel!");
                   } else {
                       controlo3 = 0;
                   }
               }
            } while (controlo3 == 1);
        
            Loja l = new Loja (codigoV.toString(), nomeV, latitudeV, longitudeV, new ArrayList<Encomenda>(), 0, emailV, passwordV);
            listaDeLojas.add(l.clone());
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }
    }
      
    private String geraEmailUser (String codigoU)
    {   StringBuilder email = new StringBuilder();
        email.append("utilizador").append(codigoU).append("@gmail.com");
        return email.toString();
    }
    
    private String geraPasswordUser (String codigoU)
    {   StringBuilder password = new StringBuilder();
        password.append(codigoU).append("TrazAqui");
        return password.toString();
    }
    
    private String geraEmailVoluntario (String codigoV)
    {   StringBuilder email = new StringBuilder();
        email.append("voluntario").append(codigoV).append("@gmail.com");
        return email.toString();
    }
    
    private String geraPasswordVoluntario (String codigoV)
    {   StringBuilder password = new StringBuilder();
        password.append(codigoV).append("TrazAqui");
        return password.toString();
    }
    
    private String geraEmailTransportadora (String codigoE)
    {   StringBuilder email = new StringBuilder();
        email.append("empresa").append(codigoE).append("@gmail.com");
        return email.toString();
    }
    
    private String geraPasswordTransportadora (String codigoE)
    {   StringBuilder password = new StringBuilder();
        password.append(codigoE).append("TrazAqui");
        return password.toString();
    }
    
    private String geraEmailLoja (String codigoL)
    {   StringBuilder email = new StringBuilder();
        email.append("loja").append(codigoL).append("@gmail.com");
        return email.toString();
    }
    
    private String geraPasswordLoja (String codigoL)
    {   StringBuilder password = new StringBuilder();
        password.append(codigoL).append("TrazAqui");
        return password.toString();
    }
    
    private String geraNumEncomenda ()
    {   StringBuilder num = new StringBuilder();
        Random gerador = new Random();
        int codigoI = gerador.nextInt();
        String codigo = String.valueOf(codigoI);
        num.append("e").append(codigo);
        return num.toString();
    }
    
    private void showMenuUtilizador (UtilizadorTrazAqui u)
    {   try{
            System.out.println("Que açao pretende realizar " + u.getNomeU() + " :");
            System.out.println("\n\n\n\n\n1 - Realizar uma encomenda\n2 - Solicitar a entrega de uma encomenda\n3 -  Aceder a propostas de Transporte");
            System.out.println("4 - Aceder a informacao de uma Empresa Transportadora");
            System.out.println("5 - Aceder a informacao de um Voluntario\n6 - Classificar uma Empresa Transportadora\n7 - Classificar um Voluntario");
            System.out.println("8 - Apresentar a lista de Utilizadores com mais encomendas efetudas");
            System.out.println("9 - Apresentar a lista de Empresas com mais Kms percorridos\n10 - Gravar o estado do Programa\n0 - Sair");
            int opcao = lerOpcao();
            switch (opcao) {
                case 1 : Encomendar(u);
                         break;
            
                case 2 : System.out.println("Que encomenda pretende solicitar a entrega?");
                         Scanner encomenda = new Scanner(System.in);
                         String numEnc = encomenda.nextLine();
                         solicitaEntrega(u.clone(),numEnc);
                         break;
            
                case 3 : System.out.println("Para que encomenda pretende solicitar propostas de transporte?");
                         Scanner encomenda2 = new Scanner(System.in);
                         String numEnc2 = encomenda2.nextLine();
                         solicitaPropostas(u.clone(),numEnc2);
                         System.out.println("Pretende aceitar alguma das propostas?");
                         Scanner answer = new Scanner(System.in);
                         String answerE = answer.nextLine();
                         if (answerE == "Sim"){
                             System.out.println("Qual proposta pretende aceitar?");
                             Scanner answer2 = new Scanner(System.in);
                             String answerE2 = answer2.nextLine();
                             Encomenda enc = getEncomenda(numEnc2);
                             for (Proposta p : enc.getPropostas()){
                                 if (answerE2 == p.getCodigoProposta()){
                                     p.setAceite(true);
                                 }
                             }
                             System.out.println("Aceitou a proposta com sucesso!");
                             u.atualizaNumEncs();
                         } else {
                             break;
                         }
            
                case 4 : System.out.println("A que Empresa Transportadora gostaria de ter informacao?");
                         Scanner empresa = new Scanner(System.in);
                         String empresaE = empresa.nextLine();
                         boolean controlo = false;
                         for (RegistoTransportadoras r : registoDeTransportadoras){
                             if (r.getEmpresa().getCodigoE() == empresaE){
                                 r.toString();
                                 controlo = true;
                             }
                         }
                         if (controlo == false){
                             System.out.println("A empresa nao possui qualquer registos!");
                         }
                         break;
                              
                case 5 : System.out.println("A que Voluntario gostaria de ter informacao?");
                         Scanner voluntario = new Scanner(System.in);
                         String voluntarioV = voluntario.nextLine();
                         boolean controlo2 = false;
                         for (RegistoVoluntarios r : registoDeVoluntarios){
                             if (r.getVoluntario().getCodigoV() == voluntarioV){
                                 r.toString();
                                 controlo2 = true;
                             }
                         }
                         if (controlo2 == false){
                             System.out.println("O Voluntario nao possui qualquer registos!");
                         }
                         break;
            
                case 6 : System.out.println("Que Empresa pretende classificar ?");
                         Scanner empresa2 = new Scanner(System.in);
                         String empresaE2 = empresa2.nextLine();
                         for (EmpresaTransportadora e : listaDeTransportadoras){
                             if (e.getCodigoE() == empresaE2){
                                 System.out.println("Que classificacao pretende atribuir a esta empresa? De 1 a 10");
                                 Scanner nota = new Scanner(System.in);
                                 String notaC = nota.nextLine();
                                 int notaE = Integer.parseInt(notaC);
                                 if (notaE >= 1 && notaE <= 10){
                                     e.classifica(notaE);
                                     System.out.println("Empresa classificada com sucesso!");
                                 } else {
                                     System.out.println("Tem de atribuir uma classificacao de 1 a 10!");
                                 }
                             }
                         }
                         break;
            
                case 7 : System.out.println("Que Voluntario pretende classificar ?");
                         Scanner voluntario2 = new Scanner(System.in);
                         String voluntarioV2 = voluntario2.nextLine();
                         for (Voluntario v : listaDeVoluntarios){
                             if (v.getCodigoV() == voluntarioV2){
                                 System.out.println("Que classificacao pretende atribuir a este voluntario? De 1 a 10");
                                 Scanner nota = new Scanner(System.in);
                                 String notaC = nota.nextLine();
                                 int notaE = Integer.parseInt(notaC);
                                 if (notaE >= 1 && notaE <= 10){
                                     v.classifica(notaE);
                                     System.out.println("Voluntario classificado com sucesso!");
                                 } else {
                                     System.out.println("Tem de atribuir uma classificacao de 1 a 10!");
                                 }
                             }
                         }
                         break;
                     
                case 8 : System.out.println("Apresenta-se a lista de utilizadores com mais encomendas efetuadas!");
                         ArrayList<UtilizadorTrazAqui> ordem = utilizadorOrdemCrescenteNumEncs();
                         for (UtilizadorTrazAqui ump : ordem){
                             System.out.println(ump.getNomeU());
                         }
                         break;
                     
                case 9 : System.out.println("Apresenta-se a lista de empresas com mais kms percorridos!");
                         ArrayList<EmpresaTransportadora> ordem2 = empresaOrdemCrescenteKmsPercorridos();
                         for (EmpresaTransportadora emp : ordem2){
                             System.out.println(emp.getNome());
                         }
                         break;
                         
                case 10 : try{
                              gravaEmObjStream("Save");
                          } catch (IOException e){ 
                              System.out.println(e.getMessage());
                          }
                          break;
                                
                case 0 : break;
            
                case -1 : System.out.println("Escolha uma das opcoes disponiveis!");
                          break;
            
                default : break;
            }
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }
    }
    
    private void Encomendar (UtilizadorTrazAqui u)
    {   try{
            String nomeProdutoV = new String();
            String codigoProdutoV = new String();
            double pesoV = 0.0;
            double pesoGeral = 0.0;
            double quantidadeV = 0.0;
            boolean encMedica = false;
            boolean encMedicaGeral = false;
            System.out.println("Aqui esta a nossa lista de Produtos disponiveis para encomenda!");
            for (CatalogoDeProdutos c : catalogoDeProdutos){
                c.toString();
            }
            System.out.println("Quantos produtos deseja encomendar ?");
            int num = lerOpcao();
            ArrayList<LinhaDeEncomenda> listaDeProdutos = new ArrayList<LinhaDeEncomenda>();
            for (int i = 0; i < num; i++){
                Scanner produto = new Scanner(System.in);
                System.out.println("Que produto deseja adicionar a sua encomenda?");
                boolean controlo1 = false;
                do {
                   nomeProdutoV = produto.nextLine();
                   for (CatalogoDeProdutos c : catalogoDeProdutos){
                       if (nomeProdutoV == c.getNomeProduto()){
                           codigoProdutoV = c.getCodigoProduto();
                           pesoV = c.getPeso();
                           pesoGeral = pesoGeral + c.getPeso();
                           encMedica = c.getEncomendaMedica();
                           controlo1 = true;
                       } else {
                           System.out.println("Produto Indisponivel!");
                       }
                   }
                } while (controlo1 == false);
                Scanner quantidade = new Scanner(System.in);
                System.out.println("Quantas unidades deseja comprar?");
                quantidadeV = quantidade.nextDouble();
                LinhaDeEncomenda encomenda = new LinhaDeEncomenda(codigoProdutoV,nomeProdutoV,quantidadeV,1.0,pesoV,encMedica);
                listaDeProdutos.add(encomenda.clone());
            }
            for (LinhaDeEncomenda l : listaDeProdutos){
                if (l.getEncomendaMedica() == true){
                    encMedicaGeral = true;
                }
            }
            System.out.println("A que loja pretende efetuar esta encomenda?");
            for (Loja l : listaDeLojas){
                l.toString();
            }
            Scanner loja = new Scanner(System.in);
            String lojaE = loja.nextLine();
            String numEnc = geraNumEncomenda();
            Encomenda e = new Encomenda(numEnc,u.getCodigoU(),lojaE,pesoGeral,listaDeProdutos,encMedicaGeral,new ArrayList<Proposta>());
            gestaoDeEncomendas.add(e.clone());
            System.out.println("A sua encomenda foi registada com sucesso!");
            System.out.println("O codigo da sua encomenda e:" + numEnc);
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }
    }
    
    private void solicitaEntrega (UtilizadorTrazAqui u, String e)
    {   try{
            Encomenda enc = new Encomenda();
            for (Encomenda eG : gestaoDeEncomendas){
                if (e == eG.getCodigoEnc() && u.getCodigoU() == eG.getCodigoUser()){
                    enc = eG;
                }
            }
            if (enc == new Encomenda()){
                System.out.println("Nao possuimos essa encomenda no nosso sistema! Por favor tente outra vez.");
            } else {
                listaDeEncAceites.add(enc);
                for (Loja l : listaDeLojas){
                    if (enc.getCodigoLoja() == l.getCodigoL()){
                        l.getPedidos().add(enc.clone());
                    }
                }
                System.out.println("Encomenda solicitada com sucesso!");
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
    }
    
    private void solicitaPropostas (UtilizadorTrazAqui u, String enc)
    {   try{
            Loja l = new Loja();
            Encomenda encomenda = new Encomenda();
            l = getLoja(getEncomenda(enc).getCodigoLoja());
            for (Encomenda e : l.getPedidos()){
                if (e.equals(enc)){
                    e.getPropostas().toString();
                }
            }
        } catch (NullPointerException e){ 
            System.out.println(e.getMessage());
        }
    }
    
    private  ArrayList<UtilizadorTrazAqui> utilizadorOrdemCrescenteNumEncs()
    {   ArrayList<UtilizadorTrazAqui> e = new ArrayList<UtilizadorTrazAqui>();
        try{
            TreeSet<UtilizadorTrazAqui> s = new TreeSet<UtilizadorTrazAqui>(new ComparatorUtilizadorNumEncs());
            for (UtilizadorTrazAqui u : listaDeUtilizadores){
                s.add(u.clone());
            }
            for (UtilizadorTrazAqui ump : s){
                e.add(ump.clone());
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
        return e;
    }
    
    private void showMenuVoluntario (Voluntario v)
    {   try{
            System.out.println("Que açao pretende realizar " + v.getNome() + " :");
            System.out.println("\n\n\n\n\n1 - Sinalizar que estou disponivel para transportar uma encomenda\n2 - Escolher uma encomenda, efetuar o transporte e registar o tempo\n");
            System.out.println("3 - Verificar o meu registo de transporte de encomendas\n4 - Gravar o estado do Programa\n0 - Sair");
            int opcao = lerOpcao();
            switch (opcao) {
                case 1 : v.setEstado(0);
                         System.out.println("Esta agora disponivel para transportar uma encomenda.");
                         Scanner resposta = new Scanner(System.in);
                         System.out.println("Pretende estar disponivel para transportar encomendas medicas?");
                         String answer = resposta.nextLine();
                         if (answer == "Sim"){
                             v.aceitaMedicamentos(true);
                         } else {
                             v.aceitaMedicamentos(false);
                         }
                         break;
                      
                case 2 : Encomenda enc = new Encomenda();
                         enc = irBuscarEncomenda(v.clone());
                         if (enc.equals(new Encomenda())){
                             System.out.println("Nao ha nenhuma encomenda disponivel para transportar neste momento!");
                             break;
                         }
                         System.out.println("Foi-lhe atribuido o transporte da encomenda " + enc.getCodigoEnc());
                         enc.toString();
                         v.setEstado(1);
                     
                         double distancia1 = distancia(v.getLatitude(),v.getLongitude(),getLoja(enc.getCodigoLoja()).getLatitude(),getLoja(enc.getCodigoLoja()).getLongitude());
                         double distancia2 = distancia(getLoja(enc.getCodigoLoja()).getLatitude(),getLoja(enc.getCodigoLoja()).getLongitude(),
                                                       getUtilizador(enc.getCodigoUser()).getLatitude(),getUtilizador(enc.getCodigoUser()).getLongitude());
                         double distancia = distancia1 + distancia2;                              
                         double tempo = tempoDeViagemVoluntario(distancia);                              
                         transporteDeEncomendaVoluntario(v.clone(),tempo,enc);
                         System.out.println("Encomenda entregue com sucesso!");
                         break;
            
                case 3 : System.out.println ("----- Voluntario " + v.getNome() + " :");
                         for (RegistoVoluntarios r : registoDeVoluntarios){
                             if (r.getVoluntario() == v){
                                 r.toString();
                             }
                         }
                         break;
                         
                case 4 : try{
                              gravaEmObjStream("Save");
                         } catch (IOException e){ 
                              System.out.println(e.getMessage());
                         }
                         break;
                             
                case 0 : break;
            
                case -1 : System.out.println("Escolha uma das opcoes disponiveis!");
                          break;
            
                default : break;
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
    }
    
    private Encomenda irBuscarEncomenda (Voluntario v)
    {   Encomenda enc = new Encomenda();
        try{
            for (Loja l : listaDeLojas){
                double distancia = distancia(v.getLatitude(),v.getLongitude(),l.getLatitude(),l.getLongitude());
                if (distancia < v.getRaioGeografico()){
                    ArrayList<Encomenda> pedidos = l.getPedidos();
                    for (Encomenda e : pedidos){
                        UtilizadorTrazAqui user = getUtilizador(e.getCodigoUser());
                        double distancia2 = distancia(user.getLatitude(),user.getLongitude(),l.getLatitude(),l.getLongitude());
                        if (distancia2 < v.getRaioGeografico() && enc.getEncomendaMedica() == v.aceitoTransporteMedicamentos()){
                            enc = e;
                            l.getPedidos().remove(e);
                        }
                    }
                }
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
        return enc;
    }
    
    private void transporteDeEncomendaVoluntario (Voluntario v,double tempo, Encomenda enc)
    {   LocalDateTime instanteI = LocalDateTime.now();
        double time = tempo;
        Encomenda e = enc;
        registoDeVoluntarios.add(new RegistoVoluntarios(v,instanteI,time,e).clone());
    }
    
    private double tempoDeViagemVoluntario (double distancia)
    {   Random random1 = new Random();
        Random random2 = new Random();
        Random random3 = new Random();
        double condicoesDoPiso = 1.4*(1.0 + random1.nextDouble());
        double condicoesAtmosfericas = 1.4*(1.0 + random2.nextDouble());
        double condicoesDoTransito = 1.8*(1.0 + random3.nextDouble());
        double tempo = (2.5 * distancia) * (condicoesDoPiso + condicoesAtmosfericas + condicoesDoTransito);
        return tempo;
    }
    
    private UtilizadorTrazAqui getUtilizador (String codigoUser)
    {   UtilizadorTrazAqui user = new UtilizadorTrazAqui();
        try{
            for (UtilizadorTrazAqui u : listaDeUtilizadores){
                if (u.getCodigoU() == codigoUser){
                    user = u;
                }
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
        return user;
    }
    
    private Loja getLoja (String codigoLoja)
    {   Loja loja = new Loja();
        try{
            for (Loja l : this.listaDeLojas){
                if (l.getCodigoL() == codigoLoja){
                    loja = l;
                }
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
        return loja;
    }
    
    private Encomenda getEncomenda (String codigoEnc)
    {   Encomenda enc = new Encomenda();
        try{
            for (Encomenda e : gestaoDeEncomendas){
                if (e.getCodigoEnc() == codigoEnc){
                    enc = e;
                }
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
        return enc;
    }
    
    private double distancia (double x1, double y1, double x2, double y2)
    {   double distancia = 0.0;
        distancia = Math.sqrt(Math.pow(x2-x1,2) + Math.pow(y2-y1,2));
        return distancia;
    }
    
    private void showMenuTransportadora(EmpresaTransportadora e)
    {   try{
            System.out.println("Que açao pretende realizar " + e.getNome() + " :");
            System.out.println("\n\n\n\n\n1 - Sinalizar que estamos disponiveis para transportar uma encomenda\n2 - Determinar os precos de transporte e realizar as propostas\n");
            System.out.println("3 - Realizar o transporte da encomenda e registar o custo e o tempo\n4 - Mostrar registo da Empresa");
            System.out.println("5 - Apresentar a lista de Empresas que percorreram mais Kms\n6 - Gravar o estado do Programa\n0 -Sair");
            int opcao = lerOpcao();
            switch (opcao) {
                case 1 : e.setEstado(0);
                         System.out.println("Estao agora disponiveis para transportar uma encomenda.");
                         break;
            
                case 2 : for (Loja l : listaDeLojas){
                             double distancia1 = distancia(l.getLatitude(),l.getLongitude(),e.getLatitude(),e.getLongitude());
                             if (distancia1 < e.getRaioAcao()){
                                 for (Encomenda enc : l.getPedidos()){
                                     double distancia2 = distancia(l.getLatitude(),l.getLongitude(),
                                     getUtilizador(enc.getCodigoUser()).getLatitude(),getUtilizador(enc.getCodigoUser()).getLongitude());
                                     if (distancia2 < e.getRaioAcao() && enc.getEncomendaMedica() == false){
                                         double distancia = distancia1 + distancia2; 
                                         double preco = getPrecoTransporte(enc.clone(),e.clone());
                                         double tempo = tempoDeViagemEmpresa(distancia);
                                         StringBuilder codigo = new StringBuilder();
                                         Random gerador = new Random();
                                         int codigoI = gerador.nextInt();
                                         String codigoP = String.valueOf(codigoI);
                                         codigo.append("proposta").append(codigoP);
                                         enc.getPropostas().add(new Proposta(codigo.toString(),e,preco,tempo,distancia,false,enc).clone());
                                         e.getListaDePropostas().add(new Proposta(codigo.toString(),e,preco,tempo,distancia,false,enc).clone());
                                     }
                                 }
                             }
                         }
                         System.out.println("Os precos das encomendas foram calculados e propostos com sucesso!");
                         break;
                
                case 3: if (e.getEstado() == 0){
                            for (Proposta p : e.getListaDePropostas()){
                                if (p.getAceite() == true){
                                    e.setEstado(1);
                                    transporteDeEncomendaEmpresa(e,p.getTempo(),p.getPreco(),p.getEncomenda());
                                    e.atualizaKms(getDistancia(p.getEncomenda(),e.clone()));
                                    System.out.println("Encomenda entregue com sucesso");
                                    break;
                                } else {
                                    System.out.println("Nao tem encomendas aceites no momento! Por favor tente mais tarde!");
                                    break;
                                }
                            }
                        } else {
                            System.out.println("Nao estao preparados para transportar outra encomenda!");
                            break;
                        }
            
                case 4:  double faturacao = 0.0;
                         System.out.println ("----- Empresa Transportadora " + e.getNome() + " :");
                         for (RegistoTransportadoras r : registoDeTransportadoras){
                             if (r.getEmpresa() == e){
                                 r.toString();
                                 faturacao = faturacao + r.getPreco();
                             }
                         }
                         System.out.println("Faturacao total da empresa: " + faturacao);
                         break;
                     
                case 5:  System.out.println("Apresenta-se a lista de empresas com mais kms percorridos!");
                         ArrayList<EmpresaTransportadora> ordem = empresaOrdemCrescenteKmsPercorridos();
                         for (EmpresaTransportadora emp : ordem){
                             System.out.println(emp.getNome());
                         }
                         break;
                         
                case 6 : try{
                              gravaEmObjStream("Save");
                         } catch (IOException exception){ 
                              System.out.println(exception.getMessage());
                         }
                         break;
            
                case 0: break;
            
                case -1 : System.out.println("Escolha uma das opcoes disponiveis!");
                          break;
            
                default : break;
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
    }
    
    private  ArrayList<EmpresaTransportadora> empresaOrdemCrescenteKmsPercorridos()
    {   ArrayList<EmpresaTransportadora> empresa = new ArrayList<EmpresaTransportadora>();
        try{
            TreeSet<EmpresaTransportadora> s = new TreeSet<EmpresaTransportadora>(new ComparatorEmpresaKms());
            for (EmpresaTransportadora e : listaDeTransportadoras){
                s.add(e.clone());
            }
            for (EmpresaTransportadora emp : s){
                empresa.add(emp.clone());
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
        return empresa;
    }
    
    private double getDistancia (Encomenda enc, EmpresaTransportadora empresa)
    {   double latitudeLoja = getLoja(enc.getCodigoLoja()).getLatitude();
        double longitudeLoja = getLoja(enc.getCodigoLoja()).getLongitude();
        double distancia1 = distancia(latitudeLoja,longitudeLoja,empresa.getLatitude(),empresa.getLongitude());
        double distancia2 = distancia(latitudeLoja,longitudeLoja,getUtilizador(enc.getCodigoUser()).getLatitude(),getUtilizador(enc.getCodigoUser()).getLongitude());
        double distancia = distancia1 + distancia2;
        return distancia;
    }
    
    private double getPrecoTransporte (Encomenda enc, EmpresaTransportadora empresa)
    {   double latitudeLoja = getLoja(enc.getCodigoLoja()).getLatitude();
        double longitudeLoja = getLoja(enc.getCodigoLoja()).getLongitude();
        double distancia1 = distancia(latitudeLoja,longitudeLoja,empresa.getLatitude(),empresa.getLongitude());
        double distancia2 = distancia(latitudeLoja,longitudeLoja,getUtilizador(enc.getCodigoUser()).getLatitude(),getUtilizador(enc.getCodigoUser()).getLongitude());
        double distancia = distancia1 + distancia2;
        double preco = distancia * empresa.getPrecoPorKm();
        return preco;
    }
    
    private double tempoDeViagemEmpresa (double distancia)
    {   Random random1 = new Random();
        Random random2 = new Random();
        Random random3 = new Random();
        double condicoesDoPiso = 1.2*(1.0 + random1.nextDouble());
        double condicoesAtmosfericas = 1.2*(1.0 + random2.nextDouble());
        double condicoesDoTransito = 1.4*(1.0 + random3.nextDouble());
        double tempo = (1.5 * distancia) * (condicoesDoPiso + condicoesAtmosfericas + condicoesDoTransito);
        return tempo;
    }
    
    private void transporteDeEncomendaEmpresa (EmpresaTransportadora e,double tempo, double preco, Encomenda enc)
    {   LocalDateTime instanteI = LocalDateTime.now();
        double time = tempo;
        double custo = preco;
        Encomenda encomenda = enc;
        registoDeTransportadoras.add(new RegistoTransportadoras(e,instanteI,time,custo,encomenda).clone());
    }
      
    private void showMenuLoja (Loja l)
    {   System.out.println("Que açao pretende realizar " + l.getNome() + " :");
        System.out.println("\n\n\n\n\n1 - Sinalizar que existe uma encomenda a ser entregue, apresentando as suas informacoes\n2 - Indicar a quantidade de encomendas em fila de espera");
        System.out.println("3 - Gravar o estado do Programa\n0 - Sair");
        int opcao = lerOpcao();
        switch (opcao) {
            case 1 : sinalizaEntrega(l);
                     break;
            
            case 2 : int pedidosEmEspera = l.getPedidosEmEspera();
                     System.out.println("Temos " + pedidosEmEspera + "pedidos em espera");
                     break;
                     
            case 3 : try{
                         gravaEmObjStream("Save");
                     } catch (IOException e){ 
                         System.out.println(e.getMessage());
                     }
                     break;
            
            case 0 : break;
            
            case -1 : System.out.println("Escolha uma das opcoes disponiveis!");
                      break;
            
            default : break;
        }
    }
    
    private void sinalizaEntrega (Loja l)
    {   try{
            for (Encomenda e : l.getPedidos()){
                 System.out.println("O pedido do " + e.getCodigoUser() + "esta pronto para ser entregue");
                 e.toString();
            }
        } catch (NullPointerException exception){ 
            System.out.println(exception.getMessage());
        }
    }
}