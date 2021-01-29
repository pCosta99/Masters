import java.util.Date;
import java.io.*;
import java.util.Scanner;
import java.util.Map;
import java.util.HashMap;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.ArrayList;
import java.net.SecureCacheResponse;
/**
 * Write a description of class TrazAqui here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */
public class TrazAqui
{
    private ConjUtilizadores utilizadores;
    private ConjVoluntarios voluntarios;
    private ConjLojas lojas;
    private ConjEncomendas encomendas;
    private ConjTransportadores transportadores;
    
    private Menu menuPrincipal, menuLogin, menuRegistar, menuUtilizador, menuVoluntario, menuTransportadora, menuLoja;
    
    /**
     * O método main cria a aplicação e invoca o método run()
     */
    public static void main(String[] args){
        new TrazAqui().run();
    }
    
    /**
     * Construtor.
     * 
     * Cria os menus e a camada de negócio.
     */
    
    private TrazAqui() {
        String[] opcoes = {"Login",
                           "Registar"};
                           
        String[] opcoesLogin = {"Login do Utilizador",
                                "Login do Transportadora",
                                "Login do Voluntario",
                                "Login da Loja"};
                                 
        String[] opcoesRegistar = {"Registar como Utilizador",
                                   "Registar como Transportadora",
                                   "Registar como Voluntario",
                                   "Registar como Loja"};
                                 
        String[] opcoesUtilizador = {"Lista de encomendas",
                                     "Consultar encomendas",
                                     "Fazer pedido de encomenda",
                                     "Classificar um Voluntario",
                                     "Classificar a Transportadora"};
        
        String[] opcoesVoluntario = {"Aceitar a encomenda",
                                     "Recolher a encomenda na loja",
                                     "Registar o tempo do transporte"};                          
                                  
        String[] opcoesTransportadora = {"Aceitar a encomenda",
                                         "Calcular preço de transporte",
                                         "Registar o tempo do transporte",
                                         "Registar custo do transporte"};
              
        String[] opcoesLoja ={"Sinalizar a encomenda",
                              "Identificar fila de espara"        
                               };
                                       
                                       
        this.menuPrincipal = new Menu(opcoes);        
        this.menuLogin = new Menu(opcoesLogin);
        this.menuRegistar = new Menu(opcoesRegistar);
        this.menuUtilizador = new Menu(opcoesUtilizador);
        this.menuVoluntario = new Menu(opcoesVoluntario);
        this.menuTransportadora = new Menu(opcoesTransportadora);
        this.menuLoja = new Menu(opcoesLoja);
        
        
        try {
            this.utilizadores =new ConjUtilizadores();
        }
        catch (IOException e) {
            System.out.println("Erro de leitura nos utilizadores!");     
            }
        
        try {
            this.transportadores = new ConjTransportadores();
        }
        catch (IOException e) {
            System.out.println("Erro de leitura nos transportadores!");    
            
        }
        
        try {
            this.lojas = new ConjLojas();
        }
        catch (IOException e) {
            System.out.println("Erro de leitura nas lojas!");     
            
        }
        
        try {
            this.voluntarios = new ConjVoluntarios();
        }
        catch (IOException e) {
            System.out.println("Erro de leitura nos voluntarios!");     
            
        }
        try {
            this.encomendas = new ConjEncomendas();
        }
        catch (IOException e) {
            System.out.println("Erro de leitura nas encomendas!");     
            
        }
    }

    /**
     * Executa o menu principal e invoca o método correspondente à opção seleccionada.
     */
    private void run(){
        
        Localizacao lc = new Localizacao(12,23);
        Localizacao lc2 = new Localizacao(59,61);
        Localizacao lv = new Localizacao(20,32);
        Localizacao lv2 = new Localizacao(51,62);
        Localizacao lv3 = new Localizacao(20,47);
        
       
        
        
        Scanner sc = new Scanner(System.in);
        do {
            menuPrincipal.executa();
            switch (menuPrincipal.getOpcao()) {
                case 1: do{
                            System.out.println("\n\n----LOGIN----");
                            menuLogin.executa();
                            switch (menuLogin.getOpcao()){
                                case 1: System.out.println("Insira o seu email:");
                                        String email = sc.next();
                                        System.out.println("Insira a sua password:");
                                        String pass = sc.next();
                                        if(this.utilizadores.validaDados(email,pass)){
                                            System.out.println("Login bem sucedido!!");
                                            String codUtilizadorLogado = this.utilizadores.getUtilizadorByEmail(email);
                                            do{
                                                menuUtilizador.executa();                                            
                                                switch (menuUtilizador.getOpcao()){
                                                    case 1: 
                                                            break;
                                                    case 2: System.out.println("Insira o código da encomenda:");
                                                            String cod = sc.next();
                                                            System.out.println("Aqui está a sua encomenda:\n");
                                                            System.out.println(this.encomendas.buscarEncomenda(cod));
                                                            break;
                                                    case 3: System.out.println("Escolha a loja onde comprar");
                                                            List<Loja> lojasa = this.lojas.getListaLojas();
                                                            for ( Loja loja : lojasa) {
                                                                System.out.println( loja.getCodLoja()+ " - " + loja.getNomeLoja());
                                                            }
                                                            String codLoja = sc.next();
    							boolean lojaFound = false;
    							for (Loja loja : this.lojas.ler()) {
    								if(loja.getCodLoja().equals(codLoja)) {
    									lojaFound = true;
    								}
    							}
                                                        if(lojaFound) {
                                                            List<LinhaEncomenda> linhasEncomendas = new ArrayList<>();
                                                            boolean produtosEnd = false;
                                                            while (produtosEnd == false) {
                                                                System.out.println("Insira o produto:");
                                                                String produto = sc.next();
                                                                System.out.println("Insira a quantidade:");
                                                                Double quantidade = sc.nextDouble();
                                                                LinhaEncomenda le = new LinhaEncomenda(produto,produto,quantidade,10.0);
                                                                linhasEncomendas.add(le);
                                                                System.out.println("Selecione 1 para inserir mais produtos 0 para terminar");
                                                                if(sc.nextInt() == 0) {
                                                                    produtosEnd = true;
                                                                }
                                                            }
                                                            try{
                                                                    this.encomendas.addDados(this.utilizadores.geraCodigo("e"),codUtilizadorLogado,codLoja,10.0,linhasEncomendas);
                                                            } catch (IOException e) {
                                                                    System.out.println("Erro ao criar encomenda");     
                                                            }
                                                        } else {
                                                            System.out.println("Loja não encontrada");
                                                        }
                                                            break;  
                                                     case 4:
                                                        System.out.println("Insira a avalição que pretende dar:");
                                                        double a = sc.nextDouble();
                                                        if(voluntarios.aceitaEncV(voluntarios.getLV(),utilizadores.getUtilizadorBycod(utilizadores.getUtilizadorByEmail(email)),encomendas.getEncomendaByco(encomendas.getEncomendabyCodU(utilizadores.getUtilizadorByEmail(email))),lojas.getLL()) != null) voluntarios.AvaliaV(voluntarios.aceitaEncV(voluntarios.getLV(),utilizadores.getUtilizadorBycod(utilizadores.getUtilizadorByEmail(email)),encomendas.getEncomendaByco(encomendas.getEncomendabyCodU(utilizadores.getUtilizadorByEmail(email))),lojas.getLL()),a);
                                                        else transportadores.AvaliaT(transportadores.aceitaEncT(transportadores.getLT(),utilizadores.getUtilizadorBycod(utilizadores.getUtilizadorByEmail(email)),encomendas.getEncomendaByco(encomendas.getEncomendabyCodU(utilizadores.getUtilizadorByEmail(email))),lojas.getLL()),a);
                                                        
                                                }
                                            }while (menuUtilizador.getOpcao()!=0);
                                        } 
                                        break;
                                case 2:System.out.println("Insira o seu email:");
                                         email = sc.next();
                                        System.out.println("Insira a sua password:");
                                         pass = sc.next();
                                        if(this.transportadores.validaDados(email,pass)){
                                            System.out.println("Login bem sucedido!!");
                                            do{
                                                menuTransportadora.executa();
                                                switch (menuTransportadora.getOpcao()){
                                                    case 1:
                                                }
                                            }while (menuTransportadora.getOpcao()!=0);
                                        }
                                        break;
                                 case 3:System.out.println("Insira o seu email:");
                                         email = sc.next();
                                        System.out.println("Insira a sua password:");
                                         pass = sc.next();
                                        if(this.voluntarios.validaDados(email,pass)){
                                            System.out.println("Login bem sucedido!!");
                                            do{
                                                menuVoluntario.executa();
                                                switch (menuVoluntario.getOpcao()){
                                                    case 1:
                                                }
                                            }while (menuVoluntario.getOpcao()!=0);
                                        }
                                        break;
                                case 4: System.out.println("Insira o seu email:");
                                         email = sc.next();
                                        System.out.println("Insira a sua password:");
                                         pass = sc.next();
                                        if(this.lojas.validaDados(email,pass)){
                                            System.out.println("Login bem sucedido!!");
                                            do{
                                                menuLoja.executa();
                                                switch (menuLoja.getOpcao()){
                                                    case 1:
                                                }
                                            }while (menuLoja.getOpcao()!=0);
                                        } 
                                        break;
                            }
                        }while (menuLogin.getOpcao()!=0);
                            break;
                 case 2: do{
                            System.out.println("\n\n----REGISTAR----");
                            menuRegistar.executa();
                            switch (menuRegistar.getOpcao()){
                                case 1: System.out.println("Insira o seu email:");
                                        String email = sc.next();
                                        System.out.println("Insira a sua password:");
                                        String pass = sc.next();
                                        System.out.println("Insira o seu nome:");
                                        String name = sc.next(); 
                                        System.out.println("Insira a Localização(x,y):");
                                        Double x= sc.nextDouble();
                                        Double y = sc.nextDouble();
                                try{this.utilizadores.addDados(this.utilizadores.geraCodigo("u"),
                                                                name,  x,  y, email ,  pass);
                                }
                                catch (IOException e) {
                                    System.out.println("Erro de leitura nos utilizadores!");     
                                }
                                break;
                                case 2:System.out.println("Insira o seu email:");
                                         email = sc.next();
                                        System.out.println("Insira a sua password:");
                                         pass = sc.next();
                                        System.out.println("Insira o seu nome:");
                                         name = sc.next(); 
                                        System.out.println("Insira a Localização(x,y):");
                                         x= sc.nextDouble();
                                         y = sc.nextDouble();
                                        System.out.println("Insira raio:");
                                        Double r= sc.nextDouble();
                                        System.out.println("Insira NIF:");
                                        int nif= sc.nextInt();
                                        System.out.println("Insira preço por km:");
                                        Double p= sc.nextDouble();
                                try{this.transportadores.addDados(this.transportadores.geraCodigo("t"), 
                                                                  name,  x,  y, r, p, nif, email ,  pass);
                                }
                                catch (IOException e) {
                                    System.out.println("Erro de leitura nas transportadoras!");     
                                }
                                break;
                                case 3:System.out.println("Insira o seu email:");
                                         email = sc.next();
                                        System.out.println("Insira a sua password:");
                                         pass = sc.next();
                                        System.out.println("Insira o seu nome:");
                                         name = sc.next(); 
                                        System.out.println("Insira a Localização(x,y):");
                                         x= sc.nextDouble();
                                         y = sc.nextDouble();
                                        System.out.println("Insira raio:");
                                         r= sc.nextDouble();                                        
                                try{this.voluntarios.addDados(this.voluntarios.geraCodigo("v"),
                                                               name,  x,  y, r, email ,  pass);
                                }
                                catch (IOException e) {
                                    System.out.println("Erro de leitura nvoluntarios!");     
                                }
                                break;
                                case 4:System.out.println("Insira o seu email:");
                                         email = sc.next();
                                        System.out.println("Insira a sua password:");
                                         pass = sc.next();
                                        System.out.println("Insira o seu nome:");
                                         name = sc.next(); 
                                        System.out.println("Insira a Localização(x,y):");
                                         x= sc.nextDouble();
                                         y = sc.nextDouble();                                                                               
                                try{this.lojas.addDados(this.lojas.geraCodigo("l"),  name,  x,  y, email ,  pass);
                                }
                                catch (IOException e) {
                                    System.out.println("Erro de leitura nas lojas!");     
                                }
                                break;
                             }
                            }while(menuRegistar.getOpcao()!=0);
                        break;
                    }
                }while (menuLogin.getOpcao()!=0);
            }
        }

