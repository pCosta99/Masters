/**
 * Escreva a descrição da classe Main aqui.
 * 
 * @author Anabela Pereira - A87990, Fernando Lobo - A87988, Márcia Cerqueira - A87992; 
 * @version 20200611
 */
import java.util.Scanner;
import java.io.IOException;
import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;
import java.lang.reflect.Array;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.concurrent.Callable;
import java.time.LocalDateTime;

public class TrazAqui implements Serializable
{
    private Estado estado = new Estado();
    private int opcao_0;
    private Scanner sc = new Scanner(System.in);
    
    /**
     * Construtores
     */
    public void menu(){
        System.out.println("____________________________________________________________________________________________");
        System.out.println("                                       Menu                                                 ");
        System.out.println("____________________________________________________________________________________________");
        System.out.println("1. Login                                                                                    ");
        System.out.println("2. Registar                                                                                 ");
        System.out.println("3. Carregar logs                                                                            ");
        System.out.println("4. Top 10 Clientes                                                                          ");
        System.out.println("5. Top 10 Empresas                                                                          ");
        System.out.println("6. Gravar estado                                                                            ");
        System.out.println("7. Recuperar estado                                                                         ");
        System.out.println("8. Sair                                                                                     ");
        System.out.println("____________________________________________________________________________________________");
    }
    
    public void registar(){
        System.out.println("Registar como:");
        System.out.println("1. Cliente");
        System.out.println("2. Loja");
        System.out.println("3. Empresa");
        System.out.println("4. Voluntário");
        System.out.println("5. Sair");
        
        int escolha = sc.nextInt();
        
        
        switch(escolha){
            
            case 1:
                try{
                    System.out.println("Código de cliente(por exemplo, \"u42\"): ");
                
                    String codC = sc.next();
                
                    if(codC.charAt(0) != 'u'){
                        System.out.println("Utilizador inválido!");
                        break;
                    }
                
                    System.out.println("Password: ");
                
                    String pass = sc.next();
                
                    System.out.println("Nome: ");
                
                    String nome = sc.next();
                
                
                    System.out.println("Coordenadas: ");
                    System.out.println("Latitude: ");
                    double latitude = sc.nextDouble();
                    System.out.println("Longitude: ");
                    double longitude = sc.nextDouble();
                    
                    this.estado.registaCliente(0, codC, nome, latitude, longitude, 0, pass);
                    System.out.println("Registado com sucesso!");
                
                }
                catch(InputMismatchException e){
                    System.out.println(e.getMessage());
                }
                
                
                break;
                
            case 2:
                try{
                    System.out.println("Código da loja(por exemplo, \"l42\"): ");
                
                    String codL = sc.next();
                    
                    if(codL.charAt(0) != 'l'){
                        System.out.println("Utilizador inválido!");
                        break;
                    }
                    
                    System.out.println("Password: ");
                
                    String pass1 = sc.next();
                
                    System.out.println("Nome da loja: ");
                
                    String nomeL = sc.next();
                
                    System.out.println("Coordenadas: ");
                    System.out.println("Latitude: ");
                    double latitude1 = sc.nextDouble();
                    System.out.println("Longitude: ");
                    double longitude1 = sc.nextDouble();
                
                    this.estado.registaLoja(codL, nomeL, latitude1, longitude1, pass1);
                
                    System.out.println("Registado com sucesso!");
                }
                catch(InputMismatchException e){
                    System.out.println(e.getMessage());
                }
                
                break;
            
            case 3:
                try{
                    System.out.println("Código da empresa(por exemplo, \"t42\"): ");
                
                    String codE = sc.next();
                    
                    if(codE.charAt(0) != 't'){
                        System.out.println("Utilizador inválido!");
                        break;
                    }
                
                    System.out.println("Password: ");
                
                    String pass2 = sc.next();
                
                    System.out.println("Nome da empresa: ");
                
                    String nomeE = sc.next();
                    
                    System.out.println("Coordenadas: ");
                    System.out.println("Latitude: ");
                    double latitude2 = sc.nextDouble();
                    System.out.println("Longitude: ");
                    double longitude2 = sc.nextDouble();
                    
                    System.out.println("Raio: ");
                    
                    double raio = sc.nextDouble();
                    
                    System.out.println("NIF: ");
                    
                    int nif = sc.nextInt();
                    
                    String nifs = Integer.toString(nif);
                    if (nifs.length()!=9){
                        System.out.println("NIF incorreto!");
                        break;
                    }
                    
                    System.out.println("Preço por Km: ");
                    
                    double preco_por_km = sc.nextDouble();
                    
                    System.out.println("Aceita encomendas médicas:\n1.Sim\n2.Não");
                    
                    int encMed = sc.nextInt();
                    
                    boolean aceita = true;
                    
                    if (encMed == 1){aceita = true;}
                    if (encMed == 2){aceita = false;}
                    if (encMed != 1 && encMed != 2) break;
                    
                    this.estado.registaEmpresa(codE, nomeE, latitude2, longitude2, raio, nif, preco_por_km, aceita, pass2);
                    
                    System.out.println("Registado com sucesso!");
                }
                catch(InputMismatchException e){
                    System.out.println(e.getMessage());
                }
                break;
                
            case 4:
                try{
                    System.out.println("Código de voluntário(por exemplo, \"v42\"): ");
                    
                    String codV = sc.next();
                    
                    if(codV.charAt(0) != 'v'){
                        System.out.println("Utilizador inválido!");
                        break;
                    }
                    
                    System.out.println("Password: ");
                    
                    String pass3 = sc.next();
                    
                    System.out.println("Nome: ");
                    
                    String nomeV = sc.next();
                    
                    System.out.println("Coordenadas: ");
                    System.out.println("Latitude: ");
                    double latitude3 = sc.nextDouble();
                    System.out.println("Longitude: ");
                    double longitude3 = sc.nextDouble();
                    
                    System.out.println("Raio: ");
                    
                    double raio1 = sc.nextDouble();
                    
                    this.estado.registaVoluntario(codV, nomeV, latitude3, longitude3, raio1, pass3);
                    
                    System.out.println("Registado com sucesso!");
                }
                catch(InputMismatchException e){
                    System.out.println(e.getMessage());
                }
                break;
            
            default:
               System.out.println("Adeus!");
                       }
    }
    
    public void main(){
        opcao_0 = 1;
        
        do{
            
            menu();
        
            int opcao_1 = sc.nextInt();
            
            if(opcao_1 < 1 || opcao_1 > 7) {opcao_1 = 0; break;}         
            
            switch (opcao_1) {
                //login
                case 1:
                    int opcao_2 = 0;
                    System.out.println("Código de utilizador:");
                    String cod = sc.next();
                    String str[] = cod.split("");
                    

                    System.out.println("Password: ");
                    String pass = sc.next();
                    
                    
                    while(opcao_2 == 0){
                        switch (str[0]){
                            case "u":
                                try{
                                    boolean bol = this.estado.loga_cliente(cod, pass);
                                    //verifica se o utilizador está na lista de logins
                                    if (bol == true){
                                        System.out.println("Logado com sucesso!");
                                        menuCliente(cod);
                                    }
                                }
                                catch (NullPointerException e){
                                    System.out.println("Utilizador não registado ou password ou utilizador inválida!");
                                    opcao_2 = 1;
                                    break;
                                }
                                //Cliente cliente = loga_utilizador(cod, pass, logins);
                                //printar o saldo
                                //cria um e coloca se não existir
                                opcao_2 = 1;
                                break;
                            
                            case "l":
                                
                                try{
                                    boolean loja = this.estado.loga_loja(cod,pass);
                                    //verifica se a loja está na lista de logins
                                    if (loja == true){
                                        System.out.println("Logado com sucesso!");
                                        menuLoja(cod);
                                    }
                                }
                                catch (NullPointerException e){
                                    System.out.println("Utilizador não registado ou password ou utilizador inválida!");
                                    opcao_2 = 1;
                                    break;
                                }
                                //Cliente cliente = loga_utilizador(cod, pass, logins);
                                //printar o saldo
                                //cria um e coloca se não existir
                                opcao_2 = 1;
                                break;
                            
                            case "t":
                                
                                try{
                                    boolean emp = this.estado.loga_empresa(cod,pass);
                                    //verifica se a loja está na lista de logins
                                    if (emp == true){
                                        System.out.println("Logado com sucesso!");
                                        menuEmpresa(cod);
                                    }
                                }
                                catch (NullPointerException e){
                                    System.out.println("Utilizador não registado ou password ou utilizador inválida!");
                                    opcao_2 = 1;
                                    break;
                                }
                                //Cliente cliente = loga_utilizador(cod, pass, logins);
                                //printar o saldo
                                //cria um e coloca se não existir
                                opcao_2 = 1;
                                break;
                            
                            case "v":
                                try{
                                    boolean vol = this.estado.loga_voluntario(cod,pass);
                                
                                    if (vol == true){
                                        System.out.println("Logado com sucesso!");
                                        menuVoluntario(cod);
                                    }
                                }
                                catch (NullPointerException e){
                                    System.out.println("Utilizador não registado ou password ou utilizador inválida!");
                                    opcao_2 = 1;
                                    break;
                                }
                                
                                opcao_2 = 1;
                                break;
                                
                            default:
                                opcao_2 = 1;
                                System.out.println("Nome de utilizador ou password inválidos!");
                            }
                    }
                    break;
                    
                case 2:
                    
                    registar();
                    opcao_0 = 1;
                    break;
                
                case 3:
                    this.estado = this.estado.parse(this.estado);
                    opcao_0 = 1;
                    break;
               
                
                case 4:
                    System.out.println("TOP 10 Clientes:");
                    
                    ArrayList<String> top10 = this.estado.obtem_top10C();
                    
                    for(String s : top10){
                        System.out.println(s);
                    }
                    
                    opcao_0 = 1;
                    break;
                
                case 5:
                    System.out.println("TOP 10 Empresas:");
                    
                    ArrayList<String> top10_E = this.estado.obtem_top10E();
                    
                    for(String s : top10_E){
                        System.out.println(s);
                    }
                    
                    opcao_0 = 1;
                    break;
                
                case 6:
                    System.out.println("Nome do ficheiro:");
                    String nomefile = sc.next().toLowerCase();
                    try{
                        this.estado.gravaObj(nomefile);
                        System.out.println("Gravado com sucesso!");
                    }
                    catch(IOException e){
                        System.out.println(e.getMessage());
                    }
                    
                    opcao_0 = 1;
                    break;
                 
                case 7:
                    System.out.println("Nome do ficheiro:");
                    String nomefile1 = sc.next().toLowerCase();
                    try{
                        this.estado.lerObj(nomefile1);
                        System.out.println("Carregado com sucesso!");
                    }
                    catch(IOException e){
                        System.out.println(e.getMessage());
                    }
                    catch(ClassNotFoundException f){
                        System.out.println(f.getMessage());
                    }
                    
                    opcao_0 = 1;
                    break;
                    
                default:
                    System.out.println("Adeus!");
                    opcao_0 = 0;
                    break;
            }
        }while(opcao_0 != 0);
    }
    
    /**
     * Menu Cliente 
     */
    public void menuCliente(String codC){
        int opcaoc = 0;
        
        while(opcaoc == 0) {
            System.out.println("____________________________________________________________________________________________");
            System.out.println("                                 Menu de Cliente                                            ");
            System.out.println("____________________________________________________________________________________________");
            System.out.println("1. Ver histórico de Encomendas                                                              ");
            System.out.println("2. Fazer Encomenda                                                                          ");
            System.out.println("3. Mudar posicão atual (coordenadas)                                                        ");
            System.out.println("4. Aceitar encomendas                                                                       ");
            System.out.println("5. Classificar Transportadora                                                               ");
            System.out.println("6. Voltar                                                                                   ");
            System.out.println("____________________________________________________________________________________________");

        
            int opcao_Cliente = sc.nextInt();
            
            switch (opcao_Cliente) {
                case 1:              
                    
                    verHistorico(codC);
                    
                    opcaoc = 0;
                    break;
                
                case 2:
                
                    fazPedido(codC);
                    
                    opcaoc = 0;
                    break;
                    
                case 3:
                    //Mudar posicão atual (coordenada)
                    
                    mudarCoords(codC);
                    
                    opcaoc = 0;
                    break;
                
                
                case 4:
                    
                    aceitaEncomendas(codC);
                    
                    opcaoc = 0;
                    break;
                    
                case 5:
                        
                    classificaTransporte(codC);
                    break;
                
                case 6:
                        System.out.println("Saiu");
                        opcaoc = 1;
                        break;
                        
                default:
                        System.out.println("Opção inválida");
                        opcaoc = 1;
                        break;
            }
        }
    }
    
    /**
     * Menu Loja
     */
    public void menuLoja(String codL){
        int opcaol = 0;

        do {

            System.out.println("____________________________________________________________________________________________");
            System.out.println("                                 Menu da Loja                                               ");
            System.out.println("____________________________________________________________________________________________");
            System.out.println("1. Ver pedidos de Encomendas                                                                ");
            System.out.println("2. Ver Clientes                                                                             ");
            System.out.println("3. Ver Encomendas já prontas                                                                ");
            System.out.println("4. Gerar pedido                                                                             ");
            System.out.println("5. Adiciona produtos                                                                        ");
            System.out.println("6. Voltar                                                                                   ");
            System.out.println("____________________________________________________________________________________________");
            
            int opcao_Loja = sc.nextInt();
            
            switch (opcao_Loja) {

                //Opção de ver os pedidos de Encomenda  
                case 1:
                    visualizarPedidosEncomendas(codL);
                    
                    opcao_Loja = 0;
                    break;
                
                //Opção de ver os Clientes
                case 2:
                    visualizarClientes(codL);
                    
                    opcao_Loja = 0;
                    break;
                
                //Opção de ver Encomendas já prontas
                case 3:
                    visualizarEncomendasFeitas(codL);
                     
                    opcao_Loja = 0;
                    break;
                //Gera encomenda
                case 4:
                    
                    geraEncomenda(codL);
                    
                    opcao_Loja = 0;
                    break;
                
                case 5:
                   
                    adicionaProduto(codL);
                    
                    opcao_Loja = 0;
                    break;
                    
                default:
                    System.out.println("Voltar!");
                    opcaol = 1;
                    break;
            }
        }while (opcaol == 0);
    }
    
    /**
     * Permite visualizar o histórico de encomendas de uma determinada Loja
     */
    public void visualizarEncomendasFeitas(String codL){
        System.out.println("Selecione a hora que pretende verificar as encomendas:");
        System.out.println("Exemplo (yyyy-MM-ddTHH:mm): 2007-12-03T10:15:30");
        String hora = sc.next(); 

        LocalDateTime hora_1 = LocalDateTime.parse(hora);
        
        System.out.println("Selecione a hora que pretende verificar as encomendas:");
        System.out.println("Exemplo (yyyy-MM-ddTHH:mm): 2007-12-03T10:15:30");
        String hora1 = sc.next();
                        
        LocalDateTime hora_2 = LocalDateTime.parse(hora1);
        
        ArrayList<String> lista_encomendas = this.estado.obteminfoencomendasL(codL, hora_1, hora_2);
        System.out.println("Encomendas feitas:");
        
        for(String e: lista_encomendas){
            System.out.println(e);
        }
    }
    
    /**
     * Permite visualizar os pedidos de encomendas da loja
     */
    public void visualizarPedidosEncomendas(String codL){
        ArrayList<String> pedidos = this.estado.obteminfoPedidos(codL);
        System.out.println("Pedidos: ");
        
        for(String e: pedidos){
            System.out.println(e);
        }
    }
    
    /**
     * Visualiza todos os Clientes da fila da loja
     */
    public void visualizarClientes(String codL){
        ArrayList<String> lista_clientes = this.estado.obteminfoFila(codL);
        System.out.println("Fila: ");
        
        for(String e: lista_clientes){
            System.out.println(e);
        }
    }
    
    /**
     * Gera uma encomenda (na loja)
     */
    public void geraEncomenda(String codL){
        visualizarPedidosEncomendas(codL);
        
        System.out.println("Escolha uma encomenda para gerar!");
        
        String codE = sc.next();
        
        String codT = null;
        
        if (this.estado.getVoluntarios().isEmpty()){
            System.out.println("Selecione empresa: ");
            
            ArrayList<String> lista_empresas = this.estado.lista_empresa(codE);
            
            for(String e: lista_empresas){
                System.out.println(e);
            }
            
            codT = sc.next();
        }
        else{
            System.out.println("Voluntario encontrado!");
            ArrayList<String> vol = this.estado.lista_vol_raio(codE);
            codT = vol.get(0);
        }
        
        this.estado.gera_encomenda(codT, codE);
        
        System.out.println("Encomenda gerada com sucesso!");
        
    }
    
    /**
     * Menu Empresa
     */
    public void menuEmpresa(String codT){
        int opcaoe = 0;

        do {

            System.out.println("____________________________________________________________________________________________");
            System.out.println("                                 Menu da Empresa                                            ");
            System.out.println("____________________________________________________________________________________________");
            System.out.println("1. Escolher disponibilidade                                                                 ");
            System.out.println("2. Aceitar encomendas                                                                       ");
            System.out.println("3. Mudar coordenadas                                                                        ");
            System.out.println("4. Ver encomendas transportadas                                                             ");
            System.out.println("5. Total Faturado                                                                           ");
            System.out.println("6. Voltar                                                                                   ");
            System.out.println("____________________________________________________________________________________________");

            int opcao_Empresa = sc.nextInt();
            
            switch (opcao_Empresa){

                //Opção de ver os pedidos de Encomenda  
                case 1:
                    mudar_disponivel(codT);
                    
                    opcao_Empresa = 0;
                    break;
                
                //Opção de ver os Clientes
                case 2:
                    aceitar_encomenda(codT);
                    
                    opcao_Empresa = 0;
                    break;
                
                //Opção de ver Encomendas já prontas
                case 3:
                    mudarCoordenadas(codT);
                     
                    opcao_Empresa = 0;
                    break;
                //Gera encomenda
                case 4:
                    
                    ver_encomendas_transportadas(codT);
                    
                    opcao_Empresa = 0;
                    break;
                
                case 5:
                    
                    verGuito(codT);
                    opcao_Empresa = 0;
                    break;
                    
                default:
                    System.out.println("Voltar!");
                    opcaoe = 1;
                    break;
            }
        }while (opcaoe == 0);
    }
    
    /**
     * Menu Voluntário
     */
    public void menuVoluntario(String codT){
        int opcaol = 0;

        do {

            System.out.println("____________________________________________________________________________________________");
            System.out.println("                                 Menu do Voluntário                                         ");
            System.out.println("____________________________________________________________________________________________");
            System.out.println("1. Escolher disponibilidade                                                                 ");
            System.out.println("2. Mudar coordenadas                                                                        ");
            System.out.println("3. Ver encomendas transportadas                                                             ");
            System.out.println("4. Voltar                                                                                   ");
            System.out.println("____________________________________________________________________________________________");
        
            int opcao_Voluntario = sc.nextInt();
            
            switch (opcao_Voluntario) {

                //Opção de ver os pedidos de Encomenda  
                case 1:
                    mudar_disponivel(codT);
                    
                    opcaol = 0;
                    break;
                
                case 2:
                    mudarCoordenadas(codT);
                     
                    opcaol = 0;
                    break;
                //Gera encomenda
                case 3:
                    
                    ver_encomendas_transportadas(codT);
                    
                    opcaol = 0;
                    break;
                
                default:
                    System.out.println("Voltar!");
                    opcaol = 1;
                    break;
            }
        }while (opcaol == 0);
    }
    
    /**
     * Mudar disponibilidade do transportador
     */
    public void mudar_disponivel(String codT){
        System.out.println("Escolha a opção:\n1. Disponível\n2. Indisponível");
        int n = sc.nextInt();
        this.estado.mudadisponivel(codT,n);
    }
    
    /**
     * Transportador aceita encomendas
     */
    public void aceitar_encomenda(String codT){
        System.out.println("Escolha a encomenda para aceitar: ");
        
        ArrayList<String> lista = this.estado.obteminfoencomendas(codT);
        
        for(String x : lista){
            System.out.println(x);
        }
        
        String codE = sc.next();
        
        this.estado.aceita_encomendaT(codT, codE);
        
        System.out.println("Aceite com sucesso!");
    }
    
    /**
     * Muda coordenadas do transportador
     */
    public void mudarCoordenadas(String codT){
        try{
            System.out.println("Insira a latitude: ");
            double latitude = sc.nextDouble();
            System.out.println("Insira a longitude: ");
            double longitude = sc.nextDouble();
            
            this.estado.atualizaCoordsT(codT, latitude, longitude);
            
            System.out.println("Coordenadas alteradas com sucesso!");
        }
        catch(InputMismatchException e){
            System.out.println(e.getMessage());
        }
    }
    
    /**
     * Visualiza encomendas transportadas
     */
    public void ver_encomendas_transportadas(String codT){
        System.out.println("Encomendas transportadas: ");
        
        ArrayList<String> lista = this.estado.obtemTransportadas(codT);
        
        for(String x : lista){
            System.out.println(x);
        }
    }
    
    /**
     * Total faturado por uma empresa
     */
    public void verGuito(String codT){
        try{
            System.out.println("Total faturado");
            
            System.out.println("Selecione a hora que pretende verificar as encomendas:");
            System.out.println("Exemplo (yyyy-MM-ddTHH:mm): 2007-12-03T10:15:30");
            String hora = sc.next(); 
    
            LocalDateTime hora_1 = LocalDateTime.parse(hora);
            
            System.out.println("Selecione a hora que pretende verificar as encomendas:");
            System.out.println("Exemplo (yyyy-MM-ddTHH:mm): 2007-12-03T10:15:30");
            String hora1 = sc.next();
                            
            LocalDateTime hora_2 = LocalDateTime.parse(hora1);
            
            double total = this.estado.obteminfodinheiroT(codT,hora_1,hora_2);
            
            System.out.println("Total faturado: "+total);
        }
        catch(InputMismatchException e){
            System.out.println(e.getMessage());
        }
    }
    
    /**
     * Visualizar histórico do cliente
     */
    public void verHistorico(String codC){
        try{
            System.out.println("Selecione a hora que pretende verificar as encomendas:");
            System.out.println("Exemplo (yyyy-MM-ddTHH:mm): 2007-12-03T10:15:30");
            String hora = sc.next();
            
            LocalDateTime hora_1 = LocalDateTime.parse(hora);
                            
            System.out.println("Selecione a hora que pretende verificar as encomendas:");
            System.out.println("Exemplo (yyyy-MM-ddTHH:mm): 2007-12-03T10:15:30");
            String hora1 = sc.next();
                            
            LocalDateTime hora_2 = LocalDateTime.parse(hora1);
                            
            ArrayList<String> lista_encomendas = this.estado.obteminfoencomendasCliente(codC, hora_1, hora_2);
            System.out.println("Encomendas feitas:");
            for (String e: lista_encomendas){
                System.out.println(e);
            }
        }
        catch(InputMismatchException e){
            System.out.println(e.getMessage());
        }
    }
    
    /**
     * mudar coordenadas do cliente
     */
    public void mudarCoords(String codC){
        try{
            System.out.println("Insira a latitude: ");
            int latitude = sc.nextInt();
            System.out.println("Insira a longitude: ");
            int longitude = sc.nextInt();
                        
            this.estado.atualizaCoordsC(codC, latitude, longitude);
            
            System.out.println("Coordenadas alteradas com sucesso!");
        }
        catch(InputMismatchException e){
            System.out.println(e.getMessage());
        }
    }
    
    /**
     * Possibilita o cliente fazer um pedido
     */
    public void fazPedido(String codC){
        
        System.out.println("Lojas: ");
                    
        ArrayList<String> lojas1 = this.estado.obtemLojas();
        
        if(!lojas1.isEmpty()) {
                    
            for (String loja: lojas1){
                System.out.println(loja);
            }
                    
            System.out.println("\nEscolha a loja: ");
                    
            String nome = sc.next();
        
            this.estado.fazPedido(codC, nome);
        
            System.out.println("Pedido adicionado com sucesso!");
        }
        else System.out.println("Não existem lojas!");
    }
    
    /**
     * De todas as encomendas prontas para transporte, o cliente aceita uma 
     */
    public void aceitaEncomendas(String codC){
        System.out.println("Encomendas prontas!\n Escolha uma para aceitar:");
                    
        this.estado.aceitaEncomendas(codC);
    }
    
    /**
     * Classifica transporte
     */
    public void classificaTransporte(String codC){
        System.out.println("Transportadoras: ");
        
        this.estado.mostraTransportadoras(codC);
        
        String trans = sc.next();
        
        System.out.println("Classificar transportadora de 0 a 5:");
        
        int classifica = sc.nextInt();
        
        this.estado.classificar_transporte(trans, classifica);
        
        System.out.println("Obrigado pela sua classificação!");
    }
    
    /**
     * Adiciona um produto à loja
     */
    public void adicionaProduto(String codL){
        System.out.println("Código do produto: ");
        
        String codP = sc.next();
        
        System.out.println("Descrição: ");
        
        String descricao = sc.next();
        
        System.out.println("Preço unitário: ");
        
        double valorUnitario = sc.nextDouble();
        
        System.out.println("Quantidade: ");
        
        int quantidade = sc.nextInt();
        
        System.out.println("Peso Unitário: ");
        
        double pesounitario = sc.nextDouble();
        
        this.estado.add_Prod_Loja(codL, codP, descricao, valorUnitario, quantidade, pesounitario);
        
        System.out.println("Produto adicionado com sucesso!");
    }
}
