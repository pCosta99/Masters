import java.util.Scanner;
import java.io.*;
import java.lang.String;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.time.LocalDateTime;
import java.util.InputMismatchException;
import java.lang.ClassNotFoundException;
import java.time.DateTimeException;


public class TrazAquiAPP{
    
    private TrazAqui logAPP;
    private Menu menuinicial,menuTipoDeUsuario,menuUtilizadorBasico,menuLoja,menuLojaComFila,
            menuEmpresaTransportadora,menuVoluntario,menuMudarInfo;
    
    public static void main(String args[]) throws IOException,ClassNotFoundException{
        new TrazAquiAPP().run();
    }
    
    private TrazAquiAPP(){
        this.logAPP=new TrazAqui();
        
        String[] st={"1) Fazer login;",
                     "2) Criar usuario;",
                     "3) Criar usuario apartir de um usuario pre-existente;",
                     "4) Lista dos 10 utilizadores que mais usam o sistema;",
                     "5) Lista das 10 empresas tramsportadoras que mais usam o sistema;"};
        this.menuinicial=new Menu(st);
        String[] st1={"1) Utilizador normal;",
                      "2) Loja;",
                      "3) Loja com Fila;",
                      "4) Empresa Transportadora;",
                      "5) Voluntario;"};
        this.menuTipoDeUsuario = new Menu(st1);
        String[] st2 = {"1) Ver encomendas nao entregues;",
                        "2) Informacao sobre encomendas efetuadas;",
                        "3) Solicitar a entrega de uma encomenda;",
                        "4) Ver encomendas prontas;",
                        "5) Ver propostas de trasnporte;",
                        "6) Aceitar proposta de transporte;",
                        "7) Ver classificacao de transportador;",
                        "8) Classificar transportador (entre 1 e 10);",
                        "9) Tabela de precos dos transportadores;",
                        "10) Pedir nova encomenda;",
                        "11) Mudar informacao;"};
        this.menuUtilizadorBasico=new Menu(st2);
        String[] st3={"1) Ver encomendas nao entregues;",
                      "2) Informacao sobre encomendas efetuadas;",
                      "3) Solicitar transporte de encomenda;",
                      "4) Sinalizar encomenda pronta ao cliente;",
                      "5) Ver pedidos de encomendas;",
                      "6) Aceitar pedido de encomenda;",
                      "7) Recusar pedido de encomenda;",
                      "8) Mudar informacao;"};
        this.menuLoja=new Menu(st3); 
        String[] st4={"1) Ver encomendas nao entregues;",
                      "2) Informacao sobre encomendas efetuadas;",
                      "3) Solicitar transporte de encomenda;",
                      "4) Sinalizar encomenda pronta ao cliente;",
                      "5) Ver pedidos de encomendas;",
                      "6) Aceitar pedido de encomenda;",
                      "7) Recusar pedido de encomenda;",
                      "8) Mudar numero de pessoa na fila;",
                      "9) Mudar informacao;"};
        this.menuLojaComFila=new Menu(st4);    
        String[] st5={"1) Ver encomendas disponibelizadas pelas lojas;",
                      "2) Informacao sobre encomendas efetuadas;",
                      "3) Aceitar transportar encomenda;",
                      "4) Registar encomenda entrege;",
                      "5) Mudar transporte de encomendas medicas;",
                      "6) Informacao sobre encomendas por entregar;",
                      "7) Ver total faturado num determinado periodo;",
                      "8) Mudar informacao;",
                      "9) Mudar preco por kilometro;",
                      "10) Mudar nif;",
                      "11) Mudar capacidade;",
                      "12) Mudar raio;"};
        this.menuEmpresaTransportadora=new Menu(st5);
        String[] st6={"1) Ver encomendas disponibelizadas pelas lojas;",
                      "2) Ver encomendas por entregar;",
                      "3) Alterar disponibilidade;",
                      "4) Transportar encomenda;",
                      "5) Registar encomenda entrege;",
                      "6) Mudar transporte de encomendas medicas;",
                      "7) Ver informacoes sobre encomendas;",
                      "8) Ver encomendas efetuadas num determinado periodo;",
                      "9) Mudar informacao;",
                      "10) Mudar raio;"};
        this.menuVoluntario=new Menu(st6);
        
        String[] st7={"1) Alaterar nome;",
                      "2) Alaterar localizacao;",
                      "3) Alaterar e-mail;",
                      "4) Mudar password;"};
        this.menuMudarInfo=new Menu(st7);
        
        try {
           this.logAPP.carregarFicheiro();
           
        }catch (FileNotFoundException e){
            System.out.println("Ficheiro nao encontrado.\n");
            
        }catch (IOException e){
            System.out.println("Erro de leitura.\n");
            
        }catch (NumberFormatException e){
            System.out.println("Erro de leitura.\n");
            
        }
        System.out.println("------Traz Aqui!! App------\n");
    }
    
    private void run() throws IOException{
        String input,mail,pass,cod;
        
        do {
            this.menuinicial.executa();
            switch(this.menuinicial.getOpcao()){
                case 1:
                    System.out.println("Email de login: ");
                    mail=new Scanner(System.in).nextLine();
                    System.out.println("\nPassword: ");
                    pass=new Scanner(System.in).nextLine();
                    if(this.logAPP.login(mail,pass)){
                        this.menuUsuario();
                    }else{
                        System.out.println("Email ou password nao correspodem a nenhum usuario.\n");
                    }
                    break;
                case 2:
                    do{
                        System.out.println("Escolha o tipo de usuario:\n");
                        menuTipoDeUsuario.executa();
                        switch (menuTipoDeUsuario.getOpcao()){
                            case 1://utilizador basico
                                System.out.println("Utilizador basico:\nForma:<latitude>;<longitude>;<codigo>;<nome>;<e-mail>;<password>\n");
                                input=new Scanner(System.in).nextLine();
                                try{
                                    if (confirmar(input,6)){
                                        switch (this.logAPP.novoUtilizadorBasico(input)){
                                        case 0:
                                            System.out.println("Utilizador criado com sucesso.\n");
                                            break;
                                        case 1:
                                            System.out.println("Email ja registado.\n");
                                            break;
                                        default:
                                            System.out.println("Codigo ja registado.\n");
                                            break;
                                        }
                                    }  
                                }catch (ArrayIndexOutOfBoundsException a){
                                    System.out.println("Input invalido.\n");
                                }catch (Exception e){
                                    System.out.println("Input invalido.\n");
                                }
                                menuTipoDeUsuario.setOpcao(0);
                                break;
                            case 2://loja
                                System.out.println("Loja:\nForma:<latitude>;<longitude>;<codigo>;<nome>;<e-mail>;<password>\n");
                                input=new Scanner(System.in).nextLine();
                                try{
                                    if (confirmar(input,6)){
                                        switch (this.logAPP.novaLoja(input)){
                                        case 0:
                                            System.out.println("Utilizador criado com sucesso.\n");
                                            break;
                                        case 1:
                                            System.out.println("Email ja registado.\n");
                                            break;
                                        default:
                                            System.out.println("Codigo ja registado.\n");
                                            break;
                                        }
                                    }  
                                }catch (ArrayIndexOutOfBoundsException a){
                                    System.out.println("Input invalido.\n");
                                }catch (Exception e){
                                    System.out.println("Input invalido.\n");
                                }
                                menuTipoDeUsuario.setOpcao(0);
                                break;
                            case 3://loja com fila
                                System.out.println("Loja:\nForma:<latitude>;<longitude>;<codigo>;<nome>;<e-mail>;<password>\n");
                                input=new Scanner(System.in).nextLine();
                                try{
                                    if (confirmar(input,6)){
                                        switch (this.logAPP.novaLojaComFila(input)){
                                        case 0:
                                            System.out.println("Utilizador criado com sucesso.\n");
                                            break;
                                        case 1:
                                            System.out.println("Email ja registado.\n");
                                            break;
                                        default:
                                            System.out.println("Codigo ja registado.\n");
                                            break;
                                        }
                                    } 
                                }catch (ArrayIndexOutOfBoundsException a){
                                    System.out.println("Input invalido.\n");
                                }catch (Exception e){
                                    System.out.println("Input invalido.\n");
                                }
                                menuTipoDeUsuario.setOpcao(0);
                                break;
                            case 4://Empresa transportadora
                                System.out.println("Empresa transportadora:\nForma:<latitude>;<longitude>;<codigo>;<nome>;<e-mail>;<password>;<nif>;<capacidade>;<raio>;<preco por km>\n");
                                input=new Scanner(System.in).nextLine();
                                try{
                                    if (confirmar(input,10)){
                                        switch (this.logAPP.novaEmpresaTransportadora(input)){
                                        case 0:
                                            System.out.println("Utilizador criado com sucesso.\n");
                                            break;
                                        case 1:
                                            System.out.println("Email ja registado.\n");
                                            break;
                                        default:
                                            System.out.println("Codigo ja registado.\n");
                                            break;
                                        }
                                    }  
                                }catch (ArrayIndexOutOfBoundsException a){
                                    System.out.println("Input invalido.\n");
                                }catch (Exception e){
                                    System.out.println("Input invalido.\n");
                                }
                                menuTipoDeUsuario.setOpcao(0);
                                break;
                            case 5://voluntario
                                System.out.println("Voluntario:\nForma:<latitude>;<longitude>;<codigo>;<nome>;<e-mail>;<password>;<nif>;<raio>\n");
                                input=new Scanner(System.in).nextLine();
                                try{
                                    if (confirmar(input,8)){
                                        switch (this.logAPP.novoVoluntario(input)){
                                        case 0:
                                            System.out.println("Utilizador criado com sucesso.\n");
                                            break;
                                        case 1:
                                            System.out.println("Email ja registado.\n");
                                            break;
                                        default:
                                            System.out.println("Codigo ja registado.\n");
                                            break;
                                        }
                                    }  
                                }catch (ArrayIndexOutOfBoundsException a){
                                    System.out.println("Input invalido.\n");
                                }catch (Exception e){
                                    System.out.println("Input invalido.\n");
                                }
                                menuTipoDeUsuario.setOpcao(0);
                                break;
                            case 0:
                                System.out.println("Voltou ao menu inicial com sucesso!\n");
                                break;
                            default:
                                System.out.println("Opcao invalida.");
                                break;

                        }
                    }while(menuTipoDeUsuario.getOpcao()!=0 );
                    break;
                case 3://3) Criar usuario apartir de um usuario pre-existente
                    System.out.println("Codigo: ");
                    cod=((new Scanner(System.in)).nextLine());
                    System.out.println("\nNovo email: ");
                    mail=(new Scanner(System.in)).nextLine();
                    System.out.println("\nNova password: ");
                    pass=(new Scanner(System.in)).nextLine();
                    try{
                        if (confirmar((cod+";"+mail+";"+pass),3)){
                            switch (this.logAPP.criarUsuarioExistente(cod,mail,pass)){
                                case 0:
                                    System.out.println("Email e password defenidos com sucesso.\n");
                                    break;
                                case 1:
                                    System.out.println("Email ja registado.\n");
                                    break;
                                case 2:
                                    System.out.println("Usuario ja tem email.\n");
                                    break;
                                default:
                                    System.out.println("Usuario com codigo dado nao existe.\n");
                                    break;
                            }
                        }
                    }catch (ArrayIndexOutOfBoundsException a){
                        System.out.println("Input invalido.\n");
                    }
                    break;
                case 4:
                    System.out.println(this.logAPP.dezutilizadores());
                    break;
                case 5:
                    System.out.println(this.logAPP.dezempresas());
                    break;
                case 0:
                    System.out.println("Saiu com exito.\n");
                    break;

            }   
            System.out.println("__________________________________\n\n");
        }while (menuinicial.getOpcao()!=0);
        
        try {
            this.logAPP.salvarEstado();
        }catch (IOException e){
            System.out.println("Nao foi possivel salvar o estado.");
        }
    }
    
    public boolean confirmar(String input,int t){
        
        int i;
        String[] campos = input.split(";");
        if (campos.length>t) return false;
        StringBuilder sb=new StringBuilder();
        for (i=0;i<t;i++){
            sb.append(campos[i]).append(";");        
        }
        
        System.out.println("Confirme: \n"+sb.toString());
        System.out.println("1 -> SIM\n");
        
        try {
            i = (new Scanner(System.in)).nextInt();
        }catch (InputMismatchException e){i=0;}
        return i==1;
    }    
    
    public void menuUsuario () throws IOException{
        switch (this.logAPP.getClassUsuario()){
            case 0:
                this.menuUtilizadorBasico();
                break;
            case 1:
                this.menuLoja();
                break;
            case 2:
                this.menuLojaComFila();
                break;
            case 3:
                this.menuEmpresaTransportadora();
                break;
            case 4:
                this.menuVoluntario();
                break;

        }
    }
    
    public void menuUtilizadorBasico() throws IOException{
              
        String estado;
        String input,codLoja,codEnc,codTra,nproduto;
        String campos[];
        Encomenda enc;
        
        do{
            System.out.println(this.logAPP.getEstadoUB());
            this.menuUtilizadorBasico.executa();
            switch (this.menuUtilizadorBasico.getOpcao()){
                case 1:
                    System.out.println(this.logAPP.getGEncomendas());
                    break;
                case 2:
                    System.out.println(this.logAPP.informacao());
                    break;
                case 3://3) Solicitar a entrega de uma encomenda
                    System.out.println("Codigo de encomenda: ");
                    codEnc=new Scanner(System.in).nextLine();
                    try {
                        switch (this.logAPP.solicitarTransporte(codEnc)){
                            case 0:
                                System.out.println("Transporte de encomenda solicitado com sucesso.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de encomenda nao e valida.\n");
                                break;
                        }
                    }catch (ClassCastException e) {
                        System.out.println("Erro\n");
                    }catch (NullPointerException e) {
                        System.out.println("Codigo invalido\n");
                    }
                    break;
                case 4://ver encomendas prontas
                    System.out.println(this.logAPP.getEncomendasProntas());
                    break;
                case 5:
                    System.out.println(this.logAPP.getPropostasDeTransporteUB());
                    break;
                case 6://6) Aceitar proposta de transporte
                    System.out.println("Codigo de encomenda: ");
                    codEnc=new Scanner(System.in).nextLine();
                    System.out.println("Codigo de transportador: ");
                    codTra=new Scanner(System.in).nextLine();
                    try{
                        switch (this.logAPP.aceitarPropostaUB(codEnc,codTra)){
                            case 0:
                                System.out.println("Proposta aceite.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de encomenda invalido.\n");
                                break;
                            case 2:
                                System.out.println("Codigo de transportador invalido.\n");
                                break;    
                        }
                    }catch (ClassCastException e){
                        System.out.println("Erro.\n");
                    }catch (NullPointerException e){
                        System.out.println("Codigo invalido.\n");
                    }
                    break;
                case 7:
                    System.out.println("Codigo do transportador:");
                    codTra=new Scanner(System.in).nextLine();
                    String s=this.logAPP.verClassificacao(codTra);
                    if (s==null){
                        System.out.println("Codigo de transportador invalido.\n");
                    }else{
                        System.out.println("Classificacao de "+codTra+": "+s+"\n");
                    }                    
                    break;
                case 8://8) Classificar transportador (entre 1 e 10)
                    System.out.println("Codigo do transportador:");
                    codTra=new Scanner(System.in).nextLine();
                    try {
                        System.out.println("Classificacao que pretende dar (entre 1 e 10):");
                        int i=new Scanner(System.in).nextInt();
                        switch(this.logAPP.classificarTransportadorUB(codTra,i)){
                            case 0:
                                System.out.println("Classificou transportador com sucesso.\n");
                                break;
                            case 1:
                                System.out.println("Codigo nao corresponde a nenhum transportador.\n");
                                break;
                            default:
                                System.out.println("Valor invalido (tem de ser um valor entre 1 e 10)\n");
                                break;
                        }
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");
                    }catch (ClassCastException e){System.out.println("Codigo invalido.\n");}
                    break;
                case 9:
                    System.out.println(this.logAPP.verTabelaDePrecos());
                    break;
                case 10://pedir nova encomenda
                    List<String> produtos=new ArrayList<>();
                    System.out.print("Codigo de loja:\n");
                    codLoja=new Scanner(System.in).nextLine();
                    try {
                        System.out.println("Insira os produtos um de cada vez\ncom a forma:<nome do produto>;<quantidade>\npressa 0 quando acabar\n");
                        do{
                            nproduto=new Scanner(System.in).nextLine();
                            if (nproduto.equals("0")) continue;
                            produtos.add(nproduto);
                        }while(!(nproduto.equals("0")));                    
                    }catch(InputMismatchException e){System.out.println("Erro input.\n");}
                    
                    try {
                        switch (this.logAPP.pedirNovaEncomendaUB(codLoja,produtos)){
                            case 0:
                                System.out.println("Novo pedido efetuado.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de loja nao e valido.\n");
                                break;
                            case 2:
                                System.out.println("Produtos mal introduzidos.\n");
                                break;
                        }
                    }catch (InputMismatchException e){System.out.println("Erro input.\n");
                    }catch (ArrayIndexOutOfBoundsException e){System.out.println("Input mal inserido.\n");
                    }catch (NumberFormatException e){System.out.println("Input mal inserido.\n");}
                    break;
                case 11:
                    this.menuInfo();
                    break;
                case 0:
                    System.out.println("Voltou ao menu inicial com sucesso!\n");
                    this.logAPP.removerUsuarioAtual();
                    break;

            }
        }while(this.menuUtilizadorBasico.getOpcao()!=0);
    }
    
    public void menuLoja() throws IOException{
        String input,codEnc,codTra,codU;
        int nu;
        
        do{
            System.out.println(this.logAPP.getEstadoL());
            this.menuLoja.executa();
            switch (this.menuLoja.getOpcao()){
                case 1:
                    System.out.println(this.logAPP.getGEncomendas());
                    break;
                case 2:
                    System.out.println(this.logAPP.informacao());
                    break;
                case 3://3) Solicitar transporte de uma encomenda
                    codEnc = new Scanner(System.in).nextLine();
                    try {
                        switch (this.logAPP.solicitarTransporte(codEnc)){
                            case 0:
                                System.out.println("Transporte solicitado com sucesso.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de encomenda nao e valido.\n");
                                break;
                        }
                    }catch (ClassCastException e){System.out.println("ERRO.\n");}
                    break;
                case 4://4) Sinalizar encomenda pronta ao cliente
                    System.out.println("Insira o codigo de encomenda a sinalizar:");
                    codEnc = new Scanner(System.in).nextLine();
                    try {
                        switch (this.logAPP.sinalizarEncPronta(codEnc)){
                            case 0:
                                System.out.println("Encomenda sinalizada com sucesso.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de encomenda nao valido.\n");
                                break;
                            default:
                                System.out.println("Encomenda ja foi sinalizada ao cliente");
                                break;
                        }
                    }catch (ClassCastException e){
                        System.out.println("Erro.\n");
                    }catch (NullPointerException e){
                        System.out.println("Codigo invalido.\n");
                    }
                    break;
                case 5://ver pedidos de encomendas
                    System.out.println(this.logAPP.verPedidosDeEncomendas());
                    break;
                case 6://aceitar pedido de encomenda
                    System.out.println("Codigo de utilizador:");
                    codU=new Scanner(System.in).nextLine();
                    System.out.println("Numero da encomenda:");
                    try{ nu=new Scanner(System.in).nextInt();
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");continue;}
                    try{
                        switch (this.logAPP.aceitarPedidoDeEncomenda(codU,nu)){
                            case 0:
                                System.out.println("Encomenda adicionada.\n");
                                break;
                            case 1:
                                System.out.println("Utilizador nao tem pedidos.\n");
                                break;
                            case 2:
                                System.out.println("Numero de pedido invalido.\n");
                                break;
                            case 3:
                                System.out.println("Codigo de encomenda ja existe.\n");
                                break;
                        }
                    }catch (ArrayIndexOutOfBoundsException a){System.out.println("Input invalido.\n");}
                    break;
                case 7://recusar pedido de encomenda
                    System.out.println("Codigo de utilizador:");
                    codU=new Scanner(System.in).nextLine();
                    System.out.println("Numero da encomenda:");
                    try{ nu=new Scanner(System.in).nextInt();
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");continue;}

                    switch (this.logAPP.recusarPedido(codU,nu)){
                         case 0:
                            System.out.println("Pedido de encomenda removido.\n");
                            break;
                         case 1:
                            System.out.println("Utilizador nao tem pedidos.\n");
                            break;
                         case 2:
                            System.out.println("Numero de pedido invalido.\n");
                            break;
                    }
                    
                    break;
                case 8:
                    this.menuInfo();
                    break;
                case 0:
                    System.out.println("Voltou ao menu inicial com sucesso!\n");
                    this.logAPP.removerUsuarioAtual();
                    break;

            }
        }while(this.menuLoja.getOpcao()!=0);
    }
    
    public void menuLojaComFila() throws IOException{
        String input,codEnc,codTra,codU;
        int nu;
        
        do{
            System.out.println(this.logAPP.getEstadoL());
            this.menuLojaComFila.executa();
            switch (this.menuLojaComFila.getOpcao()){
              case 1:
                    System.out.println(this.logAPP.getGEncomendas());
                    break;
                case 2:
                    System.out.println(this.logAPP.informacao());
                    break;
                case 3://3) Solicitar transporte de uma encomenda
                    codEnc = new Scanner(System.in).nextLine();
                    try {
                        switch (this.logAPP.solicitarTransporte(codEnc)){
                            case 0:
                                System.out.println("Transporte solicitado com sucesso.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de encomenda nao e valido.\n");
                                break;
                        }
                    }catch (ClassCastException e){System.out.println("ERRO.\n");}
                    break;
                case 4://4) Sinalizar encomenda pronta ao cliente
                    System.out.println("Insira o codigo de encomenda a sinalizar:");
                    codEnc = new Scanner(System.in).nextLine();
                    try {
                        switch (this.logAPP.sinalizarEncPronta(codEnc)){
                            case 0:
                                System.out.println("Encomenda sinalizada com sucesso.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de encomenda nao valido.\n");
                                break;
                            default:
                                System.out.println("Encomenda ja foi sinalizada ao cliente");
                                break;
                        }
                    }catch (ClassCastException e){
                        System.out.println("Erro.\n");
                    }catch (NullPointerException e){
                        System.out.println("Codigo invalido.\n");
                    }
                    break;
                case 5://ver pedidos de encomendas
                    System.out.println(this.logAPP.verPedidosDeEncomendas());
                    break;
                case 6://aceitar pedido de encomenda
                    System.out.println("Codigo de utilizador:");
                    codU=new Scanner(System.in).nextLine();
                    System.out.println("Numero da encomenda:");
                    try{ nu=new Scanner(System.in).nextInt();
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");continue;}
                    try{
                        switch (this.logAPP.aceitarPedidoDeEncomenda(codU,nu)){
                            case 0:
                                System.out.println("Encomenda adicionada.\n");
                                break;
                            case 1:
                                System.out.println("Utilizador nao tem pedidos.\n");
                                break;
                            case 2:
                                System.out.println("Numero de pedido invalido.\n");
                                break;
                            case 3:
                                System.out.println("Codigo de encomenda ja existe.\n");
                                break;
                        }
                    }catch (ArrayIndexOutOfBoundsException a){System.out.println("Input invalido.\n");}
                    break;
                case 7://recusar pedido de encomenda
                    System.out.println("Codigo de utilizador:");
                    codU=new Scanner(System.in).nextLine();
                    System.out.println("Numero da encomenda:");
                    try{ nu=new Scanner(System.in).nextInt();
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");continue;}

                    switch (this.logAPP.recusarPedido(codU,nu)){
                         case 0:
                            System.out.println("Pedido de encomenda removido.\n");
                            break;
                         case 1:
                            System.out.println("Utilizador nao tem pedidos.\n");
                            break;
                         case 2:
                            System.out.println("Numero de pedido invalido.\n");
                            break;
                    }
                    
                    break;
                case 8:
                    try{
                        int f=new Scanner(System.in).nextInt();
                        this.logAPP.mudarFila(f);
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");}
                    break;
                case 9:
                    this.menuInfo();
                    break;
                case 0:
                    System.out.println("Voltou ao menu inicial com sucesso!\n");
                    this.logAPP.removerUsuarioAtual();
                    break;

            }
        }while(this.menuLojaComFila.getOpcao()!=0);
    }
    
    public void menuEmpresaTransportadora() throws IOException{
        String input,codEnc, codTra,codL,tempo,tempo1,tempo2;
        float custo,distancia;
        int i;
 
        do{
            
            System.out.println(this.logAPP.getEstadoET());
            this.menuEmpresaTransportadora.executa();
            switch (this.menuEmpresaTransportadora.getOpcao()){
                case 1:
                    System.out.println(this.logAPP.verTodosPedidosTransporte());
                    break;
                case 2:
                    System.out.println(this.logAPP.informacao());
                    break;
                case 3://solicitar transportar encomenda
                    System.out.println("Codigo da loja: ");
                    codL=new Scanner(System.in).nextLine();
                    System.out.println("\nCodigo da encomenda:");
                    codEnc=new Scanner(System.in).nextLine();
                    System.out.println("\nCusto:");
                    try {
                        custo=new Scanner(System.in).nextFloat();
                    }catch (InputMismatchException e){System.out.println("Custo dado invalido.\n");continue;}
                    try{
                       if (this.logAPP.excedeCapacidade()){
                           System.out.println("Encomenda pode exceder capacidade pressa 1 se quiser continuar;");
                           if (!((new Scanner(System.in).nextLine()).equals("1"))) continue;
                        }
                        switch (this.logAPP.transportarET(codL,codEnc,custo)){
                            case 0:
                                System.out.println("Transporte solicitado.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de loja invalido.\n");
                                break;
                            case 2:
                                System.out.println("Codigo de encomenda invalido.\n");
                                break;
                            case 3:
                                System.out.println("Nao esta no raio.\n");
                                break;
                       }
                    }catch (ClassCastException e){
                        System.out.println("Erro.\n");
                    }catch (NullPointerException e){
                        System.out.println("Codigo invalido.\n");
                    }
                    break;
                case 4://4) Registar encomenda entregue
                    System.out.println("Codigo de encomenda: ");
                    codEnc=new Scanner(System.in).nextLine();
                    System.out.println("Data e tempo de entrega:\n(Forma:<dia>;<mes>;<ano>;<hora>;<minutos>)\n");
                    tempo=new Scanner(System.in).nextLine();
                    System.out.println("Custo:");
                    try {custo=new Scanner(System.in).nextFloat();}
                    catch(InputMismatchException e){System.out.println("Valor invalido\n");continue;}
                    System.out.println("Kilometros percorridos:");
                    try {distancia=new Scanner(System.in).nextFloat();}
                    catch(InputMismatchException e){System.out.println("Valor invalido\n");continue;}
                    try {
                        switch (this.logAPP.registaEncEntregue(codEnc,tempo,custo,distancia)){
                            case 0:
                                System.out.println("Encomenda registada.\n");
                                break;
                            case 1:
                                System.out.println("Input invalido.\n");
                                break;
                            case 2:
                                System.out.println("Codigo de encomenda invalido.\n");
                                break;
                        }
                    }catch (ClassCastException e){
                        System.out.println("Erro.\n");
                    }catch (NullPointerException e){
                        System.out.println("Codigo invalido.\n");
                    }catch (DateTimeException a){System.out.println("Input invalido.\n");
                    }catch (IllegalStateException a){System.out.println("Input invalido.\n");}
                    break;
                case 5://transpora medicamentos
                    this.logAPP.mudarTransportaMedicamentos();
                    break;
                case 6:
                    System.out.println(this.logAPP.getGEncomendas());
                    break;
                case 7://Ver total faturado num determinado periodo
                    System.out.println("Insira data e tempo na forma:<dia>;<mes>;<ano>;<hora>;<minutos>\nDepois de:");
                    tempo1=new Scanner(System.in).nextLine();
                    System.out.println("Antes de:");
                    tempo2=new Scanner(System.in).nextLine();
                    try {
                        System.out.println(this.logAPP.getfaturas(tempo1,tempo2));
                    }catch (ArrayIndexOutOfBoundsException a){System.out.println("Input invalido.\n");
                    }catch (NullPointerException a){System.out.println("Input invalido.\n");
                    }catch (DateTimeException a){System.out.println("Input invalido.\n");
                    }catch (IllegalStateException a){System.out.println("Input invalido.\n");}
                    break;
                case 8:
                    this.menuInfo();
                    break;
                case 9://mudar preco
                    System.out.println("Novo preco:");
                    try{
                        custo = new Scanner(System.in).nextFloat();
                    }catch(InputMismatchException e) {System.out.println("Input invalido.\n");continue;}
                    this.logAPP.mudarpreco(custo);
                    System.out.println("Preco alterado com sucesso.\n");
                    break;
                case 10://mudar nif
                    System.out.println("Novo nif:");
                    try {i = new Scanner(System.in).nextInt();
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");continue;}
                    this.logAPP.mudarnif(i);
                    System.out.println("Nif alterado com sucesso.\n");
                    break;
                case 11:// mudar capacidade
                    System.out.println("Nova capacidade:");
                    try {i = new Scanner(System.in).nextInt();
                    }catch (InputMismatchException e){System.out.println("Input invalido.\n");continue;}
                    this.logAPP.mudarcapacidade(i);
                    System.out.println("Capacidade alterada com sucesso.\n");
                    break;
                case 12:
                    System.out.println("Novo valor de raio:");
                    try {distancia=new Scanner(System.in).nextFloat();}
                    catch(InputMismatchException e){System.out.println("Valor invalido\n");continue;}
                    this.logAPP.mudarraio(distancia);
                    break;
                case 0:
                    System.out.println("Voltou ao menu inicial com sucesso!\n");
                    this.logAPP.removerUsuarioAtual();
                    break;

            }
        }while(this.menuEmpresaTransportadora.getOpcao()!=0);
    }
    
    public void menuVoluntario() throws IOException{
        String input,codEnc,tempo,tempo1,tempo2,codL;
        float distancia;
        
        do{
            System.out.println(this.logAPP.getEstadoV());
            this.menuVoluntario.executa();
            switch (this.menuVoluntario.getOpcao()){
                case 1:
                    System.out.println(this.logAPP.verTodosPedidosTransporte());
                    break;
                case 2:
                    System.out.println(this.logAPP.getGEncomendas());
                    break;
                case 3:
                    this.logAPP.mudarDisponibilidade();
                    break;
                case 4://esta disposto a entregar uma encomenda 
                    System.out.println("Codigo da loja: ");
                    codL=new Scanner(System.in).nextLine();
                    System.out.println("Codigo da encomenda:\n");
                    codEnc=new Scanner(System.in).nextLine();
                    try{
                        switch (this.logAPP.entregarEncomendaV(codL,codEnc)){
                            case 0:
                                System.out.println("Loja e cliente foram informados.\n");
                                break;
                            case 1:
                                System.out.println("Codigo da loja invalido.\n");
                                break;
                            case 2:
                                System.out.println("Codigo da encomenda invalido.\n");
                                break;
                            case 3:
                                System.out.println("Loja nao esta no raio.\n");
                                break;
                        }
                    }catch (ClassCastException e){
                        System.out.println("Erro.\n");
                    }catch (NullPointerException e){
                        System.out.println("Codigo invalido.\n");
                    } 
                    break;
                case 5://5) Registar encomenda entrege
                    System.out.println("Codigo de encomenda: ");
                    codEnc=new Scanner(System.in).nextLine();
                    System.out.println("Data e tempo de entrega:\n(Forma:<dia>;<mes>;<ano>;<hora>;<minutos>)\n");
                    tempo=new Scanner(System.in).nextLine();
                    System.out.println("Kilometros percorridos:");
                    try {distancia=new Scanner(System.in).nextFloat();}
                    catch(InputMismatchException e){System.out.println("Valor invalido\n");continue;}
                    try {
                        switch (this.logAPP.registaEncEntregueV(codEnc,tempo,distancia)){
                            case 0:
                                System.out.println("Registo efetuado.\n");
                                break;
                            case 1:
                                System.out.println("Codigo de encomenda invalido\n");
                                break;
                        }
                    }catch (ClassCastException e){
                        System.out.println("Erro.\n");
                    }catch (NullPointerException e){
                        System.out.println("Codigo invalido.\n");
                    }catch (DateTimeException a){System.out.println("Input invalido.\n");
                    }catch (IllegalStateException a){System.out.println("Input invalido.\n");}
                    break;
                case 6:
                    this.logAPP.mudarTransportaMedicamentos();
                    break;
                case 7://informacao
                    System.out.println(this.logAPP.informacao());
                    break;
                case 8:
                    System.out.println("Insira data e tempo na forma:<dia>;<mes>;<ano>;<hora>;<minutos>\nDepois de:");
                    tempo1=new Scanner(System.in).nextLine();
                    System.out.println("Antes de:");
                    tempo2=new Scanner(System.in).nextLine();
                    try {
                        System.out.println(this.logAPP.getfaturas(tempo1,tempo2));
                    }catch (ArrayIndexOutOfBoundsException a){System.out.println("Input invalido.\n");}
                    break;
                case 9:
                    this.menuInfo();
                    break;
                case 10:
                    System.out.println("Novo valor de raio:");
                    try {distancia=new Scanner(System.in).nextFloat();}
                    catch(InputMismatchException e){System.out.println("Valor invalido\n");continue;}
                    this.logAPP.mudarraio(distancia);
                    break;
                case 0:
                    System.out.println("Voltou ao menu inicial com sucesso!\n");
                    this.logAPP.removerUsuarioAtual();
                    break;

            }
        }while(this.menuVoluntario.getOpcao()!=0);
    }
    
    public void menuInfo () throws IOException{
        String input;
        String campos[];
        
        do{
            this.menuMudarInfo.executa();
            switch (this.menuMudarInfo.getOpcao()){
                case 1:
                    System.out.println("Novo nome:");
                    input = new Scanner(System.in).nextLine();
                    if (input.equals("0")) {System.out.println("Nome inalterado.\n");continue;}
                    this.logAPP.mudarNome(input);
                    System.out.println("Nome alterado com sucesso.\n");
                    break;
                case 2:
                    System.out.println("Nova localizacao:\nForma:<Latitude>;<Longitude>");
                    input = new Scanner(System.in).nextLine();
                    if (input.equals("0")) {System.out.println("Localizacao inalterada.\n");continue;}
                    try {
                        campos=input.split(";");
                        this.logAPP.mudarGps(Float.parseFloat(campos[0]),Float.parseFloat(campos[1]));
                        System.out.println("Localizacao alterada com sucesso.\n");
                    }catch (InputMismatchException i){
                        System.out.println("Input invalido.\n");
                    }catch (ArrayIndexOutOfBoundsException e){
                        System.out.println("Input invalido.\n");
                    }
                    
                    break;
                case 3:
                    System.out.println("Novo e-mail:\n(pressa 0 para nao fazer alteracoes)");
                    input = new Scanner(System.in).nextLine();
                    if (input.equals("0")){System.out.println("E-mail inalterado.\n");continue;}
                    this.logAPP.mudarNome(input);
                    System.out.println("Email alterado com sucesso.\n");
                    break;
                case 4:
                    System.out.println("Nova password:");
                    input = new Scanner(System.in).nextLine();
                    if (input.equals("0")){ System.out.println("Password inalterada.\n");continue;}
                    this.logAPP.mudarPass(input);
                    System.out.println("Password alterada com sucesso.\n");
                    break;
                case 0:
                    System.out.println("Voltou ao menu de usuario.\n");
                    break;
            }
        
        }while(this.menuMudarInfo.getOpcao()!=0);
    }
}
