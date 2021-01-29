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

/**
 * Classe Teste :: Corre a aplicação. 
 */

public class Teste implements Serializable
{
    private TrazAqui t;
    
    
    public static void main(){
        new Teste().run();
    }
        
    private Teste(){
            try{
                this.t = TrazAqui.lerLog("logs.txt");
            }
            catch(FileNotFoundException f){
                System.out.println("Ficheiro logs não encontrado!");
                this.t = new TrazAqui();
            }catch (IOException f){
                System.out.println("Io logs não encontrado!");
                this.t = new TrazAqui();
            }
    }
    
    
    //Termina a aplicação
    private void terminar(){
        try{ 
                this.t.writeFile2("logs.txt");
        }catch (FileNotFoundException f){
                System.out.print("Erro na gravação!");
        }catch(IOException f){
                System.out.print("Erro na gravação!");
        }
    }
    
    //Corre a aplicação
    public void run(){
        System.out.println("Seja bem vindo!!");
        System.out.println("1: Login");
        System.out.println("2: Registo");
        System.out.println("0: Cancelar");
        Scanner sc = new Scanner(System.in);
        System.out.print("Selecione uma das opções: ");
        String opcao = sc.nextLine();
        if(Integer.parseInt(opcao)==1){
            Scanner mail = new Scanner(System.in);
            System.out.println("Introduza o seu email: ");
            String email = mail.nextLine();
            boolean existe = this.t.emailPertence(email);
            
            if(existe){
                Scanner pass = new Scanner(System.in);
                System.out.println("Introduza a sua password: ");
                String p = pass.nextLine();
                
                if(this.t.credenciasCorretas(email,p)){
                    System.out.println("É bom vê-lo/a novamente " + this.t.getUser(email).getName()+"!");
                    
                    if(this.t.getUser(email).getClass().getName().equals("Comprador")){
                        this.opsComprador(email);
                    }
                    
                    if(this.t.getUser(email).getClass().getName().equals("Loja")){
                        this.opsLoja(email);
                    }
                    
                    if(this.t.getUser(email).getClass().getName().equals("Empresa")){
                        this.opsEmpresa(email);
                    }
                    
                    if(this.t.getUser(email).getClass().getName().equals("Voluntarios")){
                        this.opsVoluntario(email);
                    }
                }
                else{
                    System.out.println("Password não corresponde! Processo terminado.");
                    this.terminar();
                    return;
                }
            }
            else{
                System.out.println("Email não registado! Processo terminado.");
                this.terminar();
                return;
            }
        }
        
        else if(Integer.parseInt(opcao)==2){
            Scanner ut = new Scanner(System.in);
            System.out.println("1: Comprador");
            System.out.println("2: Loja");
            System.out.println("3: Voluntário");
            System.out.println("4: Empresa Transportadora");
            int usertype = ut.nextInt();
            
            if(usertype==1){
                String mailC = registaComp();
                if(mailC==null){
                    System.out.println("Não foi possível fazer o registo! Processo terminado.");
                    this.terminar();
                    return;
                }
                else{
                    this.opsComprador(mailC);
                }
            }
            
            if(usertype==2){
                String mailL = registaLoja();
                if(mailL==null){
                    System.out.println("Não foi possivel fazer o registo! Processo terminado.");
                    this.terminar();
                    return;
                }
                else{
                    this.opsLoja(mailL);
                }
            }
            
            if(usertype==3){
                String mailV = registaVol();
                if(mailV==null){
                    System.out.println("Não foi possivel fazer o registo! Processo terminado.");
                    this.terminar();
                    return;
                }
                else{
                    this.opsVoluntario(mailV);
                }
            }
            
            if(usertype==4){
                String mailE = registaEmp();
                if(mailE==null){
                    System.out.println("Não foi possivel fazer o registo! Processo terminado.");
                    this.terminar();
                    return;
                }
                else{
                    this.opsEmpresa(mailE);
                }
            }
        }
        
        else if(Integer.parseInt(opcao)==0){
            System.out.println("A aplicação vai encerrar.");
            this.terminar();
            return;
        }
        else{
            System.out.println("Comando inválido!");
            System.out.println("A aplicaçao vai encerrar.");
            this.terminar();
            return;
        }
    }
    
    
    //Registar um novo comprador
    public String registaComp(){
        Scanner e = new Scanner(System.in);
        System.out.println("Indique o seu email: ");
        String newEmail = e.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("Escreva o seu nome: ");
        String newName = n.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("Crie uma password: ");
        String newPass = p.nextLine();
        
        Scanner gpsx = new Scanner(System.in);
        Scanner gpsy = new Scanner(System.in);
        System.out.println("Indique as coordenadas da localização para deixar as suas encomendas.");
        System.out.println("Gpsx: ");
        String newgpsx = gpsx.nextLine();
        System.out.println("Gpsy: ");
        String newgpsy = gpsy.nextLine();
        
        Localizacao coord = new Localizacao(Double.parseDouble(newgpsx),Double.parseDouble(newgpsy));
        
        Comprador c = null;
        
        try{
            c = new Comprador(newEmail,newPass,newName,coord,new ArrayList<>());
        }
        catch(NullPointerException erro){
            System.out.println("Erro no preenchimento!");
            return null;
        }
        
        if(this.t.addUser(c)==1){
            return newEmail;
        }
        else{
            return null;
        }    
    }
    
    
    //Regista uma nova loja
    public String registaLoja(){
        Scanner e = new Scanner(System.in);
        System.out.println("Indique o email da loja: ");
        String newEmail = e.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("Escreva o nome da loja: ");
        String newName = n.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("Crie uma password: ");
        String newPass = p.nextLine();
        
        Scanner gpsx = new Scanner(System.in);
        Scanner gpsy = new Scanner(System.in);
        System.out.println("Indique as coordenadas da localização para deixar as suas encomendas");
        System.out.println("Gpsx: ");
        String newgpsx = gpsx.nextLine();
        System.out.println("Gpsy: ");
        String newgpsy = gpsy.nextLine();
        
        Localizacao coord = new Localizacao(Double.parseDouble(newgpsx),Double.parseDouble(newgpsy));
        
        
        Loja l = null;
        
        try{
            l = new Loja(newEmail,newPass,newName,coord,false,new ArrayList<>());
        }
        catch(NullPointerException erro){
            System.out.println("Erro no preenchimento!");
            return null;
        }
        
        if(this.t.addUser(l)==1){
            return newEmail;
        }
        else{
            return null;
        }    
    }
    
    
    //Regista um novo voluntário
    public String registaVol(){
        Scanner e = new Scanner(System.in);
        System.out.println("Indique o seu email: ");
        String newEmail = e.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("Escreva o seu nome: ");
        String newName = n.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("Crie uma password: ");
        String newPass = p.nextLine();
        
        Scanner gpsx = new Scanner(System.in);
        Scanner gpsy = new Scanner(System.in);
        System.out.println("Indique as coordenadas da localização para deixar as suas encomendas");
        System.out.println("Gpsx: ");
        String newgpsx = gpsx.nextLine();
        System.out.println("Gpsy: ");
        String newgpsy = gpsy.nextLine();
        
        Localizacao coord = new Localizacao(Double.parseDouble(newgpsx),Double.parseDouble(newgpsy));
        
        Scanner r = new Scanner(System.in);
        System.out.println("Indique o seu raio de ação: ");
        String newRaio = r.nextLine();

        Voluntarios v = null;
        
        try{
            v = new Voluntarios(newEmail,newPass,newName,coord,Double.parseDouble(newRaio),false,0,0);
        }
        catch(NullPointerException erro){
            System.out.println("Erro no preenchimento!");
            return null;
        }
        
        if(this.t.addUser(v)==1){
            return newEmail;
        }
        else{
            return null;
        }
    }
    
    
    //Registar uma nova empresa
    public String registaEmp(){
        Scanner e = new Scanner(System.in);
        System.out.println("Indique o email da empresa: ");
        String newEmail = e.nextLine();
        
        Scanner n = new Scanner(System.in);
        System.out.println("Escreva o nome da empresa: ");
        String newName = n.nextLine();
        
        Scanner p = new Scanner(System.in);
        System.out.println("Crie uma password: ");
        String newPass = p.nextLine();
        
        Scanner gpsx = new Scanner(System.in);
        Scanner gpsy = new Scanner(System.in);
        System.out.println("Indique as coordenadas da localização para deixar as suas encomendas");
        System.out.println("Gpsx: ");
        String newgpsx = gpsx.nextLine();
        System.out.println("Gpsy: ");
        String newgpsy = gpsy.nextLine();
        
        Localizacao coord = new Localizacao(Double.parseDouble(newgpsx),Double.parseDouble(newgpsy));
        
        Scanner nif = new Scanner(System.in);
        System.out.println("Indique o NIF da empresa: ");
        String newNif = nif.nextLine();
        
        Scanner r = new Scanner(System.in);
        System.out.println("Indique o raio de ação da empresa: ");
        String newRaio = r.nextLine();
        
        Scanner pkm = new Scanner(System.in);
        System.out.println("Indique o custo por Km do transporte de mercadorias: ");
        String newPKM = pkm.nextLine();
        
        Scanner vel = new Scanner(System.in);
        System.out.println("Indique a velociade média de deslocamento: ");
        String newVel = vel.nextLine();
        
        
        Empresa emp = null;
        
        try{
            emp = new Empresa(newEmail,newPass,newName,coord,Integer.parseInt(newNif),Double.parseDouble(newRaio),Double.parseDouble(newPKM),0,0);
        }
        catch(NullPointerException erro){
            System.out.println("Erro  no preenchimento!");
            return null;
        }
        
        if(this.t.addUser(emp)==1){
            return newEmail;
        }
        else{
            return null;
        }
    }

    
    public void opsComprador(String email){
        boolean f = true;

        while(f){
            
            if(this.t.encomendasPorConfirmar(email)>0){
                System.out.println("(!) Tem " +this.t.encomendasPorConfirmar(email)+" encomenda(s) para autorizar o transporte!");
            }
            
            System.out.println("");
            Scanner optn = new Scanner(System.in);
            System.out.println("Selecione a operação que pretende efetuar:");
            System.out.println("1: Fazer uma encomenda");
            System.out.println("2: Autorizar o transporte de encomendas por empresas transportadoras");
            System.out.println("3: Listagem das minhas encomendas");
            System.out.println("4: Entregas feitas por um voluntário ou empresa num determinado periodo");
            System.out.println("5: Top 10 utilizadores que mais usam a aplicação para fazer compras");
            System.out.println("6: Top 10 empresas transportadoras por Kms percorridos");
            System.out.println("7: Classificar um voluntário ou empresa transportadora");
            System.out.println("0: Sair");
            int op = optn.nextInt();
            
            if(op==1){
                List<Loja> lojasdisp = this.t.lojasinSys();
                
                if(lojasdisp.size()==0){
                    System.out.println("Ainda não há lojas registadas no sistema.");
                    System.out.println("A aplicação vai encerrar.");
                    this.terminar();
                    return;
                }
               
                Scanner l = new Scanner(System.in);
                int i = 1;
                
                System.out.println("");
                System.out.println("Selecione uma das seguintes lojas disponíveis: ");
                
                for(Loja loja : lojasdisp){
                    System.out.println(i + " - " + loja.getName());
                    i++;
                }
                
                int o = l.nextInt();
                if(o<i){
                    Loja escolhida = (Loja) lojasdisp.get(o-1);

                    Encomenda e = new Encomenda();
                    e.setCEnc("e"+geraRandom());
                    e.setCUser(email);
                    e.setCLoja(escolhida.getEmail());
                    e.setEntregador("n/a");
                    e.setNDisp();
                
                    ArrayList<LinhaEncomenda> le = new ArrayList<>();
                    int sair = 1;
                
                    while(sair==1){

                        System.out.println("Descrição do produto: ");
                        Scanner desc = new Scanner(System.in);
                        String newName = desc.nextLine();
                        
                        System.out.println("Código do produto: ");
                        Scanner p = new Scanner(System.in);
                        String newCod = p.nextLine();
                        
                        System.out.println("Preço: ");
                        Scanner price = new Scanner(System.in);
                        String newPrice = price.nextLine();
                        
                        System.out.println("Que quantidade deseja, em Kgs?");
                        Scanner quant = new Scanner(System.in);
                        String newQuant = quant.nextLine();
                    
                        e.addLinhaEncomenda(new LinhaEncomenda(newCod,newName,Double.parseDouble(newQuant),Double.parseDouble(newPrice)));
                    
                        System.out.println("Deseja adicionar outro produto? 1: Sim / 0: Não");
                        Scanner opsair = new Scanner(System.in);
                        sair = opsair.nextInt();
                        
                        e.acrescentaPeso(Double.parseDouble(newQuant));
                    }
                    System.out.println("Aqui está a sua encomenda:");
                    System.out.println(e.toString());
                    this.t.addEncomenda(e);
                }
                else{
                    System.out.println("Opção inválida!");
                }
                
            }else if(op==2){

                if(this.t.encomendasPorConfirmar(email)==0){
                    System.out.println("Não existem encomendas para autorizar o transporte.");
                
                }else{
                    List<Encomenda> encPconfirm = this.t.encPconf(email);
                    Scanner l = new Scanner(System.in);
                    int j = 1;
                
                    System.out.println("Selecione a encomenda para a qual deseja autorizar transporte: ");
                
                    for(Encomenda e : encPconfirm){
                        System.out.println(j + " - " + e.toString());
                        j++;
                    }
                    System.out.println("0 - Cancelar operação");
                
                    int o2 = l.nextInt();
                    if(o2<j && o2>0){
                        Encomenda escolhida = (Encomenda) encPconfirm.get(o2-1);
                        String cod = escolhida.getCEnc();
                        
                        Empresa emp = (Empresa) this.t.getUser(escolhida.getEntregador());
                        
                        double custo = (emp.custoTransE(escolhida,this.t.getUser(escolhida.getCLoja()).getCoord(),this.t.getUser(escolhida.getCUser()).getCoord()))/100;
                    
                        System.out.println("O custo do transporte será " + custo +"€.");
                    
                        boolean f2 = true;
                    
                        while(f2){
                            System.out.println("Deseja autorizar o transporte? 1: Sim / 0: Não");
                            Scanner l2 = new Scanner(System.in);
                            int escolha = l2.nextInt();
                        
                            if(escolha==1){
                                this.t.mudaEstadoConfirmado(cod);
                                f2 = false;
                                System.out.println(this.t.getEncomenda(cod).toString());
                                System.out.println("Transporte da encomenda autorizado com sucesso.");
                            }
                        
                            else if(escolha==0){
                                this.t.mudaEstadoDis(cod);
                                f2 = false;
                                System.out.println(this.t.getEncomenda(cod).toString());
                                System.out.println("Transporte da encomenda cancelado com sucesso.");
                            }
                        
                            else {
                                System.out.println("Opção errada!");
                                System.out.println("");
                            }
                        }
                    }else if(o2==0){
                        System.out.println("Operação cancelada.");
                    }
                    
                    else{
                        System.out.println("Opção inválida!");
                    }
                }
                
                
            }else if(op==3){
                
                List<Encomenda> myenc = this.t.getEncomendasbyEmail(email);
                
                if(myenc.size()==0){
                    System.out.println("Sem encomendas realizadas.");
                }
                else{
                    int count = 1;
                    System.out.println("As suas encomendas:");
                    for(Encomenda e : myenc){
                        System.out.println(count + " - " + e.toString());
                    }
                }
                
                
            }else if(op==4){
                System.out.println("Indique o email da empresa ou voluntário: ");
                Scanner em = new Scanner(System.in);
                String mail = em.nextLine();
                
                this.consultaEntregas(mail);
            }
            else if(op==5){
                this.t.top10Users();
            }
            else if(op==6){
                this.t.top10Entregadores();
            }
            
            else if(op==7){
                this.t.classificar1entrega(email);
            }
            
            else if(op==0){
                f=false;
            }
            else{
                System.out.println("Opção inválida!");
            }
        }
        System.out.println("A aplicação vai encerrar.");
        this.terminar();
        return;
    }
    
    
    public void opsLoja(String email){
        boolean f = true;
        
        while(f){
            
            if(this.t.encomendasPorDisp(email)>0){
                System.out.println("(!) Existem encomendas para disponibilizar.");
            }
            
            Scanner optn = new Scanner(System.in);
            System.out.println("");
            System.out.println("Selecione a operação que pretende realizar: ");
            System.out.println("1: Disponiblizar um encomenda para ser transportada");
            System.out.println("2: Entregas feitas por um voluntário ou empresa num determinado período");
            System.out.println("3: Top 10 utilizadores que mais usam a aplicação para fazer compras");
            System.out.println("4: Top 10 empresas transportadoras por Kms percorridos");
            System.out.println("0: Sair");
            int op = optn.nextInt();
            
            
            if(op==1){

                if(this.t.encomendasPorDisp(email)==0){
                    System.out.println("Não existem encomendas para disponibilizar.");
                }
                else{
                    
                    List<Encomenda> encNdisp = this.t.encomendasNaoDisp(email);
                    Scanner l = new Scanner(System.in);
                    int i = 1;
                
                    System.out.println("Selecione uma das seguintes encomendas: ");
                
                    for(Encomenda e : encNdisp){
                        System.out.println(i + " - " + e.toString());
                        i++;
                    }
                    System.out.println("0 - Cancelar");
                    int o = l.nextInt();
                    if(o<i && o>0){
                        Encomenda escolhida = (Encomenda) encNdisp.get(o-1);
                        String cod = escolhida.getCEnc();
                        this.t.mudaEstadoDis(cod);
                    }
                    else if(o==0){
                        System.out.println("Operação cancelada.");
                    }
                    else{
                        System.out.println("Opção inválida!");
                    }
                }
            }
            
            else if(op==2){
                System.out.println("Indique o email da empresa ou voluntário: ");
                Scanner em = new Scanner(System.in);
                String mail = em.nextLine();
                
                this.consultaEntregas(mail);
            }
            
            else if(op==3){
                this.t.top10Users();
            }
            else if(op==4){
                this.t.top10Entregadores();
            }
            
            else if(op==0){
                f = false;
            }
            else{
                System.out.println("Opção inválida!");
            }
        
        }
        
        System.out.println("A aplicação vai encerrar.");
        this.terminar();
        return;
    }
    
    
    public void opsVoluntario(String email){
        boolean f = true;
        
        while(f){
            Scanner optn = new Scanner(System.in);
            System.out.println("");
            System.out.println("Selecione a operação que pretende efetuar:");
            System.out.println("1: Sinalizar que está pronto para recolher encomendas");
            System.out.println("2: Sinalizar que não está pronto para recolher encomendas");
            System.out.println("3: Ver encomendas disponíveis para levantamento");
            System.out.println("4: Registar entrega");
            System.out.println("5: Ver as encomendas que entreguei num dado período");
            System.out.println("6: Ver total faturado entre datas");
            System.out.println("7: Top 10 utilizadores que mais usam a aplicação para fazer compras");
            System.out.println("8: Top 10 empresas transportadoras por Kms percorridos");
            System.out.println("9: Classificação atual");
            System.out.println("0: Sair");
            int op = optn.nextInt();
            
            if(op==1){
                if(this.t.entregando(email)==true){
                    System.out.println("Impossível alterar estado. Finalize entrega atual!");
                }
                else{
                    if(this.t.estadoV(email)==true){
                        System.out.println("Já se encontra disponível!");
                    }
                    
                    else{
                        this.t.setDisponivel(email,true);
                        System.out.println("Estado alterado com sucesso.");
                    }
                }
            
                
            }else if(op==2){
                if(this.t.estadoV(email)==false){
                    System.out.println("Já se encontra não disponível!");
                }
                else{
                    this.t.setDisponivel(email,false);
                    System.out.println("Estado alterado com sucesso.");
                    }
            
                    
            }else if(op==3){
   
                if(this.t.entregando(email)==true){
                    System.out.println("Termine entrega atual para obter acesso!");
                
                }else if(this.t.estadoV(email)==false){
                        System.out.println("Não se encontra disponível! Acesso negado.");
                    
                    }else{
                
                        List<Loja> lojasalcance = this.t.dentrodoRaioV(email);
                        if(lojasalcance.size()==0){
                            System.out.println("Não existem lojas dentro do seu raio de operação.");
                            
                        }
                        else{
                            Scanner l = new Scanner(System.in);
                            int i = 1;
                            System.out.println("Selecione uma das seguintes lojas ao seu alcance: "); 
                
                            for(Loja loja : lojasalcance){
                                System.out.println(i + " - " + loja.getName());
                                i++;
                            }
                
                            int o = l.nextInt();
                
                            if(o<i){
                                Loja escolhida = (Loja) lojasalcance.get(o-1);
                                List<Encomenda> encdisp = this.t.encomendasDispAlcanceV(escolhida.getEmail(),email);
                    
                                if(encdisp.size()==0){
                                    System.out.println("Sem encomendas disponíveis para levantamento.");
                                    
                                }
                                else{
                                    Scanner l2 = new Scanner(System.in);
                                    int i2 = 1;
                
                                    System.out.println("Selecione uma das seguintes encomendas: ");
                
                                    for(Encomenda e : encdisp){
                                        System.out.println(i2 + " - " + e.toString());
                                        i2++;
                                    }
                                    System.out.println("0 - Cancelar");
                            
                                    int o2 = l.nextInt();
                                    if(o2==0){
                                        System.out.println("Operação cancelada.");
                                    }
                            
                                    else if(o2<=i2 && o2>0){
                                        Encomenda encEscolhida = (Encomenda) encdisp.get(o2-1);
                                        String cod = encEscolhida.getCEnc();
                                        this.t.mudaEstadoAceite(cod,email);
                                        this.t.setDisponivel(email,false);
                                        System.out.println("Parabéns! Está encarregue de entregar a encomenda nº: " + cod);
                                        System.out.println("Estado alterado para não disponível.");
                                    }
                                    else{
                                        System.out.println("Opção inválida!");
                                    }
                                }
                    
                            }
                            else{
                                System.out.println("Opção inválida!");
                            }
                        }
                    }
        
            }else if(op==4){
                if(this.t.entregando(email)==false){
                    System.out.println("Não está a realizar uma entrega de momento!");
                
                }else{
                    List<String> entregas = this.t.entregasAfazer(email);
                    System.out.println("Encomenda corrente: ");
                    
                    for(String s : entregas){
                        System.out.println("Cod. Encomenda: " + s); 
                    }
                    
                    System.out.println("Indique o código da encomenda: ");
                    Scanner c = new Scanner(System.in);
                    String cod = c.nextLine();
                    Encomenda enc = this.t.getEncomenda(cod);
                    if(enc==null){
                        System.out.println("Código inválido!!");
                    
                    
                    }else if(!enc.getEntregador().equals(email) || (enc.getEntregador().equals(email) && !enc.getState().equals("ACEITE"))){
                        System.out.println("Não está a entregar esta encomenda!");
                    
                    } else{

                        Entrega ent = this.t.fazEntregaV(enc,email);
                        
                        System.out.println("Deseja continuar com a operação? 1: Sim / 0: Não");
                        
                        Scanner optns = new Scanner(System.in);
                        int optn2 = optns.nextInt();;
                        
                        if(optn2==1){
                        
                            int flag = this.t.addEntrega(ent);
                            if(flag==1){
                                System.out.println(ent.toString());
                                System.out.println("Encomenda registada com sucesso.");
                                this.t.mudaEstadoEntregue(cod);
                            }else{
                                System.out.println("Erro ao registar encomenda!");
                            }
                            
                        }else if(optn2==0){
                            System.out.println("Operação cancelada.");
                        }
                        
                        else{
                            System.out.println("Opção inválida!");
                        }
                    } 
                }
                
            }else if(op==5){
                this.consultaEntregas(email);
            
            }else if(op==6){
                this.consultaLucro(email);
            }
            
            else if(op==7){
                this.t.top10Users();
            }
            
            else if(op==8){
                this.t.top10Entregadores();
            }
            
            else if(op==9){
                Voluntarios v = (Voluntarios) this.t.getUser(email);
                
                if(v.getReviews()==0){
                    System.out.println("Nenhuma entrega realizada. Classificação inexistente.");
                }
                else{
                    System.out.println("Classificação atual: " + v.getNota());
                }
            }
            
            else if(op==0){
                f = false;
                
            }else{
                System.out.println("Opção inválida!");
            }
        
        }
        
        System.out.println("A aplicação vai encerrar");
        this.terminar();
        return;
    }
    
    
    
    public void opsEmpresa(String email){
        boolean f = true;
        
        while(f){
            
            if(this.t.encomendasConfirm(email)>0){
                System.out.println("(!) Tem " +this.t.encomendasConfirm(email)+" encomenda(s) autorizadas para transporte.");
            }
            
            Scanner optn = new Scanner(System.in);
            System.out.println("");
            System.out.println("Selecione a operação que pretende realizar:");
            System.out.println("1: Ver encomendas disponíveis para levantamento");
            System.out.println("2: Levantar encomendas autorizadas");
            System.out.println("3: Registar entrega");
            System.out.println("4: Ver as encomendas que a empresa entregou num dado período");
            System.out.println("5: Ver total faturado entre datas");
            System.out.println("6: Top 10 utilizadores que mais usam a aplicação para fazer compras");
            System.out.println("7: Top 10 empresas transportadoras por Kms percorridos");
            System.out.println("8: Classificação atual");
            System.out.println("0: Log out");
            int op = optn.nextInt();
            
            if(op==1){
                List<Loja> lojasalcance = this.t.dentrodoRaioE(email);
                
                if(lojasalcance.size()==0){
                    System.out.println("Não existem lojas dentro do seu raio de operação.");
                    
                }
                else{
                    Scanner l = new Scanner(System.in);
                    int i = 1;
                    System.out.println("Selecione uma das seguintes lojas ao seu alcance: "); 
                
                    for(Loja loja : lojasalcance){
                        System.out.println(i + " - " + loja.getName());
                        i++;
                    }
                
                    int o = l.nextInt();
                
                    if(o<i){
                        Loja escolhida = (Loja) lojasalcance.get(o-1);
                        List<Encomenda> encdisp = this.t.encomendasDispAlcanceE(escolhida.getEmail(),email);
                    
                        if(encdisp.size()==0){
                            System.out.println("Sem encomendas para levantar.");
                            
                        }
                        else{
                            Scanner l2 = new Scanner(System.in);
                            int i2 = 1;
                
                            System.out.println("Selecione uma das seguintes encomendas: ");
                
                            for(Encomenda e : encdisp){
                                System.out.println(i2 + " - " + e.toString());
                                i2++;
                            }
                    
                            System.out.println("0 - Cancelar");
                            
                            int o2 = l.nextInt();
                            if(o2==0){
                                System.out.println("Operação cancelada.");
                                
                            }
                            
                            else if(o2<=i2){
                                Encomenda encEscolhida = (Encomenda) encdisp.get(o2-1);
                                String cod = encEscolhida.getCEnc();
                                this.t.mudaEstadoPconfirmar(cod,email);
                                System.out.println("A aguardar autorização de entrega por comprador.");
                            }
                            else{
                                System.out.println("Opção inválida!");
                            }
                        }
                    }
                    else{
                        System.out.println("Opção inválida!");
                    }
                }
            }
            
            else if(op==2){
                if(this.t.encomendasConfirm(email)==0){
                    System.out.println("Sem encomendas autorizadas para transporte.");
                }
                else{
                    List<Encomenda> confirmadas = this.t.encConf(email);
                    int i3 = 1;
                    
                    System.out.println("Selecione uma das seguintes encomendas: ");
                    
                    for(Encomenda e : confirmadas){
                            System.out.println(i3 + " - " + e.toString());
                            i3++;
                        }
                    
                    System.out.println("0 - Cancelar");
                    Scanner l3 = new Scanner(System.in);
                    int o3 = l3.nextInt();
                    if(o3==0){
                        System.out.println("Operação cancelada.");
                    }
                    
                    else if(o3<=i3){
                         Encomenda encEscolhida = (Encomenda) confirmadas.get(o3-1);
                         String cod = encEscolhida.getCEnc();
                         this.t.mudaEstadoAceite(cod,email);
                         System.out.println("Parabéns! Está encarregue de entregar a encomenda nº: " + cod);
                    }
                    else {
                        System.out.println("Opção inválida!");
                    }
                }
            }
            
            else if(op==3){
                if(this.t.entregando(email)==false){
                    System.out.println("Não está a realizar uma entrega de momento!");
                
                }else{
                    List<String> entregas = this.t.entregasAfazer(email);
                    System.out.println("Encomenda(s) que está a entregar: ");
                    
                    for(String s : entregas){
                        System.out.println("Cod. Encomenda - " + s); 
                    }
                    
                    System.out.println("Indique o código da encomenda: ");
                    
                    Scanner c = new Scanner(System.in);
                    String cod = c.nextLine();
                    Encomenda enc = this.t.getEncomenda(cod);
                    if(enc==null){
                        System.out.println("Encomenda inexistente!");
                        
                    }else if(!enc.getEntregador().equals(email) || (enc.getEntregador().equals(email) && !enc.getState().equals("ACEITE"))){
                        System.out.println("Não está a entregar esta encomenda!");
                    
                    } else{

                        Entrega ent = this.t.fazEntregaE(enc,email);

                        System.out.println("Deseja continuar com a operacao? 1: Sim / 0: Não");
                        
                        Scanner optns = new Scanner(System.in);
                        int optn2 = optns.nextInt();;
                        
                        if(optn2==1){
                        
                            int flag = this.t.addEntrega(ent);
                            if(flag==1){
                                System.out.println(ent.toString());
                                System.out.println("Encomenda registada com sucesso.");
                                this.t.mudaEstadoEntregue(cod);
                            }else{
                                System.out.println("Erro ao registar encomenda!");
                            }
                            
                        }else if(optn2==0){
                            System.out.println("Operação cancelada.");
                        }
                        
                        else{
                            System.out.println("Opção inválida!");
                        }
                    }
                }
            }
            else if(op==4){
                this.consultaEntregas(email);
            }
            
            else if(op==5){
                this.consultaLucro(email);
            }
            
            else if(op==6){
                this.t.top10Users();
            }
            else if(op==7){
                this.t.top10Entregadores();
            }
            
            else if(op==8){
                Empresa e = (Empresa) this.t.getUser(email);
                
                if(e.getReviews()==0){
                    System.out.println("Nenhuma entrega realizada. Classificação inexistente.");
                }
                else{
                    System.out.println("Classificação atual: " + e.getNota());
                }
            }
            else if(op==0){
                f = false;
            }
            
            else{
                System.out.println("Opção inválida!");
            }
        
        }
        
        System.out.println("A aplicação vai encerrar.");
        this.terminar();
        return;
    }
    
    
    public void consultaEntregas(String email){
        //Pedir Data Inicial
        LocalDate di = null;
        while(di==null){
            System.out.println("Data Inicial");
            Scanner d= new Scanner(System.in);
            System.out.print("Dia: ");
            int newDiaI = d.nextInt();
            Scanner me= new Scanner(System.in);
            System.out.print("Mês: ");
            int newMesI = me.nextInt();
            Scanner an= new Scanner(System.in);
            System.out.print("Ano: ");
            int newAnoI = an.nextInt();
            di = criaData(newDiaI,newMesI,newAnoI);
        }
              
        //Pedir Data Final
        LocalDate df = null;
        while(df==null){
            System.out.println("Data Final");
            Scanner d= new Scanner(System.in);
            System.out.print("Dia : ");
            int newDiaF = d.nextInt();
            Scanner me= new Scanner(System.in);
            System.out.print("Mês: ");
            int newMesF = me.nextInt();
            Scanner an= new Scanner(System.in);
            System.out.print("Ano: ");
            int newAnoF = an.nextInt();
            df = criaData(newDiaF,newMesF,newAnoF);
        }
        List<Entrega> e = new ArrayList<>();
        
        e = this.t.entregasPeriodo(email, di, df);
       
        if(e.size()==0){
            System.out.println("Entregas inexistentes no período indicado.");
        }
        else{
            System.out.println("Entregas realizadas no período indicado: ");
            int count = 1;
            for(Entrega ent : e){
                System.out.println(count + " - " + e.toString());
                count++;
                Encomenda enc = this.t.getEncomenda(ent.getIDenc());
                System.out.println(enc.toString());
            }
        }
    }
 
    public void consultaLucro(String email){
        //Pedir Data Inicial
        LocalDate di = null;
        while(di==null){
            System.out.println("Data Inicial");
            Scanner d= new Scanner(System.in);
            System.out.print("Dia: ");
            int newDiaI = d.nextInt();
            Scanner me= new Scanner(System.in);
            System.out.print("Mês: ");
            int newMesI = me.nextInt();
            Scanner an= new Scanner(System.in);
            System.out.print("Ano: ");
            int newAnoI = an.nextInt();
            di = criaData(newDiaI,newMesI,newAnoI);
        }
              
        //Pedir Data Final
        LocalDate df = null;
        while(df==null){
            System.out.println("Data Final");
            Scanner d= new Scanner(System.in);
            System.out.print("Dia: ");
            int newDiaF = d.nextInt();
            Scanner me= new Scanner(System.in);
            System.out.print("Mês: ");
            int newMesF = me.nextInt();
            Scanner an= new Scanner(System.in);
            System.out.print("Ano: ");
            int newAnoF = an.nextInt();
            df = criaData(newDiaF,newMesF,newAnoF);
        }

        double l = this.t.lucroPeriodo(email, di, df);
        
        if(l==0){
            System.out.println("Não houve faturação neste período.");
        }
        else{
            System.out.println("Faturado: " + l);
        }
    }
    
    
    public static int geraRandom(){
        Random rand = new Random();
        int n = rand.nextInt(Integer.MAX_VALUE);
        return n;
    }
    
    public static LocalDate criaData(int dia,int mes, int ano){
            try{
                LocalDate nascimento = LocalDate.of(ano,mes,dia);
                return nascimento;
            }catch(DateTimeException e ){
                System.out.println("Data incorreta");
                return null;
            }
        }
}