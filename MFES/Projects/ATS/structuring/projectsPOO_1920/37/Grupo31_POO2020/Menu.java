import java.util.List;
import java.util.Scanner;
import java.util.Map.Entry;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.io.*;

public class Menu extends TrazAquiController implements Serializable{
    // apenas um scanner para todas as operações
    static final Scanner in = new Scanner(System.in);
    
    //mesmo numero para voltar para tras
    
    //construtor vazio (é sempre chamado pelos outros menus, visto que é uma superclasse dos outros menus)
    public Menu(){}

    // menu para a aplicação
    public Menu(Object o){ 
        System.out.println("-----TRABALHO PRÁTICO P.O.O. 2019/2020-----");
        System.out.println("\n-----------------TRAZ AQUI-----------------");
        // leitura logs
        leituraFicheiro(super.getGT());
        // escolha de menu
        this.menuInicial(super.getGT());
    }

    
    // método de leitura de ficheiro logs
    public void leituraFicheiro(GestaoTotal gt){
        String s;
        LeituraLogs logs = new LeituraLogs();
        System.out.println("\n> Deseja ler algum ficheiro de logs? [0] Não, [1] Sim: ");
        s = in.nextLine();
        switch(s){
            case "0": break;
            case "1": 
                // ler uma String e passar ao parse
                System.out.println("--------------------------------------");
                System.out.println("Introduza o nome do ficheiro de logs: ");
                System.out.println("--------------------------------------");
                s = in.nextLine();
                logs.parse(s,gt);
                break;
            default: break;
        }
    }

    // menu para selecionar qual secção
    public void menuInicial(GestaoTotal gt){
        String s;
        System.out.println("--------------------------------------");
        System.out.println("Selecione a secção: ");
        System.out.println("\n[0] Sair da Aplicação");
        System.out.println("\n[1] Secção de Gestão");
        System.out.println("\n[2] Secção de Utilizador");
        System.out.println("\n[3] Tops 10 ");
        System.out.println("\n[4] Carregar/Guardar Estado");
        s = in.nextLine();
        
        switch(s){
            // acede e cria um menu para gestão
            case "0": System.exit(0);
            case "1": MenuGestao mg = new MenuGestao();
                        mg.menuInicial(gt); break;
            // acede e cria um menu para Utilizador
            case "2": MenuUtilizador mu = new MenuUtilizador(); 
                        mu.menuInicial(gt); break;
            case "3": menuTops10(gt); break;
            case "4" : guardarCarregarEstado(gt);break;
            
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); break;
        }
        this.menuInicial(gt);
    }

    public void guardarCarregarEstado(GestaoTotal gt){
        String s;
        System.out.println("--------------------------------------");
        System.out.println("\n[0] Voltar ao Menu Inicial ");
        System.out.println("\n[1] Guardar Estado");
        System.out.println("\n[2] Carregar Estado");
        System.out.println("\n[3] Guardar estado a cada 10 segundos");
        s = in.nextLine();
        switch (s){
            case "0" : this.menuInicial(gt); break;
            case "1" :
                System.out.println("****Gravar em binário***");
                try{ super.guardaEstado();}
                catch(FileNotFoundException e){
                    System.out.println("Não foi possivel abrir o ficheiro");
                }   
                catch(IOException e){
                    System.out.println("Não foi possivel gravar o ficheiro");
                }
                break; 
               
               
            case "2" : 
                System.out.println("****Ler em binário***");
                try{
                    super.carregaEstado();
                }
                catch(FileNotFoundException e){
                    System.out.println("Não foi possivel abrir o ficheiro");
                }
        
                catch(IOException e){
                    System.out.println("Não foi possivel gravar o ficheiro");
                }  
                catch(ClassNotFoundException e){
                    System.out.println("Não foi possivel encontrar a classe correcta no ficheiro");
                } 
            break;
            case "3":
                guardarEstadoAutomatico();
                System.out.println("Guardar estado iniciou. Primeira gravação dentro de 10 segundos");
                guardarCarregarEstado(gt);
                break;
            
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); guardarCarregarEstado(gt);break;
        }
    }
    // buscar apontador da GestaoTotal
    public GestaoTotal getGT(){
        return super.gt;
    }
    
    
    public void menuTops10(GestaoTotal gt){
        String s;
        System.out.println("--------------------------------------");
        System.out.println("Selecione a secção: ");
        System.out.println("\n[0] Voltar ao Menu Inicial ");
        System.out.println("\n[1] Top 10 Empresas");
        System.out.println("\n[2] Top 10 Utilizadores");
        
        s = in.nextLine();
        
        switch(s){
            case "0": this.menuInicial(gt); break;
            case "1": printTop10Empresas(gt); menuTops10(gt); break;
            case "2": printTop10Utilizadores(gt); menuTops10(gt); break;
            
            default: System.out.printf("\n !!!! Input inválido. Tente novamente.\n"); menuTops10(gt); break;
            
        }
    }
    // print do top 10 das Empresas
    public void printTop10Empresas(GestaoTotal gt){
        StringBuilder sb = new StringBuilder();
        for (EmpresaTransportadora et : gt.getGEMP().top10Emp()){
            sb.append ("\nEmpresa Transportadora: ").append(et.getNomeEmpresa()).append(", ");
            sb.append ("Código: ").append(et.getCodEmp()).append(", ");
            sb.append ("Número de viagens: ").append(et.getNumViagens()).append(", ");
            sb.append ("Número de kilómetros: ").append(et.getNumKms());
        }
        System.out.println(sb.toString());
    }

    // print do top 10 dos utilizadores que fizeram encomendas
    public void printTop10Utilizadores(GestaoTotal gt){
        StringBuilder sb = new StringBuilder();
        for(Entry<String, Integer> par : gt.getGE().top10Utilizadores(gt)) {
            sb.append("\nNome do utilizador: "+gt.getGU().getUtilizador(par.getKey()).getNomeUt()).append(", ");
            sb.append("Código de utilizador: "+par.getKey()).append(", ");
            sb.append("Número de encomendas: "+par.getValue());
        }
        System.out.println(sb.toString());
    }
    

    // guardar estado automaticamente a cada 10segs (cria um processo paralelo em background)
    private void guardarEstadoAutomatico(){
       // criar um processo no background que vai correr um processo que guarda o estado a cada 10 segundos
       ScheduledExecutorService background = Executors.newScheduledThreadPool(1);

       Runnable task1 = () -> {
           try {
               super.guardaEstado();
           } catch (FileNotFoundException e) {
               e.printStackTrace();
           } catch (IOException e) {
               e.printStackTrace();
           }
           //System.out.println("~~~~~~Guardou estado~~~~~~");
       };
       // delay inicial: 10 segundos, repete a task1 a cada 10 segundos
       ScheduledFuture<?> scheduledFuture = background.scheduleAtFixedRate(task1, 10, 10, TimeUnit.SECONDS);
       //scheduledFuture.cancel(true);
       //ses.shutdown();
   }
   
    
}