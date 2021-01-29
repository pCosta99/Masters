
/**
 * Classe Main
 * 
 * @author (João Barbosa a82044)
 * @author (Nuno Morais ae5220)
 * @author (Rui Neto a80433)
 * @version (23/04/2020)
 */
import java.util.Scanner;
import java.util.Iterator;
import java.util.Map;
import java.util.ArrayList;
import java.util.Set;
import java.time.Duration;
import java.util.HashSet;
import java.util.List;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.io.Serializable;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
public class Main implements Serializable
{
    /**
     * Funcao principal
     */
    public static void main(String[] args) throws Exception {
        Infos i = new Infos();
        Parser p = new Parser();
        try {
            i = Infos.carregaObjetoInfos();
        }
        catch (Exception e) {
            System.out.println("Erro a carregar ou Base de Dados inexistente");
        }
        try{
            i = p.parse(i);
        }
        catch (Exception e){
            System.out.println("Erro no parse");
        }
        executa(i);
        try {
            i.gravaObjetoInfos();
        } catch (Exception e) {
            System.out.println("Erro a guardar");
            return;
        }
    }
    
    private static void executa(Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        int x = -1;
        limparEcra();
        Menu.menuPrincipal();
        while(x!=0 && x!=1 && x!=2){
            x = input.nextInt();
        }
        limparEcra();
        switch(x){
            case(1):
                menuEntrar(i);
                break;
            case(2):
                registarEntidade(i);
                break;
            case(0):
                break;
        }
    }

    /**
     * Funcao responsavel pelo menu de entrada
     *
     * @param  i   Estado atual da informacao
     */
    public static void menuEntrar (Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        int x = -1;
        limparEcra();
        Menu.menuEntrar();
        while(x!=0 && x!=1 && x!=2 && x!=3 && x!=4 && x!=5){
            x = input.nextInt();
        }
        limparEcra();
        switch(x){
            case(1):
                try{
                    entrarUtilizador(i);
                }
                catch(Exception e){
                    System.out.println("Email ou Password errados");
                    entrarUtilizador(i);
                }
                break;
            case(2):
                try{
                    entrarVoluntario(i);
                }
                catch(Exception e){
                    System.out.println("Email ou Password errados");
                    entrarVoluntario(i);
                }
                break;
            case(3):
                try{
                    entrarEmpresaTransportadora(i);
                }
                catch(Exception e){
                    System.out.println("Email ou Password errados");
                    entrarEmpresaTransportadora(i);
                }
                break;
            case(4):
                try{
                    entrarLoja(i);
                }
                catch(Exception e){
                    System.out.println("Email ou Password errados");
                    entrarLoja(i);
                }
                break;
            case(5):
                try{
                    entrarAdministrador(i);
                }
                catch(Exception e){
                    System.out.println("Password errada");
                    entrarAdministrador(i);
                }
                break;
            case(0):
                limparEcra();
                executa(i);
                break;
        }
    }

    /**
     * Funcao responsavel pela entrada do utilizador na aplicacao
     *
     * @param  i   Estado atual da Informacao
     */
    private static void entrarUtilizador (Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        System.out.println("Email (0 para voltar): ");
        String x = "";
        Utilizador u = null;
        x = input.next();
        if(x.equals("0")){
            menuEntrar(i);
            return;
        }
        String xfinal = x.split("@")[0];
        try{
            String mail = x.split("@")[1];
            if (!mail.equals("gmail.com")) throw new Exception("Email incorreto");
        }catch(Exception e){
            System.out.println("Utilizador inexistente");
            entrarUtilizador(i);
            return;
        }
        u = i.getUtilizador(xfinal);
        System.out.println("Password: ");
        String y = "";
        y = input.next();
        int j = 0;
        while(j<3){
            if (!u.getPassword().equals(y)){
                System.out.println("Email ou password errados");
                System.out.println("\n");
                System.out.println("Email: ");
                x = input.next();
                System.out.println("Password: ");
                y = input.next();
            }
            j+=1;
        }
        if (j==3 && !(u.getPassword().equals(y))) menuEntrar(i);
        else{
            limparEcra();
            opcoesUtilizador((Utilizador)u,i);
        }
    }

    /**
     * Funcao responsavel pelo menu de utilizadores
     *
     * @param  u   Objecto tipo Utilizador
     * @param  i    Estado atual da Informacao
     */
    private static void opcoesUtilizador (Utilizador u, Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        int x=-1;
        Menu.menuOpcoesUtilizador();
        while(x!=0 && x!=1 && x!=2 && x!=3 && x!=4 && x!=5 && x!=6){
            x = input.nextInt();
        }
        limparEcra();
        switch(x){
            case(1):
                Encomenda e = new Encomenda();
                List<LinhaEncomenda> les = new ArrayList<>();
                LinhaEncomenda le = new LinhaEncomenda();
                System.out.println("Codigo da Encomenda: ");
                String codEnc = input.next();
                while (i.getEncomendas().containsKey(codEnc)){
                    System.out.println("Encomenda ja existe");
                    codEnc = input.next();
                }
                e.setCodEnc(codEnc);
                System.out.println("Codigo do Utilizador: "+u.getCodU());
                e.setCodU(u.getCodU());
                System.out.println("Codigo da Loja: ");
                String codL = input.next();
                while(!i.getLojas().containsKey(codL)){
                    System.out.println("Loja inexistente");
                    codL = input.next();
                }
                e.setCodL(codL);
                System.out.println("Peso da Encomenda: ");
                e.setPeso(input.nextDouble());
                System.out.println("Codigo do Produto: ");
                le.setCodP(input.next());
                System.out.println("Descricao do Produto: ");
                le.setDescr(input.next());
                System.out.println("Quantidade: ");
                le.setQt(input.nextDouble());
                System.out.println("Valor Unitario: ");
                le.setValorU(input.nextDouble());
                les.add(le.clone());
                System.out.println("Quer comprar mais produtos?\n1 - Sim\n0 - Nao");
                int opcaoLE = input.nextInt();
                while (opcaoLE == 1){
                    le = new LinhaEncomenda();
                    System.out.println("Codigo do Produto: ");
                    le.setCodP(input.next());
                    System.out.println("Descricao do Produto: ");
                    le.setDescr(input.next());
                    System.out.println("Quantidade: ");
                    le.setQt(input.nextDouble());
                    System.out.println("Valor Unitario: ");
                    le.setValorU(input.nextDouble());
                    les.add(le.clone());
                    System.out.println("Quer acrescentar mais produtos?\n1 - Sim\n0 - Nao");
                    opcaoLE = input.nextInt();
                }
                e.setLe(les);
                i.addEncomenda(e);
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesUtilizador(u, i);
                break;
            case(2):
                System.out.println("Codigo da Encomenda: ");
                String y = input.next();
                System.out.println(i.getEncomendas().get(y).getClassificacao());
                System.out.println("Classificacao: ");
                double z = input.nextDouble();
                i.setClass(y, z);
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesUtilizador(u,i);
                break;
            case(3):
                System.out.println("Codigo da Transportadora: ");
                String a = input.next();
                List<Encomenda> encs1 = new ArrayList<>();
                for (Encomenda enc : i.getEncomendasTransportadora(a)){
                    encs1.add(enc.clone());
                }
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesUtilizador(u,i);
                break;
            case(4):
                System.out.println("Codigo do Voluntario: ");
                String b = input.next();
                List<Encomenda> encs2 = new ArrayList<>();
                for (Encomenda enc : i.getEncomendasVoluntario(b)){
                    encs2.add(enc.clone());
                }
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesUtilizador(u,i);
                break;
            case(5):
                for (Encomenda enc : i.getEncomendas().values()){
                    if (enc.getCodU().equals(u.getCodU())){
                        System.out.println(enc.toString()+"\n");
                    }
                }System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesUtilizador(u,i);
                break;
            case(6):
                System.out.println("Codigo da Encomenda a aceitar: ");
                String s = input.next();
                i.aceitaEncomenda(s, u.getCodU());
                opcoesUtilizador(u, i);
                break;
            case(0):
                executa(i);
                break;
        }
    }

    /**
     * Funcao responsavel pela entrada do voluntario na aplicacao
     *
     * @param  i    Estado atual da Informacao
     */
    private static void entrarVoluntario (Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        System.out.println("Email (0 para voltar): ");
        String x = "";
        Voluntario v = null;
        x = input.next();
        if(x.equals("0")){
            menuEntrar(i);
            return;
        }
        String xfinal = x.split("@")[0];
        try{
            String mail = x.split("@")[1];
            if (!mail.equals("hotmail.com")) throw new Exception("Email incorreto");
        }catch(Exception e){
            System.out.println("Voluntario inexistente");
            entrarVoluntario(i);
            return;
        }
        v = i.getVoluntario(xfinal);
        System.out.println("Password: ");
        String y = "";
        y = input.next();
        int j = 0;
        while(!(v.getPassword().equals(y)) && j<3){
            j+=1;
            System.out.println("Email ou password errados");
            System.out.println("\n");
            System.out.println("Email: ");
            x = input.next();
            System.out.println("Password: ");
            y = input.next();
        }
        if (j==3 && !(v.getPassword().equals(y))) menuEntrar(i);
        else{
            limparEcra();
            opcoesVoluntario((Voluntario)v,i);
        }
    }
    
    /**
     * Funcao responsavel pelo menu de voluntarios
     *
     * @param  v   Objecto tipo Voluntario
     * @param  i    Estado atual da Informacao
     */
    private static void opcoesVoluntario (Voluntario v, Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        int x=-1;
        Menu.menuOpcoesVoluntario();
        while(x!=0 && x!=1 && x!=2){
            x = input.nextInt();
        }
        limparEcra();
        switch(x){
            case(1):
                LocalDateTime ti2 = null;
                System.out.print("Tempo inicial (yyyy-MM-dd HH:mmy): ");
                input.nextLine();
                boolean f1 = true;
                do{
                    try{
                        String str1 = input.nextLine();
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                        ti2 = LocalDateTime.parse(str1, formatter);
                        f1=false;
                    }
                    catch(Exception e){System.out.print("Data invalida. (yyyy-MM-dd HH:mm)\nData (yyyy-MM-dd HH:mm): ");}
                }while(f1);
                System.out.print("Tempo final (yyyy-MM-dd HH:mmy): ");
                LocalDateTime tf2 = null;
                boolean f2 = true;
                do{
                    try{
                        String str2 = input.nextLine();
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                        tf2 = LocalDateTime.parse(str2, formatter);
                        f2=false;
                    }
                    catch(Exception e){System.out.print("Data invalida. (yyyy-MM-dd HH:mm)\nData (yyyy-MM-dd HH:mm): ");}
                }while(f2);
                for (Encomenda e : i.encsEntreDatas(ti2,tf2)){
                    System.out.println(e.toString());
                }
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesVoluntario(v,i);
                break;
            case(2):
                v.setEstado(true);
                i.addVoluntarioDisponivel(v);
                opcoesVoluntario(v,i);
                break;
            case(0):
                executa(i);
                break;
        }
    }
    
    /**
     * Funcao responsavel pela entrada da empresa transportadora na aplicacao
     *
     * @param  i    Estado atual da Informacao
     */
    private static void entrarEmpresaTransportadora (Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        System.out.println("Email (0 para voltar): ");
        String x = "";
        Transportadora t = null;
        x = input.next();
        if(x.equals("0")){
            menuEntrar(i);
            return;
        }
        String xfinal = x.split("@")[0];
        try{
            String mail = x.split("@")[1];
            if (!mail.equals("empresa.com")) throw new Exception("Email incorreto");
        }catch(Exception e){
            System.out.println("Transportadora inexistente");
            entrarEmpresaTransportadora(i);
            return;
        }
        t = i.getTransportadora(xfinal);
        System.out.println("Password: ");
        String y = "";
        y = input.next();
        int j = 0;
        while(!(t.getPassword().equals(y)) && j<3){
            j+=1;
            System.out.println("Email ou password errados");
            System.out.println("\n");
            System.out.println("Email: ");
            x = input.next();
            System.out.println("Password: ");
            y = input.next();
        }
        if (j==3 && !(t.getPassword().equals(y))) menuEntrar(i);
        else{
            limparEcra();
            opcoesEmpresaTransportadora((Transportadora)t,i);
        }
    }
    
    /**
     * Funcao responsavel pelo menu de empresas transportadoras
     *
     * @param  t   Objecto tipo Transportadora
     * @param  i    Estado atual da Informacao
     */
    private static void opcoesEmpresaTransportadora (Transportadora t, Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        int x=-1;
        Menu.menuOpcoesEmpresaTransportadora();
        while(x!=0 && x!=1 && x!=2 && x!=3){
            x = input.nextInt();
        }
        limparEcra();
        switch(x){
            case(1):
                LocalDateTime ti2 = null;
                System.out.print("Tempo inicial (yyyy-MM-dd HH:mmy): ");
                input.nextLine();
                boolean f1 = true;
                do{
                    try{
                        String str1 = input.nextLine();
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                        ti2 = LocalDateTime.parse(str1, formatter);
                        f1=false;
                    }
                    catch(Exception e){System.out.print("Data invalida. (yyyy-MM-dd HH:mm)\nData (yyyy-MM-dd HH:mm): ");}
                }while(f1);
                System.out.print("Tempo final (yyyy-MM-dd HH:mmy): ");
                LocalDateTime tf2 = null;
                boolean f2 = true;
                do{
                    try{
                        String str2 = input.nextLine();
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                        tf2 = LocalDateTime.parse(str2, formatter);
                        f2=false;
                    }
                    catch(Exception e){System.out.print("Data invalida. (yyyy-MM-dd HH:mm)\nData (yyyy-MM-dd HH:mm): ");}
                }while(f2);
                for (Encomenda e : i.encsEntreDatas(ti2,tf2)){
                    System.out.println(e.toString());
                }
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesEmpresaTransportadora(t,i);
                break;
            case(2):
                LocalDateTime ti1 = null;
                System.out.print("Tempo inicial (yyyy-MM-dd HH:mmy): ");
                input.nextLine();
                boolean ff1 = true;
                do{
                    try{
                        String str1 = input.nextLine();
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                        ti1 = LocalDateTime.parse(str1, formatter);
                        ff1=false;
                    }
                    catch(Exception e){System.out.print("Data invalida. (yyyy-MM-dd HH:mm)\nData (yyyy-MM-dd HH:mm): ");}
                }while(ff1);
                System.out.print("Tempo final (yyyy-MM-dd HH:mmy): ");
                LocalDateTime tf1 = null;
                boolean ff2 = true;
                do{
                    try{
                        String str2 = input.nextLine();
                        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm");
                        tf1 = LocalDateTime.parse(str2, formatter);
                        ff2=false;
                    }
                    catch(Exception e){System.out.print("Data invalida. (yyyy-MM-dd HH:mm)\nData (yyyy-MM-dd HH:mm): ");}
                }while(ff2);
                System.out.println(i.totalFaturado(t,ti1,tf1));
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesEmpresaTransportadora(t,i);
                break;
            case(3):
                t.setEstado(true);
                i.addTransportadoraDisponivel(t);
                opcoesEmpresaTransportadora(t,i);
                break;
            case(0):
                executa(i);
                break;
        }
    }
    
    /**
     * Funcao responsavel pela entrada da loja na aplicacao
     *
     * @param  i    Estado atual da Informacao
     */
    private static void entrarLoja (Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        System.out.println("Email (0 para voltar): ");
        String x = "";
        Loja l = null;
        x = input.next();
        if(x.equals("0")){
            menuEntrar(i);
            return;
        }
        String xfinal = x.split("@")[0];
        try{
            String mail = x.split("@")[1];
            if (!mail.equals("loja.com")) throw new Exception("Email incorreto");
        }catch(Exception e){
            System.out.println("Utilizador inexistente");
            entrarLoja(i);
            return;
        }
        l = i.getLoja(xfinal);
        System.out.println("Password: ");
        String y = "";
        y = input.next();
        int j = 0;
        while(!(l.getPassword().equals(y)) && j<3){
            j+=1;
            System.out.println("Email ou password errados");
            System.out.println("\n");
            System.out.println("Email: ");
            x = input.next();
            System.out.println("Password: ");
            y = input.next();
        }
        if (j==3 && !(l.getPassword().equals(y))) menuEntrar(i);
        else{
            limparEcra();
            opcoesLoja((Loja)l,i);
        }
    }
    
    /**
     * Funcao responsavel pelo menu de lojas
     *
     * @param  l   Objecto tipo Lojas
     * @param  i    Estado atual da Informacao
     */
    private static void opcoesLoja (Loja l, Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        int x=-1;
        Menu.menuOpcoesLoja();
        while(x!=0 && x!=1 && x!=2){
            x = input.nextInt();
        }
        limparEcra();
        switch(x){
            case(1):
                for (Encomenda e: i.getEncomendasProntas(l.getCodL())){
                    System.out.println(e);
                }
                System.out.println("\n\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                opcoesLoja(l,i);
                break;
            case(2):
                System.out.println("Codigo da Encomenda: ");
                String cod = input.next();
                for (Encomenda e : i.encomendasLoja(l.getCodL()).values()){
                    if (e.getCodEnc().equals(cod)){
                        i.assocEncomendaTransporte(e);
                    }
                }
                opcoesLoja(l,i);
                break;
            case(0):
                executa(i);
                break;
        }
    }
    
    /**
     * Funcao responsavel pelo menu principal de admin
     *
     * @param  i    Estado atual da Informacao
     */
    private static void entrarAdministrador (Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        System.out.println("Password: ");
        String y="";
        y = input.next();
        while(!y.equals("12345")){
            System.out.println("Password: ");
            y=input.next();
        }
        limparEcra();
        menuOpcoesAdministrador(i);
    }

    /**
     * Funcao responsavel pelas opcoes do admin
     */
    private static void menuOpcoesAdministrador(Infos i) throws Exception{
        int x=-1;
        Scanner input = new Scanner(System.in);
        Menu.menuOpcoesAdministrador();
        while(x!=0 && x!=1 && x!=2 && x!=3 && x!=4 && x!=5 && x!=6 && x!=7 && x!=8 && x!=9 && x!=10 && x!=11 && x!=12 && x!=13 && x!=14 && x!=15 && x!=16 && x!=17 && x!=18){
            x = input.nextInt();
        }
        limparEcra();
        switch(x){
            case(1):
                for(Transportadora t : i.transportadorasMaisFreqs()){
                    System.out.println(t.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(2):
                for(Utilizador u : i.utilizadoresMaisFreqs()){
                    System.out.println(u.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(3):
                System.out.println("Codigo da Encomenda: ");
                String a = "";
                a = input.next();
                System.out.println(i.adminClass(a));
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(4):
                for (Utilizador u : i.getUtilizadores().values()){
                    System.out.println(u.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(5):
                for (Loja l : i.getLojas().values()){
                    System.out.println(l.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(6):
                for (Voluntario v : i.getVoluntarios().values()){
                    System.out.println(v.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(7):
                for (Transportadora t : i.getTransportadoras().values()){
                    System.out.println(t.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(8):
                for (Encomenda e : i.getEncomendas().values()){
                    System.out.println(e.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(9):
                for (Encomenda e : i.getEncomendasProntas().values()){
                    System.out.println(e.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(10):
                for (EncomendasAceites ea : i.getEncomendasAceites()){
                    System.out.println(ea.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(11):
                System.out.println("Codigo Empresa Transportadora: ");
                String trans = input.next();
                for (Encomenda e: i.getEncomendasTransportadora(trans)){
                    System.out.println(e.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(12):
                System.out.println("Codigo Voluntario: ");
                String volunt = input.next();
                for (Encomenda e: i.getEncomendasVoluntario(volunt)){
                    System.out.println(e.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(13):
                for (Voluntario v: i.getVoluntariosDisponiveis()){
                    System.out.println(v.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(14):
                for (Transportadora t: i.getTransportadorasDisponiveis()){
                    System.out.println(t.toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(15):
                for (String s: i.getTransportadorasIndisponiveis()){
                    for (Transportadora t : i.getTransportadoras().values()){
                        if (t.getCodE().equals(s)){
                            System.out.println(t.toString());
                        }
                    }
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(16):
                Iterator<Map.Entry<String,LocalDateTime>> it = i.getEncomendasData().entrySet().iterator();
                while (it.hasNext()){
                    Map.Entry<String, LocalDateTime> stdate = it.next();
                    System.out.println(stdate.getKey() + " " + stdate.getValue().toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(17):
                Iterator<Map.Entry<String,Duration>> it2 = i.getEncomendasDuracao().entrySet().iterator();
                while (it2.hasNext()){
                    Map.Entry<String,Duration> b = it2.next();
                    System.out.println(b.getKey() + " " + b.getValue().toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(18):
                Iterator<Map.Entry<String,List<String>>> it3 = i.getEncsPorAceitarU().entrySet().iterator();
                while (it3.hasNext()){
                    Map.Entry<String,List<String>> c = it3.next();
                    System.out.println(c.getKey() + " " + c.getValue().toString());
                }
                System.out.println("\n0 - Voltar");
                x = input.nextInt();
                while(x!=0){
                    x = input.nextInt();
                }
                limparEcra();
                menuOpcoesAdministrador(i);
                break;
            case(0):
                executa(i);
                break;
        }
    }

    /**
     * Funcao para registo de utilizadores, voluntarios, empresas transportadoras e lojas
     *
     * @param  i   Estado atual da Informacao
     */
    private static void registarEntidade (Infos i) throws Exception{
        Scanner input = new Scanner(System.in);
        limparEcra();
        Menu.menuRegistarEntidade();
        int x = input.nextInt();
        if(x==0){
            executa(i);
        }
        if(x==1){
            limparEcra();
            System.out.println("Codigo do Utilizador: ");
            String codUtilizador = input.next();
            System.out.println("Nome: ");
            String nomeUtilizador = input.next();
            System.out.println("Coordenada X: " );
            double coordX = input.nextDouble();
            System.out.println("Coordenada Y: ");
            double coordY = input.nextDouble();
            Utilizador u = new Utilizador(codUtilizador,nomeUtilizador,coordX,coordY);
            try{
                i.addUtilizador(u);
            }
            catch (Exception e){
                Menu.menuEntrar();
            }
        }
        if (x==2){
            limparEcra();
            System.out.println("Codigo do Voluntario: ");
            String codVoluntario = input.next();
            System.out.println("Nome: ");
            String nomeVoluntario = input.next();
            System.out.println("Coordenada X: ");
            double coordX= input.nextDouble();
            System.out.println("Coordenada Y: ");
            double coordY = input.nextDouble();
            System.out.println("Raio: ");
            double raio = input.nextDouble();
            Voluntario v = new Voluntario(codVoluntario,nomeVoluntario,coordX,coordY,raio);
            try{
                i.addVoluntario(v);
            }
            catch (Exception e){
                Menu.menuEntrar();
            }
        }
        if (x==3){
            limparEcra();
            System.out.println("Codigo da Empresa Transportadora: ");
            String codEmpresa = input.next();
            System.out.println("Nome: ");
            String nomeEmpresa = input.next();
            System.out.println("Coordenada X: ");
            double coordX= input.nextDouble();
            System.out.println("Coordenada Y: ");
            double coordY = input.nextDouble();
            System.out.println("NIF: ");
            long nif = input.nextLong();
            System.out.println("Raio: ");
            double raio = input.nextDouble();
            System.out.println("PrecoKm: ");
            double precoKm = input.nextDouble();
            Transportadora t = new Transportadora(codEmpresa,nomeEmpresa,coordX,coordY,nif,raio,precoKm);
            try{
                i.addTransportadora(t);
            }
            catch (Exception e){
                Menu.menuEntrar();
            }
        }
        if(x==4){
            limparEcra();
            System.out.println("Codigo da Loja: ");
            String codLoja = input.next();
            System.out.println("Nome: ");
            String nomeLoja = input.next();
            System.out.println("Coordenada X:" );
            double coordX = input.nextDouble();
            System.out.println("Coordenada Y: ");
            double coordY = input.nextDouble();
            Loja l = new Loja(codLoja,nomeLoja,coordX,coordY);
            try{
                i.addLoja(l);
            }
            catch (Exception e){
                Menu.menuEntrar();
            }
        }
        executa(i);
    }

    /**
     * Funcao que limpaa o ecra
     */
    private static void limparEcra(){
        System.out.print('\f');
    }
}
