import java.util.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.io.*;
import java.lang.String;
import java.time.LocalDate;
import java.time.DateTimeException;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Date;
import java.awt.Desktop;
import java.time.LocalDate;
import java.util.ArrayList;
import java.lang.Double;

/**
 * Classe TrazAqui :: Classe main da aplicação.
 */
public class TrazAqui implements Serializable
{
    private Map<String,Utilizador> users;            //email ->  user
    private Map<String,Encomenda> encomendas;     //id da encomenda -> encomenda
    private Map<String,Entrega> entregas;        //id da encomenda -> entrega

    public TrazAqui(){
        this.users = new HashMap<String,Utilizador>();
        this.encomendas = new HashMap<String,Encomenda>();
        this.entregas = new HashMap<String,Entrega>();    
    }
    
    
    /**
     * Métodos que vão auxiliar na execução das opções selecionadas pelos users.
     */
    
    
    public Utilizador getUser(String email){
        Utilizador u = null;
        u = this.users.get(email);
        return u.clone();
    }
       
    public Encomenda getEncomenda(String id){
        Encomenda e = null;
        e = this.encomendas.get(id);
        return e.clone();
    }
    
    public List<Encomenda> getEncomendasbyEmail(String email){
        List<Encomenda> r = new ArrayList<>();
        
        for(Encomenda e : this.encomendas.values()){
            if(e.getCUser().equals(email)){
                r.add(e.clone());
            }
        }
        return r;
    }
    
    public void addEncomenda(Encomenda e){
        if(this.encomendas.containsValue(e)) return;
        else{
           Encomenda r = e.clone();
           this.encomendas.put(r.getCEnc(),r);
        }
    }
    
    public int addUser(Utilizador u){
        if(this.users.containsValue(u)) return -1;
        else{
            this.users.put(u.getEmail(),u.clone());
            return 1;
        }
    }
        
    public boolean emailPertence(String email){
        return this.users.containsKey(email);
    }
    
    //Diz se um entregador já fez entregas
    public boolean fezEntregas(String emailE){
        return this.entregas.containsKey(emailE);
    }
          
    //Vê se Email e Palavra Passe dadas constam no sistema
    public boolean credenciasCorretas(String email,String pass){
        return(this.users.get(email).getPass().equals(pass));
    }
        
    //Muda o estado de uma encomenda para "DISPONIVEL" para levantamento
    public void mudaEstadoDis(String cod){
        this.encomendas.get(cod).setState("DISPONIVEL");
        this.encomendas.get(cod).setEntregador("n/a");
    }
        
    //Muda o estado de uma encomenda para "ACEITE" por um voluntario ou empresa
    public void mudaEstadoAceite(String cod, String emailV){
        this.encomendas.get(cod).setState("ACEITE");
        this.encomendas.get(cod).setEntregador(emailV);
    }
        
    //Muda o estado de uma encomenda para "POR CONFIRMAR", e diz a empresa que a vai transportar
    public void mudaEstadoPconfirmar(String cod, String emailV){
        this.encomendas.get(cod).setState("POR CONFIRMAR");
        this.encomendas.get(cod).setEntregador(emailV);
    }
        
    //Muda o estado de uma encomenda para "ENTREGUE"
    public void mudaEstadoEntregue(String cod){
        this.encomendas.get(cod).setState("ENTREGUE");
    }
        
    //Muda o estado de uma encomenda para "CONFIRMADO"
    public void mudaEstadoConfirmado(String cod){
        this.encomendas.get(cod).setState("CONFIRMADO");
    }
        
    //Retorna todos os users que são lojas
    public List<Loja> lojasinSys(){
        List<Loja> r = new ArrayList<>();
        
        for(Utilizador u : this.users.values()){
            if(u.getClass().getName().equals("Loja")){
                r.add((Loja) u.clone());
            }
        }
        return r;
    }
        
    //Retorna a lista de entregas feitas por um entregador entre duas datas
    public List<Entrega> entregasPeriodo(String emailE, LocalDate inicio, LocalDate fim){
        ArrayList<Entrega> res = new ArrayList<>();
        
        if(this.users.containsKey(emailE)){
            
            for(Entrega e : this.entregas.values()){
                if(e.getEntregador().equals(emailE) && e.getData().isAfter(inicio) && e.getData().isBefore(fim)){
                    res.add(e.clone());
                }
            }
        }
        return res;
    }
        
    //Lucro de um entregador num dado período de tempo
    public double lucroPeriodo(String emailE, LocalDate inicio, LocalDate fim){
        double res=0;
        
        if(this.users.containsKey(emailE)){
            
            for(Entrega e : this.entregas.values()){
                if(e.getEntregador().equals(emailE) && e.getData().isAfter(inicio) && e.getData().isBefore(fim)){
                    res=res+e.getCusto();
                }
            }
        }
        return res;
    }
        
    //Cria a entrega de um voluntário
    public Entrega fazEntregaV(Encomenda enc, String emailE){
        Voluntarios v = (Voluntarios) this.users.get(emailE);
        Comprador com = (Comprador) this.users.get(enc.getCUser());
        Loja loja = (Loja) this.users.get(enc.getCLoja());
        Localizacao cbuyer = com.getCoord();
        Localizacao cloja = loja.getCoord();
        Localizacao centregador = v.getCoord(); 
        double custo = (v.custoTransV(enc,cloja,cbuyer))/100;
        String buyer = enc.getCUser();
        String loja2 = enc.getCLoja();
            
        Entrega ent = new Entrega(cbuyer,cloja,centregador,custo,buyer,loja2,emailE,enc.getCEnc(),LocalDate.now(),"NAO CLASSIFICADA");
        return ent;
    }
        
    //Cria a entrega de uma empresa
    public Entrega fazEntregaE(Encomenda enc, String emailE){
        Empresa ent = (Empresa) this.users.get(emailE);
        Comprador com = (Comprador) this.users.get(enc.getCUser());
        Loja loja = (Loja) this.users.get(enc.getCLoja());
        Localizacao cbuyer = com.getCoord();
        Localizacao cloja = loja.getCoord();
        Localizacao centregador = ent.getCoord(); 
        double custo = (ent.custoTransE(enc,cloja,cbuyer))/100;
        String buyer = enc.getCUser();
        String loja2 = enc.getCLoja();
            
        Entrega entrega = new Entrega(cbuyer,cloja,centregador,custo,buyer,loja2,emailE,enc.getCEnc(),LocalDate.now(),"NAO CLASSIFICADA");
        return entrega;
    }
        
    //Retorna lista das encomendas ainda por disponibilizar de uma loja dada
    public List<Encomenda> encomendasNaoDisp(String emailLoja){
        List<Encomenda> r = new ArrayList<>();
        
        for(Encomenda e : this.encomendas.values()){
            if(e.getCLoja().equals(emailLoja) && e.getState().equals("NAO DISPONIVEL")){
                r.add(e.clone());
            }
        }
        return r;
    }
        
    //Diz a uma loja quantas encomendas ela tem por disponibilizar
    public int encomendasPorDisp(String emailLoja){
        int r = 0;
        
        for(Encomenda e : this.encomendas.values()){
            if(e.getCLoja().equals(emailLoja) && e.getState().equals("NAO DISPONIVEL")){
                r++;
            }
        }
        return r;
    }
    
    //Encomendas disponíveis para levantar numa dada loja, onde o local de entrega está dentro do raio de ação do voluntário
    public List<Encomenda> encomendasDispAlcanceV(String emailLoja, String emailE){
        List<Encomenda> r = new ArrayList<>();
        Voluntarios v = (Voluntarios) this.users.get(emailE);
        
        for(Encomenda e : this.encomendas.values()){
            Comprador c = (Comprador) this.getUser(e.getCUser());
            if(e.getCLoja().equals(emailLoja) && e.getState().equals("DISPONIVEL") && v.getCoord().distancia(c.getCoord())<=v.getRaio()){
                r.add(e.clone());
            }
        }
        return r;
    }
        
    //Encomendas disponíveis para levantar numa dada loja, onde o local de entrega esta dentro do raio de ação da empresa
    public List<Encomenda> encomendasDispAlcanceE(String emailLoja, String emailE){
        List<Encomenda> r = new ArrayList<>();
        Empresa emp = (Empresa) this.users.get(emailE);
        
        for(Encomenda e : this.encomendas.values()){
            Comprador c = (Comprador) this.getUser(e.getCUser());
            if(e.getCLoja().equals(emailLoja) && e.getState().equals("DISPONIVEL") && emp.getCoord().distancia(c.getCoord())<=emp.getRaio()){
                r.add(e.clone());
            }
        }
        return r;
    }
        
    //Adiciona uma entrega ao Map de entregas
    public int addEntrega(Entrega ent){
        if(this.entregas.containsValue(ent)) return -1;
        else{
           Entrega r = ent.clone();
           this.entregas.put(r.getIDenc(),r);
           return 1;
        }
    }
        
    //Lojas que estão dentro do raio de um entregador: voluntário
    public List<Loja> dentrodoRaioV(String emailE){
        List<Loja> l = new ArrayList<>();
        Voluntarios v = (Voluntarios) this.users.get(emailE);
        
        for(Utilizador u : this.users.values()){
            if(u.getClass().getName().equals("Loja")){
                Loja ls = (Loja) u;
                if(v.getCoord().distancia(u.getCoord())<=v.getRaio()){
                    l.add(ls.clone());
                }
            }
        }
        return l;
    }
    
    //Lojas que estão dentro do raio de um entregador: empresa
    public List<Loja> dentrodoRaioE(String emailE){
        List<Loja> l = new ArrayList<>();
        Empresa e = (Empresa) this.users.get(emailE);
        
        for(Utilizador u : this.users.values()){
            if(u.getClass().getName().equals("Loja")){
                Loja ls = (Loja) u;
                if(e.getCoord().distancia(u.getCoord())<=e.getRaio()){
                    l.add(ls.clone());
                }
            }
        }
        return l;
    }
        
    //Vê se um entregador está a fazer uma entrega
    public boolean entregando(String emailV){
        
        for(Encomenda e : this.encomendas.values()){
            if(e.getEntregador().equals(emailV) && e.getState().equals("ACEITE")){
                return true;
            }
        }
        return false;
    }
    
    //Lista com os códigos das encomendas que um entregador está a fazer
    public List<String> entregasAfazer(String emailE){
        List<String> r = new ArrayList<>();
        
        for(Encomenda e : this.encomendas.values()){
            if(e.getEntregador().equals(emailE) && e.getState().equals("ACEITE")){
                r.add(e.getCEnc());
            }
        }
        return r;
    }
            
    //Retorna o estado atual de um voluntário
    public boolean estadoV(String emailV){
        Voluntarios v = (Voluntarios) this.users.get(emailV);
        return v.getAvailable();
    }
        
    //Muda um estado de um voluntário
    public void setDisponivel(String emailV,boolean state){
        Voluntarios v = (Voluntarios) this.users.get(emailV);
        v.setAvailable(state);
    }
        
    //Vê quantas encomendas há para aceitar por parte de um comprador
    public int encomendasPorConfirmar(String emailC){
        int r = 0;
        for(Encomenda e : this.encomendas.values()){
            if(e.getCUser().equals(emailC) && e.getState().equals("POR CONFIRMAR")){
                r++;
            }
        }
        return r;
    }
    
    //Vê quantas encomendas foram aceites por um comprador
    public int encomendasConfirm(String emailE){
        int r = 0;
        for(Encomenda e : this.encomendas.values()){
            if(e.getEntregador().equals(emailE) && e.getState().equals("CONFIRMADO")){
                r++;
            }
        }
        return r;
    }
        
    //Retorna lista das encomendas por autorizar
    public List<Encomenda> encPconf(String emailC){
        List<Encomenda> r = new ArrayList<>();
        
        for(Encomenda e : this.encomendas.values()){
            if(e.getCUser().equals(emailC) && e.getState().equals("POR CONFIRMAR")){
                r.add(e.clone());
            }
        }
        return r;
    }
    
    //Retorna lista das encomendas autorizadas para transporte
    public List<Encomenda> encConf(String emailC){
        List<Encomenda> r = new ArrayList<>();
        
        for(Encomenda e : this.encomendas.values()){
            if(e.getEntregador().equals(emailC) && e.getState().equals("CONFIRMADO")){
                r.add(e.clone());
            }
        }
        return r;
    }
        
    //Nº de encomendas que um dado comprador já recebeu
    public int encomendasRecebidas(String email){
        int r = 0;
        
        for(Entrega e : this.entregas.values()){
            if(e.getComprador().equals(email)){
                r++;
            }
        }
        
        return r;
    }
    
    //Top 10 compradores da aplicação
    public void top10Users(){
        List<Comprador> c = new ArrayList<>();
        Comparator<Comprador> comp = (Comprador u1, Comprador u2) -> {
            return Integer.compare(this.encomendasRecebidas(u2.getEmail()),this.encomendasRecebidas(u1.getEmail()));
        };
        
        for(Utilizador u : this.users.values()){
            if(u.getClass().getName().equals("Comprador") && this.encomendasRecebidas(u.getEmail())>0){
                Comprador com = (Comprador) u;
                c.add(com.clone());
            }
        }
        
        Collections.sort(c,comp);
        
        int n = c.size();
        
        System.out.println("");
        System.out.println("Top 10 utilizadores que mais usam a aplicação:");
        
        if(n<=10){
            int j = 1;
            for(Comprador c2 : c){
                System.out.println(j + "º " + c2.getName() + " : " + this.encomendasRecebidas(c2.getEmail()) + " encomenda(s) recebida(s)");
                j++;
            }
                
        }else {
            int j = 1;
            for(Comprador c2 : c){
                if(j>=10){
                    break;
                }
                else{
                    System.out.println((j) + "º " + c2.getName() + " : " + this.encomendasRecebidas(c2.getEmail()) + " encomenda(s) recebida(s)");
                    j++;
                }
            }
        }
    }
    
    
    //Nº de encomendas entregues por um entregador
    public int encomendasEntregues(String email){
        int r = 0;
        
        for(Entrega e : this.entregas.values()){
            if(e.getEntregador().equals(email)){
                r++;
            }
        }
        
        return r;
    }
    
    //Kms percorridos por uma empresa
    public double kmpercorridos(String email){
        double r = 0;
        
        for(Entrega e : this.entregas.values()){
            if(e.getEntregador().equals(email)){
                r = r + e.getCentregador().distancia(e.getCloja()) + e.getCloja().distancia(e.getCbuyer());
            }
        }
        return r;
    }
    
    //Top 10 empresas da aplicação
    public void top10Entregadores(){
        List<Empresa> c = new ArrayList<>();
        Comparator<Empresa> comp = (Empresa u1, Empresa u2) -> {
            return Double.compare(this.kmpercorridos(u2.getEmail()),this.kmpercorridos(u1.getEmail()));
        };
        
        for(Utilizador u : this.users.values()){
            if(u.getClass().getName().equals("Empresa") && this.encomendasEntregues(u.getEmail())>0){
                Empresa emp = (Empresa) u;
                c.add(emp.clone());
            }
        }
        
        Collections.sort(c,comp);
        
        int n = c.size();
        
        System.out.println("");
        System.out.println("Top 10 empresas transportadoras por Kms percorridos: ");
        
        if(n<=10){
            int j = 1;
            for(Empresa c2 : c){
                System.out.println(j + "º " + c2.getName() + " : " + this.kmpercorridos(c2.getEmail()) + " Kms");
                j++;
            }
                
        }else {
            int j = 1;
            for(Empresa c2 : c){
                if(j>=10){
                    break;
                }
                else{
                    System.out.println((j) + "º " + c2.getName() + " : " + this.kmpercorridos(c2.getEmail()) + " Kms");
                    j++;
                }
            }
        }
    }

    //Permite classificar uma entrega após a sua receção.
    public void classificar1entrega(String emailC){
        
        if(encomendasRecebidas(emailC)==0){
            System.out.println("Ainda não recebeu encomendas.");
        }
        else{

            if(this.numeroentregasPclassi(emailC)==0){
                System.out.println("Já classificou todas as entregas.");
            }
            else{
                
                List<Entrega> entregas = entregasPclassificar(emailC);
                
                System.out.println("Entregas por classificar: ");
                
                int i = 1;
                
                for(Entrega e : entregas){
                    System.out.println(i + " : " + e.toString());
                }
                
                System.out.println("Indique a entrega que deseja classificar: ");
                Scanner optn = new Scanner(System.in);
                int o = optn.nextInt();
                
                if(o<=0 || o>i){
                    System.out.println("Opção Errada!");
                }
                else{
                    Entrega e2 = entregas.get(o-1);
                    
                    System.out.println("Qual a classificação que deseja atribuir ao voluntário ou empresa transportadora? (0 a 10)");
                    
                    Scanner classi = new Scanner(System.in);
                    double nota = Double.parseDouble(classi.nextLine());
                    
                    if(nota<0 || nota>10){
                        System.out.println("Classificação fora dos limites!");
                    }
                    else{
                        
                        String emailE = e2.getEntregador();
                        String enc = e2.getIDenc();
                        
                        if(this.users.get(emailE).getClass().getName().equals("Empresa")){
                            classificarEmpresa(emailE,nota);
                            alteraAvaliada(enc);
                        }
                        if(this.users.get(emailE).getClass().getName().equals("Voluntarios")){
                            classificarVoluntario(emailE,nota);
                            alteraAvaliada(enc);
                        }

                    }
                }
            }
        }
        
    }
    
    //Classificar uma empresa ou voluntário
    public void classificarEmpresa(String email, double classificacao){
        Empresa e = (Empresa) this.users.get(email);
        if(e!=null ){
            double notaT = (e.getNota()*e.getReviews()+classificacao)/(e.getReviews()+1);
            e.setNota(notaT);
            e.setReviews(e.getReviews()+1);
        }
    }
    
    public void classificarVoluntario(String email, double classificacao){
        Voluntarios v = (Voluntarios) this.users.get(email);
        if(v!=null){
            double notaT = (v.getNota()*v.getReviews()+classificacao)/(v.getReviews()+1);
            v.setNota(notaT);
            v.setReviews(v.getReviews()+1);
        }
    }
    
    //Lista de entregas não classificadas
    public List<Entrega> entregasPclassificar(String emailC){
        List<Entrega> r = new ArrayList<>();
        
        for(Entrega e : this.entregas.values()){
            if(e.getComprador().equals(emailC) && e.getAvaliada().equals("NAO CLASSIFICADA")){
                r.add(e.clone());
            }
        }
        return r;
    }
    
    //Nº de entregas não classificadas
    public int numeroentregasPclassi(String emailC){
        int r = 0;
        
        for(Entrega e : this.entregas.values()){
            if(e.getComprador().equals(emailC) && e.getAvaliada().equals("NAO CLASSIFICADA")){
                r++;
            }
        }
        return r;
    }
    
    //Altera classificação de uma entrega
    public void alteraAvaliada(String codEnc){
        Entrega e = this.entregas.get(codEnc);
        e.setClassificada();
    }
 
   
    
    /**
     * Métodos para a leitura e escrita de ficheiro
     */
    
    
    //Grava no ficheiro o estado atual
    public void writeFile2(String nomeFicheiro) throws FileNotFoundException,IOException{
        PrintWriter pw = new PrintWriter(nomeFicheiro);
        pw.println("........ TrazAqui ........");
        
        for(Utilizador u : this.users.values()){
                String s = u.stringtoFile();
                pw.println(s);
        }
        
        for(Encomenda e : this.encomendas.values()){
            String s = e.stringtoFile();
            pw.println(s);
        }
        
        for(Entrega ent : this.entregas.values()){
            String s = ent.stringtoFile();
            pw.println(s);
        }

        pw.flush();
        pw.close();
    }
    
    //Lê e cria objetos com a informação do ficheiro
    public static TrazAqui lerLog(String file) throws FileNotFoundException, IOException{
        List<String> linhas = lerFicheiro(file); 
        TrazAqui t = new TrazAqui();
        String[] s;
        for (String linha : linhas) {
            s = linha.split(":", 2);
            
            if(s[0].equals("Utilizador")){
                try{
                    Comprador c = lerComprador(s[1]);
                    
                    t.addUser(c);
                }
                catch(NullPointerException e){
                }
            }
            
            if(s[0].equals("Loja")){
                try{
                    Loja loja = lerLoja(s[1]);
                    t.addUser(loja);
                }
                catch(NullPointerException e){
                    }
            }
          
            if(s[0].equals("Voluntario")){
                 try{
                     Voluntarios v = lerVoluntario(s[1]);
                     
                     t.addUser(v);
                 }
                 catch(NullPointerException e){
                    }
            }
          
            if(s[0].equals("Transportadora")){
                 try{
                     Empresa e = lerEmpresa(s[1]);
                     
                     t.addUser(e);
                    }
                 catch(NullPointerException e){
                    }
            }
          
            if(s[0].equals("Encomenda")){
                try{
                    Encomenda enc = lerEncomenda(s[1]);
                    
                    t.addEncomenda(enc);
                }
                catch(NullPointerException e){
                }
            }
            
            if(s[0].equals("Entrega")){
                try{
                    Entrega ent = lerEntrega(s[1]);
                    t.addEntrega(ent);
                    //System.out.println(ent.toString());
                }
                catch(NullPointerException e){
                }
            }
          
        }
    
        return t;
    }

    
    public static List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try { lines = Files.readAllLines(Paths.get(nomeFich), StandardCharsets.UTF_8); }
        catch(IOException exc) { System.out.println(exc.getMessage()); }
        return lines;
    }
    
    
    //Cria um comprador com os dados do ficheiro
    private static Comprador lerComprador(String input){
        String[] buffer = input.split(",");
        String email = buffer[0];
        String pass = buffer[1];
        String nome = buffer[2];
        double gpsx = Double.parseDouble(buffer[3]);
        double gpsy = Double.parseDouble(buffer[4]);
        Localizacao l = new Localizacao(gpsx,gpsy);
        Comprador c = new Comprador(email,pass,nome,l,new ArrayList<>());
        return c;
    }
        
    //Cria uma loja com os dados do ficheiro
    private static Loja lerLoja(String input){
        String[] s = input.split(",");
        String email = s[0];
        String pass = s[1];
        String nome = s[2];
        Localizacao l = new Localizacao(Double.parseDouble(s[3]),Double.parseDouble(s[4]));
        Loja loja = new Loja(email,pass,nome,l,false,new ArrayList<>());
        return loja;
    }
        
    //Cria um voluntário com os dados do ficheiro
    private static Voluntarios lerVoluntario(String input){
        String[] s = input.split(",");
        String email = s[0];
        String pass = s[1];
        String nome = s[2];
        double raio = Double.parseDouble(s[5]);
        boolean ava = Boolean.parseBoolean(s[6]);
        double nota = Double.parseDouble(s[7]);
        double nreviews = Double.parseDouble(s[8]);
        Localizacao l = new Localizacao(Double.parseDouble(s[3]),Double.parseDouble(s[4]));
        Voluntarios v = new Voluntarios(email,pass,nome,l,raio,ava,nota,nreviews);
        return v;
    }
        
    //Cria uma empresa com os dados do ficheiro
    private static Empresa lerEmpresa(String input){
        String[] s = input.split(",");
        String email = s[0];
        String pass = s[1];
        String nome = s[2];
        int NIF = Integer.parseInt(s[5]);
        double raio = Double.parseDouble(s[6]);
        double pkm = Double.parseDouble(s[7]);
        double nota = Double.parseDouble(s[8]);
        double nreviews = Double.parseDouble(s[9]);
        Localizacao l = new Localizacao(Double.parseDouble(s[3]),Double.parseDouble(s[4]));
        Empresa e = new Empresa(email,pass,nome,l,NIF,raio,pkm,nota,nreviews);
        return e;
    }
        
    //Cria uma encomenda com os dados do ficheiro
    private static Encomenda lerEncomenda(String input){
        String[] campos = input.split(",");
        String cod = campos[0];
        String buyer = campos[1];
        String loja = campos[2];
        String entregador = campos[3];
        double peso = Double.parseDouble(campos[4]);
        String state = campos[5];
        Encomenda e = new Encomenda();
        e.setCEnc(cod);
        e.setCUser(buyer);
        e.setCLoja(loja);
        e.setEntregador(entregador);
        e.setPeso(peso);
        e.setState(state);
        
        int i = 6;
        while(i<=(campos.length-4)){
            LinhaEncomenda le = parseLinhaEncomenda(campos[i],campos[i+1],Double.parseDouble(campos[i+2]),Double.parseDouble(campos[i+3]));
            e.addLinhaEncomenda(le);
            i+=4;
        }
      
        return e;
    }
      
    //Cria uma linha de encomenda com os dados do fichiero
    private static LinhaEncomenda parseLinhaEncomenda(String cod, String desc, double quantidade, double valor){
        return new LinhaEncomenda(cod,desc,quantidade,valor);
    }
       
    //Cria uma entrega com os dados do ficheiro
    private static Entrega lerEntrega(String input){
        String[] campos = input.split(",");
        double gpsxB = Double.parseDouble(campos[0]);
        double gpsyB = Double.parseDouble(campos[1]);
        Localizacao lb = new Localizacao(gpsxB,gpsyB);
        
        double gpsxL = Double.parseDouble(campos[2]);
        double gpsyL = Double.parseDouble(campos[3]);
        Localizacao ll = new Localizacao(gpsxL,gpsyL);
        
        double gpsxE = Double.parseDouble(campos[4]);
        double gpsyE = Double.parseDouble(campos[5]);
        Localizacao le = new Localizacao(gpsxE,gpsyE);
        
        double custo = Double.parseDouble(campos[6]);
        String comprador = campos[7];
        String vendedor = campos[8];
        String entregador = campos[9];
        String idEnc = campos[10];
        LocalDate data = lerdata(campos[11]);
        String avaliada = campos[12];
        
        Entrega ent = new Entrega(lb,ll,le,custo,comprador,vendedor,entregador,idEnc,data,avaliada);
        
        return ent;
    }
    
    
    
    private static LocalDate lerdata(String input){
        String campos[] = input.split("-");
        
        int year = Integer.parseInt(campos[0]);
        int month = Integer.parseInt(campos[1]);
        int day = Integer.parseInt(campos[2]);
        
        LocalDate data = LocalDate.of(year,month,day);
        
        return data;
    }
}