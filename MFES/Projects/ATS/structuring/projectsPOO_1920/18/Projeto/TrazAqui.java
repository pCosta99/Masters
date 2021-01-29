import java.util.Scanner;
import java.io.*;
import java.lang.String;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.time.LocalDateTime;
import java.util.InputMismatchException;




public class TrazAqui{
    
    private GestaoUsuarios log;
    private String usuarioAtual;
    private Parse p;
    
    public TrazAqui (){
        this.log = new GestaoUsuarios();
        this.usuarioAtual=null;
        this.p = new Parse();
    }
    
    public TrazAqui (GestaoUsuarios g,String s, Parse a){
        this.log=g.clone();
        this.usuarioAtual=s;
        this.p = a;
    }
    
    public TrazAqui(TrazAqui t){
        this.log=t.getLog();
        this.usuarioAtual=t.getUsuarioAtual();
        this.p=t.getParse();
    }
    
    public GestaoUsuarios getLog(){
        return this.log.clone();
    }
    
    public void setLog(GestaoUsuarios gu){
        this.log=gu.clone();
    }
    
    public String getUsuarioAtual(){
        return this.usuarioAtual;
    }
    
    public void setUsuarioAtual(String u){
        this.usuarioAtual=u;
    }
    
    public Parse getParse(){
        return this.p;
    }
    
    public void setParse(Parse pa){
        this.p=pa;
    }
    
    public boolean equals(Object obj){
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        TrazAqui ta = (TrazAqui) obj;
        return this.log.equals(ta.getLog()) && this.usuarioAtual.equals(ta.getUsuarioAtual());
    }
    
    public TrazAqui clone(){
        return new TrazAqui(this);
    }
    
    public String toString (){
        StringBuilder sb=new StringBuilder();
        sb.append(this.log.toString()).append(",").append(this.usuarioAtual);
        return sb.toString();
    }
    
    public void carregarFicheiro() throws IOException{
        this.log = p.carregaEstado();
    }
    
    public void salvarEstado() throws IOException{
        this.p.guardarEstado(this.log);
    }
    
    //retorna true se o login foi efetuado com sucesso
    public boolean login (String mail,String pass){
        if (this.log.containsEmail(mail)){
           if  (this.log.passCorreta(mail,pass)){
               this.usuarioAtual=this.log.getCodigo(mail);
           }else{
               return false;
           }
        }else{
           return false;
        }
        return true;
    }
    
    public void removerUsuarioAtual(){
        this.usuarioAtual=null;
    }
    
    public int novoUtilizadorBasico(String input) throws IOException{
        String[] campos=input.split(";");

        if (this.log.containsEmail(campos[4])){return 1;}
        else if (this.log.contains(campos[2])){return 2;}
        else{
           UtilizadorBasico ub=new UtilizadorBasico(new GPS(Float.parseFloat(campos[0]),Float.parseFloat(campos[1])),campos[4],campos[5],
                                                          new GestaoEncomendas(),new GestaoEncomendas(),campos[3],campos[2],
                                                          new ArrayList<>(),new HashMap<>(),new ArrayList<>());
           this.log.addEmail(campos[4],campos[2]);
           this.log.addUsuario(ub);
        }
        return 0;
    }
    
    
    public int novaLoja(String input) throws IOException{
        String[] campos = input.split(";");
        
        if (this.log.containsEmail(campos[4])){return 1;}
        else if (this.log.contains(campos[2])){return 2;}
        else{
           Loja l=new Loja(campos[2],campos[3],new GPS(Float.parseFloat(campos[0]),Float.parseFloat(campos[1])),campos[4],
                           campos[5],new GestaoEncomendas(),new GestaoEncomendas(),new ArrayList<>()
                           ,new ArrayList<>(),new HashMap<>());
           this.log.addEmail(campos[4],campos[2]);
           this.log.addUsuario(l);     
        }                  
        return 0;
    }

    public int novaLojaComFila(String input) throws IOException{
        String[] campos = input.split(";");
        
        if (this.log.containsEmail(campos[4])){return 1;}
        else if (this.log.contains(campos[2])){return 2;}
        else{
           LojaComFila l=new LojaComFila(campos[2],campos[3],new GPS(Float.parseFloat(campos[0]),Float.parseFloat(campos[1])),
                        new GestaoEncomendas(),new GestaoEncomendas(),campos[4],campos[5],0,new ArrayList<>()
                        ,new ArrayList<>(),new HashMap<>());
           this.log.addEmail(campos[4],campos[2]);
           this.log.addUsuario(l);     
        }                  
        return 0;
    }
    
    public int novaEmpresaTransportadora(String input) throws IOException{
        String[] campos = input.split(";");
        
        if (this.log.containsEmail(campos[4])){return 1;}
        else if (this.log.contains(campos[2])){return 2;}
        else{
           EmpresaTransportadora et=new EmpresaTransportadora(new GPS(Float.parseFloat(campos[0]),Float.parseFloat(campos[1])),
                        campos[4],campos[5],campos[3],campos[2],Float.parseFloat(campos[8]),false,new GestaoEncomendas(),
                        new GestaoEncomendas(),Integer.parseInt(campos[6]),Float.parseFloat(campos[9]),Integer.parseInt(campos[7])
                        ,new GestaoEncomendas(),new ArrayList<>(),new HashMap<>(),0,new HashMap<>());
           this.log.addEmail(campos[4],campos[2]);
           this.log.addUsuario(et);    
        }                  
        return 0;
    } 
     
    public int novoVoluntario(String input) throws IOException{
        String[] campos = input.split(";");
        
        if (this.log.containsEmail(campos[4])){return 1;}
        else if (this.log.contains(campos[2])){return 2;}
        else{
           Voluntario v=new Voluntario(new GPS(Float.parseFloat(campos[0]),Float.parseFloat(campos[1])),campos[4],campos[5],
                      campos[3],campos[2],Float.parseFloat(campos[7]),false,new GestaoEncomendas(),new GestaoEncomendas(),
                      new HashMap<>(),false,0);
           this.log.addEmail(campos[4],campos[2]);
           this.log.addUsuario(v);  
        }                  
        return 0;
    }
    
    public int criarUsuarioExistente(String cod,String mail,String pass){
        if (this.log.temUsuario(cod) ){
            if (this.log.usuarioSemEmail(cod)){
                if (this.log.containsEmail(mail)){return 1;//Email ja registado
                }else{
                   this.log.setEmailPass(cod,mail,pass);            
                }
            }else{return 2;}//usuario ja tem email
        }else{return 3;}//Usuario com esse codigo nao existe
        return 0;//email e pass defenidos com sucesso
    }
    
    public int getClassUsuario(){
        Class s = this.log.getUsuario(this.usuarioAtual).getClass();
        
        if (s.equals(UtilizadorBasico.class)){
            return 0;      
        }else if (s.equals(Loja.class)){
            return 1;
        }else if (s.equals(LojaComFila.class)){
            return 2;
        }else if (s.equals(EmpresaTransportadora.class)){
            return 3;
        }else if (s.equals(Voluntario.class)){
            return 4;
        }
        return -1;
    }
    
    /* Opcoes UTILIZADOR BASICO */
    public String getEstadoUB (){
        UtilizadorBasico ub = (UtilizadorBasico) this.log.getUsuario(this.usuarioAtual);
        StringBuilder sb=new StringBuilder();
        sb.append("Codigo: ").append(this.usuarioAtual).append("\n");
        return sb.toString();
    }
    
    //comum a todas as classes
    public String getGEncomendas (){
        return this.log.getGEncomendas(this.usuarioAtual);
    }
    
    //comum a todas as classes
    public String informacao(){
        return this.log.informacao(this.usuarioAtual);
    }
    
    // solicitar transporte de encomenda
    public int solicitarTransporte(String codEnc){
       return this.log.solicitarTransporte(codEnc,this.usuarioAtual);
    }
    
    //opcao 4 -> ver encomendas prontas
    public String getEncomendasProntas(){
        UtilizadorBasico ub = (UtilizadorBasico) this.log.getUsuario(this.usuarioAtual);
        StringBuilder sb=new StringBuilder();
        
        sb.append("Encomendas prontas: ").append(ub.getEncomendasProntas().toString());
        return sb.toString();
    }
    
    //opcao
    public String verTransportadoresnoRaio(){
        return this.log.verTransportadoresnoRaio(this.usuarioAtual);
    }
    
    //opcao 5 -> ver propostas de transporte
    public String getPropostasDeTransporteUB(){
        UtilizadorBasico ub = (UtilizadorBasico) this.log.getUsuario(this.usuarioAtual);
        return ub.getStrPropostasDeTransporte();
    }
    
    //opcao 6 -> aceitar proposta de transporte
    public int aceitarPropostaUB(String codEnc,String codTra){
        UtilizadorBasico ub = (UtilizadorBasico) this.log.getUsuario(this.usuarioAtual);
        if (ub.propostasContains(codEnc)){
            if (ub.propostasContainsTra(codEnc,codTra)){
                this.log.transporteDeEncomendaAceite(this.usuarioAtual,codEnc,codTra);
            }else{return 1;}//codigo de encomenda nao e valido                        
        }else{return 2;}//codigo de transportador invalido
        return 0;//tudo correu bem
    }
    
    //opcao -> ver encomendas efetuadas ((comum
    public String verEncomendasEfetuadas(){
        return this.log.getStrEncomendasEfetuadas(this.usuarioAtual);
    }
    
    //opcao 7 -> ver classificacao de transportador
    public String verClassificacao(String codTra){
        String s;
        if (this.log.containsTransportador(codTra)){
             s = this.log.getClassificacao(codTra);
        }else{return null;}//codigo nao e valido
        return s;
    }
    
    //opcao 8 -> Classificar transportador
    public int classificarTransportadorUB (String codTra,int i){
        if (this.log.containsTransportador(codTra)){
           if (i>=1 && i<=10){
              this.log.setClassificacao(codTra,this.usuarioAtual,i);
           }else{return 2;}//Valor invalido (tem de ser um valor entre 1 e 10)
        }else{return 1;}//"Codigo nao corresponde a nenhum transportador
        return 0;//caso de sucesso
    }
   
    //opcao 9 -> ver tabela de precos
    public String verTabelaDePrecos(){
        return this.log.getTabelaPrecos();
    }
    
    //opcao 10 -> pedir nova encomenda a uma loja
    public int pedirNovaEncomendaUB(String codLoja,List<String> produtos){
        List<LinhaEncomenda> pro=new ArrayList<>();
        String campos[];
        String s;
        if (this.log.containsLoja(codLoja)){
            for (int i=0;i<produtos.size();i++){
                s=produtos.get(i);
                campos=s.split(";");
                if (campos.length<2) return 2;//um produto esta mal introduzido
                pro.add(new LinhaEncomenda(null,campos[0],Float.parseFloat(campos[1]),0,0));
            }          
        }else{return 1;}//codigo de loja nao e valido
        this.log.novoPedidoDeEncomenda(this.usuarioAtual,codLoja,pro);
        return 0;
    }
    
    /* OPCOES LOJA COMUNS COM LOJA FILA*/   
    public String getEstadoL(){
        Loja l = (Loja) this.log.getUsuario(this.usuarioAtual);
        StringBuilder sb=new StringBuilder();
        sb.append("Codigo: ").append(this.usuarioAtual).append("\n").append("Quantidade de pedidos: ").
                            append(l.quantidadePedidos()).append("\n");
        return sb.toString();
    }
    

    
    //opcao 4 -> encomenda pronta
    public int sinalizarEncPronta(String codEnc){
        if (this.log.containsEncomenda(this.usuarioAtual,codEnc)){
            if (this.log.jaFoiSinalizada(this.usuarioAtual,codEnc)){
                this.log.sinalizarEncomendaPronta(this.usuarioAtual,codEnc);
            }else{return 2;}//Encomenda ja foi sinalizada ao cliente
        }else{return 1;}//Codigo de encomenda nao valido
        return 0;
    }
    
    //opcao 5 -> ver pedidos de encomendas
    public String verPedidosDeEncomendas(){
        Loja l=(Loja) this.log.getUsuario(this.usuarioAtual);
        return l.getPedidosDeEncomendas();
    }
    
    //opcao 6 -> aceitar pedido de utilizador
    public int aceitarPedidoDeEncomenda(String codUB,int index){
        Loja l = (Loja) this.log.getUsuario(this.usuarioAtual);
        List<LinhaEncomenda> p;
        String input;
        LinhaEncomenda le;
        
        if (!(l.pedidoTemUB(codUB))) return 1;//utilazador nao tem pedidos
        if (!(l.pedidoUBtemIndex(codUB,index))) return 2;//numero de pedido invalido
        
        p=l.getPedido(codUB,index);
        
        //(String codE, String codU, String codL,float peso, Map<String,LinhaEncomenda> prod)
        Encomenda e=new EncomendaBasica();
        e.setCodUtilizador(codUB);
        e.setCodLoja(l.getCodigo());
        String campos[];
        
        System.out.println("Insira o codigo da encomenda:\n");
        input = new Scanner(System.in).nextLine();
        if (l.containsCodEncomenda(input)) return 3;//codigo de encomenda ja existe
        e.setCodEncomenda(input);
        
        System.out.println("Para cada linha de encomenda insira o codigo do produto, o valor unitario \ne o peso unitario na forma:<codigo de produto>;<valor unitario>;<peso unitario>");
        for (int i = 0; i<p.size() ;i++){
            le = p.get(i);
            System.out.println(le);
            input=new Scanner(System.in).nextLine();
            campos=input.split(";");
            le.setCodProduto(campos[0]);
            le.setValorUnitario(Float.parseFloat(campos[1]));
            le.setPesoUnitario(Float.parseFloat(campos[2]));
            e.addLEncomenda(le.clone());
        }
        
        this.log.addEncomenda(e.getCodUtilizador(),e);
        l.addEncomenda(e);
        l.removePedidoEncomenda(codUB,index);
        
        return 0;
    }
    
    //opcao 7 -> recusar pedidos de encomendas
    public int recusarPedido (String codUB,int index){
        Loja l = (Loja) this.log.getUsuario(this.usuarioAtual);
        
        if (!(l.pedidoTemUB(codUB))) return 1;//utilazador nao tem pedidos
        if (!(l.pedidoUBtemIndex(codUB,index))) return 2;//numero de pedido invalido
        
        l.recusarPedidoDeEncomenda(codUB,index);
        return 0;
    }
    
    //opcao 8 -> mudar tamanho de fila ((loja com fila
    public void mudarFila (int i){
        LojaComFila l = (LojaComFila) this.log.getUsuario(this.usuarioAtual);
        l.setFila(i);
    }
    
    /* EMPRESA TRANSPORTADORA */
    
    public String getEstadoET(){
        EmpresaTransportadora et = (EmpresaTransportadora) this.log.getUsuario(this.usuarioAtual);
        StringBuilder sb=new StringBuilder();
        sb.append("Codigo: ").append(this.usuarioAtual).append("\n");
        sb.append("Transporto medicamentos: ").append(et.getTransportaMedicamentos()).append("\n");
        return sb.toString();
    }
    
    //Opcao 1 -> ver pedidos de transporte solicitados
    public String verTodosPedidosTransporte(){
        StringBuilder sb=new StringBuilder();
        sb.append("Pedidos:\n").append(this.log.getPedidosDeEncomendas());
        return sb.toString();
    }
    
    public boolean excedeCapacidade (){
        EmpresaTransportadora e = (EmpresaTransportadora) this.log.getUsuario(this.usuarioAtual);
        return e.excedeCapacidade();
    }
    
    //opcao 3 -> solicitar transportar encomenda
    public int transportarET(String codL,String codEnc,float custo){
        
        if (this.log.containsLoja(codL)){
           if (this.log.eEncomendaSolicitada(codL,codEnc)){
              if (this.log.estanoraio(codL,this.usuarioAtual)) {
                  this.log.aceitaTransportarEncomenda(this.usuarioAtual,codL,codEnc,custo);
              }else{return 3;}//nao esta no raio
           }else{return 2;}//Codigo de encomenda invalido
        }else{return 1;}//loja nao existe
        return 0;
    }
    
    //opcao 4 -> registar encomenda entregue
    public int registaEncEntregue(String codEnc,String tempo,float c,float d){
        EmpresaTransportadora et = (EmpresaTransportadora) this.log.getUsuario(this.usuarioAtual);
        
        if (et.containsEncomenda(codEnc)){
           LocalDateTime t=this.p.parseTempo(tempo);
           if (t==null){return 1;}
           this.log.encomendaEntregue(this.usuarioAtual,codEnc,t,c);
           et.addCustoEncomenda(codEnc,c);
           et.addKmsPercorridos(d);
        }else{return 2;}//Codigo nao corresponde a nenhuma encomenda
        return 0;
    }
    
    //opcao 5 -> mudar transporta medicamentos ((comum ao voluntario
    public void mudarTransportaMedicamentos(){
        Transportador et = (Transportador) this.log.getUsuario(this.usuarioAtual);
        et.setTransportaMendicamentos(!(et.getTransportaMedicamentos()));
    }
    
    
    /* VOLUNTARIO */
    public String getEstadoV(){
        Voluntario v = (Voluntario) this.log.getUsuario(this.usuarioAtual);
        StringBuilder sb=new StringBuilder();
        sb.append("Codigo: ").append(this.usuarioAtual).append("\n").
        append("Transporto medicamentos: ").append(v.getTransportaMedicamentos()).append("\n").
        append("Disponibilidade: ").append(String.valueOf(v.getDisponibilidade())).append("\n");
        return sb.toString();
    }
    
    //opcao 3 -> mudar disponibilidade
    public void mudarDisponibilidade(){
        Voluntario v = (Voluntario) this.log.getUsuario(this.usuarioAtual);
        v.setDisponibilidade(!(v.getDisponibilidade()));
    }
    
    //opcao 4 -> entregar encomenda 
    public int entregarEncomendaV (String codL,String codEnc){

        if (this.log.containsLoja(codL)){
            if (this.log.eEncomendaSolicitada(codL,codEnc)){
                if (this.log.estanoraio(codL,this.usuarioAtual)) {
                    this.log.aceitaTransportarEncomendaVoluntario(this.usuarioAtual,codL,codEnc);
                }else{return 3;}//nao esta no raio
            }else{return 2;}//Codigo de encomenda invalido
        }else{return 1;}//Codigo da loja invalido
        return 0;
    }
    
    //opcao 5 -> registar encomenda entregue
    public int registaEncEntregueV(String codEnc,String tempo,float d){
        Voluntario et = (Voluntario) this.log.getUsuario(this.usuarioAtual);
        
        if (et.containsEncomenda(codEnc)){
           LocalDateTime t=this.p.parseTempo(tempo);
           if (t==null){System.out.println("Input invalido.\n");return 1;}
           this.log.encomendaEntregue(this.usuarioAtual,codEnc,t,0);
           et.addKmsPercorridos(d);
        }else{return 1;}//Codigo nao corresponde a nenhuma encomenda
        return 0;
    }      
    
    public String getfaturas(String tempo1,String tempo2){//nullpointerexception
        Transportador et = (Transportador) this.log.getUsuario(this.usuarioAtual);
        LocalDateTime t1=this.p.parseTempo(tempo1);
        LocalDateTime t2=this.p.parseTempo(tempo2);
        return et.getfatura(t1,t2);
    }
    
    /*_______________________*/
    
    public String dezutilizadores(){
        return this.log.dezutilizadores();
    }
    
    public String dezempresas(){
        return this.log.dezempresas();
    }
    
    
    /*_------------Menu info..........*/
    //opcao 1 -> mudar nome
    public void mudarNome(String input){
        this.log.mudarNome(this.usuarioAtual,input);
    }
    
    //opcao 2 -> mudar gps
    public void mudarGps(float la,float lo){
        this.log.mudargps(this.usuarioAtual,la,lo);
    }
    
    //opcao 3 -> mudar mail
    public void mudarEmail(String input){
        this.log.mudarEmail(this.usuarioAtual,input);
    }
    
    //opcao 4 -> mudar pass
    public void mudarPass(String input){
        this.log.mudarPass(this.usuarioAtual,input);
    }
    
    //mudar preco
    public void mudarpreco(float input){
        EmpresaTransportadora et = (EmpresaTransportadora) this.log.getUsuario(this.usuarioAtual);
        et.setPrecoPorKm(input);
    }
    
    public void mudarnif (int input){
        EmpresaTransportadora et = (EmpresaTransportadora) this.log.getUsuario(this.usuarioAtual);
        et.setNif(input);
    }
    
    public void mudarcapacidade (int input){
        EmpresaTransportadora et = (EmpresaTransportadora) this.log.getUsuario(this.usuarioAtual);
        et.setCapacidade(input);
    }   
    
    public void mudarraio (float input){
        Transportador et = (Transportador) this.log.getUsuario(this.usuarioAtual);
        et.setRaio(input);
    }    
    
}













































