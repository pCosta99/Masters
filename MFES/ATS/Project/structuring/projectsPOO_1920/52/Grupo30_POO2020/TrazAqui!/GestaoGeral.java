import java.util.*;
import java.util.ArrayList;
import java.io.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
/**
 * Escreva a descrição da classe GestaoGeral aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class GestaoGeral implements Serializable{
  private GestaoCliente clientes;
  private GestaoEmpresa empresas;
  private GestaoVoluntario voluntarios;
  private GestaoLojas lojas;
  private GestaoEncomenda encomendas;
  
  //construtores
  public GestaoGeral(){
      this.clientes= new GestaoCliente();
      this.empresas= new GestaoEmpresa();
      this.voluntarios= new GestaoVoluntario();
      this.lojas= new GestaoLojas();
      this.encomendas= new GestaoEncomenda();
    }
    
  public HashMap<String,Cliente> getClientes(){
      return this.clientes.getCliente();
    }
    
  public HashMap<String,Empresa> getEmpresas(){
      return this.empresas.getEmpresa();
    }
    
  public HashMap<String,Voluntario> getVoluntarios(){
      return this.voluntarios.getVoluntario();
    }
    
  public HashMap<String,Loja> getLojas(){
      return this.lojas.getLoja();
    }
    
  public HashMap<String,Encomenda> getEncomendas(){
      return this.encomendas.getEncomenda();
    }
    
  //metodos para registar utilizadores
  public void registaCliente(String emailAux, String nomeAux, String passwordAux,Localizacao localizacaoAux, List<RealizadaEmpresa> reAux, List<RealizadaVoluntario> rvAux) throws GestaoGeralException{
       if (clientes.verifica(emailAux)) throw new GestaoGeralException(emailAux);
       else {
            Cliente c_aux = new Cliente(emailAux, nomeAux, passwordAux, localizacaoAux,reAux,rvAux);
            this.clientes.addCliente(c_aux);
        }
    }
  
  public void registaEmpresa(String emailAux, String nomeAux, String passwordAux,Localizacao localizacaoAux,int nifAux,double raiogeograficoAux,int velocidadeAux,double ratingAux,int nmrClassificacoesAux,double taxaAux,int multitaskingAux,int indicadorAux,int capacidadeAux, List<RealizadaEmpresa> reAux) throws GestaoGeralException{
       if (empresas.verifica(emailAux)) throw new GestaoGeralException(emailAux);
       else {
            Empresa e_aux = new Empresa(emailAux, nomeAux, passwordAux, localizacaoAux,nifAux,raiogeograficoAux,velocidadeAux,ratingAux,nmrClassificacoesAux,taxaAux,multitaskingAux,indicadorAux,capacidadeAux,reAux);
            this.empresas.addEmpresa(e_aux);
        }
    }
  
  public void registaVoluntario(String emailAux, String nomeAux, String passwordAux,Localizacao localizacaoAux,double raiogeograficoAux,int velocidadeAux,double ratingAux,int nmrClassificacoesAux,int verificadorAux,List<RealizadaVoluntario> rvAux) throws GestaoGeralException{
      if (voluntarios.verifica(emailAux)) throw new GestaoGeralException(emailAux);
       else {
            Voluntario v_aux = new Voluntario(emailAux,nomeAux, passwordAux, localizacaoAux,raiogeograficoAux,velocidadeAux,ratingAux,nmrClassificacoesAux,verificadorAux,rvAux);
            this.voluntarios.addVoluntario(v_aux);
        }
    }
   
  public void registaLoja(String emailAux, String nomeAux, String passwordAux,Localizacao localizacaoAux, double tempoEsperaAux) throws GestaoGeralException{
      if (lojas.verifica(emailAux)) throw new GestaoGeralException(emailAux);
       else {
            Loja l_aux = new Loja(emailAux,nomeAux, passwordAux, localizacaoAux,tempoEsperaAux);
            this.lojas.addLoja(l_aux);
        }
    }
    
  //metodos para login
  public void loginCliente(String emailAux, String passwordAux) throws GestaoGeralException {
        if (clientes.login(emailAux, passwordAux) == false) throw new GestaoGeralException(emailAux);
    }

  public void loginEmpresa(String emailAux, String passwordAux) throws GestaoGeralException {
        if (empresas.login(emailAux, passwordAux) == false) throw new GestaoGeralException(emailAux);
    }
 
  public void loginVoluntario(String emailAux, String passwordAux) throws GestaoGeralException {
        if (voluntarios.login(emailAux, passwordAux) == false) throw new GestaoGeralException(emailAux);
    }  
    
  public void loginLoja(String emailAux, String passwordAux) throws GestaoGeralException {
        if (lojas.login(emailAux, passwordAux) == false) throw new GestaoGeralException(emailAux);
    }
    
   // metodos para adicioanr listas de entidades lidas dos logs � gestao geral 
  public void addListaClientes(List<Cliente> c){
        for (Cliente temp : c) {
        this.clientes.addCliente(temp);
     }
   }
   
   public void addListaEmpresas(List<Empresa> e){
        for (Empresa temp : e) {
        this.empresas.addEmpresa(temp);
     }
   }
   
    public void addListaLojas(List<Loja> l){
        for (Loja temp : l) {
        this.lojas.addLoja(temp);
     }
   }
   
    public void addListaVoluntarios(List<Voluntario> v){
        for (Voluntario temp : v) {
        this.voluntarios.addVoluntario(temp);
     }
   }
   
    public void addListaEncomendas(List<Encomenda> v){
        for (Encomenda temp : v) {
        this.encomendas.addEncomenda(temp);
     }
   }
   
    public void addListaRV(List<RealizadaVoluntario> v){
        for (RealizadaVoluntario temp : v) {
        this.encomendas.addRealizadaVoluntario(temp);
     }
   }
   
   
    public void addListaRE(List<RealizadaEmpresa> v){
        for (RealizadaEmpresa temp : v) {
        this.encomendas.addRealizadaEmpresa(temp);
     }
   }
   
   //metodos de listagens de entidades presentes no sistema para o admin ver
   public List<Cliente> listagemClientes(){
       ArrayList<Cliente> list = new ArrayList<Cliente>(this.getClientes().values());
       return list;
    }
    
    public List<Empresa> listagemEmpresas(){
       ArrayList<Empresa> list = new ArrayList<Empresa>(this.getEmpresas().values());
       return list;
    }
    
    public List<Voluntario> listagemVoluntarios(){
       ArrayList<Voluntario> list = new ArrayList<Voluntario>(this.getVoluntarios().values());
       return list;
    }
    
    public List<Loja> listagemLojas(){
       ArrayList<Loja> list = new ArrayList<Loja>(this.getLojas().values());
       return list;
    }
    
    public List<Encomenda> listagemEncomendas(){
       ArrayList<Encomenda> list = new ArrayList<Encomenda>(this.getEncomendas().values());
       return list;
    }
    
    public List<RealizadaEmpresa> listagemEncomendasRealizadasEmpresas(){
        ArrayList<RealizadaEmpresa> list = new ArrayList<RealizadaEmpresa>(this.encomendas.getRealizadaEmpresa());
        return list;
    }
    
    public List<RealizadaVoluntario> listagemEncomendasRealizadasVoluntarios(){
        ArrayList<RealizadaVoluntario> list = new ArrayList<RealizadaVoluntario>(this.encomendas.getRealizadaVoluntario());
        return list;
    }
 
   public List<Pronta> listagemPedidos(){

        ArrayList<Encomenda> list = new ArrayList<Encomenda>(this.getEncomendas().values());
        ArrayList<Pronta> list2 = new ArrayList<Pronta>();

        for(Encomenda a : list) {
            if (a instanceof Pronta) list2.add((Pronta) a);
        }

        return list2;
    }
    
    //metodo que devolve a lista de encomendas de uma loja que ainda nao est�o prontas a ser entregues
    public List<Encomenda> listagemEncomendasNaoRespondidas(Loja l){
       ArrayList<Encomenda> list = new ArrayList<Encomenda>(this.getEncomendas().values());
        List<Encomenda> r = new ArrayList<>();
        
        for (Encomenda a : list)
            if(a.getFlagLojaPronta()==false && a.getLoja().getEmail().equals(l.getEmail()) ) r.add(a);
    return r;
    }
    
    //metodo que apresenta a lista de todas as encomendas de qualquer loja ja prontas a ser entregues
    public List<Encomenda> listagemEncomendasNaoRespondidas1(){
       ArrayList<Encomenda> list = new ArrayList<Encomenda>(this.getEncomendas().values());
        List<Encomenda> r = new ArrayList<>();
        
        for (Encomenda a : list)
            if(a.getFlagLojaPronta()==true && ( a instanceof RealizadaEmpresa==false) && (a instanceof RealizadaVoluntario==false) && (a instanceof Pronta==false)) r.add(a);
    return r;
    }
    
    //metodo para voluntario sinalizar e entregar encomenda
    public void voluntarioEntrega(String idE, Voluntario v) throws GestaoGeralException{
        if (this.getEncomendas().get(idE) == null) throw new GestaoGeralException(String.valueOf(idE));
        if (this.getEncomendas().get(idE).getFlagLojaPronta()==false)throw new GestaoGeralException(String.valueOf(idE));
        Encomenda pd=this.getEncomendas().get(idE);
       
        double lat1=pd.getCliente().getLocalizacao().getX();
        double lat2=pd.getLoja().getLocalizacao().getX();
        double lon1=pd.getCliente().getLocalizacao().getY();
        double lon2=pd.getLoja().getLocalizacao().getY();
        double lat3=v.getLocalizacao().getX();
        double lon3=v.getLocalizacao().getY();
        double distanciaViagem=distance(lat2,lat3,lon2,lon3)+ distance(lat1,lat2,lon1,lon2);
        if(distanciaViagem>getVoluntarios().get(v.getEmail()).getRaiogeografico()) throw new GestaoGeralException(String.valueOf(idE));
        else{
            LocalDate data = LocalDate.now();
            RealizadaVoluntario novo=new RealizadaVoluntario(idE,pd.getCliente(),pd.getLoja(),pd.getPeso(),pd.getState(),pd.getData(),true,true,pd.getLinhas(),v,false,-1);
            novo.setData(data);
            Cliente c1 = getClientes().get(pd.getCliente().getEmail());
            c1.atualizaLV(novo);
            clientes.addCliente(c1);
            Voluntario v1= getVoluntarios().get(v.getEmail());
            v1.atualizaLV(novo);
            voluntarios.addVoluntario(v1);
            this.encomendas.addRealizadaVoluntario(novo);
        }
    }
    
    //metodo para empresa mandar pedido a cliente se aceita transportar
    public void empresaPede(String idE, Empresa e) throws GestaoGeralException{
        if (this.getEncomendas().get(idE) == null) throw new GestaoGeralException(String.valueOf(idE));
        if (this.getEncomendas().get(idE).getFlagLojaPronta()==false  || this.getEncomendas().get(idE).getRespostaCliente()==true)throw new GestaoGeralException(String.valueOf(idE));
        Encomenda pd=this.getEncomendas().get(idE);
        double lat1=pd.getCliente().getLocalizacao().getX();
        double lat2=pd.getLoja().getLocalizacao().getX();
        double lon1=pd.getCliente().getLocalizacao().getY();
        double lon2=pd.getLoja().getLocalizacao().getY();
        double lat3=e.getLocalizacao().getX();
        double lon3=e.getLocalizacao().getY();
        double distanciaViagem=distance(lat2,lat3,lon2,lon3)+ distance(lat1,lat2,lon1,lon2);
        if(distanciaViagem>e.getRaiogeografico()) throw new GestaoGeralException(String.valueOf(idE));
        else{
        Date data = new Date();
        double preco=e.getTaxa()*(distanciaViagem);
        Pronta nova= new Pronta(idE,pd.getCliente(),pd.getLoja(),pd.getPeso(),pd.getState(),pd.getData(),true,true,pd.getLinhas(),e,preco);
        this.encomendas.addPronta(nova);
        }
    }
    
    
    //metodo para uma loja indicar que uma encomenda esta pronta
    public void registaEncomendaLoja(String idE, Loja l)throws GestaoGeralException{
        
        if (this.getEncomendas().get(idE) == null) throw new GestaoGeralException(String.valueOf(idE));
        if (this.getEncomendas().get(idE).getLoja().getEmail().equals(l.getEmail()) == false) throw new GestaoGeralException(String.valueOf(idE));
        if (this.getEncomendas().get(idE).getFlagLojaPronta()==true)throw new GestaoGeralException(String.valueOf(idE));
        else {
            Encomenda p= this.getEncomendas().get(idE);
            p.setFlagLojaPronta(true);
            this.encomendas.addEncomenda(p);
        }
    }
    
    
    
    
    //metodo para o cliente ver os pedidos que tem para responder
    public List<Pronta> listagemEncomendasProntas(Cliente c) {   
        List<Encomenda> list = new ArrayList<>(this.getEncomendas().values());
        List<Pronta> r = new ArrayList<>();

        for (Encomenda a : list)
            if(a instanceof Pronta && a.getCliente().getEmail().equals(c.getEmail()) ) r.add((Pronta)a);
    return r;
}

//metodos para dar a listagem de encomendas que clientes podem classificar
    public List<RealizadaEmpresa> listaClassificarEmpresa(Cliente c){
        return this.encomendas.realizadosClassificarClienteEmpresa(c);
    }
    
    public List<RealizadaVoluntario> listaClassificarVoluntario(Cliente c){
        return this.encomendas.realizadosClassificarClienteVoluntario(c);
    }

//metodos para classificar empresa e voluntarios
public void registaClassEmpresa(String idE,double classificacao,Cliente c,Empresa aux) throws GestaoGeralException{
  
        if (this.getEncomendas().get(idE) == null) throw new GestaoGeralException(String.valueOf(idE));
        if (this.getEncomendas().get(idE).getCliente().getEmail().equals(c.getEmail()) == false) throw new GestaoGeralException(String.valueOf(idE));
        if (((RealizadaEmpresa)(this.getEncomendas().get(idE))).getClassificado()==true )throw new GestaoGeralException(String.valueOf(idE));
        else {
            this.encomendas.classificacaoClienteEmpresa(idE,classificacao);
            this.empresas.atualizaClassificacaoEmpresa(classificacao,aux);
        }
    }
    
public void registaClassVoluntario(String idE,double classificacao,Cliente c,Voluntario v) throws GestaoGeralException{

        if (this.getEncomendas().get(idE) == null) throw new GestaoGeralException(String.valueOf(idE));
        if (this.getEncomendas().get(idE).getCliente().getEmail().equals(c.getEmail()) == false) throw new GestaoGeralException(String.valueOf(idE));
        if (((RealizadaVoluntario)(this.getEncomendas().get(idE))).getClassificado()==true )throw new GestaoGeralException(String.valueOf(idE));
        else {
            this.encomendas.classificacaoClienteVoluntario(idE,classificacao);
            this.voluntarios.atualizaClassificacaoVoluntario(classificacao,v);
        }
    }
    
    public void AceitaPedido(String idP,Cliente c) throws GestaoGeralException {
        if ((this.getEncomendas().get(idP)) instanceof RealizadaEmpresa) throw new GestaoGeralException(String.valueOf(idP));
        Pronta pd = (Pronta) this.getEncomendas().get(idP);
        if (pd == null) throw new GestaoGeralException(String.valueOf(idP));
        if (pd.getRespostaCliente()== false || pd.getFlagLojaPronta() == false) throw new GestaoGeralException(String.valueOf(idP));
        if (pd.getCliente().getEmail().equals(c.getEmail())==false) throw new GestaoGeralException(String.valueOf(idP));
        else {
            double lat1=pd.getCliente().getLocalizacao().getX();
            double lat2=pd.getLoja().getLocalizacao().getX();
            double lon1=pd.getCliente().getLocalizacao().getY();
            double lon2=pd.getLoja().getLocalizacao().getY();
            double lat3=pd.getEmpresa().getLocalizacao().getX();
            double lon3=pd.getEmpresa().getLocalizacao().getY();
            double distanciaViagem=distance(lat2,lat3,lon2,lon3)+ distance(lat1,lat2,lon1,lon2);
            double preco = pd.getPreco();
            LocalDate data = LocalDate.now();
            RealizadaEmpresa r = new RealizadaEmpresa(idP,c,pd.getLoja(),pd.getPeso(),pd.getState(),pd.getData(),true,true,pd.getLinhas(),pd.getEmpresa(),preco,distanciaViagem,false,-1);;
            r.setData(data);
            Cliente c1= getClientes().get(c.getEmail());
            c1.atualizaLE(r);
            clientes.addCliente(c1); 
            Empresa e1 = getEmpresas().get(pd.getEmpresa().getEmail());
            e1.atualizaLE(r);
            empresas.addEmpresa(e1);
            this.encomendas.addRealizadaEmpresa(r);
         
            
        }

    }
    
    //CALCULA A DISTANCIA EM METROS ENTRE DUAS COORDENAS GEOGRAFICAS
    public static double distance(double lat1, double lat2, double lon1,double lon2) {
    return Math.sqrt((Math.pow(lat1+lat2,2))+(Math.pow(lon1+lon2,2)));
}
    
    //metodos que devolvem encomendas entregues a um cliente por um determinado periodo
    public List<RealizadaEmpresa> getEncEmpPorPeriodo(Cliente c, LocalDate inicio, LocalDate fim) {
        return this.clientes.EncEmpPorPeriodo(c,inicio,fim);
    }
    
    public List<RealizadaVoluntario> getEncVolPorPeriodo(Cliente c, LocalDate inicio, LocalDate fim) {
        return this.clientes.EncVolPorPeriodo(c,inicio,fim);
    }
    
    //metodos que devolvem encomendas entregues por uma empresa por um determinado periodo
    public List<RealizadaEmpresa> getEncEPorPeriodo(Empresa e, LocalDate inicio, LocalDate fim){
        return this.empresas.EncEmpresaPorPeriodo(e,inicio,fim);
    }
    
    //metodos que devolvem encomendas entregues por um voluntario por um determinado periodo
    public List<RealizadaVoluntario> getEncVPorPeriodo(Voluntario v, LocalDate inicio, LocalDate fim){
        return this.voluntarios.EncVoluntarioPorPeriodo(v,inicio,fim);
    }
    
    
    // metodo p criar encomenda 
    
    public void criaEncomenda(Cliente c,Encomenda e,String idLoja) throws LojaNaoExisteException{
        if (this.getLojas().get(idLoja) == null) throw new LojaNaoExisteException(String.valueOf(idLoja));
        this.encomendas.addEncomenda(e);
    }
        
    
    //metodo faturacao por periodo
    public double faturacaoPeriodo(Empresa e, LocalDate inicio, LocalDate fim){
        return this.encomendas.faturacao(e.getEmail(),inicio,fim);
    }
    
    //metodo para ranking dos 10 clientes que t?m mais encomendas entregues
     public TreeSet<Cliente> rank10Vezes() {
        TreeSet<Cliente> t = new TreeSet<>(new ComparadorNrVezes());
        ArrayList<Cliente> clientes1 = new ArrayList<Cliente> (this.getClientes().values());
        for (Cliente c : clientes1)
             t.add(c.clone());
        return t;
    }
    
    //metodo para ranking das 10 empresas com mais kms percorridos
     public TreeSet<Empresa> rank10km() {
        TreeSet<Empresa> t = new TreeSet<>(new ComparadorKmPercorridos());
        ArrayList<Empresa> empresas1 = new ArrayList<Empresa>(this.getEmpresas().values());
        for (Empresa e : empresas1)
            t.add(e.clone());
        return t;
    }
        
    
    
     //metodo para gravar estado
    public void guardaEstado() throws FileNotFoundException,IOException {

        FileOutputStream fos1 = new FileOutputStream("GClientes");
        FileOutputStream fos2 = new FileOutputStream("GEmpresas");
        FileOutputStream fos3 = new FileOutputStream("GVoluntarios");
        FileOutputStream fos4 = new FileOutputStream("GLojas");
        FileOutputStream fos5 = new FileOutputStream("GEncomendas");
        ObjectOutputStream oos1 = new ObjectOutputStream(fos1);
        ObjectOutputStream oos2 = new ObjectOutputStream(fos2);
        ObjectOutputStream oos3 = new ObjectOutputStream(fos3);
        ObjectOutputStream oos4 = new ObjectOutputStream(fos4);
        ObjectOutputStream oos5 = new ObjectOutputStream(fos5);
        oos1.writeObject(this.clientes);
        oos2.writeObject(this.empresas);
        oos3.writeObject(this.voluntarios);
        oos4.writeObject(this.lojas);
        oos5.writeObject(this.encomendas);
        oos1.flush(); oos2.flush(); oos3.flush(); oos4.flush(); oos5.flush();
        oos1.close(); oos2.close(); oos3.close(); oos4.close(); oos5.close();

    }

    //metodo para carregar estado
    public void carregaEstado() throws FileNotFoundException,IOException, ClassNotFoundException{
        FileInputStream fis1 = new FileInputStream("GClientes");
        FileInputStream fis2 = new FileInputStream("GEmpresas");
        FileInputStream fis3 = new FileInputStream("GVoluntarios");
        FileInputStream fis4 = new FileInputStream("GLojas");
        FileInputStream fis5 = new FileInputStream("GEncomendas");
        ObjectInputStream ois1 = new ObjectInputStream(fis1);
        ObjectInputStream ois2 = new ObjectInputStream(fis2);
        ObjectInputStream ois3 = new ObjectInputStream(fis3);
        ObjectInputStream ois4 = new ObjectInputStream(fis4);
        ObjectInputStream ois5 = new ObjectInputStream(fis5);
        clientes = (GestaoCliente) ois1.readObject();
        empresas = (GestaoEmpresa) ois2.readObject();
        voluntarios = (GestaoVoluntario) ois3.readObject();
        lojas = (GestaoLojas) ois4.readObject();
        encomendas = (GestaoEncomenda) ois5.readObject();
        ois1.close(); ois2.close(); ois3.close(); ois4.close(); ois5.close();
    }
}
   
   
    
        

