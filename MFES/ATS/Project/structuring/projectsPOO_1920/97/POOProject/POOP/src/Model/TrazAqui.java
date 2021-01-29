package Model;
import java.io.*;
import java.time.LocalDateTime;
import java.util.*;
import Exceptions.*;

/**
 * Classe que representa a parte Model do projeto
 */

public class TrazAqui implements Serializable{
    private static final long serialVersionUID = 7582768867862556061L;

    private UsersDB users;
    private LojasDB lojas;
    private VoluntariosDB voluntarios;
    private EmpresasTransDB empresas;
    private EncomendasDB encomendas;
    private EncomendasAceitesDB encomendasA;
    private int utilizadorN;
    private int lojaN;
    private int empresaN;
    private int voluntarioN;
    private int encomendaN;

    public TrazAqui(){
        this.users= new UsersDB();
        this.lojas= new LojasDB();
        this.voluntarios= new VoluntariosDB();
        this.empresas= new EmpresasTransDB();
        this.encomendas= new EncomendasDB();
        this.encomendasA = new EncomendasAceitesDB();
        this.utilizadorN = 1;
        this.lojaN = 1;
        this.empresaN = 1;
        this.voluntarioN = 1;
        this.encomendaN = 1000;

    }

    public TrazAqui(UsersDB u,LojasDB l,VoluntariosDB v,EmpresasTransDB e, EncomendasDB enc,EncomendasAceitesDB encA){
        this.users= u;
        this.lojas= l;
        this.voluntarios= v;
        this.empresas= e;
        this.encomendas= enc;
        this.encomendasA = encA;
    }
    /**
     * Guarda a informação que o parse carrega para o sistema
     */
    
    public void fazParse (String file) throws UserJaExisteException,JaExisteLojaException,
                                              JaExisteVoluntarioException,JaExisteEmpresaException,
                                              JaExisteEncomendaException,JaExisteEncomendaAceiteException{
        Parse p = new Parse();
        p.parse(file);
        this.users = p.users;
        this.lojas = p.lojas;
        this.voluntarios = p.voluntarios;
        this.empresas = p.empresas;
        this.encomendas = p.encomendas;
        this.encomendasA = p.encomendasA;
    }
    /**
     * Getter de uma loja dado o codigo de uma loja
     */

    public Lojas getLoja (String lojaID) throws NaoExisteLojaException{  
        return this.lojas.getLoja(lojaID);
    }

     /**
     * Lista das lojas
     */

    public List<Lojas> getLojas(){
        return this.lojas.getLojas();
    }
    
     /**
     * Getter de uma empresa Transportadora dado o seu codigo
     */

    public EmpresasTrans getTransportadora (String empID) throws NaoExisteEmpresaException{  
        return this.empresas.getEmpresa(empID);
    }

     /**
     * Getter de um voluntatio dado o seu codigo 
     */

    public Voluntarios getVoluntario (String VoluntarioID) throws NaoExisteVoluntarioException{  
        return this.voluntarios.getvoluntario(VoluntarioID);
    }

     /**
     * Getter de um user dado o seu codigo 
     */

    public User getUser(String userID) throws UserInexistenteException{  
        return this.users.getUser(userID);
    }
    
     /**
     * Método que retorna os clientes que realizaram um maior numero de encomendas
     */

    public Set<User> getBestClientNrEnc(){
        Set<User> u = new TreeSet<>(new ComparatorNumeroEncomendas());
        Set<User> l = this.users.getUsers();
        for(User user : l ){
            u.add(user);
        }
        return u;
    }
    
     /**
     * Método que retorna as empresas que efetuaram um maior numero de kms
     */

    public Set<EmpresasTrans> getBestEmpresaKm(){
        Set<EmpresasTrans> emp = new TreeSet<>(new ComparatorKmsFeitos());
        Set<EmpresasTrans> l = this.empresas.getEmpresas();
        for(EmpresasTrans e : l){
            emp.add(e);
        }
        return emp;
    }

     /**
     * Método que verifica se o email é válido
     */

    public void verificaMail(String email) throws EmailExistenteException{
        this.users.verificaMail(email);
    }

    /**
     * Método que adiciona um utilizador
     */
    
    public void addUser(User a) throws UserJaExisteException{
        this.users.addUtilizador(a); 
    }

     /**
     * Método que adiciona o utilizador criado nos menus
     */

    public void addUserCriado(User a){
        try{
        String codigo = a.getCodUtilizador() + this.utilizadorN;
        a.setCodUtilizador(codigo);
        this.utilizadorN++;
        this.users.addUtilizador(a);
        }catch(UserJaExisteException e){
            this.utilizadorN++;
        } 
    }

    
    /**
     * Método que adiciona uma encomenda
     */

    public void addEncomendas(Encomendas a) throws JaExisteEncomendaException {
        this.encomendas.addEncomenda(a);
    }

    /**
     * Método que adiciona uma encomenda criada nos menus
     */

    public Encomendas addEncomendaCriada (Encomendas a) throws JaExisteEncomendaException {
        String codigo = a.getCodEncomenda() + this.encomendaN;
        a.setCodEncomenda(codigo);
        this.encomendaN++;
        this.encomendas.addEncomenda(a);
        return a;
    }    
    

    /**
     * Método que adiciona uma empresa 
     */

    public void addEmpresa(EmpresasTrans a) throws JaExisteEmpresaException {
        this.empresas.addEmpresa(a);
    }

    /**
     * Método que adiciona uma empresa criada nos menus
     */

    public void addEmpresaCriada(EmpresasTrans a) {
        try{
        String codigo = a.getCodEmpresa() + this.empresaN;
        a.setCodEmpresa(codigo);
        this.empresaN++;
        this.empresas.addEmpresa(a);
        }catch(JaExisteEmpresaException e){
            this.empresaN++;
        }
    }


    /**
     * Método que adiciona um voluntário
     */

    public void addVoluntario(Voluntarios a) throws JaExisteVoluntarioException {
        this.voluntarios.addVoluntario(a);
    }
    /**
     * Método que adiciona um voluntário criado nos menus
     */
    
    public void addVoluntarioCriado(Voluntarios a){
        try{
        String codigo = a.getCodVoluntario() + this.voluntarioN;
        a.setCodVoluntario(codigo);
        this.voluntarioN++;
        this.voluntarios.addVoluntario(a);
        }catch(JaExisteVoluntarioException e){
            this.voluntarioN++;
        }
    }


     /**
     * Método que adiciona uma loja
     */
    
    public void addLoja(Lojas a) throws JaExisteLojaException {
        this.lojas.addLoja(a);
    }

     /**
     * Método que adiciona uma loja criada nos menus
     */
    
    public void addLojaCriada(Lojas a) {
        try{
        String codigo = a.getCodLoja() + this.lojaN;
        a.setCodLoja(codigo);
        this.lojaN++;
        this.lojas.addLoja(a);
        }catch(JaExisteLojaException e){
            this.lojaN++;
        }
    }

     /**
     * Método que adiciona um produto 
     */

    public void adicionaProduto(String codLoja, LinhaEncomenda l) throws JaExisteProdutoException{
        this.lojas.adicionaProduto(codLoja, l);
    }


    /**
     * Método que altera o estado de disponibilidade de um voluntário
     */
    
    public void swapState(Voluntarios v) {
        v.swapState();
    }

    /**
     * Método que altera se um voluntário transporta encomendas médicas ou não
     */
    
    public void swapStateMed(Voluntarios v) {
        v.swapStateMed();
    }

    /**
     * Método que altera o estado de disponibilidade das empresas Transportadoras
     */

    public void swapState(EmpresasTrans emp) {
        emp.swapState();
    }

    /**
     * Método que altera se uma empresa transporta encomendas médicas ou não
     */

    public void swapStateMed(EmpresasTrans emp) {
        emp.swapStateMed();
    }


    /**
     * Método que permite a um utilizador fazer login
     */

    public User logIn(String username, String passwd) throws UserInexistenteException, PasswordErradaExecption,NaoExisteEmailException {
        User c = users.getUserEmail(username);
        if(c==null)throw new UserInexistenteException();
        if(!(c.getPassword().equals(passwd)))
            throw new PasswordErradaExecption();
        return c;
    }

    /**
     * Método que permite a uma loja fazer login
     */

    public Lojas logInL(String username, String passwd) throws NaoExisteLojaException, PasswordErradaExecption {
        Lojas l = lojas.getLoja(username);
        if(l==null)throw new NaoExisteLojaException();
        if(!(l.getPassword().equals(passwd)))
            throw new PasswordErradaExecption();
        return l;
    }

    /**
     * Método que permite a um voluntário fazer login
     */

    public Voluntarios logInV(String username, String passwd) throws NaoExisteVoluntarioException, PasswordErradaExecption {
        Voluntarios v = voluntarios.getvoluntario(username);
        if(v==null)throw new NaoExisteVoluntarioException();
        if(!(v.getPassword().equals(passwd)))
            throw new PasswordErradaExecption();
        return v;
    }
    
    /**
     * Método que permite a uma empresa Transportadora fazer login
     */

    public EmpresasTrans logInN(String username, String passwd) throws NaoExisteEmpresaException, PasswordErradaExecption {
        EmpresasTrans e = empresas.getEmpresa(username);
        if(e==null)throw new NaoExisteEmpresaException();
        if(!(e.getPassword().equals(passwd)))
            throw new NaoExisteEmpresaException();
        return e;
    }
    
    /**
     * Método que retorna a lista de encomendas de um utilizador num determindado espaço de tempo
     */
    public List<Encomendas> getListEncUtilizaforDH(User client, LocalDateTime init, LocalDateTime end) throws UserInexistenteException{
    return this.users.getUser(client.getCodUtilizador()).getListencUDH(init,end);
    }

     /**
     * Método que retorna a lista de encomendas de um utilizador
     */
    public List<Encomendas> getListEncUtilizador(User client)throws UserInexistenteException {
        return this.users.getUser(client.getCodUtilizador()).getEncFeitas();
    }

    /**
     * Dado um voluntario e um intervalo de tempo, devolve a lista das encomendas transportadas pelo mesmo nesse intervalo
     */
    public List<Encomendas> getListEncVoluntariosDH(Voluntarios v,LocalDateTime init, LocalDateTime end)
            throws NaoExisteVoluntarioException {
        return this.voluntarios.getvoluntario(v.getCodVoluntario()).getListencVDH(init,end);
    }

    /**
     * Metodo que dado um voluntario devolve a lista das encomendas transportadas pelo mesmo
     */
    public List<Encomendas> getListEncVoluntario(Voluntarios v) throws NaoExisteVoluntarioException {
        return this.voluntarios.getvoluntario(v.getCodVoluntario()).getEnc();
    }

    /**
     * Metodo que dada uma empresa e um intervalo de tempo, devolve a lista das encomendas transportadas pela mesma nesse intervalo
     */
    public List<Encomendas> getListEncEmpresasDH(EmpresasTrans emp,LocalDateTime init, LocalDateTime end)
            throws NaoExisteEmpresaException {
        return this.empresas.getEmpresa(emp.getCodEmpresa()).getListencEDH(init,end);
    }

    /**
     * Metodo que dada uma empresa devolve um set das encomendas transportadas pela mesma 
     */
    public Set<Encomendas> getListEncEmpresas(EmpresasTrans emp) throws NaoExisteEmpresaException {
        return this.empresas.getEmpresa(emp.getCodEmpresa()).getEncomendas(); 
    }

    /**
     * Metodo que dada uma loja devolve a lista de encomendas registadas pelo mesmo
     */
    public List<Encomendas> getListEncLoja(Lojas loja) throws NaoExisteLojaException {
        return this.lojas.getLoja(loja.getCodLoja()).getEncR();
    }
    
    /**
     * Metodo que dada uma loja e um intervalo de tempo, devolve a lista de encomendas registadas nesse periodo
     */
    public List<Encomendas> getListEncLojaDH(Lojas l,LocalDateTime init, LocalDateTime end) throws NaoExisteLojaException {
        return this.lojas.getLoja(l.getCodLoja()).getListencLDH(init,end);
    }

    /**
     * Metodo que dado o codigo de uma loja, devolve os produtos que esta possui
     */
    public List<LinhaEncomenda> getProdutos(String codLoja) throws NaoExisteLojaException{
        return this.lojas.getProdutos(codLoja);
    }

    /**
     * Dada uma empresa e um intervalo de tempo, devolve o total faturado por essa empresa nesse intervalo de tempo
     */
    public double getTotalFaturadoE(EmpresasTrans e, LocalDateTime init, LocalDateTime end) throws UserInexistenteException, NaoExisteLojaException {
        double soma=0;
        Set<Encomendas> encomendas = e.getEncomendas();
        for(Encomendas enc: encomendas){
            User u = this.users.getUser(enc.getCodUtilizador());
            Lojas l = this.lojas.getLoja(enc.getNomeLoja());
            if(enc.getDataHora().isBefore(end)&&enc.getDataHora().isAfter(init))
                soma+=e.calculaPrecoTotal(u,enc,l);
        }
        return soma;
    }

    /**
     * Metodo que dado o codigo de empresa obtem a sua classificacao
     */
    public double getClassificacaoE(String empresaID) throws NaoExisteEmpresaException {
        return this.empresas.getEmpresa(empresaID).getClassificacoes();
    }

    /**
     * Metodo que dado um codigo de voluntario obtem a sua classificacao
     */
    public double getClassificacaoV(String voluntarioID) throws NaoExisteVoluntarioException {
        return this.voluntarios.getvoluntario(voluntarioID).getClassi();
    }


   /**
    * Algoritmo responsavel por uma serie de coisas, todas relacionadas com o pedido de uma encomenda por parte de um utilizador e o transporte 
    bem como a gestao gestao do seu tempo e custo.
    */

    public String utFazPedidoE(Encomendas e, String utilizador) throws UserInexistenteException, NaoExisteLojaException, NaoExisteEncomendaException, UserForaDeAlcanceException,
            OpcaoInvalidaException {
      int i=0;                  

      String cdU = utilizador;
      User c =this.users.getUser(cdU);
      String cdL =e.getNomeLoja();
      Lojas l = this.lojas.getLoja(cdL);

      boolean isMed = e.isMed;

        List<Voluntarios> vM = new ArrayList<>();
        List<Voluntarios> v = new ArrayList<>();
        List<EmpresasTrans> emp=new ArrayList<>();
        List<EmpresasTrans> empM= new ArrayList<>();
        Voluntarios vol = null;
        Voluntarios volM = null;

      if(isMed){
          vM = voluntarios.volNoRaioMedL(l,c);
          if(vM.size()>0) {
            volM = vM.get(0);
          }
          empM = empresas.empresasNoRaioMed(l.getLat(),l.getLon(),c.getLatitude(),c.getLongitude());
      }
      else {
          v = voluntarios.volNoRaioL(l,c);
          if(v.size()>0) {
              vol = v.get(0);
          }
          emp = empresas.empresasNoRaio(l.getLat(),l.getLon(),c.getLatitude(),c.getLongitude());
          
      }
  

      if(v.isEmpty() && vM.isEmpty() && emp.isEmpty() && empM.isEmpty()){
          throw new UserForaDeAlcanceException();
      } 

      EncomendaAceite ea = new EncomendaAceite(e.getCodEncomenda());
      boolean aceitaEmp=false;

      if (vol!=null){
          l.adicionaEncomendas(ea);
          l.produzEncomenda(this.encomendas);
          vol.buscarEncomenda(l,e,c);
          c.addEncHist(e);
          return vol.getCodVoluntario();
      }
      if(volM!=null) {
          l.adicionaEncomendas(ea);
          l.produzEncomenda(this.encomendas);
          volM.buscarEncomenda(l,e,c);
          c.addEncHist(e);
          return volM.getCodVoluntario();
      }
     if(emp!=null) {

        for (i =0; i < emp.size()  ;i++) {
             EmpresasTrans ep = emp.get(i);
             double custo = ep.calculaPrecoTotal(c, e, l);
             double t = ep.estimaTempo(c, l);
             aceitaEmp = c.aceitarEmpresaTr(custo, t,ep); 
             if(aceitaEmp) {
             l.adicionaEncomendas(ea);
             l.produzEncomenda(this.encomendas);
             double dist = ep.calculaDistanciaEnc(l,c);
             custoTempo ct = new custoTempo();
             ct = ep.buscarenc(c,l,dist,e);
             ep.adicionaEncomenda(e, ct);
             c.addEncHist(e);
             //System.out.println("Empresa Aqui:" + ep.toString());
             //Scanner sc = new Scanner(System.in);
             //int ale = sc.nextInt();
             return ep.getCodEmpresa();
             }
     }
    }

     else{
           for(i = 0;i<empM.size() ;i++ ) {
               EmpresasTrans ep = empM.get(i);
               double custo = ep.calculaPrecoTotal(c,e,l);
               double t = ep.estimaTempo(c,l);
               aceitaEmp = c.aceitarEmpresaTr(custo,t,ep);
               if (aceitaEmp) {
                   l.adicionaEncomendas(ea);
                   l.produzEncomenda(this.encomendas);
                   double dist = ep.calculaDistanciaEnc(l,c);
                   custoTempo ct = new custoTempo();
                   ct = ep.buscarenc(c,l,dist,e);
                   ep.adicionaEncomenda(e, ct);
                   c.addEncHist(e);
                   //System.out.println("Empresa Aqui:" + ep.toString());
                   //Scanner sc = new Scanner(System.in);
                   //int ale = sc.nextInt();
                   return ep.getCodEmpresa();
               }
      }
    }
     return null;
    
    }

    /**
     * Metodo responsavel pela salvaguarda da informacao no sistema 
     */
    public void save(String fName) throws IOException {
        FileOutputStream a = new FileOutputStream(fName);
        ObjectOutputStream r = new ObjectOutputStream(a);
        r.writeObject(this);
        r.flush();
        r.close();
    }

    /**
     * Metodo responsavel pela leitura do ficheiro Binario onde é guardada a informação
     */
    public TrazAqui read(String fName)throws IOException ,ClassNotFoundException {
        FileInputStream r = new FileInputStream(fName);
        ObjectInputStream a = new ObjectInputStream(r);
        TrazAqui u = (TrazAqui) a.readObject();
        a.close();
        return u;
    }

  
}