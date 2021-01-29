import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class Gestor implements Serializable {

    private CatEncomenda ce;
    private CatVoluntarios cv;
    private CatTransportadoras ct;
    private CatLojas cl;
    private CatUtilizadores cu;
    private Aceite a;
    private Historico h;
    private RandomEvents r;

    public Gestor(){
        this.ce = new CatEncomenda();
        this.cv = new CatVoluntarios();
        this.ct = new CatTransportadoras();
        this.cl = new CatLojas();
        this.cu = new CatUtilizadores();
        this.a = new Aceite();
        this.h = new Historico();
        this.r = new RandomEvents();
    }

    /**Método que faz load de todos os catálogos*/
    public void loadCat(){
        Parse.parse(this.cu,this.cl,this.cv,this.ct,this.ce,this.a);
    }

    /**Método que regista um utilizador no seu catálogo
     *
     * @param cod Código user
     * @param nome Nome user
     * @param cord Coordenadas
     * @param pass Password
     * @param mail Email
     */
    public void registarUser (String cod, String nome, Coordenadas cord, String pass, String mail){
        this.cu.signupU(cod,nome,cord,pass,mail);
    }

    /**Método que regista uma transportadora no seu catálogo
     *
     * @param cod Código Transportadora
     * @param nome Nome Transportadora
     * @param cord Coordenadas
     * @param range Range
     * @param prkm Preço
     * @param nif NIF
     * @param clas Classificação
     * @param med Medicamentos
     * @param vkm Velocidade
     * @param email Email
     * @param pass Password
     * @param kmp Kilómetros percorridos
     * @param cap Capacidade
     */
    public void registarEmpresa (String cod, String nome, Coordenadas cord, double range, double prkm, double nif, double clas, boolean med,double vkm, String email, String pass,double kmp, int cap){
        this.ct.signupT(cod, nome, cord, range, prkm, nif, clas, med,vkm , email, pass,kmp,cap);
    }

    /**Método que regista um voluntário no seu catálogo
     *
     * @param cod Código voluntário
     * @param nome Nome voluntário
     * @param range Range
     * @param cord Coordenadas
     * @param clas Classificação
     * @param med Medicamentos
     * @param vkm Velocidade
     * @param email Email
     * @param pass Password
     * @param cap Capacidade
     */
    public void registarVoluntario (String cod, String nome, double range, Coordenadas cord, double clas, boolean med, double vkm, String email, String pass, int cap){
        this.cv.signupV(cod, nome, range,cord, clas, med,vkm, email,pass,cap);
    }

    /**Método que regista uma Loja no seu catálogo
     *
     * @param cod Código loja
     * @param nome Nome loja
     * @param cord Coordenadas
     * @param fila Fila
     * @param email Email
     * @param pass Password
     */
    public void registarLoja (String cod, String nome, Coordenadas cord,int fila, String email, String pass){
        this.cl.signupL(cod, nome,cord, fila, email,pass);
    }

    /**Método que regista uma encomenda no seu catálogo
     *
     * @param a Encomenda
     */
    public void registarEnc (Encomenda a){
        this.ce.adicionaEnc(a);
    }

    /**Método que dependendo do modo introduzido faz o login de um utilizador, transportadora, voluntário ou loja
     *
     * @param modo Modo
     * @param e Email
     * @param p Password
     */
    public String login (int modo, String e, String p){
        if (modo == 1) return this.cu.loginU(e,p);
        else if (modo == 2) return this.ct.loginT(e,p);
        else if (modo == 3) return this.cv.loginV(e,p);
        else return this.cl.loginL(e,p);
    }

    /**Método que dependendo do modo introduzido faz o sign up de um utilizador, transportadora, voluntário ou loja
     *
     * @param modo Modo
     * @param email Email
     */
    public boolean verificaMail (int modo, String email){
        if (modo == 1) return this.cu.verMail(email);
        else if (modo == 2) return this.ct.verMail(email);
        else if (modo == 3) return this.cv.verMail(email);
        else return this.cl.verMail(email);
    }

    /**Método que gera códigos únicos de empresa, tranportadora, utilizador ou voluntário
     *
     * @param i indíce catálogo
     */
    public String geraCods (int i) {
        if (i==0) return this.ce.codUnicoE();
        else if (i==1) return this.ct.codUnicoT();
        else if (i==2) return this.cu.codUnicoU();
        else if (i==3) return this.cv.codUnicoV();
        else return this.cl.codUnicoL();
    }

    /**Método que retorna uma lista de lojas num determinado range
     *
     * @param a Coordenadas
     * @param b Distância
     */
    public List<Loja> getLojasinRange (Coordenadas a, double b) {
        return this.cl.LojasinRange(a,b);
    }

    /**Método que dando um código de utilizador retorna o utilizador com esse código do respectivo catálogo
     *
     * @param cod código de utilizador
     */
    public Utilizador procUtil(String cod){
        return this.cu.procUser(cod);
    }

    /**Método que dado as coordenas da loja e do utilizador e um boolean de prioridade retorna uma lista de StringDistAux ordenada por distâncias  respectiva às Lojas com melhores condições
     *
     * @param cu Coordenadas utilizador
     * @param cl Coordenadas loja
     * @param pri prioridade
     */
    public List<StringDistAux> getBestLoja (Coordenadas cu, Coordenadas cl, boolean pri){
        List<StringDistAux> s = new ArrayList<>();
        s.addAll(this.ct.melhorTransporteT(cl,cu,pri));
        s.addAll(this.cv.melhorTransporteV(cl,cu,pri));
        s.sort(Comparator.comparing(StringDistAux::getDist));
        return s;
    }

    /**Método que remove um registo do modo pending e adiciona o mesmo ao modo accepted
     *
     * @param a Registo
     */
    public void avanca (Registos a ){
        double cl = this.cl.getTmpF(a.getLoja());
        Coordenadas l = this.cl.getCord(a.getLoja());
        Coordenadas u = this.cu.getCord(a.getUser());
        Registos f =this.h.removeReg(3,a);
        if (a.getDriver().charAt(0)=='v') {
            f.setTmp(this.cv.calcTmp(l, u, cl, a.getDriver()));
            f.setCustoT(0.0);
        }
        else {
            double custoT = this.ct.calcCost(l, u, a.getPeso(), a.getDriver());
            f.setTmp(this.ct.calcTmp(l, u, cl, a.getDriver()));
            f.setCustoT(custoT);
        }
        this.h.adicionaReg(1,f.clone());
    }

    /**Método que retorna uma lista de Registos de utilizador,empresa,voluntário ou loja no modo que for desejado
     *
     * @param cod Código
     * @param modo Modo
     */
    public List<Registos> histReg (String cod , int modo) {
        List<Registos> reg;
        if (cod.charAt(0) == 'u')
            reg = this.h.userHist(cod, modo);
        else if (cod.charAt(0) == 'v')
            reg = this.h.volHist(cod, modo);
        else if (cod.charAt(0) == 't')
            reg = this.h.empHist(cod, modo);
        else reg = this.h.lojaHist(cod,modo);

        reg.sort(new ComparatorData());
        return reg;
    }

    /**Método que retorna o total facturado num periodo de tempo
     *
     * @param cod Código transportadora
     * @param in Data inicial
     * @param fin Data final
     */
    public double totalFact(String cod, LocalDateTime in , LocalDateTime fin){
        return this.h.empHist(cod,5).stream().filter(x->x.getData().compareTo(in)>=0 && x.getData().compareTo(fin)<=0).mapToDouble(Registos::getCustoT).sum();
    }

    /**Método que retorna a Lista de topUsers*/
    public List<topUsers> topUsers(){
        return this.h.topUser();
    }

    /**Método que adiciona um novo registo à fila de terminated se a List<StringDistAux> estiver vazia e pending se não. Retorna 0 se for para a terminated e 1 se for para pending
     *
     * @param enc Encomenda
     * @param cod Código
     * @param a List StringDistAux
     */
    public int gestaoEncomenda (Encomenda enc, String cod, List<StringDistAux> a) {
        return this.h.gestaoEncomenda(enc,cod,a);
    }

    /**Método que faz a gestaoEncomenda para os registos declined
     *
     * @param a Registo
     */
    public void declinedT (Registos a){
        this.h.declinedT(a);
    }

    /**Método que finaliza a confirmação de encomenda caso seja completamente aceite, adicionando o registo a fila de delivered
     *
     * @param enc Código Encomenda
     * @param loja Código Loja
     * @param user Código Utilizador
     * @param driver Código Driver
     * @param tmp Tempo
     */
    public void getItDone (String enc,String loja, String user, String driver, double tmp){
        double b,cl = this.cl.getTmpF(loja);
        Coordenadas l = this.cl.getCord(loja);
        Coordenadas u = this.cu.getCord(user);

        if (driver.charAt(0)=='v'){
            Coordenadas v = this.cv.getCord(driver);
            b=tmp + 10/this.r.getSeedT(l)*5 + 10/this.r.getSeedC(l)*5
                    +10/this.r.getSeedT(u)*5 + 10/this.r.getSeedC(u)*5
                    +10/this.r.getSeedT(v)*5 + 10/this.r.getSeedC(v)*5;
            this.cv.setCoords(driver,u);
        }
        else{
            Coordenadas v = this.ct.getCord(driver);
            b=tmp+10/this.r.getSeedT(l)*5 + 10/this.r.getSeedC(l)*5
                    +10/this.r.getSeedT(u)*5 + 10/this.r.getSeedC(u)*5
                    +10/this.r.getSeedT(v)*5 + 10/this.r.getSeedC(v)*5;
            this.ct.addKmps(driver,l,u);
            this.ct.setCoords(driver,u);
        }
        this.h.adicionaReg(5,this.h.done(enc,b));
        this.h.removeFila(enc);
    }

    /**Método que classifica uma encomenda e adiciona o registo a fila de finalized
     *
     * @param a Registo
     * @param rating Rating
     */
    public void classifica (Registos a, double rating){
        Registos f = this.h.removeReg(5,a);
        if (f.getDriver().charAt(0)=='v'){
            this.cv.adicionaCla(f.getDriver(),rating);
        }
        else this.ct.adicionaCla(f.getDriver(),rating);
        this.h.adicionaReg(2,f);
    }

    /**Método que retorna a classificacao
     *
     * @param cod Código Driver
     */
    public double getClas (String cod){
        double a;
        if (cod.charAt(0)=='v') a=this.cv.getClas(cod);
        else a=this.ct.getClas(cod);
        return a;
    }

    /**Método que retorna uma seed random
     *
     * @param a Coordenadas
     * @param modo Modo
     */
    public double getRando (Coordenadas a, int modo){
        if (modo == 0) return this.r.getSeedC(a);
        else return this.r.getSeedT(a);
    }

    /**Método que retorna a coordenada de um voluntário ou empresa ou loja
     *
     * @param cod Código
     */
    public Coordenadas getCoord (String cod){
        Coordenadas a;
        if (cod.charAt(0) == 'v')
            a = this.cv.getCord(cod);
        else if (cod.charAt(0) == 't')
            a = this.ct.getCord(cod);
        else a =this.cl.getCord(cod);
            return a;
    }

    /**Método que adiciona os dados default aos registos */
    public void adicionaDef (){
        this.ce.adicionaReg(this.a.encDef()).forEach(x->this.h.adicionaReg(2,x));
    }

    /**Método que retorna uma lista dos topUsers (as dez transportadoras que mais usaram o sistema) */
    public List<topUsers> topTransp(){
        Map <String,topUsers> a  = new HashMap();

        for(Registos reg : this.h.finList()){
                a.putIfAbsent(reg.getDriver(), new topUsers(reg.getDriver()));
                a.get(reg.getDriver()).addKm(this.ct.getKmp(reg.getDriver()));
        }

        return a.values().stream().sorted(Comparator.comparing(topUsers::getQnt).reversed()).limit(10).collect(Collectors.toList());
    }

    /**Método que diminui a capacidade de um voluntário ou Empresa
     *
     * @param cod Código Driver
     */
    public void dimCap (String cod){
        if (cod.charAt(0) == 'v')
            this.cv.dimCap(cod);
        else this.ct.dimCap(cod);
    }

    /**Método que aumenta a capacidade de um voluntário ou Empresa
     *
     * @param cod Código Driver
     */
    public void aumCap (String cod){
        if (cod.charAt(0) == 'v')
            this.cv.aumCap(cod);
        else this.ct.aumCap(cod);
    }

    /**Método que retorna a capacidade de um voluntário ou Empresa
     *
     * @param cod Código Driver
     */
    public int getCap (String cod){
        if (cod.charAt(0) == 'v')
           return this.cv.getCap(cod);
        else return this.ct.getCap(cod);
    }
}
