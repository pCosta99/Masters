import java.time.LocalDateTime;
import java.util.List;
import java.util.Random;
import java.util.TreeMap;

/**
 * Classe controller que faz a comunicação com o model e a view
 */
public class Controller {
    private Registo reg;

    /**
     * Contrutor vazio
     */
    public Controller(){
        this.reg = new Registo();
    }

    /**
     * Construtor com argumentos
     * @param r Registo
     */
    public Controller(Registo r){
        this.reg = r.clone();
    }

    /**
     * Contrutor com um Controller
     * @param c Controller
     */
    public Controller(Controller c){
        this.reg = c.reg.clone();
    }

    /**
     * Devolve um Registo
     * @return Registo
     */
    public Registo getRegisto() {
        return reg.clone();
    }

    /**
     * Introduz o Registo
     * @param reg Registo
     */
    public void setRegisto(Registo reg) {
        this.reg = reg.clone();
    }

    /**
     * Verifica se existe um determinado registo
     * @param cod Codigo do RegistoTULV
     * @return Resultado da verificação
     */
    public boolean existeRegisto(String cod){
        return this.reg.existeRegisto(cod);
    }

    /**
     * Verifica se a password está correta
     * @param nome Código do RegistoTULV
     * @param pass Password introduzida
     * @return Resultado da verificação
     */
    public boolean verificaPass(String nome, String pass){
        return this.reg.verificaPass(nome,pass);
    }

    /**
     * Devolve as encomendas de um RegistoTULV
     * @param nome Codigo do RegistoTULV
     * @return Lista de EncDistr
     */
    public List<EncDistr> devolveMyEncs(String nome){
        return this.reg.devolveMyEncs(nome);
    }

    /**
     * Devolve as encomendas de um RegistoTULV
     * @param nome Codigo do RegistoTULV
     * @return Lista de EncDistr
     */
    public String devolveMyEncsToString(String nome) {
        StringBuilder sb = new StringBuilder();
        int i = 1;
        for (EncDistr e : this.reg.devolveMyEncs(nome)) {
            sb.append(i).append("\n").append(e).append("\n");
            i++;
        }
        return sb.toString();
    }

    /**
     * Devolve as encomendas pendentes de um RegistoTULV
     * @param nome Codigo do RegistoTULV
     * @return Lista de Encomendas
     */
    public List<Encomenda> devolveEncsPendentes(String nome){
        return this.reg.getRegistoTULV(nome).getEncPendentes();
    }

    /**
     * Devolve as encomendas pendentes de um RegistoTULV numa String
     * @param nome Codigo do RegistoTULV
     * @return Lista de Encomendas em String
     */
    public String devolveEncsPendentesToString(String nome){
        StringBuilder sb = new StringBuilder();
        int i = 1;
        for (Encomenda e : this.reg.getRegistoTULV(nome).getEncPendentes()) {
            sb.append(i).append("\n").append(e).append("\n");
            i++;
        }
        return sb.toString();
    }

    /**
     * Devolve as encomendas a avaliar de um Utilizador
     * @param nome Codigo do Utilizador
     * @return Lista de EncDistr
     */
    public List<EncDistr> devolveMyEncsAv(String nome){
        Utilizador u = (Utilizador) this.reg.getRegistoTULV(nome);
        return u.getMyEncsAv();
    }

    /**
     * Devolve as encomendas a avaliar de um Utilizador numa String
     * @param nome Codigo do Utilizador
     * @return Lista de EncDistr numa String
     */
    public String devolveMyEncsAvToString(String nome) {
        StringBuilder sb = new StringBuilder();
        Utilizador u = (Utilizador) this.reg.getRegistoTULV(nome);
        int i = 1;
        for (EncDistr e : u.getMyEncsAv()) {
            sb.append(i).append("\n").append(e).append("\n");
            i++;
        }
        return sb.toString();
    }

    /**
     * Devolve o Top10 de Utilizadores
     * @return Lista de Utilizadores
     */
    public String top10Utilizadores () {
        return reg.top10Utilizadores();
    }

    /**
     * Devolve o Top10 de Transportadoras
     * @return Lista de Transportadoras
     */
    public String top10Transportadoras(){
        return reg.top10Transportadoras();
    }

    /**
     * Calcula a faturação de um Registo num certo periodo
     * @param nome Código do RegistoTULV
     * @param d1 Data inicial
     * @param d2 Data final
     * @return Faturação
     */
    public double faturacaoPeriodoT(String d1, String d2, String nome){
        Transportadora d = (Transportadora) reg.getRegistoTULV(nome);
        return d.faturacaoPeriodo(d1, d2, reg);
    }

    /**
     * Dado um código, devolve o respetivo RegistoTULV
     * @param cod Codigo do RegistoTULV
     * @return RegistoTULV
     */
    public RegistoTULV devolveRegistoTULV(String cod){
        return reg.getRegistoTULV(cod);
    }

    /**
     * Dado um código de uma transporadora e uma classificação, adiciona essa classificação à transporadora
     * @param cod Código da Trasnportadora
     * @param classi Classificação a adicionar
     * @return Transporadora
     */
    public Transportadora classificaTransportadora(String cod, double classi){
        Transportadora lol = (Transportadora) reg.getRegistoTULV(cod);
        lol.addClassi(classi);
        TreeMap<String,RegistoTULV> ax = reg.getRegistos();
        ax.put(lol.getCodigo(),lol);
        reg.setRegistos(ax);
        return (Transportadora)ax.get(cod);
    }

    /**
     * Dado um código de um voluntário e uma classificação, adiciona essa classificação ao voluntário
     * @param cod Código da Voluntário
     * @param classi Classificação a adicionar
     * @return Voluntário
     */
    public Voluntario classificaVoluntario(String cod, double classi){
        Voluntario lol = (Voluntario) reg.getRegistoTULV(cod);
        lol.addClassi(classi);
        TreeMap<String,RegistoTULV> ax = reg.getRegistos();
        ax.put(lol.getCodigo(),lol);
        reg.setRegistos(ax);
        return (Voluntario)ax.get(cod);
    }

    /**
     * Dado um código de um Utilizador e uma EncDistr, remove a EncDistr da Lista de encomendas a avaliar
     * @param cod Código do Utilizador
     * @param e EncDistr
     */
    public void removeMyEncAv(String cod, EncDistr e){
        Utilizador u = (Utilizador) reg.getRegistoTULV(cod);
        u.removeMyEncAv(e);
        reg.putRegistoTULV(u);
    }

    /**
     * Verifica se o email está correto
     * @param email Email
     * @return Resultado da verificação
     */
    public boolean verificaEmail(String email){
        return reg.verificaEmail(email);
    }

    /**
     * Adiciona um RegistoTULV aos registos
     * @param r RegistoTULV
     * @param c Caractér que identifica o tipo de RegistoTULV
     */
    public void adicionaRegisto (RegistoTULV r, char c){
        reg.adicionaRegisto(r,c);
    }

    /**
     * Gera um novo Código para uma encomenda
     * @return Código
     */
    public String geraCodEncomenda(){
        return reg.geraCodEncomenda();
    }

    /**
     * Gera um novo Código para um produto
     * @return Código
     */
    public String geraCodProd(int cod){
        return reg.geraCodProduto(cod);
    }

    /**
     * Devolve todas as Lojas dos Registos
     * @return String de lojas
     */
    public String devolveLojas(){
        return reg.getLojas();
    }

    /**
     * Calcula a Trasportadora mais rápida a chegar de um Utilizador a uma Loja
     * @param cod Código do Utilizador
     * @param l Loja
     * @param fM Flag que diz se a encomenda é médica
     * @return Transportadora
     */
    public Transportadora devolveTranspFaster(String cod, Loja l, int fM){
        Utilizador u = (Utilizador) reg.getRegistoTULV(cod);
        return reg.transpFaster(u,l,fM);
    }

    /**
     * Calcula o Voluntário mais rápido a chegar de um Utilizador a uma Loja
     * @param cod Código do Utilizador
     * @param l Loja
     * @param fM Flag que diz se a encomenda é médica
     * @return Voluntário
     */
    public Voluntario devolveVolFaster(String cod, Loja l, int fM){
        Utilizador u = (Utilizador) reg.getRegistoTULV(cod);
        return reg.volFaster(u,l,fM);
    }

    /**
     * Finaliza uma encomenda realizada por um Utilizador, sendo um Voluntário o distribuidor
     * @param cod Código do Utilizador
     * @param e Encomenda
     * @param v Voluntário
     * @param l Loja
     */
    public void finalizaEncomendaV(Encomenda e, String cod,  Voluntario v, Loja l){
        Utilizador u = (Utilizador) reg.getRegistoTULV(cod);
        e.setDataA(LocalDateTime.now());
        EncDistr et = new EncDistr(e,v.getCodigo());
        v.inserirEncomenda(et); l.inserirEncomenda(et); u.inserirEncomenda(et);
        reg.atualizaRegistos(v,l,u);
    }

    /**
     * Finaliza uma encomenda realizada por um Utilizador, sendo uma Transportadora o distribuidor
     * @param cod Código do Utilizador
     * @param e Encomenda
     * @param t Transportadora
     * @param l Loja
     */
    public void finalizaEncomendaT(Encomenda e, String cod,  Transportadora t, Loja l){
        Utilizador u = (Utilizador) reg.getRegistoTULV(cod);
        e.setDataA(LocalDateTime.now());
        EncDistr et = new EncDistr(e,t.getCodigo());
        t.inserirEncomenda(et); l.inserirEncomenda(et); u.inserirEncomenda(et);
        reg.atualizaRegistos(t,l,u);
    }

    /**
     * Função que atualiza o estado da encomenda caso esta tenha sido aceite pelo distribuidor
     * @param trans Código do Distribuidor
     * @param e Encomenda
     */
    public void aceitarEncomenda (Encomenda e, String trans) {
        e.aceitaEncomenda();
        Transportadora t; Voluntario v;
        Loja l = (Loja) reg.getRegistoTULV(e.getCodLoja());
        Utilizador u = (Utilizador) reg.getRegistoTULV(e.getCodUtilizador());
        Random rs = new Random();
        if (trans.charAt(0) == 't') {
            t = (Transportadora) reg.getRegistoTULV(trans);
            e.setDataP(LocalDateTime.now());
            double levantamento = 60 * rs.nextDouble();
            e.setDataL(LocalDateTime.now().plusMinutes((long) levantamento));
            double dl = t.calculoTempoAteRecolher(l,e.getDataL());
            double recolher = levantamento  + (dl * 60);
            e.setDataR(LocalDateTime.now().plusMinutes((long) recolher));
            double du = (t.calculoTempoAteUser(l,u,e.getDataR())/2);
            double viagem = recolher + (du *60);
            e.setDataV(LocalDateTime.now().plusMinutes((long) viagem));
            e.setDataE(LocalDateTime.now().plusMinutes((long) ((long) ((du + dl) * 60)+viagem)));
            t.adicionaKm(t.calculoDistancia(l,u));
        }
        else if (trans.charAt(0) == 'v') {
            LocalDateTime prep;
            v = (Voluntario) reg.getRegistoTULV(trans);
            LocalDateTime x = v.dataUltEncomenda();
            if (v.dataUltEncomenda() == null || v.dataUltEncomenda().isBefore(LocalDateTime.now())) {
                prep = LocalDateTime.now();
                System.out.println("entrou if");
            }
            else {
                prep = x.plusMinutes((long) (180 * rs.nextDouble()));
                System.out.println("entrou else");
            }
            e.setDataP(prep);
            double levantamento = 60 * rs.nextDouble();
            e.setDataL(prep.plusMinutes((long) levantamento));
            double dl = v.calculoTempoAteRecolher(l,e.getDataL());
            double recolher = levantamento  + (dl * 60);
            e.setDataR(prep.plusMinutes((long) recolher));
            double du = (v.calculoTempoAteUser(l,u,e.getDataR())/2);
            double viagem = recolher + (du * 60);
            e.setDataV(prep.plusMinutes((long) viagem));
            e.setDataE(prep.plusMinutes((long) ((long) ((du + dl) * 60)+viagem)));
        }
        reg.alteraEstadoEncomenda(trans, e);
    }

    /**
     * Função que atualiza o estado da encomenda caso esta tenha sido rejeitada pelo distribuidor
     * @param trans Código do Distribuidor
     * @param e Encomenda
     */
    public void rejeitarEncomenda (Encomenda e, String trans) {
        e.rejeitaEncomenda();
        Voluntario v;
        Random rs = new Random();
        if (trans.charAt(0) == 't') {
            e.setDataP(LocalDateTime.now());
        }
        else if (trans.charAt(0) == 'v') {
            LocalDateTime prep;
            v = (Voluntario) reg.getRegistoTULV(trans);
            LocalDateTime x = v.dataUltEncomenda();
            prep = x.plusMinutes((long) (180 * rs.nextDouble()));
            e.setDataP(prep);
        }
        reg.alteraEstadoEncomenda(trans, e);
    }

    /**
     * Dado um código da Distribuidora e uma Encomenda, adiciona a Encomenda á Lista de encomendas a avaliar
     * @param trans Código da Distribuidora
     * @param e Encomenda
     */
    public void adicionaMyEncAv(Encomenda e, String trans) {
        Transportadora t; Voluntario v; EncDistr et = new EncDistr();
        if (trans.charAt(0) == 't') {
            t = (Transportadora) reg.getRegistoTULV(trans);
            et = new EncDistr(e,t.getCodigo());
        }
        else if (trans.charAt(0) == 'v') {
            v = (Voluntario) reg.getRegistoTULV(trans);
            et = new EncDistr(e,v.getCodigo());
        }
        reg.adicionaMyEncAv(et);
    }
}

