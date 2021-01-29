package Model;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.io.UncheckedIOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.stream.Collectors;
import Exceptions.InvalidInputException;
import Exceptions.NoEntityException;
import Utilities.Ponto;
import Utilities.Rating;
import View.PrintFormat;

import static java.util.stream.Collectors.toMap;


/**
 *   @class Gestao define a gestão da classe @class TrazAqui.
 */
public class Gestao implements Serializable, IGestao
{
    /*
     * variaveis de classe
     */
    private static final long serialVersionUID = 128L;
    private static final String GESTAO_SAVE_FILE = "gestao.dat";

    /*
     * variaveis de instancia
     */
    private IRegistoEncomendas registoEncomendas;
    private IBaseDeDados baseDeDados;

    /**
     * Construtores da classe RegistoEncomendas.
     */

    /**
     * Construtor por omissao de Gestao.
     */
    public Gestao() {
        this.registoEncomendas = new RegistoEncomendas();
        this.baseDeDados = new BaseDeDados();
    }

    /**
     * Construtor parametrizado de Gestao.
     */
    public Gestao( BaseDeDados baseDeDados,
                   RegistoEncomendas registoEncomendas) {
        setRegistoEncomendas(registoEncomendas);
        setBaseDeDados(baseDeDados);
    }

    /**
     * Construtor de copia de Gestao.
     */
    public Gestao(Gestao umaGestao) {
        setRegistoEncomendas(umaGestao.getRegistoEncomendas());
        setBaseDeDados(umaGestao.getBaseDeDados());
    }


    /**
     * metodos de instancia
     */

    //gets

    /**
     * Devolve o registo de encomendas da gestao do sistema.
     *
     * @return a gestao do sistema.
     */
    public IRegistoEncomendas getRegistoEncomendas() {
        return this.registoEncomendas.clone();
    }


    /**
     * Devolve a base de dados da gestao do sistema.
     *
     * @return base de dados.
     */
    public IBaseDeDados getBaseDeDados() {
        return this.baseDeDados.clone();
    }


    public Set<String> getCodEnc(){
        Set<String> encs = new HashSet<>();
        encs.addAll(this.registoEncomendas.getEncAceites().values().stream().map(m -> m.getCodigo()).collect(Collectors.toSet()));
        encs.addAll(this.registoEncomendas.getEncFinalizadas().values().stream().map(m -> m.getCodigo()).collect(Collectors.toSet()));
        encs.addAll(this.registoEncomendas.getEncPendentes().values().stream().map(m -> m.getCodigo()).collect(Collectors.toSet()));
        encs.addAll(this.registoEncomendas.getEncProntas().values().stream().map(m -> m.getCodigo()).collect(Collectors.toSet()));
        return encs;
    }


    //sets

    /**
     * Atualiza o registo de encomendas da gestao do sistema.
     *
     * @param novoRegistoEncomendas novo registo de encomendas
     */
    public void setRegistoEncomendas(IRegistoEncomendas novoRegistoEncomendas) {
        this.registoEncomendas = novoRegistoEncomendas.clone();
    }

    /**
     * Atualiza o a base de dados da base de dados.
     *
     * @param novoBaseDeDados nova base de dados
     */
    public void setBaseDeDados(IBaseDeDados novoBaseDeDados) {
        this.baseDeDados = novoBaseDeDados.clone();
    }

    //outros metodos obrigatorios

    /**
     * Metodo que devolve a representacao em String do Gestao.
     * @return String com o Registo de Encomendas e a Base de Dados que constituem o Gestao.
     */
    public String toString() {
        StringBuilder s=new StringBuilder();
        s.append("\nRegistoEncomendas: ");
        s.append(registoEncomendas.toString());
        s.append("\nBase De Dados: ");
        s.append(baseDeDados.toString());
        s.append("\n");
        return s.toString();
    }

    /**
     * Metodo que verifica se o Objeto o e igual ao Voluntario para o qual a funçao e chamada
     */
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if ((o == null) || (this.getClass() != o.getClass()))
            return false;
        Gestao p = (Gestao) o;
        return (   this.registoEncomendas.equals(p.getRegistoEncomendas())
                && this.baseDeDados.equals(p.getBaseDeDados()) );
    }

    /**
     * Metodo que faz uma  Increase font sizecopia do objecto receptor da mensagem.
     */

    public Gestao clone() {
        return new Gestao(this);
    }


    //metodos especificos

    //remoçoes

    /**
     * Metodo que remove um utilizador do sistema.
     */
    public void removeEntidade(String idEntidade) throws NoEntityException{
        this.baseDeDados.delEntidade(idEntidade);
    }

    //registo de novos dados (query 2)

    /**
     * Metodo que adiciona um utilizador ao sistema.
     */
    public void registaUtilizador(String id, String nome, Ponto posicao) throws InvalidInputException{
            this.baseDeDados.addUtilizador(id, nome, posicao);
    }

    /**
     * Metodo que adiciona um voluntario ao sistema.
     */
    public void registaVoluntario(String id, String nome, Ponto posicao, double raio) throws InvalidInputException{
            this.baseDeDados.addVoluntario(id, nome, posicao, raio) ;
    }

    /**
     * Metodo que adiciona uma transpostadora ao sistema.
     */
    public void registaTransportadora(String id, String nome, String nif, Ponto local,
                                      double raio, BigDecimal ppkm) throws InvalidInputException{
        this.baseDeDados.addTransportadora(id, nome, nif, local, raio, ppkm);
    }

    /**
     * Metodo que adiciona uma loja ao sistema.
     */
    public void registaLoja(String id, String nome, Ponto local) throws InvalidInputException{
        this.baseDeDados.addLoja(id, nome, local);
    }


    /**
     * Metodo que adiciona uma Encomenda.
     */
    public void registaNovaEncomenda(String cod, String codUtil, String codLoja, double peso,
                                     Set<LinhaEncomenda> lista, LocalDateTime data, LocalDateTime dataAceitacao, boolean medica) throws InvalidInputException, NoEntityException{
        if(this.baseDeDados.getUtilizador(codUtil) == null)
            throw new NoEntityException("utilizador \'" + codUtil + "\' inexistente");
        if(this.baseDeDados.getLoja(codLoja) == null)
            throw new NoEntityException("loja \'" + codLoja + "\' inexistente");
        this.registoEncomendas.adicionaPendentes(new Encomenda(cod, codUtil, codLoja, peso, lista, data, dataAceitacao, medica));
    }

    /**
     * Metodo que adiciona uma Encomenda Aceite.
     */
    public void registaAceite(String cod){
        this.registoEncomendas.adicionaAceites(cod);
    }

    public void registaConta(String id, String email, String password) throws InvalidInputException{
        this.baseDeDados.addConta(id, email, password);
    }


    public List<IEncomenda> encomendasDisponiveisParaEntrega(String dist) throws NoEntityException{
        ITransportadora d = this.baseDeDados.getTransportadora(dist);
        IVoluntario v = this.baseDeDados.getVoluntario(dist);
        if(d == null && v == null) throw new NoEntityException("distribuidor \'" + dist + "\' inexistente.");
        if(v == null){
            Predicate<IEncomenda> pt = e -> this.baseDeDados.getLoja(e.getCodLoja()).getPosicao().distancia(d.getPosicao()) <= d.getRaio()
                                    && !d.existeRejeitada(e.getCodigo());
            return this.registoEncomendas.getEncProntas().values().stream().filter(pt).collect(Collectors.toList()); 
        }else{
            Predicate<IEncomenda> pv = e -> this.baseDeDados.getLoja(e.getCodLoja()).getPosicao().distancia(v.getPosicao()) <= v.getRaio()
                               && (e.getMedica() == false ||  v.getTransportaMed() == e.getMedica());

            return this.registoEncomendas.getEncProntas().values().stream().filter(pv).collect(Collectors.toList());
        }
    }


    //gestao de açoes de recolha e entrega

    public Map<String, IEncomenda> encomendasDisponiveisParaEntrega (){
        return this.registoEncomendas.getEncProntas();
    }

    /**
     * Método que devolve um conjunto com os codigos das encomendas que a lojas cujo codigo é recebido ainda não tem prontas
     * @param lojaCod código da loja cujas encomendas que ainda não estão prontas queremos saber
     * @return conjunto com os codigos das encomendas que a lojas cujo codigo é recebido ainda não tem prontas
     */
    public Set<String> encNaoProntasLoja (String lojaCod){
        return this.registoEncomendas.getEncProntas().values().stream().filter(enc -> enc.getCodLoja().equals(lojaCod)).map(enc -> enc.getCodLoja()).collect(Collectors.toSet());
    }

    /**
     * Método que devolve um conjunto com os codigos das encomendas que ainda não estão prontas
     * @return conjunto com os codigos das encomendas que ainda não estão prontas
     */
    public Set<String> encsNaoProntas(){
        return this.registoEncomendas.getEncProntas().keySet().stream().collect(Collectors.toSet());
    }


    //Query 5
    public void lojaTemEncomendaPronta(String encCod) throws NoEntityException{
        String notif = ("A encomenda "+encCod+" está pronta para ser entregue.");
        this.registoEncomendas.pendentesParaProntas(encCod);
        this.baseDeDados.aumentaFilaLoja(this.registoEncomendas.getEncPronta(encCod).getCodLoja());
        this.baseDeDados.addNotificacaoDistribuidores(notif);
    }



    public boolean aceitaEntregar(String distribuidor, String encCod) throws InvalidInputException, NoEntityException{
        IEncomenda e;
        BigDecimal portes = BigDecimal.ZERO;
        if((e = this.registoEncomendas.getEncPronta(encCod)) == null) throw new NoEntityException("encomenda \'" + encCod + "\' inexistente");   
            switch(distribuidor.charAt(0)){
                case 'v': if(!this.baseDeDados.aceitaEntregar(distribuidor, encCod)) return false;
                          this.registoEncomendas.aceitada(encCod, LocalDateTime.now());
                          this.baseDeDados.addNotificacaoDistribuidoresMenosEste("A encomenda \'" + encCod + "\' já foi atribuída.", distribuidor);
                          this.baseDeDados.addNotificacaoPorCodigo("A encomenda "+encCod+" foi-lhe atribuída.", distribuidor);
                          this.baseDeDados.addNotificacaoPorCodigo("O voluntário \'" + distribuidor + "\' irá realizar a entrega da encomenda \'" + encCod + "\'.", e.getCodUtil());
                          this.baseDeDados.limpaRejeitadas(encCod);
                          return true;
                          
                case 't': ITransportadora t;
                          if((t = this.baseDeDados.getTransportadora(distribuidor)) == null) throw new NoEntityException("transportadora \'" + distribuidor + "\' inexistente" );
                          if(!t.getDisponivel()) return false;
                          portes = this.baseDeDados.calcularPortes(distribuidor, e.getCodUtil(), e.getCodLoja());
                          this.registoEncomendas.transportadoraPorAceitar(distribuidor, encCod, portes);
                          this.baseDeDados.addNotificacaoPorCodigo("A transportadora \'" + distribuidor + "\' pretende realizar a entrega da encomenda \'" + encCod + "\'.\n Portes: " + PrintFormat.currencyFormat(portes) , e.getCodUtil());
                          return true;
                default: throw new InvalidInputException("\'" + distribuidor + "\' é um código de distribuidor inválido.");
            }   
    }


    
    public void utilizadorAceitaTranportadora(boolean decisao, String utilizador, String transportadora) throws InvalidInputException, NoEntityException{
        IEncomenda e;
        if((e = this.registoEncomendas.getEncPorAceitar(transportadora)) == null) throw new NoEntityException("transportadora \'" + transportadora + "\' não tem pedido por aceitar");   
        if(decisao){
            this.registoEncomendas.transportadoraAceitada(transportadora, LocalDateTime.now());
            this.baseDeDados.aceitaEntregar(transportadora, e.getCodigo());
            this.baseDeDados.addNotificacaoDistribuidoresMenosEste("A encomenda \'" + e.getCodigo() + "\' já foi atribuída.", transportadora);
            this.baseDeDados.addNotificacaoPorCodigo("O utilizador \'" + utilizador + "\' aceitou o seu pedido para transportar a encomenda \'", transportadora);
            this.baseDeDados.limpaRejeitadas(e.getCodigo());
        }else{ 
            this.baseDeDados.rejeitada(transportadora, e.getCodigo());
            this.registoEncomendas.transportadoraRejeitada(transportadora);
        }
    }


    public void entregue(String distribuidor, String encCod, LocalDateTime tempoEntrega, double kms) throws NoEntityException, InvalidInputException{
        String not = ("A encomenda "+encCod+" já foi entregue.");
        IEncomenda e = this.registoEncomendas.getEncAceite(encCod);
        if(e == null) throw new NoEntityException("encomenda \'" + encCod + "\' inexistente.");
        this.baseDeDados.addNotificacaoPorCodigo(not, e.getCodUtil());
        this.registoEncomendas.aceitesParaFinalizadas(encCod);
        this.baseDeDados.diminuiFilaLoja(e.getCodLoja());
        this.baseDeDados.entregue(distribuidor, tempoEntrega, kms);

    }


    /**
     * Metodo que devolve o estado de uma encomeda
     *
     * @param e encomenda cujo estado queremos saber
     *
     * @return Pendente se a loja ainda nao tiver a encomenda completa,Pronta se a encomenda aind anao tiver um entregador associado, Aceite se estiver a ser levada ao utilizador ou Finalizada se ja tiver sido entregue
     */
    public String estadoEnc(IEncomenda e) throws NoEntityException{
        return this.registoEncomendas.estadoEnc(e.getCodigo());
    }


    // query 7
    public void query7 (String utilizador, int classificacao, String entregador) throws InvalidInputException{
        this.baseDeDados.addClassificacao(utilizador, classificacao, entregador);
    }

     /**
     * Incrementa o número de encomendas efetuadas por um dado utilizador.
     * @param id de um Utilizador ao qual será incrementado o número de encomendas.
     */
    public void incNEncUtilizador(String id) throws NoEntityException {
        this.baseDeDados.incNEncUtilizador(id);
    }

    public static IGestao loadStdLog() throws Exception{
            return Leitura.readStandardPath();
    }

    public static IGestao loadLogFile(String path) throws Exception{
            return Leitura.readFromFilePath(path);
    }

    /*  Query 9
    *   indicar o total faturado por uma empresa transportadora num determinado período;
    */
    public double query9(String idTransp, LocalDateTime inicio, LocalDateTime fim) throws NoEntityException{
       Predicate<Map.Entry<LocalDateTime, Double>> p = x -> !(x.getKey().isBefore(inicio)) && !(x.getKey().isAfter(fim));
       ITransportadora t;
       if((t = this.baseDeDados.getTransportadora(idTransp)) == null) throw new NoEntityException("transportadora \'" + idTransp + "\' inexistente.");
       return t.getRegistoKm().entrySet().stream().filter(p).mapToDouble(x -> x.getValue()).sum();
    }


    /*  Query 10
    *   determinar a listagens dos 10 utilizadores que mais utilizam o sistema (em número de encomendas transportadas);
    */
    public List<IUtilizador> query10(){
        Comparator<IUtilizador> c = (u1,u2) -> u2.getnEnc() - u1.getnEnc();
        return this.baseDeDados.getListUtilizadores().stream().sorted(c).limit(10).collect(Collectors.toList());
    }


    /*  Query 11
    *   determinar a listagens das 10 empresas transportadoras que mais utilizam o sistema (em número de kms percorridos);
    */
    public List<ITransportadora> query11(){
        Comparator<ITransportadora> c = (t1,t2) -> (int) t2.getKmTotais() - (int) t1.getKmTotais();
        return this.baseDeDados.getListTransportadoras().stream().sorted(c).limit(10).collect(Collectors.toList());
    }


    /*
    * Função que lê um .dat de uma Gestao
    */
    public void load() throws IOException, ClassNotFoundException {
        ObjectInputStream is = new ObjectInputStream(new FileInputStream(GESTAO_SAVE_FILE));
        Gestao g = (Gestao) is.readObject();

        this.baseDeDados = g.getBaseDeDados();
        this.registoEncomendas = g.getRegistoEncomendas();

        is.close();
    }

    public void load(String path) throws IOException, ClassNotFoundException {
        ObjectInputStream is = new ObjectInputStream(new FileInputStream(path));
        Gestao g = (Gestao) is.readObject();

        this.baseDeDados = g.getBaseDeDados();
        this.registoEncomendas = g.getRegistoEncomendas();

        is.close();
    }


    /* Query 12
    * gravar o estado da aplicação em ficheiro, para que seja possível retomar mais tarde a execução.
    */
    public void save() throws IOException {
        ObjectOutputStream os = new ObjectOutputStream(new FileOutputStream(GESTAO_SAVE_FILE));
        os.writeObject(this);
        os.close();
    }

    /**
     * @classe que compara map entry de IEncomenda e String com base na data de aceitação da IEncomenda
     **/
    class ComparadorEncPorDataAceitacao implements Comparator<Map.Entry<IEncomenda, String>> {
        public int compare(Map.Entry<IEncomenda, String> a, Map.Entry<IEncomenda, String> b) {
            LocalDateTime ldt_a, ldt_b;
            ldt_a = a.getKey().getDataAceitacao();
            if(ldt_a == null)
                ldt_a = a.getKey().getData();
            ldt_b = b.getKey().getDataAceitacao();
            if(ldt_b == null)
                ldt_b = b.getKey().getData();    
            return ldt_b.compareTo(ldt_a);
        }
    }

    

    /**
     *  Query 8
     * ter acesso no perfil de cada uma das entidades à informação sobre as encomendas transportadas (em função de data/hora de transporte)
     */
    public Map<IEncomenda, String> query8 (String cod, LocalDateTime time) throws NoEntityException{
        Map<IEncomenda, String> res = null;
        switch (cod.charAt(0)){
            case 'u' :
                res = this.registoEncomendas.getEncUt(cod).stream()
                        .filter(e -> e.getDataAceitacao().isAfter(time)).collect(toMap(Function.identity(), (e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        })));
                break;
            case 'l' :
                res = this.registoEncomendas.getEncLoja(cod).stream()
                        .filter(e -> e.getDataAceitacao().isAfter(time)).collect(toMap(Function.identity(), (e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        })));
                break;
            case 't' :
                res = this.baseDeDados.encAceitesPorTransportadora(cod).stream()
                        .filter(e -> this.registoEncomendas.getEncomenda(e).getDataAceitacao().isAfter(time))
                        .map(e -> this.registoEncomendas.getEncomenda(e)).collect(toMap(Function.identity(),(e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        })));
                break;
            case 'v' :
                res = this.baseDeDados.encAceitesPorVoluntario(cod).stream()
                        .filter(e -> this.registoEncomendas.getEncomenda(e).getDataAceitacao().isAfter(time))
                        .map(e -> this.registoEncomendas.getEncomenda(e)).collect(toMap(Function.identity(),(e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        })));
                break;
        }

        return res;
    }


    /**
     *  Query 8 ALternativo
     * ter acesso no perfil de cada uma das entidades à informação sobre as encomendas transportadas (em função de data/hora de transporte)
     */
    public List<Map.Entry<IEncomenda, String>> query8 (String cod) throws NoEntityException{
        Map<IEncomenda, String> res = null;
        List<Map.Entry<IEncomenda, String>> resFinal = null;
        Comparator<Map.Entry<IEncomenda, String>> c = new ComparadorEncPorDataAceitacao();
        switch (cod.charAt(0)){
            case 'u': 
                res = this.registoEncomendas.getEncUt(cod).stream()
                        .collect(toMap(Function.identity(), e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        }));
                resFinal = res.entrySet().stream().sorted(c).collect(Collectors.toList());
                break;
            case 'l' :
                res = this.registoEncomendas.getEncLoja(cod).stream()
                        .collect(toMap(Function.identity(), (e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        })));
                resFinal = res.entrySet().stream().sorted(c).collect(Collectors.toList());
                break;
            case 't' :
                res = this.baseDeDados.encAceitesPorTransportadora(cod).stream()
                        .map(e -> this.registoEncomendas.getEncomenda(e)).collect(toMap(Function.identity(),(e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        })));
                IEncomenda enc = this.registoEncomendas.encsPorAceitar(cod);
                if(enc != null)
                    res.put(enc, "Por aceitar");
                resFinal = res.entrySet().stream().sorted(c).collect(Collectors.toList());
                break;
            case 'v' :
                res = this.baseDeDados.encAceitesPorVoluntario(cod).stream()
                        .map(e -> this.registoEncomendas.getEncomenda(e)).collect(toMap(Function.identity(),(e -> {
                            try {
                                return estadoEnc(e);
                            } catch (NoEntityException e1) {
                                throw new UncheckedIOException(e1.getMessage(), null);
                            }
                        })));
                resFinal = res.entrySet().stream().sorted(c).collect(Collectors.toList());
        }

        return resFinal;
    }


    public void save(String path) throws IOException{
        ObjectOutputStream os = new ObjectOutputStream(new FileOutputStream(path));
        os.writeObject(this);
        os.close();
    }


    /**
     * Metodo que devolve o conjunto de Ids das contas existentes
     */
    public Set<String> getIDsContas(){
        return this.baseDeDados.getIDsContas();
    }

    /**
     * Metodo que devolve o conjunto dos códigos das encomendas que o distribuidor (cujo codigo é recebido como paramentro) entregou
     */
    public Set<String> getEncsDistribuidor(String id) throws NoEntityException, InvalidInputException{
        ITransportadora t;
        IVoluntario v;
        switch(id.charAt(0)){
            case 't': if((t = this.baseDeDados.getTransportadora(id)) == null) throw new NoEntityException("transportadora \'" + id + "\' inexistente."); else return t.getEncomendas();
            case 'v': if((v = this.baseDeDados.getVoluntario(id)) == null) throw new NoEntityException("voluntário \'" + id + "\' inexistente."); else return v.getEncomendas();
            default : throw new InvalidInputException("\'" + id + "\' não é um código de distribuidor válido.");
        }
    }

    /**
     * Metodo que adiciona uma conta às já existentes
     */
    public void addConta(IConta c){
        this.baseDeDados.addConta(c);
    }

    /**
     * Metodo que devolve a lista das encomendas que a loja (identificada pelo codigo que é recebido como parametro) ainda não tem prontas para serem recolhidas
     */
    public List<IEncomenda> encomendasPendentesALoja(String loja){
        Comparator<IEncomenda> c = (e1,e2) -> e2.getData().compareTo(e1.getData()); 
        return this.registoEncomendas.encomendasPendentesALoja(loja).stream().sorted(c).collect(Collectors.toList());
    }

    /**
     * Metodo que devolve o número de encomendas feitas pelo utilizador (cujo id é recebido como parametro) que ainda não foram associadas a um entregador
     */
    public List<Map.Entry<String, IEncomenda>>  nEncomendasPorAceitarDoUtilizador(String utilizador){
        return  this.registoEncomendas.encomendasPorAceitarDoUtilizador(utilizador);
    }

    public void transpMedicamentos(String vol, boolean b) throws NoEntityException{
        this.baseDeDados.transpMedicamentos(vol, b);
    }

    public void pendenteAtualizadaParaProntas(IEncomenda atualizada) throws NoEntityException{
        this.registoEncomendas.pendenteAtualizadaParaProntas(atualizada);
    }

    public Map<String, LocalDateTime> dataPrevistaEntregas(Set<String> encs) throws NoEntityException{
        Map<String, LocalDateTime> res = new HashMap<>();
        IEncomenda aux;
        double dist;
        Distribuidor d;
        for(String s : encs){
            if((aux = this.registoEncomendas.getEncAceite(s)) != null){
                d = this.baseDeDados.getDistDaEncomenda(s);
                dist = this.baseDeDados.getLoja(aux.getCodLoja()).getPosicao().distancia(d.getPosicao());
                res.put(s, aux.getDataAceitacao().plusMinutes(Gerador.aleatoriedadeTempoPercurso(dist,  this.baseDeDados.getLoja(aux.getCodLoja()).getEmFila())));
            }
            else
                res.put(s,null);
        }
        return res;
    }


    public Rating getDistRating(String code) throws NoEntityException, InvalidInputException {
        try {
            if (code.charAt(0) == 't')
                return baseDeDados.getTransportadora(code).getClassificacao();
            if (code.charAt(0) == 'v')
                return baseDeDados.getVoluntario(code).getClassificacao();
        } catch (Exception e) {
            throw new NoEntityException("O distribuidor \'" + code + "\' não existe.");
        }

        throw new InvalidInputException("O código \'" + code + "\' não é um código de distribuidor válido.");
    }
    
}
