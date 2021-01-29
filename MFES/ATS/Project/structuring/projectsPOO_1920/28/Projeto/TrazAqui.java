
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;
import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;





public class TrazAqui implements Serializable {

    private Map<String, User> contas;    /*uma map de codigos de User -> User, usado para
                                        para fazermos o login e acessarmos uma dada conta
                                        pelo seu codigo*/
    private Map<String,String> nomelogin; /*um map de nome de User -> codigo de User, para
                                        no caso do User preferir fazer login pelo nome */
    
    //comparadores depois definidos nos construtores pelas classes
    private Comparator<User> registo; 
    private Comparator<Transportador> km;
    
    private Map<String,List<Transportadoras>> correspondencia; /* Correspondencia de codigos de 
                                                                encomenda para transportadoras que estao 
                                                                interessadas na entrega da mesma */
    private EncomendasAceites aindaNaoEntregues;              //lista de encomendas pedidas mas ainda nao entregues
    private Integer contadorEnc;                            /*contador do numero de encomendas ate agora para atribuirmos
                                                            os proximos codigos de encomenda*/

    /**
     * Cores usadas em diversos prints
     */
    private static final String RESET = "\u001B[0m";
    private static final String BLACK = "\u001B[30m";
    private static final String RED = "\u001B[31m";
    private static final String GREEN = "\u001B[32m";
    private static final String YELLOW = "\u001B[33m";
    private static final String BLUE = "\u001B[34m";
    private static final String PURPLE = "\u001B[35m";
    private static final String CYAN = "\u001B[36m";
    private static final String WHITE = "\u001B[37m";

    //construtor
    public TrazAqui()
    {
        this.contas=new HashMap<>();
        this.nomelogin=new HashMap<>();
        this.registo = new ComparatorNumRegistos();
        this.km = new ComparatorKm();
        this.correspondencia = new HashMap<>();
        this.aindaNaoEntregues = new EncomendasAceites();
        this.contadorEnc = 1;

    }
    
    
    public void addUser(User u,String nome){
        this.contas.put(u.getId(),u.clone());
        this.nomelogin.put(nome,u.getId());
    }

    public Boolean existeNome(String nome){
        return this.nomelogin.containsKey(nome);
    }

    public String nomeCodigo(String nome) {
        return this.nomelogin.get(nome);
    }

    public User getUser(String nome){
        return this.contas.get(nome);
    }
    
    /**
     * Ver os registos de um User dado pelo seu codigo
     */
    public String registoGeral(String nomeCod){
        return this.contas.get(nomeCod).verRegistosGeral().toString();
    }

    public Integer getcontadorEnc(){
        return this.contadorEnc++;
    }


    public List<User> getContasList()
    {
        return this.contas.values().stream().map(u->u.clone()).collect(Collectors.toList());
    }


    // metodos  utilizador

    public void adicionaEncomenda(String nomeCod, Encomenda encomendaNova)
    {
        this.aindaNaoEntregues.addEncomendaAceite(encomendaNova);
    }

    /**
     * Dar return as encomendas para ser entregues ao utilizador em questao
     * ou seja as encomendas ja preparadas por uma loja e ainda nao entregues ao Utilizador
     */
    public String utilizadorEncomendasParaEntrege(String nomeCod) throws ListaVaziaException
    {
        Utilizador atualUtilizador = (Utilizador) this.contas.get(nomeCod);
        List <Encomenda> e = atualUtilizador.getencomendasJaPreparadas();
        if (e.size() == 0){
            throw new ListaVaziaException("Lista Vazia!");
        }
        StringBuilder sb = new StringBuilder();
        for ( Encomenda enc : e){
            sb.append(YELLOW + "\nEncomenda ==> " + enc.getCodEncomenda() + " :\n" + RESET).append(enc.toString()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Solicitar a entrega de uma encomenda, tiramos da lista das que falta por solicitar e criamos 
     * uma nova entrada na correspondencia
     */
    public void solicitarEncomenda(String nomeCod,String encomendaEscolhida)
    {
        Utilizador atualUtilizador = (Utilizador) this.contas.get(nomeCod);
        Encomenda e = atualUtilizador.pop(encomendaEscolhida);
        atualUtilizador.addEncomendaPorAceitar(encomendaEscolhida);
        this.correspondencia.put(e.getCodEncomenda(), new ArrayList<>());
    }

    /**
     * Apresentar a lista de encomendas por aceitar do Utilizador (que ainda nao escolheu
     * nenhuma transportadora para a transportar)
     */
    public String utilizadorEncomendasPorAceitar(String nomeCod) throws ListaVaziaException
    {
        Utilizador atualUtilizador= (Utilizador) this.contas.get(nomeCod);
        return atualUtilizador.apresentarListaEncomendasPorAceitar().toString();
    }

    /**
     * Aceitar uma certa encomenda 
     * @param respostasAceitar codigo da transportadora aceite
     * @param codEncAceitar codigo da encomenda Aceite
     */
    public void aceitarEncomenda(String respostasAceitar,String codEncAceitar)
    {
        Transportador tAceite = (Transportador) contas.get(respostasAceitar);
        Encomenda escolhida = this.aindaNaoEntregues.buscaEncomenda(codEncAceitar);
        Utilizador u = (Utilizador) this.contas.get (escolhida.getCodUtilizador());
        u.removeEncomendaPorAceitar(codEncAceitar);
        tAceite.addParaLevar(codEncAceitar);
        correspondencia.remove(codEncAceitar);
    }


    /**
     * @param nomeCod Transportador a escolher para classificar
     * @return lista dos Transportadores
     * @throws ListaVaziaException
     */
    public String escolheClassificar(String nomeCod) throws ListaVaziaException
    {

        Utilizador atualUtilizador= (Utilizador) this.contas.get(nomeCod);
        StringBuilder sb = new StringBuilder();

        for ( RegistoEncomenda r : atualUtilizador.naoClassificado())
            sb.append(r.toString() + "\n");
            
        return sb.toString();
    }

    /**
     * Classificar uma certa encomenda
     * @param nomeCod Transportador a classificar
     * @param codEnc Encomenda respetiva que o Transportador vai obter classificacao
     * @param classificacao classificacao a dar
     */
    public void classificarEntrega(String nomeCod,String codEnc, int classificacao)
    {
        Utilizador atualUtilizador =(Utilizador) this.contas.get(nomeCod);
        atualUtilizador.classificarEntrega(codEnc, classificacao);
    }


    //metodos Transportador
    public String  transportadorClassificacoes(String nomeCod)
    {
        Transportador atual = (Transportador) this.contas.get(nomeCod);
        return atual.verClassificacoes().toString();
    }


    /**
     * @param nomeCod codigo do Transportador a alterar a disponibilidade
     * @param b booleano em que a disponibilidade vai ficar
     * @return String com mensagem
     * @throws MudancaDisponibilidadeException
     */
    public String alteraDisponibilidade(String nomeCod ,boolean b) throws MudancaDisponibilidadeException
    {
        String s=new String();
        Transportador atualTransportador =(Transportador) this.contas.get(nomeCod);
        if (atualTransportador.sizeJaFoiBuscar() != 0){
            throw new MudancaDisponibilidadeException("Erro nao pode mudar a sua disponibilidade enquanto nao entregar as encomendas que ja foi buscar as lojas");

        }else{
            atualTransportador.setDisponibilidade(b);
            if ( b == false){
                atualTransportador.limpaListas(correspondencia);
                s="Todas as encomendas anteriormente aceites foram removidas!";
            }else{
                s="Já pode aceitar novas encomendas!";
            }
        }
        return s;

    }

    // metodos Lojas 

    public double lojaTotalFaturado(String nomeCod){
        Lojas atualLoja =(Lojas) this.contas.get(nomeCod);
        return atualLoja.totalFaturado();
    }

    public int lojaSizePreparacao(String nomeCod)
    {
        Lojas atualLoja =(Lojas) this.contas.get(nomeCod);
        return atualLoja.sizeParaPreparacao();
    }
    

    public String lojaEncomendasPreparacao(String nomeCod) throws ListaVaziaException
    {
        Lojas atualLoja = (Lojas) this.contas.get(nomeCod);
        return atualLoja.verEncomendasEmPreparacao().toString();
    }
    
    /**
    * Ver encomendas em preparacao de uma certa loja em forma de lista com cores
    * difere do anterior da forma como faz a disposicao da lista
    * @param nome codigo da loja a ver
    * @return String com as opcoes
    * @throws ListaVaziaException
    */
    public String LojaEncomendasPreparacao(String nome) throws ListaVaziaException
    {
        Lojas atualLoja =(Lojas) this.contas.get(nome);
        List <Encomenda> l = atualLoja.verEncomendasEmPreparacao();
        StringBuilder sb = new StringBuilder();
        for (Encomenda e : l){
            sb.append(YELLOW + "Encomenda ==> " + e.getCodEncomenda() + " :\n" + RESET).append(e.toString()).append("\n");
        }
        return sb.toString();
    }

    /**
     * Adicionar uma encomenda que acabou de preparar
     * @param nomeCod codigo da loja
     * @param encomendaPreparada codigo da encomenda
     */
    public void encomendaPreparada(String nomeCod, String encomendaPreparada)
    {
        Lojas atualLoja=(Lojas) this.contas.get(nomeCod);
        Encomenda e = atualLoja.removeDasPreparadas(encomendaPreparada);
        String codUtilizador = e.getCodUtilizador();
        Utilizador u = (Utilizador) this.contas.get(codUtilizador);
        u.adicionaEncomenda(e);
    }

    /**
     * Remover alguem da fila de espera 
     * @param codUser user a remover da fila
     * @param encomenda encomenda respetiva
     */
    public void removeDaFila (String codUser , String encomenda){
        Encomenda e = this.aindaNaoEntregues.buscaEncomenda(encomenda);
        User l = this.contas.get(e.getCodLoja());

        if (l.getClass().getName().equals("LojaComFila")){
            LojaComFila lcf = (LojaComFila) l;
            lcf.removeDaFila(e);
    }}


    public String verTopN ( String codUser , String classe , int n){
        User u = this.contas.get(codUser);
        return u.verTopN(contas, n, new ComparatorNumRegistos(), classe);
    }

    public String verTopNKm ( String codUser , String classe , int n){
        User t = (User) this.contas.get(codUser);
        return t.verTopNKm(contas, n, new ComparatorKm(), classe);
    }

    public boolean getDisponibilidade ( String codUser){
        Transportador t = (Transportador) this.contas.get(codUser);
        return t.getDisponibilidade();
    }

    public List < String > getJaFoiBuscar (String codUser ){
        Transportador t = (Transportador) this.contas.get(codUser);
        List <String> l = t.getJaFoiBuscar();
        return l;
    }

    public int getCapacidade (String codUser ){
        Transportadoras t = (Transportadoras) this.contas.get(codUser);
        return t.getCapacidade();
    }

    public Transportadoras cloneTransportadoras ( String codUser){
        Transportadoras t = (Transportadoras) this.contas.get(codUser);
        return t.clone();
    }

    /**
     * Ver as encomendas que uma transportadora pode aceitar para entregar, vai a 
     * correspondencia e ve as que estao dentro do raio da transportadora, colocando-as
     * na lista de return
     * @param codUser Transportador para ver as encomendas possiveis para aceitar
     * @return Lista das encomendas que pode aceitar 
     * @throws ListaVaziaException
     */
    public List <Encomenda> escolherEncomendaAEntregar ( String codUser) throws ListaVaziaException {

        Transportador atualTransportadora = (Transportador) this.contas.get(codUser).clone();
        List <Encomenda> l = new ArrayList <>();

        
        for (String codEnc : correspondencia.keySet()){
                String codLojaPedida = aindaNaoEntregues.buscaEncomenda(codEnc).getCodLoja();
            if (contas.get(codLojaPedida).getCoordenadas().dentroRaio(atualTransportadora.getCoordenadas(), atualTransportadora.getRaio())){
                l.add(aindaNaoEntregues.buscaEncomenda(codEnc));
            }
        }

        if (l.size() == 0){
            throw new ListaVaziaException("Lista Vazia");
        }

        return l;
    }

    public boolean aceitoTransporteMedicamentos (String codUser){
        Transportador t = (Transportador) this.contas.get(codUser);
        return t.aceitoTransporteMedicamentos();
    }

    public String getParaLevar (String codUser) throws ListaVaziaException {
        Transportador t = (Transportador) this.contas.get(codUser);
        return t.getParaLevar().toString();
    }


    /**
     * adicionar uma encomenda a lista das que foi buscar a uma determinada loja
     * @param codUser transportador
     * @param codEncomendaJaRecolhida encomenda que ja recolheu
     */
    public void addJaFoiBuscar (String codUser , String codEncomendaJaRecolhida){
        Transportador t = (Transportador) this.contas.get(codUser);
        t.addJaFoiBuscar(codEncomendaJaRecolhida);
    }

    /**
     * Remover uma encomenda da lista das que ja foi buscar a uma determinada loja
     * @param codUser transportador
     * @param codEncomenda encomenda que ja entregou
     */
    public void removeJaFoiBuscar ( String codUser , String codEncomenda){
        Transportador t = (Transportador) this.contas.get(codUser);
        t.removeJaFoiBuscar(codEncomenda);
    }

    
    /**
     * Remover uma encomenda da lista das que tem para levar (depois de recolher)
     * @param codUser transportador
     * @param codEncomendaJaRecolhida encomenda que ja recolheu
     */
    public void removeParaLevar (String codUser , String codEncomendaJaRecolhida){
        Transportador t = (Transportador) this.contas.get(codUser);
        t.removeParaLevar(codEncomendaJaRecolhida);
    }

    public String totalFaturadoTaxas ( String codUser){
        Transportadoras t = (Transportadoras) this.contas.get(codUser);
        return t.totalFaturadoTaxas();
    }

    public String verRegistosEntrePeriodoDeTempo (String codUser , LocalDate dataI , LocalDate dataF){
        User u = (User) this.contas.get(codUser);
        return u.verRegistos(codUser, dataI, dataF).toString();

    }

    public Encomenda getEncomendaQuePrecisaSerEntregue ( String e){
        return aindaNaoEntregues.buscaEncomenda(e).clone();
    }

    public void removeEncomendaQuePrecisaSerEntregue (Encomenda e){
        aindaNaoEntregues.removeEncomenda(e);
    }

    /**
     * adicionar uma transportadora a correspondencia
     * @param codUser Transportadora
     * @param codEncomenda Encomenda do map correspondencia
     */
    public void adicionaTransportadoraACorrespondenciaDaEncomendaEscolhida (String codUser , String codEncomenda){
        Transportadoras t = (Transportadoras) this.contas.get(codUser);

        List <Transportadoras> correspondenciaAntiga = correspondencia.get(codEncomenda); // lista das possiveis transportadoras a entregar esta encomenda antes de adicionar a transportadora
        correspondenciaAntiga.add(t.clone()); // adicionamos a nova transportadora a lista
        correspondencia.put(codEncomenda, correspondenciaAntiga); // e damos replace no map com a nova lista
    }

    public Utilizador getUtilizadorQuePediuEncomenda (Encomenda e){
        String codUtilizador = e.getCodUtilizador();
        return (Utilizador) this.contas.get(codUtilizador);
    }

    public Lojas getLojaQuePreparouEncomenda (Encomenda e){
        String codLoja = e.getCodLoja();
        return (Lojas) this.contas.get(codLoja);
    }

    public double custoEntrega ( String codTransportadora , GPS loja , GPS utilizador , double pesoEncomenda){
        Transportadoras t = (Transportadoras) this.contas.get(codTransportadora);
        return t.custoEntrega(loja, utilizador, pesoEncomenda);
    }

    public double kmsPercorridos ( String codTransportadora , GPS loja , GPS utilizador){
        Transportador t = (Transportador) this.contas.get(codTransportadora);
        GPS localInicialTransportadora = t.getCoordenadas();
        return localInicialTransportadora.dist(loja) + localInicialTransportadora.dist(utilizador);
    }

    public double tempoRecolherEntrega (String codUser , LojaComFila l , User.Meteorologia met , User.EstadoCovid estadoCovid){
        Transportador t = (Transportador) this.contas.get(codUser);
        return l.tempoRecolherEntrega(t, met , estadoCovid);
    }

    public double tempoChegadaLoja ( String codUser , Lojas loja , User.Meteorologia tempo){
        Transportador t = (Transportador) this.contas.get(codUser);
        return t.tempoChegadaLoja(loja , tempo);
    }

    public double tempoChegadaUtilizador ( String codUser , Lojas loja , Utilizador utilizador , User.Meteorologia tempo){
        Transportador t = (Transportador) this.contas.get(codUser);
        return t.tempoChegadaUser(loja, utilizador, tempo);
    }


    public void addToRegistoUser ( String codUser , String codTransportadora , Encomenda e , int classificacao  , LocalDate data , double kmsPercorridos , double taxaDeEntrega , double tempoDemorado ){
        User u = this.contas.get(codUser);
        u.addToRegisto(codTransportadora, e, classificacao, data, kmsPercorridos, taxaDeEntrega, tempoDemorado);
    }


    

    //UTILIZADOR


    public void utilizadorRemoveEncomendaPorAceitar(String nomeCod,String codEncAceitar)
    {
        Utilizador atualUtilizador = (Utilizador) this.contas.get(nomeCod);
        atualUtilizador.removeEncomendaPorAceitar(codEncAceitar);
    }

    public Encomenda getEncomendaParaEntrega(String codEncAceitar)
    {
        return aindaNaoEntregues.buscaEncomenda(codEncAceitar);
    }

    public List<Transportadoras> getTransportadorasEncomenda(String codEncAceitar)
    {
        return correspondencia.get(codEncAceitar);
    }

    public Map<String,User> getContasMap(){
        Map<String,User> ret = new HashMap<>();
        for (User u:this.contas.values())
        {
            ret.put(u.getId(),u.clone());
        }
        return ret;
    }

    public String verRegistos(String nomeCod,String entregador, LocalDate dI,LocalDate dF)
    {
        return this.contas.get(nomeCod).verRegistos(entregador,dI,dF).toString();
    }


    public void adicionaParaPreparacao (Encomenda e , String codLoja){

        Lojas loja = (Lojas) this.contas.get(codLoja);
        loja.addParaPreparacao(e);
    }

    // metodos ler e escrever Ficheiros
    
    /**
     * ler um ficheiro CVS e carregar os seus dados para o model, utiliza
     * funcoes do Parse
     * @param nomeFicheiro 
     * @throws FileNotFoundException
     */
    public void lerFicheiroCSV(String nomeFicheiro) throws FileNotFoundException{
        Parse p = new Parse();
        p.parse(nomeFicheiro);
        contas = p.getContas();
        for (String cod : contas.keySet()) nomelogin.put(cod, cod);
            
        List<Encomenda> encomendasAindaNaoPreparadas = p.getEncomendas();
        aindaNaoEntregues.setEncomendas(encomendasAindaNaoPreparadas);
        for (Encomenda enc : encomendasAindaNaoPreparadas){
            Lojas l = (Lojas) contas.get(enc.getCodLoja());
            l.addParaPreparacao(enc.clone());
        }
    
        List <Encomenda> encomendasAceites = p.getEncomendasAceites();
        for (Encomenda encAceite : encomendasAceites){
            aindaNaoEntregues.addEncomendaAceite(encAceite.clone());
            correspondencia.put(encAceite.getCodEncomenda(), new ArrayList<>());
            Utilizador uti = (Utilizador) contas.get(encAceite.getCodUtilizador());
            uti.addEncomendaPorAceitar(encAceite.getCodEncomenda());
            }
            
        }
    
    /**
     * Imprimir para um ficheiro CSV
     */
    public  void paraFicheiroCSV (String nomeFicheiro) throws FileNotFoundException{
        PrintWriter pw = new PrintWriter(nomeFicheiro);
        for (User u : this.contas.values() ){
            switch (u.getClass().getName()){
                case "Lojas":
                    pw.println( "Loja:" + u.paraCSV());
                    break;
        
                case "Transportadoras":
                    pw.println( "Transportadora:" + u.paraCSV());
                    break;
            
                case "Voluntarios":
                    pw.println( "Voluntario:" + u.paraCSV());
                    break;
            
                default:
                    pw.println(u.getClass().getName() + ":" + u.paraCSV());
                    break;
            }}
            
        for (Encomenda e : this.aindaNaoEntregues.getEncomendas() ){
            pw.println(e.paraCSV());
        }
            
        pw.println (this.aindaNaoEntregues.paraCSV());
            
        pw.flush();
        pw.close();
    }
    
    /**
     * Guardar o estado em binario
     * @param nomeFicheiro
     * @throws FileNotFoundException
     * @throws IOException
     */
    public void guardaFicheiro (String nomeFicheiro) throws FileNotFoundException , IOException{
    
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    
    }
    
    /**
     * ler de ficheiro em binario
     * @param nomeFicheiro
     * @return objeto TrazAqui
     * @throws FileNotFoundException
     * @throws IOException
     * @throws ClassNotFoundException
     */
    public static TrazAqui lerFicheiro (String nomeFicheiro) throws FileNotFoundException, IOException , ClassNotFoundException {
    
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);
        TrazAqui ta = (TrazAqui) ois.readObject();
        ois.close();
        return ta;
    }


}







