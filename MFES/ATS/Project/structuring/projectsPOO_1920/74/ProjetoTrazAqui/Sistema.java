import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.Map;
import java.util.HashMap;
import java.time.LocalDateTime;
import java.util.LinkedList;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.Serializable;
import java.util.List;
import java.util.ArrayList;
import java.util.AbstractMap.SimpleEntry;
import java.time.Duration;
import java.util.function.Consumer;
public class Sistema implements Serializable, ISistema{
    private Entidades entidades;
    private Pedidos pedidos;
    /**
     * Construtor por omissao da classe
     */
    public Sistema(){
        this.entidades = new Entidades();
        this.pedidos = new Pedidos();
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros Transportadoras t, Utilizadores u, Lojas l, Voluntarios v
     */ 
    public Sistema(Entidades e){
        this.entidades = e.clone();
        this.pedidos = new Pedidos();
    }
    /**
     * Construtor parametrizado da classe
     * Aceita como parametros Transportadoras t, Utilizadores u, Lojas l, Voluntarios v, Pedidos p
     */ 
    public Sistema(Entidades e, Pedidos p){
        this.entidades = e.clone();
        this.setPedidos(p);
    }
    /**
     * Construtor de copia da classe
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */  
    public Sistema(Sistema sist){
        this.entidades = sist.getEntidades();
        this.pedidos = sist.getPedidos();
    }
    /**
     * Metodo que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
    public boolean equals(Object o){
        if(o == this)
            return true;
        if(o.getClass() != this.getClass())
            return false;
        Sistema p = (Sistema) o;
        return (this.entidades.equals(p.getEntidades()) && this.pedidos.equals(p.getPedidos()));
    }
    /**
     * Faz clone da classe
     * @return o clone da classe
     */
    public Sistema clone(){
        return new Sistema(this);
    }
     /**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Entidades: ")
            .append(this.entidades)
            .append("Pedidos: ")
            .append(this.pedidos);
        return sb.toString();
    }
    /**
     * Devolve as entidades do Sistema
     * @return Entidades entidades
     */
    public Entidades getEntidades(){
        return this.entidades.clone();
    }
    /**
     * Metodo que devolve uma dada Entidade existente na classe
     * @param String cod
     * @return Entidade encontrada
     */ 
    public Entidade getEntidade(String cod) throws EntidadeNaoExistenteException{
        return this.entidades.getEntidade(cod).clone();
    }
    /**
     * Devolve o Pedidos pedidos da classe
     * @return Pedidos pedidos
     */
    public Pedidos getPedidos(){
        return this.pedidos.clone();
    }
    /**
     * Atualiza a Entidades entidades  da classe
     * @param e nova entidades da classe
     */
    public void setEntidades(Entidades e){
        this.entidades = e.clone();
    }
    /**
     * Atualiza o Pedidos pedidos da classe
     * @param p novo pedidos da classe
     */
    public void setPedidos(Pedidos p){
        this.pedidos = p.clone();
    }
    /**
     * Metodo que adiciona uma Entidade à classe
     * @param Entidade t
     * @return boolean resultante da inserçao
     */
    public boolean adicionaEntidade(Entidade e) throws AddEntidadeRepetidaException{
        return this.entidades.adicionaEntidade(e.clone());
    }
    /**
     * Metodo que verifica se os dados de login de uma Entidade sao validos e devolve a Entidade caso sejam validas
     * @param String email,String pass
     * @return Entidade 
     */
    public Entidade validaCredenciaisEntidades(String email,String pass) throws CredenciaisErradasException, EntidadeNaoExistenteException{
        return this.entidades.getEntidade(this.entidades.logIn(email,pass));
    }
    /**
     * Metodo que devolve as 10 empresas do Sistema que mais kilometros fizeram
     * @return Map<String,Double> resultante
     */
    public List<SimpleEntry<String,Double>> top10EmpresasKmPercorridos(){
        Map<String,Double> kmPorEmpresa = this.entidades.kmPercorridosPorEmpresa();
        return kmPorEmpresa.entrySet().stream()
                                      .sorted((a,b)-> b.getValue().intValue() - a.getValue().intValue())
                                      .limit(10)
                                      .map(v->new SimpleEntry<String,Double>(v.getKey(), v.getValue()))
                                      .collect(Collectors.toList());
    }
    /**
     * Metodo que devolve os 10 utilizadores que mais fizeram compras no Sistema
     * @return Map<String,Integer> resultante
     */
    public List<SimpleEntry<String,Integer>> top10Utilizadores(){
        Map<String,Integer> nrEntregasPorUser = this.entidades.nrEncomendasPorUtilizador();
        return nrEntregasPorUser.entrySet().stream()
                                           .sorted((a,b)-> b.getValue().intValue() - a.getValue().intValue())
                                           .limit(10)
                                           .map(v->new SimpleEntry<String,Integer>(v.getKey(), v.getValue()))
                                           .collect(Collectors.toList());
    }
    /**
     * Metodo que devolve o total faturado por uma Transportadora num dado intervalo de tempo
     * @param String codigoEmpresa,LocalDateTime t1, LocalDateTime t2
     * @return double final
     */
    public double totalFaturadoPorEmpresa(String codigoEmpresa, LocalDateTime t1, LocalDateTime t2)throws EntidadeNaoExistenteException{
        return this.entidades.faturacaoDeEmpresa(codigoEmpresa,t1,t2);
    }
    /**
     * Metodo que atribuiu uma classificacao a um Voluntario ou uma Transportadora dependendo de quem fez a entrega
     * @param String nota , int nota
     */ 
    public void classifica(String codigo, int nota)throws EntidadeNaoExistenteException{
        this.entidades.adicionaClassificacao(codigo, nota);
    }
    /**
     * Metodo que verifica se um dado codigo corresponde a alguma Entidade do Sistema
     * @param String cod
     * @return boolean resultante da verificaçao
     */
    public boolean existeCodigo(String cod){
        return this.entidades.existe(cod); 
    }
    /**
     * Metodo que devolve num Map<String,Produto> os produtos de uma dada Loja
     * @param String loja
     * @return Map<String,Produto> criado
     */
    public Map<String,Produto> getProdutosLoja(String loja) throws EntidadeNaoExistenteException{
        Map<String,Produto> ret = new HashMap<>();
        Loja l = null;
        Entidade aux = this.entidades.getEntidade(loja);
        if(aux instanceof Loja){
            l = (Loja) aux;
            ret = l.getProdutos();
        }
        return ret;
    }
    /**
     * Metodo que adiciona um dado Produto a uma dada Loja
     * @param String loja , Produto p
     */
    public void adicionaProdutoLoja(String loja, Produto p)throws EntidadeNaoExistenteException{
        Consumer<Loja> c = v -> v.adicionaProduto(p);
        this.entidades.atualizaLoja(loja,c);
    }
    /**
     * Metodo que dado um produto o adiciona às lojas do sistema
     * @param Produto p
     */
    public void adicionaProdutoLojas(Produto p)throws EntidadeNaoExistenteException{
        this.entidades.adicionaProdutoLojas(p);
    }
    /**
     * Metodo que devolve num List<RegistosTransporte> os RegistosTransporte de um Voluntario ou de uma Transportadora
     * @param String codEntity
     * @return List<RegistosTransporte> resultante
     */  
    public List<RegistosTransporte> registosTransporte(String codEntity)throws EntidadeNaoExistenteException{
        return this.entidades.encomendasFeitas(codEntity);
    }
    /**
     * Metodo que dado uma String de uma Entidade devolve os RegistosTransporte da Entidade num dado intervalo de tempo 
     * @param String codEntity, LocalDateTime t1, LocalDateTime t2
     * @List<RegistosTransporte> obtido
     */
    public List<RegistosTransporte> registosIntervalo(String codEntity, LocalDateTime t1, LocalDateTime t2) throws EntidadeNaoExistenteException{
        return this.entidades.encomendasFeitas(codEntity).stream().filter(a -> a.getHoraEntrega().isAfter(t1) && a.getHoraEntrega().isBefore(t2)).collect(Collectors.toList());
    }

    /**
     * Metodo que devolve num List<RegistoEntregas> os RegistoEntregas de um Utilizador
     * @param String codEntity
     * @return List<RegistoEntrgas> resultante
     */  
    public List<RegistoEntregas> getRegistosUtilizador(String codEntity)throws EntidadeNaoExistenteException{
        return this.entidades.getRegistosEntrega(codEntity);
    }
    
    //Flow dos pedidos
    /**
     * Metodo que cria um Objecto da classe Pedido o adiciona à estrutura dos Pedidos e escolhe uma Entidade para fazer a entrega
     * @param Encomenda enc
     */
    public String fazPedido(Encomenda e, List<String> ignore) throws EntidadeNaoExistenteException, TransporteNaoExistenteException{ // Utilizador pede nova encomenda
        ITransporte t = this.entidades.getMaisProximo(e,ignore);
        Pedido p = new Pedido(e,ignore);
        String ret = t.getCodigo();
        this.pedidos.adicionaPedido(p);
        if(t instanceof Transportadora){
            Consumer<Transportadora> c = v -> v.adicionaEncomenda(e.getCodEncomenda());
            this.entidades.atualizaTransportadora(ret,c);    
        }else if(t instanceof Voluntario){
            Consumer<Voluntario> c = v -> v.adicionaEncomenda(e.getCodEncomenda());
            this.entidades.atualizaVoluntario(ret,c);    
        }
        this.pedidos.atualizaPendenteTransporte(e.getCodEncomenda(), true); // Indica que a encomenda está pendente da resposta da empresa
        this.pedidos.atualizaTransporte(e.getCodEncomenda(), ret); //Indica no pedido em que transporte está a encomenda
        return ret;
    }
    /** 
     * Metodo que devolve a List<Encomenda> de uma dada Entidade do Sistema
     * @param String cod
     * @return List<Encomenda> resultante
     */   
    public List<Encomenda> listaDeEncomendas(String cod)throws EntidadeNaoExistenteException{
        List<String> e = new ArrayList<>();
        e = this.entidades.getEncomendas(cod);
        List<Encomenda> ret = this.pedidos.getListEncomendas(e);
        return ret;
    }
    /**
     * Metodo que dado um Voluntario ou Transportadora devolve a lista de encomendas que ainda nao foram entregues
     * @param String cod
     * @return List<Encomenda> resultante
     */
    public List<Encomenda> listaDeEncomendasPorEntregar(String cod)throws EntidadeNaoExistenteException{
        List<String> e = this.entidades.getEncomendas(cod);
        return this.pedidos.getListEncomendasPorEntregar(e);
    }
    /**
     * Metodo que caso uma Transportadora rejeite uma Encomenda escolhe outro para fazer o transporte
     * @param  String codT, String codigoEnc
     */
    public String transportadoraRejeitaEncomenda(String codT, String codigoEnc)throws PedidoNaoExistenteException, EntidadeNaoExistenteException, TransporteNaoExistenteException{
        Pedido e = this.pedidos.getPedido(codigoEnc); 
        List<String> rej = e.getRejeitaram();
        rej.add(codT);
        this.pedidos.removePedido(e.getEncomenda());
        String ret = this.fazPedido(e.getEncomenda(),rej); 
        Consumer<Transportadora> c = v -> v.removeEncomenda(codigoEnc);
        this.entidades.atualizaTransportadora(codT,c);
        return ret;
    }
    /**
     * Metodo que caso um Voluntario rejeite uma Encomenda escolhe outro para fazer o transporte
     * @param  String codT, String codigoEnc
     */
    public String voluntarioRejeitaEncomenda(String codT, String codigoEnc)throws PedidoNaoExistenteException, EntidadeNaoExistenteException, TransporteNaoExistenteException{
        Pedido e = this.pedidos.getPedido(codigoEnc); 
        List<String> rej = e.getRejeitaram();
        rej.add(codT);
        this.pedidos.removePedido(e.getEncomenda());
        String ret = this.fazPedido(e.getEncomenda(),rej); 
        Consumer<Voluntario> c = v -> v.removeEncomenda();
        this.entidades.atualizaVoluntario(codT,c);
        return ret;
    }

    /** 
     * Metodo que apos o Voluntario aceitar a Encomenda a adiciona ao Utilizador
     * @param String codigoEnc
     */
    public void voluntarioAceitaEncomenda(String codigoEnc)throws EntidadeNaoExistenteException{
        Encomenda e = this.pedidos.getEncomenda(codigoEnc);
        Consumer<Utilizador> c = v -> v.adicionaEncomendaPorAceitar(codigoEnc);
        this.entidades.atualizaUtilizador(e.getCodUtilizador(), c);  
        this.utilizadorAceitaEncomenda(e.getCodUtilizador(), codigoEnc); 
    }
    /** 
     * Metodo que apos a Transportadora aceitar a Encomenda indica que o Utilizador tem de decidir se aceita ou nao o serviço de transporte
     * @param String codigoT,String codigoEnc
     */
    public void transportadoraAceitaEncomenda(String codigoT,String codigoEnc)throws EntidadeNaoExistenteException{
        Encomenda e = this.pedidos.getEncomenda(codigoEnc); 
        double custo = this.entidades.getCustoEncomenda(codigoT, e);
        this.pedidos.atualizaCusto(codigoEnc, custo);
        this.pedidos.atualizaPendenteTransporte(codigoEnc, false); 
        this.pedidos.atualizaPendenteUtilizador(codigoEnc, true); 
        Consumer<Utilizador> c = v -> v.adicionaEncomendaPorAceitar(codigoEnc);
        this.entidades.atualizaUtilizador(e.getCodUtilizador(), c); 
    }

    /** 
     * Metodo que se um Utilizador rejeitar um serviço de entrega volta a procurar outra Entidade para fazer a entrega
     * @param String codu, String codEnc
     */
    public String utilizadorRejeitaEncomenda(String codu, String codEnc) throws EntidadeNaoExistenteException, TransporteNaoExistenteException, PedidoNaoExistenteException{
        Pedido e = this.pedidos.getPedido(codEnc); 
        List<String> rej = e.getRejeitaram();
        rej.add(e.getTransporte());
        
        this.pedidos.removePedido(e.getEncomenda());
        String ret = this.fazPedido(e.getEncomenda(),rej);  
        Consumer<Utilizador> c = v -> v.removeEncomenda(codEnc);
        this.entidades.atualizaUtilizador(codu, c);
        return ret; 
    }
    /**
     * Metodo que apos um Utilizador aceitar o serviço de Entrega indica à Loja que pode preparar o pedido
     * @param String codu, String codEnc
     */
    public void utilizadorAceitaEncomenda(String codu, String codEnc)throws EntidadeNaoExistenteException{
        Encomenda e = this.pedidos.getEncomenda(codEnc);
        this.pedidos.atualizaPendenteTransporte(codEnc, false); 
        this.pedidos.atualizaPendenteUtilizador(codEnc, false);
        this.pedidos.atualizaLoja(codEnc, e.getCodLoja());
        Consumer<Loja> c = v -> v.adicionaEncomenda(codEnc);
        this.entidades.atualizaLoja(e.getCodLoja(), c); 
        this.pedidos.atualizaAceite(codEnc,true); 
    }
    
    /**
     * Metodo que faz uma Loja processar a Encomenda seguinte na Lista
     * @param String codLoja, String codEnc 
    */ 
    public void processarProxEncomenda(String codLoja, String codEnc)throws EntidadeNaoExistenteException{ //loja processa uma encomenda
        this.entidades.processarProximaEnc(codLoja,codEnc);
        this.pedidos.atualizaLoja(codEnc,"");
    }
    /**
     * Metodo que simula a entrega de uma Encomenda por parte de um voluntario ou Transportadora
     * @param String encomenda
     */
    public void entregaEncomenda(String encomenda) throws EntidadeNaoExistenteException, PedidoNaoExistenteException{ //Voluntario ou empresa fazem a entrega da encomenda
        Pedido e = this.pedidos.getPedido(encomenda);
        Encomenda en = e.getEncomenda();
        long tempoInLoja = this.entidades.getTempoInLoja(en.getCodLoja());
        Boolean ret = false;
        if(e.ready()){
            String transp = e.getTransporte();
            this.pedidos.removePedido(encomenda);
            Duration p = this.entidades.entregaEncomenda(transp, en,tempoInLoja);
            RegistoEntregas n = new RegistoEntregas(e.getCusto(),transp, en, p);
            Consumer<Utilizador> d = v->v.adicionaRegistoEntrega(n);
            this.entidades.atualizaUtilizador(en.getCodUtilizador(),d);   
        }
    }
    
    /**
     * Metodo que  devolve as Encomendas pendentes de aceitaçao de uma Transportadora ou Utilizador
     * @param String codU
     * @return List<SimpleEntry<Encomenda,Double>>
     */ 
    public List<Pedido> listaDeEncomendasPendentesUtilizador(String codU)throws EntidadeNaoExistenteException{
       return this.pedidos.encomendasPendentesUtilizador(codU);
    }
    /**
     * Metodo que dado um codigo devolve todas as encomendas que estao pendentes de transporte
     * @param String codU
     * @return List<Encomenda> com as encomendas encontradas
     */
    public List<Encomenda> listaDeEncomendasPendentesTransporte(String codU)throws EntidadeNaoExistenteException{
        return this.pedidos.encomendasPendentesTransporte(codU);
    }
    /**
     * Metodo que devolve a List<String> com os codigos das lojas do Sistema
     * @return List<String> resultante
     */
    public List<String> getListLojas(){
        return this.entidades.getListLojas();
    }
    /**
     * Metodo que atualiza um dado objeto de uma Entidade atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaEntidade(String cod, Consumer<Entidade> c) throws EntidadeNaoExistenteException{
       this.entidades.atualizaEntidade(cod, c);
    }  
    /**
     * Metodo que atualiza um dado objeto de uma Loja atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaLoja(String codLoja, Consumer<Loja> c) throws EntidadeNaoExistenteException{
        this.entidades.atualizaLoja(codLoja, c);
    }
    /**
     * Metodo que atualiza um dado objeto de um Utilizador atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaUtilizador(String codUt, Consumer<Utilizador> c) throws EntidadeNaoExistenteException{
        this.entidades.atualizaUtilizador(codUt, c);
    }
    /**
     * Metodo que atualiza um dado objeto de um Voluntario atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaVoluntario(String codVol, Consumer<Voluntario> c) throws EntidadeNaoExistenteException{
        this.entidades.atualizaVoluntario(codVol, c);
    }
    /**
     * Metodo que atualiza um dado objeto de uma Transportadora atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaTransportadora(String codTransp, Consumer<Transportadora> c) throws EntidadeNaoExistenteException{
        this.entidades.atualizaTransportadora(codTransp, c);
    }
    /**
     * Metodo que devolve todas as encomendas de um dado Utilizador que ainda se encontram no sistema
     * @param String codU
     * @return List<Encomenda> resultante
     */
    public List<Encomenda> encomendasNoSistema(String codU){
        return this.pedidos.encomendasNoSistema(codU);
    }

    /**
     * Metodo que indica o estado de uma dada encomenda que se encontra no sistema
     * @param String codEntity, String codEnc
     * @return int que corresponde ao estado da encomenda
     */
    public int estadoDeEncomenda(String codEntity, String codEnc) throws PedidoNaoExistenteException{
        Pedido p = this.pedidos.getPedido(codEnc);
        int status = 0;
        if(p.getPendenteUtilizador())
            status = 1;
        else if(p.getPendenteTransporte())
            status = 2;
        else if(!p.getLoja().equals(""))
            status = 3;
        else if(!p.ready())
            status = 4;
        return status;
    }   
    /**
     * Metodo que faz as transformaçoes necessarias apos uma encomenda ter sido aceite
     * @param String codEnc
     */
    public void encomendaAceite(String codEnc) throws PedidoNaoExistenteException, EntidadeNaoExistenteException{
        Pedido p = this.pedidos.getPedido(codEnc);
        Encomenda e = p.getEncomenda();
        this.pedidos.atualizaPendenteTransporte(codEnc, false);
        double custo = this.entidades.getCustoEncomenda(p.getTransporte(), e);
        this.pedidos.atualizaCusto(codEnc, custo);
        Consumer<Loja> c = v -> v.adicionaEncomenda(codEnc);
        this.entidades.atualizaLoja(e.getCodLoja(), c); 
        this.pedidos.atualizaLoja(codEnc, e.getCodLoja()); 
        this.pedidos.atualizaAceite(codEnc,true); 
        
    }
    /**
     * Metodo que dado um codigo verifica se pertence a uma Transportador e se pertencer devolve a media da sua classificaçao
     * @param String cod
     * @return double que representa a classificaçao media
     */
    public double getRating(String cod) throws EntidadeNaoExistenteException{
        List<Integer> classi = this.entidades.classificacoes(cod);
        return classi.stream().mapToInt(v->v).average().orElse(0.0);
    }
    /**
     * Metodo que atualiza um dado objeto que implementa a interface ITransporte
     * atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaTransporte(String cod, Consumer<ITransporte> c)throws EntidadeNaoExistenteException{
        this.entidades.atualizaTransporte(cod,c);
    }
    /**
     * Metodo que atualiza um dado objeto que implementa a interface ITransporte
     * atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    public void atualizaTransporteMedico(String cod, Consumer<ITransporteMedico> c)throws EntidadeNaoExistenteException{
        this.entidades.atualizaTransporteMedico(cod,c);
    }
    /**
     * Metodo que verifica se um email ja existe 
     * @param String que representa o email
     * @return boolean resultante da verificaçao
     */
    public boolean checkEmail(String email){
        return this.entidades.checkEmail(email);
    }
}