import java.util.Map;
import java.time.LocalDateTime;
import java.io.FileNotFoundException;
import java.io.IOException;	
import java.util.AbstractMap.SimpleEntry;
import java.util.List;
import java.util.function.Consumer; 
public interface ISistema{
     
     /**
     * Devolve as entidades do Sistema
     * @return Entidades entidades
     */
    Entidades getEntidades();
    /**
     * Metodo que devolve uma dada Entidade existente na classe
     * @param String cod
     * @return Entidade encontrada
     */ 
    Entidade getEntidade(String cod) throws EntidadeNaoExistenteException;
     /**
     * Devolve o Pedidos pedidos da classe
     * @return Pedidos pedidos
     */
    Pedidos getPedidos();
    /**
     * Atualiza a Entidades entidades da classe
     * @param e novo entidades da classe
     */
    void setEntidades(Entidades e);
     /**
     * Atualiza o Pedidos pedidos da classe
     * @param p novo pedidos da classe
     */
    void setPedidos(Pedidos p);
    /**
     * Metodo que adiciona uma Entidade à classe
     * @param Entidade t
     * @return boolean resultante da inserçao
     */
    boolean adicionaEntidade(Entidade e) throws AddEntidadeRepetidaException;
    /**
     * Metodo que verifica se os dados de login de uma Entidade sao validos e devolve a Entidade caso sejam validas
     * @param String email,String pass
     * @return Entidade 
     */
    Entidade validaCredenciaisEntidades(String email,String pass) throws CredenciaisErradasException, EntidadeNaoExistenteException;
    /**
     * Metodo que devolve as 10 empresas do Sistema que mais kilometros fizeram
     * @return Map<String,Double> resultante
     */
    List<SimpleEntry<String,Double>> top10EmpresasKmPercorridos();
    /**
     * Metodo que devolve os 10 utilizadores que mais fizeram compras no Sistema
     * @return Map<String,Integer> resultante
     */
    List<SimpleEntry<String,Integer>> top10Utilizadores();
    /**
     * Metodo que devolve o total faturado por uma Transportadora num dado intervalo de tempo
     * @param String codigoEmpresa,LocalDateTime t1, LocalDateTime t2
     * @return double final
     */
    double totalFaturadoPorEmpresa(String codigoEmpresa, LocalDateTime t1, LocalDateTime t2)throws EntidadeNaoExistenteException;
    /**
     * Metodo que atribuiu uma classificacao a um Voluntario ou uma Transportadora dependendo de quem fez a entrega
     * @param String nota , int nota
     */ 
    void classifica(String codigo, int nota)throws EntidadeNaoExistenteException;
    /**
     * Metodo que verifica se um dado codigo corresponde a alguma Entidade do Sistema
     * @param String cod
     * @return boolean resultante da verificaçao
     */
    boolean existeCodigo(String cod);
     /**
     * Metodo que devolve num Map<String,Produto> os produtos de uma dada Loja
     * @param String loja
     * @return Map<String,Produto> criado
     */
    Map<String,Produto> getProdutosLoja(String loja) throws EntidadeNaoExistenteException;
    /**
     * Metodo que adiciona um dado Produto a uma dada Loja
     * @param String loja , Produto p
     */
    void adicionaProdutoLoja(String loja, Produto p)throws EntidadeNaoExistenteException;
    /**
     * Metodo que dado um produto o adiciona às lojas do sistema
     * @param Produto p
     */
    void adicionaProdutoLojas(Produto p)throws EntidadeNaoExistenteException;
    /**
     * Metodo que devolve num List<RegistosTransporte> os RegistosTransporte de um Voluntario ou de uma Transportadora
     * @param String codEntity
     * @return List<RegistosTransporte> resultante
     */  
    List<RegistosTransporte> registosTransporte(String codEntity)throws EntidadeNaoExistenteException;
    /**
     * Metodo que devolve num List<RegistoEntregas> os RegistoEntregas de um Utilizador
     * @param String codEntity
     * @return List<RegistoEntrgas> resultante
     */  
    List<RegistoEntregas> getRegistosUtilizador(String codEntity)throws EntidadeNaoExistenteException;
    /**
     * Metodo que cria um Objecto da classe Pedido o adiciona à estrutura dos Pedidos e escolhe uma Entidade para fazer a entrega
     * @param Encomenda enc
     */
    String fazPedido(Encomenda enc, List<String> ignore)throws EntidadeNaoExistenteException,TransporteNaoExistenteException;
    /** 
     * Metodo que devolve a List<Encomenda> de uma dada Entidade do Sistema
     * @param String cod
     * @return List<Encomenda> resultante
     */
    List<Encomenda> listaDeEncomendas(String cod)throws EntidadeNaoExistenteException;
    /**
     * Metodo que dado um Voluntario ou Transportadora devolve a lista de encomendas que ainda nao foram entregues
     * @param String cod
     * @return List<Encomenda> resultante
     */
    List<Encomenda> listaDeEncomendasPorEntregar(String cod)throws EntidadeNaoExistenteException;
    /**
     * Metodo que caso uma Transportadora rejeite uma Encomenda escolhe outro para fazer o transporte
     * @param  String codT, String codigoEnc
     */
    String transportadoraRejeitaEncomenda(String codT, String codigoEnc)throws PedidoNaoExistenteException,EntidadeNaoExistenteException, TransporteNaoExistenteException;
    /**
     * Metodo que caso um Voluntario rejeite uma Encomenda escolhe outro para fazer o transporte
     * @param  String codT, String codigoEnc
     */
    String voluntarioRejeitaEncomenda(String codT, String codigoEnc)throws PedidoNaoExistenteException,EntidadeNaoExistenteException, TransporteNaoExistenteException;
    /** 
     * Metodo que apos o Voluntario aceitar a Encomenda a adiciona ao Utilizador
     * @param String codigoEnc
     */
    void voluntarioAceitaEncomenda(String codigoEnc)throws EntidadeNaoExistenteException;
     /** 
     * Metodo que apos a Transportadora aceitar a Encomenda indica que o Utilizador tem de decidir se aceita ou nao o serviço de transporte
     * @param String codigoT,String codigoEnc
     */
    void transportadoraAceitaEncomenda(String codigoT,String codigoEnc)throws EntidadeNaoExistenteException;
    /** 
     * Metodo que se um Utilizador rejeitar um serviço de entrega volta a procurar outra Entidade para fazer a entrega
     * @param String codu, String codEnc
     */
    String utilizadorRejeitaEncomenda(String codu, String codEnc) throws EntidadeNaoExistenteException, TransporteNaoExistenteException, PedidoNaoExistenteException;
    /**
     * Metodo que apos um Utilizador aceitar o serviço de Entrega indica à Loja que pode preparar o pedido
     * @param String codu, String codEnc
     */
    void utilizadorAceitaEncomenda(String codu, String codEnc)throws EntidadeNaoExistenteException;
    /**
     * Metodo que faz uma Loja processar a Encomenda seguinte na Lista
     * @param String codLoja, String codEnc 
    */ 
    void processarProxEncomenda(String codLoja, String codEnc)throws EntidadeNaoExistenteException;
    /**
     * Metodo que simula a entrega de uma Encomenda por parte de um voluntario ou Transportadora
     * @param String encomenda
     */
    void entregaEncomenda(String encomenda) throws EntidadeNaoExistenteException, PedidoNaoExistenteException;
    /**
     * Metodo que  devolve as Encomendas pendentes de aceitaçao de uma Transportadora ou Utilizador
     * @param String codU
     * @return List<SimpleEntry<Encomenda,Double>>
     */ 
    List<Pedido> listaDeEncomendasPendentesUtilizador(String codU)throws EntidadeNaoExistenteException;
    /**
     * Metodo que dado um codigo devolve todas as encomendas que estao pendentes de transporte
     * @param String codU
     * @return List<Encomenda> com as encomendas encontradas
     */
    List<Encomenda> listaDeEncomendasPendentesTransporte(String codU)throws EntidadeNaoExistenteException;
    /**
     * Metodo que devolve a List<String> com os codigos das lojas do Sistema
     * @return List<String> resultante
     */
    List<String> getListLojas();
     /**
     * Metodo que atualiza um dado objeto de uma Entidade atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    void atualizaEntidade(String cod, Consumer<Entidade> c) throws EntidadeNaoExistenteException;
    /**
     * Metodo que atualiza um dado objeto de uma Loja atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    void atualizaLoja(String codLoja, Consumer<Loja> c) throws EntidadeNaoExistenteException;
    /**
     * Metodo que atualiza um dado objeto de um Utilizador atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    void atualizaUtilizador(String codUt, Consumer<Utilizador> c) throws EntidadeNaoExistenteException;
     /**
     * Metodo que atualiza um dado objeto de um Voluntario atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    void atualizaVoluntario(String codVol, Consumer<Voluntario> c) throws EntidadeNaoExistenteException;
     /**
     * Metodo que atualiza um dado objeto de uma Transportadora atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    void atualizaTransportadora(String codTransp, Consumer<Transportadora> c) throws EntidadeNaoExistenteException;
    /**
     * Metodo que devolve todas as encomendas de um dado Utilizador que ainda se encontram no sistema
     * @param String codU
     * @return List<Encomenda> resultante
     */
    List<Encomenda> encomendasNoSistema(String codU);
     /**
     * Metodo que indica o estado de uma dada encomenda que se encontra no sistema
     * @param String codEntity, String codEnc
     * @return int que corresponde ao estado da encomenda
     */
    int estadoDeEncomenda(String codEntity, String codEnc) throws PedidoNaoExistenteException;
     /**
     * Metodo que dado uma String de uma Entidade devolve os RegistosTransporte da Entidade num dado intervalo de tempo 
     * @param String codEntity, LocalDateTime t1, LocalDateTime t2
     * @List<RegistosTransporte> obtido
     */
    List<RegistosTransporte> registosIntervalo(String codEntity, LocalDateTime t1, LocalDateTime t2) throws EntidadeNaoExistenteException;
    /**
     * Metodo que faz as transformaçoes necessarias apos uma encomenda ter sido aceite
     * @param String codEnc
     */
    void encomendaAceite(String cod) throws PedidoNaoExistenteException, EntidadeNaoExistenteException;
    /**
     * Metodo que dado um codigo verifica se pertence a uma Transportador e se pertencer devolve a media da sua classificaçao
     * @param String cod
     * @return double que representa a classificaçao media
     */
    double getRating(String cod) throws EntidadeNaoExistenteException;
    /**
     * Metodo que atualiza um dado objeto que implementa a interface ITransporte
     * atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */

    void atualizaTransporte(String cod, Consumer<ITransporte> c)throws EntidadeNaoExistenteException;
    /**
     * Metodo que atualiza um dado objeto que implementa a interface ITransporte
     * atraves da funçao passada como argumento
     * @param String cod, Consumer<Entidade> c
     */
    void atualizaTransporteMedico(String cod, Consumer<ITransporteMedico> c)throws EntidadeNaoExistenteException;
    /**
     * Metodo que verifica se um email ja existe 
     * @param String que representa o email
     * @return boolean resultante da verificaçao
     */
    boolean checkEmail(String email);
}