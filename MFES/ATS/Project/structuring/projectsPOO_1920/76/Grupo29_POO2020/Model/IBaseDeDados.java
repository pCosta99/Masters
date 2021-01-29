package Model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;

import Exceptions.InvalidInputException;
import Exceptions.NoEntityException;
import Utilities.Ponto;

/** Interface da classe BaseDeDados. */
public interface IBaseDeDados {

    /**
     * Implementação do método clone().
     * @return Cópia de uma instância de BaseDeDados.
     */
    BaseDeDados clone();

    /**
     * Adiciona um utilizador.
     * @param codigo id do utilizador.
     * @param nome do utilizador.
     * @param posicao do utilizador.
     * @throws InvalidInputException quando se introduzem inputs inválidos.
     */
    void addUtilizador(String codigo, String nome, Ponto posicao) throws InvalidInputException;

    /**
     * Adiciona um utilizador.
     * @param util Utilizador.
     */
    void addUtilizador(IUtilizador util);

    /**
     * Adiciona uma transportadora.
     * @param codigo da transportadora.
     * @param nome da transportadora.
     * @param nif da transportadora.
     * @param posicao da transportadora.
     * @param raio da transportadora.
     * @param ppkm da transportadora.
     * @throws InvalidInputException quando se introduzem inputs inválidos.
     */
    void addTransportadora(String codigo, String nome, String nif, Ponto posicao, double raio, BigDecimal ppkm) throws InvalidInputException;
    
    /**
     * Adiciona uma transportadora.
     * @param transp Transportadora.
     */
    void addTransportadora(ITransportadora transp);

    /**
     * Adiciona um voluntário.
     * @param codigo do voluntário.
     * @param nome do voluntário.
     * @param posicao do voluntário.
     * @param raio do voluntário.
     * @throws InvalidInputException quando se introduzem inputs inválidos.
     */
    void addVoluntario(String codigo, String nome, Ponto posicao, double raio) throws InvalidInputException;

    /**
     * Adiciona voluntário.
     * @param volun Voluntário.
     */
    void addVoluntario(IVoluntario volun);

    /**
     * Adiciona uma loja.
     * @param codigo de uma loja.
     * @param nome de uma loja.
     * @param posicao de uma loja.
     * @throws InvalidInputException quando se introduzem inputs inválidos.
     */
    void addLoja(String codigo, String nome, Ponto posicao) throws InvalidInputException;

    /**
     * Adiciona uma loja.
     * @param loja Loja.
     */
    void addLoja(ILoja loja);

    /**
     * Adiciona uma conta.
     * @param codigo de uma conta.
     * @param email de uma conta.
     * @param password de uma conta.
     * @param notificacoes de uma conta.
     */
    void addConta(String codigo, String email, String password, List<String> notificacoes);
    
    /**
     * 
     * @param codigo de uma conta.
     * @param email de uma conta.
     * @param password de uma conta.
     */
    void addConta(String codigo, String email, String password);

    /**
     * Adiciona uma conta.
     * @param c Conta.
     */
    void addConta(IConta c);

    /**
     * Elimina uma entidade.
     * @param codigo de uma entidade
     * @throws NoEntityException se a entidade não existir.
     */
    void delEntidade(String codigo) throws NoEntityException;

    /**
     * Elimina uma conta.
     * @param email da conta.
     * @throws NoEntityException se a conta não existir.
     */
    void delConta(String email) throws NoEntityException;

    /** 
     * Adiciona um código de encomenda a um dado distribuidor.
     * 
     * @param dist   Distribuidor da dada encomenda.
     * @param codigo Código da encomenda.
     */
    void addEncomendaDistribuidor(Distribuidor dist, String codigo);

    /**
     * Incrementa o número de encomendas efetuadas por um dado utilizador.
     * @param id de um Utilizador ao qual será incrementado o número de encomendas.
     * @throws NoEntityException Caso o utilizador não exista.
     */
    void incNEncUtilizador(String id) throws NoEntityException;

    /**
     * Procura um utilizador.
     * @param codigo do utilizador.
     * @return o utilizador caso ele exista, ou null.
     */
    IUtilizador getUtilizador(String codigo);

    /**
     * Procura uma transportadora.
     * @param codigo da transportadora.
     * @return a transportadora caso ela exista, ou null.
     */
    ITransportadora getTransportadora(String codigo);

    /**
     * Procura um voluntário.
     * @param codigo do voluntário.
     * @return o voluntário caso ele exista, ou null.
     */
    IVoluntario getVoluntario(String codigo);

    /**
     * Procura uma loja.
     * @param codigo da loja.
     * @return a loja caso ela exista, ou null.
     */
    ILoja getLoja(String codigo);

    /**
     * Indica todos os utilizadores na base de dados.
     * @return a uma lista com todos os utilizadores.
     */
    List<IUtilizador> getListUtilizadores();

    /**
     * Indica todas as transportadoras na base de dados.
     * @return a uma lista com todos as transportadoras.
     */
    List<ITransportadora> getListTransportadoras();

    /**
     * Procura uma conta.
     * @param email da conta.
     * @return a conta caso ela exista, ou null.
     */
    IConta getConta(String email);

    /**
     * Verifica se uma conta existe.
     * @param email da conta.
     * @return true se existir, false se não existir.
     */
    boolean existeConta(String email);

    /**
     * Adiciona uma notificação a todos os distirbuidor exceto a um dado.
     * @param not notificação.
     * @param distCod código do distribuidor.
     * @throws NoEntityException caso o distribuidor não exista.
     */
    void addNotificacaoDistribuidoresMenosEste(String not, String distCod) throws NoEntityException;

    /**
     * Adiciona uma notifacação a uma conta com um dado email.
     * @param not notificação.
     * @param email da conta.
     * @throws NoEntityException caso não exista nenhuma conta com o email dado.
     */
    void addNotificacao(String not, String email) throws NoEntityException;

    /**
     * Indica todas as encomendas aceites por uma transportadora.
     * @param transCod código da transportadora.
     * @return um set com os códigos de todas as transportadoras.
     * @throws NoEntityException se a transportadora não existir.
     */
    Set<String> encAceitesPorTransportadora(String transCod) throws NoEntityException;

    /**
     * Indica todas as encomendas aceites por um voluntário.
     * @param transCod código do voluntário.
     * @return um set com os códigos de todos os voluntários.
     * @throws NoEntityException se o voluntário não existir.
     */
    Set<String> encAceitesPorVoluntario(String transCod) throws NoEntityException;

    /**
     * Indica os IDs de todas as contas na base de dados.
     * @return o set com todos os ids.
     */
    Set<String> getIDsContas();

    /**
     * Indica que um distribuidor concluiu uma entrega.
     * @param distribuidor código do distribuidor.
     * @param tempoEntrega tempo da entrega.
     * @param kms precorridos durante a entrega.
     * @throws InvalidInputException se algum input é inválido.
     * @throws NoEntityException se o distribuidor não existir.
     */
    void entregue(String distribuidor, LocalDateTime tempoEntrega, double kms) throws InvalidInputException, NoEntityException;

    /**
     * Indica que um distribuidor aceitou entregar uma encomenda.
     * @param distribuidor código do distribuidor.
     * @param encCod código da encomenda.
     * @return true se o distribuidor estiver disponível, false se não estiver.
     * @throws InvalidInputException se algum input é inválido.
     * @throws NoEntityException se o distribuidor não existir.
     */
    boolean aceitaEntregar(String distribuidor, String encCod) throws InvalidInputException, NoEntityException;

    /**
     * Adiciona uma notificação a uma conta que tem um dado ID.
     * @param noti notificação.
     * @param cod id.
     * @throws NoEntityException caso não exista nenhuma conta.
     */
    void addNotificacaoPorCodigo(String noti, String cod) throws NoEntityException;

    /**
     * Calcula os portes de uma entrega.
     * @param dist código da transportadora.
     * @param util código do utilizador.
     * @param loja código da loja.
     * @return o valor dos portes.
     * @throws NoEntityException caso a transportadora, o utilizador ou a loja não exista.
     */
    BigDecimal calcularPortes(String dist, String util, String loja) throws NoEntityException;

    /**
     * Adiciona uma notificação a todos os distribuidores.
     * @param notif notificação.
     */
    void addNotificacaoDistribuidores(String notif);

    /**
     * Atualiza o estado de um voluntário de que transporta (ou não) encomendas médicas.
     * @param vol código do voluntário.
     * @param b true se transportar, false se não.
     * @throws NoEntityException casoo voluntário não exista.
     */
    void transpMedicamentos(String vol, boolean b) throws NoEntityException;


    /**
     * Adiciona um classificação de um utilizador a um dado entregador.
     * @param utilizador id do utilizador
     * @param classificacao (0 - 10)
     * @param entregador id do entregador
     * @throws InvalidInputException
     */
    void addClassificacao(String utilizador, int classificacao, String entregador) throws InvalidInputException;


    /**
     * Adiciona o código de uma encomenda ao conjunto de encomendas rejeitadas de uma transportadora.
     * @param transportadora id da transportadora
     * @param encomenda id da encomenda
     */
    void rejeitada(String transportadora, String encomenda) throws NoEntityException;


    /**
     * Retira a encomenda do conjunto de rejeitadas das transportadora, caso esta exista no conjunto.
     * @param encomenda id da encomenda
     */
    void limpaRejeitadas(String encomenda);

    /**
     * Indica o distribuidor que entregou ou irá entregar uma dada encomenda.
     * @param enc id da encomenda.
     * @return distribuidor associado.
     */
    Distribuidor getDistDaEncomenda(String enc) throws NoEntityException;

    /**
     * Método que incrementa o número de elementos na fila da loja cujo código é recebido como parametro
     * @param encLoja código da loja cuja fila está a aumentar
     */
    void aumentaFilaLoja(String encLoja);

    /**
     * Método que decrementa o número de elementos na fila da loja cujo código é recebido como parametro
     * @param encLoja código da loja cuja fila está a diminuir
     */
    void diminuiFilaLoja(String encLoja);
}