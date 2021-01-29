package Model;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;
import Exceptions.NoEntityException;

/** Interface da classe RegistoEncomendas. */
public interface IRegistoEncomendas {

    /**
     * Devolve o conjunto de encomendas que foram feitas mas que as lojas ainda não têm disponiveis para serem entregues.
     *
     * @return o conjunto das encomendas pendentes.
     */
    Map<String, IEncomenda> getEncPendentes();

    /**
     * Devolve o conjunto de encomendas que a loja tem prontas para serem entregues mas ainda não têm um entregador associado.
     *
     * @return o conjunto de encomendas prontas.
     */
    Map<String, IEncomenda> getEncProntas();

    /**
     * Devolve o conjunto de encomendas que estão prontas para serem entregues e já têm entregador associado, mas ainda não foram entregues ao utilizador.
     *
     * @return o conjunto de encomendas aceites.
     */
    Map<String, IEncomenda> getEncAceites();

    /**
     * Devolve o conjunto de encomendas que já foram entregues ao utilizador.
     *
     * @return o conjunto de encomendas finalizadas.
     */
    Map<String, IEncomenda> getEncFinalizadas();

    /**
     * Implementação do método clone().
     * @return Cópia de uma instância de RegistoEncomendas.
     */
    public RegistoEncomendas clone();
    
     /**
     * Metodo que adiciona uma Encomenda ao conjunto de encomendas Pendentes.
     */
    void adicionaPendentes(IEncomenda novaEncomenda);

    /**
     * Metodo que adiciona uma Encomenda ao conjunto de encomendas Aceites.
     */
    void adicionaAceites(String cod);

    /**
     * Metodo que passa uma encomenda do conjunto das Encomendas pendentes para o conjunto das encomendas prontas.
     *
     * @param codEnc codigo da encomenda a ser movida
     */
    void pendentesParaProntas(String codEnc) throws NoEntityException;

    /**
     * Metodo que passa uma encomenda do conjunto das Encomendas prontas para o conjunto das encomendas aceites.
     *
     * @param codEnc codigo da encomenda a ser movida
     */
    void prontasParaAceites(String codEnc) throws NoEntityException;

    /**
     * Metodo que passa uma encomenda do conjunto das Encomendas aceites para o conjunto das encomendas finalizadas.
     *
     * @param codEnc codigo da encomenda a ser movida
     */
    public void aceitesParaFinalizadas(String codEnc) throws NoEntityException;

    
    /**
     * Metodo que diz o estado de uma determinada encomenda.
     *
     * @param codEnc codigo da encomenda cujo estado queremos saber
     */
    String estadoEnc(String codEnc) throws NoEntityException;

    /**
     * Metodo que devolve a encomenda pronta de codigo enc.
     */
    public IEncomenda getEncPronta(String enc);

    /**
     * Metodo que devolve a encomenda aceite de codigo enc.
     */
    public IEncomenda getEncAceite(String enc);

    /**
     * Metodo que devolve a encomenda por aceitar de codigo enc.
     */
    IEncomenda getEncPorAceitar(String dist);

    /**
     * Metodo que devolve a encomenda de codigo enc.
     */
    public IEncomenda getEncomenda(String codEnc);

    /**
     * Metodo que devolve o conjunto de encomendas feitas por um dado utilizador
     */
    public Set<IEncomenda> getEncUt(String cod);

    /**
     * Metodo que devolve o conjunto de encomendas de produtos de uma dada loja
     * @param cod codigo da loja cujas encomendas queremos
     */
    public Set<IEncomenda> getEncLoja(String cod);

    /**
     * Metodo que passa uma encomenda de pronta para aceite
     */
    void aceitada(String enc, LocalDateTime ldt) throws NoEntityException;

    /**
     * Metodo que retira a encomenda de prontas para por aceitar e a associa ao custo dos portes da transportadora que está a ser "aconcelhada" ao utilizador
     */
    void transportadoraPorAceitar(String transportadora, String enc, BigDecimal portes) throws NoEntityException;

    /**
     * Metodo que passa a enc de por aceitar para aceite no caso que o utilizador ter aceite os custos dos portes da transportadora que lhe estava a ser aconcelhada
     */
    void transportadoraAceitada(String transportadora, LocalDateTime ldt)throws NoEntityException;

    /**
     * Metodo que retira as informações da transportadora e zera os portes no caso que o utilizador não ter aceite os custos dos portes da transportadora que lhe estava a ser aconcelhada
     */
    void transportadoraRejeitada(String transportadora) throws NoEntityException;

    /**
     * Metodo que devolve a lista de encomendas de uma determinada loja que se encontram como pendentes
     */
    List<IEncomenda> encomendasPendentesALoja(String loja);

    /**
     * Metodo que devolve a lista de map entry de Strings e encomendas que contem as encomendas feitas por um utilizador que se encontram por aceitar (ordenada em função do valor)
     */
    List<Map.Entry<String, IEncomenda>> encomendasPorAceitarDoUtilizador(String utilizador);

    /**
     * Metodo que devolve a encomenda por aceitar de codigo cod
     */
    IEncomenda encsPorAceitar(String cod);

    /**
     * Atualiza o estado de uma encomenda de pendente para pronta, mas substituindo a versão pendente por uma completamente preenchida pela loja.
     * @param atualizada encomenda atualizada.
     */
    void pendenteAtualizadaParaProntas(IEncomenda atualizada) throws NoEntityException;
}