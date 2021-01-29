package Model;

import java.io.IOException;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.Set;

import Exceptions.InvalidInputException;
import Exceptions.NoEntityException;
import Utilities.Ponto;
import Utilities.Rating;

public interface IGestao {

    
    /**
     * Devolve o registo de encomendas da gestao do sistema.
     *
     * @return a gestao do sistema.
     */
    IRegistoEncomendas getRegistoEncomendas();   
    
    /**
     * Devolve a base de dados da gestao do sistema.
     *
     * @return base de dados.
     */
    IBaseDeDados getBaseDeDados();

    /**
     * Devolve o conjunto de todos os codigos de encomendas.
     *
     * @return Conjunto de todos os codigos de encomendas.
     */
    public Set<String> getCodEnc();


    /**
     * Metodo que adiciona um utilizador ao sistema.
     */
    void registaUtilizador(String id, String nome, Ponto posicao) throws InvalidInputException;

    /**
     * Metodo que adiciona um voluntario ao sistema.
     */
    void registaVoluntario(String id, String nome, Ponto posicao, double raio) throws InvalidInputException;

    /**
     * Metodo que adiciona uma transpostadora ao sistema.
     */
    void registaTransportadora(String id, String nome, String nif, Ponto local,
                                      double raio, BigDecimal ppkm) throws InvalidInputException;


    /**
     * Metodo que adiciona uma loja ao sistema.
     */
    void registaLoja(String id, String nome, Ponto local) throws InvalidInputException;


    /**
     * Metodo que adiciona uma Encomenda.
     */
    void registaNovaEncomenda(String cod, String codUtil, String codLoja, double peso,
                                     Set<LinhaEncomenda> lista, LocalDateTime data, LocalDateTime dataAcecacao, boolean medica) throws InvalidInputException, NoEntityException;


    /**
     * Metodo que adiciona uma Encomenda Aceite.
     */
    void registaAceite(String cod) throws InvalidInputException;

    /**
     * Método que adiciona uma Conta.
     * @param id da entidade.
     * @param email da conta.
     * @param password da conta.
     */
    void registaConta(String id, String email, String password) throws InvalidInputException;

    /**
     * Incrementa o número de encomendas efetuadas por um dado utilizador.
     * @param id String de um Utilizador ao qual será incrementado o número de encomendas.
     */
    void incNEncUtilizador(String id) throws InvalidInputException, NoEntityException;


    /** Query 12 
    * gravar o estado da aplicação em ficheiro, para que seja possível retomar mais tarde a execução.
    */
    void save() throws IOException;

    /**  Query 12 
    * gravar o estado da aplicação num ficheiro num certo caminho, para que seja possível retomar mais tarde a execução.
    * @param path caminho do ficheiro.
    */
    void save(String path) throws IOException;

    /**
    * Função que lê um .dat de uma Gestao.
    */
    void load() throws IOException, ClassNotFoundException;

    /** 
    * Função que lê um .dat num certo caminho de uma Gestao.
    * @param path caminho do ficheiro.
    */
    void load(String path) throws IOException, ClassNotFoundException;

    /*
    * Função que devolve um set com os IDs de todas as contas.
    */
    Set<String> getIDsContas(); 

    /**
     * Permite um utilizador classificar um entregador.
     * @param utilizador o utilizador que irá classifica.
     * @param classificacao (0 - 10).
     * @param entregador a classificar.
     * @throws InvalidInputException quando há inputs inválidos.
     * @throws NoEntityException quando o utilizador ou o entregador não existem.
     */
    void query7 (String utilizador, int classificacao, String entregador) throws InvalidInputException, NoEntityException;

    /**
    * Permite atualizar o estado de uma encomenda como pronta.
    * @param encCod a encomenda a atualizar.
    * @throws InvalidInputException quando há inputs inválidos.
    * @throws NoEntityException quando a encomenda não existe.
    */
    void lojaTemEncomendaPronta(String encCod) throws InvalidInputException, NoEntityException;

    /**
    * Função que devolve todas as encomendas que estão disponíveis para entrega.
    * @return Map<String, IEncomenda> com todas as encomendas prontas para entrega.
    */
    Map<String, IEncomenda> encomendasDisponiveisParaEntrega ();


    /**
    * Devolve todas as encomendas associadas a um distribuidor.
    * @param id do distribuidor.
    * @throws InvalidInputException quando há inputs inválidos.
    * @throws NoEntityException quando o distribuidor não existem.
    * @return Set<String> com os códigos das encomendas.
    */
    Set<String> getEncsDistribuidor(String id) throws InvalidInputException, NoEntityException;;

     /**
    * Indica o total faturado por uma transportadora.
    * @param idTransp da transportadora.
    * @param inicio data inicial.
    * @param final data final.
    * @throws InvalidInputException quando há inputs inválidos.
    * @throws NoEntityException quando o distribuidor não existem.
    * @return double total faturado.
    */
    double query9(String idTransp, LocalDateTime inicio, LocalDateTime fim) throws InvalidInputException, NoEntityException;

     /**
    * Top 10 dos utilizadores que mais utilizaram o sistema.
    * @return a lista ordenada dos utilizadores.
    */
    List<IUtilizador> query10();

    /**
    * Top 10 das transportadoras que mais utilizaram o sistema.
    * @return a lista ordenada dos utilizadores.
    */
    List<ITransportadora> query11();

    /**
    * Adiciona uma conta à Gestao
    * @param c conta a adicionar.
    */
    void addConta(IConta c);

    /**
     * Indica que um distribuidor concluiu uma entrega.
     * @param distribuidor que concluiu a entrega.
     * @param encCod da entrega.
     * @param tempoEntrega tempo em que concluiu a entrega.
     * @param kms totais feitos na entrega.
     * @throws InvalidInputException quando há inputs inválidos.
     * @throws NoEntityException quando o distribuidor, ou a entrega não existem.
     */
    void entregue(String distribuidor, String encCod, LocalDateTime tempoEntrega, double kms) throws InvalidInputException, NoEntityException;
    /**
     * Informa sobre todas as encomendas associadas a uma entidade.
     * @param cod da entidade.
     * @return Lista com as entradas que contêm as encomendas assim como os seus código, ordenada por ordem anti-cronológica.
     * @throws InvalidInputException quando há inputs inválidos.
     * @throws NoEntityException quando o distribuidor, ou a entrega não existem.
     */
    List<Map.Entry<IEncomenda, String>> query8 (String cod) throws InvalidInputException, NoEntityException;

    /**
     * Atualiza o estado de uma encomenda para aceita. 
     * No caso de transportadoras, ficam à espera que o utilizador em questão aceite a encomenda.
     * @param distribuidor id do distribuidor.
     * @param encCod da encomenda.
     * @return boolean se o distribuidor não está disponível para entregar.
     * @throws InvalidInputException quando há inputs inválidos.
     * @throws NoEntityException quando o distribuidor, ou a entrega não existem.
     */
    boolean aceitaEntregar(String distribuidor, String encCod) throws InvalidInputException, NoEntityException;

    /**
     * Informa sobre todas as encomendas feitas a uma loja.
     * @param loja id da loja.
     * @return Lista com todas as encomendas, ordenadas por ordem anti-cronológica.
     */
    List<IEncomenda> encomendasPendentesALoja(String loja);

    /**
     * Informa sobre todas as encomendas que um utilizador tem por aceitar.
     * @param utilizador id do utilizador.
     * @return Lista com as entradas que contêm as encomendas assim como os seus código, ordenada por ordem anti-cronológica.
     */
    List<Map.Entry<String, IEncomenda>> nEncomendasPorAceitarDoUtilizador(String utilizador);

    /**
     * Atualiza o estado de uma encomenda sobre a decisão que um utilizador fez sobre uma transportadora.
     * @param decisao true se aceitou, false se não.
     * @param utilizador id do utilizador.
     * @param transportadora id da transportadora.
     * @throws InvalidInputException quando há inputs inválidos.
     * @throws NoEntityException quando o distribuidor, ou a entrega não existem.
     */
    void utilizadorAceitaTranportadora(boolean decisao, String utilizador, String transportadora) throws InvalidInputException, NoEntityException;

    /**
     * Indica todas as encomendas ao alcance de um distribuidor que estão disponíveis para entregar.
     * @param dist id do distribuidor.
     * @return Lista com todas as encomendas.
     * @throws NoEntityException quando o distribuidor, ou a entrega não existem.
     */
    List<IEncomenda> encomendasDisponiveisParaEntrega(String dist) throws NoEntityException;

    /**
     * Muda um voluntário para poder (ou não) transportar encomendas médicas.
     * @param vol id do voluntário.
     * @param b true se pode transportar, false se não.
     * @throws NoEntityException quando o distribuidor, ou a entrega não existem.
     */
    void transpMedicamentos(String vol, boolean b) throws NoEntityException;

    /**
     * Atualiza o estado de uma encomenda de pendente para pronta, mas substituindo a versão pendente por uma completamente preenchida pela loja.
     * @param atualizada encomenda atualizada.
     */
    void pendenteAtualizadaParaProntas(IEncomenda atualizada) throws NoEntityException;

    /**
     * Calcula as datas previstas de encomendas.
     * @param encs Set de códigos de encomendas.
     * @return os códigos de encomendas e as datas associadas.
     */
    Map<String, LocalDateTime> dataPrevistaEntregas(Set<String> encs) throws NoEntityException;

    /**
     * Devolve a classificação de um dado distribuidor.
     * @param code código do distribuidor
     * @return classificação
     * @throws NoEntityException caso o distribuidor não exista
     * @throws InvalidInputException caso o código dado não seja de um distribuidor
     */
    Rating getDistRating(String code) throws NoEntityException, InvalidInputException;
}