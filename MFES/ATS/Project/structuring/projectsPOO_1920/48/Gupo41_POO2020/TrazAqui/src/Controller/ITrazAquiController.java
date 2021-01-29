package Controller;

import Model.Exceptions.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

public interface ITrazAquiController {

    /**
     * Método que processa o registo de um utilizador
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @throws EmailExistenteException
     */
    void processaRegistaUtilizador(String nome, String email, String password, String nif,
                                   String latitude, String longitude) throws EmailExistenteException;

    /**
     * Método que processa o registo de uma loja
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @throws EmailExistenteException
     */
    void processaRegistaLoja(String nome, String email, String password, String nif, String latitude,
                             String longitude) throws EmailExistenteException;

    /**
     * Método que processa o registo de um voluntário
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @param raio
     * @throws EmailExistenteException
     */
    void processaRegistaVoluntario(String nome, String email, String password, String nif, String latitude,
                                   String longitude, String raio, String velocidade) throws EmailExistenteException;

    /**
     * Método que processa o registo de uma empresa
     * @param nome
     * @param email
     * @param password
     * @param nif
     * @param latitude
     * @param longitude
     * @param raio
     * @param precoKm
     * @throws EmailExistenteException
     */
    void processaRegistaEmpresa(String nome, String email, String password, String nif, String latitude,
                                String longitude, String raio, String precoKm, boolean medica,
                                String velocidade, String precoPorKg) throws EmailExistenteException;

    /**
     * Método que processa o login de uma entidade
     * @param tipo
     * @param email
     * @param pass
     * @return
     * @throws EmailNaoValidoException
     * @throws PasswordNaoValidoException
     */
    String processaLogin(String tipo, String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException;

    /**
     * Método que processa um pedido de encomenda por parte de um utilizador
     * @param codUtil
     * @param codLoja
     * @param encomendas
     * @throws ProdutosNaoExisteException
     * @throws LojaInexistenteException
     */
    void processaPedidoEncomenda(String codUtil, String codLoja, List<List<String>> encomendas,
                                 boolean encomenda) throws ProdutosNaoExisteException, LojaInexistenteException;

    /**
     * Método que processa uma encomenda aceite por uma loja
     * @param codLoja
     * @param codEnc
     * @throws LojaInexistenteException
     * @throws EncomendaInexistenteException
     */
    void processaEncomendaPronta(String codLoja, String codEnc) throws LojaInexistenteException, EncomendaInexistenteException;

    /**
     * Método que processa as encomendas em espera para serem mostradas à empresa
     * @param codEmp
     * @return
     * @throws EmpresaInexistenteException
     */
    String processaEncomendasParaEntrega(String codEmp) throws EmpresaInexistenteException;

    /**
     * Método que processa as encomendas pedidas
     * a uma dada loja, para serem mostradas à mesma
     * @param codLoja
     * @return
     */
    String processaEncomendasPedidas(String codLoja);

    /**
     * Método que processa as encomendas aceites
     * por uma empresa de transporte, que aguardam
     * aprovação do utilizador
     * @param codUtil
     * @return
     */
    String processaEncomendasAguardandoUtilizador(String codUtil);

    /**
     * Método responsável por aceitar o transporte
     * de uma encomenda por parte do utilizador,
     * através de uma empresa de transporte
     * @param codEnc
     */
    void processaAceitaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException;


    /**
     * Método responsável por recusar o transporte
     * de uma encomenda por parte do utilizador,
     * através de uma empresa de transporte
     *
     * @param codEnc
     */
    void processaRecusaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException;

    /**
     * Método responsável por processar uma consulta de faturação de uma empresa
     * @param cod
     * @param dataI
     * @param dataF
     * @return
     */
    double processaFaturacaoEmp(String cod, LocalDate dataI, LocalDate dataF) throws EmpresaInexistenteException;

    /**
     * Método responsável por processar uma consulta de faturação de uma empresa
     * @param cod
     * @param dataI
     * @param dataF
     * @return
     */
    double processaFaturacaoLoja(String cod, LocalDate dataI, LocalDate dataF) throws LojaInexistenteException;

    /**
     * Método que processa a classificação de uma encomenda
     *
     * @param codEnc
     * @param classificacao
     */
    void processaClassificarEncomenda(String codEnc, int classificacao) throws EncomendaInexistenteException;

    /**
     * Método que processa o histórico de encomendas feitas
     * por um utilizador para serem apresentadas na view
     *
     * @param codUt
     */
    String processaHistoricoEntregasUt(String codUt);

    /**
     * Método que processa o histórico de encomendas feitas
     * por uma loja para serem apresentadas na view
     *
     * @param codLoja
     */
    String processaHistoricoEntregasLoja(String codLoja);

    /**
     * Método que processa o histórico de encomendas feitas
     * por um voluntário para serem apresentadas na view
     *
     * @param codTransportadora
     */
    String processaHistoricoEntregasTransportadora(String codTransportadora);

    /**
     * Método que devolve os produtos disponiveis
     * para serem vistos pelo utilizador
     *
     * @return
     */
    String processaVerProdutos();

    /**
     * Método que processa o registo de um produto
     * por parte de uma loja
     *
     * @param codLoja
     * @param descricao
     * @param peso
     * @param preco
     */
    void processaRegistaProduto(String codLoja, String descricao, String peso, String preco);

    /**
     * Método que processa o pedido de transporte de
     * uma encomenda por parte de uma empresa
     *
     * @param codEmp
     * @param codEnc
     */
    void processaPedidoTransporteEncomenda(String codEmp, String codEnc) throws EncomendaInexistenteException;

    /**
     * Método que devolve os códigos das encomendas aceites
     * pelos utilizadores e que aguardam o transporte por
     * parte da empresa transportadora
     *
     * @param codTrans
     * @return
     */
    String processaEncomendasAceitesUtilizador(String codTrans);

    /**
     * Método que devolve uma string com os códigos da
     * encomendas que a empresa pode transportar
     * @param codEmp
     */
    String processaEncomendaParaTransporte(String codEmp);

    /**
     * Método que processa as encomendas que o
     * voluntário pode transportar
     *
     * @param codVoluntario
     * @return
     */
    String processaEncomendasParaEntregaVoluntario(String codVoluntario);

    /**
     * Método que sinaliza uma encomenda como
     * transportada por um voluntario
     *
     * @param codTrans
     * @param codEnc
     */
    void processaTransportarEncomendaEmpresa(String codTrans, String codEnc)
            throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException;

    /**
     * Método que sinaliza uma encomenda como
     * transportada por um voluntario
     *
     * @param codTrans
     * @param codEnc
     */
    void processaTransportarEncomendaVoluntario(String codTrans, String codEnc)
            throws EncomendaInexistenteException, TransportadoraNaoMedicaException;

    /**
     * Método que processa o resulta da consulta dos dez
     * utilizadores mais ativos do sistema
     */
    String processaUtilizadoresMaisAtivos();

    /**
     * Método que processa o resultado da consulta das dez
     * empresas mais ativas do sistema (mais Kms)
     * @return
     */
    String processaEmpresaMaisAtivas();

    /**
     * Processa a gravação em ficheiro de objetos do estado atual da aplicação
     * @param nomeFicheiro
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     */
    boolean processaGuardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException;

    /**
     * Processo o carregamento do estado da aplicação a partir de um ficheiro de objetos
     * @param nomeFicheiro
     * @return
     * @throws FileNotFoundException
     * @throws IOException
     * @throws ClassNotFoundException
     */
    boolean processaCarregarEstado(String nomeFicheiro) throws FileNotFoundException, IOException, ClassNotFoundException;

    /**
     * Método encarregue de atualizar o tempo médio de espera de cada encomenda numa determinada loja
     * @param tempoEspera
     */
    void processaAtualizaTempoEspera (String codLoja, Duration tempoEspera);

    /**
     * Método encarregue de sinalizar que uma transportadora acabou de entregar uma encomenda com sucesso
     * @param codTransportadora
     * @param codEnc
     */
    void processaEncomendaEntregue (String codTransportadora, String codEnc, Duration tempo) throws EncomendaNaoTransportadaException;

    /**
     * Permite à loja atualizar o tamanho da fila de espera para levantar encomendas
     * @param codLoja
     * @param tamanho
     */
    void processaTamanhoFila(String codLoja, int tamanho) throws FilaDeEsperaException;

    /**
     * Permite atualizar a velocidade média de uma empresa
     * @param codEmpresa
     * @param vel
     */
    void processaAtualizaVelocidadeEmp(String codEmpresa, double vel);

    /**
     * Permite atualizar a velocidade média de um voluntário
     * @param coVol
     * @param vel
     */
    void processaAtualizaVelocidadeVol(String coVol, double vel);
    /**
     * Devolve o histórico das encomendas processadas por uma determinada empresa ou voluntario
     * num determinado período de tempo
     * @param cod
     * @param di
     * @param df
     * @return
     */
    String processaHistoricoEncomendasTransportadoraData(String cod, LocalDate di, LocalDate df)
            throws EmpresaInexistenteException, VoluntarioInexistenteException;

    /**
     * Permite a uma empresa transportadora atualizar o preço por Km a cobrar
     * @param codEmp
     * @param preco
     */
    void processaAtualizaPrecoKm(String codEmp, double preco);

    /**
     * Permite a uma empresa transportadora atualizar o preço por Kg a cobrar
     * @param codEmp
     * @param preco
     */
    void processaAtualizaPrecoKg(String codEmp, double preco);

    /**
     * Processa o pedido de transporte de várias encomendas para vários utilizadores
     * @param codTransp
     * @param encs
     * @throws EncomendaInexistenteException
     */
    void processapedidoTransporteEncomendaGrupo(String codTransp, List<String> encs) throws EncomendaInexistenteException;

    /**
     * Processa o inicio do processo de transporte de um conjunto de encomendas para vários utilizadores
     * @param codTransp
     * @param encs
     * @throws EncomendaInexistenteException
     * @throws TransportadoraNaoMedicaException
     * @throws EncomendaNaoAceiteUtilizadorException
     */
    void processatransportaParaUtilizadoresGrupo(String codTransp, List<String> encs)
            throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException;

    /**
     * Método que devolve a metereologia atual
     * @return
     */
    int processaGetMetereologia();

    /**
     * Método que devolve o trânsito atual
     * @return
     */
    int processaGetTransito();

    /**
     * Método responsável por devolver o nome de um utilizador
     * @param codUt
     * @return
     */
    public String processaGetNomeUt(String codUt);
}
