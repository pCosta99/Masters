package Model;

import Model.Exceptions.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.time.Duration;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.List;

public interface ITrazAquiModel {

    /**
     * Método que devolve a metereologia atual
     * @return
     */
    int getMetereologia();

    /**
     * Método que devolve o trânsito atual
     * @return
     */
    int getTransito();
    /**
     * Regista um utilizador
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */
    void registaUtilizador(String nome, String email, String password, String nif, double latitude, double longitude) throws EmailExistenteException;

    /**
     * Regista um utilizador
     * @param cod
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */
    void registaUtilizador(String cod, String email, String nif, String nome, String password, double latitude, double longitude) throws EmailExistenteException;

    /**
     * Regista um utilizador passado como parâmetro
     * @param ut
     */
    void registaUtilizador(Utilizador ut) throws EmailExistenteException;

    /**
     * Regista uma loja
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */
    void registaLoja(String email, String nif, String nome, String password, double latitude, double longitude) throws EmailExistenteException;

    /**
     * Regista uma loja
     * @param cod
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     */
    void registaLoja(String cod, String email, String nif, String nome, String password, double latitude, double longitude) throws EmailExistenteException;

    /**
     * Regista uma loja passada como parâmetro
     * @param loja
     */
    void registaLoja(Loja loja) throws EmailExistenteException;

    /**
     * Regista um voluntario
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     * @param raio
     */
    void registaVoluntario(String email, String nif, String nome, String password, double latitude, double longitude, double raio, double velocidade) throws EmailExistenteException;

    /**
     * Regista um voluntário passado como parâmetro
     * @param voluntario
     */
    void registaVoluntario(Voluntario voluntario) throws EmailExistenteException;

    /**
     * Regista uma empresa de transporte
     * @param email
     * @param nif
     * @param nome
     * @param password
     * @param latitude
     * @param longitude
     * @param raio
     * @param precoKm
     * @param medica
     * @param velocidade
     * @throws EmailExistenteException
     */
    void registaEmpresaTransporte(String email, String nif, String nome, String password, double latitude, double longitude, double raio, double precoKm, boolean medica, double velocidade, double precoKg) throws EmailExistenteException;

    /**
     * Regista uma empresa de transporte passada como parâmetro
     * @param empresaTransporte
     */
    void registaEmpresaTransporte(EmpresaTransporte empresaTransporte) throws EmailExistenteException;

    /**
     * Regista uma encomenda passada como parâmetro
     * @param encomenda
     */
    void registaEncomenda(Encomenda encomenda) throws EmailExistenteException;

    /**
     * Método responsável pelo login do Utilizador, retorna o
     * código do utilizador se as credenciais forem válidas,
     * caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    String loginUtilizador(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException;

    /**
     * Método responsável pelo login da Loja, retorna o
     * código do utilizador se as credenciais forem válidas,
     * caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    String loginLoja(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException;

    /**
     * Método responsável pelo login do Voluntário, retorna o
     * código do utilizador se as credenciais forem válidas,
     * caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    String loginVoluntario(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException;

    /**
     * Método responsável pelo login da Empresa, retorna o
     * código do utilizador se as credenciais forem válidas,
     * caso contrário devolve uma string vazia
     * @param email
     * @param pass
     * @return
     */
    String loginEmpresa(String email, String pass) throws EmailNaoValidoException, PasswordNaoValidoException;

    /**
     * Método responsável por efetuar o pedido de uma
     * encomenda por parte de um utilizador
     * @param encomendas
     */
    void pedidoEncomenda(String cod, String codLoja, List<List<String>> encomendas, boolean medica) throws ProdutosNaoExisteException, LojaInexistenteException;

    /**
     * Método responsável por sinalizar encomendas aceites pela Loja,
     * mudando o estado de pendente para aceite.
     * @param codEnc
     */
    void aceitaEncomenda(String codEnc) throws EncomendaInexistenteException;

    /**
     * Método responsável por sinalizar encomendas aceites pela Loja,
     * mudando o estado de pendente para aceite.
     * @param codLoja
     * @param codEnc
     */
    void aceitaEncomendaLoja(String codLoja, String codEnc) throws LojaInexistenteException, EncomendaInexistenteException;


    /**
     * Método que devolve as encomendas para entrega numa string,
     * para apresentar à entidade transporte
     * @param cod
     * @return
     */
    String encomendasParaEntregaEmpresa(String cod) throws EmpresaInexistenteException;

    /**
     * Método que devolve as encomendas para entrega numa String
     * para entregar ao voluntário
     * @param cod
     * @return
     */
    String encomendasParaEntregaVoluntario(String cod);

    /**
     * Devolve as encomendas em espera numa determinada loja
     * @param codLoja
     * @return
     */
    String encomendasEmEspera(String codLoja);

    /**
     * Devolve o número de kms percorridos por uma empresa
     * @param codEmpresa
     * @return
     */
    double quantosKmEmpresa(String codEmpresa);

    /**
     * Devolve os 10 utilizadores que mais utilizam o sistema
     */
    String utilizadoresMaisAtivos();

    /**
     * Devolve as 10 empresas mais ativas (mais kilometros)
     * @return
     */
    String empresasMaisAtivas();

    /**
     * Método que devolve as encomendas a aguardar aprovação para o utilizador
     * @param codUt
     * @return
     */
    List<List<String>> encomendasAguardandoUtilizador(String codUt);

    /**
     * Utilizador aceita encomenda
     * @param codEnc
     */
    void aceitaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException;

    /**
     * Utilizador recusa encomenda
     *
     * @param codEnc
     */
    void recusaEncomendaUtilizador(String codEnc, String codTrans) throws EncomendaInexistenteException, EmpresaInexistenteException;


    /**
     * Determina a faturação de uma empresa num determinado período de tempo
     * @param codEmpresa
     * @param dataI
     * @param dataF
     * @return
     */
    double faturacaoEmpresa(String codEmpresa, LocalDate dataI, LocalDate dataF) throws EmpresaInexistenteException;


    /**
     * Retorna a faturação de uma loja num determinado intervalo de tempo
     *
     * @param codLoja
     * @param dataI
     * @param dataF
     * @return
     */
    double faturacaoLoja(String codLoja, LocalDate dataI, LocalDate dataF) throws LojaInexistenteException;

    /**
     * Suporta a classificação da encomenda por parte do utilizador
     */
    void classificarEntrega(String codEnc, int classificacao) throws EncomendaInexistenteException;

    /**
     * Devolve o histórico das encomendas entregues a um determinado utilizador
     *
     * @param codUt
     * @return
     */
    List<String> historicoEncomendasUtilizador(String codUt);


    /**
     * Devolve o histórico das encomendas processadas por uma determinada loja
     *
     * @param codLoja
     * @return
     */
    List<String> historicoEncomendasLoja(String codLoja);

    /**
     * Método que devolve os produtos do
     * sistema para poderem se encomendados
     *
     * @return
     */
    String verProdutos();

    /**
     * Adiciona um produto a uma determinada loja
     *
     * @param descricao
     * @param peso
     * @param preco
     */
    void adicionaProdutoLoja(String codLoja, String descricao, double peso, double preco);

    /**
     * Devolve o histórico das encomendas processadas por uma determinada empresa ou voluntario
     *
     * @param codTrans
     * @return
     */
    List<String> historicoEncomendasTransportadora(String codTrans);

    /**
     * Método que regista o pedido de transporte de
     * uma encomenda por parte de uma empresa
     *
     * @param codEmpresa
     * @param codEnc
     */
    void pedidoTransporteEncomenda(String codEmpresa, String codEnc) throws EncomendaInexistenteException;

    /**
     * Método que devolve os códigos das encomendas aceites
     * pelos utilizadores e que aguardam o transporte por
     * parte da empresa transportadora
     * @param codTrans
     * @return
     */
    String encomendasAceitesUtilizador(String codTrans);

    /**
     * Devolve a lista dos códigos de encomenda que estão a ser transportados
     * por uma dada empresa
     *
     * @param codEmpresa
     * @return
     */
    List<String> encomendasEmTransporteEmpresa(String codEmpresa);

    /**
     * Coloca uma encomenda como transportada
     *
     * @param codTransp
     */
    void transportaEncomendaEmpresa(String codTransp, String codEncomenda) throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException;

    /**
     * Coloca uma encomenda como transportada por um voluntario
     * @param codTransp
     */
    void transportaEncomendaVoluntario(String codTransp, String codEncomenda) throws EncomendaInexistenteException, TransportadoraNaoMedicaException;

    /**
     * Grava em ficheiro de objetos o estado atual da aplicação
     * @param nomeFicheiro
     */
    boolean guardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException;

    /**
     * Atualiza o tempo de espera médio de cada encomenda numa dada loja
     * @param codLoja
     * @param tempoEspera
     */
    void atualizaTempoEspera (String codLoja, Duration tempoEspera);

    /**
     * Regista que uma encomenda foi entregue com sucesso
     * @param codTrans
     * @param codEnc
     */
    void encomendaEntregue (String codTrans, String codEnc, Duration tempo) throws EncomendaNaoTransportadaException;

    /**
     * Permite à loja atualizar o tamanho da sua fila de espera para levantar encomendas
     * @param codLoja
     * @param tamanho
     */
    void atualizaFilaDeEspera(String codLoja, int tamanho) throws FilaDeEsperaException;

    /**
     * Atualiza a velocidade média de uma empresa
     * @param codEmpresa
     * @param vel
     */
    void atualizaVelMediaEmp(String codEmpresa, double vel);

    /**
     * Atualiza a velocidade média de um voluntário
     * @param codVol
     * @param vel
     */
   void atualizaVelMediaVoluntario(String codVol, double vel);

    /**
     * Devolve o histórico das encomendas processadas por uma determinada empresa ou voluntario
     * num determinado período de tempo
     * @param codTrans
     * @return
     */
    List<String> historicoEncomendasTransportadoraData(String codTrans, LocalDate di, LocalDate df)
            throws EmpresaInexistenteException, VoluntarioInexistenteException;

    /**
     * Atualiza o preço por Km que uma empresa cobra
     * @param codEmp
     * @param preco
     */
    void atualizaPrecoKm(String codEmp, double preco);

    /**
     * Atualiza o preço por Kg que uma empresa cobra
     * @param codEmp
     * @param preco
     */
    void atualizaPrecoKg(String codEmp, double preco);

    /**
     * Solicita a diversos utilizadores para aprovarem uma entrega de uma empresa, que irá realizar
     * diversas entergas numa só viagem
     * @param codEmpresa
     * @param codEncomendas
     * @throws EncomendaInexistenteException
     */
    void pedidoTransporteEncomendaGrupo(String codEmpresa,List<String> codEncomendas) throws EncomendaInexistenteException;

    /**
     * Empresa inicia o processo de transporte de um conjunto de encomendas para vários utilizadores
     * @param codTransp
     * @param codEncomendas
     * @throws EncomendaInexistenteException
     * @throws TransportadoraNaoMedicaException
     * @throws EncomendaNaoAceiteUtilizadorException
     */
    void transportaParaUtilizadoresGrupo(String codTransp, List<String> codEncomendas)
            throws EncomendaInexistenteException, TransportadoraNaoMedicaException, EncomendaNaoAceiteUtilizadorException;

    /**
     * Devolve o nome de um utilizador dado o seu código
     * @param codUt
     * @return
     */
    String getNomeUtilizador(String codUt);
}


