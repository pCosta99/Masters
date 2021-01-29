import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

public interface IModelo {
    Collection<Utilizador> getUsers();

    MeioTransporte getTransportador(String codigo);

    Collection<Loja> getLojas();

    Collection<MeioTransporte> getTransportadores();

    Collection<Encomenda> getEncomendas();

    void adicionaCatalogoALoja(String codigo);

    void loadFromLogs();

    String toString();

    boolean equals(Object o);

    boolean existeConta(String codigo);

    boolean passCorreta(String codigo, String pass);

    void novoUtilizador(String codigo, String nome, GPS gps, String email, String password);

    void novoVoluntario(String codigo, String nome, GPS gps, String email, String password, double raio, boolean certificado, double velocidade);

    void novaTransportadora(String codigo, String nome, GPS gps, String email, String password, String nif,
                            double raio, double taxaDistancia, double taxaPeso, boolean variasEncs, boolean certificado, double velocidade);

    void novaLoja(String code, String nome, GPS gps, String email,
                  String pw, boolean infoFilas);

    String lojasDisponiveis();

    boolean lojaTemInfoFilaEspera(String cod);

    boolean existeLoja(String cod);

    String buscarProdsAoCat(String loja, int p);

    boolean existeProdutoNaLoja(String codLoja, String codProd);

    LinhaEncomenda criarLinha(double qtd, String cod, String codLoja);

    String estadoEncomenda(Collection<LinhaEncomenda> c);

    double precoTotalEncomenda(Collection<LinhaEncomenda> c);

    double calculaPesoCarrinho(Collection<LinhaEncomenda> carrinho);

    boolean existeCodEnc(String cod);

    String gerarCodigoEnc();

    void novaEncomenda(Encomenda e);

    String pessoasEmEspera(String cod);

    String tempoAtendimentoPorPessoa(String cod);

    String tempoEstimadoDeEspera(String cod);

    void setNumeroDePessoasEmEspera(String cod, int num);

    void setTempoMedioAtendimentoPorPessoa(String cod, int num);

    String finalizaUmaEnc(String codigo);

    String finalizaVariasEncomendas(String codigo, String codEnc);

    String historicoEncTransp(String codigo);

    String historicoEncLojas(String codigo);

    String historicoEncUtilizador(String codigo);

    String imprimeEncNovasLojas(String codigo);

    boolean tryParseInt(String value);

    void alteraDisponibilidadeTransporte(String codT);

    void alteraDisponibilidadeTransporteMedico(String codTransp);

    boolean existeEncomendaNasEncomendasDisponiveis(String codTransportador, String codEncomenda);

    String aceitaUmaEncomenda(String codigoTransporte, String codEncomenda);

    double distanciaTotal(GPS gpsTransporte, GPS gpsLoja, GPS gpsUtilizador);

    boolean Certificado(String codigo);

    boolean podeTransportarVariasEncomendas(String codigo);

    boolean estaDisponivel(String codigo);

    String mostraEstados(String codigo, int signal);

    ArrayList<Encomenda> encomendasDisponiveisAuxiliar(String codigo);

    Map<Double, Encomenda> encomendasDisponiveis(String codigoMeioTransporte);

    String verEncomendasDisponiveis(Map<Double, Encomenda> listaEncomendas);

    String encomendasASerEntreguesTransportadorasVariasEncomendas(String codTransportadora);

    void sinalizaEncomendaProntaParaEntrega(String codigo);

    boolean estaNoMomentoATransportarOuEmAceitacao(String codigo);

    int totalFaturadoEmpTrans(String codigo, LocalDate inicio, LocalDate fim);

    LocalDate dateInput(String userInput);

    String utilizadoresMaisAtivos();

    String empresasTransportadorasMaisAtivas();

    String verEncomendasPorAceitar(String codigo);

    boolean existeEncomendaPorAceitarCodUser(String codigo);

    boolean existeEncomendaPorAceitarCodEnc(String codigo);

    void sinalizaEncomendaAceite(String codigo);

    void sinalizaEncomendaRejeitada(String codigo);

    String verEncomendasPorClassificar(String codigo);

    boolean existeEncomendaPorClassificar(String codigo);

    void classificaEncomenda(String codigo, int classificacao);

    String historicoEncUtilizadorFiltrado(LocalDate inicio, LocalDate fim, String codUser, String codTransportadora);

    boolean existeCodMeioTransporte(String st);
}
