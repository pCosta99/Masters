import Enums.Estado;
import Exception.EncomendaNotFoundException;
import Exception.NotPositiveNumberException;
import Exception.ClassificacaoInvalidaException;

import java.util.List;

/**
 * Interface com métodos utilizados pelos Transportadores.
 *
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */
public interface Transportador {
    /**
     * Função que devolve o código de um Transportador.
     * @return Código do Transportador.
     */
    public String getCodigo();

    /**
     * Função que cria um clone de um Transportador.
     * @return Transportador clonado.
     */
    public Transportador clone();

    /**
     * Função que devolve o Mail de um Transportador.
     * @return Mail do Transportador.
     */
    public String getMail();

    /**
     * Função que devolve a disponibilidade de um Transportador.
     * @return true se estiver disponível.
     */
    public boolean getAvailable();

    /**
     * Função que verifica a Password de um Transportador.
     * @param password Password a verificar.
     * @return true se coincidirem.
     */
    public boolean checkPassword(String password);

    /**
     * Função que modifica a disponibilidade de um Transportador.
     * @param available Nova disponibilidade.
     */
    public void setAvailable(boolean available);

    /**
     * Função que devolve a Localização de um Transportador.
     * @return Localização de um Transportador.
     */
    public Localizacao getLocalizacao();

    /**
     * Função que atribui uma entrega a um Transportador.
     * @param pe PossibilidadeEntrega do Transportador modificada.
     */
    public void entregar(PossibilidadeEntrega pe);

    /**
     * Função que devolve o raio de ação de um Transportador.
     * @return Raio do Transportador.
     */
    public double getRaio();

    /**
     * Função que diz se aceita encomendas médicas.
     * @return true se aceitar.
     */
    public boolean aceitoTransporteMedicamentos();

    /**
     * Função que altera o estado de aceitação de encomendas médicas.
     * @param state Novo estado de aceitação.
     */
    public void aceitaMedicamentos(boolean state);

    /**
     * Função que verifica se um Transportador está numa Localização acessivel.
     * @param l Localização.
     * @return true se a localização do Transportador for menor ou igual à Localização dada.
     */
    public boolean acessivel(Localizacao l);

    /**
     * Função que sucede uma entrega de um Transportador.
     * @param tempo Tempo da entrega.
     * @return InfoHistorico com a informação da Encomenda entregue.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public InfoHistorico entregue(double tempo) throws EncomendaNotFoundException;

    /**
     * Função que devolve a distância total percorrida por um Transportador.
     * @return Distância total de um Transportador.
     */
    public double getDistanciaTotal();

    /**
     * Função que faz um Transportador aceitar uma entrega.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public void aceitar() throws EncomendaNotFoundException;

    /**
     * Função que faz um Transportador recusar uma entrega.
     * @return PossibilidadeEntrega.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public PossibilidadeEntrega recusar() throws EncomendaNotFoundException;

    /**
     * Fução que retorna o estado de entrega de um Transportador.
     * @return Estado de entrega de um Transportador.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public Estado getEstadoEntrega() throws EncomendaNotFoundException;

    /**
     * Função que devolve o código da Encomenda da entrega de um Transportador.
     * @return Código da Encomenda.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     */
    public String getCodigoEncomenda() throws EncomendaNotFoundException;

    /**
     * Função que modifica o Raio de um Transportador.
     * @param raio Novo Raio.
     * @throws NotPositiveNumberException Quando o raio for menor que 0.
     */
    public void setRaio(double raio) throws NotPositiveNumberException;

    /**
     * Função que devolve o custo de um Transportador.
     * @param peso Peso da encomenda.
     * @param distancia Distância da entrega.
     * @param tempo Tempo da entrega.
     * @return Custo do Transportador.
     */
    public double getCusto(double peso, double distancia, double tempo);

    /**
     * Função que classifica a entrega pela parte do Transportador.
     * @param encomenda Código da encomenda entregue.
     * @param classificacao Classificação atribuida.
     * @throws EncomendaNotFoundException Quando a encomenda for inválida ou não for encontrada.
     * @throws ClassificacaoInvalidaException Quando a classificação não se encontra entre 1 e 10, inclusive.
     */
    public void classificar(String encomenda, int classificacao) throws EncomendaNotFoundException, ClassificacaoInvalidaException;

    /**
     * Função que devolve a classificção média de um Transportador.
     * @return Classificação Média do Transportador.
     */
    public double classificacaoMedia();

    /**
     * Função que converte os parâmetros de um Transportador para String.
     * @return String com os parâmetros do Transportador.
     */
    public String toString();

    /**
     * Devolve informação relativa às encomendas tranportadas pelo transportador.
     * @return Estrutura com informação sobre as encomendas transportadas.
     */
    public List<String> encomendasTransportadas();
}
