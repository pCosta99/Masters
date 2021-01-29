/**
 * Escreva a descrição da classe Histórico aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import Exception.EncomendaNotFoundException;
import Exception.ClassificacaoInvalidaException;
import Exception.OrdemCronologicaErradaException;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Classe que lida com o Historico de encomendas.
 * @author Bruno Cerqueira A89503
 * @author Ricardo Carvalho A89504
 * @author Romeu Silva A89617
 */

public class Historico implements Serializable {
    //Instance Variables
    private final Map<String,InfoHistorico> historico; //encomenda - infohistorico

    //Constructors

    /**
     * Construtor do Historico.
     */
    public Historico(){
        this.historico = new HashMap<>();
    }

    /**
     * Construtor do Historico.
     * @param t, Map com o codigo da Encomenda e o InfoHistorico da mesma, do Historico a construir.
     */
    public Historico(Map<String,InfoHistorico> t){
        this.historico = t.values()
                          .stream()
                          .collect(Collectors.toMap(InfoHistorico::getCodEncomenda,
                                                    InfoHistorico::clone));
    }

    /**
     * Construtor do Historico.
     * @param h, Historico a construir.
     */
    public Historico(Historico h){
        this.historico = h.getHistorico();
    }

    //Getters

    /**
     * Funçãp que retorna o Map com os codigos de encomenda e os seus respetivos InfoHistorico.
     * @return Map.
     */
    public Map<String,InfoHistorico> getHistorico(){
        return this.historico.values()
                             .stream()
                             .collect(Collectors.toMap(InfoHistorico::getCodEncomenda,
                                                       InfoHistorico::clone));
    }

    /**
     * Função que adiciona um InfoHistorico ao Map.
     * @param i, InfoHistorico a adicionar.
     */
    public void add (InfoHistorico i){
        this.historico.put(i.getCodEncomenda(),i.clone());
    }

    /**
     * Função que cria um clone de um Historico.
     * @return Historico clonado.
     */
    public Historico clone(){return new Historico(this);}

    /**
     * Função que compara parametros do Historico.
     * @param o, Objeto a comparar.
     * @return 'true' se forem iguais.
     */
    public boolean equals(Object o){
        if (this==o) return true;
        if(o==null || this.getClass()!=o.getClass()) return false;
        Historico h = (Historico) o;
        return this.historico.equals(h.getHistorico());
    }

    /**
     * Função que converte os parametros do Histórico para string.
     * @return String com os parametros convertidos.
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        this.historico.values().forEach(ih -> sb.append(ih).append('\n'));
        return sb.toString();
    }

    /**
     * Função que percorre o Map e calcula o Faturado num dado intervalo.
     * @param inf, Limite inferior do intervalo.
     * @param sup, Limite superior do intervalo.
     * @return Total faturado.
     * @throws OrdemCronologicaErradaException, Quando a Orden cronológica estiver errada
     */
    public double getFaturadoIntervalo(LocalDateTime inf, LocalDateTime sup) throws OrdemCronologicaErradaException {
        if(inf.isAfter(sup)) throw new OrdemCronologicaErradaException(inf,sup);
        return this.historico.values()
                             .stream()
                             .filter(h-> h.getData().isAfter(inf) && h.getData().isBefore(sup))
                             .mapToDouble(InfoHistorico::getCusto)
                             .sum();
    }

    //

    /**
     * Função que percorre o Map e calcula a distância total.
     * @return Distância total.
     */
    public double getDistanciaTotal(){
        return this.historico.values()
                             .stream()
                             .mapToDouble(InfoHistorico::getDistancia)
                             .sum();
    }

    /**
     * Função que retorna uma Stream cujos InfoHistorico nao foram classificados.
     * @return Stream.
     */
    private Stream<InfoHistorico> getNaoClassificados(){
        return this.historico.values()
                             .stream()
                             .filter(i -> !i.isClassificado());
    }

    /**
     * Função que percorre a stream anteriormente referida e retorna uma List<String> cuja String contem o codigo da encomenda e do transportador da mesma.
     * @return List.
     */
    public List<String> getListNaoClassificados(){
        return this.getNaoClassificados()
                   .map(ih -> String.format("Transportador da encomenda %s: %s",ih.getCodEncomenda(),ih.getCodTransportador()))
                   .collect(Collectors.toList());
    }

    /**
     * Função que atribui uma classificação e retorna o código do transportador classificado.
     * @param encomenda, Encomenda a partir da qual se irá determinar o InfoHistorico com o objetivo de se poder classifcar.
     * @param classificacao, Classificação a atribuir.
     * @return String com o código do transportador classificado.
     */
    public String classificar(String encomenda, int classificacao) throws ClassificacaoInvalidaException, EncomendaNotFoundException {
        InfoHistorico infoHistorico = this.historico.get(encomenda);
        if(infoHistorico == null) throw new EncomendaNotFoundException(encomenda);
        infoHistorico.setClassificacao(classificacao);
        return infoHistorico.getCodTransportador();
    }

    /**
     * Função que retorna o tamanho do Map.
     * @return Tamanho.
     */
    public int getSize(){
        return this.historico.size();
    }

    /**
     * Função que retorna um Collection<String>, cujos InfoHistoricos estão compreendidos num intervalo.
     * @param inf, Limite inferior do intervalo.
     * @param sup, Limite superior do intervalo.
     * @param transportador, Código transportador.
     * @return Collection<String>.
     * @throws OrdemCronologicaErradaException, Quando a Orden cronológica estiver errada.
     */
    public Collection<String> getInfoHistoricoIntervalo(LocalDateTime inf, LocalDateTime sup, String transportador) throws OrdemCronologicaErradaException {
        if(inf.isAfter(sup)) throw new OrdemCronologicaErradaException(inf,sup);
        return this.historico.values()
                             .stream()
                             .filter(h -> h.getData().isAfter(inf)  &&
                                          h.getData().isBefore(sup) &&
                                          h.getCodTransportador().equals(transportador))
                             .sorted()
                             .map(ih -> String.format("Data: %s, Transportador: %s, Encomenda: %s",ih.getData(),
                                                                                                   ih.getCodTransportador(),
                                                                                                   ih.getCodEncomenda()))
                             .collect(Collectors.toList());
    }

    /**
     * Função que retorna o rating médio de um dado transportador.
     * @return Rating médio.
     */
    public double ratingMedioTransportador (){
        return this.historico.values()
                             .stream()
                             .filter(InfoHistorico::isClassificado)
                             .mapToDouble(InfoHistorico::getClassificacao)
                             .average()
                             .orElse(0);
    }

    /**
     * Devolve informação relativa às encomendas tranportadas pelo transportador.
     * @return Estrutura com informação sobre as encomendas transportadas.
     */
    public List<String> getListTransportadas() {
        return this.historico.values()
                             .stream()
                             .sorted()
                             .map(InfoHistorico::toString)
                             .collect(Collectors.toList());
    }
}
