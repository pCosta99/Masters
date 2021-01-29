import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toList;

public class GestorEncomendas implements Serializable {
    private Map<String, Encomenda> encomendas;

    public GestorEncomendas(){
        this.encomendas = new HashMap<String, Encomenda>();
    }

    public GestorEncomendas(Map<String, Encomenda> encomendas){
        this.encomendas = encomendas;
    }

    public GestorEncomendas(GestorEncomendas umaBaseDados){
        this.encomendas = umaBaseDados.getEncomendas();
    }

    /**
     * Getters & Setters
     */

    /**
     * Obter uma copia do mapeamento das Encomendas
     * @return Map<String: código da Encomenda, Encomenda>
     */
    public Map<String, Encomenda> getEncomendas(){
        return this.encomendas;
    }

    /**
     * @param novasEncomendas
     */
    public void setEncomendas(Map<String, Encomenda> novasEncomendas){
        this.encomendas = novasEncomendas;
    }

    /**
     * Verificar a existência de uma encomenda dado o seu codigo
     * @param cod da encomenda
     * @return True se a encomenda existir
     */
    public boolean existeEncomenda(String cod){
        return this.encomendas.containsKey(cod);
    }

    /**
     *
     * @return Número de encomendas
     */
    public int numEncomendas(){
        return this.encomendas.size();
    }

    /**
     * Verifica quantas encomendas existem de um determinado estado
     * @param estado Estado da encomenda ("0 - Pedido", "1 - Preparar", "2 - Pronto" , "3 - Transporte", "4 - Entregue")
     * @return Número de encomendas de um determinado estado
     */
    public int quantosT(int estado){
        int count=0;
        for(Encomenda e : this.encomendas.values())
            if(e.getEstado() == estado)
                count++;
        return count;
    }

    /**
     * @param estado Estado da encomenda ("0 - Pedido", "1 - Preparar", "2 - Pronto" , "3 - Transporte", "4 - Entregue")
     * @return Lista de encomendas com os códigos (String) das encomendas com um certo estado
     */
    public List<String> getEncomendasEstado(int estado){
        return this.encomendas.values()
                .stream()
                .filter(enc -> enc.getEstado() == estado)
                .map(Encomenda::getCodigo)
                .collect(toList());
    }


    /**
     * @param codigosEncomenda codigos de encomendas
     * @return Lista de encomendas
     * @throws EncomendaInexistenteException não existe Encomenda com o seguinte código
     */
    public List<Encomenda> getEncomendas(List<String> codigosEncomenda) throws EncomendaInexistenteException {
        List<Encomenda> l = new ArrayList<>();

        for(String codigo : codigosEncomenda)
            l.add(getEncomenda(codigo));

        return l;
    }
    /**
     * Adiciona a informação de uma nova encomenda
     * @param e nova encomenda a inserir
     */
    public void addEncomenda(Encomenda e){
        this.encomendas.put(e.getCodigo(), e.clone());
    }

    /**
     * @param cod código da encomenda a procurar
     * @return cópia da encomenda encontrada
     * @throws EncomendaInexistenteException caso o código da encomenda não exista
     */
    public Encomenda getEncomenda(String cod) throws EncomendaInexistenteException{
        Encomenda e;
        if(!encomendas.containsKey(cod))
            throw new EncomendaInexistenteException(cod);
        e = encomendas.get(cod);
        return e;
    }

    public int tamanho(){
        return this.encomendas.size();
    }

    /**
     *
     * @return Lista de encomendas que contém produtos médicos
     */
    public List<Encomenda> getEncomendasMedicas(){
        return this.encomendas.values()
                .stream()
                .filter(Encomenda::isEncomendaMedica)
                .collect(toList());
    }

    /**
     *
     * @return Lista de códigos de encomendas que contêm produtos méidocs
     */
    public List<String> getCodigosEncomendasMedicas(){
        return this.encomendas.values()
                .stream()
                .filter(Encomenda::isEncomendaMedica)
                .map(Encomenda::getCodigo)
                .collect(toList());
    }

    /**
     * @return Lista das encomendas
     */
    public List<Encomenda> getEncomendaAsList(){
        return new ArrayList<>(encomendas.values());
    }

    /**
     *
     * @param localizcao Localização da entidade
     * @param raio Raio máximo
     * @return Lista de encomendas dentro de um raio
     */
    public List<Encomenda> getEncomedasRaio(GPS localizcao, double raio){
        return this.encomendas.values().stream()
                .filter(enc -> enc.getLocalizacao().dentroRaio(localizcao, raio))
                .collect(Collectors.toList());
    }

    // ----------------- Clientes ----------------- \\

    /**
     *
     * @param cliente Código de um cliente
     * @return Lista de códigos de encomendas entregues a um cliente
     */
    public List<Encomenda> getEncomendasCliente(String cliente){
        return encomendas.values()
                .stream()
                .filter(enc -> enc.getEstado() == 4) //Estado = 4 -> encomenda entregue
                .filter(enc -> enc.getDestinatario().equals(cliente))
                .collect(toList());
    }

    public List<Encomenda> historicoEncomendas(LocalDateTime periodo, String cliente){
        return getEncomendasCliente(cliente).stream()
                .filter(enc -> enc.getEntregue().isAfter(periodo))
                .collect(toList());
    }

    // ----------------- Estafetas ----------------- \\

    /**
     *
     * @param codigoEstafeta Código de um estafeta
     * @return Lista de códigos de encomendas realizadas por um estafeta
     */
    public List<String> getEncomendasEstafeta(String codigoEstafeta){
        return encomendas.values()
                .stream()
                .filter(enc -> enc.getEstafeta().equals(codigoEstafeta))
                .map(Encomenda::getCodigo)
                .collect(toList());
    }

    public List<String> getPedidosPeriferia(String codigoEstafeta, GPS localizacao, double raio, boolean medico){
        List<Encomenda> periferia =  encomendas.values()
                                            .stream()
                                            .filter(enc -> enc.getEstado() == 0)
                                            .filter(enc -> enc.getLocalizacao().dentroRaio(localizacao,raio))
                                            .collect(toList());

        return medico ? periferia.stream().map(Encomenda::getCodigo).collect(toList())
                      : periferia.stream().filter(Encomenda::isEncomendaMedica).map(Encomenda::getCodigo).collect(toList());

    }

    /**
     *
     * @param estafetas códigos de estafetas
     * @return encomendas que certos estafetas realizaram
     */
    public List<String> getEncomendasEstafetas(List<String> estafetas){
        List<String> encs = new ArrayList<>();

        for(String e : estafetas){
            encs.addAll(getEncomendasEstafeta(e));
        }
        return encs;
    }

    /**
     *
     * @param codigoEstafeta Código de um estafeta
     * @return Classificação de um estafeta
     */
    public int getClassificacaoEstafeta(String codigoEstafeta){
        return (int) encomendas.values()
                .stream()
                .filter(enc -> enc.getEstafeta().equals(codigoEstafeta))
                .mapToInt(Encomenda::getClassificacao)
                .average().orElse(0.0);
    }

    /**
     *
     * @param estafetas Lista de códigos de estafetas
     * @return classificação de um conjunto de estafetas
     */
    public int getClassificacaoEstafetas(List<String> estafetas){
        List<Integer> stars = new ArrayList<>();
        for(String e : estafetas){
            stars.add(getClassificacaoEstafeta(e));
        }
        return (int) stars.stream().mapToInt(a -> a).average().orElse(0.0);
    }



    // ----------------- Lojas ----------------- \\
    public List<Encomenda> getHistoricoLoja(String codigoLoja, int estado){
        return this.encomendas.values()
                .stream()
                .filter(enc -> enc.getEstabelecimento().equals(codigoLoja))
                .filter(enc -> enc.getEstado() == estado)
                .collect(Collectors.toList());
    }

}
