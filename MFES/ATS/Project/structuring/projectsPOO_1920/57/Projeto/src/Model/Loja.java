/**
 * classe que representa uma loja
 */
package Model;

import java.io.Serializable;
import java.util.*;

public class Loja implements Serializable {

    private String storeCode;
    private String storeName;
    private Coordenadas gps;
    private boolean hasQueueInfo;
    private double queueTime;
    private int queueSize;
    private List<String> prods;
    private Set<String> encomendas;
    private List<Notificacao> notificacoes;

    //--------------------------------------------------------------Construtores--------------------------------------------------------------------------\\

    public Loja() {
        this("", "", new Coordenadas(), false, -1, 0, new ArrayList<>(), new ArrayList<>());
    }

    public Loja(String storeCode, String storeName, Coordenadas gps, boolean hasQueueInfo, double queueTime, int queueSize, List<String> prods, List<Notificacao> notificacoes) {
        this.storeCode = storeCode;
        this.storeName = storeName;
        this.gps = gps.clone();
        this.hasQueueInfo = hasQueueInfo;
        this.queueTime = queueTime;
        this.queueSize = queueSize;
        this.prods = new ArrayList<>(prods);
        this.notificacoes = new ArrayList<>(notificacoes);
        this.encomendas = new TreeSet<>();
    }

    public Loja(Loja l) {
        this.storeCode = l.getStoreCode();
        this.storeName = l.getStoreName();
        this.gps = l.getGps();
        this.hasQueueInfo = l.isHasQueueInfo();
        this.queueTime = l.getQueueTime();
        this.queueSize = l.getQueueSize();
        this.prods = l.getProds();
        this.notificacoes = l.getNotificacoes();
        encomendas = l.getEncomendas();
    }

    //--------------------------------------------------------------Getters e Setters--------------------------------------------------------------------------\\

    /**
     * devolve store code
     * @return store code
     */
    public String getStoreCode() {
        return storeCode;
    }

    /**
     * devolve nome
     * @return nome
     */
    public String getStoreName() {
        return storeName;
    }

    /**
     * devolve gps
     * @return gps
     */
     public Coordenadas getGps() {
        return gps.clone();
    }

    /**
     * devolve isHasQueueInfo
     * @return isHasQueueInfo
     */
    public boolean isHasQueueInfo() {
        return hasQueueInfo;
    }

    /**
     * devolve queue time
     * @return queue time
     */
    public double getQueueTime() {
        return queueTime;
    }

    /**
     * altera queue time
     * @param queueTime queue time
     */
    public void setQueueTime(double queueTime) {
        this.queueTime = queueTime;
    }

    /**
     * devolve o tamanho da fila
     * @return  Inteiro que representa o tamanho da fila
     */
    public int getQueueSize() {
        return queueSize;
    }

    /**
     * altera o tamanho da fila
     * @param queueSize novo tamanho da fila
     */
    public void setQueueSize(int queueSize) {
        this.queueSize = queueSize;
    }

    /**
     * devolve produtos
     * @return  list codigos de produtos
     */
    public List<String> getProds() {
        return new ArrayList<>(prods);
    }

    /**
     * devolve codigos de encomendas
     * @return  set de codigos de encomendas
     */
    public Set<String> getEncomendas() {
        return new TreeSet<>(encomendas);
    }

    /**
     * devolve notificaçoes
     * @return list de notificaçoes
     */
    public List<Notificacao> getNotificacoes() {
        return notificacoes;
    }

    /**
     * devolve numero de notificaçõpes
     * @return numero de notificações
     */
    public int getNumNotificacoes() {
        return notificacoes.size();
    }

    /**
     * adicionar notificação
     * @param not     coteúdo da notificação
     * @param type    type
     * @param estCode estafeta code
     */
    public void addNotificacao(String not, int type, String estCode) {
        notificacoes.add(new Notificacao(not, type, estCode));
    }

    /**
     * adiciona encCode
     * @param enc encCode
     */
    public void addEncomenda(String enc) {
        encomendas.add(enc);
    }

    //--------------------------------------------------------------toString, equals e clone--------------------------------------------------------------------------\\


    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("Loja{");
        sb.append("storeCode='").append(storeCode).append('\'');
        sb.append(", storeName='").append(storeName).append('\'');
        sb.append(", gps=").append(gps);
        sb.append(", hasQueueInfo=").append(hasQueueInfo);
        sb.append(", queueTime=").append(queueTime);
        sb.append('}').append("\n");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Loja loja = (Loja) o;
        return hasQueueInfo == loja.hasQueueInfo &&
                Double.compare(loja.queueTime, queueTime) == 0 &&
                Objects.equals(storeCode, loja.storeCode) &&
                Objects.equals(storeName, loja.storeName) &&
                Objects.equals(gps, loja.gps);
    }

    public Loja clone() {
        return new Loja(this);
    }

    //---------------------------------------------------------------------------------outros metodos----------------------------------------------------------------------------\\

    /**
     * adiciona prod
     * @param prodCode prodCode
     */
    public void addProd(String prodCode) {
        this.prods.add(prodCode);
    }

    /**
     * adiciona lista de produtos
     * @param produtos list de produtos
     */
    public void addProdList(List<String> produtos) {
        for (String s: produtos)
            addProd(s);
    }

    /**
     * verifica se o produto existe
     * @param prodCode prodCode
     * @return         true se existe
     */
    public boolean containsProd(String prodCode) {
        return prods.contains(prodCode);
    }

    /**
     * limpa notificações
     */
    public void limpaNotificacoes() {
        notificacoes = new ArrayList<>();
    }
}
