/**
 * classe que representa o modelo de projeto
 */
package Model;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class GestTrazAqui implements IGestTrazAqui, Serializable {

    private Map<String, Utilizador> users;
    private Map<String, Loja> lojas;
    private Map<String, Estafeta> estafetas;
    private Map<String, Encomenda> encomendas;
    private Map<String, Produto> produtos;
    private Map<String, Login> loginMap;
    private int[] randomTraffic;
    private int[] randomWeather;
    private int[] randomQueue;

    public GestTrazAqui() {
        this.users = new HashMap<>();
        this.lojas = new HashMap<>();
        this.estafetas = new HashMap<>();
        this.encomendas = new HashMap<>();
        this.produtos = new HashMap<>();
        this.loginMap = new HashMap<>();
        this.randomTraffic = new int[]{1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 5, 6};
        this.randomWeather = new int[]{1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4};
        this.randomQueue = new int[]{0, 0, 1, 1, 2, 2, 3, 4, 5, 7, 8, 10};
    }

    //--------------------------------------------------------------Métodos Utilizador--------------------------------------------------------------------------\\

    /**
     * Devolve o utilizador com um determinado código
     * @param userCode Código de utilizador
     * @return Utilizador
     */
    public Utilizador getUser(String userCode) {
        return users.get(userCode).clone();
    }

    /**
     * altera codigo de um utilizador
     * @param user userCode
     */
    public void setUser(Utilizador user) {
        users.replace(user.getCodigoUtilizador(), user);
    }

    /**
     * adiciona um utilizador
     * @param user utilizador
     */
    public void addUser(Utilizador user) {
        users.put(user.getCodigoUtilizador(), user);
    }

    /**
     * devolve uma notificação
     * @param code userCode
     * @return  Lista de notificacoes
     */
    public List<Notificacao> getUserNotificacoes(String code) {
        return users.get(code).getNotificacoes();
    }

    /**
     * devolve numero de notificações de um utilizador
     * @param code userCode
     * @return Numero de notificações
     */
    public int getUserNumNotificacoes(String code) {
        return users.get(code).getNumNotificacoes();
    }

    /**
     * adiciona uma notificação a um utilizador
     * @param code      userCode
     * @param not       conteúdo notificação
     * @param type      tipo de notificação
     * @param estCode   estafeta code
     */
    public void addUserNotificacao(String code, String not, int type, String estCode) {
        users.get(code).addNotificacao(not, type, estCode);
    }

    /**
     * verifica se a encomenda esta em stand-By
     * @param enc   encCode
     * @return      true se a encomenda estiver enm standBy
     */
    public boolean getUserEncStandBy(String enc){
        return users.get(encomendas.get(enc).getUserCode()).isEncStandBy(enc);
    }

    /**
     * devolve lista de encomendas em standBy em que o estafeta é uma transportadora
     * @param userCode  userCode
     * @return          list de codigos de encomendas
     */
    public Set<String> getUserStandByTransp(String userCode){
        Set<String> list = users.get(userCode).getStandBy().stream().collect(Collectors.toSet());
        if(list.size() == 0)
            return list;
        return list.stream().filter(enc -> estafetas.get(encomendas.get(enc).getTranspCode()).getType().equals("Transportadora"))
                            .collect(Collectors.toSet());
    }

    /**
     * limpa as notificações de um utilizador
     * @param code  userCode
     */
    public void limpaUserNotificacoes(String code) {
        users.get(code).limpaNotificacoes();
    }


    /**
     * devolve lista de encomendas de um user que ocorreram entre a data min e max
     * @param code userCode
     * @param type 1 voluntario, 2 transportadora, 3 total
     * @param min  data min
     * @param max  data max
     * @return     lista de encomendas
     */
    public List<Encomenda> getUserEncbyData(String code,int type, LocalDateTime min,LocalDateTime max) {
        List<Encomenda> list = new ArrayList<>();

        for (String c : users.get(code).getEncomendas()) {
            list.add(encomendas.get(c).clone());
        }
        if(type == 1)
            list = list.stream().filter(e -> e.isEntregue() && e.isVoluntario() && e.encData(min,max)).collect(Collectors.toList());
        else if(type == 2)
            list = list.stream().filter(e -> e.isEntregue() && e.isTransportadora() && e.encData(min,max)).collect(Collectors.toList());
        else
            list = list.stream().filter(e -> e.isEntregue() && e.encData(min,max)).collect(Collectors.toList());

        return list;
    }

    /**
     * Devolve status das encomendas de um utilizador
     * @param userCode userCode
     * @return         lista strings que representa encCode e status
     */
    public List<String> getUserEncStatus(String userCode){
        List<String> list = new ArrayList<>();
        String status;

        for (String encCode : users.get(userCode).getEncomendas()) {
            if(encomendas.get(encCode).isEntregue())
                status = "entregue";
            else if(encomendas.get(encCode).isStandBy())
                status = "Stand-By";
            else if(!encomendas.get(encCode).isAceiteLoja())
                status = "não aceite na loja";
            else
                status = "por entregar";

            list.add(String.format("%6s %2s %15s", encCode, ":",status));
        }
        return list;
    }

    /**
     * devolve list de codigos de encomenda de um user prontas para ser entregues
     * @param userCode  userCode
     * @return          list de codigos de encomenda
     */
    public List<String> getEncReady(String userCode){
        return users.get(userCode).getEncomendas().stream().filter(c -> encomendas.get(c).isAceiteLoja() && !encomendas.get(c).isEntregue()).collect(Collectors.toList());
    }

    /**
     * devolve os codigos dos 10 utilizadores que receberam mais encomendas
     * @return  lista de codigos de utilizador
     */
    public List<String> getTopUsers() {
        return users.values().stream().sorted().limit(10).map(c -> String.format("%3s %2s %45s %2s %3d",c.getCodigoUtilizador(), "|", c.getName(), "|", c.getEntregasSize())).collect(Collectors.toList());
    }

    /**
     * adiciona um encCode ao standBy de um utilizador e remove-a das encomendas
     * @param userCode  userCode
     * @param encCode   encCode
     */
    public void addUserStandBy(String userCode, String encCode) {
        users.get(userCode).addStandBy(encCode);
        users.get(userCode).removeEncomenda(encCode);
        encomendas.get(encCode).setStandBy(true);
    }

    /**
     * remove uma encomenda do standBy e adiciona-a às entregues de um utilizador
     * @param userCode userCode
     * @param encCode  encCode
     */
    public void removeUserStandBy(String userCode, String encCode) {
        users.get(userCode).removeStandBy(encCode);
        users.get(userCode).addEncomenda(encCode);
    }

    /**
     * Método que retira uma encomenda de StandBy
     *
     * @param encCode   Código da encomenda que vai ser alterada
     */
    public void removeEncStandBy(String encCode) {
        encomendas.get(encCode).setStandBy(false);
    }

    /**
     * Método que verifica se uma encomenda está em StandBy
     *
     * @param encCode   Código da encomenda que vai ser consultada
     * @return          Verdadeiro se a encomenda estiver em StandBy, falso caso contrário
     */
    public boolean isEncStandBy(String encCode) {
        return encomendas.get(encCode).isStandBy();
    }

    //---------------------------------------------------------------Métodos Estafeta--------------------------------------------------------------------------\\

    /**
     * devolve um estafeta
     * @param code  estafeta code
     * @return      copia do estafeta
     */
    public Estafeta getEstafeta(String code) {
        return estafetas.get(code).clone();
    }

    /**
     * devolve numero de encomendas maximo que um estafeta pode transportar
     * @param transpCode    transpCode
     * @return              numero de encomendas
     */
    public int getEstafetaNumEnc(String transpCode) {
        return ((Transportadora)estafetas.get(transpCode)).getNumEncomendas();
    }

    /**
     * altera o estafeta
     * @param estafeta estasfeta
     */
    public void setEstafeta(Estafeta estafeta) {
        estafetas.replace(estafeta.getCode(), estafeta);
    }

    /**
     * adiciona entrada no map de estafetas
     * @param estafeta estafeta
     */
    public void addEstafeta(Estafeta estafeta) {
        estafetas.put(estafeta.getCode(), estafeta);
    }

    /**
     * adiciona encCode a um estafeta
     * @param code      estafeta code
     * @param encCode   encCode
     */
    public void addEncomendaEstafeta(String code, String encCode) {
        estafetas.get(code).addEncomenda(encCode);
    }

    /**
     * adiciona uma encCode a uma rota de entregas de uma transportadora
     * @param transpCode    transpCode
     * @param rota          encCode de rota
     */
    public void addEstafetaRota(String transpCode,String rota){
        ((Transportadora)estafetas.get(transpCode)).addRota(rota);
    }

    /**
     * devolve a rota de entregas de uma transportadora
     * @param transpCode transpCode
     * @return           list de codigos de encomendas
     */
    public Set<String> getEstafetaRota(String transpCode){
        return ((Transportadora)estafetas.get(transpCode)).getRota();
    }

    /**
     * devolve tamanho da rota de uma transportadora
     * @param transpCode    transpCode
     * @return              tamanho
     */
    public int getEstafetaRotaSize(String transpCode){
        return ((Transportadora)estafetas.get(transpCode)).getRotaSize();
    }

    /**
     * devolve tipo de estafeta
     * @param estCode   estCode
     * @return          tipo de estafeta
     */
    public String getEstafetaType(String estCode){
        return estafetas.get(estCode).getType();
    }

    /**
     * verifica se a rota do estafeta tem a encCode dada
     * @param estCode   estCode
     * @param encCode   encCode
     * @return          true se o estafeta contem a encCode na rota
     */
    public boolean containsEncRota(String estCode,String encCode){
        return ((Transportadora)estafetas.get(estCode)).containsRota(encCode);
    }

    /**
     * devolve o nome do estafeta
     * @param estCode   estafeta code
     * @return          nome
     */
    public String getEstafetaName(String estCode){
        return estafetas.get(estCode).getName();
    }

    /**
     * devolve coordenada do estafeta
     * @param code  estafeta code
     * @return      coordenada
     */
    public Coordenadas getEstafetaCoord(String code) {
        return estafetas.get(code).getGps();
    }

    /**
     * devolve classificação do estafeta
     * @param code  estafeta code
     * @return      classificação
     */
    public double getEstafetaClassificacao(String code) {
        return estafetas.get(code).getClassificacao();
    }

    /**
     * devolve notificações
     * @param code  estafeta code
     * @return      list de notificações
     */
    public List<Notificacao> getEstafetaNotificacoes(String code) {
        return estafetas.get(code).getNotificacoes();
    }

    /**
     * devolve numero de notificações
     * @param code  estafeta code
     * @return      numero de notificações
     */
    public int getEstafetaNumNotificacoes(String code) {
        return estafetas.get(code).getNumNotificacoes();
    }

    /**
     * adiciona uma notificação a um estafeta
     * @param estCode   estafetaCode
     * @param not       conteúdo notificação
     * @param type      tipo
     * @param code      encCode
     */
    public void addEstafetaNotificacao(String estCode, String not, int type, String code) {
        estafetas.get(estCode).addNotificacao(not, type, code);
    }

    /**
     * remove um encCode da rota de uma transportadora
     * @param transpCode    transpCode
     * @param enc           encCode
     */
    public void removeEstafetaEncRota(String transpCode,String enc) {
        ((Transportadora)estafetas.get(transpCode)).remEncRota(enc);
    }

    /**
     * limpa as notificações de um estafeta
     * @param code  estafeta code
     */
    public void limpaEstafetaNotificacoes(String code) {
        estafetas.get(code).limpaNotificacoes();
    }

    /**
     * devolve os codigos dos estafetas que podem entregar a encomenda
     * @param enc encCode
     * @return    list de codigos de estafeta
     */
    public List<String> possiveisEstafetas(String enc) {
        List<String> estafetaList;
        Coordenadas cr = lojas.get(encomendas.get(enc).getStoreCode()).getGps();
        Coordenadas cr2 = users.get(encomendas.get(enc).getUserCode()).getGps();
        boolean isMedic = encomendas.get(enc).isMedic();

        estafetaList = estafetas.values().stream().filter(e -> ((!isMedic || e.isMedic()) && e.isFree() && !e.isOccup() && (e.getGps().distancia(cr) < e.getRaio())
                                                                && (e.getGps().distancia(cr2) < e.getRaio())))
                                                                    .map(Estafeta::getCode).collect(Collectors.toList());

        return estafetaList;
    }

    /**
     * devolve codigos de encomendas que um estafeta pode transportar
     * @param transpCode transpCode
     * @return           lista de codigos de encomenda
     */
    public List<String> encomendasPossiveis(String transpCode){
        List<String> encList;
        Coordenadas cr = estafetas.get(transpCode).getGps();
        boolean isMedic = estafetas.get(transpCode).isMedic();
        double raio = estafetas.get(transpCode).getRaio();

        encList = encomendas.values().stream().filter(e -> ((!isMedic || e.isMedic()) && e.isAceiteLoja() && !e.isEntregue() && !((Transportadora)estafetas.get(transpCode)).containsRota(e.getEncCode())
                                                            && lojas.get(e.getStoreCode()).getGps().distancia(cr) < raio && users.get(e.getUserCode()).getGps().distancia(cr) < raio) && !e.isStandBy())
                                                            .map(Encomenda::getEncCode).collect(Collectors.toList());

        return encList;
    }

    /**
     * escolhe o melhor estafeta para entregar uma encomenda
     * @param listEst   list de codigos de estafeta
     * @param encCode   encCode
     * @return          codigo de estafeta
     */
    public String escolheEstafeta(List<String> listEst,String encCode) {
        String best = "";
        Coordenadas coord = getStoreCoordFromEnc(encCode);
        double distmin = Double.MAX_VALUE,curr;

        for(String code:listEst){
            if((curr = getEstafetaCoord(code).distancia(coord)) <= distmin) {
                best = code;
                distmin = curr;
            }
        }

        return best;
    }

    /**
     * classificar um estafeta
     * @param pontuacao pontuação
     * @param code      estafeta code
     */
    public void classificarEstafeta(double pontuacao,String code){
        estafetas.get(code).atualizaClassificacao(pontuacao);
    }

    /**
     * verifica se o estafeta esta livre
     * @param code  estafeta code
     * @return      true se o estafeta esta livre
     */
    public boolean isEstafetaFree(String code) {
        return estafetas.get(code).isFree();
    }

    /**
     * altera o estado de um estafeta
     * @param code  estafeta code
     * @param free  estado
     */
    public void setEstafetaFree(String code, boolean free) {
        estafetas.get(code).setFree(free);
    }

    /**
     * devolve codigos de transportadoras com mais km percorridos
     * @return list de codigos de estafeta
     */
    public List<String> getTopTrans() {
        return this.estafetas.values().stream().filter(c -> c.getType().equals("Transportadora")).sorted().limit(10).map(c -> String.format("%3s %2s %40s %2s %5.2f",c.getCode(), "|", c.getName(), "|", c.getNumKm())).collect(Collectors.toList());
    }

    /**
     * retorna encomenda de um estafeta num intervalo de tempo
     * @param code  estafeta code
     * @param min   data min
     * @param max   data max
     * @return      list de encomendas
     */
    public List<Encomenda> getEncomendasEstafeta(String code, LocalDateTime min, LocalDateTime max) {
        List<Encomenda> list = new ArrayList<>();

        for (String c : estafetas.get(code).getEncomendas()) {
            list.add(encomendas.get(c).clone());
        }

        return list.stream().filter(e -> e.encData(min,max)).collect(Collectors.toList());
    }

    /**
     * calcula a faturação de uma transportadora num dado intervalo de tempo
     * @param code  transpCode
     * @param min   data min
     * @param max   data max
     * @return      faturação
     */
    public double calcularFaturacao(String code, LocalDateTime min, LocalDateTime max) {
        return encomendas.values().stream().filter(e -> e.encData(min,max) && e.getTranspCode().equals(code))
                .map(e -> precoEncomenda(e.getEncCode(), code)).reduce(0d, Double::sum);
    }

    /**
     * verifica se o estafeta contem a encomenda dada
     * @param encCode   encCode
     * @param code      estafetaCode
     * @return          true se o estafeta contem o encCode
     */
    public boolean containsEncomendaEstafeta(String encCode, String code) {
        return estafetas.get(code).containsEncomenda(encCode);
    }

    /**
     * devolve encomenda por entregar em standBy
     * @param estCode   estafetaCode
     * @return          encCode
     */
    public String encomendaStandBy(String estCode){
        return estafetas.get(estCode).getEncomendas().stream().filter(enc -> !encomendas.get(enc).isEntregue()).findFirst().orElse("");
    }

    /**
     * remove encomenda da lista de encomendas de um estafeta
     * @param code      estafetaCode
     * @param encCode   encCode
     */
    public void removerEnc(String code,String encCode){
        estafetas.get(code).removeEnc(encCode);
        encomendas.get(encCode).setStandBy(false);
    }

    /**
     * altera estado do estafeta
     * @param code  estafeta code
     * @param occup estado
     */
    public void setEstafetaOccup(String code,boolean occup){
        estafetas.get(code).setOccup(occup);
    }

    /**
     * Método que verifica se um estafeta é do tipo médico
     *
     * @param estafetaCode  Código do estafeta
     * @return              Verdadeiro se for medic, falso caso contrário
     */
    public boolean isEstafetaMedic (String estafetaCode) {
        return estafetas.get(estafetaCode).isMedic();
    }

    /**
     * Método que altera a disponibilidade para entregar encomendas médicas
     *
     * @param estafetaCode      Código do estafeta
     */
    public void changeMedic(String estafetaCode) {
        estafetas.get(estafetaCode).setMedic(!isEstafetaMedic(estafetaCode));
    }

    //-----------------------------------------------------------------Métodos Lojas--------------------------------------------------------------------------\\

    /**
     * devolve list de codigos de lojas
     * @return  list de codigos de lojas
     */
    public List<String> getLojas() {
        List<String> lojas = new ArrayList<>();

        for (Loja l: this.lojas.values())
            lojas.add(l.getStoreCode() + ": " + l.getStoreName());

        return lojas;
    }

    /**
     * devolve um loja
     * @param storeCode store code
     * @return          loja
     */
    public Loja getLoja(String storeCode) {
        return lojas.get(storeCode).clone();
    }

    /**
     * altera loja
     * @param loja  loja
     */
    public void setLoja(Loja loja) {
        lojas.replace(loja.getStoreCode(), loja);
    }

    /**
     * adiciona mapEntry nas lojas
     * @param loja loja
     */
    public void addLoja(Loja loja) {
        lojas.put(loja.getStoreCode(), loja);
    }

    /**
     * adiciona encCode a uma loja
     * @param enc   encCode
     */
    public void addEncLoja(String enc) {
        lojas.get(encomendas.get(enc).getStoreCode()).addEncomenda(enc);
    }

    /**
     * devolve codigos de encomenda da loja
     * @param storeCode storeCode
     * @return          list de codigos de encomenda
     */
    public Set<String> getEncLoja(String storeCode){
        return lojas.get(storeCode).getEncomendas();
    }

    /**
     * adiciona produto a uma loja
     * @param storeCode storeCode
     * @param produtos  list de codigos de produtos
     */
    public void addProdLoja(String storeCode, List<String> produtos) {
        lojas.get(storeCode).addProdList(produtos);
    }

    /**
     * devolve coordenadas de uma loja
     * @param storeCode store code
     * @return          coordenada
     */
    private Coordenadas getStoreCoord(String storeCode) {
        return lojas.get(storeCode).getGps();
    }

    /**
     * devolve coordenadas de uma loja a partir de uma encomenda
     * @param encCode encCode
     * @return        coordenada
     */
    public Coordenadas getStoreCoordFromEnc(String encCode) {
        return getStoreCoord(encomendas.get(encCode).getStoreCode());
    }

    /**
     * devolve produtos de uma loja
     * @param storeCode store code
     * @return          list de codigos de produtos
     */
    public List<String> getProdutosLoja(String storeCode) {
        List<String> produtos = new ArrayList<>();

        for (String prodCode: getLoja(storeCode).getProds())
            produtos.add(prodCode + ": " + getProdName(prodCode) + " " + getProdWeight(prodCode) + "Kg " + getProdPrice(prodCode) + "€ ");

        return produtos;
    }

    /**
     * verifica se a loja existe
     * @param storeCode store code
     * @return          true se loja existes
     */
    public boolean containsLoja(String storeCode) {
        return lojas.containsKey(storeCode);
    }

    /**
     * verifica se o produto exisrte na loja
     * @param storeCode storecode
     * @param prodCode  prodCode
     * @return          true se o produto existe
     */
    public boolean containsProdutoLoja(String storeCode, String prodCode) {
        return lojas.get(storeCode).containsProd(prodCode);
    }

    /**
     * verifica se a loja tem informação sobre a lista de espera
     * @param storeCode store code
     * @return          true se a loja tem info
     */
    public boolean hasQueueInfoLoja(String storeCode) {
        return lojas.get(storeCode).isHasQueueInfo();
    }

    /**
     * altera tempo da fila de espera
     * @param storeCode store code
     * @param time      tempo
     */
    public void setStoreQueueTime(String storeCode, double time) {
        lojas.get(storeCode).setQueueTime(time);
    }

    /**
     * altera o número de pessoas em fila de espera
     * @param storeCode store code
     * @param time      tempo
     */
    public void setStoreQueueSize(String storeCode, int time) {
        lojas.get(storeCode).setQueueSize(time);
    }

    /**
     * devolve notificações de uma loja
     * @param code  store code
     * @return      list de notificações
     */
    public List<Notificacao> getLojaNotificacoes(String code) {
        return lojas.get(code).getNotificacoes();
    }

    /**
     * devolve numero de notificações de uma loja
     * @param code  store code
     * @return      numero
     */
    public int getLojaNumNotificacoes(String code) {
        return lojas.get(code).getNumNotificacoes();
    }

    /**
     * adiciona notificação a uma loja
     * @param code      store code
     * @param not       conteúdo da notificação
     * @param type      tipo
     * @param estCode   estafeta code
     */
    public void addLojaNotificacao(String code, String not, int type, String estCode) {
        lojas.get(code).addNotificacao(not, type, estCode);
    }

    /**
     * limpa a lista de notificações de uma loja
     * @param code  store code
     */
    public void limpaLojaNotificacoes(String code) {
        lojas.get(code).limpaNotificacoes();
    }

    //--------------------------------------------------------------Métodos Encomenda--------------------------------------------------------------------------\\

    /**
     * devolve encomenda
     * @param encCode   encCode
     * @return          encomenda
     */
    public Encomenda getEncomenda(String encCode) {
        return encomendas.get(encCode).clone();
    }

    /**
     * altera encomenda
     * @param enc   encCode
     */
    public void setEncomenda(Encomenda enc) {
        encomendas.replace(enc.getEncCode(), enc);
    }

    /**
     * adiciona encomenda
     * @param encomenda encomenda
     */
    public void addEncomenda(Encomenda encomenda) {
        users.get(encomenda.getUserCode()).addEncomenda(encomenda.getEncCode());
        encomendas.put(encomenda.getEncCode(), encomenda);
    }

    /**
     * verifica se a encomenda existe
     * @param encCode   enc code
     * @return          true se existe
     */
    public boolean containsEncomenda(String encCode) {
        return encomendas.containsKey(encCode);
    }

    /**
     * aceitar encomenda por uma loja
     * @param encCode   encCode
     */
    public void aceitarEncomenda(String encCode) {
        encomendas.get(encCode).setAceiteLoja(true);
        addEncLoja(encCode);
    }

    /**
     * entrega encomenda
     * @param encCode       encCode
     * @param estafetaCode  estafetaCode
     */
    public void entregarEncomenda(String encCode,String estafetaCode) {
        Encomenda enc = encomendas.get(encCode);
        Estafeta e = estafetas.get(estafetaCode);
        Loja l = lojas.get(enc.getStoreCode());
        Coordenadas cr = lojas.get(enc.getStoreCode()).getGps();

        enc.setTranspCode(estafetaCode);
        enc.setEntregue(true);
        enc.setPreco(precoEncomenda(encCode,estafetaCode));
        enc.setTempoEntrega(calculaTempo(e.getGps(),l.getGps(),users.get(enc.getUserCode()).getGps(),l.getQueueTime(), l.getQueueSize(),e.getVelocidade()));
        e.setEnc(encCode);
        e.addNumKm(e.getGps().distancia(cr) + l.getGps().distancia(users.get(enc.getUserCode()).getGps()));
        enc.setStandBy(false);
    }

    /**
     * altera o transpCode de uma encomenda
     * @param enc           encCode
     * @param transpCode    transpCode
     */
    public void sugerirTransp(String enc,String transpCode){
        encomendas.get(enc).setTranspCode(transpCode);
    }

    /**
     * devolve lista de encomendas aceite
     * @return list de codigos de encomenda
     */
    public Set<String> encomendasAceites() {
        Set<String> res = new HashSet<>();

        encomendas.values().stream().map(Encomenda::clone).filter(Encomenda::isAceiteLoja).map(Encomenda::getEncCode).forEach(res::add);
        return res;
    }

    /**
     * verifica se a encomenda foi aceite
     * @param encCode   encCode
     * @return          true se foi aceite
     */
    public boolean isEncomendaAceite(String encCode) {
        return encomendasAceites().contains(encCode);
    }

    /**
     * calcula preço de encomenda
     * @param encCode       encCode
     * @param transpCode    transpCode
     * @return              preço
     */
    public double precoEncomenda(String encCode,String transpCode) {
        if(estafetas.get(transpCode).getType().equals("Voluntario"))
            return 0;

        Encomenda enc = getEncomenda(encCode);
        double dist = estafetas.get(transpCode).getGps().distancia(lojas.get(enc.getStoreCode()).getGps())
                + lojas.get(enc.getStoreCode()).getGps().distancia(users.get(enc.getUserCode()).getGps());
        return  ((Transportadora)estafetas.get(transpCode)).getTaxaKm() * dist + enc.getWeight() * ((Transportadora)estafetas.get(transpCode)).getTaxaPeso();
    }

    /**
     * devolve user que fez a encomenda
     * @param encCode   encCode
     * @return          userCode
     */
    public String getEncUser(String encCode){
        return encomendas.get(encCode).getUserCode();
    }

    /**
     * devolve transp que entregou uma encomenda
     * @param encCode   encCode
     * @return          transpCode
     */
    public String getEncTransp(String encCode){
        return encomendas.get(encCode).getTranspCode();
    }

    /**
     * devolve tempo de entrega da encomenda
     * @param encCode   encCode
     * @return          tempo
     */
    public double getEncTime(String encCode){
        return encomendas.get(encCode).getTempoEntrega();
    }

    /**
     * devolve o nome da loja da encomenda
     * @param encCode   encCode
     * @return          Loja
     */
    public String getEncStoreName(String encCode){
        return lojas.get(encomendas.get(encCode).getStoreCode()).getStoreName();
    }

    /**
     * devolve o preço de entrega da encomenda
     * @param encCode   encCode
     * @return          tempo
     */
    public double getEncPrice(String encCode){
        return encomendas.get(encCode).getPreco();
    }

    /**
     * devolve nome do utilizador que fez a encomenda
     * @param encCode   encCode
     * @return          username
     */
    public String getEncUserName(String encCode){
        return users.get(encomendas.get(encCode).getUserCode()).getName();
    }

    /**
     * devolve as encomendas não aceite numa loja
     * @param storeCode storeCode
     * @return          list de codigos de encomenda
     */
    public List<String> encomendasNaoAceitesLoja(String storeCode) {
        return encomendas.values().stream().filter(c -> c.getStoreCode().equals(storeCode)).filter(c-> !c.isAceiteLoja()).map(Encomenda::getEncCode).collect(Collectors.toList());
    }

    /**
     * remove uma encomenda
     * @param encCode   encCode
     */
    public void removeEncomenda(String encCode) {
        String userCode = getEncUser(encCode);
        users.get(userCode).removeEncomenda(encCode);
        encomendas.remove(encCode);
    }

    /**
     * devolve uma lista de produtos
     * @return   list de codigos de produtos
     */
    public List<String> randomListaProdutos() {
        List<String> prods = produtos.values().stream().map(Produto::getProdCode).collect(Collectors.toList());

        Random rand = new Random();
        int size = prods.size();
        int totalItems = rand.nextInt((2*size)/3 - size/2) + size/2;
        int randomIndex;

        while(prods.size() != totalItems) {
            randomIndex = rand.nextInt(prods.size());
            prods.remove(randomIndex);
        }

        return prods;
    }

    /**
     * Método que devolve a informação da encomenda
     * @param encomendas    Lista de códigos das encomendas
     * @return              Lista com a informação das encomendas
     */
    public List<String> getEncInfo(List<String> encomendas) {
       return encomendas.stream().map(e -> e + " -> " + getEncStoreName(e) + " " + String.format("%.2f", getEncPrice(e)) + "€").collect(Collectors.toList());
    }

    //----------------------------------------------------------------Métodos Produto--------------------------------------------------------------------------\\

    /**
     * adiciona produto
     * @param prod produto
     */
    public void addProduto(Produto prod) {
        produtos.put(prod.getProdCode(), prod);
    }

    /**
     * devolve nome do produto
     * @param prodCode  prodCode
     * @return          nome
     */
    public String getProdName(String prodCode) {
        return produtos.get(prodCode).getName();
    }

    /**
     * devolve peso do produto
     * @param prodCode  prodCode
     * @return          nome
     */
    public double getProdWeight(String prodCode) {
        return produtos.get(prodCode).getWeight();
    }

    /**
     * devolve preço do produto
     * @param prodCode  prodCode
     * @return          nome
     */
    public double getProdPrice(String prodCode) {
        return produtos.get(prodCode).getPrice();
    }

    /**
     * verifica se o produto é médico
     * @param prodCode  prodCode
     * @return          nome
     */
    public boolean getProdisMedic(String prodCode) {
        return produtos.get(prodCode).isMedic();
    }


    //----------------------------------------------------------------Métodos Login--------------------------------------------------------------------------\\

    /**
     * devolve login
     * @param code loginCode
     * @return     login
     */
    public Login getLogin(String code) {
        return loginMap.get(code).clone();
    }

    /**
     * altera login
     * @param login login
     */
    public void setLogin(Login login) {
        loginMap.replace(login.getCode(), login);
    }

    /**
     * adiciona login
     * @param login login
     */
    public void addLogin(Login login) { loginMap.put(login.getCode(), login);}

    /**
     * verifica se o loginUser existe
     * @param code  loginCode
     * @return      true se existe
     */
    public boolean containsUser(String code) {
        return loginMap.containsKey(code);
    }

    /**
     * verifica se o login existe
     * @param code      loginCode
     * @param password  password
     * @return          true se existe
     */
    public boolean containsPassword(String code, String password) {
        return loginMap.get(code).getPassword().equals(password);
    }

    /**
     * verifica se o login existe
     * @param  name  login name
     * @param  type  tipo de conta
     * @return      true se existe
     */
    public boolean containsNameAndType(String name, String type) {
        return loginMap.values().stream().filter(c -> c.getNome().equals(name)).anyMatch(c -> c.getTipoConta().equals(type));
    }

    //----------------------------------------------------------------Outros Métodos--------------------------------------------------------------------------\\

    /**
     * calcula tempo de entrega de uma encomenda
     * @param crE               coordenada estafeta
     * @param crL               coordenada loja
     * @param crU               coordenada utilizador
     * @param tempoFilaEspera   tempo em fila de espera da loja
     * @param queueSize         tamanho da fila de espera
     * @param velocidade        velocidade da transportadora
     * @return                  tempo
     */
    public double calculaTempo(Coordenadas crE,Coordenadas crL,Coordenadas crU,double tempoFilaEspera, int queueSize,double velocidade) {
        Random rand = new Random();
        int condicoesAtmosfericas = randomWeather[rand.nextInt(15)];
        int transito = randomTraffic[rand.nextInt(15) ];
        int numPessoasFila = randomQueue[rand.nextInt(12)];
        double tempo;
        if(tempoFilaEspera == -1) {
            tempo = rand.nextDouble() * 10 * numPessoasFila;
        }

        else {
            tempo = tempoFilaEspera * queueSize;
        }
        double dist = crE.distancia(crL) + crL.distancia(crU);
        tempo += (dist / velocidade) * 60;
        if(condicoesAtmosfericas == 2)
            tempo *= 1.2;
        else if(condicoesAtmosfericas == 3)
            tempo *= 1.5;
        else if(condicoesAtmosfericas == 4)
            tempo *= 2;

        if(transito == 2)
            tempo *= 1.2;
        else if(transito == 3)
            tempo *= 1.5;
        else if(transito == 4)
            tempo *= 2;
        else if(transito == 5)
            tempo *= 2.5;
        else if(transito == 6)
            tempo *= 3;

        return tempo;
    }

    /**
     * gera um loginCode
     * @param tipoConta tipo de conta
     * @return          codigo de login
     */
    public String generateCode(String tipoConta) {
        StringBuilder sb = new StringBuilder();
        Random rand = new Random();
        char c = ' ';
        int randInt;

        c = tipoConta.toLowerCase().charAt(0);

        if (c != 'e')
            randInt = rand.nextInt(100);

        else
            randInt = rand.nextInt(10000);

        sb.append(c).append(randInt);

        return sb.toString();
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder();
        sb.append("Controlador{").append("users=").append(users).append("\n");
        sb.append(", lojas=").append(lojas).append("\n");
        sb.append(", voluntarios=").append(estafetas).append("\n");
        sb.append(", encomendas=").append(encomendas).append("\n");
        sb.append('}');
        return sb.toString();
    }
}
