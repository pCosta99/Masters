import com.sun.jdi.connect.Connector;
import com.sun.source.tree.UsesTree;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Classe do Sistema de Gestao de Encomendas
 */
public class SGE implements Serializable {
    /*Variaveis de instancia*/
    private Map<String, Utilizador> utilizadores;
    private Map<String, Voluntario> voluntarios;
    private Map<String, Transportadora> transportadoras;
    private Map<String, Loja> lojas;
    private Map<String, Encomenda> encomendas;

    /**
     * Construtor por omissao
     */
    public SGE() {
        utilizadores = new TreeMap<>();
        voluntarios = new TreeMap<>();
        transportadoras = new TreeMap<>();
        lojas = new TreeMap<>();
        encomendas = new TreeMap<>();
    }

    /*GETS*/
    public Map<String, Loja> getLojas() {
        return lojas;
    }

    public Loja getLoja(String codLoja){
        return lojas.get(codLoja);
    }

    public Map<String, Transportadora> getTransportadoras() {
        return transportadoras;
    }

    public Transportadora getTransportadora(String codTrans){
        return transportadoras.get(codTrans);
    }

    public Map<String, Utilizador> getUtilizadores() {
        return utilizadores;
    }

    public Utilizador getUtilizador(String codUtilizador){
        return utilizadores.get(codUtilizador);
    }

    public Map<String, Voluntario> getVoluntarios() {
        return voluntarios;
    }

    public Voluntario getVoluntario(String codVoluntario){
        return voluntarios.get(codVoluntario);
    }

    public Map<String, Encomenda> getEncomendas() {
        return encomendas;
    }

    public Encomenda getEncomenda(String codEnc){
        return encomendas.get(codEnc);
    }

    /*SETS*/
    public void setLojas(Map<String, Loja> lojas) {
        this.lojas = lojas;
    }

    public void setTransportadoras(Map<String, Transportadora> transportadoras) {
        this.transportadoras = transportadoras;
    }

    public void setUtilizadores(Map<String, Utilizador> utilizadores) {
        this.utilizadores = utilizadores;
    }

    public void setVoluntarios(Map<String, Voluntario> voluntarios) {
        this.voluntarios = voluntarios;
    }

    public void setEncomendas(Map<String, Encomenda> encomendas) {
        this.encomendas = encomendas;
    }

    /*ADDS*/
    public void addLoja(Loja l) {
        lojas.put(l.getCodLoja(), l.clone());
    }

    public void addTransportadora(Transportadora t) {
        transportadoras.put(t.getCodEmpresa(), t);
    }

    public void addUtilizador(Utilizador u) {
        utilizadores.put(u.getCodUtilizador(), u);
    }

    public void addVoluntario(Voluntario v) {
        voluntarios.put(v.getCodVoluntario(), v);
    }

    public void addEncomenda(Encomenda e){encomendas.put(e.getCodEncomenda(), e);}

    /**
     * Funçao que retorna o conjunto de lojas dentro do raio do voluntario
     */
    public List<Loja> nearbyLojas(Map<String, Loja> l, Voluntario v){
        List<Loja> ret = new ArrayList<>();

        for(Map.Entry<String, Loja> m : l.entrySet()){
            /*Validaçao do raio*/
            if(m.getValue().getPosicao().distancia(v.getPosicao()) < v.getRaio()
                    && m.getValue().getFilaDeRecolha().size() != 0){
                /*Adiçao*/
                ret.add(m.getValue());
            }
        }

        return ret;
    }

    /**
     * Funçao que retorna o conjunto de lojas dentro do raio da transportadora
     */
    public List<Loja> nearbyLojas(Map<String, Loja> l, Transportadora t){
        List<Loja> ret = new ArrayList<>();

        for(Map.Entry<String, Loja> m : l.entrySet()){
            /*Validaçao do raio*/
            if(m.getValue().getPosicao().distancia(t.getPosicao()) < t.getRaio()
                    && m.getValue().getFilaDeRecolha().size() != 0){
                /*Adiçao*/
                ret.add(m.getValue());
            }
        }

        return ret;
    }

    /**
     * Funçao que verifica se um codigo de encomenda existe numa lista de encomendas
     */
    public boolean containsE(List<Encomenda> l, String e){
        boolean flag = false;

        for(int i = 0; i < l.size(); i++){
            if(l.get(i).getCodEncomenda().equals(e)){
                flag = true;
                break;
            }
        }

        return flag;
    }

    /**
     * Funçao que verifica se uma loja esta contida numa lista de encomendas
     */
    public boolean listContainsLoja(List<Loja> l, String s){
        boolean flag = false;
        for(Loja loja : l){
            if(loja.getCodLoja().equals(s)){
                flag = true;
                break;
            }
        }
        return flag;
    }

    /**
     * Funçao que verifica se uma encomenda esta contida numa lista de encomendas
     */
    public boolean ListContainsEncomenda(List<Encomenda> encs, String s){
        boolean flag = false;
        for(Encomenda encomendas : encs){
            if(encomendas.getCodEncomenda().equals(s)){
                flag = true;
                break;
            }
        }
        return flag;
    }

    /**
     * Funçao que atualiza a encomenda no sistema, loja e user
     */
    public void atualizaEntregador(Encomenda e, String codEntregador){
        encomendas.get(e.getCodEncomenda()).setCodEntregador(codEntregador);

        lojas.get(e.getCodLoja()).getFilaDeRecolha().get(e.getCodEncomenda()).setCodEntregador(codEntregador);

        utilizadores.get(e.getCodUtilizador()).getEncomendas().get(e.getCodEncomenda()).setCodEntregador(codEntregador);
    }

    /**
     * Função que retorna o total facturado por uma empresa transportadora
     */
    public double totalFatTransportadora(String codEmpresa, LocalDateTime d1, LocalDateTime d2){
        double ret = 0;

        /*selecionar a transportadora*/
        Transportadora t = this.transportadoras.get(codEmpresa);

        /*percorrer as encomendas*/
        for(Encomenda e: t.getRegisto().values()){
            /*selecionar apenas as que estao no intervalo de tempo*/
            if(e.getDataChegada().isAfter(d1) && e.getDataChegada().isBefore(d2)) {
                ret += e.getPortes();
            }
        }

        return ret;
    }

    /**
     * Funçao que retorna uma lista de 10 utilizadores ordenados decrescentemente por número de encomendas transportadas
     */
    public List<Utilizador> topUtilizadores(){
        /*Lista de return*/
        List<Utilizador> ret = new ArrayList<>(10);

        /*adiconar as utilizadores à lista*/
        for (Utilizador u: this.utilizadores.values()){
            ret.add(u);
        }

        /*dar sort à lista*/
        Collections.sort(ret, new ComparatorUtilizador());

        /*return dos 10 primeiros*/
        return ret.stream().limit(10)
                .collect(Collectors.toList());
    }

    /**
     * Função que retorna uma lista d 10 transportadoreas ordenadas decrescentemente por número de Km percorridos
     * @return
     */
    public List<Transportadora> topTransportadoras(){
        /*Lista de return*/
        List<Transportadora> ret = new ArrayList<>(10);

        /*adiconar as transportadoras à lista*/
        for(Transportadora t: this.transportadoras.values()){
            ret.add(t);
        }
        /*dar sort à lista*/
        Collections.sort(ret, new ComparatorTransportadorakm());

        /*return dos 10 primeiros*/
        return ret.stream().limit(10)
                .collect(Collectors.toList());
    }

    public double calculaPortes(Encomenda e, Transportadora t){
        /*Loja da encomenda*/
        Loja l = getLoja(e.getCodLoja());

        /*User da encomenda*/
        Utilizador user = getUtilizador(e.getCodUtilizador());

        /*Distancia entre loja e utilizador*/
        double dist = l.getPosicao().distancia(user.getPosicao());

        return dist * t.getPrecoKm();
    }

    public void clienteAceitaTransportadora(SGE sge, String info){
        String[] campos = info.split(",");

        /*Transportadora em concreto*/
        Transportadora t = sge.getTransportadora(campos[0]);

        /*Encomenda em concreto*/
        Encomenda enc = sge.getEncomenda(campos[1]);

        /*Loja em concreto*/
        Loja l = sge.getLojas().get(enc.getCodLoja());

        /*Flag da encomenda set true*/
        enc.setAceite(true);

        /*Set da data de sairda da encomenda*/
        enc.setDataDeSaida(LocalDateTime.now());

        /*Associar a encomenda o codigo de transportador*/
        enc.setCodEntregador(t.getCodEmpresa());

        /*Associar a encomenda os portes*/
        enc.setPortes(Double.parseDouble(campos[2]));

        /*Encomenda adicionada aos registos*/
        t.addRegisto(enc);

        /*Encomenda adicionada a queue*/
        t.addQueue(enc);

        /*Retirar a encomenda da fila de recolha*/
        l.getFilaDeRecolha().remove(enc.getCodEncomenda());
    }

}
