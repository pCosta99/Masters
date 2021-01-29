package MVC.Models.Catalogs;

import MVC.Models.BaseModels.Encomenda;
import MVC.Models.BaseModels.GPS;
import MVC.Models.BaseModels.Transportadora;
import MVC.Models.BaseModels.TransportadoraMed;
import MVC.Comparators.DistanciaLojaComparator;

import java.io.Serializable;
import java.util.*;

/**
 * Write a description of class Transportadoras here.
 *
 * @author 89510-89561-89501
 * @version 01/05/2020
 */
public class Transportadoras implements Serializable {
    private Map<String, Transportadora> transportadoras;

    /**
     * Construtor de Transportadoras por defeito.
     */
    public Transportadoras(){
        this.transportadoras = new HashMap<>();
    }

    /**
     * Construtor de Transportadoras parametrizado.
     * @param tr Catálogo de Transportadoras.
     */
    public Transportadoras(Map<String, Transportadora> tr){
        setTransportadoras(tr);
    }

    /**
     * Construtor de Transportadoras por Cópia.
     * @param t Transportadoras a copiar.
     */
    public Transportadoras(Transportadoras t){
        this.setTransportadoras(t.getTransportadoras());
    }

    /**
     * Método que devolve o Catálogo de Transportadora.
     * @return Catálogo de Transportadora.
     */
    public Map<String, Transportadora> getTransportadoras() {
        Map<String, Transportadora> aux = new HashMap<>();
        for(Map.Entry<String, Transportadora> e : this.transportadoras.entrySet())
            aux.put(e.getKey(), e.getValue().clone());
        return aux;
    }

    /**
     * Método Auxiliar ao Método entregaEncomendaTransportadora.
     * Cria um Set de Transportadora, por ordem de menor distância de um dado GPS.
     * @param g GPS.
     * @return Set de Transportadora
     */
    private Set<Transportadora> getClosestAvailable(GPS g,boolean b){
        Set<Transportadora> r = new TreeSet<>(new DistanciaLojaComparator(g));
        if(b)
            this.transportadoras.values().stream()
                    .filter(t -> t instanceof TransportadoraMed && t.getEstaLivre() && t.getGPS().distancia(g) < t.getRaio()).forEach(r::add);
        else
            this.transportadoras.values().stream()
                .filter(t -> t.getEstaLivre() && t.getGPS().distancia(g) < t.getRaio()).forEach(r::add);
        return r;
    }

    /**
     * Método que verifica se existe uma Transportadora que possa realizar a Entrega de uma encomenda.
     * Caso exista, a Transportadora mais perto realiza a encomenda.
     * @param e Encomenda.
     * @param g Localização Encomenda.
     * @return True caso exista Transportadora que possa realizar, false caso contrário.
     */
    public Encomenda entregaEncomendaTransportadora(Encomenda e, GPS g, double duracaoLoja){
        TreeSet<Transportadora> aux = ((TreeSet<Transportadora>) this.getClosestAvailable(g,e.getMedica()));

        if (aux.size() == 0){
            return e;
        }
        Transportadora t = aux.first();
        //t.addEncomenda(e.getCodEnc());
        double km = t.getGPS().distancia(g);
        e.setDistancia(km);
        double time = (km/t.getVelocidadeMed())+duracaoLoja; // horas
        if(time>3)
            e.setPreco((km*t.getPrecoKm())*1.2);
        else
            e.setPreco(km*t.getPrecoKm());
        e.setDuracao(time);
        e.setCodEntregador(t.getCod());
        return e;
    }

    /**
     * Método que define se uma Transportadora está livre ou não.
     * @param s Código da Transportadora.
     * @param b True para livre, false para o caso contrário.
     */
    public void setEstaLivreTransportadora(String s, Boolean b){
        this.transportadoras.get(s).setEstaLivre(b);
    }

    /**
     * Método que define o Catálogo de Transportadoras.
     * @param cat Catálogo de Transportadoras.
     */
    public void setTransportadoras(Map<String, Transportadora> cat) {
        this.transportadoras = new HashMap<>();
        cat.entrySet().forEach(e -> this.transportadoras.put(e.getKey(), e.getValue().clone()));
    }

    /**
     * Método toString.
     * @return String com os dados das Transportadoras.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        Collection<Transportadora> values = this.transportadoras.values();
        sb.append("Transportadoras:\n").append(values);
        return sb.toString();
    }

    /**
     * Método que adiciona uma Transportadora ao Catálogo de Transportadora.
     * @param l Transportadora a iniciar.
     */
    public void addTransportadora(Transportadora l){
        this.transportadoras.put(l.getCod(), l.clone());
    }

    /**
     * Método que remove uma Transportadora do Catálogo de Transportadora.
     * @param code Código da Transportadora a remover.
     */
    public void removeTransportadora(String code){
        this.transportadoras.remove(code);
    }

    /**
     * Método que verifica se existe uma Transportadora no Catálogo de Transportadora.
     * @param code Código da Transportadora.
     * @return True caso exista, false caso contrário.
     */
    public boolean existeTransportadora(String code){
        return this.transportadoras.containsKey(code);
    }

    /**
     * Método que adiciona uma Encomenda a uma Transportadora.
     * @param codE Código da Encomenda.
     * @param codT Código da Transportadora.
     */
    public void addEncomendaTransportadora(String codE, String codT, double km){
        this.transportadoras.get(codT).addEncomenda(codE);
        this.transportadoras.get(codT).addKmsTotal(km);
    }

    /**
     * Método que devolve uma Transportadora a partir de um Código.
     * @param s Código da Transportadora.
     * @return Código resultante.
     */
    public Transportadora getTransportadora(String s){
        return this.transportadoras.get(s).clone();
    }

    /**
     * Método que decrementa a Capacidade de uma determinada Transportadora.
     * @param s Código da Transportadora.
     */
    public void decTransportadora(String s){
        this.transportadoras.get(s).decCapacidade();
    }

    /**
     * Método que incrementa a Capacidade de uma determinada Transportadora.
     * @param s Código da Transportadora.
     */
    public void incTransportadora(String s){
        this.transportadoras.get(s).incCapacidade();
    }

    /**
     * Método que dá uma Classificação a uma determinada Transportadora.
     * @param cod Código da Transportadora.
     * @param nota Classificação dada.
     */
    public void classificaTransportadora(String cod, int nota){
        this.transportadoras.get(cod).classificaTransportadora(nota);
    }

    /**
     * Método Clone.
     * @return Transportadoras Clonado.
     */
    public Transportadoras clone(){
        return new Transportadoras();
    }
}
