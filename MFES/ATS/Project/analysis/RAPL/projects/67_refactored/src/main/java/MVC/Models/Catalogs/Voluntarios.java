package MVC.Models.Catalogs;

import MVC.Models.BaseModels.Encomenda;
import MVC.Models.BaseModels.GPS;
import MVC.Models.BaseModels.Voluntario;
import MVC.Models.BaseModels.VoluntarioMed;
import MVC.Comparators.DistanciaLojaComparator;

import java.io.Serializable;
import java.util.*;

/**
 * Class para guardar todos os Voluntários.
 *
 *
 * @author 89510-89561-89501
 * @version 25/04/2020
 */

public class Voluntarios implements Serializable {

    private Map<String, Voluntario> dataVoluntarios;

    /**
     * Construtor de Voluntarios por defeito.
     */
    public Voluntarios(){
        this.dataVoluntarios = new HashMap<>();
    }

    /**
     * Construtor de Voluntarios de Cópia.
     * @param d Voluntarios a copiar.
     */
    public Voluntarios(Voluntarios d){
        setDataVoluntarios(d.getDataVoluntarios());
    }

    /**
     * Método que define o Catálogo de Voluntario.
     * @param data Catálogo de Voluntario.
     */
    public void setDataVoluntarios(Map<String, Voluntario> data){
        this.dataVoluntarios = new HashMap<>();
        data.forEach((key, value) -> this.dataVoluntarios.put(key, value.clone()));
    }

    /**
     * Método que retorna o Catálogo de Voluntario.
     * @return Cópia de Catálogo de Voluntario.
     */
    public Map<String, Voluntario> getDataVoluntarios(){
        Map<String, Voluntario> r = new HashMap<>();
        for (Map.Entry<String,  Voluntario> e : this.dataVoluntarios.entrySet())
            r.put(e.getKey(), e.getValue().clone());

        return r;
    }

    /**
     * Método Auxiliar ao método entregaEncomendaVoluntario.
     * Cria um Set de Voluntario, por ordem de menor distância de um dado GPS.
     * @param g GPS.
     * @return Set de Voluntario resultante.
     */
    private Set<Voluntario> getClosestAvailable(GPS g, boolean b){
        Set<Voluntario> r = new TreeSet<>(new DistanciaLojaComparator(g));
        if(b)
            this.dataVoluntarios.values().stream()
                    .filter(v -> v instanceof VoluntarioMed && v.getEstaLivre() && v.getGPS().distancia(g) < v.getRaio()).forEach(r::add);
        else
            this.dataVoluntarios.values().stream()
                .filter(v -> v.getEstaLivre() && v.getGPS().distancia(g) < v.getRaio()).forEach(r::add);
        return r;
    }

    /**
     * Método que verifica se existe um Voluntario que possa realizar a Entrega de uma encomenda.
     * Caso exista, o Voluntario mais perto realiza a encomenda.
     * @param e Encomenda.
     * @param g Localização encomenda.
     * @return True caso exista Voluntario que possa realizar, false caso contrário.
     */
    public Encomenda entregaEncomendaVoluntario(Encomenda e, GPS g, double duracaoLoja){
        TreeSet<Voluntario> aux = ((TreeSet<Voluntario>) this.getClosestAvailable(g,e.getMedica()));

        if (aux.size() == 0){
            return e;
        }
        Voluntario v = aux.first();
        double km = v.getGPS().distancia(g);
        e.setDistancia(km);
        double time = (km/v.getVelocidadeMed())+duracaoLoja;
        e.setDuracao(time);
        e.setCodEntregador(v.getCod());
        return e;
    }

    /**
     * Método que adiciona um Voluntario ao Catálogo de Voluntario.
     * @param v Voluntario a adicioanr.
     */
    public void addVoluntario(Voluntario v){
        this.dataVoluntarios.put(v.getCod(), v.clone());
    }

    /**
     * Método que verifica se existe um determinado Voluntario no Catálogo de Voluntario.
     * @param cod Código do Voluntario.
     * @return True caso exista, false caso contrário.
     */
    public boolean existeVoluntario(String cod){
        return this.dataVoluntarios.containsKey(cod);
    }

    /**
     * Método que adiciona uma Encomenda a um Voluntario.
     * @param codE Código da Encomenda.
     * @param codV Código do Voluntario.
     */
    public void addEncomendaVoluntario(String codE, String codV){
        this.dataVoluntarios.get(codV).addEncomenda(codE);
    }

    /**
     * Método que retorna uma Cópia de um determinado Voluntario.
     * @param s Código do Voluntario.
     * @return Cópia do Voluntario resultante.
     */
    public Voluntario getVoluntario(String s) {
        return this.dataVoluntarios.get(s).clone();
    }

    /**
     * Método que classifica um determinado Voluntario
     * @param s Código do Voluntario.
     * @param nota Classificação.
     */
    public void classificaVoluntario(String s, int nota){
        this.dataVoluntarios.get(s).classificaVoluntario(nota);
    }

    /**
     * Método toString.
     * @return String que contém os dados do Voluntarios.
     */
    public String toString(){
        StringBuilder sb = new StringBuilder();
        Collection<Voluntario> values = this.dataVoluntarios.values();
        sb.append("Voluntários: \n").append(values);

        return sb.toString();
    }

    /**
     * Método Clone.
     * @return Voluntarios Clonado.
     */
    public Voluntarios clone(){
        return new Voluntarios(this);
    }

    /**
     * Método que define se um Voluntario está livre ou não.
     * @param s Código da Transportadora.
     * @param b True para livre, false para o caso contrário.
     */
    public void setEstaLivreVoluntario(String s, Boolean b){
        this.dataVoluntarios.get(s).setEstaLivre(b);
    }

}