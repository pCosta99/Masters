package Model;
/**
 * Write a description of class Model.GestaoEncomenda here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

import java.io.Serializable;
import java.net.PortUnreachableException;
import java.util.*;
import java.util.stream.Collectors;
import java.time.LocalDate;

public class GestaoEncomenda implements Serializable {
    
    private Map<String, Encomenda> gestor;

    /* Class Constructors*/
    public GestaoEncomenda() {
        this.gestor = new HashMap<>();
    }
    public GestaoEncomenda(Map<String, Encomenda> g){
        setGestor(g);
    }
    public GestaoEncomenda(GestaoEncomenda g){
        setGestor(g.getGestor());
    }

    /* Getters and Setters*/
    public void setGestor(Map <String, Encomenda> g){
        this.gestor = new HashMap<>();
        g.entrySet().forEach(e -> this.gestor.put(e.getKey(), e.getValue().clone()));
    }
    public Map<String, Encomenda> getGestor(){
        return this.gestor.entrySet().stream().
                collect(Collectors.toMap(e -> e.getKey(), e -> e.getValue().clone()));
    }
    public void addEncomenda(Encomenda enc){
        this.gestor.put(enc.getCodigo(), enc.clone());
    }
    public void removeEncomenda(String codEnc){

        this.gestor.remove(codEnc);
    }
    public Encomenda getEncomenda(String codEnc){

        return this.gestor.get(codEnc).clone();
    }
    public boolean existsEncomenda(String codEnc){
        return gestor.containsKey(codEnc);
    }

    public Set<String> todosCodigosEnc(){
        return this.gestor.keySet();
    }

    public String encomendaComMaisProdutos(){
        TreeSet <Encomenda> res = new TreeSet <Encomenda> (new ComparatorQtd());

        for(Encomenda e : this.gestor.values()){
            res.add(e.clone());
        }
        return res.last().getCodigo();
    }

    public Set<String> encomendasComProduto(String codProd){
        return this.gestor.values().stream().map(Encomenda::clone).
               filter(l -> l.existeProdutoEncomenda(codProd)).
               map(a -> a.getCodigo()).collect(Collectors.toSet());
    }

    public Set<Encomenda> encomendasValorDecrescente(){
        TreeSet<Encomenda> t = new TreeSet<>(new ComparatorQtd());

        for(Encomenda e : this.gestor.values()){
            t.add(e.clone());
        }
        return t;
    }

    public Map<String,List<String>> encomendasDeProduto(){
        HashMap<String, List<String>> aux = new HashMap<>();
        for(Encomenda e : this.gestor.values()){
            aux.put(e.getCodigo(),
                    this.gestor.values().stream().map(Encomenda::clone).
                    filter(a -> a.existeProdutoEncomenda(e.getCodigo())).
                    map(Encomenda::getCodigo).collect(Collectors.toList()));
        }
        return aux;
    }

    public int size(){
        return gestor.size();
    }

    // ordena as encomendas por data de entrega, apos a dada fornecida
    public void dataSort() {
        List<Map.Entry<String, Encomenda>> list = new LinkedList<>(this.gestor.entrySet());
        list.sort(Comparator.comparing(o -> (o.getValue().getData())));
        for (Map.Entry<String, Encomenda> entry : list) {
            this.gestor.put(entry.getKey(), entry.getValue());
        }
    }
    public void filterData(LocalDate data){
        List<Map.Entry<String, Encomenda>> list = new LinkedList<>(this.gestor.entrySet());
        list.stream().filter(o -> o.getValue().getData().isAfter(data));
        for (Map.Entry<String, Encomenda> entry : list) {
            this.gestor.put(entry.getKey(), entry.getValue());
        }
    }



    @Override
    public GestaoEncomenda clone(){
        return (new GestaoEncomenda(this));
    }
    @Override
    public boolean equals(Object o){
        if (this == o) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        GestaoEncomenda g = (GestaoEncomenda) o;
        return (this.gestor.equals(g.getGestor()));
    }
    @Override
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomendas: \n").append(this.gestor);
        return sb.toString();
    }
}