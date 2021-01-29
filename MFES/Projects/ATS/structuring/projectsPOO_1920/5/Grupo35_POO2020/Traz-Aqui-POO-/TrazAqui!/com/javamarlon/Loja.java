package com.javamarlon;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;
import java.util.stream.Collectors;

public class Loja extends Entidade {

    private Queue<Encomenda> novas;
    private HashMap<String, Encomenda> processadas;
    private ArrayList<Encomenda> entregues;
    //TODO: Tempo medio de espera para precessamento de encomendas

    public Loja(String nome, GPS gps, String email, String password) {
        super(nome, gps, email, password);
        novas = new LinkedList<>();
        processadas = new HashMap<>();
        entregues = new ArrayList<>();
    }

    public void addEncomenda(Encomenda encomenda) {
        novas.offer(encomenda);
    }

    public void processar() {
        Encomenda encomenda = novas.poll();
        assert encomenda != null;
        processadas.put(encomenda.getCodEncomenda(), encomenda);
    }

    public void removeFromNovas(Encomenda encomenda) {
        novas.remove(encomenda);
    }

    public void addToProcessadas(Encomenda encomenda) {
        processadas.put(encomenda.getCodEncomenda(), encomenda);
    }

    public void entregar(String cod) {
        Encomenda encomenda = processadas.get(cod);
        processadas.remove(cod);
        entregues.add(encomenda);
    }

    public void printNovas() {
        if (novas.isEmpty()) {
            System.out.println("\nNão existem encomendas por processar");
        } else {
            System.out.println("\nEstas são as encomendas por aceitar da sua loja: ");
            novas.forEach(System.out::println);
        }
    }

    public void printProcessadas() {
        if (processadas.isEmpty()) {
            System.out.println("\nAinda não foi processada nenhuma encomenda");
        } else {
            System.out.println("\nEstas são as encomendas prontas para serem enviadas: ");
            processadas.forEach((key, enconemda) -> System.out.println(enconemda));
        }
    }

    public void printEntregues() { // As encomendas que foram entregues a transportadora. Para consulta de histórico.
        if (entregues.isEmpty()) {
            System.out.println("\nNenhuma encomenda foi entregue");
        } else {
            System.out.println("\nEste é o histórico das encomendas entregues da sua loja:");
            entregues.forEach(System.out::println);
        }
    }

    public int filaDeEsperaParaProcessar(Encomenda encomenda) {
        return novas.size();
    } //TODO: Verificar quantas encomendas estão à frende na fila de espera.

    public Encomenda proximaEncomenda() {
        return novas.peek(); //Devolve a head da Queue sem a remover.
    }

    public ArrayList<Encomenda> getProcessadas() {
        return processadas.keySet().stream().map(key -> processadas.get(key)).collect(Collectors.toCollection(ArrayList::new));
    }

    public Queue<Encomenda> getNovas() {
        return novas;
    }
}

