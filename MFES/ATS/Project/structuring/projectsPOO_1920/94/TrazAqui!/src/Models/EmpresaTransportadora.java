package Models;

import View.View;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe que guarda informação sobre uma Transportadora
 *
 */


public class EmpresaTransportadora extends Agente implements Serializable {
    private final String NIF;
    private final double taxa_km;
    private final int capacidade;
    private final Map<Encomenda, StatsEntrega> encomendas_ativas; ///encomendas por entregar
    private double distancia_percorrida;

    public EmpresaTransportadora(String nome, String cod, double clas, int t, double x, double y, double r, double taxa, int cap, double dist, String n, boolean med, boolean d) {
        super(nome, cod, clas, t, x, y, r, med, d);
        this.taxa_km = taxa;
        this.capacidade = cap;
        this.encomendas_ativas = new HashMap<>();
        this.distancia_percorrida = dist;
        this.NIF = n;
    }

    public EmpresaTransportadora(EmpresaTransportadora e) {
        super(e.getNome(), e.getCodigo(), e.getClassificacao(), e.getTotalClassifs(), e.getCoordenadaX(), e.getCoordenadaY(), e.getRaio_acao(), e.getMed(), e.getDisponibilidade());
        this.taxa_km = e.taxa_km;
        this.capacidade = e.capacidade;
        this.encomendas_ativas = e.getEncomendas_Ativas();
        this.distancia_percorrida = e.distancia_percorrida;
        this.NIF = e.NIF;
    }

    public Map<Encomenda, StatsEntrega> getEncomendas_Ativas() {
        HashMap<Encomenda, StatsEntrega> res = new HashMap<>();
        for (Map.Entry<Encomenda, StatsEntrega> e : this.encomendas_ativas.entrySet()) {
            res.put(e.getKey().clone(), e.getValue().clone());
        }

        return res;
    }

    public EmpresaTransportadora clone() {
        return new EmpresaTransportadora(this);
    }

    public double getDistancia_percorrida() {
        return distancia_percorrida;
    }

    public void setDistancia_percorrida(double distancia_percorrida) {
        this.distancia_percorrida = distancia_percorrida;
    }

    public double orcamento(Utilizador u) {
        return taxa_km * this.get_distance(u.getCoordenadaX(), u.getCoordenadaX());
    }

    public double delivery_time(Utilizador u) {
        int tempo=1 + (int)(Math.random() * ((3 - 1) + 1)); //simular o tempo meteorologico 1 se estiver sol, 2 a chover e 3 a nevar
        int transito=1 + (int)(Math.random() * ((3 - 1) + 1));//simular o transito 1 se estiver livre, 2 com algum transito e 3 com transito parado
        return tempo*transito*2 * this.get_distance(u.getCoordenadaX(), u.getCoordenadaX()); //2 minutos por Km (devolve double minutos)
    }


    public boolean solicitar_transporte(Encomenda x, Utilizador u, Loja l) {
        if (this.encomenda_in_range(u, l) && getDisponibilidade()) {
            double preco = this.orcamento(u);
            if (!View.pedido_orcamento(preco, this.delivery_time(u))) return false; //user nao aceita orcamento

            if (encomendas_ativas.size() < capacidade) {

                this.encomendas_ativas.put(x.clone(), new StatsEntrega(LocalDateTime.now(), preco));
                this.distancia_percorrida += this.distanticia_entrega(u, l);
                return true;
            } else return false; //a empresa tem o nº de encomendas máx( ==capacidade)

        } else return false; //a encomenda não está no alcance da empresa

    }

    public double total_faturado(LocalDateTime inicio, LocalDateTime fim) {
        return this.encomendas_ativas.values()
                .stream()
                .filter(s -> s.getHora_entrada().compareTo(inicio) >= 0 && s.getHora_saida().compareTo(fim) <= 0)
                .map(StatsEntrega::getFaturado)
                .reduce(Double::sum).orElse(0.0);
    }

    public boolean has_order_in_name(String user_code) {
        for (Encomenda e:this.encomendas_ativas.keySet()) {
            if (e.getUser().equals(user_code)) return true;
        }
        return false;
    }

    public Encomenda get_order_by_user (String user_code) {
        for (Encomenda e:this.encomendas_ativas.keySet()) {
            if (e.getUser().equals(user_code)) return e;
        }
        return null;
    }


    public void remove_encomenda(String user_code, LocalDateTime hora_chegada) {
        Encomenda aux=get_order_by_user(user_code);
        StatsEntrega novo=this.encomendas_ativas.get(aux);

        novo.setHora_saida(hora_chegada);
        this.add_encomenda_to_historico(aux,novo);

        this.encomendas_ativas.remove(aux);
    }

}
