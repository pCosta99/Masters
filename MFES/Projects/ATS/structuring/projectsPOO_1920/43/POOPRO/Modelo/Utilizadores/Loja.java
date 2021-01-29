package Modelo.Utilizadores;

import Modelo.Encomendas.Encomenda;
import Modelo.Encomendas.LinhaEncomenda;
import Modelo.Encomendas.RegistoEncomendas;
import Modelo.Produtos.Produto;

import java.util.*;

public class Loja extends Utilizador {

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private boolean temFilaEspera;
    private int tempoEspera;
    private int numeroFila;
    private int tempoMedio;
    private Map<String,Produto> produtos;
    private List<Encomenda> pedidosAceites;
    private List<Encomenda> pedidosProntos;

    /**
     * CONSTRUTOR VAZIO
     */

    public Loja() {
        super();
        this.temFilaEspera = false;
        this.tempoEspera = 0;
        this.numeroFila = 0;
        this.tempoMedio = 0;
        this.produtos = new HashMap<>();
        this.pedidosAceites = new ArrayList<>();
        this.pedidosProntos = new ArrayList<>();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 1
     */

    public Loja(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude) {
        super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude);
        this.temFilaEspera = false;
        this.tempoEspera = 0;
        this.numeroFila = 0;
        this.tempoMedio = 0;
        this.produtos = new HashMap<>();
        this.pedidosAceites = new ArrayList<>();
        this.pedidosProntos = new ArrayList<>();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 2
     */

    public Loja(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, double nLatitude, double nLongitude, boolean temFila, int tempoMedio) {
        super(nNome, nEmail, nPassword, nCodUtilizador, nNif, nLatitude, nLongitude);
        this.temFilaEspera = temFila;
        this.tempoEspera = 0;
        this.numeroFila = 0;
        this.tempoMedio = tempoMedio;
        this.produtos = new HashMap<>();
        this.pedidosAceites = new ArrayList<>();
        this.pedidosProntos = new ArrayList<>();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO 3
     */

    public Loja(String nNome, String nEmail, String nPassword, String nCodUtilizador, String nNif, RegistoEncomendas nRegisto, boolean nTemFilaEspera, int nTempoEspera, int nNumeroFila, int nTempoMedio, Map<String,Produto> nProdutos, List<Encomenda> nProntos, List<Encomenda> nPedidos, double nLatitude, double nLongitude, List<Encomenda> nPedidosAceites, List<Encomenda> nPedidosProntos) {
        super(nNome,nEmail,nPassword,nCodUtilizador,nNif, nLatitude, nLongitude, nPedidos, nRegisto);
        this.temFilaEspera = nTemFilaEspera;
        this.tempoEspera = nTempoEspera;
        this.numeroFila = nNumeroFila;
        this.tempoMedio = nTempoMedio;
        this.produtos = new HashMap<>();
        for (Map.Entry<String, Produto> p : nProdutos.entrySet()) {
            this.produtos.put(p.getKey(), p.getValue().clone());
        }
        this.pedidosAceites = new ArrayList<>();
        for(Encomenda e: nPedidosAceites) {
            this.pedidosAceites.add(e.clone());
        }
        this.pedidosProntos = new ArrayList<>();
        for(Encomenda e: nPedidosProntos) {
            this.pedidosProntos.add(e.clone());
        }
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    public Loja(Loja nLoja) {
        super(nLoja);
        this.temFilaEspera = nLoja.temFilaEspera();
        this.tempoEspera = nLoja.getTempoEspera();
        this.numeroFila = nLoja.getNumeroFila();
        this.tempoMedio = nLoja.getTempoMedio();
        this.produtos = nLoja.getProdutos();
        this.pedidosAceites = nLoja.getPedidosAceites();
        this.pedidosProntos = nLoja.getPedidosProntos();
    }

    /**
     * GETTERS
     */

    public int getNumeroFila() {
        return this.numeroFila;
    }

    public int getTempoEspera() {
        return this.tempoEspera;
    }

    public int getTempoMedio() {
        return this.tempoMedio;
    }

    public boolean temFilaEspera() {
        return this.temFilaEspera;
    }

    public Map<String, Produto> getProdutos() {
        Map<String, Produto> rprodutos = new HashMap<>();

        for(Map.Entry<String, Produto> p: this.produtos.entrySet()) {
            rprodutos.put(p.getKey(), p.getValue().clone());
        }

        return rprodutos;
    }

    public List<Encomenda> getPedidosAceites() {
        List<Encomenda> res = new ArrayList<>();

        for(Encomenda e: this.pedidosAceites) {
            res.add(e.clone());
        }
        return res;
    }

    public List<Encomenda> getPedidosProntos() {
        List<Encomenda> res = new ArrayList<>();

        for(Encomenda e: this.pedidosProntos) {
            res.add(e.clone());
        }
        return res;
    }

    /**
     * SETTERS
     */

    public void setFilaEspera(boolean nFilaEspera) {
        this.temFilaEspera = nFilaEspera;
    }

    public void setNumeroFila(int numeroFila) {
        this.numeroFila = numeroFila;
    }

    public void setTempoEspera(int tempoEspera) {
        this.tempoEspera = tempoEspera;
    }

    public void setTempoMedio(int tempoMedio) {
        this.tempoMedio = tempoMedio;
    }

    public void setProdutos(Map<String, Produto> nProdutos) {
        this.produtos = new HashMap<>();

        for(Map.Entry<String, Produto> p: nProdutos.entrySet()) {
            this.produtos.put(p.getKey(),p.getValue().clone());
        }
    }

    public void setPedidosAceites(List<Encomenda> nPedidosAceites) {
        this.pedidosAceites = new ArrayList<>();

        for(Encomenda e: nPedidosAceites) {
            this.pedidosAceites.add(e.clone());
        }
    }

    public void setPedidosProntos(List<Encomenda> nPedidosProntos) {
        this.pedidosProntos = new ArrayList<>();

        for(Encomenda e: nPedidosProntos) {
            this.pedidosProntos.add(e.clone());
        }
    }

    /**
     * MÉTODO CLONE
     */

    public Loja clone() {
        return new Loja(this);
    }

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        if (!super.equals(o)) return false;
        Loja loja = (Loja) o;
        return  super.equals(loja) && temFilaEspera == loja.temFilaEspera &&
                tempoEspera == loja.tempoEspera &&
                numeroFila == loja.numeroFila &&
                tempoMedio == loja.tempoMedio &&
                Objects.equals(produtos, loja.produtos) &&
                Objects.equals(pedidosAceites, loja.pedidosAceites) &&
                Objects.equals(pedidosProntos, loja.pedidosProntos);
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("Loja{");
        sb.append(" Tem Fila de Espera=").append(this.temFilaEspera);
        sb.append(", Tempo Espera=").append(this.tempoEspera);
        sb.append(", Numero de Clientes na Fila=").append(this.numeroFila);
        sb.append(", Tempo Medio de Espera=").append(this.tempoMedio);
        sb.append(", " + super.toString());
        sb.append(", Produtos=").append(this.produtos);
        sb.append(", Pedidos Aceites=").append(this.pedidosAceites);
        sb.append(", Pedidos Prontos=").append(this.pedidosProntos);
        sb.append('}');
        return sb.toString();
    }

    /**
     * MÉTODO ADICIONAR PRODUTO
     */

    public void adicionarProduto(Produto nProduto) {
        this.produtos.put(nProduto.getCodProduto(),nProduto.clone());
    }

    /*
    public void removerProduto(String codProtudo) {
        this.produtos.remove(codProtudo);
    }
    */

    public void adicionarPedidoAceite(Encomenda nEncomenda) {
        this.pedidosAceites.add(nEncomenda.clone());
    }

    public void removerPedidoAceite(Encomenda nEncomenda) {
        this.pedidosAceites.remove(nEncomenda);
    }

    /*
    public void removerPedidoAceite(String codEncomenda) {
        for(Encomenda e: this.pedidosProntos) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                this.pedidosProntos.remove(e);
            }
        }
    }*/

    public void adicionarPedidoPronto(Encomenda nEncomenda) {
        this.pedidosProntos.add(nEncomenda.clone());
    }

    public void removerPedidoPronto(Encomenda nEncomenda) {
        this.pedidosProntos.remove(nEncomenda);
    }


    /*
    public void removerPedidoPronto(String codEncomenda) {
        for(Encomenda e: this.pedidosProntos) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                this.pedidosProntos.remove(e);
            }
        }
    }


    public boolean temPedidoPronto(String codEncomenda) {
        for(Encomenda e: this.pedidosProntos) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                return true;
            }
        }
        return false;
    }

    public Encomenda getPedidoPronto(String codEncomenda) {
        for(Encomenda e: this.pedidosProntos) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                return e.clone();
            }
        }
        return null;
    }*/

    public boolean temPedidoAceite(String codEncomenda) {
        for(Encomenda e: this.pedidosAceites) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                return true;
            }
        }
        return false;
    }

    public Encomenda getPedidoAceite(String codEncomenda) {
        for(Encomenda e: this.pedidosAceites) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                return e.clone();
            }
        }
        return null;
    }

    public void adicionarQueue() {
        this.numeroFila++;
    }

    public void removerQueue() {
        this.numeroFila--;
    }

    public void atualizarPedidoAceite(Encomenda enc) {
        for(Encomenda e: this.pedidosAceites) {
            if(e.getCodEncomenda().matches(enc.getCodEncomenda())) {
                this.pedidosAceites.remove(e);
            }
        }
        this.pedidosAceites.add(enc.clone());
    }
}
