import Exceptions.*;
import Modelo.Encomendas.*;
import Modelo.Produtos.Produto;
import Modelo.Utilizadores.*;

import java.io.*;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

public class Model implements Serializable{

    /**
     * VARIÁVEIS DE INSTÂNCIA
     */

    private Utilizador utilizador;
    private Map<String, Cliente> clientes;
    private Map<String, Loja> lojas;
    private Map<String, Transportadora> transportadoras;
    private Map<String, Voluntario> voluntarios;
    private Map<String, Encomenda> encomendas;

    /**
     * CONSTRUTOR VAZIO
     */

    public Model() {
        this.utilizador = null;
        this.clientes = new HashMap<>();
        this.lojas = new HashMap<>();
        this.transportadoras = new HashMap<>();
        this.voluntarios = new HashMap<>();
        this.encomendas = new HashMap<>();
    }

    /**
     * CONSTRUTOR PARAMETRIZADO
     */

    public Model(Utilizador nUtilizador, Map<String,Cliente> nClientes, Map<String,Loja> nLojas, Map<String,Transportadora> nTransportadoras, Map<String,Voluntario> nVoluntarios, Map<String,Encomenda> nEncomendas) {
        this.utilizador = nUtilizador.clone();
        this.clientes = new HashMap<>();
        for(Map.Entry<String, Cliente> c: nClientes.entrySet()) {
            this.clientes.put(c.getKey(),c.getValue().clone());
        }
        this.lojas = new HashMap<>();
        for(Map.Entry<String, Loja> l: nLojas.entrySet()) {
            this.lojas.put(l.getKey(),l.getValue().clone());
        }
        this.transportadoras = new HashMap<>();
        for(Map.Entry<String, Transportadora> t: nTransportadoras.entrySet()) {
            this.transportadoras.put(t.getKey(),t.getValue().clone());
        }
        this.voluntarios = new HashMap<>();
        for(Map.Entry<String, Voluntario> v: nVoluntarios.entrySet()) {
            this.voluntarios.put(v.getKey(),v.getValue().clone());
        }
        this.encomendas = new HashMap<>();
        for(Map.Entry<String, Encomenda> e: nEncomendas.entrySet()) {
            this.encomendas.put(e.getKey(),e.getValue().clone());
        }
    }

    /**
     * CONSTRUTOR POR CÓPIA
     */

    /*
    public Model(Model nModel) {
        this.utilizador = nModel.getUtilizador();
        this.clientes = nModel.getClientes();
        this.lojas = nModel.getLojas();
        this.transportadoras = nModel.getTransportadoras();
        this.voluntarios = nModel.getVoluntarios();
        this.encomendas = nModel.getEncomendas();
    }*/

    /**
     * GETTERS
     */


    public Map<String, Cliente> getClientes() {
        Map<String,Cliente> res = new HashMap<>();

        for(Map.Entry<String, Cliente> c: this.clientes.entrySet()) {
            res.put(c.getKey(),c.getValue().clone());
        }

        return res;
    }

    public Map<String, Loja> getLojas() {
        Map<String,Loja> res = new HashMap<>();

        for(Map.Entry<String, Loja> l: this.lojas.entrySet()) {
            res.put(l.getKey(),l.getValue().clone());
        }

        return res;
    }

    public Map<String, Transportadora> getTransportadoras() {
        Map<String,Transportadora> res = new HashMap<>();

        for(Map.Entry<String, Transportadora> t: this.transportadoras.entrySet()) {
            res.put(t.getKey(),t.getValue().clone());
        }

        return res;
    }

    public Map<String, Voluntario> getVoluntarios() {
        Map<String,Voluntario> res = new HashMap<>();

        for(Map.Entry<String, Voluntario> v: this.voluntarios.entrySet()) {
            res.put(v.getKey(),v.getValue().clone());
        }

        return res;
    }

    public Map<String, Encomenda> getEncomendas() {
        Map<String,Encomenda> res = new HashMap<>();

        for(Map.Entry<String, Encomenda> e: this.encomendas.entrySet()) {
            res.put(e.getKey(),e.getValue().clone());
        }

        return res;
    }

    public Utilizador getUtilizador() {
        return this.utilizador.clone();
    }

    public Loja getLoja(String codLoja) {
        return this.lojas.get(codLoja);
    }

    public Transportadora getTransportadora(String codTransportadora) {
        return this.transportadoras.get(codTransportadora);
    }

    public Voluntario getVoluntarios(String codVoluntario) {
        return this.voluntarios.get(codVoluntario);
    }

    public Cliente getCliente(String codCliente) {
        return this.clientes.get(codCliente);
    }

    /**
     * SETTERS
     */

    public void setUtilizador(Utilizador utilizador) {
        this.utilizador = utilizador.clone();
    }

    public void setClientes(Map<String, Cliente> clientes) {
        this.clientes = new HashMap<>();

        for(Map.Entry<String, Cliente> c: clientes.entrySet()) {
            this.clientes.put(c.getKey(),c.getValue().clone());
        }
    }

    public void setLojas(Map<String, Loja> lojas) {
        this.lojas = new HashMap<>();

        for(Map.Entry<String, Loja> l: lojas.entrySet()) {
            this.lojas.put(l.getKey(),l.getValue().clone());
        }
    }

    public void setTransportadoras(Map<String, Transportadora> transportadoras) {
        this.transportadoras = new HashMap<>();

        for(Map.Entry<String, Transportadora> t: transportadoras.entrySet()) {
            this.transportadoras.put(t.getKey(),t.getValue().clone());
        }
    }

    public void setVoluntarios(Map<String, Voluntario> voluntarios) {
        this.voluntarios = new HashMap<>();

        for(Map.Entry<String, Voluntario> v: voluntarios.entrySet()) {
            this.voluntarios.put(v.getKey(),v.getValue().clone());
        }
    }

    public void setEncomendas(Map<String, Encomenda> encomendas) {
        this.encomendas = new HashMap<>();

        for(Map.Entry<String, Encomenda> e: encomendas.entrySet()) {
            this.encomendas.put(e.getKey(),e.getValue().clone());
        }
    }

    /**
     * MÉTODO CLONE
     */

    /*
    public Model clone() {
        return new Model(this);
    }*/

    /**
     * MÉTODO EQUALS
     */

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Model model = (Model) o;
        return Objects.equals(utilizador, model.utilizador) &&
                Objects.equals(clientes, model.clientes) &&
                Objects.equals(lojas, model.lojas) &&
                Objects.equals(transportadoras, model.transportadoras) &&
                Objects.equals(voluntarios, model.voluntarios) &&
                Objects.equals(encomendas, model.encomendas);
    }

    /**
     * MÉTODO TOSTRING
     */

    public String toString() {
        final StringBuffer sb = new StringBuffer();
        sb.append("Model{");
        sb.append("Utilizador=").append(this.utilizador.toString());
        sb.append(", Clientes=").append(this.clientes.toString());
        sb.append(", Lojas=").append(this.lojas.toString());
        sb.append(", Transportadoras=").append(this.transportadoras.toString());
        sb.append(", Voluntarios=").append(this.voluntarios.toString());
        sb.append(", Encomendas=").append(this.encomendas.toString());
        sb.append('}');
        return sb.toString();
    }

    /**
     * MÉTODOS LOGIN
     */

    public void loginCliente(String codUtilizador, String password) throws ClienteInvalidoException, PasswordInvalidaException {

        if(!this.clientes.containsKey(codUtilizador)) {
            throw new ClienteInvalidoException("CLIENTE INVALIDO");
        }

        Cliente cliente = this.clientes.get(codUtilizador);

        if(!cliente.getPassword().equals(password)) {
            throw new PasswordInvalidaException("PASSWORD INVALIDA");
        }

        this.utilizador = cliente.clone();
    }

    public void loginLoja(String codUtilizador, String password) throws LojaInvalidaException, PasswordInvalidaException {

        if(!this.lojas.containsKey(codUtilizador)) {
            throw new LojaInvalidaException("LOJA INVALIDA");
        }

        Loja loja = this.lojas.get(codUtilizador);

        if(!loja.getPassword().equals(password)) {
            throw new PasswordInvalidaException("PASSWORD INVALIDA");
        }

        this.utilizador = loja.clone();
    }

    public void loginTransportadora(String codUtilizador, String password) throws TransportadoraInvalidaException, PasswordInvalidaException {

        if(!this.transportadoras.containsKey(codUtilizador)) {
            throw new TransportadoraInvalidaException("TRANSPORTADORA INVALIDA");
        }

        Transportadora transportadora = this.transportadoras.get(codUtilizador);

        if(!transportadora.getPassword().equals(password)) {
            throw new PasswordInvalidaException("PASSWORD INVALIDA");
        }

        this.utilizador = transportadora.clone();
    }

    public void loginVoluntario(String codUtilizador, String password) throws VoluntarioInvalidoException, PasswordInvalidaException {

        if(!this.voluntarios.containsKey(codUtilizador)) {
            throw new VoluntarioInvalidoException("VOLUNTARIO INVALIDO");
        }

        Voluntario voluntario = this.voluntarios.get(codUtilizador);

        if(!voluntario.getPassword().equals(password)) {
            throw new PasswordInvalidaException("PASSWORD INVALIDA");
        }

        this.utilizador = voluntario.clone();
    }

    /**
     * MÉTODOS REGISTAR
     */

    public void registarCliente(String nome, String mail, String password, String codUtilizador, String nif, double latitude, double longitude) throws ClienteInvalidoException, ValorInvalidoException {

        if (this.clientes.containsKey(codUtilizador)) {
            throw new ClienteInvalidoException("CLIENTE INVALIDO");
        }


        if (90 >= latitude && latitude >= -90) {
            if(180 >= longitude && longitude >= -180) {
                if (nif.matches("[0-9]+") && nif.length() == 9) {
                    this.clientes.put(codUtilizador, new Cliente(nome, mail, password, codUtilizador, nif, latitude, longitude));
                } else {
                    throw new ValorInvalidoException("NIF INVALIDO");
                }

            } else {
                throw new ValorInvalidoException("LONGITUDE INVALIDA");
            }
        } else {
            throw new ValorInvalidoException("LATITUDE INVALIDA");
        }
    }


    public void registarLoja(String nome, String mail, String password, String codUtilizador, String nif, double latitude, double longitude, boolean temFila, int tempoMedio) throws LojaInvalidaException, ValorInvalidoException {

        if (this.lojas.containsKey(codUtilizador)) {
            throw new LojaInvalidaException("LOJA INVALIDA");
        }

        if (90 >= latitude && latitude >= -90) {
            if(180 >= longitude && longitude >= -180) {
                if (nif.matches("[0-9]+") && nif.length() == 9) {
                    if(temFila == false && tempoMedio > 0 || temFila == true) {
                        this.lojas.put(codUtilizador, new Loja(nome, mail, password, codUtilizador, nif, latitude, longitude, temFila, tempoMedio));
                    } else {
                        throw new ValorInvalidoException("TEMPO MEDIO INVALIDO");
                    }
                } else {
                    throw new ValorInvalidoException("NIF INVALIDO");
                }
            } else {
                throw new ValorInvalidoException("LONGITUDE INVALIDA");
            }
        } else {
            throw new ValorInvalidoException("LATITUDE INVALIDA");
        }
    }

    public void registarTransportadora(String nome, String mail, String password, String codUtilizador, String nif, double latitude, double longitude, double raioAcao, double taxaKM, int nrTrabalhadores) throws TransportadoraInvalidaException, ValorInvalidoException {

        if (this.transportadoras.containsKey(codUtilizador)) {
            throw new TransportadoraInvalidaException("TRANSPORTADORA INVALIDA");
        }

        if (90 >= latitude && latitude >= -90) {
            if(180 >= longitude && longitude >= -180) {
                if (nif.matches("[0-9]+") && nif.length() == 9) {
                    if(raioAcao > 0) {
                        if(taxaKM > 0) {
                            if(nrTrabalhadores > 0) {
                                this.transportadoras.put(codUtilizador, new Transportadora(nome, mail, password, codUtilizador, nif, latitude, longitude,raioAcao,taxaKM,nrTrabalhadores));
                            } else {
                                throw new ValorInvalidoException("NR DE TRABALHADORES INVALIDO");
                            }
                        } else {
                            throw new ValorInvalidoException("TAXA POR KM INVALIDA");
                        }
                    } else {
                        throw new ValorInvalidoException("RAIO DE ACAO INVALIDO");
                    }
                } else {
                    throw new ValorInvalidoException("NIF INVALIDO");
                }
            } else {
                throw new ValorInvalidoException("LONGITUDE INVALIDA");
            }
        } else {
            throw new ValorInvalidoException("LATITUDE INVALIDA");
        }
    }

    public void registarVoluntario(String nome, String mail, String password, String codUtilizador, String nif, double latitude, double longitude, double raioAcao) throws VoluntarioInvalidoException, ValorInvalidoException {

        if (this.voluntarios.containsKey(codUtilizador)) {
            throw new VoluntarioInvalidoException("VOLUNTARIO INVALIDO");
        }

        if (90 >= latitude && latitude >= -90) {
            if(180 >= longitude && longitude >= -180) {
                if (nif.matches("[0-9]+") && nif.length() == 9) {
                    if(raioAcao > 0) {
                        this.voluntarios.put(codUtilizador, new Voluntario(nome, mail, password, codUtilizador, nif, latitude, longitude, raioAcao));
                    } else {
                        throw new ValorInvalidoException("RAIO DE ACAO INVALIDO");
                    }
                } else {
                    throw new ValorInvalidoException("NIF INVALIDO");
                }
            } else {
                throw new ValorInvalidoException("LONGITUDE INVALIDA");
            }
        } else {
            throw new ValorInvalidoException("LATITUDE INVALIDA");
        }
    }

    /**
     * MÉTODO TERMINAR SESSÃO
     */

    public void terminarSessao() {
        this.utilizador = null;
    }

    /**
     * MÉTODO CRIAR ENCOMENDA
     */

    public void criarEncomenda(String codLoja, Map<String,Integer> valores) throws ValorInvalidoException, LojaInvalidaException, ProdutoInvalidoException {

        Encomenda nEncomenda = new Encomenda(codLoja,this.utilizador.getCodUtilizador());
        Produto produto = new Produto();
        LinhaEncomenda nLinha = new LinhaEncomenda();

        if(this.lojas.containsKey(codLoja)) {

            Loja loja = this.lojas.get(codLoja);

            for(Map.Entry<String,Integer> valor : valores.entrySet()) {
                if(valor.getValue() > 0) {
                    if(loja.getProdutos().containsKey(valor.getKey())) {

                        produto = loja.getProdutos().get(valor.getKey());
                        nLinha = new LinhaEncomenda(produto,valor.getValue());
                        nEncomenda.adicionarLinhaEncomenda(nLinha);
                        nEncomenda.atualizarPesoTotal(produto.getPesoUnitario()*valor.getValue());
                        nEncomenda.atualizarPrecoLoja(produto.getValorUnitario()*valor.getValue());

                    } else {
                        throw new ProdutoInvalidoException("CODIGO DE PRODUTO INVALIDO");
                    }
                } else {
                    throw new ValorInvalidoException("QUANTIDADE INVALIDA");
                }
            }

            loja.adicionarPedido(nEncomenda);

        } else {
            throw new LojaInvalidaException("CODIGO DE LOJA INVALIDO");
        }
    }

    /**
     * MÉTODO ADICIONAR PRODUTO
     */

    public void adicionarProduto(String descricao, double valorUnitario, double pesoUnitario) throws ValorInvalidoException {
        if(valorUnitario > 0) {
            if(pesoUnitario > 0) {
                Loja loja = this.lojas.get(this.utilizador.getCodUtilizador());
                Produto produto = new Produto(descricao,valorUnitario,pesoUnitario);
                loja.adicionarProduto(produto);
            } else {
                throw new ValorInvalidoException("Peso Unitario Invalido");
            }
        } else {
            throw new ValorInvalidoException("Valor Unitario Invalido");
        }
    }

    /**
     * MÉTODOS ACEITAR E REJEITAR PEDIDO CLIENTE
     */

    public void aceitaPedidoC(String codEncomenda) throws EncomendaInvalidaException {
        if(this.utilizador.temPedido(codEncomenda)) {
            Encomenda enc = this.utilizador.getPedido(codEncomenda);
            this.utilizador.removerPedido(enc);
            Transportadora nTrans = this.transportadoras.get(enc.getCodTransportadora());
            Loja loja = this.lojas.get(enc.getCodLoja());
            loja.atualizarPedidoAceite(enc);
            nTrans.removerPedido(enc);
            nTrans.adicionarPedidoAceite(enc);
            nTrans.removerTrabalhador();
        } else {
            throw new EncomendaInvalidaException("PEDIDO INVALIDO");
        }
    }

    public void rejeitaPedidoC(String codEncomenda) throws EncomendaInvalidaException {
        if(this.utilizador.temPedido(codEncomenda)) {
            Encomenda enc = this.utilizador.getPedido(codEncomenda);
            this.utilizador.removerPedido(enc);
        } else {
            throw new EncomendaInvalidaException("PEDIDO INVALIDO");
        }
    }

    /**
     * MÉTODOS ACEITAR E REJEITAR PEDIDO LOJA
     */

    public void aceitaPedidoL(String codEncomenda) throws EncomendaInvalidaException {
        if(this.utilizador.temPedido(codEncomenda)) {
            Loja loja = this.lojas.get(this.utilizador.getCodUtilizador());
            if(loja.temFilaEspera()) {
                loja.adicionarQueue();
            }
            Encomenda enc = this.utilizador.getPedido(codEncomenda);
            this.utilizador.removerPedido(enc);
            loja.adicionarPedidoAceite(enc);
        } else {
            throw new EncomendaInvalidaException("PEDIDO INVALIDO");
        }
    }

    public void rejeitaPedidoL(String codEncomenda) throws EncomendaInvalidaException {
        if(this.utilizador.temPedido(codEncomenda)) {
            Encomenda enc = this.utilizador.getPedido(codEncomenda);
            this.utilizador.removerPedido(enc);
        } else {
            throw new EncomendaInvalidaException("PEDIDO INVALIDO");
        }
    }

    /**
     * MÉTODO PEDIDO PRONTO LOJA
     */

    public boolean temEstafeta(String codEncomenda) {
        for(Map.Entry<String,Transportadora> t : this.transportadoras.entrySet()) {
            for(Encomenda e: t.getValue().getPedidosAceites()) {
                if(e.getCodEncomenda().matches(codEncomenda)) {
                    return true;
                }
            }
        }
        for(Map.Entry<String,Voluntario> v : this.voluntarios.entrySet()) {
            for(Encomenda e: v.getValue().getPedidos()) {
                if(e.getCodEncomenda().matches(codEncomenda)) {
                    return true;
                }
            }
        }
        return false;
    }

    public void pedidoProntoL(String codEncomenda) throws EncomendaInvalidaException {
        Loja loja = this.lojas.get(this.utilizador.getCodUtilizador());
        if(loja.temPedidoAceite(codEncomenda)) {
            Encomenda enc = loja.getPedidoAceite(codEncomenda);
            if(enc.getPrecoTransporte() == 0) {

                Voluntario voluntario = this.voluntarios.get(enc.getCodTransportadora());

                if(voluntario.temPedido(codEncomenda)) {
                    if(loja.temFilaEspera()) {
                        loja.removerQueue();
                    }
                    loja.removerPedidoAceite(enc);
                    LocalDateTime tempoDeLoja = LocalDateTime.now();
                    enc.setTempoLoja(tempoDeLoja);
                    voluntario.atualizarPedidoAceite(enc);
                    loja.adicionarPedidoPronto(enc);
                } else {
                    throw new EncomendaInvalidaException("PEDIDO INVALIDO");
                }

            } else {

                Transportadora transportadora = this.transportadoras.get(enc.getCodTransportadora());

                if(transportadora.temPedidoAceite(codEncomenda)) {
                    if(loja.temFilaEspera()) {
                        loja.removerQueue();
                    }
                    loja.removerPedidoAceite(enc);
                    LocalDateTime tempoDeLoja = LocalDateTime.now();
                    enc.setTempoLoja(tempoDeLoja);
                    transportadora.atualizarPedidoAceite(enc);
                    loja.adicionarPedidoPronto(enc);
                } else {
                    throw new EncomendaInvalidaException("PEDIDO INVALIDO");
                }

            }
        } else {
            throw new EncomendaInvalidaException("PEDIDO INVALIDO");
        }
    }

    public List<Encomenda> getPedidosAceitesLojas() {
        List<Encomenda> res = new ArrayList<>();

        for(Map.Entry<String,Loja> l : this.lojas.entrySet()) {
            for(Encomenda e: l.getValue().getPedidosAceites()) {
                res.add(e.clone());
            }
        }

        return res;
    }

    public Encomenda extratorEncomenda(List<Encomenda> lista, String codEncomenda) {
        for(Encomenda e : lista) {
            if(e.getCodEncomenda().matches(codEncomenda)) {
                return e.clone();
            }
        }
        return null;
    }

    public void selecionaPedidoT(List<Encomenda> lista, String codEncomenda) throws EncomendaInvalidaException,TransportadoraInvalidaException, ValorInvalidoException {

        Encomenda enc = extratorEncomenda(lista,codEncomenda);

        Transportadora transportadora = this.transportadoras.get(this.utilizador.getCodUtilizador());
        Cliente cliente = this.clientes.get(enc.getCodUtilizador());
        Loja loja = this.lojas.get(enc.getCodLoja());

        double distanciaLoja = this.utilizador.getDistancia(loja.getLatitude(), loja.getLongitude());
        double distanciaCliente = this.utilizador.getDistancia(cliente.getLatitude(), cliente.getLongitude());

        if(distanciaLoja < transportadora.getRaioAcao()) {
            if(distanciaCliente < transportadora.getRaioAcao()) {
                if(transportadora.getNrTrabalhadores() > 0) {
                    if(!temEstafeta(codEncomenda)) {
                        enc.setCodTransportadora(transportadora.getCodUtilizador());
                        enc.setPrecoTransporte(distanciaLoja*transportadora.getTaxaKM() + distanciaCliente*transportadora.getTaxaKM());
                        transportadora.adicionarPedido(enc);
                        cliente.adicionarPedido(enc);
                    } else {
                        throw new EncomendaInvalidaException("JA TEM ESTAFETA");
                    }
                } else {
                    throw new TransportadoraInvalidaException("NENHUM TRABALHADOR DISPONIVEL");
                }
            } else {
                throw new ValorInvalidoException("DISTANCIA DEMASIADO GRANDE");
            }
        } else {
            throw new ValorInvalidoException("DISTANCIA DEMASIADO GRANDE");
        }
    }

    public void selecionaPedidoV(List<Encomenda> lista, String codEncomenda) throws EncomendaInvalidaException,VoluntarioInvalidoException, ValorInvalidoException {

        Encomenda enc = extratorEncomenda(lista,codEncomenda);

        Voluntario voluntario = this.voluntarios.get(this.utilizador.getCodUtilizador());
        Cliente cliente = this.clientes.get(enc.getCodUtilizador());
        Loja loja = this.lojas.get(enc.getCodLoja());

        double distanciaLoja = this.utilizador.getDistancia(loja.getLatitude(), loja.getLongitude());
        double distanciaCliente = this.utilizador.getDistancia(cliente.getLatitude(), cliente.getLongitude());

        if(distanciaLoja < voluntario.getRaioAcao()) {
            if(distanciaCliente < voluntario.getRaioAcao()) {
                if(voluntario.getPedidos().isEmpty()) {
                    if(!temEstafeta(codEncomenda)) {
                        enc.setCodTransportadora(voluntario.getCodUtilizador());
                        voluntario.adicionarPedido(enc);
                        loja.atualizarPedidoAceite(enc);
                    } else {
                        throw new EncomendaInvalidaException("JA TEM ESTAFETA");
                    }
                } else {
                    throw new VoluntarioInvalidoException("JA TEM UMA ENCOMENDA");
                }
            } else {
                throw new ValorInvalidoException("DISTANCIA DEMASIADO GRANDE");
            }
        } else {
            throw new ValorInvalidoException("DISTANCIA DEMASIADO GRANDE");
        }
    }

    public void pedidoEntregueT(String codEncomenda) throws EncomendaInvalidaException {

        Transportadora transportadora = this.transportadoras.get(this.utilizador.getCodUtilizador());

        if(transportadora.temPedidoAceite(codEncomenda)) {

            Encomenda enc = transportadora.getPedidoAceite(codEncomenda);

            Loja loja = this.lojas.get(enc.getCodLoja());

            if(loja.getPedidosProntos().contains(enc)) {

                loja.removerPedidoPronto(enc);
                transportadora.removerPedidoAceite(enc);
                enc.setTempoTransporte(LocalDateTime.now());
                enc.setTempoTotal(LocalDateTime.now());
                enc.atualizarPrecoTotal();
                transportadora.adicionarTrabalhador();
                this.encomendas.put(enc.getCodEncomenda(),enc.clone());
            } else {
                throw new EncomendaInvalidaException("PEDIDO INVALIDO");
            }
        } else {
            throw new EncomendaInvalidaException("PEDIDO INVALIDO");
        }
    }

    public void pedidoEntregueV(String codEncomenda) throws EncomendaInvalidaException {

        Voluntario voluntario = this.voluntarios.get(this.utilizador.getCodUtilizador());

        if(voluntario.temPedido(codEncomenda)) {

            Encomenda enc = voluntario.getPedido(codEncomenda);

            Loja loja = this.lojas.get(enc.getCodLoja());

            if(loja.getPedidosProntos().contains(enc)) {

                loja.removerPedidoPronto(enc);
                voluntario.removerPedido(enc);
                enc.setTempoTransporte(LocalDateTime.now());
                enc.setTempoTotal(LocalDateTime.now());
                enc.atualizarPrecoTotal();
                this.encomendas.put(enc.getCodEncomenda(),enc.clone());
            } else {
                throw new EncomendaInvalidaException("PEDIDO INVALIDO");
            }
        } else {
            throw new EncomendaInvalidaException("PEDIDO INVALIDO");
        }
    }

    /**
     * QUERIES
     */

    /**
     * MÉTODO lista das 10 empresas que mais utilizam o sistema em kms percorridos
     */

    public List<String> empresasMaisUtilizadas () {

        Map<String,List<Encomenda>> encAceites = new HashMap<>();
        Map<String,Integer> codTrans = new HashMap<>();

        String cT;
        List<Integer> values = new ArrayList<>();
        List<String> keys = new ArrayList<>();
        List<String> keysR = new ArrayList<>();

        int varT;
        String varK;



        for (Map.Entry<String, Encomenda> e : this.encomendas.entrySet()) {

            cT = e.getValue().getCodTransportadora();

            if (codTrans.containsKey(cT)) {

                codTrans.put(cT, codTrans.get(cT) + 1);
            } else codTrans.put(cT, 1);
        }
        //Adiciona as keys num arrayList e os values noutro
        for (Map.Entry<String, Integer> m : codTrans.entrySet()) {
            values.add(m.getValue());
        }

        for (Map.Entry<String, List<Encomenda>> m : encAceites.entrySet()) {
            keys.add(m.getKey());
        }


        for (int i = 0; i < values.size() - 1; i++) {
            for (int j = i + 1; j < values.size() - 1; j++) {
                if (values.get(i) < values.get(j)) {
                    varT = values.get(j);
                    varK = keys.get(j);
                    values.add(j, values.get(i));
                    keys.add(j, keys.get(i));
                    values.add(i, varT);
                    keys.add(i, varK);

                }
            }
        }

        for ( int i= 0; i<10; i++){
            keysR.add(i,keys.get(i));
        }
        return keysR;

    }

    public double totalFaturadoPeríodoTransportadora(String CodUtilizador, LocalDate inicio, LocalDate fim){
        double total =  this.getEncomendas()
                            .values()
                            .stream()
                            .filter(e -> e.getCodTransportadora().matches(CodUtilizador))
                            .filter(e -> (!(e.getData().isBefore(inicio)) || (e.getData().isAfter(fim))))
                            .mapToDouble(e -> e.getPrecoTransporte())
                            .sum();

        return total;
    }

    //retorna as encomendas concretizadas
    public List<Encomenda> listaEncomendas(){
        List<Encomenda> l = this.getEncomendas()
                                .values()
                                .stream()
                                .filter(e -> e.getCodUtilizador().equals(this.getUtilizador().getCodUtilizador()))
                                .sorted(Comparator.comparing(Encomenda::getData))
                                .map(Encomenda::clone)
                                .collect(Collectors.toList());
        return l;
    }



    //retorna as encomendas satisfeitas pela loja
    public List<Encomenda> listaEncomendasLojas(){
        List<Encomenda> l = this.getEncomendas()
                .values()
                .stream()
                .filter(e -> e.getCodLoja().equals(this.getUtilizador().getCodUtilizador()))
                .sorted(Comparator.comparing(Encomenda::getData))
                .map(Encomenda::clone)
                .collect(Collectors.toList());
        return l;
    }

    //retorna as encomendas do utilizador q fez o transporte
    public List<Encomenda> listaEncomendasTransp(){
        List<Encomenda> l = this.getEncomendas()
                .values()
                .stream()
                .filter(e -> e.getCodTransportadora().equals(this.getUtilizador().getCodUtilizador()))
                .sorted(Comparator.comparing(Encomenda::getData))
                .map(Encomenda::clone)
                .collect(Collectors.toList());
        return l;
    }
    /*
    //retorna as encomendas do voluntario q fez o transporte
    public List<Encomenda> listaEncomendasVolunts(){
        List<Encomenda> l = this.getEncomendas()
                .values()
                .stream()
                .filter(e -> e.getCodTransportadora().equals(this.getUtilizador().getCodUtilizador()))
                .sorted(Comparator.comparing(Encomenda::getData))
                .map(Encomenda::clone)
                .collect(Collectors.toList());
        return l;
    }
    */

    public void atualizaClassificacao(String codEncomenda, int classificacao) throws EncomendaInvalidaException, TransporteInvalidoException{

        if(!(this.getEncomendas().containsKey(codEncomenda))){
            throw new EncomendaInvalidaException("ENCOMENDA INVÁLIDA");
        }


        String codTransp = this.getEncomendas().get(codEncomenda).getCodTransportadora();


        if(this.transportadoras.containsKey(codTransp)){
            double  classificacaoAntiga = this.transportadoras.get(codTransp).getClassificacao();
            int nrClassificacoes = this.transportadoras.get(codTransp).getNrClassificacoes();

            double novaClassificacao = (((classificacaoAntiga*nrClassificacoes)+classificacao)/(++nrClassificacoes));

            this.transportadoras.get(codTransp).setClassificacao(novaClassificacao);
            this.transportadoras.get(codTransp).setNrClassificacoes(nrClassificacoes);

        }
        else if (this.voluntarios.containsKey(codTransp)) {

            double classificacaoAntiga = this.voluntarios.get(codTransp).getClassificacao();
            int nrClassificacoes = this.voluntarios.get(codTransp).getNrClassificacoes();

            double novaClassificacao = (((classificacaoAntiga * nrClassificacoes) + classificacao) / (++nrClassificacoes));

            this.voluntarios.get(codTransp).setClassificacao(novaClassificacao);
            this.voluntarios.get(codTransp).setNrClassificacoes(nrClassificacoes);
        }
        else{
            throw new TransporteInvalidoException("TRANSPORTE INVÁLIDO");
        }

    }

    public String notaV(){
        String utilizador = this.getUtilizador().getCodUtilizador();
        double classificacao = this.getVoluntarios().get(utilizador).getClassificacao();
        int nr = this.getVoluntarios().get(utilizador).getNrClassificacoes();
        String nota = "A nota atual é de: " + classificacao + " dada por " + nr + " classificações!";

        return nota;
    }

    public String notaT(){
        String utilizador = this.getUtilizador().getCodUtilizador();
        double classificacao = this.getTransportadoras().get(utilizador).getClassificacao();
        int nr = this.getTransportadoras().get(utilizador).getNrClassificacoes();
        String nota = "A nota atual é de: " + classificacao + " dada por " + nr + " classificações!";

        return nota;
    }

    public void guardaEstado(String nomeFicheiro) throws FileNotFoundException, IOException {
        FileOutputStream fos = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(this);
        oos.flush();
        oos.close();
    }

    public static Model carregaEstado(String nomeFicheiro) throws FileNotFoundException, IOException,ClassNotFoundException {
        FileInputStream fis = new FileInputStream(nomeFicheiro);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Model m = (Model) ois.readObject();
        ois.close();
        return m;
    }


}
