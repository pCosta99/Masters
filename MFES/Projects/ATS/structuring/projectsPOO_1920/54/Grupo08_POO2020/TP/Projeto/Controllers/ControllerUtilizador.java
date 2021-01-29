package Projeto.Controllers;
import Projeto.Avisos.AvisoEmpresa;
import Projeto.Avisos.AvisoOrcamentoRecebido;
import Projeto.Encomenda.Encomenda;
import Projeto.Encomenda.LinhaDeEncomenda;
import Projeto.Entidades.Produto;
import Projeto.Exceptions.EntidadeNaoExisteException;
import Projeto.Exceptions.ListaVaziaException;
import Projeto.Interfaces.*;
import Projeto.Model.TrazAqui;
import Projeto.Util.GPS;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Classe que implementa o Controller do Cliente.
 * O Controller é conhecido da View(neste caso ViewCliente) e conhece o model(TrazAqui).
 * É este que faz a mediação da entrada e saída, comandando a view e o model para serem alterados de forma apropriada conforme o que o usuário solicitou.
 */
public class ControllerUtilizador {
    private final TrazAqui model;
    private final IUtilizador utilizador;
    private final boolean gravar;

    /**
     * Construtor parametrizado do Controller do Cliente.
     * @param model - TrazAqui é o nosso model
     * @param idCliente -ID do Cliente
     */
    public ControllerUtilizador(TrazAqui model, String idCliente, boolean gravar){
        this.model = model;
        this.utilizador = this.model.getCliente(idCliente);
        this.gravar = gravar;
    }

    /**
     * Método que retorna o utilizador que está a usar a app neste momento.
     */
    public IUtilizador getUtilizador() {
        return this.utilizador;
    }

    /**
     * Método que grava o estado atual num ficheiro objeto.
     */
    public void gravar() throws IOException {
        if(this.gravar)
            this.model.gravarObj();
    }

    /**
     * Método que altera o nome do Cliente que esta a usar a APP.
     * @param novoNome - nome novo do Cliente.
     */
    public void changeName(String novoNome) {
        this.utilizador.setNome(novoNome);
    }

    /**
     * Método que altera a localização do Cliente que esta a usar a APP.
     */
    public void changeLocal(float latitude, float longitude) {
        GPS gps = new GPS(latitude, longitude);
        this.utilizador.setLocalizacao(gps);
    }

    /**
     * Método que altera o NIF do Cliente que esta a usar a APP.
     */
    public void changeNIF(String nif) {
        this.utilizador.setNIF(nif);
    }

    /**
     * Método que devolve uma List de Strings em que cada String corresponde ao ID de uma encomenda feita pelo Cliente.
     * Caso o Cliente não tenha nenhuma Encomenda no histórico "atira" a ListaVaziaException.
     */
    public Collection<String> historicoEncomendas() throws ListaVaziaException {
        Collection<IEncomenda> list = this.utilizador.getEncomendas();
        Collection<String> ret = new ArrayList<>();
        if(!list.isEmpty()){
            for(IEncomenda e : list) {
                ret.add(e.getID());
            }
        } else throw new ListaVaziaException("Histórico de encomendas vazio!");
        return ret;
    }

    /**
     * Metodo que permite ao cliente classificar o voluntario ou empresa que efetuou o transporte da sua encomenda.
     */
    public void classificar(String idEnc, int classificacao) throws ListaVaziaException{
        String idTransportador = this.utilizador.getEncomenda(idEnc).getIdTransportador();
        if(idTransportador.charAt(0) == 'v'){
            IVoluntario v = this.model.getVoluntario(idTransportador);
            v.insereClassificacao(classificacao);
        }
        else if(idTransportador.charAt(0) == 't'){
            IEmpresa e = this.model.getEmpresa(idTransportador);
            e.insereClassificacao(classificacao);
        }
        else throw new ListaVaziaException("Histórico de encomendas vazio!");
    }

    /**
     * Método que devolve a Encomenda, com o id igual ao recebido, na lista de encomendas do Cliente.
     */
    public String getEncomenda(String idEnc) throws EntidadeNaoExisteException {
        String ret;
        IEncomenda e = this.utilizador.getEncomenda(idEnc);
        if(e != null) {
            ret = e.toString();
        }else throw new EntidadeNaoExisteException("Encomenda Inválida!");
        return ret;
    }

    /**
     * Método que devolve os ids dos 10 clientes que realizaram mais encomendas nesta APP.
     */
    public Collection<String> top10Clientes() {
        return this.model.topNClientesMaisEncomendaram(10)
                         .stream().map(IUtilizador::getId)
                         .collect(Collectors.toList());
    }

    /**
     * Método que devolve, ao Cliente, todas as lojas disponiveis.
     * Devolve uma collection de Strings, em que cada elemento é um array de Strings
     * de tamanho 2, a primeira posição corresponde ao id da Loja e a última ao nome da respetiva loja.
     */
    public Collection<String[]> getLojas() {
        Collection<ILoja> lojas = this.model.getLojas();
        Collection<String[]> ret = new ArrayList<>();
        for(ILoja l : lojas) {
            String[] r = new String[2];
            r[0] = l.getId();
            r[1] = l.getNome();
            ret.add(r);
        }
        return ret;
    }

    /**
     * Metodo que devolve, ao Cliente, uma lista dos produtos disponiveis na loja selecionada.
     * @param idLoja -ID da loja
     */
    public Collection<IProduto> getLojaProds(String idLoja) throws EntidadeNaoExisteException {
        Collection<IProduto> col;
        ILoja loja = this.model.getLoja(idLoja);
        if(loja != null)
            col = loja.getListProds();
        else throw new EntidadeNaoExisteException("Loja Inválida");
        return col;
    }

    /**
     * Método que cria uma linha de Encomenda.
     */
    public LinhaDeEncomenda criaLinhaEncomenda(IProduto p, int quant) {
        String cod = p.getCodigo();
        String nome = p.getNome();
        float preco = p.getPreco();
        double peso = p.getPeso();
        boolean med = p.getMedicinal();
        Produto prod = new Produto(nome, cod, peso, preco, med);
        return new LinhaDeEncomenda(prod, quant);
    }

    /**
     * Metodo que cria uma encomenda.
     */
    public IEncomenda criaEncomenda(String idLoja, Collection<LinhaDeEncomenda> l) {
        String id = "e" + Encomenda.getLastnumber();
        Encomenda.incLastnumber();
        float peso = (float) l.stream().mapToDouble(x -> x.getProduto().getPeso()).sum();
        IEncomenda e = new Encomenda(id, idLoja, "", this.utilizador.getId(), peso, l);
        ILoja loja = this.model.getLoja(idLoja);
        loja.adicionaEnc(e);
        this.utilizador.adicionaEnc(e);
        return e;
    }



    /**
     * Método que devolve a lista de avisos deste Utilizador.
     */
    public Collection<IAviso> getAvisos() {
        return this.utilizador.getNotificacoes();
    }

    /**
     * Método que remove uma notificação da lista de notificações deste utilizador.
     */
    public void removeNotificacao(IAviso a) {
        this.utilizador.removeNotificacao(a);
    }

    /**
     * Método que aceita ou recusa uma proposta feita por uma Empresa a este Utilizador.
     */
    public void trataEncomendaEmpresa(IAviso a, boolean aceitou) {
        AvisoOrcamentoRecebido a1 = (AvisoOrcamentoRecebido) a;
        String idEmpresa = a1.getIdEmpresa();
        IEmpresa emp = this.model.getEmpresa(idEmpresa);
        IAviso a2;
        if(aceitou) {
            a2 = new AvisoEmpresa(true, a1.getIdEncomenda(), a1.getIdEmpresa());
            IEncomenda e = this.model.getEncomenda(((AvisoOrcamentoRecebido) a).getIdEncomenda());
            this.model.removeEncomenda(e);
            e.setIdTransportador(idEmpresa);
            emp.addEncomendaPorEntregar(e);
        } else {
            a2 = new AvisoEmpresa(false, a1.getIdEncomenda(), a1.getIdEmpresa());
        }
        emp.addNotificacao(a2);
    }

    /**
     * Método que remove esta conta da app.
     */
    public void removeConta() {
        this.model.remCliente(this.utilizador);
    }

    public void avalia(String id, int i) {
        char first = id.charAt(0);
        if(first == 'v') {
            this.model.getVoluntario(id).insereClassificacao(i);
        } else {
            this.model.getEmpresa(id).insereClassificacao(i);
        }
    }
}