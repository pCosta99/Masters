package Projeto.Controllers;
import Projeto.Avisos.AvisoVoluntario;
import Projeto.Entidades.Produto;
import Projeto.Exceptions.EntidadeNaoExisteException;
import Projeto.Exceptions.IdRepetidoException;
import Projeto.Exceptions.ListaVaziaException;
import Projeto.Interfaces.*;
import Projeto.Model.TrazAqui;
import Projeto.Util.GPS;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Classe que implementa o Controller da Loja.
 * O Controller tem como objetivo interligar as necessidades da View com as informações do model.
 * É chamado pela View, "pede" a informação do model, que a View precisa, e envia essa informação para a View.
 */
public class ControllerLoja {
    private final TrazAqui model;
    private final ILoja loja;
    private final boolean gravar;

    /**
     * Construtor parametrizado do Controller da Loja.
     * @param loja - TrazAqui é o nosso model
     * @param lo - ID da loja
     */
    public ControllerLoja(TrazAqui loja, String lo, boolean gravar){
        this.model = loja;
        this.loja = this.model.getLoja(lo);
        this.gravar = gravar;
    }

    /**
     * Método que trata de gravar o estado atual do programa num ficheiro objeto.
     */
    public void gravar() throws IOException {
        if(this.gravar) this.model.gravarObj();
    }

    /**
     * Metodo que nos diz quantas pessoas estão na fila de espera de uma dada loja
     * @param size - tamanho da fila
     */
    public void qtsPessoasAtual(int size){
       model.setSizeFilaLoja(loja, size);
    }

    /**
     * Metodo que altera o nome da loja, caso esta assim o pretenda
     * @param nome - novo nome definido pela loja em questao
     */
    public void setNomeLoja( String nome) {
        loja.setNome(nome);
    }

    /**
     * Metodo que altera a localizaçao da loja em questao, caso esta assim o pretenda
     * @param lat - latitude
     * @param lon - longitude
     */
    public void setLocLoja(float lat, float lon) {
        loja.setLocalizacao(new GPS(lat, lon));
    }

    /**
     * Metodo que altera a decisao de fornecer dados sobre as filas de espera
     */
    public void setDFilaControl(boolean dados) {
        loja.setDFila(dados);
    }

    /**
     * Metodo que altera o tempo de atendimento medio de uma dada loja
     * @param tempo - novo tempo de atendimento medio
     */
    public void setTempMedControl(float tempo) {
        loja.setTempMed(tempo);
    }

    /**
     * Metodo que adiciona um Produto à lista de produtos.
     */
    public void adicionaProdutos(String nome, double peso, float preco, boolean med, int i) {
        try {
            int ls = this.model.quantasLojas();
            String codigo = "p" + ls + i;
            IProduto p = new Produto(nome, codigo, peso, preco, med);
            this.loja.addProduto(p);
        } catch (IdRepetidoException e) {
            this.adicionaProdutos(nome, peso,preco,med,i);
        }
    }

    /**
     * Metodo que remove um Produto da lista de produtos.
     */
    public void removeProdutoControl(String codigo) {
        loja.removeProduto(codigo);
    }

    /**
     * Método que devolve uma List de Strings em que cada String corresponde ao ID de uma encomenda feita pelo Cliente.
     * Caso a loja não tenha nenhuma Encomenda no histórico "atira" a ListaVaziaException.
     */
    public Collection<String> historicoEncomendas() throws ListaVaziaException {
        Collection<IEncomenda> list = this.loja.getEncomendas();
        Collection<String> ret = new ArrayList<>();
        if(!list.isEmpty()){
            for(IEncomenda e : list) {
                ret.add(e.getID());
            }
        } else throw new ListaVaziaException("Lista de encomendas vazia!");
        return ret;
    }

    /**
     * Método que devolve a Encomenda, com o id igual ao recebido, na lista de encomendas do Cliente.
     */
    public String getEncomenda(String idEnc) throws EntidadeNaoExisteException {
        String ret;
        IEncomenda e = this.loja.getEncomenda(idEnc);
        if(e != null) {
            ret = e.toString();
        }else throw new EntidadeNaoExisteException("Encomenda Inválida!");
        return ret;
    }

    /**
     * Método que apaga uma conta.
     */
    public void removeConta() {
        this.model.remLoja(this.loja);
    }

    /**
     * Método que devolve a lista dos produtos desta loja.
     */
    public Collection<IProduto> getProdutos() {
        return this.loja.getListProds();
    }

    public Collection<IEncomenda> getEncomendas() {
        return this.loja.getEncomendas();
    }

    public int prontarEnc(IEncomenda e) {
        this.loja.removeEnc(e);
        this.model.addEncomenda(e);
        GPS loja = this.loja.getLocalizacao();
        GPS user = this.model.getCliente(e.getUserID()).getLocalizacao();
        List<IVoluntario> col = (List<IVoluntario>) this.model.volsDisponiveis(loja, user, e.containsMed());
        IAviso a = new AvisoVoluntario(e.getID());
        for(IVoluntario v : col) v.addNotificacao(a);
        return col.size();
    }
}