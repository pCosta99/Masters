package Projeto.Controllers;
import Projeto.Avisos.AvisoEncomendaEntregue;
import Projeto.Avisos.AvisoOrcamentoRecebido;
import Projeto.Interfaces.*;
import Projeto.Model.TrazAqui;
import Projeto.Util.GPS;

import java.io.IOException;
import java.util.*;

/**
 * Classe que implementa o Controller da Empresa.
 * O Controller é conhecido da View(neste caso ViewEmpresa) e conhece o model(TrazAqui).
 * É este que faz a mediação da entrada e saída, comandando a view e o model para serem alterados de forma apropriada conforme o que o usuário solicitou.
 */
public class ControllerEmpresa {
    private TrazAqui model;
    private final IEmpresa iEmpresa;
    private final boolean grava;


    /**
     * Construtor parametrizado do Controller da Empresa.
     *
     * @param model - TrazAqui é o nosso model
     */
    public ControllerEmpresa(TrazAqui model, String id, boolean grava) {
        this.model = model;
        this.iEmpresa = this.model.getEmpresa(id);
        this.grava = grava;
    }

    public void gravar() throws IOException {
        if (this.grava) this.model.gravarObj();
    }

    public TrazAqui getModel() {
        return this.model;
    }

    public IEmpresa getiEmpresa() {
        return this.iEmpresa.clone();
    }

    public void setModel(TrazAqui model) {
        this.model = model;
    }

    /**
     * Lista dos top n clientes que mais usaram uma determinada empresa
     *
     * @param n - inteiro correspondente ao tamanho da lista
     * @return lista dos n clientes
     */
    public Collection<IUtilizador> topNClientesEmpresa(int n) {
        return this.model.topNClientesEmpresa(this.iEmpresa.getId(), n);
    }

    /**
     * Total faturado por uma empresa num determinado intervalo de tempo
     *
     * @param tInicial periodo inicial
     * @param tFinal   periodo final
     * @return total faturado nesse intervalo
     */
    public float getTotalFaturadoPeriodo(int[] tInicial, int[] tFinal) {
        return this.model.getFaturacaoPeriodo(this.iEmpresa.getId(), tInicial, tFinal);
    }

    /**
     * Total faturado por uma empresa num determinado per
     *
     * @param periodo periodo em questao
     * @return total faturado nesse tempo
     */
    public float getTotalFaturadoPeriodo(int[] periodo) {
        return this.model.getFaturacaoPeriodo(this.iEmpresa.getId(), periodo);
    }

    /**
     * Lista dos id's das N empresas que mais participaram na aplicação em termos de kms percorridos
     * @param n valor para o tamanho da lista
     * @return lista de id's de empresas
     */
    public Collection<String> getTopNEmpresasDist(int n) {
        return this.model.topNEmpresasDist(n);
    }

    /**
     * Lista dos id's das N empresas que mais participaram na aplicação em termos encomendas transportadas
     * @param n valor para o tamanho da lista
     * @return lista de id's das empresas
     */
    public Collection<String> getTopNEmpresasEnc(int n) {
        return this.model.topNEmpresasMaisUsadas(n);
    }

    /*
    Getters e Setters
     */

    public double getDistTotalEmpresa() {
        return this.iEmpresa.getDist();
    }

    public Collection<IAviso> avisosEmpresa() {
        return this.iEmpresa.getNotificacoes();
    }

    /**
     * Metodo que diz se uma dada empresa consegue transportar medicamentos ou nao
     *
     * @return true se a empresa consegue transportar medicamentos e false caso contrario
     */
    public boolean getMedicEmpresa() {
        return this.iEmpresa.aceitoTransporteMedicamentos();
    }

    /**
     * Metodo que altera o nome de uma dada empresa, coso a empresa assim o pretenda.
     *
     * @param nome - novo nome da empresa
     */
    public void setNomeEmpresa(String nome) {
        this.iEmpresa.setNome(nome);
    }

    /**
     * Metodo que altera o raio de açao de uma empresa, caso esta assim o pretenda.
     *
     * @param raio - raio de açao
     */
    public void setRaioEmpresa(float raio) {
        this.iEmpresa.setRaio(raio);
    }

    /**
     * Metodo que altera a localizaçao da empresa, caso esta assim o pretenda.
     *
     * @param lat -latitude
     * @param lon - longitude
     */
    public void setLocEmpresa(float lat, float lon) {
        this.iEmpresa.setLocalizacao(new GPS(lat, lon));
    }

    /**
     * Metodo que altera a "decisao" da empresa de poder transportar produtos medicinais
     *
     * @param medic - atualizacao sobre o certificado de medicamentos
     */
    public void setMedicEmpresa(boolean medic) {
        this.iEmpresa.aceitaMedicamentos(medic);
    }

    /**
     * Metodo que altera a capacidade maxima de produtos que uma empresa pretende/consegue transportar.
     *
     * @param cap - nova capacidade definida pela empresa em questao
     */
    public void setCapMaxEmpresa(int cap) {
        this.iEmpresa.setCapMax(cap);
    }

    /**
     * Metodo que altera a taxa de uma empresa, caso ela assim o pretenda.
     *
     * @param taxa - nova taxa definida pela empresa em questao
     */
    public void setTaxaEmpresa(float taxa) {
        this.iEmpresa.setTaxa(taxa);
    }

    /**
     * Metodo que altera o nif da empresa, caso ela assim o pretenda.
     *
     * @param nif - novo nif definido pela empresa em questao
     */
    public void setNifEmpresa(String nif) {
        this.iEmpresa.setNif(nif);
    }

    public Collection<IEncomenda> encsPorTransportar() {
        return this.model.encsPorTransportarEmpresa(this.iEmpresa);
    }

    public Collection<IEncomenda> getEncomendasPT() {
        return this.iEmpresa.getEncomendasPorEntregar();
    }

    public ILoja getLoja(String id) {
        return this.model.getLoja(id);
    }

    public IUtilizador getCliente(String id) {
        return this.model.getCliente(id);
    }

    public float calculaVel(float tempo, IUtilizador u, ILoja l) {
        double dis1 = this.iEmpresa.getLocalizacao().distancia(l.getLocalizacao());
        double dis2 = l.getLocalizacao().distancia(u.getLocalizacao());
        return (float) (dis1 + dis2) / tempo;
    }

    public void insereVelControl(float velo) {
        this.iEmpresa.insereVel(velo);
    }

    public void notifyUser(IUtilizador u, IEncomenda e) {
        IAviso a = new AvisoEncomendaEntregue(e.getID(), this.iEmpresa.getId());
        u.addNotificacao(a);
    }

    public void switch1(IEncomenda e) {
        this.iEmpresa.switchEncomenda(e);
    }

    public void userRecebeOrcamento(IEncomenda e) {
        float preco = this.calculaCusto(e);
        float tempoEstimado = this.calculaTempoEntrega(e, e.getLojaID());
        IAviso a = new AvisoOrcamentoRecebido(e.getID(), this.iEmpresa.getId(), preco, tempoEstimado);
        this.model.getCliente(e.getUserID()).addNotificacao(a);
    }

    public float calculaCusto(IEncomenda e) {
        float preco = e.calculaPrecoTotal();
        return preco * this.iEmpresa.getTaxa();
    }

    /**
     * Metodo que calcula o tempo de entrega de uma encomenda para informar o cliente que a solicitou.
     */
    private float calculaTempoEntrega(IEncomenda e, String idLoja) {
        float tempAtend = 1;
        String idCliente = e.getUserID();
        ILoja loja = this.model.getLoja(idLoja);
        if(loja.getTempMed() > 0) tempAtend = loja.getTempMed();
        float velo = 1;
        if(this.iEmpresa.calculaVelMed() > 0) velo = this.iEmpresa.calculaVelMed();
        double distanciaAteLoja = this.iEmpresa.getLocalizacao().distancia(loja.getLocalizacao());
        float tempoAteLoja = (float) distanciaAteLoja/velo;
        IUtilizador cliente = this.model.getCliente(idCliente);
        double distanciaAteCliente =  this.iEmpresa.getLocalizacao().distancia(cliente.getLocalizacao());
        float tempoAteCliente = (float) distanciaAteCliente/velo;
        return tempAtend + tempoAteLoja + tempoAteCliente;
    }

    public void apagaConta() {
        this.model.remEmpresa(this.iEmpresa);
    }

    public void atualizaDist(float dis) {
        this.iEmpresa.setDist(this.iEmpresa.getDist() + dis);
    }
}