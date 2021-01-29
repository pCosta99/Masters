package Projeto.Controllers;
import Projeto.Avisos.AvisoEncomendaAceite;
import Projeto.Avisos.AvisoEncomendaEntregue;
import Projeto.Avisos.AvisoVoluntario;
import Projeto.Exceptions.EntidadeNaoExisteException;
import Projeto.Exceptions.ListaVaziaException;
import Projeto.Interfaces.*;
import Projeto.Model.TrazAqui;
import Projeto.Util.GPS;

import java.io.IOException;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Classe que implementa o Controller do Voluntario.
 * O Controller tem como objetivo interligar as necessidades da View com as informações do model.
 * É chamado pela View, "pede" a informação do model, que a View precisa, e envia essa informação para a View.
 */

public class ControllerVoluntario {
    private final TrazAqui model;
    private final IVoluntario v;
    private final boolean grava;

    /**
     * Construtor parametrizado do Controller do Voluntario.
     */
    public ControllerVoluntario(TrazAqui model, String voluntario, boolean grava){
        this.model = model;
        this.v = this.model.getVoluntario(voluntario);
        this.grava = grava;
    }

    public void grava() throws IOException {
        if(this.grava) this.model.gravarObj();
    }

    /**
     * Metodo que altera o estado do Voluntario para livre, de acordo com a resposta que obteu na view.
     */
    public void changeEstado(){
        v.getEstado().toLivre();
    }


    /**
     * Metodo que adiciona a velocidade à lista de velocidades
     * @param velo - Velocidade a que o voluntario efetuou a entrega da encomenda
     */
    public void insereVelControl(float velo){
        IVoluntario vol = model.getVoluntario(v.getId());
        vol.insereVel(velo);
    }

    /**
     * Metodo que retorna o id do cliente, para sabermos qual a sua localizaçao, para posteriormente mudarmos a localizaçao do voluntario para esta.
     * @param idCliente - ID do cliente
     */
    public IUtilizador getClienteControl(String idCliente) {
        return this.model.getCliente(idCliente);
    }

    /**
     * Metodo que vê se, de momento, existem encomendas a ser entregues na lista de encomendas.
     */
    public boolean estaVaziaControl(){
        return v.getEncomendasPorEntregar().isEmpty();
    }

    /**
     * Metodo que calcula a velocidade a que o voluntario efetuou a entrega com base no tempo e na distancia percorrida
     * @param tempo - tempo que o voluntario demorou a efetuar a entrega da encomenda
     */
    public float calculaVelo(float tempo, IUtilizador u, ILoja l){
        double dis1 = this.v.getLocalizacao().distancia(l.getLocalizacao());
        double dis2 = l.getLocalizacao().distancia(u.getLocalizacao());
        return (float) (dis1 + dis2) / tempo;
    }

    /**
     * Metodo que altera o nome do voluntario, caso ele assim o pretenda
     */
    public void setNomeVoluntario(String nome) {
        v.setNome(nome);
    }

    /**
     * Metodo que altera o raio de açao do voluntario, caso ele assim o pretenda
     * @param raio - raio de açao
     */
    public void setRaioVoluntario(float raio) {
        v.setRaio(raio);
    }

    /**
     * Metodo que altera a localizaçoa do voluntario, caso ele assim o pretenda
     * @param lat - latitude
     * @param lon - longitude
     */
    public void setLocVoluntario(float lat, float lon) {
        v.setLocalizacao(new GPS(lat, lon));
    }

    /**
     * Metodo que altera a "decisao" do voluntario de poder transportar produtos medicinais
     */
    public void setMedicVoluntario(boolean medic) {
        v.aceitaMedicamentos(medic);
    }

    /**
     * Metodo que altera a capacidade maxima de produtos transportados que o voluntario pretende efetuar
     */
    public void setCapMaxVoluntario(int cap) {
        v.setCapMax(cap);
    }

    /**
     * Metodo que verifica se o voluntario ja tem certificado para transportar produtos medicinais
     */
    public boolean getMedicControl() {
        return v.aceitoTransporteMedicamentos();
    }

    /**
     * Método que devolve uma List de Strings em que cada String corresponde ao ID de uma encomenda feita pelo Cliente.
     * Caso o Voluntario não tenha nenhuma Encomenda no histórico "atira" a ListaVaziaException.
     */
    public Collection<String> historicoEncomendas() throws ListaVaziaException {
        Collection<IEncomenda> list = this.v.getEncomendas();
        if (list.isEmpty()) throw new ListaVaziaException("Histórico de encomendas vazio!");
        return list.stream()
                .map(IEncomenda::getID)
                .collect(Collectors.toList());
    }

    /**
     * Método que devolve a Encomenda, com o id igual ao recebido, na lista de encomendas do Cliente.
     */
    public IEncomenda getEncomenda(String idEnc) throws EntidadeNaoExisteException {
        IEncomenda e = this.v.getEncomenda(idEnc);
        if (e == null) throw new EntidadeNaoExisteException("Encomenda Inválida!");
        return e;
    }

    public Collection<IAviso> getAvisosVols() {
        return this.v.getNotificacoes();
    }

    public boolean trataAviso(AvisoVoluntario a) {
        String idEncomenda = a.getIdEncomenda();
        return (this.model.getEncomenda(idEncomenda) != null);
    }

    public void pegaEncomenda(AvisoVoluntario a) {
        String idEncomenda = a.getIdEncomenda();
        IEncomenda e = this.model.getEncomenda(idEncomenda);
        this.model.removeEncomenda(e);
        if(e != null) {
            e.setIdTransportador(this.v.getId());
            this.v.addEncomendaPorEntregar(e);
            float tempoPrevisto = this.calculaTempoEntrega(e,e.getLojaID());
            IAviso a2 = new AvisoEncomendaAceite(idEncomenda, this.v.getId(), tempoPrevisto);
            IUtilizador u = this.model.getCliente(e.getUserID());
            u.addNotificacao(a2);
        }
    }

    /**
     * Metodo que calcula o tempo de entrega de uma encomenda para informar o cliente que a solicitou.
     */
    private float calculaTempoEntrega(IEncomenda e, String idLoja) {
        float tempAtend = 1;
        String idCliente = e.getUserID();
        ILoja loja = this.model.getLoja(idLoja);
        if(loja.getTempMed() <= 0) tempAtend = loja.getTempMed();
        String id = e.getIdTransportador();
        float tempoAteLoja = 1;
        float tempoAteCliente = 1;
        if(id.charAt(0)=='v'){
            IVoluntario vol = this.model.getVoluntario(id);
            float velo = 1;
            if(!(vol.calculaVelMed() <= 0)) velo = vol.calculaVelMed();
            double distanciaAteLoja = vol.getLocalizacao().distancia(loja.getLocalizacao());
            tempoAteLoja = (float) distanciaAteLoja/velo;
            IUtilizador cliente = this.model.getCliente(idCliente);
            double distanciaAteCliente =  vol.getLocalizacao().distancia(cliente.getLocalizacao());
            tempoAteCliente = (float) distanciaAteCliente/velo;
        }
        else if(id.charAt(0) == 't'){
            IVoluntario emp = this.model.getEmpresa(id);
            float velo = 1;
            if(emp.calculaVelMed() <= 0) velo = emp.calculaVelMed();
            double distanciaAteLoja = emp.getLocalizacao().distancia(loja.getLocalizacao());
            tempoAteLoja = (float) distanciaAteLoja/velo;
            IUtilizador cliente = this.model.getCliente(idCliente);
            double distanciaAteCliente =  emp.getLocalizacao().distancia(cliente.getLocalizacao());
            tempoAteCliente = (float) distanciaAteCliente/velo;
        }
        return tempAtend + tempoAteLoja + tempoAteCliente;
    }

    public ILoja getLoja(String id) {
        return this.model.getLoja(id);
    }

    public void notifyUser(IUtilizador u, IEncomenda e) {
        IAviso a = new AvisoEncomendaEntregue(e.getID(), this.v.getId());
        u.addNotificacao(a);
    }

    public Collection<IEncomenda> getEncsPorEntregar() {
        return this.v.getEncomendasPorEntregar();
    }

    public void switch1(IEncomenda e) {
        this.v.switchEncomenda(e);
    }

    public void remNotificacao(IAviso a) {
        this.v.removeNotificacao(a);
    }

    public void apagaConta() {
        this.model.remVols(this.v);
    }
}