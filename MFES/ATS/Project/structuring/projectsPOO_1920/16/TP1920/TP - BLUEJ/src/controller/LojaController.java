package src.controller;

import src.exceptions.EncomendaInvalidaException;
import src.exceptions.ValorInvalidoException;
import src.model.Encomenda;
import src.model.Entrega;
import src.model.Loja;
import src.model.TrazAqui;

import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

/**
 * Classe do controller da loja
 */
public class LojaController {
    //variáveis de instância
    private TrazAqui sistema;
    private String lojaname;

    /**
     * Construtor parametrizado de uma instância
     */
    public LojaController(String username, TrazAqui sistema){
        this.lojaname = username;
        this.sistema = sistema;
    }


    /**
     * Atualiza na Model o valor do tempo de atendimento da instancia de loja
     * @param t double do tempo de atendimento
     */
    public void setTempoAtendimento(double t)throws ValorInvalidoException {
        this.sistema.setTempoAtendimento(this.lojaname,t);
    }

    /**
     * Atualiza na Model o valor do tempo de atendimento da instancia de loja
     * @param t double do tempo de atendimento
     */
    public Set<Encomenda> getEncomendasAceites(){
        return this.sistema.getEncomendasAceites(this.lojaname).stream().map(Entrega :: getEncomenda).collect(Collectors.toCollection(TreeSet::new));
    }

    /**
     * Atualiza o estado de uma encomenda na Model
     * @param codEnc código da encomenda a ser sinalizada como pronta
     */
    public void sinalizaEntregaPronta(String codEnc) throws EncomendaInvalidaException {
        this.sistema.sinalizaEncomendaPronta(codEnc,this.lojaname);
    }

    /**
     * Faz o registo de um produto no sistema a partir dos parâmetros de criação de um produto
     */
    public void registaProduto(String nome, double custo, double peso, boolean medicamento) throws ValorInvalidoException{
        this.sistema.registaProduto(nome,custo,peso,medicamento);
    }

    /**
     * Devolve a intância de Loja registada com o código de loja no crontroller
     */
    public Loja getPerfil(){
        return this.sistema.getPerfilLoja(this.lojaname);
    }

}
