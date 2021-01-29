package src.controller;

import src.exceptions.*;
import src.model.*;

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

/**
* Classe que faz a gestão view moidel do utilizador
*
*/



public class UtilizadorController {
    private TrazAqui sistema;
    private String username;

    /**
    * Construtor parametrizado da instância do controlador do utilizador
    *
    */


    public UtilizadorController(String username,TrazAqui sistema){
        this.username = username;
        this.sistema = sistema;
    }

    /**
    * Tem como parametros inteiros relativamente à data, neste caso ano, mês e dia, invocando o método na model
    * @return o histórico de entregas a partir de uma certa data
    *
    */

    public List<Entrega> getHistoricoEntregas(int ano1, int mes1, int dia1, int ano2, int mes2, int dia2) throws DateTimeException {
        LocalDateTime aPartirDe = LocalDateTime.of(ano1, mes1, dia1, 0, 0);
        LocalDateTime ate = LocalDateTime.of(ano2,mes2,dia2,0,0);
        return this.sistema.getHistoricoEntregas(this.username,aPartirDe,ate);
    }

    /**
    * Chama um método na model, criando uma nova encomenda
    *
    */

    // Cria um pedido de encomenda
    public void criaEncomenda(String codLoja) throws LojaInexistenteException, SemLinhasDeEncomendaException {
        this.sistema.criaEncomenda(this.username,codLoja);
    }

    /**
    * Método que cria uma linha de encomenda e regista na coleção interna do utilizador, da o codigo de um produto e uma quantidade
    * 
    */


    public void addLinhaEncomenda(String codProd, double quantidade) throws ProdutoInexistenteException,ValorInvalidoException {
        Produto p = this.sistema.getProduto(codProd);
        this.sistema.addLinhaEncomenda(this.username,p,quantidade);
    }

    /**
    * Método que atualiza o pedido de encomenda na coleção do utilizador
    *
    */


    public Set<Encomenda> getPedidosEncomenda(){
        return this.sistema.getPedidosEncomenda(this.username);
    }

    /**
    * Invoca um método no sistema solicitando o transporte de uma entrega
    *
    */

    public Optional<Map<String,Double>> requisitarEntrega(String codEnc) throws EncomendaInvalidaException, SemDistribuidoresException {
        return this.sistema.requisitaEntrega(this.username,codEnc);
    }

    /*
    *  Chama um método na model 
    *
    */

    public void aceitaPropostaTransportadora(String codDistribuidor,String codEnc){
        this.sistema.transportadoraAceitaEntrega(codDistribuidor,codEnc,this.username);
    }

    /**
    * Chama um método na model, em que o utilizador vai poder classificar o distribuidor
    *
    */

    public void classificarEntrega(String codEnc,double classificacao) throws ValorInvalidoException, EncomendaInvalidaException {
        if (classificacao < 0 || classificacao > 5) {
            throw new ValorInvalidoException(classificacao);
        }
        else { this.sistema.classificaDistribuidor(codEnc,classificacao,this.username); }
    }

    /**
    * Retorna todos os produtos presentes no sitema
    * @param codEnc
    * @param classificacao 
    */



    public Collection<Produto> getProdutos(){
        return this.sistema.getProdutos().values();
    }

    /** 
    * 
    * @return perfil de um utilizador invocando um método na model
    */

    public Utilizador getPerfil(){
        return this.sistema.getPerfilUtilizador(this.username);
    }
    
    /**
    * Devolve uma lista com o top10 de Transportadoras com o maior número de entregas
    * @return o top10 transportadoras com o maior número de entregas
    *
    */

    public List<String> getTop10TransKms(){
        return this.sistema.getTop10TransKms().stream().map(User::getUsername).collect(Collectors.toList());
    }

    /**
     * Devolve uma List com todos os códigos de lojas disponíveis
     */
    public List<String> getLojasSis(){
        return this.sistema.getLojas().stream().map(Loja :: getUsername).collect(Collectors.toList());
    }
}
