package src.controller;

import src.exceptions.EncomendaInvalidaException;
import src.exceptions.MaximaCapacidadeEntregasException;
import src.model.*;

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;


/**
* Class que faz a gestão entre view e model do volúntario
*
*/


public class VoluntarioController {
    private String voluntarioname;
    private TrazAqui sistema;



    /*
    * Construtor parametrizado da instância do controlador do voluntário
    *
    */

    public VoluntarioController(String cod, TrazAqui sis){
        this.voluntarioname = cod;
        this.sistema = sis;
    }

    /**
    * Método que chama um método na model para sinalizar a recolha de encomendas
    * 
    */

    public void sinalizaRecolhaEncomendas(boolean state) throws MaximaCapacidadeEntregasException {
        this.sistema.sinalizaRecolhaEncomendas(this.voluntarioname,state);
    }

    /*public void requisitarEntrega(){
        Encomenda e = this.sistema.requisitarEntregaVoluntario(this.voluntarioname);
    } */

    /**
    * Chama um método na model para realizar a entrega de uma dada Entrega do voluntário
    *
    */


    public void fazEntrega()throws EncomendaInvalidaException {
        this.sistema.fazEntregaVoluntario(this.voluntarioname);
    }

    /**
    * Método invocado na model que retorna o valor de uma variável de instância
    *
    */


    public String getVoluntarioname(){
        return this.voluntarioname;
    }

    /**
    * Acessa a variável sistema na instância em que o método foi invocado
    */

    public TrazAqui getSistema() {
        return this.sistema;
    }

    /** 
    * @return a entrega ativa de um voluntario invocando um método na model
    */

    public Entrega getEntregaAtiva(){
        return this.sistema.getEntregaAtiva(this.voluntarioname);
    }

    /**
    *  
    *  @return perfil de um voluntario invocando um método na model
    */

    public Distribuidor getPerfil(){
        return this.sistema.getPerfilDistribuidor(this.voluntarioname);
    }

    /**
    * Tem como parametros inteiros relativamente à data, neste caso ano, mês e dia, invocando o método na model
    * @return o histórico de entregas a partir de uma certa data
    */


    public List<Entrega> getHistoricoEntregas(int ano1, int mes1, int dia1, int ano2, int mes2, int dia2) throws DateTimeException {
        LocalDateTime aPartirDe = LocalDateTime.of(ano1, mes1, dia1, 0, 0);
        LocalDateTime ate = LocalDateTime.of(ano2,mes2,dia2,0,0);
        return this.sistema.getHistoricoEntregas(this.voluntarioname,aPartirDe,ate);
    }

    /**
    * Devolve uma lista com o top10 de utilizadores com o maior número de entregas
    * @return o top10 utilizadores com o maior número de entregas
    */

    public List<String> getTop10UserNumEntregas(){
        return this.sistema.getTop10UserNumEntregas().stream().map(User::getUsername).collect(Collectors.toList());
    }
}
