package src.controller;

import src.exceptions.EncomendaInvalidaException;
import src.exceptions.MaximaCapacidadeEntregasException;
import src.exceptions.ValorInvalidoException;
import src.model.*;

import java.time.DateTimeException;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
* Classe que faz gestão view model da transportadora
*/

public class TransportadoraController {
    private String transportadoraname;
    private TrazAqui sistema;

    /**
    * Construtor parametrizado da instância do controlador do voluntário
    * @param transportadoraname string com o nome da transportadora
    * @param sis 
    */ 

    public TransportadoraController(String transportadoraname, TrazAqui sis){
        this.transportadoraname =transportadoraname;
        this.sistema = sis;
    }

    /**
    * Chama um método na model para sinalizar se uma tranportadora vai fazer a entrega
    * @param state com o estado da entrega   
    */

    public void sinalizaRecolhaEncomendas(boolean state) throws MaximaCapacidadeEntregasException {
        this.sistema.sinalizaRecolhaEncomendas(this.transportadoraname,state);
    }

    /**
    * Chama um método na model para definir o tempo/distância de uma entrega
    * @param tempo de uma entrega feita pela transportadora
    * @param distancia de uma entrega feita pela transportadora
    */

    public void setPrecoTempoDistancia(double tempo, double distancia) throws ValorInvalidoException {
       this.sistema.setPrecoTempoDistancia(this.transportadoraname,tempo,distancia);
    }

    /**
    * Chama um método na model para realizar a entrega de uma dada Entrega da transportadora
    * @param codEnc string com o código de encomenda
    */

    public void fazEntregaTransportadora(String codEnc) throws EncomendaInvalidaException {
        this.sistema.fazEntregaTransportadora(this.transportadoraname,codEnc);
    }

    /**
    * Tem como parametros inteiros relativamente à data, neste caso ano, mês e dia, invocando o método na model
    * @return o histórico de entregas a partir de uma certa data  
    */

    public List<Entrega> getHistoricoEntregas(int ano1, int mes1, int dia1, int ano2, int mes2, int dia2) throws DateTimeException{
        LocalDateTime aPartirDe = LocalDateTime.of(ano1, mes1, dia1, 0, 0);
        LocalDateTime ate = LocalDateTime.of(ano2,mes2,dia2,0,0);
        return this.sistema.getHistoricoEntregas(this.transportadoraname,aPartirDe,ate);
    }

    /**
    * Método invocado na model que retorna o valor de uma variável de instância
    */

    public String getTransportadoraname(){
        return this.transportadoraname;
    }

    /**
    *  Acessa a variável sistema na instância em que o método foi invocado
    */

    public TrazAqui getSistema(){
        return this.sistema;
    }

    /**
    * @return a entrega ativa de uma transportadora invocando um método na model
    */

    public Set<Entrega> getEntregasAtivas(){
        return this.sistema.getEntregasAtivas(this.transportadoraname);
    }

    /**
    * Tem como parametros inteiros relativamente à data, neste caso ano, mês e dia, invocando o método na model
    * @return o valor faturado pelas transportadoras
    */

    public double getValorFaturadoTrans(int ano1, int mes1, int dia1, int ano2, int mes2, int dia2) throws DateTimeException {
        LocalDateTime aPartirDe = LocalDateTime.of(ano1,mes1,dia1,0,0);
        LocalDateTime ate = LocalDateTime.of(ano2,mes2,dia2,0,0);
        return this.sistema.getValorFaturadoTrans(this.transportadoraname, aPartirDe, ate);
    }

    /**
    * @return perfil da transportadora invocando um método na model
    */

    public Distribuidor getPerfil(){
        return this.sistema.getPerfilDistribuidor(this.transportadoraname);
    }

    /**
    * Devolve uma lista com o top10 de users com o maior número de entregas
    * @return o top10 de users com o maior número de entregas
    */

    public List<String> getTop10UserNumEntregas(){
        return this.sistema.getTop10UserNumEntregas().stream().map(User::getUsername).collect(Collectors.toList());
    }
}
