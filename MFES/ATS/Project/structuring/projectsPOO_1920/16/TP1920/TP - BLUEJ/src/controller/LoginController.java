package src.controller;

import src.exceptions.LoginErrorException;
import src.exceptions.ValorInvalidoException;
import src.model.Ponto;
import src.model.TrazAqui;

public class LoginController {
    //variáveis de Instância
    private TrazAqui sistema;
    
    /*
     * Métodos de isntância
     */

    /**
     * Contrutor parametrizado da classe Login
     */
    public LoginController(TrazAqui t){
        this.sistema = t;
    }


    /**
     * Método que provoca o registo de um Utilizador no sistema com os devidos parâmetros para uma instância de utilizador
     * @return String com o codigo de usuário se o registo for bem sucedido 
     */
    public String registaUtilizador(String email, String password, String nome, Ponto p) throws LoginErrorException {
       return this.sistema.registaUtilizador(email,password,nome,p);
    }


    /**
     * Método que provoca o registo de uma loja no sistema com os devidos parâmetros para uma instância de loja
     * @return String com o codigo de usuário se o registo for bem sucedido 
     */
    public String registaLoja(String email, String password, String nome,Ponto localizacao) throws LoginErrorException{
        return this.sistema.registaLoja(email,password,nome,localizacao);
    }


    /**
     * Método que provoca o registo de um Voluntário no sistema com os devidos parâmetros para uma instância de Voluntario
     * @return String com o codigo de usuário se o registo for bem sucedido 
     */
    public String registaVoluntario(String email, String nome, String password, Ponto localizacao, double raio) throws LoginErrorException,ValorInvalidoException{
        return this.sistema.registaVoluntario(email,password,nome, localizacao,raio);
    }

    /**
     * Método que provoca o registo de um Voluntário médico no sistema com os devidos parâmetros para uma instância de VoluntarioMedico
     * @return String com o codigo de usuário se o registo for bem sucedido 
     */
    public String registaVoluntarioMedico(String email, String nome, String password, Ponto localizacao, double raio) throws LoginErrorException,ValorInvalidoException{
        return this.sistema.registaVoluntarioMedico(email,password,nome, localizacao,raio);
    }

    /**
     * Método que provoca o registo de uma Transportadora no sistema com os devidos parâmetros para uma instância de Transportadora
     * @return String com o codigo de usuário se o registo for bem sucedido 
     */
    public String registaTransportadora(String email, String nome, String password, Ponto localizacao, double precoPorKm,double precoPorMin, double raio) throws LoginErrorException,ValorInvalidoException{
        return this.sistema.registaTransportadora(email,password,nome,localizacao,precoPorKm, precoPorMin,raio);
    }

    /**
     * Método que provoca o registo de uma TransportadoraMedica no sistema com os devidos parâmetros para uma instância de TransportadoraMedica
     * @return String com o codigo de usuário se o registo for bem sucedido 
     */
    public String registaTransportadoraMedica(String email, String nome, String password, Ponto localizacao, double precoPorKm, double precoPorMin, double raio)throws LoginErrorException,ValorInvalidoException{
        return this.sistema.registaTransportadoraMedica(email,password,nome,localizacao,precoPorKm, precoPorMin,raio);
    }
    
    /**
     * Método que valida uma tentativa de acesso à aplicação
     * @param email String com o email da tentativa de acesso
     * @param password String com a password da tentativa de acesso
     * @return String com o codigo de usuário se a validação for bem sucedida 
     */
    public String login(String email, String password, int f) throws LoginErrorException{
        return this.sistema.valida(email,password,f);
    }

    /**
     * Método que verifica no sistema se um dado código de distribuidor é um codigo de distribuidor médico
     */
    public boolean isMedico(String codDistribuidor){
        return this.sistema.isMedico(codDistribuidor);
    }
}
