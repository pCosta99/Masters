package src.controller;

import src.model.TrazAqui;

/**
* Classe que faz gestão view model da transportadora médica, invocando a função super()
*
*/


public class TransportadoraMedicaController extends TransportadoraController {
    public TransportadoraMedicaController(String cod, TrazAqui sis){
        super(cod,sis);

    /**
    * Chama um método na model para sinalizar a recolha de encomendas médicas
    * @param state novo estado na recolha de medicamentos
    */


    }
    public void sinalizaRecolhaMedicamentos(boolean state){
        this.getSistema().setRecolhaMedicamentos(this.getTransportadoraname(),state);
    }

    /**
    * Chama um método na model para sinalizar se aceita medicamentos
    */

    public boolean estouAAceitarMed(){
        return this.getSistema().estouAAceitarMed(this.getTransportadoraname());
    }

    /*
    * Chama um método na model para sinalizar se está disponível para recolher encomendas
    */
    public boolean estouARecolher(){
        return this.getSistema().estouARecolher(this.getTransportadoraname());
    }
}
