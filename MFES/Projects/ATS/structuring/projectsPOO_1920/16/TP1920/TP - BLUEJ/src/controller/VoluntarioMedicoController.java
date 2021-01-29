package src.controller;

import src.model.TrazAqui;

/**
* Classe que faz a gestão entre a view e a model de voluntários médicos
* Chama o construtor de super
*/
  
public class VoluntarioMedicoController extends VoluntarioController{
    public VoluntarioMedicoController(String cod, TrazAqui sis){
        super(cod,sis);
    }

    /**
    * Chama um método na model para sinalizar a recolha de encomendas médicas
    * @param state novo estado na recolha de medicamentos
    */

    public void sinalizaRecolhaMedicamentos(boolean state){
        this.getSistema().setRecolhaMedicamentos(this.getVoluntarioname(),state);
    }

    /**
    * Chama um método na model para sinalizar se aceita medicamentos 
    */
    
    public boolean estouAAceitarMed(){
        return this.getSistema().estouAAceitarMed(this.getVoluntarioname());
    }
}
