package Projeto;

import java.util.ArrayList;

public class EncomendasAceites {
    private ArrayList<Encomenda> encAceites;

    public EncomendasAceites(ArrayList<Encomenda> encAceites){
        this.encAceites = encAceites;
    }

    public EncomendasAceites(){
        this.encAceites = new ArrayList<Encomenda>();
    }

    public void addEncomendaAceite(Encomenda cod){
        this.encAceites.add(cod);
    }

    @Override
    public String toString() {
        return "EncomendasAceites{" +
                "encAceites=" + encAceites +
                '}';
    }
}
