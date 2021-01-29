package Perfis;

import Auxiliares.GPS;
import Auxiliares.Viagem;
import Encomendas.Encomenda;
import Registos.Registos;

import java.io.Serializable;

public class Utilizador extends Perfil implements Serializable {


    /*
    Construtores
     */


    public Utilizador(String codigoPerfil,GPS coordenadas,String nome,String email,String pass) {
        super(codigoPerfil,coordenadas,nome,email,pass);
        }

    public Utilizador(){
        super();
    }

    public Utilizador(Utilizador other){ super(other); }



     /*
    Metódos
     */

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Utilizador: ").append(this.getCodigoPerfil()).append("\n")
                .append(" - Nome: ") . append(this.getNome()).append("\n")
                .append(" - Localização: ") . append(this.getCoordenadas()).append("\n")
                .append(" - Número de encomendas feitas: ") .append(this.getHistorico().size()).append("\n");
        return  sb.toString();
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Utilizador that = (Utilizador) o;
        return super.equals(that);
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }



    // vai buscar a transportadora que fez a ultima encomenda
    public Boolean buscarTransportadora(Registos r){
        if (!this.getHistorico().isEmpty()) {

            String codTransportadora = this.getHistorico().get((this.getHistorico().size()) - 1).getCodTransportadora();
            Transportadores t = (Transportadores) r.getPerfil(codTransportadora);

            String codEncomenda = this.getHistorico().get((this.getHistorico().size()) - 1).getCodEncomenda();

            if (t.getTrips() == null || t.getTrips().isEmpty()) {
                System.out.println("De momento não pode fazer a sua classificação, volte mais tarde\n");
            } else {
                for (Viagem v : t.getTrips()) {
                    if (v.getClassificado().equals(false) && v.getCodEncomenda().equals(codEncomenda)) {
                        return false;
                    }
                }
            }

        }
        return true;
    }


    public String toStringGuardar(){
        StringBuilder sb = new StringBuilder();
        sb.append(this.getCodigoPerfil()).append(",")
                .append(this.getNome()).append(",")
                .append(this.getCoordenadas().toStringGrava());
        return sb.toString();
    }
}
