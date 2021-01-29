package Model.Atores.Transportadores;

import Model.Encomenda;

import java.awt.geom.Point2D;
import java.io.Serializable;
import java.util.Map;

/**
 * Classe Transporte
 * @author grupo 115
 */


public class Voluntario extends Transporte implements Serializable
{





    public Voluntario(){
        super();

    }

    public Voluntario(String email, String referencia, String nome, String password, Point2D.Double morada, long nif, boolean disponibilidade, float raio, boolean certeficado, double classificacao, int numeroEntregas , double velocidadeMedia, double nrKms, Map<String, Encomenda> enc) {
        super(email, referencia, nome, password, morada,nif, disponibilidade,raio, certeficado,  classificacao, numeroEntregas, velocidadeMedia, nrKms,enc);

       }


    public Voluntario(Voluntario e) {
        super(e.getEmail(),e.getReferencia(),e.getNome(),e.getPassword(),e.getMorada(),e.getNif(),e.isDisponivel(),e.getRaio(),e.isCerteficado(),e.getClassificacao(),e.getNumeroEntregas(),e.getVelocidadeMedia(),e.getNumeroKms(),e.getEncomendas());

    }


    @Override
    public String toString() {
            StringBuilder sb = new StringBuilder();
            sb.append(super.toString());

            return sb.toString();
        }

    public Voluntario clone(){
        return new Voluntario(this);
    }





}
