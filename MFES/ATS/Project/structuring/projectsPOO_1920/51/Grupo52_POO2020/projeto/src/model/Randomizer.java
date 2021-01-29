package model;

import java.io.Serializable;
import java.util.*;

public class Randomizer implements Serializable {

    private String[] tempo = {"Sol","Vento", "Muito Vento", "Chuva", "Muita Chuva"};
    private String clima;

    public Randomizer(){
        Random generator = new Random();
        this.clima = this.tempo[generator.nextInt(5)];
    }

    public String getClima(){
        return this.clima;
    }


    public void setClima(String clima){
        this.clima = clima;
    }


    public double tempoEntrega(double vkms, double dist_total){
        double tempo_entrega = vkms/dist_total * 100;
        if(this.tempo.equals("Sol")) return tempo_entrega;
        else if(this.tempo.equals("Vento")) return tempo_entrega + tempo_entrega*0.1;
        else if(this.tempo.equals("Muito Vento")) return tempo_entrega + tempo_entrega*.2;
        else if(this.tempo.equals("Chuva")) return tempo_entrega + tempo_entrega*.25;
        else return tempo_entrega + tempo_entrega*.35;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Randomizer that = (Randomizer) o;
        return this.clima.equals(that.clima);
    }

    public Randomizer clone(){
        Randomizer novo = new Randomizer();
        novo.setClima(this.getClima());
        return novo;
    }
}
