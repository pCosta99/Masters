package Model;

import java.time.LocalDateTime;
import java.util.Random;

/**
 * Classe criada para gerar uma certa aleatoriedade
 */

public class Metereologia {
    private String[] estacoesAno = {
            "Inverno", "Inverno", "Inverno",
            "Primavera", "Primavera", "Primavera",
            "Verao", "Verao", "Verao",
            "Outono", "Outono",
            "Inverno"
    };

    private String getEstacoesAno() {
        return estacoesAno[LocalDateTime.now().getMonthValue()];
    }

    public double getEstacaoAtraso() {
        Random a = new Random();
        switch (getEstacoesAno()) {
            case "Verao":
                return a.nextDouble() % 0.1;

            case "Primavera":
                return a.nextDouble() % 0.3;

            case "Outono":
                return a.nextDouble() % 0.35;

            default:
                return a.nextDouble() % 0.6;
        }

    }
}


