package view;

import java.time.LocalDateTime;

public class PeriodoTempo {
    private final LocalDateTime inicio;
    private final LocalDateTime fin;


    public PeriodoTempo(LocalDateTime i, LocalDateTime f){
        this.inicio = i;
        this.fin = f;
    }

    public LocalDateTime getInicio(){
        return inicio;
    }

    public LocalDateTime getFin(){
        return fin;
    }

}
