package View;
import java.time.LocalDateTime;

public class IntervaloDeTempo {
    private  LocalDateTime inicio;
    private  LocalDateTime fim;

    public IntervaloDeTempo(LocalDateTime inicio, LocalDateTime fim) {
        this.inicio = inicio;
        this.fim = fim;
    }

    public LocalDateTime getInicio() {
        return inicio;
    }

    public LocalDateTime getFim() {
        return fim;
    }
}