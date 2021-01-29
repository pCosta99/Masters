import java.io.Serializable;

public class LogVoluntario extends Logger implements Serializable {
    private boolean disponibilidade;
    private Voluntario dadosVoluntario;

    public LogVoluntario() {
        super();
        this.disponibilidade = false;
        this.dadosVoluntario = new Voluntario();
    }

    public LogVoluntario(String email, String password, boolean disponibilidade, Voluntario dadosVoluntario) {
        super(email, password);
        this.disponibilidade = disponibilidade;
        this.dadosVoluntario = new Voluntario(dadosVoluntario);
    }

    public LogVoluntario(LogVoluntario l) {
        super(l);
        this.disponibilidade = l.isDisponibilidade();
        this.dadosVoluntario = new Voluntario(l.getDadosVoluntario());
    }

    public boolean isDisponibilidade() {
        return this.disponibilidade;
    }

    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    public Voluntario getDadosVoluntario() {
        return dadosVoluntario.clone();
    }

    public void setDadosVoluntario(Voluntario dadosVoluntario) {
        this.dadosVoluntario = new Voluntario(dadosVoluntario);
    }

    public LogVoluntario clone(){
        return new LogVoluntario(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
                .append("\nDisponibilidade:").append(this.disponibilidade)
                .append(this.dadosVoluntario.toString());
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LogVoluntario a = (LogVoluntario) o;
        return super.equals(a)
                && this.dadosVoluntario.equals(a.getDadosVoluntario())
                && this.disponibilidade==((a.isDisponibilidade()));
    }

    public void changeDisponibilidade(){
        if(this.isDisponibilidade()) this.setDisponibilidade(false);
        else this.setDisponibilidade(true);
    }

}
