import java.io.Serializable;

public class LogEmpresa extends Logger implements Serializable{
    private Transportadora dadosEmpresa;
    private boolean disponibilidade;

    public LogEmpresa() {
        super();
        this.dadosEmpresa = new Transportadora();
        this.disponibilidade = false;
    }

    public LogEmpresa(String email, String password, Transportadora dadosEmpresa, boolean disponibilidade) {
        super(email, password);
        this.dadosEmpresa = new Transportadora(dadosEmpresa);
        this.disponibilidade = disponibilidade;
    }

    public LogEmpresa(LogEmpresa l) {
        super(l);
        this.dadosEmpresa = new Transportadora(l.getDadosEmpresa());
        this.disponibilidade = l.isDisponibilidade();
    }

    public Transportadora getDadosEmpresa() {
        return this.dadosEmpresa.clone();
    }

    public void setDadosEmpresa(Transportadora dadosEmpresa) {
        this.dadosEmpresa = new Transportadora(dadosEmpresa);
    }

    public boolean isDisponibilidade() {
        return disponibilidade;
    }

    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    public LogEmpresa clone(){
        return new LogEmpresa(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
                .append("\nDisponibilidade:").append(this.disponibilidade)
                .append(this.dadosEmpresa.toString());
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LogEmpresa a = (LogEmpresa) o;
        return super.equals(a)
                && this.dadosEmpresa.equals(a.getDadosEmpresa())
                && this.disponibilidade==((a.isDisponibilidade()));
    }

    public void changeDisponibilidade(){
        if(this.isDisponibilidade()) this.setDisponibilidade(false);
        else this.setDisponibilidade(true);
    }
}
