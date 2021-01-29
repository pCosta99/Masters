import java.io.Serializable;

public class LogUtilizador extends Logger implements Serializable {
    private Utilizador dadosUtilizador;


    public LogUtilizador() {
        super();
        this.dadosUtilizador = new Utilizador();
    }

    public LogUtilizador(String email, String password, Utilizador dadosUtilizador) {
        super(email,password);
        this.dadosUtilizador = new Utilizador(dadosUtilizador);
    }

    public LogUtilizador(LogUtilizador a){
        super(a);
        this.dadosUtilizador = new Utilizador(a.getDadosUtilizador());
    }


    public Utilizador getDadosUtilizador() {
        return dadosUtilizador.clone();
    }

    public void setDadosUtilizador(Utilizador dadosUtilizador) {
        this.dadosUtilizador = new Utilizador(dadosUtilizador);
    }





    public LogUtilizador clone(){
        return new LogUtilizador(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
                .append(this.dadosUtilizador.toString());
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LogUtilizador a = (LogUtilizador) o;
        return super.equals(a)
                && this.dadosUtilizador.equals(a.getDadosUtilizador());
    }
    }
