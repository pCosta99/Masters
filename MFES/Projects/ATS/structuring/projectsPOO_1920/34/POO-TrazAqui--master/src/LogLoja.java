import java.io.Serializable;

public class LogLoja extends Logger implements Serializable {
    private Loja dadosLoja;

    public LogLoja() {
        super();
        this.dadosLoja = new Loja();
    }

    public LogLoja(String email, String password, Loja dadosLoja) {
        super(email,password);
        this.dadosLoja = new Loja(dadosLoja);
    }

    public LogLoja(LogLoja a){
        super(a);
        this.dadosLoja = new Loja(a.getDadosLoja());
    }

    public Loja getDadosLoja() {
        return dadosLoja.clone();
    }

    public void setDadosLoja(Loja dadosLoja) {
        this.dadosLoja = new Loja(dadosLoja);
    }

    public LogLoja clone(){
        return new LogLoja(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString())
                .append(this.dadosLoja.toString());
        return sb.toString();
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        LogLoja a = (LogLoja) o;
        return super.equals(a)
                && this.dadosLoja.equals(a.getDadosLoja());
    }
}
