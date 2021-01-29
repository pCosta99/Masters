package trazaqui;

import java.io.Serializable;

public class LogUtilizador extends Utilizador implements Serializable {
    private String username;
    private String password;

    //getters
    public String getUsername(){return this.username;}

    public String getPassword(){return this.password;}

    //setters
    public void setUsername(String user){this.username=user;}

    public void setPassword(String pass){this.password=pass;}

    //construtor por omissão
    public LogUtilizador(){
        super();
        this.username="";
        this.password="";
    }

    //construtor parametrizado
    public LogUtilizador(String cod,String nome, Localizacao pos, String user, String pass){
        super(cod,nome,pos);
        this.username=user;
        this.password=pass;
    }

    public LogUtilizador(Utilizador u, String user, String pass){
        super(u.getCodUtilizador(),u.getNome(),u.getGps());
        this.username=user;
        this.password=pass;
    }

    //construtor por cópia
    public LogUtilizador(LogUtilizador log){
        super(log.getCodUtilizador(),log.getNome(),log.getGps());
        this.username=log.getUsername();
        this.password=log.getPassword();
    }

    //metodo toString
    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Username:").append(this.username).append("\n");
        sb.append("Password:").append(this.password).append("\n");
        return sb.toString();
    }

    //metodo clone
    public LogUtilizador clone(){return new LogUtilizador(this);}

    //metodo equals
    public boolean equals(Object o){
        if (this==o) return true;
        if ( (o==null) || (o.getClass() != this.getClass())) return false;
        LogUtilizador user= (LogUtilizador) o;

        return (super.equals(user))
                && user.getUsername().equals(this.getUsername())
                && user.getPassword().equals(this.getPassword());
    }
}
