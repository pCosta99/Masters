package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Conta implements Serializable, IConta{
    private static final long serialVersionUID = 124L;
    private String id;
    private String email;
    private String password;
    private List<String> notificacoes;

    public Conta(){
        this.id = this.email = this.password = "n/a";
        this.notificacoes = new ArrayList<>();
    }

    public Conta(String id, String email, String password, List<String> notificacoes){
        this.id = id;
        this.email = email;
        this.password = password;
        this.setNotificacoes(notificacoes);
    }

    public Conta(Conta c){
        this.id = c.getId();
        this.email = c.getEmail();
        this.password = c.getPassword();
        this.notificacoes = c.getNotificacoes(); 
    } 

    public void setId(String id){
        this.id = id;
    }
    public void setEmail(String email){
        this.email = email;
    }
    public void setPassword(String password){
        this.password = password;
    }
    public void setNotificacoes(List<String> notificacoes){
        this.notificacoes = new ArrayList<>();
        for(String s : notificacoes)
            this.notificacoes.add(s);
    }

    public String getId(){
        return this.id;
    }
    public String getEmail(){
        return this.email;
    }
    public String getPassword(){
        return this.password;
    }
    public List<String> getNotificacoes(){
        List<String> r = new ArrayList<>();
        for(String s : this.notificacoes)
            r.add(s);
        return r;
    }

    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Conta c = (Conta) o;
        return this.id.equals(c.getId())
            && this.email.equals(c.getEmail())
            && this.password.equals(c.getPassword())
            && this.notificacoes.equals(c.getNotificacoes());
    }

    public Conta clone(){
        return new Conta(this);
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Conta[");
        sb.append(this.id);
        sb.append("]");
        sb.append("\n\tEmail: ");
        sb.append(this.email);
        sb.append("\n\tNotificações: ");
        for(String s : this.notificacoes){
            sb.append("\n\t\t");
            sb.append(s);
        }
        return sb.toString();
    }


    public char getTipo(){
        return this.id.charAt(0);
    }

    public void addNotificacao(String message){
        this.notificacoes.add(message);
    }

    


}