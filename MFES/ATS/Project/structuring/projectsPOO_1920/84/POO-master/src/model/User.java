package model;

import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public abstract class User implements Serializable {

    private String id; //identificaçao de voluntario ou cliente
    private String nome; //Nome de cliente ou taxista
    private String email;//Endereço de email
    private String password;
    private String morada; // Necessario?


    //o estatuto de voluntario especial para levantar encomendas especiais vai ser posto em voluntario

    public User(){
        this.id = new String();
        this.nome = new String();
        this.email = new String();
        this.password = new String();
        this.morada = new String();


    }

    public User(String id,String nome,String email,String password,String morada){
        this.id = id;
        this.nome = nome;
        this.email = email;
        this.password = password;
        this.morada = morada;

    }


    public User(User u){
        setId(u.getId());
        setNome(u.getNome());
        setEmail(u.getEmail());
        setPassword(u.getPassword());
        setMorada(u.getMorada());

    }


    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getMorada() {
        return morada;
    }

    public void setMorada(String morada) {
        this.morada = morada;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }




    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("Id de User: ");
        sb.append(this.getId());
        sb.append("\n");
        sb.append("Nome de User: ");
        sb.append(getNome());
        sb.append("\n");
        sb.append("Email: ");
        sb.append(getEmail());
        sb.append("\n");
        sb.append("Password: ");
        sb.append(getPassword());
        sb.append("\n");
        sb.append("Morada: ");
        sb.append(getMorada());
        sb.append("\n");


        return sb.toString();
    }

    public abstract User clone();

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        User user = (User) o;
        return getId().equals(user.getId()) &&
                getNome().equals(user.getNome()) &&
                getEmail().equals(user.getEmail()) &&
                getPassword().equals(user.getPassword()) &&
                getMorada().equals(user.getMorada());
    }
    
     public abstract int numeroEncomendas();

    public abstract void addEncomendaRegisto(Encomenda e);

    public abstract void addEncomendaPending(Encomenda e);

    public abstract List<Encomenda> getRegistoEncomendasHistorico();

    public abstract List<Encomenda> getRegistoEncomendasPending();

    public abstract void removeEncomendaPending(Encomenda encomenda);

    public abstract void removeEncomendaRegisto(Encomenda encomenda);

    public abstract List<Encomenda> registoEncomendaemPeriodo(LocalDateTime inicio, LocalDateTime fim);

    /*
    @Override
    public int hashCode() { Necessário dps para criar maps
        return Objects.hash(getId(), getNome(), getEmail(), getPassword(), getCodPostal(), getMorada(), getData());
    }

    */




}
