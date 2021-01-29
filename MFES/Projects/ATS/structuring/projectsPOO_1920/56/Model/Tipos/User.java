package Model.Tipos;

import Model.Encomendas.IEntrega;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class User extends Tipo implements IUser, Serializable {

    //Variávieis para as Apps
    private List<IEntrega> historico;

    /**
     * Construtor por omissão
     */
    public User() {
        super();
        this.historico = new ArrayList<>();
    }

    /**
     * Construtor por cópia
     */
    public User(User user) {
        super(user);
        this.historico = user.getHistorico();
    }

    /**
     * Construtor por parametro
     */
    public User(String id, String nome, float x, float y, List<IEntrega> e){
        super(id,nome,x,y);
        this.historico = e;
    }

    public List<IEntrega> getHistorico() {
        return historico;
    }

    public void setHistorico(List<IEntrega> historico) {
        this.historico = historico;
    }

    public User clone(){
        return new User(this);
    }


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof User)) return false;
        if (!super.equals(o)) return false;
        User user = (User) o;
        return Objects.equals(getHistorico(), user.getHistorico());
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), getHistorico());
    }

    @Override
    public String toString() {
        return "User{" +
                "historico=" + historico +
                '}';
    }
}