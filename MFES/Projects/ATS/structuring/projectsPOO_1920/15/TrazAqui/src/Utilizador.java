import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Utilizador implements Serializable {
    private String codUtilizador;
    private String nome;
    private double gpsx;
    private double gpsy;
    private String password;
    private List<Encomenda> user_historico;

    public Utilizador(String codUtilizador, String nome, double gpsx, double gpsy) {
        this.codUtilizador = codUtilizador;
        this.nome = nome;
        this.gpsx = gpsx;
        this.gpsy = gpsy;
        this.password = "12345";
    }

    public Utilizador(String codUtilizador, String nome, double gpsx, double gpsy, String password) {
        this.codUtilizador = codUtilizador;
        this.nome = nome;
        this.gpsx = gpsx;
        this.gpsy = gpsy;
        this.password = password;
        this.user_historico = new ArrayList<>();
    }


    public Utilizador(Utilizador u) {
        this.codUtilizador = u.getCodUtilizador();
        this.nome = u.getNome();
        this.gpsy = u.getGpsy();
        this.gpsx = u.getGpsx();
        this.password = u.getPassword();
        this.user_historico = u.getUser_historico();
    }

    public List<Encomenda> getUser_historico(){
        List<Encomenda> history = new ArrayList<>();
        if (this.user_historico == null){
            return new ArrayList<>();
        }else{
            for(Encomenda e : this.user_historico){
                history.add(e.clone());
            }
            return history;
        }
    }

    public Utilizador(){
    }

    public void setUser_historico(List<Encomenda> user_historico) {
        this.user_historico = user_historico.stream().map(Encomenda::clone).collect(Collectors.toList());
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getCodUtilizador() {
        return codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getGpsy() {
        return gpsy;
    }

    public void setGpsy(double gpsy) {
        this.gpsy = gpsy;
    }

    public double getGpsx() {
        return gpsx;
    }

    public void setGpsx(double gpsx) {
        this.gpsx = gpsx;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("Utilizador ");
        sb.append("Id: ").append(this.codUtilizador).append(", ");
        sb.append("Nome: ").append(this.nome).append(", ");
        sb.append("Latitude: ").append(this.gpsx).append(", ");
        sb.append("Longitude: ").append(this.gpsy).append(", ");
        sb.append("Password: ").append(this.password);
        return sb.toString();
    }



    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if((o == null) || o.getClass() != this.getClass()) return false;
        Utilizador p = (Utilizador) o;
        return(p.getCodUtilizador().equals(this.codUtilizador) && p.getNome().equals(this.nome) &&
                p.getGpsy() == this.gpsy && p.getGpsy() == this.gpsx && p.getPassword().equals(this.password));
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }


    public double distEntre2Pts(Utilizador u1, Utilizador u2){
        double x1 = u1.getGpsx();
        double x2 = u1.getGpsy();
        double y1 = u2.getGpsx();
        double y2 = u2.getGpsy();
        return  Math.sqrt(Math.pow(y1 - x1, 2) + Math.pow(y2 - x2, 2));
    }


    public void adicionaEnc(Encomenda enc){
        if (!this.user_historico.contains(enc)) {
            this.user_historico.add(enc.clone());
        }
    }

}
