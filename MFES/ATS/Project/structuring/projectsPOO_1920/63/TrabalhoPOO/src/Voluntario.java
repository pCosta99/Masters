import java.io.Serializable;
import java.util.List;
import java.util.Map;

public class Voluntario extends Pessoa implements Serializable {
    /** variaveis de instancia */
    private String codVoluntario;
    private double raio; //pelo valor do ficheo inicial isto deve ser metros!
    private boolean disponibilidade;

    /** variaveis de classe */
    private static int totalVoluntarios = 0; //necessario para ir incrementando o codigo do voluntario

    /** constructores de classe */
    /** vazio */
    public Voluntario(){
        super();
        this.codVoluntario = "";
        this.raio = 0.0;
        this.disponibilidade = true;
    }

    /** parametrico */
    public Voluntario(String newCodVoluntario, String newNome, double newGpsX, double newGpsY,
                      String newEmail, String newPassword, double newRaio,
                      boolean disponibilidade){
        super(newNome, newGpsX, newGpsY, newEmail, newPassword);
        this.codVoluntario = newCodVoluntario;
        this.raio = newRaio;
        this.disponibilidade = true;
    }

    //unico construtor que dá codVoluntario
    public Voluntario(String email, String password){
        super();
        this.setEmail(email);
        this.setPassword(password);
        Voluntario.totalVoluntarios ++;
        this.codVoluntario = "v" + Voluntario.totalVoluntarios;
        this.raio = 0.0;
        this.disponibilidade = true;
    }

    /** copia */
    public Voluntario(Voluntario newVoluntario) {
        super(newVoluntario.getNome(), newVoluntario.getGps().getX(),
                newVoluntario.getGps().getY(), newVoluntario.getEmail(),
                newVoluntario.getPassword());
        this.codVoluntario = newVoluntario.getCodVoluntario();
        this.raio = newVoluntario.getRaio();
        this.disponibilidade = newVoluntario.getDisponibilidade();
    }

    /** gets/sets das variaveis de instancia */
    public String getCodVoluntario(){ return this.codVoluntario; }
    public void setCodVoluntario(String newCodVoluntario){ this.codVoluntario = newCodVoluntario; }

    public double getRaio(){ return this.raio; }
    public void setRaio(double newRaio){ this.raio = newRaio; }

    public boolean getDisponibilidade() {
        return this.disponibilidade;
    }
    public void setDisponibilidade(boolean disponibilidade) {
        this.disponibilidade = disponibilidade;
    }

    /** metodos override */
    public boolean equals(Object o){
        if(this == o) return true;
        if(o == null || this.getClass() != o.getClass()) return false;
        Voluntario passed = (Voluntario) o;
        return (super.equals(passed) &&
                this.codVoluntario.equals(passed.getCodVoluntario()) &&
                this.raio == passed.raio &&
                this.disponibilidade == passed.getDisponibilidade());
    }

    public String toString(){
        return "Voluntario:" + this.codVoluntario + "," +
                super.toString() + "," + Double.toString(this.raio) + "," +
                this.disponibilidade;
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }

    /** metodos especificos */
    /**
     * sinalizar que estão dispostos para recolher encomendas
     */
    public void podeLevantarEncomenda(Map<String,Encomenda> encomendas){
        this.disponibilidade = true;
    }

    /**
     * Escolher buscar uma encomenda disponibilizada por uma loja
     */
    public boolean buscarEncomendaALoja(Encomenda encomenda){
        return encomenda.getLoja().getGps().distancia(this.getGps()) <= raio &&
                encomenda.getUtilizador().getGps().distancia(this.getGps()) <= raio;
    }

    /**
     * fazer o transporte da encomenda e registar quanto tempo demorou
     *
     */
    public double entregarEncomenda(Encomenda encomenda){
        double dist= this.getGps().distancia(encomenda.getLoja().getGps()) + encomenda.getLoja().getGps().distancia(encomenda.getUtilizador().getGps());

        return dist/((Math.random()*21)+ 30);
    }
}
