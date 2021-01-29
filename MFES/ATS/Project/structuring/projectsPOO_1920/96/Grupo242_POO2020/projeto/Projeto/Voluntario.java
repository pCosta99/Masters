package Projeto;

// Quer os voluntários quer as empresas de transporte guardam
// sempre o registo das encomendasque transportaram, para que
// utilizador do serviço é que se destinavam e a loja onde a foramrecolher.

public class Voluntario extends Utilizador{
    private double raio;

    public Voluntario(String email, String nome, String password, Posicao pos, double raio) {
        super(email, nome, password, pos);
        this.raio = raio;
    }
    public Voluntario(Voluntario vol){
        super(vol);
        this.raio = vol.getRaio();
    }

    public double getRaio() {
        return raio;
    }

    public Voluntario clone() {
        return new Voluntario(this);
    }

    @Override
    public String toString() {
        return "Voluntário{" +
                "user=" + getEmail() + '\'' +
                "nome=" + getNome() + '\'' +
                "pos=" + getPos() + '\'' +
                "raio=" + raio +
                '}';
    }
}
