package Models;

public class EncomendaFarmaceutica extends Encomenda {
    public EncomendaFarmaceutica(String Loja, String User, String nome, double peso) {
        super(Loja, User, nome, peso);
    }

    public EncomendaFarmaceutica(Encomenda e) {
        super(e);
    }

    public EncomendaFarmaceutica clone() {
        return new EncomendaFarmaceutica(this);
    }
}
