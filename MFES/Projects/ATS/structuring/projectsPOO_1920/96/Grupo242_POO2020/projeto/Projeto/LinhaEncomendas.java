package Projeto;

public class LinhaEncomendas {
    String cod;
    String desc;
    double quantidade;
    double valor;

    public LinhaEncomendas(String cod, String desc, double quantidade, double valor){
        this.cod = cod;
        this.desc = desc;
        this.quantidade = quantidade;
        this.valor = valor;
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        return super.clone();
    }

    @Override
    public String toString() {
        return "Encomendas{" +
                "cod='" + cod + '\'' +
                ", desc='" + desc + '\'' +
                ", quantidade=" + quantidade +
                ", valor=" + valor +
                '}';
    }
}
