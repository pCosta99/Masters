package Projeto;

public class RegistoEmpresa extends RegistoEntidade{
    private int nif;
    private int taxa;
    private double raio;


    public RegistoEmpresa(String nome, Posicao pos, int nif, int taxa, double raio) {
        super(nome, pos);
        this.nif = nif;
        this.taxa = taxa;
        this.raio = raio;
    }

    public double getRaio(){
        return this.raio;
    }

    public double getTaxa(){
        return this.taxa;
    }

    public int getNif(){
        return this.nif;
    }
}
