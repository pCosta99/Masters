package Projeto;

//É possível desde já perspectivar que existam empresas detransporte
// que aceitem apenas uma encomenda e outras que tenham capacidade para
// transportarNencomendas e tenham a noção de rota.



public class Empresa extends Utilizador{
    int nif;
    private double raio;
    private double taxa;

    public Empresa(Posicao pos, int nif, double raio, double taxa) {
        super(pos);
        this.nif = nif;
        this.raio = raio;
        this.taxa = taxa;
    }


    public Empresa(String email, String nome, String password, Posicao pos, int nif, double raio, double taxa) {
        super(email,nome,password,pos);
        this.nif = nif;
        this.raio = raio;
        this.taxa = taxa;
    }

    public Empresa(Empresa arg){
        super(arg);
        this.nif = arg.getNif();
        this.raio = arg.getRaio();
        this.taxa = arg.getTaxa();
    }

    public double getRaio() {
        return raio;
    }

    public double getTaxa() {
        return taxa;
    }

    public int getNif(){
        return this.nif;
    }

    public Empresa clone(Empresa arg){
        return new Empresa(arg);
    }

    @Override
    public String toString() {
        return "Empresa{" +
                "user=" + getEmail() + '\'' +
                "nome=" + getNome() + '\'' +
                "pos=" + getPos() + '\'' +
                "nif=" + nif +
                ", raio=" + raio +
                ", taxa=" + taxa +
                '}';
    }


    //public double custo(){}
    // O custo será calculado em função do peso
    // da encomenda e da distância à loja e depois à casa do utilizador,
    // bem como da taxa que cadaempresa de transportes cobre por km.


}
