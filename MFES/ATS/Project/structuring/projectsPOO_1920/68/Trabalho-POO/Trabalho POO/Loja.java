import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

public class Loja extends AppUser implements Serializable {
    private String CodLoja;
    private String NomeLoja;
    private Coordenadas coordenadas;
    private int tempo_medio_de_espera; // em minutos
    private boolean fila_de_espera;
    private int pessoas_na_fila;
    private int numero_encomendas;
    private static List<LinhaEncomenda> produtos = initProdutos();

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Construtor por omissao de objetos da classe Loja
     */
    public Loja() {
       this.CodLoja = "";
       this.NomeLoja = "";
       this.coordenadas.setLongitude(0.0); 

       this.coordenadas.setLatitude(0.0);

       this.tempo_medio_de_espera = 0; //está em minutos
       this.fila_de_espera = false;
       this.pessoas_na_fila = 0;

       this.numero_encomendas = 0;
    }

    /**
     * Construtor parametrizado de objetos da classe Loja
     */

    public Loja(String cod, String nome, Coordenadas gps) {
        Random random = new Random();

        this.CodLoja= cod;
        this.NomeLoja= nome;
        this.coordenadas=new Coordenadas(gps);

        this.tempo_medio_de_espera = 2 + random.nextInt(7); // o tempo medio de espera pode ser entre 2 a 8 min de espera
        this.fila_de_espera = ThreadLocalRandom.current().nextBoolean();
        this.pessoas_na_fila = this.numero_encomendas + random.nextInt(6);;
    }

    /**
     * construtor de copia de objetos da classe Loja
     */
    public Loja(Loja l) {
       this.CodLoja = l.getCodLoja();
       this.NomeLoja = l.getNomeLoja();
       this.coordenadas= new Coordenadas(l.getCoordenadas());


       this.tempo_medio_de_espera = l.getTempoMedioDeEspera();
       this.fila_de_espera = l.isFilaDeEspera();
       this.pessoas_na_fila = l.getPessoasNaFila();
       this.numero_encomendas = l.getNumeroEncomendas();
    }

    /**
     * Inicializador dos produtos da classe Loja
     */
    private static List<LinhaEncomenda> initProdutos(){
        List<LinhaEncomenda> list = new ArrayList<>();
        LinhaEncomenda l1 = new LinhaEncomenda("p34","Farinha de trigo",0.0,5.5159483);
        list.add(l1);
        LinhaEncomenda l2 = new LinhaEncomenda("p40","Molho de pimenta",0.0,42.014664);
        list.add(l2);
        LinhaEncomenda l3 = new LinhaEncomenda("p4","Detergente",0.0,8.621707);
        list.add(l3);
        LinhaEncomenda l4  = new LinhaEncomenda("p60","Banana",0.0,9.522067);
        list.add(l4);
        LinhaEncomenda l5  = new LinhaEncomenda("p49","Leite condensado",0.0,0.8051496);
        list.add(l5);
        LinhaEncomenda l6  = new LinhaEncomenda("p21","Leite integral",0.0,20.61575);
        list.add(l6);
        LinhaEncomenda l7  = new LinhaEncomenda("p19", "Sumo garrafa 1l",0.0,43.476116);
        list.add(l7);
        LinhaEncomenda l8  = new LinhaEncomenda("p53","Batata",0.0,38.945602);
        list.add(l8);
        LinhaEncomenda l9  = new LinhaEncomenda("p48","Creme de leite",0.0,17.503843);
        list.add(l9);
        LinhaEncomenda l10  = new LinhaEncomenda("p11","Desinfetante",0.0,2.1354694);
        list.add(l10);
        LinhaEncomenda l11  = new LinhaEncomenda("p74","Iogurte",0.0,37.379745);
        list.add(l11);
        LinhaEncomenda l12  = new LinhaEncomenda("p10","Condicionador",0.0,42.83252);
        list.add(l12);
        LinhaEncomenda l13  = new LinhaEncomenda("p69","Queijo",0.0,40.992794);
        list.add(l13);
        LinhaEncomenda l14  = new LinhaEncomenda("p9","Shampoo",0.0,39.237083);
        list.add(l14);
        LinhaEncomenda l15  = new LinhaEncomenda("p38","Atum",0.0,19.018002);
        list.add(l15);
        LinhaEncomenda l16  = new LinhaEncomenda("p31","Bolacha",0.0,21.35203);
        list.add(l16);
        LinhaEncomenda l17  = new LinhaEncomenda("p24","Feijao 2kg",0.0,35.71549);
        list.add(l17);
        LinhaEncomenda l18  = new LinhaEncomenda("p73","Margarina",0.0,0.61904246);
        list.add(l18);
        LinhaEncomenda l19  = new LinhaEncomenda("p80","Salsicha",0.0,0.72794086);
        list.add(l19);
        LinhaEncomenda l20  = new LinhaEncomenda("p65","Melancia",0.0,45.64743);
        list.add(l20);
        LinhaEncomenda l21  = new LinhaEncomenda("p36","Farinha de mandioca",0.0,36.834995);
        list.add(l21);
        LinhaEncomenda l22  = new LinhaEncomenda("p13","Tira manchas",0.0,17.863035);
        list.add(l22);
        LinhaEncomenda l23  = new LinhaEncomenda("p29","Acucar",0.0,2.3291283);
        list.add(l23);
        LinhaEncomenda l24  = new LinhaEncomenda("p15","Alcool",0.0,17.826002);
        list.add(l24);
        LinhaEncomenda l25  = new LinhaEncomenda("p6","Esponja de aco",0.0,9.320234);
        list.add(l25);
        LinhaEncomenda l26  = new LinhaEncomenda("p17","Benuron",0.0,2.172213);
        list.add(l26);
        LinhaEncomenda l27  = new LinhaEncomenda("p7","Migretil",0.0,5.282433);
        list.add(l27);
        return list;
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    /**
     * Getters e Setters
     */
    public String getCodLoja()
    {
        return this.CodLoja;
    }

    public void setCodLoja(String cod)
    {
        this.CodLoja = cod;
    }

    public String getNomeLoja()
    {
        return this.NomeLoja;
    }

    public void setNomeLoja(String nome)
    {
        this.NomeLoja = nome;
    }

     public Coordenadas getCoordenadas()
    {
        return this.coordenadas; // fazer clone??
    }

    public void setCoordenadas(Coordenadas coord)
    {
        this.coordenadas = new Coordenadas(coord);
    }

    public int getTempoMedioDeEspera() {
        return tempo_medio_de_espera;
    }

    public void setTempoMedioDeEspera(int tempo_medio_de_espera) {
        this.tempo_medio_de_espera = tempo_medio_de_espera;
    }

    public boolean isFilaDeEspera() {
        return fila_de_espera;
    }


    public void setFilaDeEspera(boolean fila_de_espera) {
        this.fila_de_espera = fila_de_espera;

    }

    public int getPessoasNaFila() { // fazer este exceptionthrows LojaSemInformacaoFila??
        return pessoas_na_fila;
    }

    public void setPessoasNaFila(int pessoas_na_fila) {
        this.pessoas_na_fila = pessoas_na_fila;
    }

    public int getNumeroEncomendas() {
      return this.numero_encomendas;
    }

    public void setNumero_encomendas(int enc) {
        this.numero_encomendas = enc;
    }


    public  static List<LinhaEncomenda> getProdutos(){
        return produtos.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }

    public void setProdutos(List<LinhaEncomenda> newStock){
        produtos.clear();
        produtos = newStock.stream().map(LinhaEncomenda::clone).collect(Collectors.toList());
    }

    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


    public String toString() {
        StringBuilder sb = new StringBuilder();

        sb.append("CodLoja: ").append(this.CodLoja).append("\n");
        sb.append("NomeLoja: ").append(this.NomeLoja).append("\n");
        sb.append("Longitude: ").append(this.coordenadas.getLongitude()).append("\n");
        sb.append("Latitude: ").append(this.coordenadas.getLatitude()).append("\n");

        return sb.toString();
    }

    public Loja clone()
    {
        return new Loja(this);
    }

    public boolean equals(Object o)
    {
        if (o==this) return true;
        if (o==null || (o.getClass().equals(this.getClass())) == false) return false;
        Loja loja = (Loja)o;

        return loja.getCodLoja().equals(this.CodLoja) &&
               loja.getNomeLoja().equals(this.NomeLoja) &&
               loja.getCoordenadas().getLongitude() == this.coordenadas.getLongitude() &&
               loja.getCoordenadas().getLatitude() == this.coordenadas.getLatitude() &&

               loja.getTempoMedioDeEspera() == this.tempo_medio_de_espera &&
               loja.isFilaDeEspera() == this.fila_de_espera &&
                loja.getPessoasNaFila() == this.pessoas_na_fila &&
               loja.getNumeroEncomendas()==this.numero_encomendas;
    }


    ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    /**
     * Verifica se a Loja tem produtos com o código dado
     * @param codProduto código do produto a verificar
     */
    public static boolean temProduto(String codProduto){
        return produtos.stream().map(l->l.getCodProd()).anyMatch(c-> c.equals(codProduto));
    }



    /**
     * Devolve a LinhaDeEncomenda dos produtos da loja com determinado codigo de produto
     * @param codProd código do produto a ir buscar
     * @ret LinhaDeEncomenda
     */
    //ATENÇÃO: usar apenas se tivermos CERTEZA que produto existe na loja
    public static LinhaEncomenda getLinhaEncomenda(String codProd){
        LinhaEncomenda novaLinha = new LinhaEncomenda();
        for(LinhaEncomenda l: produtos){
            if(l.getCodProd().equals(codProd)){
                return new LinhaEncomenda(l);
            }
        }
        return novaLinha;
    }


    //public void novaEncomenda(List<Encomenda> encs, String cliente, ) {}

    public int tempoNaFila() throws LojaSemInformacaoFila {
        if (!this.fila_de_espera) throw new LojaSemInformacaoFila();
        return this.tempo_medio_de_espera * this.pessoas_na_fila;
    }

}
