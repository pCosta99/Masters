import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class Encomenda implements EncomendaI{
    // Variaveis de instancia
    private String codEncomenda;
    private String codUtilizador;
    private String codLoja;
    private double peso;
    private List<Linha_EncomendaI> le;

    /**
     * Construtores para objetos da classe Encomenda
     */
    public Encomenda(){
        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.le = new ArrayList<>();
    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso, List<Linha_EncomendaI> le) {
        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.le = le.stream().map(Linha_EncomendaI::clone).collect(Collectors.toList());
    }

    public Encomenda(Encomenda e){
        this.codEncomenda  = e.getCodEncomenda();
        this.codUtilizador = e.getCodUtilizador();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.le = e.getLe();
    }

    /**
     * Metodos gets e sets,
     * clone, equals e toString
     */
    public String getCodEncomenda() {
        return this.codEncomenda;
    }

    public void setCodEncomenda(String codEncomenda) {
        this.codEncomenda = codEncomenda;
    }

    public String getCodUtilizador() {
        return this.codUtilizador;
    }

    public void setCodUtilizador(String codUtilizador) {
        this.codUtilizador = codUtilizador;
    }

    public String getCodLoja() {
        return this.codLoja;
    }

    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    public double getPeso() {
        return peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public List<Linha_EncomendaI> getLe() {
        return this.le.stream()
                .map(Linha_EncomendaI::clone)
                .collect(Collectors.toList());
    }

    public void setLe(List<Linha_EncomendaI> les) {
        this.le = les.stream().
                map(Linha_EncomendaI::clone).
                collect(Collectors.toList());
    }

    public Encomenda clone(){
        return new Encomenda(this);
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda\n");
        sb.append(("Codigo da encomenda: ")).append(this.codEncomenda).append("\n");
        sb.append("Codigo utilizador: ").append(this.codUtilizador).append("\n");
        sb.append("Codigo da loja: ").append(this.codLoja).append("\n");
        sb.append("Peso: ").append(this.peso).append("\n");
        sb.append("Lista de produtos\n");

        for(Linha_EncomendaI l: this.le)
            sb.append(l.toString());

        return sb.toString();
    }

    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Encomenda le = (Encomenda) obj;
        return  le.getCodEncomenda().equals(this.codEncomenda) &&
                le.getCodUtilizador().equals(this.codUtilizador) &&
                le.getCodLoja().equals(this.codLoja) &&
                le.getPeso() ==this.peso &&
                le.getLe() == this.le;
    }



    public void criarEncomenda(String codEncomenda, String codUtilizador, String codLoja, List<String> list){
        List<Linha_EncomendaI> lencs = new ArrayList<>();
        Linha_EncomendaI lenc;
        double qnt;
        for(int i = 0; i < list.size(); i += 3){            // passa os valores da string para uma linha de encomenda
            qnt = Double.parseDouble(list.get(i+2));
            double valor = (int) (Math.random()*100*qnt);   // O valor do produto é caluculado de forma random
            lenc = new Linha_Encomenda(list.get(i), list.get(i + 1), qnt, valor/100);
            lencs.add(lenc.clone());
        }
        Encomenda e = new Encomenda(codEncomenda, codUtilizador, codLoja, Math.random()*100, lencs);
        this.setCodEncomenda(e.getCodEncomenda());
        this.setCodUtilizador(e.getCodUtilizador());
        this.setCodLoja(e.getCodLoja());
        this.setPeso(e.getPeso());
        this.setLe(e.getLe());
    }

    /**
     * Gerar Linhas de encomenda
     */
    private List<Linha_EncomendaI> gerLEnc(){
        List<Linha_EncomendaI> lenc = new ArrayList<>();
        Linha_EncomendaI le;

        String listaPr1  = ("p34,Farinha de trigo,2.2728467,5.5159483,p40,Molho de pimenta,1.6600878,42.014664,p4,Detergente,9.424281,8.621707,p60,Banana,2.2497952,9.522067,p49,Leite condensado,5.5749025,0.8051496,p21,Leite integral litro,7.543862,20.61575,p17,Saco de lixo 50l,5.0960507,44.77305,p19,Sumo garrafa 1l,6.0144415,43.476116,p53,Batata,3.306492,38.945602,p48,Creme de leite,2.444421,17.503843,p61,Ovos,1.4774193,15.0444355,p13,Tira manchas,6.2518024,25.874397,p41,Ervilha,4.591317,30.43119,p56,Beterraba,5.814199,2.5673974,p33,Farofa pronta,4.687167,1.8703786,p20,Sumo caixa 500ml,9.236412,42.498604,p16,Saco de lixo 30l,2.5450313,31.850492,");
        String listaPr2  = ("p76,Peixe,3.5041742,35.820896,p25,Macarrao,6.441414,12.5604105,p11,Desinfetante,4.3322487,9.954618,p77,Frango,8.605059,33.094822,p36,Farinha de mandioca,8.498911,14.619806,p33,Farofa pronta,2.9854317,23.440195,p59,Espinafre,7.380988,7.974126,p41,Ervilha,3.7562246,31.93044,");
        String listaPr3  = ("p34,Farinha de trigo,9.4330015,26.366282,p68,Cebola,7.620193,44.269226,p33,Farofa pronta,5.805751,16.996069,p7,Sabao em pedra,6.4082794,14.922269,p11,Desinfetante,3.6653197,2.1354694,p74,Iogurte,2.932383,37.379745,p79,Carne seca,1.6444292,22.794754,p25,Macarrao,3.8297682,23.282433,");
        String listaPr4  = ("p10,Condicionador,4.293345,42.83252,p76,Peixe,2.4648814,48.67309,p26,Extrato de tomate,3.2023249,24.705292,p69,Queijo,2.194335,40.992794,p36,Farinha de mandioca,5.509804,11.26213,p51,Alface,3.3531518,35.932766,p12,Lustra moveis,2.042992,35.91354,p72,Manteiga,1.3514535,9.90793,p22,Leite desnatado litro,6.3385053,15.118468,p21,Leite integral litro,8.156454,43.01251,p67,Salsa,3.0950258,4.195448,p11,Desinfetante,2.1602468,40.424934,p5,Agua sanitaria,2.7298756,5.834407,p9,Shampoo,6.5982757,39.237083,p33,Farofa pronta,3.2511325,10.02625,p35,Farinha de milho,8.578769,38.79504,");
        String listaPr5  = ("p38,Atum,5.5135922,19.018002,p31,Bolacha,2.4596689,21.35203,p72,Manteiga,4.4389067,1.061309,p41,Ervilha,3.6003213,30.06028,p77,Frango,2.618978,39.899994,p33,Farofa pronta,9.123546,0.29022777,p6,Esponja de aco,5.486778,9.320234,p19,Sumo garrafa 1l,3.2766204,33.87584,p74,Iogurte,3.7570014,35.695297,p24,Feijao 2kg,7.507841,35.71549,p21,Leite integral litro,3.4745572,9.707462,p42,Milho verde,7.771991,4.3852386,");
        String listaPr6  = ("p62,Uva,2.673811,6.719572,p6,Esponja de aco,6.4903765,30.066063,p52,Couve,7.260254,9.144804,p70,Queijo Mussarela,3.5899577,40.518456,p73,Margarina,2.137976,0.61904246,p24,Feijao 2kg,7.4189544,37.81148,p36,Farinha de mandioca,2.8486278,36.834995,p12,Lustra moveis,9.202424,30.999483,p69,Queijo,2.4257464,39.574135,p80,Salsicha,2.1447115,0.72794086,p44,Goiabada,1.668879,29.271198,p76,Peixe,3.1011233,23.372034,p13,Tira manchas,5.560059,17.863035,p38,Atum,9.440757,15.687467,p55,Cenoura,7.282647,15.172213,p59,Espinafre,4.3669834,6.6387744,");
        String listaPr7  = ("p63,Abacate,1.9603038,0.24796197,p31,Bolacha,4.405413,37.631218,p19,Sumo garrafa 1l,3.5922031,38.56502,p14,Limpa vidros,2.7751846,0.7447652,p9,Shampoo,1.9311718,13.039311,p65,Melancia,4.1007533,45.64743,p38,Atum,6.867813,29.236803,p28,Sal,9.335663,48.88989,");
        String listaPr8  = ("p19,Sumo garrafa 1l,5.472888,2.5973449,p29,Acucar,7.5829887,2.3291283,p67,Salsa,9.587439,44.728123,");
        String listaPr9  = ("p9,Shampoo,5.030349,0.86281204,p43,Doce de leite,9.288118,18.217018,p69,Queijo,2.833467,48.86587,p74,Iogurte,7.09538,0.96620125,p41,Ervilha,6.9419327,22.417736,p8,Sabonete,3.018824,45.574623,p55,Cenoura,9.40292,29.970015,");
        String listaPr10 = ("p10,Condicionador,4.7617254,36.898346,p41,Ervilha,8.82261,3.3189354,p71,Queijo outros,6.1653156,3.197555,p12,Lustra moveis,4.0892987,18.82121,p8,Sabonete,5.221398,13.939995,p37,Sardinha,4.479973,4.840426,p15,Alcool,5.610851,17.826002,p80,Salsicha,2.5069861,13.548766");
        String listaPr = listaPr1 + listaPr2 + listaPr3 + listaPr4 + listaPr5 + listaPr6 + listaPr7 + listaPr8 + listaPr9 + listaPr10;

        String []p;
        p = listaPr.split(",");
        for(int i = 0; i < p.length; i += 4){
            le = new Linha_Encomenda(p[i], p[i+1], Double.parseDouble(p[i+2]), Double.parseDouble(p[i+3]));
            lenc.add(le.clone());
        }
        return geraLencRandom(lenc);
    }

    /**
     * Gerar Lista de linhas de encomendas que poderao ter medicamentos
     */
    private List<Linha_EncomendaI> gerLEncMedico(){
        List<Linha_EncomendaI> lenc = new ArrayList<>();
        Linha_EncomendaI le;
        String s1 = "Nitrofurantoína Comprimidos: 100mg,Vancomicina (USO H) Pó para solução para perfusão: 500mg( cloridrato),Cicloserina Comprimidos ou cápsulas : 250 mg,Estreptomicina Solução injectável, extemporânea: 1g (sob a forma de sulfato),Etambutol Comprimidos: 400mg (de cloridrato),Etambutol + isoniazida Comprimidos: (Etambutol dicloridrato, 400mg e Isoniazida, 150mg),Etionamida Comprimidos: 250mg (de cloridrato),";
        String s2 = "Isoniazida Comprimidos: 100mg e 300mg,Isoniazida + Rifampicina Comprimidos: 150 INH / 300 RIF,Pirazinamida Cápsulas ou comprimidos: 400mg,Rifabutina Cápsulas: 150mg,Rifampicina Cápsulas: 150mg e 300mg,Rifampicina + Isoniazida + Pirazinamida Cápsulas : 60mg RIF /30mg INH / 150mg PIR,Rifampicina + Isoniazida + Pirazinamida + Etambutol Comprimidos: 150mg RIF /75mg INH / 400mg PIR/ 275mg ETAM,";
        String s3 = "Clofazimina Cápsulas: 50mg e 100mg,Dapsona Comprimidos: 100mg,Rifampicina Cápsulas: 150mg e 300mg,Talidomida Comprimidos: 100 mg,Anfotericina B Pó para solução coloidal para perfusão : 50 ( desoxicolato de sódio) - IV,Fluconazol Cápsulas: 100, 200 e 150 mg,Griseofulvina Comprimidos: 125mg e 500mg,Itraconazol Cápsulas: 100mg,Ketoconazol Comprimidos: 200mg,Nistatina Suspensão oral: 30 ml,";
        String s4 = "Atazanavir Cápsula : 150 e 200mg,Nelfinavir Comprimido : 250mg ( sob a forma de mesilato),Ritonavir Comprimido : 100mg,Efavirenze Comprimido ou cápsula : 50mg, 100mg e 600mg,Nevirapina Comprimido : 200mg,Emtricitabina Cápsula : 200mg,Estavudina Cápsula : 200 mg,Oxigénio Em recipiente apropriado,Propofol Solução injectável: 10mg/ml,Lidocaína Spray: 100mg/ml, 10%,Lidocaína e Adrenalina Solução injectável: 20mg,Flurazepam Comprimidos: 15mg e 30mg,Lorazepam Comprimidos: 1mg e 2,5mg";
        String s = s1 + s2 + s3 + s4;

        String []p;
        p = s.split(",");
        for (String value : p) {
            le = new Linha_Encomenda("p" + (int) (Math.random() * 1153), value, (int) (Math.random() * 100), (int) (Math.random() * 100));
            lenc.add(le.clone());
        }

        lenc.addAll(gerLEnc());
        return geraLencRandom(lenc);
    }

    private List<Linha_EncomendaI> geraLencRandom(List<Linha_EncomendaI> lenc){
        List<Linha_EncomendaI> res = new ArrayList<>();
        int nPr = (int) (Math.random() * 1000) % lenc.size();
        for(int i = 0; i < nPr; i++){
            int indice = (int) (Math.random() * 13346) % lenc.size()/3 ;
            res.add(lenc.get(indice).clone());
        }
        lenc.clear();
        return res;
    }

    /**
     * Metodo que gera uma encomenda Random
     */
    public void encomendaRandom(String codEnc, String codUtilizador, String codLoja, boolean medico){
        List<Linha_EncomendaI> lenc;
        if(medico) lenc = gerLEncMedico();
        else lenc = gerLEnc();

        this.setCodEncomenda(codEnc);
        this.setCodUtilizador(codUtilizador);
        this.setCodLoja(codLoja);
        this.setPeso(Math.random()*100);
        this.setLe(lenc);
    }

    /**
     * Metodo que le de uma String os dados de uma encomenda
     * e cria uma nova encomenda
     */
    public void leEnc(String cod, String[] p){
        this.setCodEncomenda(cod);
        this.setCodUtilizador(p[1]);
        this.setCodLoja(p[2]);
        this.setPeso(Double.parseDouble(p[3]));

        Linha_EncomendaI le = new Linha_Encomenda();
        ArrayList<Linha_EncomendaI> les = new ArrayList<>();
        for(int i = 4; i < p.length; i += 4){
            le.setCodProduto(p[i]);
            le.setDesc(p[i+1]);
            le.setQnt(Double.parseDouble(p[i+2]));
            le.setValor(Double.parseDouble(p[i+3]));
            les.add(le);
            le = new Linha_Encomenda();
        }
        this.setLe(les);
        les.clear();
    }
}
