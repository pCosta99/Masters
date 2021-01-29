package Model.Encomendas;

import Model.Catalogos.ICatalogoProds;
import Model.Catalogos.IProduto;
import Model.Catalogos.Produto;

import java.io.Serializable;
import java.time.LocalTime;
import java.util.ArrayList;

public class Encomenda implements IEncomenda, Serializable {
    //Variaveis do logs.txt
    private String encomendaID;
    private String userID;
    private String lojaID;
    private double pesoTotal;
    private ArrayList<LinhaEncomenda> prods;

    //Variaveis extra
    private boolean medicamentos;
    private boolean congelados;
    private LocalTime horaInicial;


    public Encomenda (){
        this.encomendaID = null;
        this.userID = "n/a";
        this.lojaID = "n/a";
        this.pesoTotal = 0.0;
        this.prods = new ArrayList<>();

        this.medicamentos = false;
        this.congelados = false;
        this.horaInicial = LocalTime.now();

    }

    public Encomenda (String userID, String lojaID, int pesoTotal, ArrayList<LinhaEncomenda> prods, boolean medicamentos,
                      boolean congelados){
        this.userID = userID;
        this.lojaID = lojaID;
        this.pesoTotal = pesoTotal;
        this.prods = new ArrayList<>();
        for(LinhaEncomenda enc : prods){
            this.prods.add(enc.clone());
        }

        this.medicamentos = medicamentos;
        this.congelados = congelados;
    }

    public Encomenda (Encomenda enco){
        this.userID = enco.getUserID();
        this.lojaID = enco.getLojaID();
        this.pesoTotal = enco.getPesoTotal();
        this.prods = enco.getProds();

        this.medicamentos = enco.getMedicamentos();
        this.congelados = enco.getCongelados();
    }

    //Variaveis do logs.txt
    public String getEncomendaID() {
        return encomendaID;
    }

    public String getUserID() {
        return this.userID;
    }

    public String getLojaID() {
        return this.lojaID;
    }

    public double getPesoTotal() {
        return this.pesoTotal;
    }

    public ArrayList<LinhaEncomenda> getProds() {
        ArrayList<LinhaEncomenda> newprods = new ArrayList<>();
        for(LinhaEncomenda linha : this.prods){
            newprods.add(linha.clone());
        }
        return newprods;
    }

    public void setEncomendaID(String encomendaID) {
        this.encomendaID = encomendaID;
    }

    public void setUserID(String userID) {
        this.userID = userID;
    }

    public void setLojaID(String lojaID) {
        this.lojaID = lojaID;
    }

    public void setPesoTotal(double pesoTotal) {
        this.pesoTotal = pesoTotal;
    }

    public void setProds(ArrayList<LinhaEncomenda> prods) {
        ArrayList<LinhaEncomenda> newProds = new ArrayList<>();
        for(LinhaEncomenda linha : prods){
            newProds.add(linha.clone());
        }
        this.prods = newProds;
    }

    //EXTRAS
    public boolean getMedicamentos(){
        return this.medicamentos;
    }

    public boolean getCongelados(){
        return this.congelados;
    }


    public void setMedicamentos(boolean medicamentos) {
        this.medicamentos = medicamentos;
    }

    public void setCongelados(boolean congelados) {
        this.congelados = congelados;
    }

    public LocalTime getHoraInicial() {
        return horaInicial;
    }

    public void setHoraInicial(LocalTime horaInicial) {
        this.horaInicial = horaInicial;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda encomenda = (Encomenda) o;
        return this.pesoTotal == encomenda.pesoTotal &&
                //this.medicamentos == encomenda.medicamentos &&
                //this.congelados == encomenda.congelados &&
               // this.validacao == encomenda.validacao &&
                this.userID.equals(encomenda.getUserID()) &&
                this.lojaID.equals(encomenda.getLojaID()) &&
                this.prods.equals(encomenda.getProds()) ;
                //this.horaEntrega.equals(encomenda.getHoraEntrega());
    }

    public String toString() {
        StringBuffer sb = new StringBuffer("Encomenda: ").append(this.encomendaID);
        //sb.append("\nUsername: ").append(this.userID).append(", ");
        //sb.append("\nLoja: ").append(this.lojaID).append(", ");
        //sb.append("\nPeso Total: ").append(this.pesoTotal).append(", ");
        //sb.append("\nProdutos encomendados: ").append(this.prods);
        //sb.append("\nFim\n");
        //sb.append("\nAlgum dos produtos é um medicamento?: ").append(this.medicamentos).append(", ");
        //sb.append("\nAlgum dos produtos é um congelado?").append(this.congelados).append(", ");
        //sb.append("\nA que horas o utilizador quer a encomenda?").append(this.horaEntrega).append(", ");
        //sb.append("\nA encomenda foi validada?").append(this.validacao).append("\n\n");
        return sb.toString();
    }

    /*public Encomenda clone(){
        return new Encomenda(this);
    }
    */

    public void criaEncomenda (String aux, ICatalogoProds catalogoProds){
        String [] auxiliar = aux.split(",");

        this.encomendaID = (auxiliar[0]);
        this.userID = (auxiliar[1]);
        this.lojaID = (auxiliar[2]);
        this.pesoTotal = Double.parseDouble(auxiliar[3]);

        ArrayList<LinhaEncomenda> linhas = new ArrayList<>();
        float precoUni;

        for(int i=4;auxiliar.length>i;i=i+4){
            IProduto produto = new Produto();
            LinhaEncomenda linha = new LinhaEncomenda();
            precoUni = (Float.parseFloat(auxiliar[i+3]))/(Float.parseFloat(auxiliar[i+2]));
            produto.criaProduto(auxiliar[i],auxiliar[i+1],precoUni);
            linha.insereLinhaEncomenda(produto,auxiliar[i+2],auxiliar[i+3]);
            catalogoProds.insereProd(produto);
            linhas.add(linha);
        }
        this.prods = linhas;
        //System.out.print(this.toString());   //ESTE PRINT É PARA VERMOS OS PRODUTOS DA ENCOMENDA É IMPORTANTE

    }
}
