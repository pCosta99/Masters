import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Uma classe encomenda onde contem todos os dados 
 * respetivos a uma encomenda
 */

public class Encomenda implements Serializable
{
    private String codEncomenda;   //codigo da encomenda
    private String codUtilizador;  //codigo do Utilizador que fez a encomenda
    private String codLoja;        //codigo da loja onde foi feita a encomenda
    private double peso;           //peso total dos artigos
    private LocalDate data;        //data de quando foi encomendada a loja
    private List <LinhaEncomenda> linhas;     //linhas de encomenda, ou seja os varios produtos da mesma, 
                                              //junto com as suas propriedades
    private boolean eMedica;        //um booleano que diz se a encomenda tem produtos medicos
                                    // true -> tem produtos medicos / false -> nao contem produtos medicos



    public Encomenda() {

        this.codEncomenda = "";
        this.codUtilizador = "";
        this.codLoja = "";
        this.peso = 0;
        this.data = LocalDate.now();
        this.linhas = new ArrayList <> ();
        this.eMedica = false;

    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso,LocalDate data , List <LinhaEncomenda> linhas) {

        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.data = data;
        setLinhas(linhas);
        this.eMedica = false;
    }

    public Encomenda(String codEncomenda, String codUtilizador, String codLoja, double peso,LocalDate data , List <LinhaEncomenda> linhas , boolean eMedica) {

        this.codEncomenda = codEncomenda;
        this.codUtilizador = codUtilizador;
        this.codLoja = codLoja;
        this.peso = peso;
        this.data = data;
        setLinhas(linhas);
        this.eMedica = eMedica;
    }

    public Encomenda ( Encomenda e){

        this.codEncomenda = e.getCodEncomenda();
        this.codUtilizador = e.getCodUtilizador();
        this.codLoja = e.getCodLoja();
        this.peso = e.getPeso();
        this.data = e.getData();
        this.linhas = e.getLinhas();
        this.eMedica = e.getEMedica();
    }

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
        return this.peso;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public LocalDate getData() {
        return this.data;
    }

    public void setData(LocalDate data) {
        this.data = data;
    }

    public boolean getEMedica (){
        return this.eMedica;
    }

    public void setEMedica (boolean b){
        this.eMedica = b;
    }

    /**
     * Getter das linhas de encomenda
     * fazemos clone para mander o encapsulamento
    */    
    
     public List <LinhaEncomenda> getLinhas(){
        return this.linhas.stream().map(le -> le.clone()).collect(Collectors.toList());
    }

    /**
     * setter das linhas de encomenda tambem com clone
    */

    public void setLinhas (List <LinhaEncomenda> linhas){

        this.linhas = linhas.stream().map(le -> le.clone()).collect(Collectors.toList());

    }

    public Encomenda clone (){
        return new Encomenda (this);
    }

    /**
     * @return true se todos os parametros da encomenda forem iguais
    */

    public boolean equals (Object obj){

        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;

        Encomenda e = (Encomenda) obj;

        return  this.codEncomenda.equals(e.getCodEncomenda()) &&
                this.codUtilizador.equals(e.getCodUtilizador()) && 
                this.codLoja.equals(e.getCodLoja()) &&
                this.peso == e.getPeso() &&
                this.data.equals (e.getData()) &&
                this.eMedica == e.getEMedica() &&
                this.linhas.equals ( e.getLinhas());
    }

    /**
     * Usado para imprimir a encomenda para um ficheiro CSV
    */

    public String paraCSV (){

        StringBuilder sb = new StringBuilder();

        sb.append("Encomenda").append(":");
        sb.append(this.codEncomenda).append(",");
        sb.append(this.codUtilizador).append(",");
        sb.append(this.codLoja).append(",");
        sb.append(this.peso);

        for (LinhaEncomenda le : this.linhas){
            sb.append(",").append(le.paraCSV());
        }

        return sb.toString();
    }

    /**
     * Usado para ver se esta encomenda tem uma determinada linha de encomenda
    */

    public boolean contains ( LinhaEncomenda le ) {

        return this.linhas.stream().anyMatch ( a -> a.equals (le));

    }

    public String toString (){

        StringBuilder sb = new StringBuilder();

        
        sb.append("Codigo de Encomenda : ").append(this.codEncomenda + "  \n");
        sb.append("Codigo de Utilizador: ").append(this.codUtilizador + "  \n");
        sb.append("Codigo de Loja: ").append(this.codLoja + "  \n");
        sb.append("Peso: ").append(this.peso + "  \n");
        sb.append("E medica: ").append(this.eMedica + "  \n");
        sb.append("Data: ").append(this.data + "  \n");
        sb.append("Linhas: ").append(this.linhas.toString());
        

        return sb.toString();
    }

    /**
     * calculamos o valor total da encomenda a partir da soma de todas as suas 
     * linhas de encomenda
    */

    public double valorTotalEncomenda  () {
        return this.linhas.stream().mapToDouble(le -> le.valorTotalLinhaEncomenda()).sum();
    }

    public void adicionaLinha(LinhaEncomenda linha){
        this.linhas.add (linha.clone());
    }

    /**
     * remover uma linha de encomenda a partir do seu codigo da linha (codigo do produto dessa linha)
     */

    public void removeLinha (String codLinha){
        this.linhas.removeIf( le -> le.getCodProduto() == codLinha);
    }

    
}
