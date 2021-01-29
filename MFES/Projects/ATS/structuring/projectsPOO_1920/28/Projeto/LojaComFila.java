import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class LojaComFila extends Lojas implements Serializable
{
    private List <Double> tempos;   //tempos de atendimento para calcular a media
    private List <Encomenda> fila;  //fila com as encomendas na fila de espera

    //Construtores
    public LojaComFila (){

        super();
        this.tempos = new ArrayList <>();
        this.fila = new ArrayList <>();
    }

    public LojaComFila (String codLoja, String nomeLoja, String password, GPS coordenadas){
        super (codLoja, nomeLoja, password, coordenadas);
        this.tempos = new ArrayList <>();
        this.fila = new ArrayList <>();
    }

    public LojaComFila (String codLoja, String nomeLoja, String password, GPS coordenadas ,  List <Double> tempos , List <Encomenda> fila){
        super (codLoja, nomeLoja, password, coordenadas);
        this.tempos = new ArrayList<>(tempos);
        setFila(fila);
    }

    public LojaComFila (LojaComFila loja) {
        super(loja);
        this.tempos = loja.getTempos();
        this.fila = loja.getFila();
    }

    //Getters e setters sempre respeitando o encapsulamento
    public List <Double> getTempos(){
        return new ArrayList<>(this.tempos);
    }

    public void setTempos (List <Double> tempos){
        this.tempos = new ArrayList<>(tempos);
    }

    public List <Encomenda> getFila(){
        return this.fila.stream().map(e -> e.clone()).collect(Collectors.toList());
    }

    public void setFila(List <Encomenda> f){
        this.fila = f.stream().map(e -> e.clone()).collect(Collectors.toList());
    }

    public LojaComFila clone(){
        return new LojaComFila(this);
    }

    /**
     * adicionar uma encomenda a fila 
     */
    public  void adicionaAFila (Encomenda e){
        this.fila.add(e.clone());
    }

    /**
     * remover uma encomenda da fila 
     */
    public  void removeDaFila (Encomenda e){
        this.fila.remove(e);
    }

    
    /**
     * tempo medio de espera (soma de todos os tempos e usar o metodo avarage)
     * se nao tiver tempos na fila iremos dar return a -1
     */

    public double tempoMedioEspera (){
        return this.tempos.stream().mapToDouble(x -> x).average().orElse(-1);
    }

    
    /**
     * adicionar um novo tempo de espera calculado pela seguinte formula:
     * (tamanho da fila X tempo medio de espera) + (tempo de atraso pelo estado da pandemia)
     */
    public void addTempoEspera (EstadoCovid e){
        this.tempos.add((this.fila.size() * this.tempoMedioEspera()) + e.getTempo());
    }

    /**
     * tempo de recolher uma certa entrega
     * calculado pela seguinte formula
     * 
     * (tempo de chegar a loja [definido nas Lojas]) + tempo de espera calculado 
     * com a formula do metodo em cima
     */
    public double tempoRecolherEntrega (Transportador t , Meteorologia tempo , EstadoCovid e){

        double tempoChegarAloja = t.tempoChegadaLoja(this, tempo);

        return tempoChegarAloja + ( this.fila.size() * this.tempoMedioEspera()) + e.getTempo();

    }

    public String toString (){
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("Tempo Medio de Atendimento: ").append(this.tempoMedioEspera()).append("\n");
        sb.append("Fila: ").append(this.fila.toString());
        return sb.toString();
    }

    public boolean equals ( LojaComFila l){
        return super.equals(l) &&
                this.fila.equals(l.getFila()) &&
                this.tempos.equals(l.getTempos());
    }



    

    
}
