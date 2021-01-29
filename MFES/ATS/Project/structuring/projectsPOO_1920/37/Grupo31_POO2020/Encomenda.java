
/**
 * Escreva a descrição da classe Encomenda aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.io.*;
public class Encomenda implements Serializable{
    private String codU; //codigo de utilizador
    private String codEnc; //codigo de encomenda
    private String codLoja; //codigo de loja
    private String distribuidor; //codigo do distribuidor
    private LocalDate dataEncomenda;
    private double peso;
    private List <LinhaEncomenda> linhas;
    
    public Encomenda (){
        this.codU = "";
        this.codEnc = "";
        this.codLoja = "";
        this.dataEncomenda = null;
        this.peso = 0.0;
        this.linhas =new ArrayList<>();
        this.distribuidor = "";
    }
    
    public Encomenda (String codU,String codEnc,String codLoja, LocalDate dataEncomenda, double peso,List <LinhaEncomenda> linhas,String distribuidor){
        this.codU = codU;
        this.codEnc = codEnc;
        this.codLoja = codLoja;
        this.dataEncomenda = dataEncomenda;
        this.peso = peso;
        setLinhas (linhas);
        this.distribuidor = distribuidor;
    }
    
    public Encomenda ( Encomenda encomenda){
        this.codU = encomenda.getCodU();
        this.codEnc = encomenda.getCodEnc();
        this.codLoja = encomenda.getCodLoja();
        this.dataEncomenda = encomenda.getDataEncomenda();
        this.peso = encomenda.getPeso();
        this.linhas = encomenda.getLinhas();
        this.distribuidor = encomenda.getdistribuidor();
    }
    
    
    
    /**
     * Gets
     */
    
    
    public String getCodU(){
        return this.codU;
    }
    
    public String getCodEnc(){
        return this.codEnc;
    }
    
    public String getCodLoja(){
        return this.codLoja;
    }
    
    public LocalDate getDataEncomenda(){
        return this.dataEncomenda;
    }
    
    public double getPeso(){
        return this.peso;
    }
   
    public ArrayList <LinhaEncomenda> getLinhas(){
        ArrayList <LinhaEncomenda> li =new ArrayList <>(this.linhas.size());
        for(LinhaEncomenda s :this.linhas){
            li.add(s);
        }
        return li;
    }
    
    public String getdistribuidor(){
        return this.distribuidor;
    }
   
    
    
    /**
     * Sets
     */
    public void setCodU(String codU){
        this.codU= codU;
    }
    
    public void setCodEnc( String codEnc){
        this.codEnc= codEnc;
    }
    
    public void setCodLoja(String codLoja){
        this.codLoja = codLoja;
    }
    
    public void setDataEncomenda(LocalDate dataEncomenda){
        this.dataEncomenda=dataEncomenda;
    }
    
    public void setPeso (double peso){
        this.peso=peso;
    }
    
    public void setLinhas(List<LinhaEncomenda> linhas){
        this.linhas =new ArrayList <>(linhas.size());
        for(LinhaEncomenda s :linhas){
            this.linhas.add(s);
        }
    }
    
    public void setdistribuidor(String distribuidor){
        this.distribuidor = distribuidor;
    }
    
    /**
     * Método clone()
     */
    public Encomenda clone() {
        return new Encomenda(this);
    }
    
    
    /**
     * Método equals()
     */
    public boolean equals(Object obj) {
        if(obj==this) return true;
        if(obj==null || obj.getClass() != this.getClass()) return false;
        Encomenda that = (Encomenda) obj;
        return that.getCodU().equals(this.codU) &&
               that.getCodEnc().equals(this.codEnc) && 
               that.getCodLoja().equals(this.codLoja) &&
               that.getDataEncomenda().equals(this.dataEncomenda) &&
               that.getPeso() == (this.peso) &&
               that.getdistribuidor() == (this.distribuidor) &&
               that.getLinhas().equals (this.linhas);
    }
    
    
    /**
     * Método toString()
     */
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("\nCódigo Utilizador: ").append(this.codU);
        sb.append("\nCódigo da Encomenda: ").append (this.codEnc);
        sb.append("\nCódigo da Loja: ").append (this.codLoja);
        //sb.append(dataEncomenda.toString()); // está a estourar (mal feito, ver depois)
        sb.append("\nPeso da Encomenda: ").append (this.peso);
        sb.append("\nLinhas da Encomenda: " + linhas.toString());
        sb.append("\nCódigo do Distribuidor: ").append (this.distribuidor);
        
        return sb.toString();
    } 
    
    public double calculaValorTotal(){
        int i;
        double valor=0;
        for(i=0;i<linhas.size();i++){
            valor=valor+ linhas.get(i).calculaValorLinhaEnc();
        }
        
        return valor;
    }
    
    
    public double calculaValorDesconto(){
        int i;
        double valor =0;
        for(i=0;i<linhas.size();i++){
            valor=valor+linhas.get(i).calculaValorDesconto();
        }
        return valor;
    }
    
    
    public double numeroTotalProdutos(){
        int i;
        double quantidade= 0;
        for(i=0;i<linhas.size();i++){
            quantidade=quantidade+ linhas.get(i).getQuantidade();
        }
        
        return quantidade;
    }
    
    public boolean existeProdutoEncomenda(String refProduto){
        int i;
        for(i=0;i<linhas.size();i++){
            if(linhas.get(i).getCodProduto()==refProduto) return true;
        }
        return false;
    }
    
    
    public LinhaEncomenda top(){
        LinhaEncomenda res =null;
        if(!this.empty() ){
            res= this.linhas.get(0);
        }
        return res;
    }
    
    public boolean empty (){
        return  linhas.isEmpty();
    }
    
    public void adicionaLinha(LinhaEncomenda linha){
        linhas.add(0,linha);
    }
    
    /*
    public void removeProduto(String codProd){
        //ver isto
        linhas.remove(codProd);
    }
    */
    
    public int somaQtd (){
        int res=0;
        for(LinhaEncomenda le : this.linhas){
            res+=le.getQuantidade();
        }
        return res;
    }
    
    public ArrayList<String> getCodProd(){
        ArrayList<String> res= new ArrayList<String> ();
        for(LinhaEncomenda le :this.linhas){
            res.add(le.getCodProduto());
        }
        return res;
    }
}


