
/**
 * Write a description of class Encomenda here.
 *
 * @author (your name)
 * @version (a version number or a date)
 */

import java.util.ArrayList;
import java.util.List;
import java.io.Serializable;



public class Encomenda implements Serializable
{
    private static int nextCod = 1;
    
    private String codLoja;
    private String cod;
    private String codCliente;  //codigo do cliente
    private int nif;
    private String morada;
    private int num;
    private String data;
    private ArrayList<LinhaEncomenda> linhas;
    
    private double peso;
    
    /**
     * Constructor for objects of class Encomenda
     */
     
    
    public Encomenda()
    {
        this.codLoja = "n/a";
        this.codCliente = "n/a";
        this.cod = "" + Encomenda.nextCod;
        Encomenda.nextCod++;
        this.nif = -1;
        this.peso = -1;
        this.morada = "n/a";       
        this.data = "n/a";
        this.linhas = new ArrayList(); 
    }
    
    public Encomenda(String cod, String codCliente, String codLoja, double peso){
        this.codLoja = codLoja;
        this.codCliente = codCliente;
        this.cod = cod;
        this.peso = peso;
        this.nif = -1;
        this.morada = "n/a";        
        this.linhas = new ArrayList();
        this.data = "n/a";
    }
    
    public Encomenda(double peso, int nif, String codUtilizador, String codLoja, ArrayList<LinhaEncomenda> linhas){
        this.codLoja = codLoja;
        this.codCliente = codUtilizador;
        this.cod = "" + Encomenda.nextCod;
        Encomenda.nextCod++;
        this.peso = peso;
        this.nif = nif;
        this.morada = "n/a";        
        this.linhas = linhas;
        this.data = "n/a";
    }
    public Encomenda(String codCliente, String codLoja, double peso){
        this.codLoja = codLoja;
        this.codCliente = codCliente;
        this.cod = "" + Encomenda.nextCod;
        Encomenda.nextCod++;
        this.peso = peso;
        this.nif = -1;
        this.morada = "n/a";        
        this.linhas = new ArrayList();
        this.data = "n/a";
    }
    
    public Encomenda(String codCliente, String codLoja, int nif, String morada, String data)
    {
        this.codLoja = codLoja;
        this.codCliente = codCliente;
        this.cod = "" + Encomenda.nextCod;
        Encomenda.nextCod++;
        this.nif = nif;
        this.peso = -1;
        this.morada = morada; 
        this.data = data;
        this.linhas = new ArrayList(); 
    }
    
    public Encomenda (Encomenda e){
        this.codLoja = e.getCodLoja();
        this.codCliente = e.getCodCliente();
        this.cod = e.getCod();
        try {
            this.nif = e.getNif(); 
        }
        catch (ExceptionNaoDefinido n){
            this.nif = -1;
        }
        try {
            this.peso = e.getPeso();
        }
        catch (ExceptionNaoDefinido p){
            this.peso = -1;
        }
        this.morada = e.getMorada();
        this.data = e.getData();
        this.linhas = e.getLinhasEncomenda();
    }
    
    
    public String getCodLoja(){
        return this.codLoja;
    }
    
    public String getCodCliente(){
        return this.codCliente;
    }
    
    public String getCod(){
        return this.cod;
    }
    
    public int getNif() throws ExceptionNaoDefinido{
        if (this.nif==-1){
            throw new ExceptionNaoDefinido("nif");
        }
        return this.nif;
    }
    
    public String getMorada(){
        return this.morada;
    }
    public String getData(){
        return this.data;
    }
    
    public double getPeso() throws ExceptionNaoDefinido{
        if (this.peso == -1){
            throw new ExceptionNaoDefinido ("peso");
        }
        else{
            return this.peso;
        }
    }
    
    public double calculaValorTotal ()
    {
        double total = 0;
         
        if (this.linhas.size() == 0)
        {
            return total;
        }
       
        
        for (LinhaEncomenda linha: this.linhas){ 
            total += linha.calculaValorLinhaEnc();
        }
                      
        return total;
    }
    
    
    public double calculaValorDesconto()
        {
        double total = 0;
         
        if (this.linhas.size() == 0)
        {
            return total;
        }
       
        
        for (LinhaEncomenda linha: this.linhas){ 
            total += linha.calculaValorDesconto();
        }           
       
        return total;
    }
    
    public double numeroTotalProdutos()
    {
        double total = 0;
        
        if (this.linhas.size() == 0)
        {
            return total;
        }       
        
        for (LinhaEncomenda linha: this.linhas){ 
            total += linha.getQuantidade();
        }
        
        return total;
    }
    
    public boolean existeProdutoEncomenda (String ref)
    {
        if (this.linhas.size() == 0)
        {
            return false;
        }       
        
        for (LinhaEncomenda linha: this.linhas){ 
            if (linha.getReferencia().equals(ref)){
                
                return true;
            }
        }
        
        return false;
    }
    
    
    
    public void adicionaLinha (LinhaEncomenda linha)
    {
        this.linhas.add(linha);   
    }
    
    public void removeProduto (String codProd){
        
        for (int i = 0; i < this.linhas.size(); i++ )
        {
            
            if (this.linhas.get(i).getReferencia().equals(codProd) && i < this.linhas.size()-1){
                for (; i < this.linhas.size()-1; i++)                
                {
                    this.linhas.set(i, (this.linhas.get(i+1)).clone());
                }
                this.linhas.remove(linhas.size()-1);
                break;
            }
        }   
    }
    
    public String getStringNum(){
        return Integer.toString(this.num);
    }
    
    public int getSize(){
         return this.linhas.size();
    }
    
    public ArrayList<LinhaEncomenda> getLinhasEncomenda(){
        ArrayList<LinhaEncomenda> res = new ArrayList(this.linhas.size());
        
        for (LinhaEncomenda l: this.linhas){
            res.add(l.clone());
        }
        
        return res;
    }
    
    public Encomenda cloneEncomenda(){
        return new Encomenda(this);
    }
}
        
        
            
           


