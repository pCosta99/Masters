import java.util.ArrayList;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.io.*;


public class Encomenda implements Serializable
{
    String id;
    private Cliente cliente;
    private Loja loja;
    public double peso;
    private boolean state; // 0 se n for medico 1 se for
    private LocalDate data;
    private boolean respostaCliente; // se alguma empresa mandou pedido ou nao
    private boolean flagLojaPronta; // 0 se a encomenda ainda nao está pronta, 1 se a loja ja tiver a encomenda pronta
    private ArrayList <LinhaEncomenda> linhas;
    
    public Encomenda(){
        this.id = "";
        this.loja= new Loja();
        this.cliente = new Cliente();
        this.peso=0.0;
        this.state= false;
        this.respostaCliente=false;
        this.flagLojaPronta=false;
        this. linhas =new ArrayList<>();
    }

    public Encomenda(String idAux, Cliente clienteAux, Loja lojaAux, double pesoAux, boolean stateAux,LocalDate dataAux,boolean respostaClienteAux, boolean flagLojaProntaAux, ArrayList<LinhaEncomenda> l){
        this.id=idAux;
        this.cliente=new Cliente(clienteAux);
        this.loja=new Loja(lojaAux);
        this.peso=pesoAux;
        this.state=stateAux;
        this.data=LocalDate.now();
        this.respostaCliente=respostaClienteAux;
        this.flagLojaPronta=flagLojaProntaAux;
         setLinhas (l);
    }

    public Encomenda(Encomenda e){
        this.id=e.getId();
        this.cliente= new Cliente(e.getCliente());
        this.loja= new Loja(e.getLoja());
        this.peso=e.getPeso();
        this.state=e.getState();
        this.respostaCliente=e.getRespostaCliente();
        this.flagLojaPronta=e.getFlagLojaPronta();
        this.linhas = e.getLinhas();
    }


    //getters


    public String getId() {
        return this.id;
    }

    public Cliente getCliente() {
        return this.cliente;
    }

    public Loja getLoja() {
        return this.loja;
    }

    public double getPeso() {
        return this.peso;
    }

    public boolean getState() {
        return this.state;
    }
    
    public LocalDate getData() {
        return this.data;
    }
    
     public boolean getRespostaCliente(){
        return this.respostaCliente;
    }
    
    public boolean getFlagLojaPronta(){
        return this.flagLojaPronta;
    }
    
public ArrayList <LinhaEncomenda> getLinhas(){
        ArrayList <LinhaEncomenda> li =new ArrayList <>(this.linhas.size());
        for(LinhaEncomenda s :this.linhas){
            li.add(s.clone());
        }
        return li;
    }
  
    //setters


    public void setId(String id) {
        this.id = id;
    }

    public void setCliente(Cliente cliente) {
        this.cliente = cliente;
    }

    public void setLoja(Loja loja) {
        this.loja = loja;
    }

    public void setPeso(double peso) {
        this.peso = peso;
    }

    public void setState(boolean state) {
        this.state = state;
    }

    public void setData(LocalDate data) {
        this.data = LocalDate.now();
    }

     public void setRespostaCliente(boolean r){
        this.respostaCliente=r;
    }
    
    public void setFlagLojaPronta(boolean r){
        this.flagLojaPronta=r;
    }
    
   public void setLinhas(ArrayList<LinhaEncomenda> linhas){
         this.linhas =new ArrayList <>(linhas.size());
        for(LinhaEncomenda s :linhas){
            this.linhas.add(s);
        }
    }
    
    //equals


    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Encomenda aux = (Encomenda) o;
        return  this.id == aux.id &&
                this.peso==aux.peso &&
                this.state==aux.state &&          
                this.getCliente().equals(aux.getCliente()) &&
                this.getLoja().equals(aux.getLoja())&&
                this.getData().equals(aux.getData()) &&
                this.respostaCliente==aux.getRespostaCliente() &&
                this.flagLojaPronta==aux.getFlagLojaPronta() &&
                this.getLinhas().equals(aux.getLinhas());

    }

    @Override
    public String toString() {
        
        return "Encomenda com id " + this.id +
                this.cliente +
                this.loja + "\n" +
                "Peso:" + this.peso +
                " Data:" + this.data + "\n" +
                "Produtos:" + this.linhas.toString() + "\n" +
                "Loja ja tem a encomenda pronta? " + this.flagLojaPronta + "\n" +
                "Ja foi enviado pedido de aceite ao cliente? " + this.respostaCliente + "\n" ;

    }
    
    
     public Encomenda clone(){
       Encomenda aux= new Encomenda(this);
       aux.setData(this.data);
         return aux;
    }
    
    public double  calculapeso(){
        double peso=0;
        linhas=this.getLinhas();
        for(LinhaEncomenda s:linhas){
        peso+=s.getQuantidade();
    }
    return peso;
    }
}

