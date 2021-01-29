
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;

public class Encomenda implements Serializable {
    private String orderCode;
    private String userCode;
    private String storeCode;
    private double weight;
    private ArrayList<LinhaEncomenda> cart;
    private LocalDateTime date;
    private boolean ready;
    
    public Encomenda (){
        this.orderCode="";
        this.userCode = "";
        this.storeCode = "";
        this.weight = 0;
        this.cart = new ArrayList <>();
        this.date = null;
        this.ready = false;
    }
    
    public Encomenda (String ocode, String ucode, String scode, double peso,LocalDateTime data, boolean pronto, ArrayList <LinhaEncomenda> carrinho)
    {
        this.orderCode = ocode;
        this.userCode = ucode;
        this.storeCode = scode;
        this.weight = peso;
        this.cart = carrinho;
        this.date=data;
        this.ready = pronto;
    }
    
    public Encomenda (Encomenda enc){
        this.orderCode=enc.getOrderCode();
        this.userCode=enc.getUserCode();
        this.storeCode=enc.getStoreCode();
        this.weight=enc.getWeight();
        this.cart=enc.getCart();
        this.date=enc.getDate();
        this.ready = enc.getReady();
    }
    
    public String getOrderCode()
    {
        return this.orderCode;
    }
    
    public String getUserCode()
    {
        return this.userCode;
    }
    
    public String getStoreCode()
    {
        return this.storeCode;
    }
    
    public double getWeight()
    {
        return this.weight;
    }
    
    public ArrayList<LinhaEncomenda> getCart()
    {
        ArrayList <LinhaEncomenda> carrinho = new ArrayList <>();
        Iterator<LinhaEncomenda> iter = carrinho.iterator();
        
        while(iter.hasNext()){
            LinhaEncomenda atual = iter.next();
            carrinho.add(atual);
        }
        
        return carrinho;
    }

    public LocalDateTime getDate(){
        return this.date;
    }

    public boolean getReady(){
        return this.ready;
    }
    
    public void setOrderCode(String ocode)
    {
        this.orderCode=ocode;
    }
    
    public void setUserCode(String ucode)
    {
        this.userCode=ucode;
    }
    
    public void setStoreCode(String scode)
    {
        this.storeCode=scode;
    }
    
    public void setWeight(double peso)
    {
        this.weight=peso;
    }
    
    public void setCart(ArrayList <LinhaEncomenda> carrinho)
    {
        this.cart=carrinho;
    }

    public void setDate(LocalDateTime data){
        this.date=data;
    }

    public void setReady(boolean pronto){
        this.ready=pronto;
    }
    
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        
        Encomenda e = (Encomenda) o;
        return(this.orderCode.equals(e.getOrderCode()));
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\nEncomenda");
        sb.append("\nCodigo da Encomenda: "+this.orderCode);
        sb.append("\nCodigo Destinatario: "+this.userCode);
        sb.append("\nCodigo da Loja: "+this.storeCode);
        sb.append("\nPeso da Encomenda: "+this.weight+"kg");
        sb.append("\n-----------------------------------");
        //sb.append("\nConteudo da Encomenda: "+this.cart);
        return sb.toString();
    }
    
    public Encomenda clone()
    {
        return new Encomenda (this);
    }
    
    public String toStringCSV(){
        StringBuilder sb = new StringBuilder();
        sb.append("Encomenda:").append(this.orderCode).append(",").append(this.userCode).append(",").append(this.storeCode).append(",").append(this.weight).append(",");
        for(LinhaEncomenda l : this.cart){
            sb.append(l.toStringCSV());
        
        }
        return sb.toString().substring(0, sb.length() - 1);
    }
}

