import java.time.LocalDate;
import java.io.Serializable;
public class Entrega implements Serializable
{
    private String idEncomenda;
    private String idEntregador;
    private String idRecetor;
    private LocalDate dataEntrega;
    private double preco;
    public Entrega()
    {
        idEncomenda="n/a";
        idEntregador="n/a";
        idRecetor="n/a";
        preco=0;
        dataEntrega=LocalDate.now();
    }
    public Entrega (String idEncomenda,String idEntregador,String idRecetor,double preco)
    {
        this.idEncomenda=idEncomenda;
        this.idEntregador=idEntregador;
        this.idRecetor=idRecetor;
        this.dataEntrega=LocalDate.now();
        this.preco=preco;
    }
    public Entrega(Entrega e){
        this.idEncomenda=e.getIdEncomenda();
        this.idEntregador=e.getIdEntregador();
        this.idRecetor=e.getIdRecetor();
        this.dataEntrega=LocalDate.now();
        this.preco=e.getPreco();
    }
    public void setIdEncomenda(String id){this.idEncomenda=id;}
    public void setIdEntregador(String id){this.idEntregador=id;}
    public void setIdRecetor(String id){this.idRecetor=id;}
    public void setDataEntrega(LocalDate data){this.dataEntrega=data;}
    public void setPreco(double d){this.preco=d;}
    public String getIdEncomenda(){return this.idEncomenda;}
    public String getIdEntregador(){return this.idEntregador;}
    public String getIdRecetor(){return this.idRecetor;}
    public LocalDate getDataEntrega(){return this.dataEntrega;}
    public double getPreco(){return this.preco;}
    public boolean equals(Object o)
    {
     if (o==this) return true;
     if (o==null||o.getClass()!=this.getClass()) return false;
     Entrega ent = (Entrega) o;
     return ent.getIdEncomenda().equals(this.idEncomenda) &&
     ent.getIdEntregador().equals(this.idEntregador) &&
     ent.getIdRecetor().equals(this.idRecetor) &&
     ent.getPreco()==this.preco &&
     ent.getDataEntrega().equals(this.dataEntrega);
    }
    public Entrega clone(){return new Entrega(this);}
    public String toString(){
     StringBuilder sb=new StringBuilder();
     sb.append("Entrega:").append(this.idEncomenda);
     sb.append(",").append(this.idEntregador);
     sb.append(",").append(this.idRecetor);
     sb.append(",").append(this.dataEntrega.toString());
     sb.append(",").append(this.preco);
     sb.append("\n");
     return sb.toString();
    }
}
