import java.util.List;
import java.io.Serializable;
import java.util.ArrayList;
public class Transportadoras extends AllUsers implements Serializable
{
    
    private String compNIF;
    private double raio;
    private double pricekm;
    private double totalKm;
    private boolean aceitamed;
    private boolean cert;
    private boolean pronto;
    private int numEnc;
    private List<Encomenda> historico;


    public Transportadoras()
    {
        super();
        this.compNIF = "";
        this.raio = 0;
        this.pricekm= 0;
        this.totalKm = 0;
        this.aceitamed=false;
        this.cert = false;
        this.numEnc = 1;
        this.historico = new ArrayList<>();
    }
    
    public Transportadoras (String codigo, String nome, Coordenadas xyr, String nif, double raio, double preco, double numKm, int numeroencomendas, boolean aceita, boolean medcert, String email, String password, boolean ready, List<Integer> aval, List<Encomenda> history)
    {
        super(codigo, nome, xyr, email, password, aval);
        this.compNIF=nif;
        this.raio=raio;
        this.pricekm=preco;
        this.totalKm = numKm;
        this.aceitamed=aceita;
        this.cert = medcert;
        this.pronto = ready;
        this.numEnc = numeroencomendas;
        setHistorico(history);
    }
    
    public Transportadoras (Transportadoras t)
    {
        super(t);
        this.compNIF=t.getNIF();
        this.raio=t.getTRaio();
        this.pricekm=t.getPricekm();
        this.totalKm = t.getTotalKm();
        this.aceitamed=t.getAceitamed();
        this.cert = t.getCert();
        this.pronto = t.getPronto();
        this.numEnc=getNumEnc();
        setHistorico(t.getHistorico());
    }
    
    
    
    public boolean getCert()
    {
        return this.cert;
    }
    
    public boolean getAceitamed()
    {
        return this.aceitamed;
    }

    public boolean getPronto()
    {
        return this.pronto;
    }
    
    public String getNIF()
    {
        return this.compNIF;
    }

    public double getTRaio()
    {
        return this.raio;
    }
    
    public double getPricekm()
    {
        return this.pricekm;
    }

    public double getTotalKm()
    {
        return this.totalKm;
    }

    public int getNumEnc(){
        return this.numEnc;
    }

    public List<Encomenda> getHistorico(){
        List<Encomenda> newhis = new ArrayList<>();
        for (Encomenda e : this.historico){
            newhis.add(e);
        }
        return newhis;
    }
    
    public void setRaio(double rad)
    {
        this.raio=rad;
    }
    
    public void setNIF(String nif)
    {
        this.compNIF=nif;
    }

    public void setPricekm(double preco)
    {
        this.pricekm=preco;
    }

    public void setTotalKm(double numKm)
    {
        this.totalKm=numKm;
    }

    public void setNumEnc(int numeroenc) {
        this.numEnc=numeroenc;
    }
    
    public void aceitaMedicamentos(boolean state)
    {
        this.aceitamed=state;
    }
    
    public void setCert(boolean state)
    {
        this.cert = state;
    }

    public void setPronto(boolean state){
        this.pronto = state;
    }

    public void setHistorico(List<Encomenda> hist){
        this.historico = new ArrayList<>();
        for (Encomenda e : hist){
            this.historico.add(e);
        }
    }
    
    public boolean equals (Object o)
    {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        
        Transportadoras t = (Transportadoras) o;
        return(super.equals(t) && this.compNIF==t.getNIF() && this.raio==t.getTRaio() && this.pricekm==t.getPricekm());
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("NIF da Empresa: " + this.compNIF);
        sb.append("\nRaio de Trabalho: " + this.raio);
        sb.append("\nPreço por KM: "+this.pricekm);
        
        return sb.toString();
    }
    
    public Transportadoras clone()
    {
        return new Transportadoras (this);
    }
    
    public String tipoUtilizador()
    {
        return "Transportadora";
    }
    
    public boolean tPronto() //Sinaliza que pode recolher uma encomenda
    {
        return true;
    }
    
    public double precoTotal() // Determina o custo de entregar uma encomenda com base na distancia e tempo de espera na loja
    {
        return 0;
    }
    
    public double tEntregaEncomenda() //Entrega uma encomenda e retorna o tempo que demorou e o custo
    {
        return 0;
    }

    public boolean aceitoTransporteMedicamentos()
    {
        return this.aceitamed;
    }

    public boolean zonaEntrega (Coordenadas cord1, Coordenadas cord2)
    {
        double rad1=this.getTRaio();
        if (rad1 >=Coordenadas.distance(cord1, cord2)) return true;
        else return false;
    }

    public void addHistorico(Encomenda e){
        this.historico.add(e);
    }
}