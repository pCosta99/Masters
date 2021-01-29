import java.util.List;
import java.io.Serializable;
import java.util.ArrayList;

public class Voluntarios extends AllUsers implements Serializable
{
    private double raio;
    private boolean aceitamed; // aceita ou nao transportar medicamentos
    private boolean cert; // se tem ou nao certificado
    private boolean pronto; // se esta pronto a receber encomendas
    private List<Encomenda> historico;
    //private List<Integer> classificacao;

    public Voluntarios()
    {
       super();
       this.raio = 0;
       this.aceitamed=false;
       this.cert=false;
       this.pronto=false;
       this.historico = new ArrayList<>();
       //this.classificacao = new ArrayList<>();
    }
    
    public Voluntarios (String volcode, String volnome, Coordenadas xy, double raio, String email, String password, boolean aceita, boolean certificado, boolean ready, List<Integer> review, List<Encomenda> history)
    {
        super(volcode, volnome, xy, email, password, review);
        this.raio = raio;
        this.aceitamed=aceita;
        this.cert=certificado;
        //this.classificacao=review;
        this.pronto=ready;
        setHistorico(history);
    }
    
    public Voluntarios (Voluntarios v)
    {
        super(v);
        this.raio = v.getVRaio();
        this.aceitamed = v.getAceitamed();
        this.cert=v.getCertificado();
        //this.classificacao=v.getClassificacao();
        this.pronto= v.getPronto();
        setHistorico(v.getHistorico());
    }
    
    public boolean getCertificado()
    {
        return this.cert;
    }
    
    public double getVRaio()
    {
        return this.raio;
    }
    
    public boolean getAceitamed()
    {
        return this.aceitamed;
    }

    public boolean getPronto(){
        return this.pronto;
    }

    public List<Encomenda> getHistorico(){
        List<Encomenda> newhis = new ArrayList <>();
        for (Encomenda e : this.historico){
            newhis.add(e);
        }
        return newhis;
    }
    
    public void setCertificado(boolean certificado)
    {
        this.cert=certificado;
    }
    
    public void setVraio(double rad)
    {
        this.raio=rad;
    }
    
    public void aceitaMedicamentos(boolean state)
    {
        this.aceitamed=state;
    }

    public void setPronto(boolean ready){
        this.pronto = ready;
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
        
        Voluntarios v = (Voluntarios) o;
        return(super.equals(v) && this.raio== v.getVRaio());
    }
    
    public String toString()
    {
        StringBuilder sb = new StringBuilder();
        sb.append(super.toString());
        sb.append("\nRaio de Trabalho:" + this.raio);
        
        return sb.toString();
    }
    
    public Voluntarios clone()
    {
        return new Voluntarios (this);
    }
    
    public boolean VPronto() //sinaliza que esta pronto para recolher encomendas
    {
        return true;
    }
    
    public int levantarEncomenda(float codencomenda)
    {
        //aceder as encomendas da loja e retirar a que vai levantar
        return 0;
    }
    
    public int vEntregaEncomenda(Coordenadas xy, float codencomenda)
    {
        //Entregar a encomenda nas coordenadas de destino
        return 0;
    }
    
    public String tipoUtilizador()
    {
        return "Voluntario";
    }
    
    public boolean aceitoTransporteMedicamentos()
    {
        return this.aceitamed;
    }

    public boolean zonaEntrega (Coordenadas cord1, Coordenadas cord2)
    {
        double rad1=this.getVRaio();
        if (rad1 >= Coordenadas.distance(cord1, cord2)) return true;
        else return false;
    }

    public void addHistorico(Encomenda e){
        this.historico.add(e);
    }
}
