import java.util.List;
import java.time.LocalTime;
import java.time.LocalDate;
import java.util.Scanner;

public class Voluntario extends Account
{
    private String codigo;
    private String nome;
    private double classificacao;
    private int uti;
    private double raio;
    private double velocidademed;
    private Location loc;
    private boolean disponibilidade;
    private boolean medicamentos;
    private Encomenda enc;
    
    public Voluntario(){
        super();
        this.codigo = "";
        this.classificacao = 0.0;
        this.loc = new Location();
        this.uti = 0;
        this.raio = 0.0;
        this.velocidademed = 0.0;
        this.disponibilidade = true;
        this.medicamentos = false;
    }
    
    public Voluntario(List<Encomenda> list,String email, String password,String cod,String n,Location loc,double r,Encomenda en){
        super(n,email, password, list, cod);
        this.codigo = cod;
        this.classificacao = 0.0;
        this.loc = loc;
        this.raio = r;
        this.velocidademed = 0;
        this.disponibilidade = true;
        this.medicamentos = false;
        this.uti = 0;
        this.enc = en;
    }
    
    public Voluntario(Voluntario v){
        super(v);
        //completar
        this.raio = v.getRaio();
        this.disponibilidade = v.getDisponibilidade();
        this.loc = v.getLoc();
        this.enc = v.getEncomendap();
    }

    public Location getLoc(){
        return this.loc;
    }
    
    public void setLoc(Location l){
        this.loc = l;
    }
    
    public Encomenda getEncomendap(){
        return this.enc.clone();
    }
    
    public void setEncomenda(Encomenda e){
        this.enc = e.clone();
    }
    
public void setVelocidademed(double l){
        this.velocidademed = l;
    }
    
    public void setDisponibilidade(Boolean b){
        this.disponibilidade = b;
    }
    
    public int getUtilizador(){
        return this.uti;
    }
    
    public void setUtilizador(int u){
        this.uti = u;
    }
    
    public String getCodigo(){
        return this.codigo;
    }
    
    public void setClassificacao(double d){
        this.classificacao = d;
    }
    
    public double getClassificacao(){
        return this.classificacao;
    }
    
    public double getRaio(){
        return this.raio;
    }
    
    public double getVelocidademed(){
        return this.velocidademed;
    }
    
    public boolean getDisponibilidade(){
        return this.disponibilidade;
    }
    
    public boolean getMedicamentos(){
        return this.medicamentos;
    }

    public void changeDisp()
    {
        this.setDisponibilidade(!this.getDisponibilidade());
    }
    
    public void showDisp()
    {
        if(this.getDisponibilidade() == true)
            System.out.println("Disponível");
        else
            System.out.println("Não disponível");
    }

    public void addClassificacao(int i){
        double sum = 0;
        this.uti++;
        sum = this.classificacao + ((i - this.classificacao)/uti);
        setClassificacao(sum);
    }

    public boolean equals(Object o){
        if(this == o) return true;
        if((o == null) || (this.getClass() != o.getClass())) return false;
        Voluntario v = (Voluntario) o;
        return super.equals(o) && this.enc.equals(v.getEncomenda());
    }
    
    public String toString(){      
        return super.toString() + "\nLoc: " + this.loc  + "\nDisponibilidade: " + this.disponibilidade  + "\n" +  this.enc.toString();
    }
    
    public Voluntario clone() {return new Voluntario(this);}

    public void transEnc()
    {
        Scanner input = new Scanner(System.in);
        List<Encomenda> lis = this.getEcs();

        System.out.println("Encomendas a transportar:");
        for(int i = 0; i < lis.size(); i++)
        {
            if(lis.get(i).getDatabusca() == null)
                System.out.println((i+1) + "-Ref: " + lis.get(i).getReferencia());
        }

        System.out.print("transportar nº: ");
        int opt = input.nextInt();

        if(lis.get(opt-1).getDatabusca() == null)
        {
            lis.get(opt-1).setDatabusca(LocalDate.now());
            lis.get(opt-1).setDataentrega(LocalDate.now());
        }
    }    
}
