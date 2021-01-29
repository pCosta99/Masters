import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.util.Scanner;
import java.time.LocalDate;

public class EmpresaV extends Account
{
    private String codigo;
    private double classificacao;
    private int uti;
    private double raio;
    private double velocidademed;
    private boolean disponibilidade;
    private boolean medicamentos;
    private Location loc;
    private String nif;
    private static double taxakm;
    private double totalfat;
    
    public EmpresaV(){
        super();
        this.codigo = "";
        this.classificacao = 0.0;
        this.loc = new Location();
        this.uti = 0;
        this.raio = 0.0;
        this.velocidademed = 0.0;
        this.medicamentos = false;
        this.nif = "";
        this.taxakm = 0;
        this.totalfat = 0;
    }
    
    public EmpresaV(List<Encomenda> list, String email, String password, String cod,String n,Location l,double r,
    String ni,double txak){
        //super(list, email, password, cod,n,l,r);
        super(n,email, password, list, cod);
        this.loc = l;
        this.codigo = cod;
        this.classificacao = 0;
        this.raio = r;
        this.velocidademed = 0;
        this.disponibilidade = true;
        this.medicamentos = false;
        this.uti = 0;
        this.nif = ni;
        this.taxakm = txak;
    }
    
    public EmpresaV(EmpresaV emp){
        super(emp);
        this.codigo = emp.getCodigo();
        this.loc = emp.getLoc();
        this.classificacao = emp.getClassificacao();
        this.raio = emp.getRaio();
        this.velocidademed = emp.getVelocidademed();
        this.disponibilidade = emp.getDisponibilidade();
        this.medicamentos = emp.getMedicamentos();
        this.nif = emp.getNif();
        this.taxakm = emp.getTaxakm();
    }
    
    public Location getLoc(){
        return this.loc;        

    }
    
    public void setLoc(Location l){
        this.loc = l;       

    }
    
    public String getNif(){
        return this.nif;
    }
    
    public double getTaxakm(){
        return this.taxakm;
    }

    public double getTotalFat(){
        return this.totalfat;
    }

    public void setTotalFat(double s)
    {
        this.totalfat = s;
    }
    
    public boolean equals(Object o){
        if(this == o) return true;
        if((o == null) || (this.getClass() != o.getClass())) return false;
        EmpresaV emp = (EmpresaV) o;
        return super.equals(o) &&
        this.nif == emp.getNif() && this.taxakm == emp.getTaxakm();
    }
    
    public String toString(){
        return super.toString() + "\nLoc: " + this.loc + "\nNIF: " + this.nif + "\nTaxa: " + this.taxakm + "\nDisp: " + this.disponibilidade;
    }
    
    public EmpresaV clone(){
        return new EmpresaV(this);
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

    public double custo(Encomenda enc, StateManager state)
    {
        Account emp = state.getTransRef(enc.getReferencia());

        return enc.getPeso()*0.2 + ((Utilizador)state.getUserByCode(enc.getCliente())).getLoc().distanceTo(((EmpresaV)emp).getLoc()) * 0.001 * ((EmpresaV)emp).getTaxakm();
    }

    public void transportar(StateManager state)
    {
        Scanner input = new Scanner(System.in);
        List<Encomenda> lis = this.getEcs();

        if(lis.size() != 0)
            System.out.println("Encomendas a transportar:");
        else
            System.out.println("Sem Encomendas");
        for(int i = 0; i < lis.size(); i++)
        {
            if(lis.get(i).getDatabusca() != null && lis.get(i).getDataentrega() == null)
                System.out.println((i+1) + "-Ref: " + lis.get(i).getReferencia());
        }

        if(lis.size() != 0)
        {
            System.out.print("Transportar nº: ");
            int opt = input.nextInt();

            if(lis.size() >= opt && lis.get(opt-1).getDatabusca() != null && lis.get(opt-1).getDataentrega() == null)
            {
                lis.get(opt-1).setDataentrega(LocalDate.now());
                setTotalFat(getTotalFat() + custo(lis.get(opt-1),state) );
            }
        }
    }

    public void transportadas()
    {
        List<Encomenda> lis = this.getEcs();

        if(lis.size() != 0)
        {
            for(int i = 0; i < lis.size(); i++)
            {
                if(lis.get(i).getDatabusca() != null && lis.get(i).getDataentrega() != null)
                    System.out.println("Encomenda " + lis.get(i).getReferencia() +  " entregue ao Cliente: " 
                    + lis.get(i).getCliente() + " no dia: " + lis.get(i).getDataentrega());
            }
        }
        else
        {
            System.out.println("Sem encomendas transportadas");
        }
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
    
    public void addClassificacao(int i){
        double sum = this.classificacao + ((i - this.classificacao)/uti);
        setClassificacao(sum);
    }
}
