import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.stream.Collectors;
import java.time.LocalDate;
import java.util.Scanner;

public class Utilizador extends Account 
{
    private String CodUtilizador;
    private Location loc;
    
    public Utilizador(){
        super();
        this.loc = new Location();
        this.CodUtilizador = "";
    }
    
    public Utilizador(String n,String em,String pass,List<Encomenda> en,String cod
    ,Location l){
        super(n,em,pass,en,cod);
        this.loc = l;
        this.CodUtilizador = cod;
    }
    
    public Utilizador(Utilizador a){
        super(a);
        this.CodUtilizador = a.getCodigo();
        this.loc = a.getLoc();
    }
    
    public Location getLoc(){
        return this.loc;
    }
    
    public void setLoc(Location l){
        this.loc = l;
    }
    
    public void setCodigo(String cod){
        this.CodUtilizador = cod;
    }
    
    public String getCodigo(){
        return this.CodUtilizador;
    }
    
    public Encomenda solicitarEncomenda(Loja l){
        return l.getEncomenda(this.CodUtilizador);
    }
    
    public String toString()
    {
        return super.toString() + "\nLoc: " + this.loc;
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }

    public Voluntario retornacloseVol(Location loc,Location l,List<Account> lis){
        Voluntario closer = null;
        double dist = Double.MAX_VALUE,temp;
        List<Voluntario> disponiveis = lis.stream()
                                       .filter(a->a.getClass().getSimpleName().equals("Voluntario"))
                                       .map(a->(Voluntario)a)
                                       .filter(a->(a.getDisponibilidade() == true)) 
                                       .collect(Collectors.toList());

        //System.out.println("Disponiveis: " + disponiveis.toString());
        
        for(Voluntario v : disponiveis){
            Location localVol = v.getLoc();
            if((temp = localVol.distanceTo(l))<dist && temp <= v.getRaio()){
                closer = v;
                dist = temp;
            }
        }

        return closer;
    }
    
    public EmpresaV retornacloseEmp(Location loc,Location l,List<Account> lis){
        EmpresaV closer = null;
        double dist = Double.MAX_VALUE,temp;
        List<EmpresaV> disponiveis = lis.stream()
                                       .filter(a->a.getClass().getSimpleName().equals("EmpresaV"))
                                       .map(a->(EmpresaV)a)
                                       .filter(a->((EmpresaV)a).getDisponibilidade() == true)
                                       .collect(Collectors.toList());
        
        for(EmpresaV v : disponiveis){
            Location localEmp = v.getLoc();
            if((temp = localEmp.distanceTo(l))<dist || temp <= v.getRaio()){
                closer = v;
                dist = temp;
            }
        }

        //System.out.println(closer);

        return closer;
    }

    public double custo(Encomenda enc, StateManager state)
    {
        Account emp = state.getTransRef(enc.getReferencia());

        return enc.getPeso()*0.2 + this.getLoc().distanceTo(((EmpresaV)emp).getLoc()) * 0.001 * ((EmpresaV)emp).getTaxakm();
    }
    
    public void aceiTrans(StateManager state)
    {
        Scanner input = new Scanner(System.in);
        List<Encomenda> enco = this.getEcs();
        System.out.println("Escolha encomenda a aceitar para transporte:");
        for(int i = 0; i < enco.size(); i++)
        {
            if(enco.get(i).getDatabusca() == null)
                System.out.println("Encomenda nº"+(i+1)+": " + enco.get(i).getReferencia() +" Transportadora: " 
                + state.getTransRef(enco.get(i).getReferencia()).getNome() + " Custo: " + custo(enco.get(i), state) );
        }
        System.out.print("Encomenda nº: ");
        int opt = input.nextInt();

        if(enco.get(opt-1).getDatabusca() == null)
            enco.get(opt-1).setDatabusca(LocalDate.now());
    }
}
