package Model.Atores.Transportadores;

import Model.Encomenda;

import java.awt.geom.Point2D;
import java.util.Map;

public class EmpresaTransportadora extends Transporte
{
    //Variáveis de instância


    private double taxa; //taxa de transporte
    private double totalFaturado;



    public EmpresaTransportadora(){
        super();
        this.taxa = 0;
        this.totalFaturado = 0;
    }


    public EmpresaTransportadora(String email, String referencia, String nome, String password, Point2D.Double morada, long nif, boolean disponibilidade, float raio, boolean certeficado, double classificacao, int numeroEntregas , double velocidadeMedia, double numeroKms, double custoTransporte, Map<String, Encomenda> encomendas, double totalFaturado) {

        super(email,referencia,nome, password, morada,nif, disponibilidade,raio, certeficado,  classificacao, numeroEntregas,velocidadeMedia, numeroKms, encomendas);
        this.taxa = custoTransporte;
        this.totalFaturado = totalFaturado;

    }

    public EmpresaTransportadora (EmpresaTransportadora e){
        super(e.getEmail(),e.getReferencia(),e.getNome(), e.getPassword(),e.getMorada(),e.getNif(),e.isDisponivel(),e.getRaio(),e.isCerteficado(),e.getClassificacao(),e.getNumeroEntregas(),e.getVelocidadeMedia(),e.getNumeroKms(),e.getEncomendas());
        setTaxa(e.getTaxa());
        setTotalFaturado(e.getTotalFaturado());

    }

    //GETTER

   public double getTaxa() {
        return taxa;
    }



    public double getTotalFaturado() {
        return totalFaturado;
    }

    //SETTER

    public void setTaxa(double taxa) {
        this.taxa = taxa;
    }


    public void setTotalFaturado(double totalFaturado) {
        this.totalFaturado = totalFaturado;
    }

    public boolean equals(Object o){
        if(this == o)
        return true;
        if((o==null) || (o.getClass() != this.getClass())) 
        return false;
        else{
        EmpresaTransportadora a = (EmpresaTransportadora) o;
        return this.taxa == a.getTaxa();
        }
    }
    



        public EmpresaTransportadora clone(){
        return new EmpresaTransportadora(this);
    }


/*Métodos*/

    public double defineCusto(Encomenda a ){
        double b = this.getMorada().distance(a.getLoja().getMorada())+a.getLoja().getMorada().distance(a.getComprador().getMorada())*this.getTaxa();
        double c = a.getPeso()*this.getTaxa();
        return b+c;
    }

    //adiciona valor de um transporte ao total faturado
    public void addFatura(Encomenda a){
        setTotalFaturado(getTotalFaturado()+defineCusto(a));
    }

    public boolean distanciaValida(Encomenda a){
        if (a.getLoja().getMorada().distance(this.getMorada())>getRaio()) return false;
        else return true;
    }










}

