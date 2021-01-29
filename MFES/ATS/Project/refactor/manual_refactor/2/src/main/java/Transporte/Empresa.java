package Transporte;

import Perfis.Ponto2D;

/**
 * Escreva a descrição da classe Transporte.Empresa aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public class Empresa extends Transporte
{
    private double custokg; //custo por peso
    private double custokm;//custo por distanciaa
    private String nif;
    
 public Empresa(){
     super();
     this.custokg=0;
     this.custokm=0;
   }
 public Empresa(String email, String nome, String password, Ponto2D local,
                double raio, int velmed , int licMedicamentos, String nif,
                double custokg, double custokm){
    super(email,nome,password,local,raio,velmed,licMedicamentos);
    this.nif=nif;
    this.custokg=custokg;
    this.custokm=custokm;
   }
 public Empresa(Empresa m){
     super(m);
     this.nif=m.getNif();
     this.custokg=m.getCustokg();
     this.custokm=m.getCustokm();
    }
 
 public double getCustokg(){
     return this.custokg;
    }
 public double getCustokm(){
     return this.custokm;
    }
 public String getNif(){
     return this.nif;
    }

    public void setCustokg(double nc){
     this.custokg=nc;
    }
 public void setCustokm(double nc){
     this.custokm=nc;
    }

 @Override
 public String toString(){
     StringBuilder sb = new StringBuilder();

     sb.append( super.toString());
     sb.append("Nif: ");
     sb.append(this.nif).append("\n");
     sb.append("Custo por km: ");
     sb.append(this.custokm).append("\n");
     sb.append("Custo por quilo: ");
     sb.append(this.custokg).append("\n");

     return sb.toString();
    }

 public Empresa clone(){return new Empresa(this);}

}

