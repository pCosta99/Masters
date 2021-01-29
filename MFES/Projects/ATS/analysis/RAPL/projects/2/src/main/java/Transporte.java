
/**
 * Escreva a descrição da classe Transporte aqui.
 * 
 * @author (seu nome) 
 * @version (número de versão ou data)
 */
public abstract class Transporte extends Perfil
{
    private double raio;
    private int licMedicamentos;//0 nao 1 sim
    private int velmed;
    
    public Transporte(){
     super();
     this.raio=0;
     this.velmed=0;
     this.licMedicamentos=0;
    }
    public Transporte(String email,String nome,String password,
        Ponto2D local,double raio,int velmed,int licMedicamentos){
     super(email,nome,password,local);
     this.raio=raio;
     this.velmed=velmed;
     this.licMedicamentos=licMedicamentos;
    }
    public Transporte(Transporte um){
     super(um);
     this.raio=um.getRaio();
     this.velmed=um.getVelmed();
     this.licMedicamentos=um.getLicMedicamentos();
    }
    
    public double getRaio(){return this.raio;}
    public int getVelmed(){return this.velmed;}
    public int getLicMedicamentos(){return this.licMedicamentos;}
    
    public void setRaio(double raio){this.raio=raio;}
    public void setVelmed(int velmed){this.velmed=velmed;}
    public void setLicMedicamentos(int licMedicamentos){this.licMedicamentos=licMedicamentos;}
    
   
    public String toString(){
        StringBuilder sb = new StringBuilder();
        
        sb.append( super.toString());
        
        sb.append("Raio de açao: ");
        sb.append(this.raio + "\n");
        sb.append("Velocidade media: " );
        sb.append(this.velmed + "\n");
        sb.append("Licença de transporte de medicamentos: ");
        sb.append(this.licMedicamentos+ "\n");
        
        return sb.toString();
    }
    
    public abstract Transporte clone();
}

