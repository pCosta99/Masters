import java.io.Serializable;
public class Voluntarios extends Transportador implements Serializable {
    

    public Voluntarios()
    {
        super();
    }

    public Voluntarios(String codVoluntario, String nome, String password, GPS coordenadas, double raio, Boolean tranportoMedicamentos)
    {
        super(codVoluntario,nome,password,coordenadas,raio,tranportoMedicamentos);
    }


    /**
    construtor que cria um voluntario com uma paasword default "123"
     */
    public Voluntarios(String codVoluntario, String nome, GPS coordenadas, double raio)
    {
        super(codVoluntario,nome,"123",coordenadas,raio,false);   // pass default
    }

    public Voluntarios(Voluntarios voluntario)
    {
        super(voluntario);
    }



    public Voluntarios clone()
    {
        return new Voluntarios(this);
    }

    public boolean equals(Object o) 
    {
        if (o == this)
            return true;
        if (!(o instanceof Voluntarios)) {
            return false;
        }
        Voluntarios voluntario = (Voluntarios) o;
        
        return super.equals(voluntario);
    }

    public String toString() 
    {
        StringBuilder sb = new StringBuilder();
        sb.append("\nCodigo Voluntario: ").append(super.getId())
        .append ("\nNome Voluntario: ").append(super.getNome())
        .append("\nCoordenadas: ").append(super.getCoordenadas().toString())
        .append("\nVelocidade: ").append(super.getVelocidade())
        .append("\nAceita transporte medico:").append(super.aceitoTransporteMedicamentos().toString())
        .append("\nPara Levar: ").append(super.getParaLevarString())
        .append("\nJa foi buscar : ").append(super.getJaFoiBuscar())
        .append("\nKms percorridos : ").append(super.getKms())
        .append(super.toString());

        return sb.toString();
    }

    /**
     * Usado para imprimir os Voluntarios para um ficheiro CSV
     */
    public String paraCSV (){

        StringBuilder sb = new StringBuilder();

        sb.append(super.getId()).append(",");
        sb.append(this.getNome()).append(",");
        sb.append(super.getCoordenadas().getLatitude()).append(",");
        sb.append(super.getCoordenadas().getLongitude()).append(",");
        sb.append(super.getRaio());

        return sb.toString();
    }


}
