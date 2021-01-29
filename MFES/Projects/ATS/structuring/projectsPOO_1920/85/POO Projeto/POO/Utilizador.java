import java.time.LocalDateTime;
import java.util.List;

public class Utilizador extends Traz_Aqui_Comum implements UtilizadorI{

    /**
     * COnstrutor para objetos da classe Voluntario
     */
    public Utilizador() {
        super();
    }

    public Utilizador(String codigo, String nome, double gpsX, double gpsY) {
        super(codigo, nome, gpsX, gpsY);
    }

    public Utilizador(Utilizador u) {
        super(u);
    }


    /**
     * Metodos gets e sets,
     * clone equals e toString
     */
    public String toString(){
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Utilizador\n")
                .append(super.toString());
        return sb.toString();
    }

    public boolean equals(Object obj){
        return super.equals(obj);
    }

    public Utilizador clone(){
        return new Utilizador(this);
    }

    /**
     * Metodo que acede à informação das entregas efectuadas num determinado período e por voluntário
     * ou transportador;
     */
    public String informacaoTransporte(List<Transporte_EncomendaI> tes, LocalDateTime inicio, LocalDateTime fim){
        StringBuilder sb = new StringBuilder();

        for(Transporte_EncomendaI te: tes) {
            if (te.getInicioTransporte().isAfter(inicio) && te.getInicioTransporte().isBefore(fim))
                sb.append(te.toString());
        }
        return sb.toString();
    }

    /**
     * Metodo que le de uma String os dados de um utilizador
     * e atualiza o conteudo do utilizador
     */
    public void leTA(String cod, String[] p){
        this.setCodigo(cod);
        this.setNome(p[1]);
        this.setGpsX(Double.parseDouble(p[2]));
        this.setGpsY(Double.parseDouble(p[3]));
    }

}
