
public class Traz_Aqui_Comum implements Traz_Aqui_ComumI{
    // Variaveis de instancia
    private String codigo;
    private String nome;
    private double gpsX;
    private double gpsY;

    public Traz_Aqui_Comum(){
        this.codigo = "";
        this.nome = "";
        this.gpsX = 0;
        this.gpsY = 0;
    }

    public Traz_Aqui_Comum(String codigo, String nome, double gpsX, double gpsY) {
        this.codigo = codigo;
        this.nome = nome;
        this.gpsX = gpsX;
        this.gpsY = gpsY;
    }

    public Traz_Aqui_Comum(Traz_Aqui_Comum ta){
        this.codigo = ta.getCodigo();
        this.nome = ta.getNome();
        this.gpsX = ta.getGpsX();
        this.gpsY = ta.getGpsY();
    }

    public String getCodigo() {
        return this.codigo;
    }

    public void setCodigo(String codigo) {
        this.codigo = codigo;
    }

    public String getNome() {
        return this.nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getGpsX() {
        return this.gpsX;
    }

    public void setGpsX(double gpsX) {
        this.gpsX = gpsX;
    }

    public double getGpsY() {
        return this.gpsY;
    }

    public void setGpsY(double gpsY) {
        this.gpsY = gpsY;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Traz_Aqui_Comum ta = (Traz_Aqui_Comum) o;
        return Double.compare(ta.gpsX, this.gpsX) == 0 &&
                Double.compare(ta.gpsY, this.gpsY) == 0 &&
                ta.getCodigo().equals(this.codigo) &&
                ta.getNome().equals(this.nome);
    }

    public String toString() {
        StringBuilder sb;
        sb = new StringBuilder();
        sb.append("Codigo : ").append(this.codigo).append("\n")
          .append("Nome: ").append(this.nome).append("\n")
          .append("Coordenadas gps (").append(this.gpsX).append(",").append(this.getGpsY()).append(")\n");

        return sb.toString();
    }
}
