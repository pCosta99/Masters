/** Classe auxiliar para determinar os topUsers e topTransp do sistema*/
public class topUsers {
    private double qnt;
    private String cod;

    public double getQnt() {
        return qnt;
    }

    public void setQnt(int qnt) {
        this.qnt = qnt;
    }

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    /** Método que incrementa a quantidade*/
    public void addQnt (){
        this.qnt++;
    }

    /** Método que vai contabilizando os kms percorridos
     *
     * @param km Kilómetros
     */
    public void addKm(double km){
        this.qnt+=km;
    }

    public topUsers(String cod) {
        this.qnt = 0;
        this.cod = cod;
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("Quantidade: ").append(this.qnt).append("-")
                .append(this.cod);
        return sb.toString();
    }

}
