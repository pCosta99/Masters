import java.io.Serializable;

public class Voluntario implements Serializable  {
    private String cod;
    private String nome;
    private boolean med;
    private double range;
    private Coordenadas cord;
    private double clas;
    private double vkm;
    private String email;
    private String pass;
    private int cap;

    public String getCod() {
        return cod;
    }

    public void setCod(String cod) {
        this.cod = cod;
    }

    public String getNome() {
        return nome;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }

    public double getRange() {
        return range;
    }

    public void setRange(double range) {
        this.range = range;
    }

    public Coordenadas getCord() {
        return cord;
    }

    public void setCord(Coordenadas cord) {
        this.cord = cord;
    }

    public boolean isMed() {
        return med;
    }

    public void setMed(boolean med) {
        this.med = med;
    }

    public double getClas() {
        return clas;
    }

    public void setClas(double clas) {
        this.clas = clas;
    }

    public double getVkm() {
        return vkm;
    }

    public void setVkm(double vkm) {
        this.vkm = vkm;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getPass() {
        return pass;
    }

    public void setPass(String pass) {
        this.pass = pass;
    }

    public int getCap() {
        return cap;
    }

    public void setCap(int cap) {
        this.cap = cap;
    }

    public Voluntario(String cod, String nome, double range, Coordenadas cord, double clas, boolean med, double vkm, String email, String pass, int cap) {
        this.cod = cod;
        this.nome = nome;
        this.range = range;
        this.cord = cord;
        this.clas = clas;
        this.med = med;
        this.vkm = vkm;
        this.email = email;
        this.pass = pass;
        this.cap = cap;
    }

    public Voluntario (Voluntario a){
        this.cod = a.getCod();
        this.nome = a.getNome();
        this.range = a.getRange();
        this.cord = a.getCord();
        this.clas=a.getClas();
        this.med = a.isMed();
        this.vkm = a.getVkm();
        this.email = a.getEmail();
        this.pass = a.getPass();
        this.cap = a.getCap();
    }

    public Voluntario clone (){
        return new Voluntario(this);
    }

    /** Método que verifica se um voluntário esta no range
     *
     * @param a Coordenadas
     * @param range Range
     */
    public boolean isRangeV (Coordenadas a, Double range){
        return this.cord.isRange(a,range);
    }

    /** Método que calcula a distância voluntário
     *
     * @param a Coordenadas
     */
    public double distanceV (Coordenadas a){
        return this.cord.distancia(a);
    }

    /** Método que cálcula o tempo de entrega
     *
     * @param l Coordenadas loja
     * @param u Coordenadas utilizador
     * @param cl Tempo
     */
    public double calcTemp (Coordenadas l,Coordenadas u,Double cl){
        return (this.cord.distancia(l) + u.distancia(l))/this.vkm + cl;
    }

    /** Método que altera um rating
     *
     * @param rating Rating
     */
    public void changeR (double rating){
        if (this.clas==-1) this.clas=rating;
        else this.clas+=rating;
    }

    /** Método que diminui a capacidade de um voluntário*/
    public void dimCap (){
        if (this.cap!= 0) this.cap--;
    }

    /** Método que aumenta a capacidade de um voluntário*/
    public void aumCap (){
        this.cap++;
    }
}