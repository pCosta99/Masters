import java.io.Serializable;

public class Transportadoras implements Serializable  {

    private String cod;
    private String nome;
    private Coordenadas cord;
    private double range;
    private boolean med;
    private double prkm;
    private double nif;
    private double clas;
    private String email;
    private String pass;
    private double vkm;
    private double kmp;
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

    public Coordenadas getCord() {
        return cord;
    }

    public void setCord(Coordenadas cord) {
        this.cord = cord;
    }

    public double getRange() {
        return range;
    }

    public void setRange(double range) {
        this.range = range;
    }

    public double getPrkm() {
        return prkm;
    }

    public void setPrkm(double prkm) {
        this.prkm = prkm;
    }

    public double getNif() {
        return nif;
    }

    public void setNif(double nif) {
        this.nif = nif;
    }

    public double getClas() {
        return clas;
    }

    public void setClas(double clas) {
        this.clas = clas;
    }

    public boolean isMed() {
        return med;
    }

    public void setMed(boolean med) {
        this.med = med;
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

    public double getVkm() {
        return vkm;
    }

    public void setVkm(double vkm) {
        this.vkm = vkm;
    }

    public double getKmp() {
        return this.kmp;
    }

    public void setKmp(double kmp) {
        this.kmp += kmp;
    }

    public int getCap() {
        return cap;
    }

    public void setCap(int cap) {
        this.cap = cap;
    }

    public Transportadoras (String cod, String nome, Coordenadas cord, double range, double prkm, double nif, double clas, boolean med, double vkm, String email, String pass, double kmp, int cap) {
        this.cod = cod;
        this.nome = nome;
        this.cord = cord;
        this.range = range;
        this.prkm = prkm;
        this.nif = nif;
        this.clas = clas;
        this.med = med;
        this.vkm = vkm;
        this.email = email;
        this.pass  = pass;
        this.kmp = kmp;
        this.cap = cap;
    }

    public Transportadoras(Transportadoras a) {
        this.cod = a.getCod();
        this.nome = a.getNome();
        this.cord = a.getCord();
        this.range = a.getRange();
        this.prkm = a.getPrkm();
        this.nif = a.getNif();
        this.med = a.isMed();
        this.vkm = a.getVkm();
        this.pass = a.getPass();
        this.email = a.getEmail();
        this.kmp = a.getKmp();
        this.cap = a.getCap();
    }

    public Transportadoras clone(){
        return new Transportadoras(this);
    }

    /** Método que verifica se uma transportadora esta no range
     *
     * @param a Coordenada
     * @param range Range
     */
    public boolean isRangeT (Coordenadas a, Double range){
        return this.cord.isRange(a,range);
    }

    /** Método que calcula a distância transportadora
     *
     * @param a Coordenada
     */
    public double distanceT (Coordenadas a){
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

    /** Método que cálcula o custo de entrega
     *
     * @param l Coordenadas loja
     * @param peso Peso
     * @param u Coordenadas utilizador
     */
    public double calcCost (Coordenadas l,double peso,Coordenadas u){
        return (this.cord.distancia(l) + u.distancia(l))/this.prkm + peso*0.5;
    }

    /** Método que altera um rating
     *
     * @param rating Rating
     */
    public void changeR (double rating){
        if (this.clas==-1) this.clas=rating;
        else this.clas+=rating;
    }

    /** Método que diminui a capacidade de uma transportadora*/
    public void dimCap (){
        if (this.cap!= 0) this.cap--;
    }

    /** Método que aumenta a capacidade de uma transportadora*/
    public void aumCap (){
        this.cap++;
    }
}
