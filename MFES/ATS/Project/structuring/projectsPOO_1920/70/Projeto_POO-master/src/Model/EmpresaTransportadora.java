package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class EmpresaTransportadora implements Serializable {
    private String codTrans;
    private String name;
    private int NIF;
    private boolean status; //True se livre, false se ocupado
    private double rate;
    private double raio_de_acao;
    private double taxpkm;
    private double taxapkg;
    private Location localizacao;
    private List<String> encomendas;
    private double distanciaPercorrida;


    public EmpresaTransportadora(){
        this.name = "";
        this.status = true;
        this.rate = 0;
        this.raio_de_acao = 0;
        this.taxpkm = 0;
        this.taxapkg = 0;
        this.localizacao = null;
        this.encomendas = new ArrayList<>();
        this.distanciaPercorrida = 0;
    }
    public EmpresaTransportadora(String code, String n, int NIF, boolean s, double r, double raio, double t, double tpk, Location l, double d, List<String> encomendas){
        this.codTrans = code;
        this.name = n;
        this.NIF = NIF;
        this.status = s;
        this.rate = r;
        this.raio_de_acao = raio;
        this.taxpkm = t;
        this.taxapkg = tpk;
        this.localizacao = l;
        setEncomendas(encomendas);
        this.distanciaPercorrida = d;
    }
    public EmpresaTransportadora(EmpresaTransportadora e){
        setCodeTrans(e.getCodTrans());
        setName(e.getName());
        setNIF(e.getNIF());
        setStatus(e.getStatus());
        setRate(e.getRate());
        setRaio(e.getRaio());
        setTaxa(e.getTaxa());
        setTaxapkg(e.getTaxapkg());
        setLocalizacao(e.getLocalizacao());
        setEncomendas(e.getEncomendas());
        setDistanciaPercorrida(e.getDistanciaPercorrida());
    }

    public String getCodTrans(){
        return this.codTrans;
    }
    public void setCodeTrans(String c){
        this.codTrans = c;
    }

    public String getName(){
        return this.name;
    }
    public void setName(String name){
        this.name = name;
    }

    public int getNIF() {
        return NIF;
    }
    public void setNIF(int NIF) {
        this.NIF = NIF;
    }

    public boolean getStatus(){
        return this.status;
    }
    public void setStatus(boolean s){
        this.status = s;
    }

    public double getRate(){
        return this.rate;
    }
    public void setRate(double rate){
        this.rate = rate;
    }

    public double getRaio(){
        return this.raio_de_acao;
    }
    public void setRaio(double r){
        this.raio_de_acao = r;
    }

    public double getTaxa(){
        return this.taxpkm;
    }
    public void setTaxa(double t){
        this.taxpkm = t;
    }

    public double getTaxapkg() {
        return taxapkg;
    }
    public void setTaxapkg(double taxapkg) {
        this.taxapkg = taxapkg;
    }

    public Location getLocalizacao(){
        return this.localizacao;
    }
    public void setLocalizacao(Location l){
        this.localizacao = localizacao;
    }

    public List<String> getEncomendas() {
        return encomendas;
    }
    public void setEncomendas(List<String> encomendas) {
        this.encomendas = encomendas;
    }

    public double getDistanciaPercorrida() {
        return distanciaPercorrida;
    }
    public void setDistanciaPercorrida(double distanciaPercorrida) {
        this.distanciaPercorrida = distanciaPercorrida;
    }

    /** Adds a given distance to the distance on the the Company.
     *
     * @param distanciaPercorrida - new distance.
     */
    public void addDistanciaPercorrida(double distanciaPercorrida) {
        this.distanciaPercorrida += distanciaPercorrida;
    }

    @Override
    public EmpresaTransportadora clone(){
        return new EmpresaTransportadora(this);
    }

    public String toString() {
        return "EmpresaTransportadora{" +
                "codTrans='" + codTrans + '\'' +
                ", name='" + name + '\'' +
                ", NIF=" + NIF +
                ", status=" + status +
                ", rate=" + rate +
                ", raio_de_acao=" + raio_de_acao +
                ", taxpkm=" + taxpkm +
                ", taxapkg=" + taxapkg +
                ", localizacao=" + localizacao +
                ", encomendas=" + encomendas +
                '}';
    }

    /** Prints the Company only showing the info that matters to the user when choosing a company to deliver his/her package.
     *
     * @param preco - price of the service.
     * @return String with the relevant information.
     */
    public String toStringChoosable(double preco){
        return "Código = "+codTrans+" | "+"Nome = "+" | "+"Rate = "+" | "+"Preco = "+preco+" | "+"Número de encomendas transportadas = "+encomendas.size();
    }

    /** Checks if a Company's Code is valid to be accepted on the system by checking if the first letter of the code is a lower case "t" and all other digits are integer numbers.
     *
     * @param et - Company whose code will be analyzed.
     * @return - Validation of the Company's code.
     */
    public static boolean validate(EmpresaTransportadora et){
        String code = et.getCodTrans();
        boolean valid = true;
        for(int i = 1; i < code.length() && valid; i++){
            if(!(Character.isDigit(code.charAt(i)))){
                valid = false;
            }
        }
        return (code.charAt(0) == 't' && valid);
    }

    /** Checks if the Company is free and has the range of action to perform a delivery on the given location.
     *
     * @param l - Location to which we're looking to see if the company can perform a service.
     * @return True is the company is free and has the range to perform the task, false otherwise.
     */
    public boolean isAvailable(Location l){
        return (this.status &&
                (this.getLocalizacao().distanceTo(l) <= this.raio_de_acao));
    }
}
