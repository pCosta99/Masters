package Model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/** Declaration of Class Voluntario which contains the required information on each Volunteer.
 *
 *  String codVoluntario - The Volunteer's system code.
 *  String name - The Volunteer's name.
 *  Location localizacao - The location from where we assume the Volunteer will start every service.
 *  double raio_de_acao - The range in which the volunteer can reach delivering goods.
 *  double rate - This will be the volunteer's rate, measured based on the client's feedback.
 *  boolean status - True is the volunteer is free, false otherwise.
 *  List of Strings encomendas - List with the codes of previously delivered packages.
 */
public class Voluntario implements Serializable {

    private String CodVoluntario;
    private String name;
    private Location localizacao;
    private double raio_de_acao;
    private double rate;
    private boolean status; //1 se livre, 0 se ocupado
    private List<String> encomendas;

    public Voluntario(){
        this.CodVoluntario = new String();
        this.name = new String();
        this.localizacao = null;
        this.rate = 0;
        this.status = true;
        this.raio_de_acao = 0;
        this.encomendas = new ArrayList<>();
    }
    public Voluntario(String CodVoluntario, String n, Location l, double r, boolean s, double raio, List<String> encomenda){
        this.CodVoluntario = CodVoluntario;
        this.name = n;
        this.localizacao = l;
        this.rate = r;
        this.status = s;
        this.raio_de_acao = raio;
        setEncomendas(encomenda);
    }
    public Voluntario(Voluntario v){
        setCodVoluntario(v.getCodVoluntario());
        setName(v.getName());
        setLocalizacao(v.getLocalizacao());
        setRate(v.getRate());
        setStatus(v.getStatus());
        setRaio(v.getRaio());
        setEncomendas(v.getEncomendas());
    }

    public String getCodVoluntario() {
        return CodVoluntario;
    }
    public void setCodVoluntario(String codVoluntario) {
        CodVoluntario = codVoluntario;
    }

    public String getName(){
        return this.name;
    }
    public void setName(String name){
        this.name = name;
    }

    public Location getLocalizacao(){
        return this.localizacao;
    }
    public void setLocalizacao(Location l){
        this.localizacao = localizacao;
    }

    public double getRate(){
        return this.rate;
    }
    public void setRate(double rate){
        this.rate = rate;
    }

    public boolean getStatus(){
        return this.status;
    }
    public void setStatus(boolean s){
        this.status = s;
    }

    public double getRaio(){
        return this.raio_de_acao;
    }
    public void setRaio(double r){
        this.raio_de_acao = r;
    }

    public List<String> getEncomendas() {
        return encomendas;
    }
    public void setEncomendas(List<String> encomendas) {
        this.encomendas = encomendas;
    }

    public Voluntario clone(){
        return new Voluntario(this);
    }
    public String toString() {
        return  "Codigo = " + CodVoluntario + " | " +
                "Nome = " + name + " | " +
                "Localizacao = " + localizacao.toString() + " | "+
                "Raio de acao = " + raio_de_acao + " | " +
                "Rate = " + rate + " | " +
                "Livre = " + status + " | " +
                "Encomendas=" + encomendas.toString();
    }
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Voluntario that = (Voluntario) o;
        return Double.compare(that.raio_de_acao, this.raio_de_acao) == 0 &&
                Double.compare(that.rate, this.rate) == 0 &&
                this.status == that.status &&
                Objects.equals(this.CodVoluntario, that.CodVoluntario) &&
                Objects.equals(this.name, that.name) &&
                Objects.equals(this.localizacao, that.localizacao) &&
                Objects.equals(this.encomendas, that.encomendas);
    }

    /** Prints the Volunteer only showing the info that matters to the user when choosing a volunteer to deliver his/her package.
     *
     * @return String with the relevant information.
     */
    public String toStringChoosable(){
        return "Codigo = " + CodVoluntario + " | " +
                "Nome = " + name + " | " +
                "Rate = " + rate + " | " +
                "Número de encomendas entregues = " +encomendas.size();
    }

    /** Checks if a Volunteer's Code is valid to be accepted on the system by checking if the first letter of the code is a lower case "v" and all other digits are integer numbers.
     *
     * @param v - Volunteer whose code will be analyzed.
     * @return - Validation of the Volunteer's code.
     */
    public static boolean validate(Voluntario v){
        String code = v.getCodVoluntario();
        boolean valid = true;
        for(int i = 1; i < code.length() && valid; i++){
            if(!(Character.isDigit(code.charAt(i)))){
                valid = false;
            }
        }
        return (code.charAt(0) == 'v' && valid);
    }

    /** Checks if the Volunteer is free and has the range of action to perform a delivery on the given location.
     *
     * @param l - Location to which we're looking to see if the Volunteer can perform a service.
     * @return True is the Volunteer is free and has the range to perform the task, false otherwise.
     */
    public boolean isAvailable(Location l){
        return (this.status &&
                (this.getLocalizacao().distanceTo(l) <= this.raio_de_acao));
    }

    /** Changes the state of the Volunteer to true, which means he/she is now free.
     *
     */
    public void isFree(){
        setStatus(true);
    }

    /** adds a package code to a list of strings that will be returned to become the volunteer's history.
     *
     * @param l - list with previously delivered packages by the user.
     * @param c - new package's code to be added to the history.
     * @return - new list with all previous deilvered packages + the new one.
     */
    public static List<String> addEnc(List<String> l,String c){
        int size = l.size();
        List<String> ret = new ArrayList<>(size+1);
        int i;
        for(i = 0; i < size+1;i++){
            ret.add(i,l.get(i));
        }
        ret.add(i,c);
        return ret;
    }
}
