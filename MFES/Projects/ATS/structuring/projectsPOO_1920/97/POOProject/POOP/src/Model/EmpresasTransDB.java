package Model;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import Exceptions.*;
import static java.lang.Math.*;

/**
 * Classe que representa a Base de dados de todas as empresas Transportadoras
 */
public class EmpresasTransDB implements Serializable{
    private static final long serialVersionUID = 8372013621494977601L;
    private Map<String, EmpresasTrans> empresas;

    EmpresasTransDB() {
        this.empresas = new HashMap<>();
    }

    private EmpresasTransDB(EmpresasTransDB e) {
        this.empresas = e.empresas
                .values()
                .stream()
                .collect(Collectors
                        .toMap(EmpresasTrans::getCodEmpresa, EmpresasTrans::clone));
    }


    /**
     * Método que obtém os códigos das empresas
     */
    List<String> getEmpresasIDS() {
        return new ArrayList<>(this.empresas.keySet());
    }

    /**
     * Método que adiciona uma empresa á Base de Dados
     */

    public void addEmpresa(EmpresasTrans e) throws JaExisteEmpresaException{
        if(this.empresas.putIfAbsent(e.getCodEmpresa(), e) != null)
            throw new JaExisteEmpresaException();
    }

    /**
    * Método que obtém a empresa dado um código de empresa
    */

    public EmpresasTrans getEmpresa (String cdE) throws NaoExisteEmpresaException {
        EmpresasTrans e = this.empresas.get(cdE);
        if(e == null)
            throw new NaoExisteEmpresaException();
        return e;
    }

    /**
    * Método que obtém todas as empresas disponiveis 
    */

    public ArrayList<EmpresasTrans> empresaslivres() {
        return this.empresas
                .values()
                .stream()
                .filter((e)-> e.isLivre())
                .map(EmpresasTrans::clone)
                .collect(Collectors
                        .toCollection(ArrayList::new));
    }

    /**
     * Método que calcula a distancia entre a distancia entre a empresa e um ponto 
     */

    public static double distanceTo(EmpresasTrans v, double lat1, double lon1) {
        double lati1= Math.toRadians(lat1);
        double long1=Math.toRadians(lon1);
        double lati2= Math.toRadians(v.getLatitude());
        double long2=Math.toRadians(v.getLongitude());

        double dist = acos(sin(lati1)* sin(lati2) + cos(lati1) * cos(lati2) * cos(long1 - long2)) * 6371;

        return dist;
    }

    /**
    * Método que obtém as empresas que estão num determinado raio e livres dado dois conjuntos de coordenadas 
    */

    public List<EmpresasTrans> empresasNoRaio(double la, double lo,double la1, double lo1 ) {
        return this.empresas
                .values()
                .stream()
                .filter(e -> e.getRaioA()>= distanceTo(e,la,lo) && e.isLivre() && e.getRaioA()>=distanceTo(e,la1,lo1))
                .map(EmpresasTrans::clone)
                .collect(Collectors.toList());
    } 

    /**
     * Método que obtém as empresas que estão num determinado raio e livres dado uma loja e um user 
     */
    public List<EmpresasTrans> empresasNoRaio(Lojas l, User u) {
        return this.empresas
                .values()
                .stream()
                .filter(e -> e.getRaioA()>= distanceTo(e,l.getLat(),l.getLon()) && e.isLivre() && e.getRaioA()>=distanceTo(e,u.getLatitude(),u.getLongitude()))
                .map(EmpresasTrans::clone)
                .collect(Collectors.toList());
    }

    /**
     * Método que obtém as empresas que transportam encomendas médicas e que estão num determinado raio e livres dado dois conjuntos de coordenadas
     */

    public List<EmpresasTrans> empresasNoRaioMed(double la, double lo,double la1, double lo1) {
        return this.empresas
                .values()
                .stream()
                .filter(e -> e.getRaioA()>= distanceTo(e,la,lo) && e.isLivre() && e.isMed && e.getRaioA()>=distanceTo(e,la1,lo1))
                .map(EmpresasTrans::clone)
                .collect(Collectors.toList());
    }

    /**
     * Método que obtém as empresas que transportam encomendas médicas e que estão num determinado raio e livres dado uma loja e um user 
     */

    public List<EmpresasTrans> empresasNoRaioMed(Lojas l , User u) {
        return this.empresas
                .values()
                .stream()
                .filter(e -> e.getRaioA()>= distanceTo(e,l.getLat(),l.getLon()) && e.isLivre() && e.isMed && e.getRaioA()>=distanceTo(e,u.getLatitude(),u.getLongitude()))
                .map(EmpresasTrans::clone)
                .collect(Collectors.toList());
    }



    /**
     * Método que obtém a lista das empresas da Base de Dados
     */

    public Set<EmpresasTrans> getEmpresas(){
        return this.empresas.values().stream().collect(Collectors.toSet());
    }
    
    /**
     * Método que obtém os numeros de kms feitos por uma determinada empresa 
     */
    
    double getNrKmsfeitosEMP(String empresaID){
        return this.empresas.get(empresaID).getNrKmsFeitos();
    }
    
    
    public EmpresasTransDB clone() { return new EmpresasTransDB(this); }
    
    public boolean equals(Object o) {
        if (this == o) return true;

        if (o == null || this.getClass() != o.getClass()) return false;

        EmpresasTransDB emp = (EmpresasTransDB) o;
        return this.empresas.equals(emp.empresas);
    }

    @Override
    public int hashCode() {
        return 1;
    }


}
