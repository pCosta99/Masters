package Model;

import java.util.List;
import java.util.ArrayList;
import java.io.Serializable;

public class RegistosEncomenda implements IRegistosEncomenda {

    private List<RegistoEncomenda> registos;

    public RegistosEncomenda() {
        this.registos = new ArrayList<>();
    }

    public RegistosEncomenda(List<RegistoEncomenda> r) {
        setRegistos(r);
    }

    public RegistosEncomenda(RegistosEncomenda r) {
        setRegistos(r.getRegistos());
    }

    public List<RegistoEncomenda> getRegistos() {
        List<RegistoEncomenda> aux = new ArrayList<>();
        for (RegistoEncomenda r : this.registos)
            aux.add(r.clone());
        return aux;
    }

    public void setRegistos(List<RegistoEncomenda> r) {
        this.registos = new ArrayList<>();
        r.forEach(as -> this.registos.add(as.clone()));
    }

    public RegistosEncomenda clone() {
        return new RegistosEncomenda(this);
    }

    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        RegistosEncomenda e = (RegistosEncomenda) o;
        return this.registos.equals(e.getRegistos());
    }

    public String toString() {
        String formattedString = new String();
        StringBuilder sb = new StringBuilder();
        sb.append(registos);
        formattedString = sb.toString().replace(",","").replace("[","").
                replace("]","");
        return formattedString;
    }

    @Override
    public List<RegistoEncomenda> extraiRegistosDeAlguem (String quemFoi) {
        List<RegistoEncomenda> r = new ArrayList<>();
        for (RegistoEncomenda e : this.registos) {
            if (e.getQuemTransportou().equals(quemFoi)) {
                r.add(e.clone());
            }
        }
        return r;
    }

    @Override
    public void insertNosRegistos(RegistoEncomenda r) {
        this.registos.add(r);
    }

    @Override
    public RegistosEncomenda exportRegistos() {
        return this.clone();
    }

}
