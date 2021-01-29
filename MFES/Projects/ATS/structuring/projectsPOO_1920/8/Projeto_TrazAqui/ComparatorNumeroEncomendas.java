import java.util.Comparator;

public class ComparatorNumeroEncomendas implements Comparator<Object> {
    public int compare(Object u1, Object u2){
        Voluntario v1 = u1 instanceof Voluntario ? (Voluntario) u1 : null;
        Empresaentrega e1 = u1 instanceof Empresaentrega ? (Empresaentrega) u1: null;
        Voluntario v2 = u2 instanceof Voluntario ? (Voluntario) u2: null;
        Empresaentrega e2 = u2 instanceof Empresaentrega ? (Empresaentrega) u2: null;

        int s1 = v1 != null ? v1.getEntregas_feitas().size(): e1.getEncomendas().size();
        int s2 = v2 != null ? v2.getEntregas_feitas().size(): e2.getEncomendas().size();

        if (s1 == s2){
            String nome1 = v1 != null ? v1.getUsername(): e1 != null ? e1.getUsername(): null;
            String nome2 = v2 != null ? v2.getUsername(): e2 != null ? e2.getUsername(): null;
            return nome1.compareTo(nome2);
        }
        if (s2 > s1){
            return 1;
        }
        return -1;
    }
}
