import java.io.Serializable;
import java.lang.reflect.Array;
import java.util.*;
import java.util.stream.Collectors;


/**
 * Método que contem e gere todas as encomendas que estao prontas para ser entregues.
 */
public class EncomendasAceites implements Serializable {
    private Map<String,List<Encomenda>> encAceites; //key é o codigo da Loja
    //Todas a encomendas prontas a ser levantadas.


    public EncomendasAceites (){
        this.encAceites = new HashMap<>();
    }

    public EncomendasAceites(Map<String,List<Encomenda>> encAceites) {
        setEncAceites(encAceites);
    }

    public EncomendasAceites(EncomendasAceites a){
        setEncAceites(a.getEncAceites());
    }

    public Map<String,List<Encomenda>> getEncAceites() {
        Map<String,List<Encomenda>> aux = new HashMap<>();
        for(Map.Entry<String,List<Encomenda>> e:this.encAceites.entrySet())
            aux.put(e.getKey(),e.getValue().stream().map(Encomenda::clone).collect(Collectors.toList()));
        return aux;
    }

    public void setEncAceites(Map<String,List<Encomenda>> encAceites) {
        this.encAceites = new HashMap<>();
        for(Map.Entry<String,List<Encomenda>> e:encAceites.entrySet())
            this.encAceites.put(e.getKey(),e.getValue().stream().map(Encomenda::clone).collect(Collectors.toList()));
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EncomendasAceites that = (EncomendasAceites) o;
        return Objects.equals(encAceites, that.encAceites);
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("EncomendasAceites{");
        sb.append("encAceites=").append(encAceites);
        sb.append('}');
        return sb.toString();
    }

    public EncomendasAceites clone(){
        return new EncomendasAceites(this);
    }

    //-----------------------------------------------------------------------------

    /**
     * Método que adiciona um clone de uma encomenda as encomendas aceites
     * @param e Encomenda a adicionar.
     */
    public void add(Encomenda e){
        if(this.encAceites.containsKey(e.getCodLoja())){
            this.encAceites.get(e.getCodLoja()).add(e.clone());
        }
        else{
            List<Encomenda> t = new ArrayList<>();
            t.add(e.clone());
            this.encAceites.put(e.getCodLoja(),t);
        }
    }

    /**
     * Método que remove uma encomenda das encomendas aceites.
     * @param codLoja Codigo da loja em que foi feita a encomenda.
     * @param enc Encomenda a remover.
     * @return Retorna a encomenda removida.
     */
    public Encomenda remove(String codLoja,Encomenda enc){
        List<Encomenda> aux = this.encAceites.get(codLoja);
        Iterator<Encomenda> it = aux.iterator();
        Encomenda e = null;
        boolean flag = false;
        int i = 0;
        while (it.hasNext() && !flag){
            e = it.next();
            if(e.getCodEncomenda().equals(enc.getCodEncomenda())){
                flag  = true;
                aux.remove(i);
            }
            i++;
        }
        return e;
    }

    /**
     * Método que altera o transporte que realiza a entrega de uma certa encomenda.
     * @param l Loja em que foi feita a encomenda.
     * @param e Encomenda
     * @param t Transporte que vai realizar a entrega.
     */
    public void alteraTransporte(Loja l,Encomenda e,Transportes t){
        List<Encomenda> ent = this.encAceites.get(l.getCodigo());
        boolean flag = false;
        for(int i = 0;i<ent.size() && !flag;i++){
            if(ent.get(i).getCodEncomenda().equals(e.getCodEncomenda())){
                ent.get(i).setCodTrans(t.getCodigo());
                flag = true;
            }
        }
    }

    /**
     * Método que retorna a lista das encomenda pendentes de um certo utilizador.
     * @param codCli Código do utilizador.
     * @return Retorna a lista das encomendas do utilizador.
     */
    public List<Encomenda> encomendasPendentes(String codCli){
        return this.encAceites.values().stream()
                                       .flatMap(Collection::stream)
                                       .filter(a->a.getCodUtilizador().equals(codCli))
                                       .map(Encomenda::clone)
                                       .collect(Collectors.toList());
    }

    /**
     * Método que retorna a lista de todas as encomendas aceitas, usado para criar um código unico de encomenda.
     * @return Lista com todos os códigos.
     */
    public List<String> codHistorico(){
        if (this.encAceites.size() == 0) return null;
        return this.encAceites.values().stream().flatMap(Collection::stream).map(Encomenda::getCodEncomenda).collect(Collectors.toList());
    }

}