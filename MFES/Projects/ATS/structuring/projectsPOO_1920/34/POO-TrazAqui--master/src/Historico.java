import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.io.Serializable;


public class Historico implements Serializable {
    private List<Servico> servicos;

    public Historico(){
        this.servicos = new ArrayList<Servico>();

    }

    public Historico(List<Servico> servicos) {
        setServicos(servicos);
    }

    public  Historico(Historico h){
        setServicos(h.getServicos());
    }

    public List<Servico> getServicos() {
        List<Servico> ret = new ArrayList<>();
        servicos.forEach((a->ret.add(a.clone())));
        return ret;
    }

    public List<Servico> getServicosSClassificacao(String cod){
        List<Servico> ret = servicos.stream().filter(e->!e.hasClassificacao() && e.getCodUtilizador().equals(cod) && e.isConcluido()).collect(Collectors.toList());
        return ret;

    }

    public void setServicos(List<Servico> servicos) {
        this.servicos= new ArrayList<>();
        servicos.forEach(e->this.servicos.add(e.clone()));
    }

    public void addServico(Servico servico) {
    	this.servicos.add(servico.clone());
    }


    public int getIndiceServico(Servico s){
        for(int i= 0;i<this.getServicos().size();i++){
            if(this.getServicos().get(i).equalsCode(s.getCodServico())) return i;
        }
        return -1;
    }

    public void setClassServico(int c,int i){
        servicos.get(i).setClassificacao(c);
    }


    public boolean equals(Object o) {
        if (o == this) return true;
        if (o == null || o.getClass() != this.getClass()) return false;
        Historico a = (Historico) o;
        return this.servicos.equals(a.getServicos());
    }

    public String toString(){
        StringBuilder sb = new StringBuilder();
        sb.append("Lista de Servicos:").append(this.servicos);
        return sb.toString();
    }

    public Historico clone(){
        return new Historico(this);
    }

    public void removeServico(Servico s){
        this.servicos.removeIf(e->e.getCodServico().equals(s.getCodServico()));
    }
}
