import java.io.Serializable;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


/**
 * classe utilizada para guardar todos os RegistoEncomenda de um dado User
 */
public class RegistoEncomendaLista implements Serializable{
    
    private List<RegistoEncomenda> registo;

    public RegistoEncomendaLista() {
        registo = new ArrayList<>();
    }

    public RegistoEncomendaLista(List<RegistoEncomenda> registo) {
        setRegisto(registo);
    }
    
    public RegistoEncomendaLista(RegistoEncomendaLista r){
        this.registo = r.getRegisto();
    }

    public List<RegistoEncomenda> getRegisto() {
        return this.registo.stream().map(x -> x.clone()).collect(Collectors.toList());
    }

    public void setRegisto(List<RegistoEncomenda> registo) {
        this.registo = registo.stream().map(x -> x.clone()).collect(Collectors.toList());
    }

    


    public boolean equals(Object o) {
        if (o == this)
            return true;
        if (!(o instanceof RegistoEncomendaLista)) {
            return false;
        }
        RegistoEncomendaLista registoEncomendas = (RegistoEncomendaLista) o;
        return this.registo.equals(registoEncomendas.getRegisto());
    }


    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.registo.toString()).append("\n");

        return sb.toString();
    }
    
    public RegistoEncomendaLista clone () {
        return new RegistoEncomendaLista(this);
    }

    public void add (RegistoEncomenda registoEnc) {
        registo.add(registoEnc.clone());
    }
        
    /**
     *esta funçao recebe o codigo do entregador e uma data inicial e uma data final e devolve uma lista de RegistoEncomenda desse entregador nesse periodo de tempo
     */
    public List<RegistoEncomenda> verRegistos (String codEntregador, LocalDate inicio, LocalDate fim){
        List<RegistoEncomenda> ret = new ArrayList<>();
        
        for (RegistoEncomenda re : registo){
            if ((re.getDataDaEntrega().equals(fim) || re.getDataDaEntrega().equals(inicio) || (re.getDataDaEntrega().isAfter(inicio) && re.getDataDaEntrega().isBefore(fim))) && re.getEntregador().equals(codEntregador)){
                ret.add(re.clone());
            }
        }

        return ret;
    }

    /**
     * mostra todos os RegistosEncomenda
     */
    public List<RegistoEncomenda> verRegistosGeral (){
        return this.getRegisto();
    }
    /**
     * devolve uma lista de RegistosEncomenda sem classificaçao
     * caso seja uma lista vazia manda uma ListaVaziaException
     */
    public List<RegistoEncomenda> naoClassificado () throws ListaVaziaException {
        List<RegistoEncomenda> ret = new ArrayList<>();

        for (RegistoEncomenda re : registo){
            if(re.getClassificacao() == 0){
                ret.add(re.clone());
            }
        }

        if (ret.size() == 0){
            throw new ListaVaziaException("Lista Vazia");
        }

        return ret;
    }

    /**
     * recebe o codigo de encomenda e uma classificaçao e classifica a entrega da encomenda
     */
    public void classificaEntrega (String codEnc, int classificacao){
        for (RegistoEncomenda re : registo){
            if(re.getEncomenda().getCodEncomenda().equals(codEnc)){
                re.setClassificacao(classificacao);
                break;
            }
        }
    }

    /**
     * devolve uma string com os RegistoEncomenda ja classificados e no fim a media de todos 
     * caso estejam todos classificar devlove a string "Ainda nao classificado"
     */
    public String verClassificacoes (){
        int atual = 0;
        int soma = 0;
        int numero = 0;
        StringBuilder out = new StringBuilder();
        out.append("Classificacoes: \n");
        for (RegistoEncomenda re : registo){
            atual = re.getClassificacao();
            if (atual == 0){
                continue;
            }
            else {
                soma += atual;
                numero ++;
                out.append(atual).append("\n");
            }
        }
        if (numero == 0)
            return out.append("Ainda nao classificado").toString();
        out.append("Media " + (soma/numero));
        return out.toString();
    }

    /**
     * devolve uma string com media dos RegistoEncomenda ja classificados 
     * caso estejam todos classificar devlove a string "Transportadora ainda nao classificada"
     */
    public String verClassificacaoMedia (){
        int atual = 0;
        double soma = 0;
        int numero = 0;
        for (RegistoEncomenda re : registo){
            atual = re.getClassificacao();
            if (atual == 0){
                continue;
            }
            else {
                soma += atual;
                numero ++;
            }
        }
        if (numero == 0){
            return "Transportadora ainda nao classificada";
        }

        Double media = (soma/numero);
        return media.toString();
    
    }

    
}