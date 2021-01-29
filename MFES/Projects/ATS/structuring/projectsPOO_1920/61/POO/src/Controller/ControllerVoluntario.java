package Controller;

import Exceptions.EncomendaInexistenteException;
import Exceptions.LocalCodeNameInexistenteException;
import Model.*;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;


/**
 * Controlador responsável por ações de voluntários
 */
public class ControllerVoluntario extends ControllerMutual implements Controller {
    private String codVoluntario;

    /**
     * Construtor vazio
     */
    public  ControllerVoluntario(){
        sys = new Sistema();
        codVoluntario = "n/d";
    }

    /**
     * Construtor parametrizado
     * @param sys sistema para atualizar
     * @param codVoluntario string com código de voluntario
     */
    public ControllerVoluntario(Sistema sys, String codVoluntario) {
        this.sys = sys;
        this.codVoluntario = codVoluntario;
    }

    /**
     * Devolve o código do voluntário
     * @return string com código de voluntário
     */
    public String getCodVoluntario() {
        return codVoluntario;
    }

    /**
     * Devolve o Sistema de um Controller
     * @return Sistema do Controller
     */
    public Sistema getSistema() {
        return sys;
    }


    /**
     * Atualiza o Sistema de um Controller
     * @param sys novo sistema para inserir
     */
    public void setSistema(Sistema sys) {
        this.sys = sys;
    }

    /**
     * Atualiza o código do voluntário
     * @param codVoluntario string com código de voluntário
     */
    public void setCodVoluntario(String codVoluntario) {
        this.codVoluntario = codVoluntario;
    }

    /**
     * Converte as encomendas entregues de um voluntário numa lista de string
     * @param util voluntario
     * @return Conversão de encomendas entregues em lista de string
     * @throws EncomendaInexistenteException
     */
    private List<String> entregues(Voluntario util) throws EncomendaInexistenteException {
        return util.getEncEntregues().entrySet().stream().map(v -> v.getKey().toString() + " " + v.getValue().toString()).collect(Collectors.toList());
    }

    /**
     * Devolve a encomenda de um voluntário em caso de sucesso
     * @param vol voluntario
     * @return String com uma encomenda
     */
    private String entregar(Voluntario vol){
        try {
            return vol.getParaEntregar().toString();
        } catch (EncomendaInexistenteException e) {
            return "Não tem encomendas para entregar.";
        }

    }

    /**
     * Executa um comando no sistema
     * @param s informação relativa ao que se pretende executar
     * @return Lista de strings com atualização de estado ou outras informações relevantes
     */
    public List<String> execute(List<String> s) {
        List<String> ret = new ArrayList<>();
        Voluntario vol = sys.getVoluntario(codVoluntario);

        switch (s.get(0)){
            case "perfil":
                ret.add(vol.toString());
                break;
            case "entregues":
                try {
                    ret = entregues(vol);
                    if(ret.size() == 0){
                        ret.add("Ainda não entregou encomendas.");
                    }
                } catch (NullPointerException | EncomendaInexistenteException e) {
                    ret.add("Ainda não entregou encomendas.");
                }
                break;
            case "espera":
                ret.add(espera(s.get(1)));
                break;
            case "entregar":
                ret.add(entregar(vol));
                break;
            case "encomenda":
                try {
                    ret.add(vol.getParaEntregar().toString());
                } catch (EncomendaInexistenteException e) {
                    ret.add("Não tem encomendas para entregar.");
                }
                break;
            case "lojas":
                try {
                    ret.add("Lojas: ");
                    Set<String> lojas = vol.getFuturasEnc().values().stream().map(e ->"  - " + e.getLoja()).collect(Collectors.toSet());
                    ret.addAll(lojas);
                    if (lojas.size() == 0)
                        ret.add("Não tem encomendas em nenhuma loja.");
                } catch (NullPointerException e) {
                    ret.add("Não tem encomendas em nenhuma loja.");
                }
                break;
            case "raio":
                if (s.size() > 1) {
                    try {
                        vol.setRaio(Double.parseDouble(s.get(1)));
                    } catch (Exception e){
                        ret.add("Valor inválido.");
                        return ret;
                    }
                    sys.addUser(vol);
                    ret.add("Raio alterado com sucesso!");
                } else {
                    ret.add("O seu raio atual é de " + vol.getRaio() + "Km.");
                }
                break;
            case "confirmar":
                Encomenda c = null;
                try {
                    c = vol.getParaEntregar();
                    c.setDataEntrega(LocalDateTime.now());
                    sys.addEncUtilizador(sys.getUtilizador(c.getNome()), c);
                    vol.addEncEntregue(sys.getUtilizador(c.getNome()),c);
                    vol.setGps(sys.getUtilizador(c.getNome()).getGps());
                    vol.setParaEntregar(null);
                    vol.trocaDisp();
                    ret.add("Entrega confirmada!");
                } catch (EncomendaInexistenteException e) {
                    ret.add("Não tem encomendas para entregar.");
                }

                break;
            case "altera":
                sys.alteraAceitacao(vol);
            case "especiais":
                if(sys.fazEncomendasEspecias(vol))
                    ret.add("Pode fazer encomendas especiais.");
                else
                    ret.add("Não pode fazer encomendas especiais.");
                break;
            case "levantar":
                Encomenda temp = null;
                try {
                    temp = vol.getFuturasEnc().values().stream().findFirst().orElse(null);
                    if(temp == null)
                        throw new EncomendaInexistenteException();
                    ret.add(levantar(vol,temp.getLoja()));
                    vol.trocaDisp();
                } catch (EncomendaInexistenteException e) {
                    ret.add("Não tem encomendas para entregar.");
                } catch (LocalCodeNameInexistenteException e) {
                    ret.add("Loja Inexistente.");
                }
                break;
            case "top":
                ret = tops(s.get(1));
                break;
            case "logout":
                ret.add("logout");
                break;
            default:
                ret.add("Comando Inválido!");

        }

        sys.addUser(vol);
        return ret;
    }
}
