package Controller;

import Exceptions.EncomendaInexistenteException;
import Exceptions.LocalCodeNameInexistenteException;
import Model.*;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Controlador responsável por ações de transportadoras
 */
public class ControllerTransportadora extends ControllerMutual implements Controller {
    private String codTransportadora;

    /**
     * Construtor vazio
     */
    public ControllerTransportadora() {
        sys = null;
        codTransportadora = null;
    }

    /**
     * Construtor parametrizado
     *
     * @param sys               Sistema a inserir no controlador
     * @param codTransportadora código de transportadora
     */
    public ControllerTransportadora(Sistema sys, String codTransportadora) {
        this.sys = sys;
        this.codTransportadora = codTransportadora;
    }

    /**
     * Devolve o Sistema de um Controller
     *
     * @return Sistema do Controller
     */
    public Sistema getSistema() {
        return sys;
    }


    /**
     * Atualiza o Sistema de um Controller
     *
     * @param sys novo sistema para inserir
     */
    public void setSistema(Sistema sys) {
        this.sys = sys;
    }

    /**
     * Devolve o código de uma transportadora
     *
     * @return código de uma transportadora
     */
    public String getCodTransportadora() {
        return codTransportadora;
    }


    /**
     * Atualiza o código de uma transportadora
     *
     * @param codTransportadora código de uma transportadora
     */
    public void setCodTransportadora(String codTransportadora) {
        this.codTransportadora = codTransportadora;
    }

    /**
     * Transforma as encomendas entregues de uma transportadora numa lista de strings
     *
     * @param transportadora transportadora
     * @return Encomendas sob a forma de strings
     * @throws EncomendaInexistenteException Inexistência de encomendas
     */
    private List<String> entregues(Transportadora transportadora) throws EncomendaInexistenteException {
        return transportadora.getEncEntregues().values().stream()
                .flatMap(v -> v.stream().map(Encomenda::toString))
                .collect(Collectors.toList());
    }

    /**
     * Executa um comando no sistema
     *
     * @param s informação relativa ao que se pretende executar
     * @return Lista de strings com atualização de estado ou outras informações relevantes
     */
    public List<String> execute(List<String> s) {
        List<String> ret = new ArrayList<>();
        Transportadora transp = sys.getTransportadora(codTransportadora);

        switch (s.get(0)) {
            case "perfil":
                ret.add(transp.toString());
                break;
            case "entregues":
                try {
                    ret = entregues(transp);
                } catch (EncomendaInexistenteException e) {
                    ret.add("Ainda não entregou encomendas.");
                }
                break;
            case "altera":
                sys.alteraAceitacao(transp);
            case "especiais":
                if (sys.fazEncomendasEspecias(transp))
                    ret.add("Pode entregar encomendas especiais.");
                else
                    ret.add("Não pode entregar encomendas especiais.");
                break;
            case "raio":
                if (s.size() > 1) {
                    try {
                        transp.setRaio(Double.parseDouble(s.get(1)));
                    } catch (Exception e){
                        ret.add("Raio inválido");
                        return ret;
                    }
                    sys.addUser(transp);
                    ret.add("Raio alterado com sucesso!");
                } else {
                    ret.add("O seu raio atual é de " + transp.getRaio() + "Km.");
                }
                break;
            case "lojas":
                try {
                    ret.add("Lojas: ");
                    Set<String> lojas = transp.getFuturasEnc().values().stream().map(e ->"  - " + e.getLoja()).collect(Collectors.toSet());
                    ret.addAll(lojas);
                    if (lojas.size() == 0)
                        ret.add("Não tem encomendas em nenhuma loja.");
                } catch (NullPointerException e) {
                    ret.add("Não tem encomendas em nenhuma loja.");
                }
                break;
            case "encomendas":
                Set<String> encds = transp.getParaEntregar().stream().map(Encomenda::toString).collect(Collectors.toSet());
                ret.addAll(encds);
                if (encds.size() == 0)
                    ret.add("Não tem encomendas para entregar.");
                break;
            case "viagens":
                Set<String> extrato = transp.extratoDeViagem().stream().map(GPS::toString).collect(Collectors.toSet());
                ret.addAll(extrato);
                if (extrato.size() == 0)
                    ret.add("Não fez nenhuma entrega.");
                break;
            case "preco":
                if (s.size() > 1) {
                    try {
                        transp.setPrec_km(Double.parseDouble(s.get(1)));
                        sys.addUser(transp);
                        ret.add("Preço alterado com sucesso!");
                    } catch (Exception e) {
                        ret.add("Valor inválido.");
                    }
                } else {
                    ret.add("Preço por Km: " + transp.getPrec_km() + "€");
                }
                break;
            case "levantar":
                try {
                    ret.add(levantar(transp, s.get(1)));
                    transp.aumentaDistancia(transp.getGps().distGPSKms(sys.getLoja(s.get(1)).getGps()));
                    transp.setGps(sys.getLoja(s.get(1)).getGps());
                    if (transp.getNoCaminho().size() == 0 && transp.getParaEntregar().size() == 0) {
                        transp.trocaDisp();
                    }
                    sys.addUser(transp);
                } catch (LocalCodeNameInexistenteException e) {
                    ret.add("Loja Inexistente.");
                }
                break;
            case "entregar":
                //pega nas encomendas para entregar e decide quais entregar /param cod Encomenda
                List<Utilizador> temp;
                Encomenda tempEnc = null;
                Utilizador tempUtil = null;
                try {
                    tempEnc = transp.getEncParaEntregar(s.get(1));
                    tempUtil = sys.getUtilizador(tempEnc.getNome());
                    List<Encomenda> paraEntregar = transp.getParaEntregar();
                    Utilizador finalTempUtil = tempUtil;
                    temp = paraEntregar.stream()
                            .map(v -> sys.getUtilizador(v.getNome()))
                            .filter(util -> transp.encomendasNoCaminho(finalTempUtil.getGps(), util.getGps()))
                            .collect(Collectors.toList());
                    transp.setNoCaminho(paraEntregar.stream()
                            .filter(enc -> temp.contains(finalTempUtil))
                            .collect(Collectors.toList()));
                    ret.add("Encomendas a entregar pelo caminho:");
                    for (Encomenda e : transp.getNoCaminho()) {
                        ret.add(e.toString());
                        transp.removeEncomenda(e);
                    }
                } catch (EncomendaInexistenteException e) {
                    ret.add("Não tem essa encomenda para entregar.");
                } catch (NullPointerException e) {
                    ret.add("Não tem mais encomendas para entregar.");
                }

                if (transp.getNoCaminho().size() == 0 && transp.getParaEntregar().size() == 0) {
                    transp.trocaDisp();
                }
                sys.addUser(transp);
                break;
            case "espera":
                ret.add(espera(s.get(1)));
                break;
            case "faturacao":
                try {
                    ret.add("Nesse período de tempo faturou " + transp.faturacaoTotal(LocalDateTime.parse(s.get(1)), LocalDateTime.parse(s.get(2))) + "€");
                } catch (EncomendaInexistenteException e) {
                    ret.add("Não faturou nada neste período de tempo.");
                }
                break;
            case "caminho":
                ret.add("Ainda tem estas encomendas para entregar nesta viagem:");
                ret.addAll(transp.getNoCaminho().stream().map(Encomenda::toString).collect(Collectors.toList()));
                break;
            case "confirmar":
                //recebe codigo de enc e confirma entrega
                List<Encomenda> encs = transp.getNoCaminho();
                Iterator<Encomenda> it = encs.iterator();
                boolean found = false;
                Encomenda entregue = null;

                while (it.hasNext() && !found) {
                    entregue = it.next();
                    if (entregue.getNumEnc().equals(s.get(1))) {
                        found = true;
                    }
                }

                if (found) {
                    transp.removeEncomendaCaminho(entregue);
                    entregue.setDataEntrega(LocalDateTime.now());
                    sys.addEncUtilizador(sys.getUtilizador(entregue.getNome()), entregue);
                    ret.add("Entrega confirmada!");
                    transp.addEncEntregue(sys.getUtilizador(entregue.getNome()), entregue);
                    transp.aumentaDistancia(transp.getGps().distGPSKms(sys.getUtilizador(entregue.getNome()).getGps()));
                    transp.setGps(sys.getUtilizador(entregue.getNome()).getGps());
                    sys.addUser(transp);
                } else {
                    ret.add("Encomenda Inválida!");
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

        return ret;
    }
}
