package Controller;

import Model.*;

import java.util.ArrayList;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;

/**
 * Controlador responsável por ações de lojas
 */
public class ControllerLoja extends ControllerMutual implements Controller {
    private String codLoja;

    /**
     * Construtor vazio
     */
    public ControllerLoja() {
        sys = new Sistema();
        codLoja = "n/d";
    }

    /**
     * Construtor parametrizado
     * @param sys sistema a inserir no controlador
     * @param codLoja código da loja que entrou
     */
    public ControllerLoja(Sistema sys, String codLoja) {
        this.sys = sys;
        this.codLoja = codLoja;
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
     * Devolve o código da loja
     * @return string com código da loja
     */
    public String getCodLoja() {
        return codLoja;
    }

    /**
     * Insere o código da loja
     * @param codLoja string com código da loja
     */
    public void setCodLoja(String codLoja) {
        this.codLoja = codLoja;
    }

    /**
     * Transforma as encomendas prontas de uma loja numa lista de strings
     * @param loja loja onde se pretende fazer operações
     * @return encomendas prontas de uma loja
     */
    private List<String> prontas(Loja loja) {
        List<String> ret = new ArrayList<>();
        try {
            ret = loja.getEncsProntas().values().stream().map(Encomenda::toString).collect(Collectors.toList());
            if(ret.size() == 0)
                ret.add("Não tem encomendas prontas.");
            return ret;
        } catch (NullPointerException e) {
            ret.add("Não tem encomendas prontas.");
            return ret;
        }
    }


    /**
     * Executa um comando no sistema
     *
     * @param s informação relativa ao que se pretende executar
     * @return Lista de strings com atualização de estado ou outras informações relevantes
     */
    public List<String> execute(List<String> s) {
        List<String> ret = new ArrayList<>();
        Loja loja = sys.getLoja(codLoja);

        switch (s.get(0)) {
            case "perfil":
                ret.add(loja.toString());
                break;
            case "prontas":
                ret = prontas(loja);
                break;
            case "clientes":
                List<String> ns = loja.getClientes().stream().map(LocalCodeName::toString).map(v -> v.concat("\n")).collect(Collectors.toList());
                ret.addAll(ns);
                if (ns.size() == 0)
                    ret.add("Não tem ninguém na fila de espera.");
                break;
            case "top":
                ret = tops(s.get(1));
                break;
            case "levantar":
                if (s.size() > 1) {
                    try {
                        while (true) {
                            Encomenda e = sys.transfereEncPronta(codLoja);
                            Transporte t = sys.getTransportes().get(e.getTransporte().getCode());
                            t.addFuturaEnc(e);
                            sys.addUser(t);
                        }
                    } catch (NoSuchElementException e) {
                        ret.add("Todas as encomendas estão prontas!");
                    }
                } else {
                    try {
                        Encomenda e = sys.transfereEncPronta(codLoja);
                        Transporte t = sys.getTransportes().get(e.getTransporte().getCode());
                        t.addFuturaEnc(e);
                        sys.addUser(t);
                        ret.add("Encomenda pronta!");
                    } catch (NoSuchElementException e) {
                        ret.add("Não tem mais encomendas ativas.");
                    }
                }
                break;
            case "levantadas":
                List<Encomenda> encs = loja.getEncsProntas().values().stream().filter(enc -> enc.getTransporte().getCode().equals(s.get(1))).collect(Collectors.toList());
                if (encs.size() == 0) {
                    ret.add("Não tem encomendas para esse tranporte.");
                    try {
                        if (s.get(1).charAt(0) == 't') {
                            Transportadora t = sys.getTransportadora(s.get(1));
                            loja.retiraCliente(t);
                        } else if (s.get(1).charAt(0) == 'v') {
                            Voluntario v = sys.getVoluntario(s.get(1));
                            loja.retiraCliente(v);
                        }
                        sys.addUser(loja);
                    }catch (NullPointerException ignored){
                    }
                } else {
                    try {
                        if (s.get(1).charAt(0) == 't') {
                            Transportadora t = sys.getTransportadora(s.get(1));
                            loja.retiraCliente(t);
                            t.addEncomendas(encs);
                            for (Encomenda e : encs) {
                                sys.retiraEncLoja(loja, e.getNumEnc());
                                t.removeFuturaEnc(e.getNumEnc());
                            }
                            sys.addUser(t);
                            sys.addUser(loja);
                            ret.add("Ok. Estado atualizado.");
                        } else if (s.get(1).charAt(0) == 'v') {
                            Voluntario v = sys.getVoluntario(s.get(1));
                            loja.retiraCliente(v);
                            sys.addUser(loja);
                            v.setParaEntregar(encs.get(0));
                            v.removeFuturaEnc(encs.get(0).getNumEnc());
                            sys.addUser(v);
                            sys.addUser(loja);
                            ret.add("Ok. Estado atualizado.");
                        } else {
                            ret.add("Transporte inválido.");
                        }
                    } catch (NullPointerException e) {
                        ret.add("Esse transporte não confirmou a presença na loja.");
                    }
                }
                break;
            case "altera":
                loja.setPartilha(!loja.getPartilha());
                sys.addUser(loja);
            case "partilha":
                if (loja.getPartilha())
                    ret.add("Partilha a fila de espera.");
                else
                    ret.add("Não partilha a fila de espera.");
                break;
            case "ativas":
                //por levantar
                try {
                    ret = loja.getEncs().stream().map(Encomenda::toString).collect(Collectors.toList());
                    if(ret.size() == 0)
                        ret.add("Não tem encomendas ativas.");
                } catch (NullPointerException e) {
                    ret.add("Não tem encomendas ativas.");
                }
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
