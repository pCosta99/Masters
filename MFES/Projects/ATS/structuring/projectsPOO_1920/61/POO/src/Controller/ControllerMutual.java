package Controller;

import Exceptions.LocalCodeNameInexistenteException;
import Model.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;


/**
 * Classe responsável pela acumulação de código pertencente a vários controllers
 */
public abstract class ControllerMutual {
    protected Sistema sys;

    /**
     * Converte um top numa Lista de String
     *
     * @param s informção sobre o tipo de top que se prentende
     * @return Lista de String
     */
    protected List<String> tops(String s) {
        List<String> ret = new ArrayList<>();
        switch (s) {
            case "user":
                ret = sys.top10U().stream().map(v -> v.getNome() + " com " + v.nEncsBought() + " encomendas.").collect(Collectors.toList());
                break;
            case "transp":
                ret = sys.top10T().stream().map(v -> v.getNome() + " com " + v.getTotal_km() + " Kms.").collect(Collectors.toList());
                break;
            default:
                ret.add("Comando Inválido!");
        }

        return ret;
    }

    /**
     * Adiciona um transporte a uma loja e devolve uma mensagem
     *
     * @param t   transporte a adicionar a uma loja
     * @param loj código de loja
     * @return Atualização de estado
     * @throws LocalCodeNameInexistenteException Loja Inexistente
     */
    protected String levantar(Transporte t, String loj) throws LocalCodeNameInexistenteException {
        try {
            Loja l = sys.getLoja(loj);
        } catch (NullPointerException e) {
            throw new LocalCodeNameInexistenteException("Loja inexistente!");
        }
        if (sys.addTrans(loj, t)) {
            return "Ok! Loja informada!";
        } else {
            return "Já está na loja.";
        }
    }

    /**
     * Devolve o tempo de espera uma loja
     * @param s código de uma loja
     * @return String com o tempo de espera de uma loja
     */
    protected String espera(String s) {
        String ret = null;
        try {
            if (sys.getLoja(s).getPartilha()) {
                ret = ("Tempo de espera: " + sys.esperaNaLoja(s) + " minutos.");
            } else {
                ret = ("Esta loja não partilha tempo de espera.");
            }
        } catch (NullPointerException e) {
            ret = ("Loja inválida.");
        }
        return ret;
    }
}
