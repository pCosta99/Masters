import Controller.*;
import Model.*;
import View.*;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;

public class App {
    public static void main(String[] args) {
        Sistema sys = new Sistema();
        Controller cnt = new ControllerLogin();
        View view = new View();
        Leitura leit = new Leitura();

        try {
            leit.lerFicheiro("data/logs");
        } catch (IOException e) {
            e.printStackTrace();
        }

        Map<String, Utilizador> utilizadorMap = leit.getUtilizadores().stream()
                .collect(Collectors.toMap(LocalCodeName::getCode, v -> v));
        Map<String, Transporte> transportes = leit.getTransportadoras().stream()
                .collect(Collectors.toMap(LocalCodeName::getCode, v -> v));
        Map<String, Transporte> voluntarios = leit.getVoluntarios().stream()
                .collect(Collectors.toMap(LocalCodeName::getCode, v -> v));

        transportes.putAll(voluntarios);

        Map<String, Loja> lojas = leit.getLojas().stream()
                .collect(Collectors.toMap(LocalCodeName::getCode, v -> v));
        Map<String, Login> logins = new HashMap<>();

        for (String key : utilizadorMap.keySet()) {
            Login log = new Login(key, "n/d", key);
            logins.put(key, log);
        }

        for (String key : transportes.keySet()) {
            Login log = new Login(key, "n/d", key);
            logins.put(key, log);
        }

        for (String key : lojas.keySet()) {
            Login log = new Login(key, "n/d", key);
            logins.put(key, log);
        }
        Random r = new Random();
        for (Transporte v : transportes.values()) {
            v.aceitaMedicamentos(r.nextBoolean());
        }

        sys.setLoja(lojas);
        sys.setTransportes(transportes);
        sys.setUtilizadores(utilizadorMap);
        sys.setLogin(logins);



        List<String> encAceites = leit.getEncomendasAceites();
        Map<String, Set<Encomenda>> enc = leit.getEncomendas();
        for (Map.Entry<String, Set<Encomenda>> pair : enc.entrySet()) {
            Loja l = sys.getLoja(pair.getKey());
            for (Encomenda e : pair.getValue()) {
                boolean med = r.nextBoolean();
                Utilizador temp = sys.getUtilizador(e.getNome());
                List<Transporte> transp = new ArrayList<>(sys.recomendaTrans(temp.getGps(), med));
                Transporte tempTr = transp.get((int) Math.abs(Math.round(Math.random() * (transp.size() - 1))));
                e.setTransporte(tempTr);
                if (encAceites.contains(e.getNumEnc())) {
                    if (tempTr instanceof Transportadora) {
                        Transportadora trans = (Transportadora) tempTr;
                        double distTotal = trans.getGps().distGPSKms(l.getGps()) + trans.getGps().distGPSKms(sys.getUtilizador(e.getNome()).getGps());
                        double precoTotal = e.geraPreco() + (distTotal * trans.getPrec_km()) + l.tempoDeEspera() * 0.1;
                        e.setPreco(precoTotal);
                        sys.addEncTrans((Transportadora) tempTr, e);
                    } else {
                        e.geraPreco();
                        sys.addEncTrans((Voluntario) tempTr, e);
                    }
                } else {

                    e.geraPreco();
                    e.setContainsMed(med);
                    e.setLoja(l.getCode());
                    sys.addEncLoja(l, e);
                }
                temp.addEnc(e);
                sys.addUser(temp);


            }
        }

        cnt.setSistema(sys);

        view.setController(cnt);

        view.run();
    }
}
