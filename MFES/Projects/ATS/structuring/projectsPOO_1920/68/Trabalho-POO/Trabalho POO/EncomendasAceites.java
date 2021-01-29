import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;

public class EncomendasAceites implements Serializable {
    private List<Encomenda> encomendas_aceites;

    /**
     * Retorna o código de uma transportadora de forma aletória . Verifica se a Encomenda inclui produtos médicos, pois se
     * assim for, são selecionadas como possíveis transportadoras aquelas que possuem certificado. Calcula o número de
     * transportadoras que existem no Sistema, reune numa lista os códigos das transportadoras, gera um número inteiro
     * aleaório entre 0 e o número de transportadoras existentes e retorna o elemento na lista de códigos nessa posição aleatória.
     *
     * @param transportadoras um mapeamento entre os códigos das transportadoras e as Transportadoras.
     * @return código da transportadora que vai entregar a Encomenda
     */

    public String atribuiTransportadoraAleatoriamente (Map<String, Transportadora> transportadoras, Encomenda e) throws TransportadorasIndisponíveis {
        if (e.isMed()) {
            List<String> empresas = transportadoras.entrySet().stream().filter(t -> t.getValue().aceitoTransporteMedicamentos()).
                                    filter(t -> t.getValue().isTransportesDisponiveis() && t.getValue().aceitaEncomenda(e)).map(Map.Entry::getKey).collect(Collectors.toList());
            int num_transportadoras = empresas.size();
            if (num_transportadoras == 0) {
                throw new TransportadorasIndisponíveis();
            }
            Random gerador = new Random();
            return empresas.get(gerador.nextInt(num_transportadoras));

        } else {
            List<String> empresas = transportadoras.entrySet().stream().filter(t -> t.getValue().isTransportesDisponiveis() && t.getValue().aceitaEncomenda(e)).map(Map.Entry::getKey).collect(Collectors.toList());
            int num_transportadoras = empresas.size();
            if (num_transportadoras == 0) {
                throw new TransportadorasIndisponíveis();
            }
            Random gerador = new Random();
            return empresas.get(gerador.nextInt(num_transportadoras));
        }
    }

    public String atribuiVoluntarioAleatoriamente (Map<String, Voluntario> voluntarios, Encomenda e) throws VoluntariosIndisponíveis {
        if (e.isMed()) {
            List<String> voluntarios_disponiveis = voluntarios.entrySet().stream().filter(t -> t.getValue().aceitoTransporteMedicamentos()).
                                                    filter(t -> t.getValue().estaDisponivel() && t.getValue().aceitaEncomenda(e)).map(Map.Entry::getKey).collect(Collectors.toList());

            int num_voluntarios_disponiveis = voluntarios_disponiveis.size();
            if (num_voluntarios_disponiveis == 0) {
                throw new VoluntariosIndisponíveis();
            }
            Random gerador = new Random();
            return voluntarios_disponiveis.get(gerador.nextInt(num_voluntarios_disponiveis));

        } else {

            List<String> voluntarios_disponiveis = voluntarios.entrySet().stream().filter(t -> t.getValue().estaDisponivel() && t.getValue().aceitaEncomenda(e)).map(Map.Entry::getKey).collect(Collectors.toList());
            int num_voluntarios_disponiveis = voluntarios_disponiveis.size();

            if (num_voluntarios_disponiveis == 0) {
                throw new VoluntariosIndisponíveis();
            }
            Random gerador = new Random();
            return voluntarios_disponiveis.get(gerador.nextInt(num_voluntarios_disponiveis));
        }
    }


    public Map<String,Encomenda> atribuiTransportes(Map<String,Voluntario> voluntarios, Map<String,Transportadora> transportadoras) throws TransportesIndisponíveis {
        Map<String,Encomenda> res = new HashMap();
        for (Encomenda e : this.encomendas_aceites) {
            try {
                String cod = atribuiTransportadoraAleatoriamente (transportadoras, e);
                res.put(cod, e);
                res.remove(cod,e);

            } catch (TransportadorasIndisponíveis exception2) {
                throw new TransportesIndisponíveis();
            }
        }
        return res;
    }


}
