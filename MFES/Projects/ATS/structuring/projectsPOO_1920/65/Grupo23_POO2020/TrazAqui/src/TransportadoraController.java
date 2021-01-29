import java.util.List;
import java.util.Map;

public class TransportadoraController {
    public static void controller(SGE sge, Transportadora t) {

        String tp = "";

        do {

            /*Print da disponibilidade*/
            View.showDisponibilidade(t);

            /*Print menu*/
            View.showTransportadoraMenu(t.getTMedico());

            /*Pedido de opçao*/
            View.showInsercao("uma opçao: ");
            tp = Input.lerString();
            tp = tp.toUpperCase();

            switch (tp) {
                case "1": {
                    /*Questao*/
                    View.showSure();

                    /*Pedido de input*/
                    View.showInsercao("S ou N: ");

                    /*Pedido de escolha*/
                    String escolha = Input.lerString();
                    escolha = escolha.toUpperCase();

                    while (!escolha.equals("S") && !escolha.equals("N")) {
                        /*Show de erro*/
                        View.showError("Opçao Invalida. Tente outra vez.");

                        /*Pedido de input*/
                        View.showInsercao("S ou N");

                        /*Nova escolha*/
                        escolha = Input.lerString();
                        escolha = escolha.toUpperCase();

                    }

                    /*Verifica se o user quer mudar a flag*/
                    if (escolha.equals("S")) {
                        t.mudaAvailability();
                    }

                    break;
                }

                /*Determinar o preço de entrega por distancia e tempo na loja*/
                case "2": {
                    if(!t.getAvailability()){
                        View.showError("Transportadora nao esta disponivel para transportes.");
                        break;
                    }

                    /*lojas na zona da transportadora*/
                    List<Loja> lista = sge.nearbyLojas(sge.getLojas(), t);

                    /*Print das lojas*/
                    View.showLojas(lista, t);

                    /*Escolha da loja*/
                    View.showInsercao("uma loja: ");
                    String l = Input.lerString();

                    /*Validaçao*/
                    while(!sge.listContainsLoja(lista, l)){
                        View.showError("Loja Invalida. Tente outra vez");

                        /*Escolha da loja*/
                        View.showInsercao("uma loja: ");
                        l = Input.lerString();
                    }

                    /*Loja em concreto*/
                    Loja loja = sge.getLoja(l);

                    /*Lista das encs normais*/
                    List<Encomenda> encsN = loja.encsN();

                    /*Caso de ser um transporte medico*/
                    if(t.getTMedico()){
                        List <Encomenda> encsM = loja.encsM();
                        View.showEncomendas(encsM, encsN);

                        /*Escolha da Encomenda*/
                        View.showInsercao("uma encomenda: ");
                        String e = Input.lerString();

                        /*Validaçao*/
                        while(!sge.ListContainsEncomenda(encsN, e) && !sge.ListContainsEncomenda(encsM, e)){
                            View.showError("Encomenda invalida tente outra vez");

                            /*Escolha da loja*/
                            View.showInsercao("uma encomenda: ");
                            e = Input.lerString();
                        }

                        /*Encomenda em concreto*/
                        Encomenda enc = sge.getEncomenda(e);

                        /*Utilizador da encomenda*/
                        Utilizador user = sge.getUtilizador(enc.getCodUtilizador());

                        double dist = user.getPosicao().distancia(loja.getPosicao());

                        /*Oferta da transportadora*/
                        user.setOfertasTrans(t.getCodEmpresa() + ","
                                + enc.getCodEncomenda() + "," + sge.calculaPortes(enc, t));

                        /*Print do preço de transporte*/
                        View.showPrecoTransporte(sge.calculaPortes(enc, t));
                    }

                    /*Lista de encomendas da loja no caso de nao ser um transporte medico*/
                    else {
                        View.showEncomendas(null, encsN);

                        /*Escolha da Encomenda*/
                        View.showInsercao("uma encomenda: ");
                        String e = Input.lerString();

                        /*Validaçao*/
                        while(!sge.ListContainsEncomenda(encsN, e)){
                            View.showError("Encomenda invalida tente outra vez");

                            /*Escolha da loja*/
                            View.showInsercao("uma encomenda: ");
                            e = Input.lerString();
                        }

                        /*Encomenda em concreto*/
                        Encomenda enc = sge.getEncomenda(e);

                        /*Utilizador da encomenda*/
                        Utilizador user = sge.getUtilizador(enc.getCodUtilizador());

                        /*Oferta da transportadora*/
                        user.setOfertasTrans(t.getCodEmpresa() + ","
                                + enc.getCodEncomenda() + "," + sge.calculaPortes(enc, t));

                        /*Print do preço de transporte*/
                        View.showPrecoTransporte(sge.calculaPortes(enc, t));
                    }

                    break;
                }

                /*fazer o transporte da encomenda e registar quanto tempo demorou e o custo associado*/
                case "3": {
                    if(t.getQueue().size() == 0){
                        View.showError("Nao ha encomendas para entregar.");
                        break;
                    }

                    Map<String, Double> map = t.fazerEntrega(sge);

                    View.showEncomendasTimerEntrega(map);

                    View.show("Encomendas entregues!");

                    break;
                }

                case "4": {
                    if(t.getRegisto().size() == 0){
                        View.showError("Transportadora nao tem encomendas registadas");
                        break;
                    }

                    List<Encomenda> l = t.encomendasToList();

                    if(l.size() == 0){
                        View.showError("Nenhuma encomenda foi entregue.");
                        break;
                    }

                    View.showEncomendas(l);
                    break;
                }

                case "M": {
                    if(!t.getTMedico()){
                        View.showError("Este veiculo nao e medico.");
                        break;
                    }

                    /*Questao*/
                    View.showSure();

                    /*Pedido de input*/
                    View.showInsercao("S ou N: ");

                    /*Pedido de escolha*/
                    String escolha = Input.lerString();
                    escolha = escolha.toUpperCase();

                    while (!escolha.equals("S") && !escolha.equals("N")) {
                        /*Show de erro*/
                        View.showError("Opçao Invalida. Tente outra vez.");

                        /*Pedido de input*/
                        View.showInsercao("S ou N: ");

                        /*Nova escolha*/
                        escolha = Input.lerString();
                        escolha = escolha.toUpperCase();
                    }

                    /*Verifica se o user quer mudar a flag*/
                    if (escolha.equals("S")) {
                        if(t.aceitoTransporteMedicamentos())
                            t.aceitaMedicamentos(false);

                        else t.aceitaMedicamentos(true);
                    }

                    break;
                }

                /*exit*/
                case "Q": {
                    View.showBB();
                    break;
                }

                default: {
                    View.showError("Opçao Invalida.");
                    break;
                }
            }

        } while (!tp.equals("Q"));
    }
}


