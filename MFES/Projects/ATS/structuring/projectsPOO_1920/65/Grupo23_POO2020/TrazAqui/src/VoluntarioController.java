import java.time.LocalDateTime;
import java.util.List;

public class VoluntarioController {
    public static void controller(SGE sge, Voluntario v) {

        String vp = "";

        /*Cases*/
        do {
            /*Print do menu*/
            View.showVoluntarioMenu(v.getTMedico());

            /*Pedido de opçao*/
            View.showInsercao("uma opçao: ");

            vp = Input.lerString();
            vp = vp.toUpperCase();

            switch (vp) {

                /*sinalizar que estão dispostos para recolher encomendas*/
                case "1": {
                    View.showVoluntarioAvailability(v);

                    /*Pedido de input*/
                    View.showInsercao("S ou N: ");

                    /*Pedido de escolha*/
                    String escolha = Input.lerString();
                    escolha = escolha.toUpperCase();

                    while (!escolha.equals("S") && !escolha.equals("N")) {
                        /*Show de erro*/
                        View.showError("Opçao Invalido");

                        /*Pedido de input*/
                        View.showInsercao("S ou N: ");

                        /*Nova escolha*/
                        escolha = Input.lerString();
                        escolha = escolha.toUpperCase();
                    }
                    if (escolha.equals("S")) {
                        v.mudaAvailability();
                    }

                    break;
                }

                /*escolher ir buscar uma encomenda de um utilizador que é disponibilizada por uma loja*/
                case "2": {

                    /*Verifica se o Voluntario ja tem uma encomenda*/
                    if(v.getEncomenda() != null){
                        View.showError("Voluntario ja possui uma encomenda.");
                        break;
                    }

                    /*Verifica se o Voluntario nao esta disponivel*/
                    if (!v.getDisponivel()) {
                        View.showError("Voluntario nao esta disponivel para entrega.");
                        break;
                    }

                    /*Lista de lojas disponiveis na zona*/
                    List<Loja> lojas = sge.nearbyLojas(sge.getLojas(), v);

                    if(lojas.size() == 0){
                        View.showError("Nao ha Lojas nearby com encomendas para entregar.");
                        break;
                    }

                    System.out.println();

                    /*Print das lojas*/
                    View.showLojas(lojas, v);

                    System.out.println();

                    /*Pedido de loja*/
                    View.showInsercao("uma loja: ");
                    String l = Input.lerString();

                    while(!sge.listContainsLoja(lojas, l)){
                        View.showError("Loja invalida. Tente outra vez.");

                        /*Pedido de loja*/
                        View.showInsercao("uma loja: ");
                        l = Input.lerString();
                    }

                    /*Loja em concreto*/
                    Loja loja = sge.getLoja(l);

                    /*Lista de Encomendas na loja*/
                    List<Encomenda> encsN = loja.encsN();

                    /*Caso de ser transporte medico*/
                    if(v.getTMedico()){
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

                        /*Atualiza o codigo de entregador na encomenda*/
                        enc.setCodEntregador(v.getCodVoluntario());

                        /*Atualizar data saida*/
                        enc.setDataDeSaida(LocalDateTime.now());

                        /*Encomenda no voluntario*/
                        v.setEncomenda(enc);

                        /*Encomenda no registo do voluntario*/
                        v.addRegisto(enc);

                        /*Remoçao da encomenda da loja*/
                        loja.getFilaDeRecolha().remove(enc.getCodEncomenda());

                        /*Mudança de flag no voluntario*/
                        v.mudaAvailability();

                        /*Mudança de flag no voluntario*/
                        v.aceitaMedicamentos(false);

                    }

                    else{
                        /*Print da lista de encomendas*/
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

                        /*Set do codigo de entregador*/
                        enc.setCodEntregador(v.getCodVoluntario());

                        /*Atualizar data saida*/
                        enc.setDataDeSaida(LocalDateTime.now());

                        /*Encomenda no voluntario*/
                        v.setEncomenda(enc);

                        /*Encomenda no registo do voluntario*/
                        v.addRegisto(enc);

                        /*Remoçao da encomenda da loja*/
                        loja.getFilaDeRecolha().remove(enc.getCodEncomenda());

                        /*Mudança de flag no voluntario*/
                        v.mudaAvailability();
                    }

                    break;
                }

                /*fazer o transporte da encomenda e registar quanto tempo demorou*/
                case "3": {
                    if(v.getEncomenda() == null){
                        View.showError("Nao ha encomenda para entregar");
                        break;
                    }

                    /*Loja da encomenda*/
                    Loja l = sge.getLojas().get(v.getEncomenda().getCodLoja());

                    /*Utilizador da encomenda*/
                    Utilizador u = sge.getUtilizadores().get(v.getEncomenda().getCodUtilizador());

                    double temp = v.fazTrans(l,u);

                    View.showIntervaloEntrega(temp);

                    View.show("Encomenda entregue!");

                    break;
                }

                case "4": {
                    if(v.getRegisto().size() == 0){
                        View.showError("Voluntario nao tem encomendas registadas");
                        break;
                    }

                    List<Encomenda> l = v.encomendasToList();

                    if(l.size() == 0){
                        View.showError("Nenhuma encomenda foi entregue.");
                        break;
                    }

                    View.showEncomendas(l);

                    break;
                }

                case "M": {
                    if(!v.getTMedico()){
                        View.showError("Este veiculo nao e medico.");
                        break;
                    }

                    View.showVoluntarioMAvailability(v);

                    /*Pedido de input*/
                    View.showInsercao("S ou N: ");

                    /*Pedido de escolha*/
                    String escolha = Input.lerString();
                    escolha = escolha.toUpperCase();

                    while (!escolha.equals("S") && !escolha.equals("N")) {
                        /*Show de erro*/
                        View.showError("Opçao Invalido");

                        /*Pedido de input*/
                        View.showInsercao("S ou N");

                        /*Nova escolha*/
                        escolha = Input.lerString();
                        escolha = escolha.toUpperCase();
                    }

                    if (escolha.equals("S")) {
                        if(v.aceitoTransporteMedicamentos()){
                            v.aceitaMedicamentos(false);
                        }
                        else {
                            v.aceitaMedicamentos(true);
                        }
                    }

                    break;
                }

                /*Exit*/
                case "Q": {
                    View.showBB();
                    break;
                }

                /*Opçao Invalida*/
                default: {
                    View.showError("Opçao Invalida.");
                    break;
                }
            }
        } while (!vp.equals("Q"));
    }
}
