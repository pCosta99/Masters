import java.util.List;

public class LojaController {
    public static void controller(SGE sge, Loja l) {

        String lp = "";

        do {
            /*Print menu da loja*/
            View.showLojaMenu();

            /*Pedido de uma opçao*/
            View.showInsercao("uma opçao: ");
            lp = Input.lerString();
            lp = lp.toUpperCase();

            switch (lp) {
                /*1 -> sinalizar que existe uma encomenda para ser entregue*/
                case "1": {
                    if(l.getFilaDeEspera().size() == 0){
                        View.showError("Nao ha encomendas em processamento.");
                        break;
                    }

                    View.showEncomendas(l.getFilaDeEspera());

                    View.showInsercao("uma encomenda: ");
                    String e = Input.lerString();

                    while (!sge.containsE(l.getFilaDeEspera(), e)) {
                        View.showError("Encomenda nao e valida. Tente outra vez.");

                        View.showInsercao("uma encomenda: ");
                        e = Input.lerString();
                    }

                    Encomenda enc = sge.getEncomendas().get(e);

                    l.sinalizaEntrega(enc);

                    break;
                }

                /*Indica a quantidade de pessoas na fila de espera*/
                case "2": {
                    View.showFilaDeRecolha(l);
                    break;
                }

                case "3": {
                    if(l.getRegisto().size() == 0){
                        View.showError("Loja nao tem encomendas registadas");
                        break;
                    }

                    List<Encomenda> list = l.encomendasToList();

                    if(list.size() == 0){
                        View.showError("Nenhuma encomenda foi entregue.");
                        break;
                    }

                    View.showEncomendas(list);

                    break;
                }

                /*Quit*/
                case "Q": {
                    View.showBB();
                    break;
                }

                /*Opçao invalida*/
                default: {
                    View.showError("Opçao Invalida.");
                    break;
                }
            }

        } while (!lp.equals("Q"));
    }
}
