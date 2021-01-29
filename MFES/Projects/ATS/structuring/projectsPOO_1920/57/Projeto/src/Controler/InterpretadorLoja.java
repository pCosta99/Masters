/**
 * Classe que controla o menu da loja
 */
package Controler;

import Model.GestTrazAqui;
import Model.Login;
import View.Apresentacao;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class InterpretadorLoja implements Serializable, IInterpretador {
    private final Input in;


    public InterpretadorLoja() {
        in = new Input();
    }

    /**
     * Metodo que aceita ou não uma encomenda numa loja
     *
     * @param c GestTrazAqui
     * @param a Apresentação
     * @param l Login
     */
    private void aceitarEncomendaLoja(GestTrazAqui c, Apresentacao a, Login l) {
        List<String> encomendas = c.encomendasNaoAceitesLoja(l.getCode());

        if(encomendas.size() == 0) {
            a.printErroSemEncomenda();
            return;
        }

        a.printArray("Compras disponíveis:", encomendas);

        String encCode = in.lerStringSolicitarEnc(a, a.pedirEncomenda(), encomendas);

        if(in.lerSN(a, "Pertende aceitar a compra? (S/N)")) {
            c.aceitarEncomenda(encCode);
            c.addUserNotificacao(c.getEncUser(encCode), a.notificacaoUtilizadorLojaAceite(l.getCode()), 1, "");
            a.printCompraAceite(encCode);
        }
        else {
            c.addUserNotificacao(c.getEncUser(encCode), a.notificacaoUtilizadorLojaRecusado(l.getCode()), 1, "");
            c.removeEncomenda(encCode);
            a.printCompraRecusada(encCode);
        }
    }

    /**
     * Interpretador menu loja
     *
     * @param c GestTrazAqui
     * @param a Apresentação
     * @param l Login
     */
    public void interpretador(GestTrazAqui c, Apresentacao a,Login l) {
        boolean r = true;
        int command;
        String encCode;

        while (r) {
            if(c.hasQueueInfoLoja(l.getCode())) {
                a.printMenuLoja();
                command = (int) in.lerDouble(a,"Escolha a sua opção:", 0, 4);

                switch (command) {
                    case 1:
                        double time = in.lerDouble(a, "Introduza o tempo de espera na fila", 0, 10);
                        c.setStoreQueueTime(l.getCode(), time);
                        break;

                    case 2:
                        int queueSize = (int) in.lerDouble(a, "Introduza o número de pessoas na fila de espera", 0, 10);
                        c.setStoreQueueSize(l.getCode(), queueSize);
                        break;

                    case 3:
                        aceitarEncomendaLoja(c, a, l);
                        break;
                    case 4:
                        a.printArray("Encomendas Loja:\n", new ArrayList<>(c.getEncLoja(l.getCode())));
                        break;

                    case 0:
                        r = false;
                        break;

                    default:
                        a.printErroComandoInvalido();
                }
            }
            else {
                a.printMenuLojaIndisponivel();
                command = (int) in.lerDouble(a,"Escolha a sua opção:", 0, 2);

                switch (command) {
                    case 1:
                        aceitarEncomendaLoja(c, a, l);
                        break;

                    case 2:
                        a.printArray("Encomendas Loja:\n", new ArrayList<>(c.getEncLoja(l.getCode())));
                        break;

                    case 0:
                        r = false;
                        break;

                    default:
                        a.printErroComandoInvalido();
                }
            }
        }
    }
}
