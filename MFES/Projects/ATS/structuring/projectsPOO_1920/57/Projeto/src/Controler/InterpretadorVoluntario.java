/**
 * Classe que controla o menu do voluntário
 */
package Controler;

import Model.GestTrazAqui;
import Model.Login;
import View.Apresentacao;

import java.io.Serializable;
import java.time.LocalDateTime;

public class InterpretadorVoluntario implements Serializable, IInterpretador{
    private final Input in;


    public InterpretadorVoluntario() {
        in = new Input();
    }

    /**
     * interpretador menu voluntario
     *
     * @param c GestTrazAqui
     * @param a Apresentação
     * @param l Login
     */
    public void interpretador(GestTrazAqui c,Apresentacao a, Login l) {
        boolean r = true;
        int command;
        String encCode;

        while (r) {
            a.printMenuVoluntario();
            command = (int) in.lerDouble(a,"Escolha a sua opção:", 0, 4);

            switch (command) {
                case 1:
                    if(c.isEstafetaFree(l.getCode())) {
                        c.setEstafetaFree(l.getCode(), false);
                        a.printEstafetaIndisponivel();
                    }
                    else {
                        c.setEstafetaFree(l.getCode(), true);
                        a.printEstafetaDisponivel();
                    }
                    break;

                case 2:
                    if(!c.isEstafetaFree(l.getCode()))
                        a.printEstafetaIndisponivel();
                    else {
                        encCode = c.encomendaStandBy(l.getCode());
                        if (!encCode.equals("")) {
                            c.removeUserStandBy(c.getEncUser(encCode), encCode);
                            if (in.lerSN(a, "Pretender aceitar a entrega da encomenda " + encCode + " ao utilizador " + c.getEncUser(encCode) + "(S/N)")) {
                                c.entregarEncomenda(encCode, l.getCode());
                                a.printEncomendaEntregueVol(c.getEncUser(encCode), c.getEncUserName(encCode), c.getEncTime(encCode));
                                c.addUserNotificacao(c.getEncUser(encCode), a.notificacaoUtilizadorEntregaVoluntario(l.getCode(), encCode), 2, l.getCode());
                            } else {
                                c.removerEnc(l.getCode(), encCode);
                                a.printEncRecusada();
                                c.sugerirTransp(encCode, "");

                                c.addUserNotificacao(c.getEncUser(encCode), a.notificacaoUtilizadorVoluntarioRecusado(l.getCode()), 1, "");
                            }
                            c.setEstafetaOccup(l.getCode(), false);
                        } else
                            a.printSemEncomendas();
                    }
                    break;

                case 3:
                    c.changeMedic(l.getCode());
                    break;

                case 4:
                    LocalDateTime min = in.lerData(a,a.pedirPrimeiraData());
                    LocalDateTime max = in.lerData(a,a.pedirSegundaData());
                    a.printEncomendas("Lista de Entregas da Transportadora", c.getEncomendasEstafeta(l.getCode(),min,max));
                    break;

                case 5:
                    a.printEstafetaClassicacao(c.getEstafetaClassificacao(l.getCode()));
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
