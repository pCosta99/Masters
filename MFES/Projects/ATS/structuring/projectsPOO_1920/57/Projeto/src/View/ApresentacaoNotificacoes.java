package View;

import java.io.Serializable;

public class ApresentacaoNotificacoes implements Serializable {

    /**
     * Apresenta menu de notificações
     * @param not   notificação
     * @param type  type
     * @param page  página
     * @param max   página max
     */
    public void notifTable(String not, int type, int page, int max) {
        String not2 = "";
        if(not.length() > 55) {
            not2 = not.substring(55);
            not = not.substring(0, 55) + "\n";
        }

        System.out.println("-------------------------------------------------------");
        System.out.println("                  Notificação ("+(page+1)+"/"+max+")");
        System.out.println("-------------------------------------------------------");
        System.out.print(not);
        System.out.println(not2);
        System.out.println("-------------------------------------------------------");
        if(type == 2)
            System.out.println("[1] Próxima | [2] Anterior | [3] Classificar | [0] Sair");
        else
            System.out.println("         [1] Próxima | [2] Anterior | [0] Sair");
        System.out.println("-------------------------------------------------------");
    }

    /**
     * Apresentação sem notificações
     */
    public void printEmptyNot() {
        System.out.println("O utilizador não tem notificações");
    }

    /**
     * Retorna notificação utilizador de loja aceite
     * @param storeCode storeCode
     * @return          notificaçao
     */
    public String notificacaoUtilizadorLojaAceite(String storeCode) {
        return "A loja " + storeCode + " aceitou a sua compra.\nSolicite o levantamento da encomenda";
    }

    /**
     * Retorna notificação utilizador de loja recusada
     * @param storeCode storeCode
     * @return          notificaçao
     */
    public String notificacaoUtilizadorLojaRecusado(String storeCode) {
        return "A loja " + storeCode + " recusou a sua compra.\nVolte a realizar o pedido";
    }

    /**
     * Retorna notificação utilizador de voluntário recusado
     * @param code codigo voluntário
     * @return          notificaçao
     */
    public String notificacaoUtilizadorVoluntarioRecusado(String code) {
        return "O voluntário " + code + " recusou a sua encomenda.\nVolte a solicitar a encomenda";
    }

    /**
     * Retorna notificação utilizador de entrega da transportadora
     * @param transCode transpCode
     * @param encCode   encCode
     * @return          notificação
     */
    public String notificacaoUtilizadorEntregaTransportadora(String transCode, String encCode) {
        return "Entrega da encomenda " + encCode + " realizada com sucesso pela transportadora " + transCode;
    }

    /**
     * Retorna notificação utilizador de entrega de voluntario
     * @param transCode transpCode
     * @param encCode   encCode
     * @return          notificação
     */
    public String notificacaoUtilizadorEntregaVoluntario(String transCode, String encCode) {
        return "Entrega da encomenda " + encCode + " realizada com sucesso pelo voluntario " + transCode;
    }

    /**
     * Retorna notificação voluntário de nova entrega
     * @param encCode   encCode
     * @param userCode  userCode
     * @return          notificação
     */
    public String notificacaoVoluntarioNovaEntrega(String encCode, String userCode) {
        return "Tem uma entrega pendente (" + encCode + ") do utilizador " + userCode + ".";
    }

    /**
     * Retorna notificação loja de nova compra
     * @param encCode   encCode
     * @param userCode  userCode
     * @return          notificação
     */
    public String notificacaoLojaNovaCompra(String encCode, String userCode) {
        return "Tem uma compra pendente (" + encCode + ") do utilizador " + userCode + ".";
    }

    /**
     * Retorna notificação user de transportadora sugerida
     * @param encCode       encCode
     * @param transpCode    transpCode
     * @return              notificação
     */
    public String notificacaoUserNovaTransportadora(String encCode, String transpCode) {
        return "Tem um entrega sugerida (" + encCode + ") da transportadora " + transpCode + ".";
    }

    /**
     * Retorna notificação transportadora aceite
     * @param encCode   encCode
     * @param userCode  userCode
     * @return          notificação
     */
    public String notificacaoTransportadoraAceite(String encCode, String userCode) {
        return "O utilizador " + userCode + " aceitou o serviço de entrega da encomenda (" + encCode + ").";
    }

    /**
     * Retorna notificação transportadora recusada
     * @param encCode   encCode
     * @param userCode  userCode
     * @return          notificação
     */
    public String notificacaoTransportadoraRecusada(String encCode, String userCode) {
        return "O utilizador " + userCode + " rejeitou o serviço de entrega da encomenda (" + encCode + ").";
    }
}
