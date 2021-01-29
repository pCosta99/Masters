package Views;

public class LojaView implements TrazAquiView {

    /**
     * Método que imprime uma mensagem pré definida.
     */
    @Override
    public void show() {
        System.out.println("   ███╗   ███╗███████╗███╗   ██╗██╗   ██╗\n" +
                "   ████╗ ████║██╔════╝████╗  ██║██║   ██║\n" +
                "   ██╔████╔██║█████╗  ██╔██╗ ██║██║   ██║\n" +
                "   ██║╚██╔╝██║██╔══╝  ██║╚██╗██║██║   ██║\n" +
                "   ██║ ╚═╝ ██║███████╗██║ ╚████║╚██████╔╝\n" +
                "   ╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝ ╚═════╝ \n" +
                "                                      \n" +
                "      ██╗      ██████╗      ██╗ █████╗      \n" +
                "      ██║     ██╔═══██╗     ██║██╔══██╗     \n" +
                "      ██║     ██║   ██║     ██║███████║     \n" +
                "      ██║     ██║   ██║██   ██║██╔══██║     \n" +
                "      ███████╗╚██████╔╝╚█████╔╝██║  ██║     \n" +
                "      ╚══════╝ ╚═════╝  ╚════╝ ╚═╝  ╚═╝     \n");
        System.out.println(" ┌───────────────────────────────────────────┐");
        System.out.println(" |   1 -> Ver encomendas prontas a entregar  |");
        System.out.println(" |   2 -> Consultar stock                    |");
        System.out.println(" |   3 -> Atualizar informações              |");
        System.out.println(" |   G -> Gravar                             |");
        System.out.println(" |   S -> Logout                             |");
        System.out.println(" └───────────────────────────────────────────┘");
        System.out.print("Option: ");
    }

    /**
     * Método que imprime um Objeto como uma mensagem, ou imprime uma mensagem de acordo com o valor do objeto.
     * @param o Objeto a avaliar.
     */
    @Override
    public void show(Object o) {
        if (o instanceof String) System.out.print((String) o );
        else if (o instanceof Boolean){
            System.out.println("   ███╗   ███╗███████╗███╗   ██╗██╗   ██╗\n" +
                    "   ████╗ ████║██╔════╝████╗  ██║██║   ██║\n" +
                    "   ██╔████╔██║█████╗  ██╔██╗ ██║██║   ██║\n" +
                    "   ██║╚██╔╝██║██╔══╝  ██║╚██╗██║██║   ██║\n" +
                    "   ██║ ╚═╝ ██║███████╗██║ ╚████║╚██████╔╝\n" +
                    "   ╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝ ╚═════╝ \n" +
                    "                                      \n" +
                    "      ██╗      ██████╗      ██╗ █████╗      \n" +
                    "      ██║     ██╔═══██╗     ██║██╔══██╗     \n" +
                    "      ██║     ██║   ██║     ██║███████║     \n" +
                    "      ██║     ██║   ██║██   ██║██╔══██║     \n" +
                    "      ███████╗╚██████╔╝╚█████╔╝██║  ██║     \n" +
                    "      ╚══════╝ ╚═════╝  ╚════╝ ╚═╝  ╚═╝     \n");
            System.out.print("              Informa : ");
            if((boolean)o){
                System.out.println("Sim");
                System.out.println(" ┌───────────────────────────────────────────┐");
                System.out.println(" |  1 -> Informar sobre a loja               |");
                System.out.println(" |  2 -> Adicionar produto ao stock          |");
                System.out.println(" |  3 -> Alterar tempo médio de atendimento  |");
                System.out.println(" |  S -> Retroceder                          |");
                System.out.println(" └───────────────────────────────────────────┘");
                System.out.print("Option: ");
            }
            else {
                System.out.println("Não");
                System.out.println(" ┌───────────────────────────────────────────┐");
                System.out.println(" |  1 -> Informar sobre a loja               |");
                System.out.println(" |  2 -> Adicionar produto ao stock          |");
                System.out.println(" |  S -> Retroceder                          |");
                System.out.println(" └───────────────────────────────────────────┘");
                System.out.print("Option: ");
            }
        }
    }
}
