package Views;

public class VoluntarioView implements TrazAquiView {

    /**
     * Método que imprime uma mensagem pré definida.
     */
    @Override
    public void show() {
        System.out.println("                            ███╗   ███╗███████╗███╗   ██╗██╗   ██╗                                            \n" +
                "                           ████╗ ████║██╔════╝████╗  ██║██║   ██║                                            \n" +
                "                           ██╔████╔██║█████╗  ██╔██╗ ██║██║   ██║                                            \n" +
                "                           ██║╚██╔╝██║██╔══╝  ██║╚██╗██║██║   ██║                                            \n" +
                "                           ██║ ╚═╝ ██║███████╗██║ ╚████║╚██████╔╝                                            \n" +
                "                           ╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝ ╚═════╝                                             \n" +
                "                                                                                  \n" +
                "██╗   ██╗ ██████╗ ██╗     ██╗   ██╗███╗   ██╗████████╗ █████╗ ██████╗ ██╗ ██████╗ \n" +
                "██║   ██║██╔═══██╗██║     ██║   ██║████╗  ██║╚══██╔══╝██╔══██╗██╔══██╗██║██╔═══██╗\n" +
                "██║   ██║██║   ██║██║     ██║   ██║██╔██╗ ██║   ██║   ███████║██████╔╝██║██║   ██║\n" +
                "╚██╗ ██╔╝██║   ██║██║     ██║   ██║██║╚██╗██║   ██║   ██╔══██║██╔══██╗██║██║   ██║\n" +
                " ╚████╔╝ ╚██████╔╝███████╗╚██████╔╝██║ ╚████║   ██║   ██║  ██║██║  ██║██║╚██████╔╝\n" +
                "  ╚═══╝   ╚═════╝ ╚══════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝ ╚═════╝ \n");
        System.out.println("1 -> Escolher encomenda para entregar");
        System.out.println("2 -> Consultar encomendas do Sistema para entregar");
        System.out.println("3 -> Alterar disponibilidade");
        System.out.println("4 -> Mostrar classificação");
        System.out.println("G -> Gravar");
        System.out.println("S -> Logout");
    }

    /**
     * Método que imprime um Objeto como uma mensagem, ou imprime uma mensagem de acordo com o valor do objeto.
     * @param o Objeto a avaliar.
     */
    @Override
    public void show(Object o) {
        if (o instanceof String) System.out.print((String) o );
        else if (o instanceof Boolean){
            System.out.println("                     ███╗   ███╗███████╗███╗   ██╗██╗   ██╗                                            \n" +
                    "                     ████╗ ████║██╔════╝████╗  ██║██║   ██║                                            \n" +
                    "                     ██╔████╔██║█████╗  ██╔██╗ ██║██║   ██║                                            \n" +
                    "                     ██║╚██╔╝██║██╔══╝  ██║╚██╗██║██║   ██║                                            \n" +
                    "                     ██║ ╚═╝ ██║███████╗██║ ╚████║╚██████╔╝                                            \n" +
                    "                     ╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝ ╚═════╝                                             \n" +
                    "                                                                                  \n" +
                    "██╗   ██╗ ██████╗ ██╗     ██╗   ██╗███╗   ██╗████████╗ █████╗ ██████╗ ██╗ ██████╗ \n" +
                    "██║   ██║██╔═══██╗██║     ██║   ██║████╗  ██║╚══██╔══╝██╔══██╗██╔══██╗██║██╔═══██╗\n" +
                    "██║   ██║██║   ██║██║     ██║   ██║██╔██╗ ██║   ██║   ███████║██████╔╝██║██║   ██║\n" +
                    "╚██╗ ██╔╝██║   ██║██║     ██║   ██║██║╚██╗██║   ██║   ██╔══██║██╔══██╗██║██║   ██║\n" +
                    " ╚████╔╝ ╚██████╔╝███████╗╚██████╔╝██║ ╚████║   ██║   ██║  ██║██║  ██║██║╚██████╔╝\n" +
                    "  ╚═══╝   ╚═════╝ ╚══════╝ ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝ ╚═════╝ \n");
            System.out.print("                          Disponibilidade : ");
            if((boolean)o)System.out.println("Disponível");
            else System.out.println("Não Disponível");
            System.out.println("            ┌──────────────────────────────────────────────────────┐");
            System.out.println("            |  1 -> Escolher encomenda para entregar               |");
            System.out.println("            |  2 -> Consultar encomendas do Sistema para entregar  |");
            System.out.println("            |  3 -> Alterar disponibilidade                        |");
            System.out.println("            |  4 -> Mostrar classificação                          |");
            System.out.println("            |  G -> Gravar                                         |");
            System.out.println("            |  S -> Logout                                         |");
            System.out.println("            └──────────────────────────────────────────────────────┘");
            System.out.print("Option: ");
        }
    }
}
