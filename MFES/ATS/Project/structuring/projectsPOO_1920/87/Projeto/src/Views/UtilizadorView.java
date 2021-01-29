package Views;

public class UtilizadorView implements TrazAquiView{

    /**
     * Método que imprime uma mensagem pré definida.
     */
    @Override
    public void show() {
        System.out.print("               ███╗   ███╗███████╗███╗   ██╗██╗   ██╗                                   \n" +
                "               ████╗ ████║██╔════╝████╗  ██║██║   ██║                                   \n" +
                "               ██╔████╔██║█████╗  ██╔██╗ ██║██║   ██║                                   \n" +
                "               ██║╚██╔╝██║██╔══╝  ██║╚██╗██║██║   ██║                                   \n" +
                "               ██║ ╚═╝ ██║███████╗██║ ╚████║╚██████╔╝                                   \n" +
                "               ╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝ ╚═════╝                                    \n" +
                "                                                                         \n" +
                "██╗   ██╗████████╗██╗██╗     ██╗███████╗ █████╗ ██████╗  ██████╗ ██████╗ \n" +
                "██║   ██║╚══██╔══╝██║██║     ██║╚══███╔╝██╔══██╗██╔══██╗██╔═══██╗██╔══██╗\n" +
                "██║   ██║   ██║   ██║██║     ██║  ███╔╝ ███████║██║  ██║██║   ██║██████╔╝\n" +
                "██║   ██║   ██║   ██║██║     ██║ ███╔╝  ██╔══██║██║  ██║██║   ██║██╔══██╗\n" +
                "╚██████╔╝   ██║   ██║███████╗██║███████╗██║  ██║██████╔╝╚██████╔╝██║  ██║\n" +
                " ╚═════╝    ╚═╝   ╚═╝╚══════╝╚═╝╚══════╝╚═╝  ╚═╝╚═════╝  ╚═════╝ ╚═╝  ╚═╝\n");
        System.out.println("        ┌───────────────────────────────────────────────────────┐");
        System.out.println("        │  1 -> Fazer nova encomenda                            │");
        System.out.println("        │  2 -> Ver encomendas feitas                           │");
        System.out.println("        │  3 -> Ver encomendas em espera                        │");
        System.out.println("        │  4 -> Ver encomendas que precisam de ser aceites      │");
        System.out.println("        │  5 -> Ver Lojas disponiveis                           │");
        System.out.println("        │  6 -> Ver produtos de uma dada loja                   │");
        System.out.println("        │  7 -> Classificar entregador                          │");
        System.out.println("        │  8 -> Top10 Utilizadores                              │");
        System.out.println("        │  G -> Gravar                                          │");
        System.out.println("        │  S -> Logout                                          │");
        System.out.println("        └───────────────────────────────────────────────────────┘");
        System.out.print("Option: ");
    }

    /**
     * Método que imprime um Objeto como uma mensagem.
     * @param o Objeto a avaliar.
     */
    @Override
    public void show(Object o) {
        System.out.print((String) o );
    }
}
