package Views;

public class LoginView implements TrazAquiView {

    /**
     * Método que imprime uma mensagem pré definida.
     */
    @Override
    public void show() {
        System.out.print("# # # # # # # # # # # # # # # # # # # # # # # #");
        System.out.print("\n#    _____              ___              _    #\n" +
                "#   |_   _|            / _ \\            (_)   #\n" +
                "#     | |_ __ __ _ ___/ /_\\ \\ __ _ _   _ _    #\n" +
                "#     | | '__/ _` |_  /  _  |/ _` | | | | |   #\n" +
                "#     | | | | (_| |/ /| | | | (_| | |_| | |   #\n" +
                "#     \\_/_|  \\__,_/___\\_| |_/\\__, |\\__,_|_|   #\n" +
                "#                               | |           #\n" +
                "#                               |_|           #\n");
        System.out.print("# # # # # # # # # # # # # # # # # # # # # # # #\n");
        System.out.println("        ┌───────────────────────────┐");
        System.out.println("        |    1 -> Login             |");
        System.out.println("        |    2 -> Register          |");
        System.out.println("        |    L -> Load object file  |");
        System.out.println("        |    S -> Exit              |");
        System.out.println("        └───────────────────────────┘");
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
