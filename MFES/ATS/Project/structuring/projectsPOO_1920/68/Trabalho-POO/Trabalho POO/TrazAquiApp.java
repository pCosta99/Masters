import java.io.IOException;

/**
 * Classe que tem a main da aplicação.
 */
public class TrazAquiApp {

    public static void main(String[] args) throws IOException, InterruptedException {
        Sistema sistema = new Sistema();
        AppUser.setSistema(sistema);

        ViewGeral.menuInicial();
        int opcao=5;
        while (opcao!=1 && opcao!=2 && opcao!=0) {
            opcao = Input.lerInt();
            switch (opcao){

                case 1:
                    try {
                        sistema.parse_logs();
                        sistema.distribuiEncomendas();
                    } catch (IOException e) {
                        ViewGeral.PrintMensagem("Erro ao carregar logs. Tente novamente.");
                        opcao=5;
                    }
                    break;

                case 2:
                    try{
                        sistema.carregaEstado();
                        sistema.distribuiEncomendas();

                    } catch (ClassNotFoundException | IOException e ) {
                        ViewGeral.PrintMensagem("Erro ao carregar logs. Tente novamente.");
                        opcao=5;
                    }
                    break;

                case 0:
                    break;

                default:
                    ViewGeral.PrintMensagem("Opção Inválida");
            }
        }

        if(opcao==0){
            ViewGeral.saida();
        }
        else{
            Controller controller = new Controller(sistema);
            controller.inicio();
        }
    }

}
