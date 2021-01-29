import Controller.ITrazAquiController;
import Controller.TrazAquiController;
import Model.ITrazAquiModel;
import Model.Parse;
import Model.TrazAquiModel;
import View.ITrazAquiView;
import View.TrazAquiView;

public class TrazAquiRun {

    /**
     * Método main que inicia a aplicação
     * @param args
     */
    public static void main(String[] args) {

        ITrazAquiModel model = new TrazAquiModel();
        ITrazAquiController controller = new TrazAquiController(model);
        ITrazAquiView view = new TrazAquiView(controller);

        Parse.parse(model);

        view.run();
    }
}
