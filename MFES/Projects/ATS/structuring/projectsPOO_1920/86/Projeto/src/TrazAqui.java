import java.io.IOException;

public class TrazAqui {
    public static void main(String[] args) throws IOException {
        TrazAquiModel model = new TrazAquiModel("logs.txt");
        TrazAquiController c = new TrazAquiController();
        TrazAquiView v = new TrazAquiView();

        c.setView(v);
        c.setModel(model);
        c.start();
    }
}
