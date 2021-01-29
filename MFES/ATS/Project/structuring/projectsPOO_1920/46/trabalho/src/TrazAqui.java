
import javafx.application.Application;
import javafx.stage.Stage;
import View.*;

public class TrazAqui extends Application {
    Stage window;

    @Override
    public void start(Stage primaryStage) throws Exception{

        IView view = new View();
        window = primaryStage;
        window.setScene(view.menu());
        window.setTitle("Menu Principal");
        window.show();
    }

    public static void main(String[] args) {
        launch(args);
    }
}
