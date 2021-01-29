import java.io.File ;
import java.io.FileNotFoundException;
import java.io.IOException;

public class TrazAqui{
    /**
     * Metodo que inicia a execuçao do programa
     */
    public static void main(String [] args){
        ISistema model = new Sistema();
        Parse p = new Parse("logs.txt");
        p.lerFicheiro(model);
        IController controller = new Controller(model);
        IViewer viewer = new Viewer(controller);
        controller.setViewer(viewer);
        viewer.runViewer();
        try{
            controller.gravarEstado("trazAqui.dat");
        }catch(FileNotFoundException e){
            viewer.showMessage("Ficheiro não encontrado.");
        }catch(IOException e){
            viewer.showMessage("Ficheiro não encontrado.");
        }
    }
}
