
import Controller.*;


public class TrazAqui {
    public static void main(String[] args) {
        try{
            IController controller = new Controller();
            controller.menu();
        }catch(Exception e){
            System.err.println(e);
        }
    }
}