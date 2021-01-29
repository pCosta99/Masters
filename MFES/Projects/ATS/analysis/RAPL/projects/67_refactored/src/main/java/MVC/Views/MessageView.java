package MVC.Views;

import MVC.IView;

public class MessageView implements IView {
    /**
     * this method doens't do anything
     * for some reason someone thought it would be a good idea to extend an interface without the need to
     * implement all the methods specified in it
     */
    public void show(){
    }

    public void show(Object o){
        System.out.println(o.toString());
    }
}
