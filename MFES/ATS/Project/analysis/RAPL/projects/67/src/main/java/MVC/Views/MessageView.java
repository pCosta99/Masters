package MVC.Views;

import MVC.IView;

public class MessageView implements IView {
    public void show(){
    }

    public void show(Object o){
        System.out.println(o.toString());
    }
}
