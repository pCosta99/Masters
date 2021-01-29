package MVC.Views;

import MVC.IView;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

public class ListView<T> implements IView {
    private List<T> l;

    public ListView(){
        this.l = new ArrayList<T>();
    }

    public ListView(Collection<T> c){
        this.l = new ArrayList<T>(c);
    }

    public void setList(List<T> l) {
        this.l = l;
    }

    public void show(){
        int size = l.size();
        System.out.println("##########################");
        int i;
        for (i = 0; i<size;i++) {
            System.out.println(l.get(i)+"\n");
        }
        System.out.println("##########################");
    }

    public void show(Object o){
        System.out.println(o);
    }


}