package MVC.Controller;

import java.util.ArrayList;
import java.util.List;

import MVC.Model.Model;
import MVC.Observer.Observable;
import MVC.Controller.Menu.Menu;

public abstract class Controller extends Observable{
    private Model model;
    private String screen;
    private final String[] sLogin = {"Menu", "Login", "Register"};
    private final Menu login = new Menu(sLogin);

    private String option;
    
    public Controller(Model model) {
        this.screen = "";
        this.model = model;
        this.screen = login.toString();
    }

    public void setScreen(String screen) {
        this.screen = screen;
        notifyObservers(screen);
    }

    public void setScreen(String[] list) {
        Menu buf = new Menu(list);
        setScreen(buf.toString());
    }

    public void setScreen(List<String> list) {
        Menu buf = new Menu(list);
        setScreen(buf.toString());
    }

    public void setScreen(Menu menu) {
        setScreen(menu.toString());
    }

    public String getScreen() {
        return this.screen;
    }

    public String getOption() {
        return this.option;
    }

    public void setOption(String option) {
        this.option = option;
    }

    public Menu getLogin() {
        return this.login;
    }


    public Model getModel() {
        return this.model;
    }

    protected abstract void update();

    public void update(String x){
        setOption(x);
        update();
    }
    
}