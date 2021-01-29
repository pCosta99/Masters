package View;

import Helpers.IPair;

import java.util.ArrayList;

public interface IView {
    void setMenuQueries(ArrayList<IPair<Integer, String>> menuQueries);

    void showWindow(Window w);

    void printStringError(String line);

    void printStringNormal(String line);


    void printStringBright(String line);

    void printInputLine();

    public IView clone();
}
