package MVC.View;

import java.util.EventListener;
import java.util.Scanner;

import MVC.Controller.Controller;
import MVC.Controller.Menu.Menu;
import MVC.Observer.Observable;
import MVC.Observer.Observer;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import java.awt.FlowLayout;
import java.awt.event.ActionEvent;

public class View extends AbstractAction implements Observer{
    private Controller controller;
    private JFrame frame;
    private JTextArea text;
    private JTextField field;

    private String ID;

    public View(Controller controller) {
        controller.addObserver(this);

        this.controller = controller;
        this.frame = new JFrame();

        this.text = new JTextArea();
        this.text.setBounds(0, 0, 600, 400);
        this.text.setEditable(false);

        this.field = new JTextField();

        this.field.addActionListener(this);

        this.field.setBounds(0, 200, 100, 30);

        this.frame.add(this.field);

        this.frame.add(this.text);
        
        this.frame.setSize(600, 400);
        this.frame.setLayout(null);
        this.frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    }

    public void run() {
        refreshText(this.controller.getScreen());
        this.ID = this.text.getText().substring(4).split("_")[0];
        refreshPagina();
    }

    private void refreshText(String x) {
        this.text.setText(x);
        this.field.setText("");
    }

    private void refreshPagina() {
        this.frame.setVisible(true);
    }

    @Override
    public void update(Observable source, Object value) {
        if(value.getClass().isAssignableFrom(String.class)) {
            refreshText((String) value);
            this.ID = this.text.getText().substring(4).split("_")[0];
            if(this.ID.equals("quit")) {
                frame.setVisible(false);
                frame.dispose();
                return;
            }
        }
        if(value.getClass().isAssignableFrom(Menu.class)) {
            this.ID = ((Menu) value).getId();
            refreshText(value.toString());
        }
        refreshPagina();
    }

    @Override
        public void actionPerformed(ActionEvent e) {
                String test = this.field.getText();
                controller.update(String.format("%s-%s", this.ID, test));
        }
}