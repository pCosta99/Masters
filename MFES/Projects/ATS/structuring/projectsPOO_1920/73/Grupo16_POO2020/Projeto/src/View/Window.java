package View;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

import static View.ViewDB.banner;
import static View.ViewHelper.*;

public class Window {
    private static final String DEFAULT_SEPARATOR = HSEP_LINE;

    private int contentWidth;
    List<Panel> panels;
    boolean hasBanner;
    List<String> separators;

    public Window(int contentWidth) {
        this.contentWidth = contentWidth;
        this.panels = new ArrayList<>();
        this.separators = new ArrayList<>();
        this.hasBanner = false;
    }

    public Window() {
        this.contentWidth = 0;
        this.panels = new ArrayList<>();
        this.separators = new ArrayList<>();
        this.hasBanner = false;
    }


    public void addPanel(Panel p) {
        this.panels.add(p);
        this.separators.add(DEFAULT_SEPARATOR);
    }

    public void addPanel(int i, Panel p) {
        this.panels.add(i,p);
        this.separators.add(i,DEFAULT_SEPARATOR);
    }

    public void addBlock(Block b){
        this.addPanel(new Panel(b));
        this.separators.add(DEFAULT_SEPARATOR);
    }


    /** With margins and spaces */
    private int getMaxContentWidthFull() {
        int max = 0;
        int current;
        for(Panel p : panels) {
            current = p.getContentWidthFull();
            max = Integer.max(current,max);
        }
        return max;
    }

    private void applyMarginsToPanels() {
        for(Panel p : panels) {
            p.applyMarginsToBlocks();
        }
    }

    public void render() {
        this.applyMarginsToPanels();
        this.contentWidth = this.getMaxContentWidthFull();
        // Caso queira que os paineis façam algo (tipo saber a largura da janela)
        for(Panel p : panels) {
            p.render(this.contentWidth);
        }
    }

    private void displayBanner() {
        Panel p = banner();
        while(!p.hasEnded()) {
            System.out.print("║");
            p.printNextLineFullWithSeparators();
            printSpace(this.contentWidth - p.getContentWidthFull());
            System.out.print("║");
            System.out.print("\n");
        }
    }

    public void addBanner() {
        this.hasBanner = true;
        this.addPanel(0,banner());
        this.separators.add(0,HSEP_TWOLINES);
    }


    public void addTitle(String title) {
        int pos = this.hasBanner ? 1 : 0;
        this.panels.add(pos,new Panel(
                new Block(Collections.singletonList(newLine(" "+title).colorBG(WHITE_BACKGROUND).setAsTitle())
                        ,0,0,0,0)
        ));
        this.separators.add(pos,null);
    }

    private void printSeparator(String sep, int num) {
        String start = null;
        String end = null;
        if(sep!=null) {
            if(sep.equals("─")) {
                start = "╟"; end = "╢";
            } else if(sep.equals("═"))     {
                start = "╠"; end = "╣";
            }
            printHorizontalNTImes(start,sep,end,num);
        }

    }

    private void printHorizontalNTImes(String start, String middle, String end, int num) {
        System.out.print(start);
        IntStream.range(0,num).forEach(a -> System.out.print(middle));
        System.out.print(end);
        System.out.print("\n");
    }

    //FIXME - está hardcoded
    public void display() {

        this.printHorizontalNTImes("╔","═","╗",this.contentWidth);

        //Banner e titulo
        //if (this.hasBanner) this.displayBanner();
        //this.printHorizontalSeparator("╠","═","╣",this.contentWidth);


        //Paineis normais
        for(int i = 0; i < this.panels.size(); i++) {
            Panel p = this.panels.get(i);
            int numOfPanelLines = p.getMaxNumOfLines();
            for(int j = 0; j<numOfPanelLines; j++) {
                //p.printNextLineFull();
                System.out.print("║");
                p.printNextLineFullWithSeparators();
                printSpace(this.contentWidth - p.getContentWidthFull());

                System.out.print("║");
                System.out.print("\n");
            }
            //todo - remove later (é só suposto fazer isto entre paineis)
            /*
            if(i==0 && this.hasBanner) this.printHorizontalSeparator("╠","═","╣",this.contentWidth);
            else this.printHorizontalSeparator("╟","─","╢",this.contentWidth);
             */

            this.printSeparator(this.separators.get(i),this.contentWidth);
        }

        this.printHorizontalNTImes("╚","═","╝",this.contentWidth);
        for(Panel p : this.panels) {
            p.reset();
        }

    }

    /** calcula qual seria naturalmente a linha mais comprida (apenas texto, sem espaços entre blocos) */
    public int getMaxNaturalWidth() {
        int max = 0;
        int current;
        for(Panel p : panels) {
            current = p.getMaxNaturalWidth();
            max = Integer.max(current,max);
        }
        return max;
    }

}


