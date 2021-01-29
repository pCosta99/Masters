package View;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.IntStream;

import static View.ViewHelper.*;
import static View.ViewHelper.RESET;

public class BoxBlock extends Block{

    private int initialWidth;
    private int nextPrint;
    private String boxTitle;

    BoxBlock(int initialWidth, String boxTitle) {
        super();
        this.initialWidth = initialWidth;
        nextPrint = 0;
        this.boxTitle = boxTitle;
    }

    public void reset() {
        super.reset();
        this.nextPrint = 0;
        this.removeLine(this.numOfLines()-1);
        this.removeLine(0);
        this.removeLine(0);
        this.removeLine(0);

    }

    public void render(int windowContentWidth) {
        int maxNaturalWidth = this.getMaxNaturalWidth();
        this.setContentWidth(Integer.max(maxNaturalWidth,this.initialWidth) + 2); //todo - hardocoded


        this.addLine(0,new Line(
                 "┌" +
                        "─".repeat(this.getContentWidth() - 2) +
                        "┐"
        ));
        this.addLine(1,newLine(
                "│"+
                        this.boxTitle+
                        " ".repeat(this.getContentWidth() - this.boxTitle.length() - 2)+
                        "│"
        ).color(1,GREEN_BOLD));
        this.addLine(2,newLine(
                "├" +
                        "─".repeat(this.getContentWidth() - 2) +
                        "┤"
        ));

        this.addLine(newLine(
                "└" +
                        "─".repeat(this.getContentWidth() - 2) +
                        "┘"
        ));

    }

    //FIXME hardcoded
    public void printNextLineFull() {
        if(this.getNextLine() < this.getNumOfLines()) {

            if(this.isPrintMargins()) printSpace(this.getMargin(MARGIN_LEFT));
            //String s = this.nextLine();
            //System.out.print(s);
            Line s = this.nextLine();
            //FIXME

            if(this.getNextLine() < 4 || this.getNextLine() == this.getNumOfLines()) {
                s.print();
                if(this.isPrintMargins()) printSpace(this.getMargin(MARGIN_RIGHT));
            } else {
                System.out.print("│");
                if (s.hasBGColor) System.out.print(s.bgColor);
                s.print();
                printSpace(this.getContentWidth() - s.length() - 2); //TODO - hardcoded
                System.out.print(RESET);
                System.out.print("│");
                if(this.isPrintMargins()) printSpace(this.getMargin(MARGIN_RIGHT));
            }


        } else {
            printSpace(this.getContentWidth()+2);
        }



    }


    public int getFullWidth() {
        int fromSuper = super.getFullWidth();
        int fromThis = this.initialWidth + this.getMargin(MARGIN_LEFT) + this.getMargin(MARGIN_RIGHT) + 2; //FIXME - hardcoded
        return Integer.max(fromSuper,fromThis);
    }



}
