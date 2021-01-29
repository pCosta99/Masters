package View;

import static View.ViewHelper.*; //pq tenho de usar static?

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;

public class Block {
    private List<Line> lines; //todo - StringBuilder?
    private int nextLine;
    private int numOfLines;
    private int contentWidth; //Without margins //todo - estar atento a existir linhas que ultrapassem
    // margins começa no topo e anda no sentido dos ponteiros do relógio
    private List<Integer> margins;
    private boolean hasEnded;

    private boolean printMargins;



    public Block() {
        this.nextLine = 0;
        this.printMargins = false;
        this.lines = new ArrayList<>();
        numOfLines = 0;
        hasEnded = false;
        this.margins = Arrays.asList(defaultMargins);
    }


    public Block(List<Line> lines) {
        this();
        applyLines(lines);
    }

    public Block(List<Line> lines, int top_margin, int right_margin, int bottom_margin, int left_margin) {
        this();
        applyLines(lines);
        this.margins = new ArrayList<>(Arrays.asList(top_margin,right_margin,bottom_margin,left_margin));
    }


    //todo - refactor com o construtor de cima
    public Block(List<Line> lines, int contentWidth) {
        this();
        applyLines(lines);
        this.contentWidth = contentWidth;
    }


    //todo - refactor com o construtor de cima
    public Block(Block otherBlock) {
        this.lines = otherBlock.getLines();
        this.nextLine = otherBlock.getNextLine();
        this.numOfLines = otherBlock.getNumOfLines();
        this.contentWidth = otherBlock.getContentWidth();
        this.margins = otherBlock.getMargins();
        this.printMargins = otherBlock.isPrintMargins();
    }


    public void applyLines(List<Line> lines) {
        this.lines = new ArrayList<>(lines);
        this.numOfLines = lines.size();
    }

    public void addLine(Line line) {
        this.lines.add(line);
        this.numOfLines++;
    }

    public void addLine(int i, Line line) {
        this.lines.add(i,line);
        this.numOfLines++;
    }

    public boolean hasEnded() {
        return this.hasEnded;
    }


    // podia ser um array normal
    // margins começa no topo e anda no sentido dos ponteiros do relógio
    public void applyMargins() {
        this.contentWidth = this.getMaxNaturalWidth(); //TODO - talvez não devesse ficar aqui
        this.printMargins = true;
        lines.addAll(0,Collections.nCopies(this.margins.get(MARGIN_TOP), new Line()));
        lines.addAll(Collections.nCopies(this.margins.get(MARGIN_BOTTOM), new Line()));
        numOfLines+=this.margins.get(MARGIN_TOP) + this.margins.get(MARGIN_BOTTOM);
    }

    public Line nextLine() {
        if (nextLine < numOfLines) {
            return lines.get(nextLine++);
        } else {
            nextLine++;
            return new Line();
        }
    }

    public Line getLine(int i) {
        if (i < numOfLines) {
            return lines.get(i);
        } else return new Line();
    }

    public void printNextLine() {
        if(nextLine<numOfLines) {
            System.out.print(lines.get(nextLine++));
        }
    }

    public boolean isEmpty() {
        return this.lines.isEmpty();
    }

    public void printLine(int i) {
        if(i<numOfLines) {
            System.out.print(lines.get(i));
        }
    }

    /** print next line + spaces(to align content) + margins*/
    //FIXME - está harcoded
    public void printNextLineFull() {
        if(printMargins) printSpace(this.margins.get(MARGIN_LEFT));
        //String s = this.nextLine();
        //System.out.print(s);
        Line s = this.nextLine();
        //FIXME
        if(!s.isTitle()) {
            if (s.hasBGColor) System.out.print(s.bgColor);
            s.print();
            printSpace(contentWidth - s.length());
            System.out.print(RESET);
            if(printMargins) printSpace(this.margins.get(MARGIN_RIGHT));
        } else {
            System.out.print(BLACK);
            if (s.hasBGColor) System.out.print(s.bgColor);
            s.print();
            printSpace(contentWidth - s.length());
            if(printMargins) printSpace(this.margins.get(MARGIN_RIGHT));
            System.out.print(RESET);
        }
        if(this.nextLine >= this.numOfLines) hasEnded = true;
    }

    public void render(int windowContentWidth) {
        boolean ret = false;
        for(Line line : this.lines) {
            //Fazer o render das linhas aqui
            if (line.isTitle()) {
                line.renderTitle(windowContentWidth);
                ret = true;
            }
        }
        if(ret) {
            this.contentWidth = windowContentWidth;
        }
    }

    public void resetPosition() {
        nextLine = 0;
        hasEnded = false;
    }

    public int numOfLines() {
        return this.numOfLines;
    }


    public int getMaxNaturalWidth() {
        int max = 0;
        int current;
        for(int i = 0; i<numOfLines; i++) {
            current = this.lines.get(i).length();
            max = Integer.max(current,max);
        }
        return max;
    }



    /** With margins */
    public int getFullWidth() {
        return this.contentWidth + this.margins.get(MARGIN_LEFT) + this.margins.get(MARGIN_RIGHT);
    }

    public void setMargins(List<Integer> margins) {
        this.margins = new ArrayList<>(margins);
    }

    public void backLine() {
        this.nextLine--;
    }


    /**
     * GETTERS & SETTERS
     */

    public List<Line> getLines() {
        return new ArrayList<>(this.lines);
    }

    public int getNextLine() {
        return nextLine;
    }

    public int getNumOfLines() {
        return numOfLines;
    }

    public int getContentWidth() {
        return contentWidth;
    }

    public List<Integer> getMargins() {
        return new ArrayList<>(this.margins);
    }

    public boolean isPrintMargins() {
        return printMargins;
    }

    public void setContentWidth(int contentWidth) {
        this.contentWidth = contentWidth;
    }

    public int getMargin(int i) {
        return this.margins.get(i);
    }

    /**
     * FUNÇÕES REIMPLEMENTADAS
     */


    public Block clone() {
        return new Block(this);
    }


    @Override
    public String toString() {
        return "Block{" +
                "lines=" + lines +
                ", nextLine=" + nextLine +
                ", numOfLines=" + numOfLines +
                ", contentWidth=" + contentWidth +
                ", margins=" + margins +
                ", printMargins=" + printMargins +
                '}';
    }

    public void reset() {
        this.resetPosition();
        for(int i = 0; i<margins.get(0); i++) {
            this.lines.remove(0);
            numOfLines--;
        }
        for(int i = 0; i<margins.get(2); i++) {
            this.lines.remove(this.lines.size()-1);
            numOfLines--;
        }
    }

    public void removeLine(int i) {
        this.lines.remove(i);
        numOfLines--;
    }

}


/*
    private boolean printMargins;

    private Block() {
        this.nextLine = 0;
        this.printMargins = false;
    }


    public Block(List<Line> lines) {
        this();
        this.lines = new ArrayList<>(lines);
        this.numOfLines = lines.size();
        this.margins = new ArrayList<>(Collections.nCopies(4, 1)); //default margins of 1

        // ------ todo - remove later
        this.applyMargins();
        this.contentWidth = this.getMaxNaturalWidth();
        // ------
    }

    public Block(List<Line> lines, int top_margin, int right_margin, int bottom_margin, int left_margin) {
        this(); //todo - refactor
        //todo - refactor (usar um construtor mais genérico)

        this.lines = new ArrayList<>(lines);
        this.nextLine = 0;
        this.numOfLines = lines.size();
        this.margins = new ArrayList<>(Arrays.asList(top_margin,right_margin,bottom_margin,left_margin));

        // ------ todo - remove later
        this.applyMargins();
        this.contentWidth = this.getMaxNaturalWidth();
        // ------
    }
 */