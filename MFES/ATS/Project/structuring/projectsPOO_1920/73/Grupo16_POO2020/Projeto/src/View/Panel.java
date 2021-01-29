package View;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static View.ViewHelper.*;

public class Panel {
    private static final String DEFAULT_SEPARATOR = SEP_LINE;

    private List<Block> blocks;
    private List<String> separators;
    private int nextLine;
    private boolean hasEnded;

    public Panel(List<Block> blocks) {
        this.blocks = new ArrayList<>(blocks);
        //todo - remove later
        this.separators = new ArrayList<>(Collections.nCopies(this.blocks.size()-1, DEFAULT_SEPARATOR));
        this.hasEnded = false;
    }

    public Panel(Block block) {
        this.blocks = new ArrayList<>(Collections.singletonList(block));
        this.separators = new ArrayList<>();
        this.hasEnded = false;
    }

    public void add(Block b) {
        this.blocks.add(b);
        this.separators.add(DEFAULT_SEPARATOR);
    }

    public boolean hasEnded() {
        return this.hasEnded;
    }

    public int getMaxNumOfLines() {
        return blocks.stream().mapToInt(Block::numOfLines).max().orElse(0);
    }

    public void setSeparators(List<String> separators) {
        this.separators = new ArrayList<>(separators);
    }

    public void setSeparator(int i, String sep) {
        this.separators.set(i,sep);
    }

    public void applyMarginsToBlocks() {
        for(Block b : this.blocks) {
            b.applyMargins();
        }
    }


    /** With margins and spaces */
    public int getContentWidthFull() {
        int ret = 0;
        for(Block b : blocks) {
            ret += b.getFullWidth();
        }
        return ret + (this.separators.stream().mapToInt(String::length).sum());
    }



    /** calcula qual seria naturalmente a linha mais comprida (apenas texto, sem espaços entre blocos) */
    public int getMaxNaturalWidth() {
        //todo - refactor to streams (using block's getMaxNaturalWidthFunction)
        int max = 0;
        int current;
        int n = getMaxNumOfLines();
        for(int i = 0; i<n; i++) {
            current = 0;
            for(Block b : blocks) {
                current += b.getLine(i).length();
            }
            max = Integer.max(current,max);
        }
        return max;
    }

    public void printNextLineFullWithSeparators() {
        for(int i = 0; i< blocks.size();i++) {
            blocks.get(i).printNextLineFull();
            if(i != blocks.size()-1) System.out.print(this.separators.get(i));
        }
        this.hasEnded = this.blocks.stream().allMatch(Block::hasEnded);
    }

    public void render(int windowContentWidth) {
        for(Block b: this.blocks) {
            b.render(windowContentWidth);
        }
    }

    public void printNextLineFull() {
        for(Block b : blocks) {
            b.printNextLineFull();
        }
        this.hasEnded = this.blocks.stream().allMatch(Block::hasEnded);
    }


    @Override
    public String toString() {
        return "Panel{" +
                "blocks=" + blocks +
                ", separators=" + separators +
                ", nextLine=" + nextLine +
                '}';
    }

    public void reset() {
        this.nextLine = 0;
        this.hasEnded = false;
        for(Block b : this.blocks) {
            b.reset();
        }
    }
}
