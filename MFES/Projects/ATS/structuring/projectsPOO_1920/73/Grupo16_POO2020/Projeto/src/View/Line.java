package View;
import static View.ViewHelper.*; //pq tenho de usar static?

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class Line {
    private int length;
    private String string;
    private Map<Integer,String> specialWords;
    //FIXME
    public boolean hasBGColor;
    public String bgColor;
    private boolean isTitle; //TODO subclass?

    Line(String string) {
        this.string = string;
        this.length = string.length();
        this.specialWords = new HashMap<>();
        hasBGColor = false;
        isTitle = false;
    }

    Line() {
        this.string = "";
        this.length = 0;
        this.specialWords = new HashMap<>();
        hasBGColor = false;
        isTitle = false;

    }

    private void addSpaces(int windowContentWidth) {
        int spaces = windowContentWidth - this.length;

        StringBuilder sb = new StringBuilder();
        sb.append(this.string);
        sb.append(" ".repeat(Math.max(0, spaces)));
        this.string = sb.toString();
        length = windowContentWidth;
    }

    public void renderTitle(int windowContentWidth) {
        this.addSpaces(windowContentWidth);
    }

    public void print() {
        //String[] splitStr = string.trim().split("\\s+"); //divide pelos espaços brancos
        String[] splitStr = string.split("((?<=\\s)|(?=\\s+))"); //mantém os espaços brancos
        int elems = splitStr.length;
        int wordPos = -1;
        for(int i = 0; i<elems; i++) {
            if(!splitStr[i].trim().isEmpty()) { //if is a word (not a space)
                wordPos++;
                boolean isSpecialWord = this.specialWords.containsKey(wordPos);
                if(isSpecialWord) System.out.print(this.specialWords.get(wordPos));
                System.out.print(splitStr[i]);
                if(isSpecialWord) System.out.print(RESET); //TODO deve ser dado na função que chama, para caso tenha background
            } else {
                System.out.print(splitStr[i]);
            }
        }


    }

    //if(i != splitStr.length-1) System.out.print(" ");


    public int length() {
        return this.length;
    }

    public Line color(int i, String color) {
        this.specialWords.put(i,color);
        return this; //so I can make color(...).color(...) ....
    }

    public Line colorBG(String color) {
        this.bgColor = color;
        this.hasBGColor = true;
        return this;
    }

    public Line setAsTitle() {
        this.isTitle = true;
        return this;
    }

    public boolean isTitle() {
        return this.isTitle;
    }


    @Override
    public String toString() {
        return "Line{" +
                "length=" + length +
                ", string='" + string + '\'' +
                ", specialWords=" + specialWords +
                '}';
    }
}
