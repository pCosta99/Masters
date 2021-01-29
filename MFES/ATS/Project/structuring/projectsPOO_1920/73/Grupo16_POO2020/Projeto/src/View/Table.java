package View;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public class Table implements ITable {

    private List<String> hTitles;
    private List<String> vTitles;

    private List<List<String>> content;

    public Table(List<String> hTitles, List<String> vTitles) {
        this.content = new ArrayList<>();
        this.hTitles = new ArrayList<>(hTitles);
        this.vTitles = new ArrayList<>(vTitles);
    }

    public Table(List<String> hTitles) {
        this.content = new ArrayList<>();
        this.hTitles = new ArrayList<>(hTitles);
        this.vTitles = new ArrayList<>();
    }

    public Table() {
        this.content = new ArrayList<>();
        this.hTitles = new ArrayList<>();
        this.vTitles = new ArrayList<>();
    }

    @Override
    public void addLine(List<String> line) {
        content.add(line);
    }

    @Override
    public void addLines(List<List<String>> lines) {
        content.addAll(lines);
    }

    private String space(int n) {
        StringBuilder sb = new StringBuilder("");
        sb.append(" ".repeat(Math.max(0, n)));
        return sb.toString();
    }

    private String topLine(int vTitleWidthFull, List<Integer> maxContentWidths, boolean printSpace) {
        StringBuilder sb = new StringBuilder();
        if(printSpace)sb.append(space(vTitleWidthFull));
        for(int i = 0; i<this.content.get(0).size(); i++) {
            if(i == 0 && i == this.content.get(0).size()-1) {
                sb.append("┌").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2))).append("┐");
            } else if(i == 0) {
                sb.append("┌").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
            } else if(i == this.content.get(0).size()-1) {
                sb.append("┬").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2))).append("┐");
            } else {
                sb.append("┬").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
            }
        }
        return sb.toString();
    }

    private String contentLine(List<String> line, int vTitleWidthFull, List<Integer> maxContentWidths, boolean printSpace) {
        StringBuilder sb = new StringBuilder();
        if(printSpace) sb.append(space(vTitleWidthFull));
        for(int i = 0; i<this.content.get(0).size(); i++) {
                sb.append("│").append(" ").append(line.get(i)).append(space(maxContentWidths.get(i)-line.get(i).length())).append(" ");
        }
        sb.append("│");
        return sb.toString();
    }

    private String separateLine(int vTitleWidthFull, List<Integer> maxContentWidths, boolean printSpace) {
        StringBuilder sb = new StringBuilder();
        boolean hasVTitles = vTitleWidthFull != 0;
        if(printSpace) sb.append(space(vTitleWidthFull));
        for(int i = 0; i<this.content.get(0).size(); i++) {
            if(i == 0 && i == this.content.get(0).size()-1) {
                if (hasVTitles) sb.append("┼"); else sb.append("├");
                sb.append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
                sb.append("┤");
            } else if(i == 0) {
                if (hasVTitles) sb.append("┼"); else sb.append("├");
                sb.append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
            } else if(i == this.content.get(0).size()-1) {
                sb.append("┼").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2))).append("┤");
            } else {
                sb.append("┼").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
            }
        }
        return sb.toString();
    }

    private String fullSeparator(int vTitleWidthFull, List<Integer> maxContentWidths, boolean isTop) {
        StringBuilder sb = new StringBuilder();
        boolean hasVTitles = vTitleWidthFull != 0;
        if(hasVTitles) {
            if(isTop) sb.append("┌");
            else sb.append("├");
            sb.append("─".repeat(Math.max(0, vTitleWidthFull)));
        }
        sb.append(this.separateLine(vTitleWidthFull,maxContentWidths,false));
        return sb.toString();
    }

    private String lastSeparator(int vTitleWidthFull, List<Integer> maxContentWidths) {
        StringBuilder sb = new StringBuilder();
        boolean hasVTitles = vTitleWidthFull != 0;
        if(hasVTitles) {
            sb.append("└");
            sb.append("─".repeat(Math.max(0, vTitleWidthFull)));
        }

        for(int i = 0; i<this.content.get(0).size(); i++) {
            if(i == 0 && i == this.content.get(0).size()-1) {
                if (hasVTitles) sb.append("┴"); else sb.append("└");
                sb.append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
                sb.append("┘");
            } else if(i == 0) {
                if (hasVTitles) sb.append("┴"); else sb.append("└");
                sb.append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
            } else if(i == this.content.get(0).size()-1) {
                sb.append("┴").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2))).append("┘");
            } else {
                sb.append("┴").append("─".repeat(Math.max(0, maxContentWidths.get(i) + 2)));
            }
        }

        return sb.toString();
    }

    private String fullContent(int pos, int vTitleWidthFull, List<Integer> maxContentWidths) {
        StringBuilder sb = new StringBuilder();
        boolean hasVTitles = vTitleWidthFull != 0;
        if(hasVTitles) {
            sb.append("│ ").append(this.vTitles.get(pos)).append(" ");
        }
        sb.append(contentLine(this.content.get(pos),vTitleWidthFull,maxContentWidths,false));
        return sb.toString();
    }

    private List<String> makeTitle(int vTitleWidthFull,List<Integer> maxContentWidths) {
        List<String> ret = new ArrayList<>();
        int width = vTitleWidthFull == 0 ? 0 : vTitleWidthFull+1;
        ret.add(topLine(width,maxContentWidths,true));
        ret.add(contentLine(hTitles,width,maxContentWidths,true));
        //ret.add(separateLine(vTitleWidthFull+1,maxContentWidths,true));
        return ret;
    }



    @Override
    public List<String> toStringList() {
        int vTitleWidth = this.maxVTitleWidth();
        List<Integer> contentWidths = this.maxContentWidths();
        List<String> ret = new ArrayList<>();
        StringBuilder sb = new StringBuilder();

        int vTitleWidthFull = vTitleWidth == 0 ? 0 : vTitleWidth + 2;
        List<Integer> maxContentWidths = this.maxContentWidths();
        if (!this.hTitles.isEmpty()) ret.addAll(this.makeTitle(vTitleWidthFull,maxContentWidths));

        for(int i = 0; i<this.content.size(); i++) {
            if(i== 0 & i == this.content.size()-1) {
                sb.append(fullSeparator(vTitleWidthFull,contentWidths,true));
                ret.add(sb.toString()); sb = new StringBuilder();
                sb.append(fullContent(i,vTitleWidthFull,maxContentWidths));
                ret.add(sb.toString()); sb = new StringBuilder();
                sb.append(lastSeparator(vTitleWidthFull,contentWidths));
                ret.add(sb.toString()); sb = new StringBuilder();
            } else if (i==0) {
                sb.append(fullSeparator(vTitleWidthFull,contentWidths,true));
                ret.add(sb.toString()); sb = new StringBuilder();
                sb.append(fullContent(i,vTitleWidthFull,maxContentWidths));
                ret.add(sb.toString()); sb = new StringBuilder();
            } else if(i == this.content.size()-1) {
                sb.append(fullSeparator(vTitleWidthFull,contentWidths,false));
                ret.add(sb.toString()); sb = new StringBuilder();
                sb.append(fullContent(i,vTitleWidthFull,maxContentWidths));
                ret.add(sb.toString()); sb = new StringBuilder();
                sb.append(lastSeparator(vTitleWidthFull,contentWidths));
                ret.add(sb.toString()); sb = new StringBuilder();
            } else {
                sb.append(fullSeparator(vTitleWidthFull,contentWidths,false));
                ret.add(sb.toString()); sb = new StringBuilder();
                sb.append(fullContent(i,vTitleWidthFull,maxContentWidths));
                ret.add(sb.toString()); sb = new StringBuilder();
            }
        }
        return ret;
    }

    private int maxVTitleWidth() {
        return vTitles.stream().mapToInt(String::length).max().orElse(0);
    }

    private List<Integer> maxContentWidths() { //conta com os hTitles
        if (this.content.isEmpty()) {
            this.addLine(Collections.nCopies(hTitles.size(),""));
        }

        int nmColumns = this.content.get(0).size();
        List<Integer> ret = new ArrayList<>(Collections.nCopies(nmColumns,0));
        for (int row = 0; row < this.content.size(); row++) {
            for (int column = 0; column < nmColumns; column++) {
                if (content.get(row).get(column).length() > ret.get(column)) {
                    ret.set(column,content.get(row).get(column).length());
                }
            }
        }
        for(int i = 0; i< hTitles.size() ; i++) {
            ret.set(i,Integer.max(ret.get(i),hTitles.get(i).length()));
        }
        return ret;
    }

}
